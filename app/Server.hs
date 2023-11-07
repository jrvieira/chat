module Main ( main ) where

import Prelude hiding ( putStrLn, unwords, words )
import Network.WebSockets
import Control.Concurrent ( myThreadId, killThread )
import Control.Concurrent.Async
import Control.Concurrent.STM
import System.Console.ANSI ( clearScreen )
import TextShow ( showt )
import Text.Read ( readMaybe )
import Data.Aeson  ( encode )
import Data.Aeson.Text ( encodeToLazyText )
import Control.Exception
import Control.Monad
import Data.Text ( Text, unwords, words, uncons, cons, pack, unpack, compareLength )
import Data.Text.IO ( putStrLn )
import Data.Char
import Data.Time
import Data.Time.Clock.POSIX
-- import Data.Set ( Set )
-- import Data.Set qualified as Set ()
import Data.Map.Strict ( Map )
import Data.Map.Strict qualified as Map ( insertWith, adjust, filter, keys, findWithDefault )

import Zero
import Zero.Color

import Config
import Types

default ([], Word, Text)

main :: IO ()
main = do

   clearScreen >> logs (Only "main base online")

   st :: TVar State <- atomically $ newTVar State
      { trip = 0
      , list = mempty
      , subs = mempty
      }

   runServer address port $ app st

{- TODO:

-- PHASE NIL

   Mode + Mark

-- PHASE I

   log to file
   user registration
   grey channel on mute

-- PHASE II

   Chat + Game modules

   create game logic
   create gameplay protocol
   multiple tables as people join
   create render
   make ws page ( join with chat )

-}

-- app thread on connection
app :: TVar State -> PendingConnection -> IO ()
app st pending = do

   -- create pipeline
   connection :: Connection <- acceptRequest pending
   queue :: TQueue (TVar Peer,Signal) <- atomically newTQueue

   -- add peer to state
   u :: UTCTime <- getCurrentTime
   peer :: TVar Peer <- atomically $ new connection queue u

   logs $ Info $ unwords ["app connect"]

   -- ask and set nick
   withAsync (forever $ sync peer) $ \_ ->
      withPingThread connection 12 (ping peer 0 True) $
      sign peer

   p :: Peer <- atomically $ readTVar peer

   when (mempty == nick p || anon == nick p) $ kill peer

   logs $ Info $ unwords ["app sign as",nick p]

   pipe peer Signal
      { base = True
      , time = u
      , code = Broadcast
      , text = Info $ unwords ["→",nick p]
      }

   atomically $ modifyTVar' st (\s -> s { list = peer : list s })

   l :: [TVar Peer] <- atomically $ list <$> readTVar st

   -- io loop with sync threadfull
   void $ withAsync (forever $ sync peer) $ \_ ->
      withPingThread connection 12 (ping peer 0 False) $
         forever $ hear peer >>= pipe peer

   -- failsafe
   logs $ Error "app hit bottom"
   kill peer

   where

   -- create a new peer
   new :: Connection -> TQueue (TVar Peer,Signal) -> UTCTime -> STM (TVar Peer)
   new connection queue u = do
      peer :: TVar Peer <- newTVar $ Peer { nick = anon , conn = connection , line = queue , open = u }
      pure peer

   -- IO
   -- hear >>= pipe --> transmit ))) sync --> echo
   --               (((
   --               ))) st

   -- wait for one incoming message
   hear :: TVar Peer -> IO Signal
   hear peer = do
      p :: Peer <- atomically $ readTVar peer
      i :: Either SomeException Text <- try $ receiveData (conn p)
      u :: UTCTime <- getCurrentTime
      case i of
         Right t -> do
            pure Signal
               { base = False
               , time = u
               , code = Pure
               , text = Only t
               }
         Left e -> do
            pure Signal
               { base = False
               , time = u
               , code = Command "kill"
               , text = Warning $ showt e
               }

   -- process data: assign signal code and relay
   pipe :: TVar Peer -> Signal -> IO ()
   pipe peer signal
   -- | False  # unpack (unwords ["pipe",showt signal]) = undefined
      -- pipe commands
      | Pure <- code signal , Only t <- text signal , Just (k,c) <- uncons t , k == commchar = pipe peer signal { code = Command c }
      -- privchar
      | Pure <- code signal , Only t <- text signal , Just (k,c) <- uncons t , k == privchar = pipe peer signal { code = Command $ unwords ["priv",c] }
      -- chanchar
      | Pure <- code signal , Only t <- text signal , Just (k,c) <- uncons t , k == chanchar = pipe peer signal { code = Command $ unwords ["chan",c] }
      --
      | Pure <- code signal = pipe peer signal { code = Broadcast }  -- for now broadcast everything by default
      -- internal
      | Internal <- code signal = do
         logs $ ("pipe internal: " <>) <$> text signal
         echo peer (peer,signal)
      -- private message to list of peers
      | Private _ c <- code signal = atomically $ transmit signal c
      -- message to channel
      | Channel c <- code signal = do
         s :: Map Text [TVar Peer] <- atomically $ subs <$> readTVar st
         mapM_ atomically $ transmit signal <$> Map.findWithDefault mempty c s
      -- send to all peers
      | Broadcast <- code signal = atomically $ mapM_ (transmit signal) . list =<< readTVar st
      -- run command
      | Command c <- code signal = command $ words c

      where

      -- add signal to peer queue
      transmit :: Signal -> TVar Peer -> STM ()
      transmit s target = do
         t :: Peer <- readTVar target
         writeTQueue (line t) (peer,s)

      -- COMMANDS

      command :: [Text] -> IO ()

      command []
         | base signal = logs $ Error "command empty from base"
         | otherwise = tell (Info "command empty") peer

      command (comm : arg)

         | base signal = logs $ Error "command unknown from base"
         -- test command
         | "test" <- comm = logs $ Info $ unwords $ "pipe test command" : arg

         -- ping server
         | "ping" <- comm , Just t <- readMaybe $ unpack $ unwords arg = do
            p :: Peer <- atomically $ readTVar peer
            unless (nick p == anon) $ ping peer t True
         | "ping" <- comm = pure ()

         | "pong" <- comm , [] <- arg = pure ()
         | "pong" <- comm = do
            u :: UTCTime <- getCurrentTime
            let r = (round $ (* 1000) $ utcTimeToPOSIXSeconds u) - (read $ unpack $ unwords arg)
            atomically $ modifyTVar' st (\s -> s { trip = r })

      -- -- change nick
      -- | "sign" <- comm , [] <- arg = sign peer
      -- | "sign" <- comm = tell (Info "command :sign takes no arguments") peer

         -- list peers
         | "list" <- comm , [] <- arg = do
            l :: [TVar Peer] <- atomically $ list <$> readTVar st
            n :: [Peer] <- atomically $ mapM readTVar l
            u :: UTCTime <- getCurrentTime
            pipe peer Signal
               { base = True
               , time = u
               , code = Private mempty peer
               , text = Only $ unwords $ nick <$> n
               }
         | "list" <- comm = tell (Info "command :list takes no arguments") peer

         -- kill command
         | "kill" <- comm , [] <- arg = kill peer
         | "kill" <- comm = tell (Info "command :kill takes no arguments") peer

         -- send private message
         | "priv" <- comm , [] <- arg = tell (Info "command :priv takes at lest one argument") peer
         | "priv" <- comm , t : a <- arg = do
            v :: Bool <- atomically $ valid t
            if not v then do
               tell (Info $ unwords [t,"not here"]) peer
            else do
               l :: [TVar Peer] <- atomically $ list <$> readTVar st
               n :: [TVar Peer] <- atomically $ filterM (((== t) . nick <$>) . readTVar) l
               if null n then do
                  tell (Info $ unwords [t,"not found"]) peer
               else do
                  -- relay
                  mapM_ (\x -> pipe peer signal
                     { code = Private t x
                     , text = Only $ unwords a
                     }) $ [peer] ∪ n

         -- list channel subscriptions
         | "subs" <- comm , [] <- arg = do
            m :: Map Text [TVar Peer] <- atomically $ subs <$> readTVar st
            tell (Only $ unwords $ Map.keys $ Map.filter (peer ∈) m) peer
         | "subs" <- comm = tell (Info "command :subs takes no arguments") peer

         -- subscribe to channel
         | "tune" <- comm , [] <- arg = tell (Info "command :tune takes at lest one argument") peer
         | "tune" <- comm , any (\x -> compareLength x 12 /= LT) arg = tell (Info "invalid channel") peer
         | "tune" <- comm = do
            atomically $ mapM_ (tune peer) arg
            p :: Peer <- atomically $ readTVar peer
            u :: UTCTime <- getCurrentTime
            mapM_ (\x -> pipe peer Signal
               { base = True
               , time = u
               , code = Channel x
               , text = Info $ unwords [">",nick p]
               }) arg
         -- unsubscribe to channel
         | "mute" <- comm , [] <- arg = tell (Info "command :mute takes at lest one argument") peer
         | "mute" <- comm = do
            atomically $ mapM_ (mute peer) arg
            p :: Peer <- atomically $ readTVar peer
            u :: UTCTime <- getCurrentTime
            mapM_ (\x -> pipe peer Signal
               { base = True
               , time = u
               , code = Channel x
               , text = Info $ unwords ["<",nick p]
               }) arg
         -- send chan message
         | "chan" <- comm , [] <- arg = tell (Info "command :chan takes at lest one argument") peer
         | "chan" <- comm , t : a <- arg = do
            m :: Map Text [TVar Peer] <- atomically $ subs <$> readTVar st
            if peer ∈ Map.findWithDefault mempty t m then do
               pipe peer signal
                  { code = Channel t
                  , text = Only $ unwords a
                  }
            else do
               tell (Info $ unwords ["not tuned in",t]) peer

         -- catch
         | otherwise = tell (Warning $ unwords [comm,"is not a command"]) peer

   -- peer update
   sync :: TVar Peer -> IO ()
   sync peer = do
      p :: Peer <- atomically $ readTVar peer
      x :: (TVar Peer,Signal) <- atomically $ readTQueue $ line p
      echo peer x
      -- alternative immediate flush of TQueue
      -- signals :: [Signal] <- atomically $ flushTQueue $ line peer  -- #flush

   -- unsignal , format output , echo out
   echo :: TVar Peer -> (TVar Peer,Signal) -> IO ()
   echo peer (from,signal)

      -- to target (always peer?)
      | Private _ target <- code signal , base signal = do
         t :: Peer <- atomically $ readTVar target
         r :: Int <- atomically $ trip <$> readTVar st
         l :: [TVar Peer] <- atomically $ list <$> readTVar st
         n :: [Peer] <- atomically $ mapM readTVar l

   --    sendTextData (conn t) $ clrt Green $ unwords [showt Base,showt $ text signal]
   --    sendTextData (conn t) $ unwords ["<span class=\"base\">" <> showt Base,showt (text signal) <> "</span>"]
         sendTextData (conn t) $ encodeToLazyText $ Echo
            { echo_type = "chat"
            , echo_base = base signal
            , echo_time = round $ (* 1000) $ utcTimeToPOSIXSeconds $ time signal :: Int
            , echo_chan = mempty
            , echo_nick = mempty
            , echo_flag = flag signal
            , echo_text = showt $ text signal
            , echo_list = nick <$> n
            }

      | Private t target <- code signal , from == peer = do
         p :: Peer <- atomically $ readTVar peer
         r :: Int <- atomically $ trip <$> readTVar st
         l :: [TVar Peer] <- atomically $ list <$> readTVar st
         n :: [Peer] <- atomically $ mapM readTVar l

   --    sendTextData (conn p) $ clrt Grey $ unwords [clrt Yellow $ cons privchar t , clrt Yellow $ nick p,showt $ text signal]
   --    sendTextData (conn p) $ unwords ["<span class=\"chan\">" <> cons privchar t <> "</span>","<span class=\"nick\">" <> nick p <> "</span>",showt $ text signal]
         sendTextData (conn p) $ encodeToLazyText $ Echo
            { echo_type = "chat"
            , echo_base = base signal
            , echo_time = round $ (* 1000) $ utcTimeToPOSIXSeconds $ time signal :: Int
            , echo_chan = cons privchar t
            , echo_nick = nick p
            , echo_flag = flag signal
            , echo_text = showt $ text signal
            , echo_list = nick <$> n
            }

      | Private _ target <- code signal = do
         f :: Peer <- atomically $ readTVar from
         t :: Peer <- atomically $ readTVar target
         r :: Int <- atomically $ trip <$> readTVar st

   --    sendTextData (conn t) $ clrt Grey $ unwords [clrt Yellow $ cons privchar $ nick f , clrt Yellow $ nick f,showt $ text signal]
   --    sendTextData (conn t) $ unwords ["<span class=\"chan\">" <> cons privchar (nick f)  <> "</span>","<span class=\"nick\">" <> nick f <> "</span>",showt $ text signal]
         sendTextData (conn t) $ encodeToLazyText $ Echo
            { echo_type = "chat"
            , echo_base = base signal
            , echo_time = round $ (* 1000) $ utcTimeToPOSIXSeconds $ time signal :: Int
            , echo_chan = cons privchar $ nick f
            , echo_nick = nick f
            , echo_flag = flag signal
            , echo_text = showt $ text signal
            , echo_list = mempty
            }

      -- to channel
      | Channel c <- code signal , base signal = do
         p :: Peer <- atomically $ readTVar peer
         r :: Int <- atomically $ trip <$> readTVar st

   --    sendTextData (conn p) $ clrt Green $ unwords [clrt Green $ cons chanchar c,clrt Green $ showt Base,showt $ text signal]
   --    sendTextData (conn p) $ unwords ["<span class=\"base\">" <> cons chanchar c,showt Base,showt (text signal) <> "</span>"]
         sendTextData (conn p) $ encodeToLazyText $ Echo
            { echo_type = "chat"
            , echo_base = base signal
            , echo_time = round $ (* 1000) $ utcTimeToPOSIXSeconds $ time signal :: Int
            , echo_chan = cons chanchar c
            , echo_nick = showt Base
            , echo_flag = flag signal
            , echo_text = showt $ text signal
            , echo_list = mempty
            }

      | Channel c <- code signal = do
         p :: Peer <- atomically $ readTVar peer
         f :: Peer <- atomically $ readTVar from
         r :: Int <- atomically $ trip <$> readTVar st

   --    sendTextData (conn p) $ clrt White $ unwords [clrt Yellow $ cons chanchar c,clrt Yellow $ nick f,showt $ text signal]
   --    sendTextData (conn p) $ unwords ["<span class=\"chan\">" <> cons chanchar c <> "</span>","<span class=\"nick\">" <> nick f <> "</span>",showt $ text signal]
         sendTextData (conn p) $ encodeToLazyText $ Echo
            { echo_type = "chat"
            , echo_base = base signal
            , echo_time = round $ (* 1000) $ utcTimeToPOSIXSeconds $ time signal :: Int
            , echo_chan = cons chanchar c
            , echo_nick = nick f
            , echo_flag = flag signal
            , echo_text = showt $ text signal
            , echo_list = mempty
            }

      -- to all
      | Broadcast <- code signal , base signal = do
         p :: Peer <- atomically $ readTVar peer
         r :: Int <- atomically $ trip <$> readTVar st
         l :: [TVar Peer] <- atomically $ list <$> readTVar st
         n :: [Peer] <- atomically $ mapM readTVar l

   --    sendTextData (conn p) $ clrt Green $ unwords [clrt Green $ showt Base,showt $ text signal]
   --    sendTextData (conn p) $ unwords ["<span class=\"base\">" <> showt Base,showt (text signal) <> "</span>"]
         sendTextData (conn p) $ encodeToLazyText $ Echo
            { echo_type = "chat"
            , echo_base = base signal
            , echo_time = round $ (* 1000) $ utcTimeToPOSIXSeconds $ time signal :: Int
            , echo_chan = mempty
            , echo_nick = showt Base
            , echo_flag = flag signal
            , echo_text = showt $ text signal
            , echo_list = nick <$> n
            }

      | Broadcast <- code signal = do
         p :: Peer <- atomically $ readTVar peer
         f :: Peer <- atomically $ readTVar from
         r :: Int <- atomically $ trip <$> readTVar st

   --    sendTextData (conn p) $ clrt White $ unwords [clrt Yellow $ cons freechar $ nick f,showt $ text signal]
   --    sendTextData (conn p) $ unwords ["<span class=\"nick\">" <> cons freechar (nick f) <> "</span>",showt $ text signal]
         sendTextData (conn p) $ encodeToLazyText $ Echo
            { echo_type = "chat"
            , echo_base = base signal
            , echo_time = round $ (* 1000) $ utcTimeToPOSIXSeconds $ time signal :: Int
            , echo_chan = mempty
            , echo_nick = cons freechar $ nick f
            , echo_flag = flag signal
            , echo_text = showt $ text signal
            , echo_list = mempty
            }

      | Internal <- code signal , Info "sign" <- text signal = do
         p :: Peer <- atomically $ readTVar peer
         r :: Int <- atomically $ trip <$> readTVar st
         l :: [TVar Peer] <- atomically $ list <$> readTVar st
         n :: [Peer] <- atomically $ mapM readTVar l

         sendTextData (conn p) $ encodeToLazyText $ Echo
            { echo_type = "sign"
            , echo_base = True
            , echo_time = round $ (* 1000) $ utcTimeToPOSIXSeconds $ time signal :: Int
            , echo_chan = mempty
            , echo_nick = nick p
            , echo_flag = flag signal
            , echo_text = mempty
            , echo_list = nick <$> n
            }

      | Internal <- code signal , Info ["ping",t] <- words <$> text signal = do
         p :: Peer <- atomically $ readTVar peer
         r :: Int <- atomically $ trip <$> readTVar st
         l :: [TVar Peer] <- atomically $ list <$> readTVar st
         n :: [Peer] <- atomically $ mapM readTVar l

         sendTextData (conn p) $ encodeToLazyText $ Echo
            { echo_type = "ping"
            , echo_base = base signal
            , echo_time = round $ (* 1000) $ utcTimeToPOSIXSeconds $ time signal :: Int
            , echo_chan = mempty
            , echo_nick = mempty
            , echo_flag = flag signal
            , echo_text = t
            , echo_list = nick <$> n
            }

      -- otherwise
      | otherwise = do
         logs $ Error $ unwords ["echo lost signal",showt signal]

      -- a more performant alternative would be
      -- sendTextDatas :: Connection -> [a] -> IO ()  -- #flush

   -- ACTIONS

   kill :: TVar Peer -> IO ()
   kill peer = do
      p :: Peer <- atomically $ readTVar peer
      logs $ Warning $ unwords ["kill",nick p]
      disconnect peer
      myThreadId >>= killThread

   -- graceful disconnect
   disconnect :: TVar Peer -> IO ()
   disconnect peer = do
      p :: Peer <- atomically $ readTVar peer
      atomically $ modifyTVar' st (\s -> s { list = filter (/= peer) (list s) , subs = filter (/= peer) <$> subs s })
      sendClose (conn p) $ unwords ["Killed by",nick p]
      u :: UTCTime <- getCurrentTime
      when (nick p /= anon) $
         pipe peer Signal
            { base = True
            , time = u
            , code = Broadcast
            , text = Info $ unwords ["←",nick p]
            }
      logs $ Info $ unwords ["disconnect",nick p]

   -- alter peer nick
   sign :: TVar Peer -> IO ()
   sign peer = do
      tell (Only "name?") peer
      signal :: Signal <- hear peer

      case (code signal,text signal) of

         (Pure,Only t) -> do
            v :: Bool <- atomically $ valid t
            if not v then do
               tell (Info "invalid name") peer
               sign peer
            else do
               l :: [TVar Peer] <- atomically $ list <$> readTVar st
               x :: [TVar Peer] <- atomically $ filterM ((((== t) . nick) <$>) . readTVar) l
               if not $ null x then do
                  tell (Info $ unwords [t,"is already taken!"]) peer
                  sign peer
               else do
                  atomically $ modifyTVar' peer (\n -> n { nick = t })
                  u :: UTCTime <- getCurrentTime
                  pipe peer Signal
                     { base = True
                     , time = u
                     , code = Internal
                     , text = Info "sign"
                     }
                  tell (Info $ unwords ["you are",t]) peer

         _ -> do
            pipe peer signal

   -- do after every ping
   ping :: TVar Peer -> Int -> Bool -> IO ()
   ping peer t silent = do
      if silent then do
         pure ()
      else do
         u :: UTCTime <- getCurrentTime
         pipe peer Signal
            { base = True
            , time = u
            , code = Internal
            , text = Info $ unwords ["ping",showt t]
            }

   -- UTILITY

   -- send signal from server to peer
   tell :: Flag Text -> TVar Peer -> IO ()
   tell t peer = do
      u :: UTCTime <- getCurrentTime
      pipe peer Signal
         { base = True
         , time = u
         , code = Private mempty peer
         , text = t
         }

   -- validate nick
   valid :: Text -> STM Bool
   valid t = do
      pure $ and
         [ t /= mempty
         , t /= anon
         , t /= "jrvieira"
         , compareLength t 12 == LT
         , all isAlphaNum $ unpack t
         ]

   tune :: TVar Peer -> Text -> STM ()
   tune peer c
      | compareLength c 12 == LT = do
         modifyTVar' st (\s -> s { subs = Map.insertWith (<>) c [peer] $ subs s })

   mute :: TVar Peer -> Text -> STM ()
   mute peer c = do
      modifyTVar' st (\s -> s { subs = Map.adjust (filter (/= peer)) c $ subs s })

utct :: UTCTime -> Text
utct = clrt Grey . pack . formatTime defaultTimeLocale "%H:%M:%S"

logs :: Flag Text -> IO ()
logs f = do
   u :: UTCTime <- getCurrentTime
   putStrLn $ unwords [utct u,x]
      where
      x
         | Noise     <- f = clrt Grey $ unwords ["..."]
         | Only t    <- f = clrt Grey t
         | Info t    <- f = clrt Blue $ unwords ["INF",t]
         | Warning t <- f = clrt Yellow $ unwords ["WRN",t]
         | Error t   <- f = clrt Red $ unwords ["ERR",t]

flag :: Signal -> Text
flag s
   | Noise <- text s = "noise"
   | Only _ <- text s = "only"
   | Info _ <- text s = "info"
   | Warning _ <- text s = "warning"
   | Error _ <- text s = "error"
   | otherwise = mempty
