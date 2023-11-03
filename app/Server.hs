module Main ( main ) where

import Prelude hiding ( putStrLn, unwords, words )
import Data.Text ( Text, unwords, words, uncons, cons, pack, unpack )
import Data.Text.IO ( putStrLn )
import TextShow ( TextShow, showt )
import Network.WebSockets
import Control.Concurrent ( myThreadId, killThread )
import Control.Concurrent.Async
import Control.Concurrent.STM
import System.Console.ANSI ( clearScreen )
import Control.Exception
import Control.Monad
import Data.Char
import Data.Time
import Data.Set ( Set )
import Data.Set qualified as Set ()
import Data.Map.Strict ( Map )
import Data.Map.Strict qualified as Map ( insertWith, adjust, filter, keys, findWithDefault )

import Zero
import Zero.Color

default ([], Word, Text)

data State = State
   { list :: [TVar Node]
   , subs :: Map Text [TVar Node]
-- , mode :: Mode
   }

-- -- TODO: Mark + Mode = modes limit groups of people in a given channel
-- -- roles
-- data Mark
--    = None
--    | Some Word
--
-- data Mode = Mode
--    { sign :: Bool  -- can change name
--    , mute :: [Mark] -- muted
--    , deaf :: [Mark] -- cannot join the channel
--    }

data Node = Base { name :: Text } | Peer
   { name :: Text
   , conn :: Connection
   , line :: TQueue Signal
   , open :: UTCTime
   }

instance Show Node where
   show = unpack . name

instance TextShow Node where
   showt = name

-- payload capsule
data Flag a = Noise | Only a | Info a | Warning a | Error a
   deriving ( Show, Functor )

instance Show a => TextShow (Flag a) where
   showt = pack . show

-- signal code
data Code
   = Pure
   | Internal
   | Private [TVar Node]
   | Channel Text
   | Broadcast
   | Command Text

instance TextShow Code where
   showt Pure = "___"
   showt Internal = "INT"
   showt (Private _) = "PVT"
   showt (Channel _) = "CHN"
   showt Broadcast = "BDC"
   showt (Command _) = "CMD"

data Signal = Signal
   { sent :: TVar Node
   , time :: UTCTime
   , code :: Code
   , text :: Flag Text
   , self :: Bool
   }

instance TextShow Signal where
   showt s = unwords ["Σ",showt $ code s,showt $ text s]

-- CONFIG

anon :: Text
anon = "_"

commchar :: Char
commchar = ':'

privchar :: Char
privchar = '@'

chanchar :: Char
chanchar = '#'

main :: IO ()
main = do

   clearScreen >> logs (Only "main node up")

   base :: TVar Node <- atomically $ newTVar Base { name = "▲" }

   st :: TVar State <- atomically $ newTVar State
      { list = mempty
      , subs = mempty
      }

   runServer "127.0.0.1" 8080 $ app st base

{- TODO:

-- PHASE NIL

   signal abstraction ( like tell ) , styler :: Signal -> Text
   run in server

   abstract echo stylings (preferably completely separate logic from render)

   Mode + Mark

-- PHASE I

   log to file
   user registration
   separate logic from interface
   grey on disconnect vvv

-- PHASE II

   Chat + Game modules

   make chat as page

   create game logic
   create gameplay protocol
   multiple tables as people join
   create render
   make ws page ( join with chat )

-}

-- app thread on connection
app :: TVar State -> TVar Node -> PendingConnection -> IO ()
app st base pending = do

   -- create pipeline
   connection :: Connection <- acceptRequest pending
   queue :: TQueue Signal <- atomically newTQueue

   -- add peer to state
   u :: UTCTime <- getCurrentTime
   peer :: TVar Node <- atomically $ new connection queue u

   logs $ Info $ unwords ["app connect"]

   -- ask and set name
   withAsync (forever $ sync peer) $ \_ ->
      withPingThread connection 30 (ping peer) $
      sign peer

   p :: Node <- atomically $ readTVar peer

   when (mempty == name p || anon == name p) $ kill peer

   logs $ Info $ unwords ["app sign as",name p]

   pipe peer Signal
      { sent = base
      , time = u
      , code = Broadcast
      , text = Info $ unwords ["->",name p]
      , self = False
      }

   -- io loop with sync threadfull
   void $ withAsync (forever $ sync peer) $ \_ ->
      withPingThread connection 30 (ping peer) $
         forever $ hear peer >>= pipe peer

   -- failsafe
   logs $ Error "app hit bottom"
   kill peer

   where

   -- create a new node
   new :: Connection -> TQueue Signal -> UTCTime -> STM (TVar Node)
   new connection queue u = do
      node :: TVar Node <- newTVar $ Peer { name = anon , conn = connection , line = queue , open = u }
      modifyTVar' st (\s -> s { list = node : list s })
      pure node

   -- IO
   -- hear >>= pipe --> transmit ))) sync --> echo
   --               (((
   --               ))) st

   -- wait for one incoming message
   hear :: TVar Node -> IO Signal
   hear peer = do
      p :: Node <- atomically $ readTVar peer
      i :: Either SomeException Text <- try $ receiveData (conn p)
      u :: UTCTime <- getCurrentTime
      case i of
         Right t -> do
            pure Signal
               { sent = peer
               , time = u
               , code = Pure
               , text = Only t
               , self = False
               }
         Left e -> do
            pure Signal
               { sent = peer
               , time = u
               , code = Command "kill"
               , text = Warning $ showt e
               , self = False
               }

   -- process data: assign signal code and relay
   pipe :: TVar Node -> Signal -> IO ()
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
      | Internal <- code signal = logs $ ("pipe internal: " <>) <$> text signal
      -- private message to list of nodes
      | Private c <- code signal = mapM_ atomically $ transmit signal <$> c
      -- message to channel
      | Channel c <- code signal = do
         s :: Map Text [TVar Node] <- atomically $ subs <$> readTVar st
         mapM_ atomically $ transmit signal <$> [peer] ∪ Map.findWithDefault mempty c s
      -- send to all nodes
      | Broadcast <- code signal = atomically $ mapM_ (transmit signal) . list =<< readTVar st
      -- run command
      | Command c <- code signal = command $ words c

      where

      -- add signal to node queue
      transmit :: Signal -> TVar Node -> STM ()
      transmit s target = do
         t :: Node <- readTVar target
         writeTQueue (line t) s { self = peer == target || self s }

      -- COMMANDS

      command [] = tell (Info "empty command") $ sent signal
      command (comm : arg)

         -- test command
         | "test" <- comm = logs $ Info $ unwords $ "pipe test command" : arg

         -- change name
         | "sign" <- comm , [] <- arg = sign $ sent signal
         | "sign" <- comm = tell (Info "command :sign takes no arguments") $ sent signal

         -- list peers
         | "list" <- comm , [] <- arg = do
            l :: [TVar Node] <- atomically $ list <$> readTVar st
            n :: [Node] <- atomically $ mapM readTVar l
            u :: UTCTime <- getCurrentTime
            pipe peer Signal
               { sent = base
               , time = u
               , code = Private $ pure $ sent signal
               , text = Only $ unwords $ name <$> n
               , self = False
               }
         | "list" <- comm = tell (Info "command :list takes no arguments") $ sent signal

         -- kill command
         | "kill" <- comm , [] <- arg = kill peer
         | "kill" <- comm = tell (Info "command :kill takes no arguments") $ sent signal

         -- send private message
         | "priv" <- comm , [] <- arg = tell (Info "command :priv takes at lest one argument") $ sent signal
         | "priv" <- comm , t : x <- arg = do
            v :: Bool <- atomically $ valid t
            if not v then do
               tell (Info $ unwords [t,"not here"]) $ sent signal
            else do
               l :: [TVar Node] <- atomically $ list <$> readTVar st
               n :: [TVar Node] <- atomically $ filterM (((== t) . name <$>) . readTVar) l
               if null n then do
                  tell (Info $ unwords [t,"not found"]) $ sent signal
               else do
                  -- send to target
                  pipe peer signal
                     { code = Private $ [sent signal] ∪ n
                     , text = Only $ unwords x
                     }

         -- list channel subscriptions
         | "subs" <- comm , [] <- arg = do
            m :: Map Text [TVar Node] <- atomically $ subs <$> readTVar st
            tell (Only $ unwords $ Map.keys $ Map.filter (peer ∈) m) peer
         | "subs" <- comm = tell (Info "command :subs takes no arguments") $ sent signal

         -- subscribe to channel
         | "tune" <- comm , [] <- arg = tell (Info "command :tune takes at lest one argument") $ sent signal
         | "tune" <- comm = atomically $ mapM_ (tune peer) arg

         -- unsubscribe to channel
         | "mute" <- comm , [] <- arg = tell (Info "command :mute takes at lest one argument") $ sent signal
         | "mute" <- comm = atomically $ mapM_ (mute peer) arg

         -- send chan message
         | "chan" <- comm , [] <- arg = tell (Info "command :chan takes at lest one argument") $ sent signal
         | "chan" <- comm , t : x <- arg = do
            pipe peer signal
               { code = Channel t
               , text = Only $ unwords x
               }

         -- catch
         | otherwise = tell (Warning $ unwords [comm,"is not a command"]) $ sent signal

   -- peer update
   sync :: TVar Node -> IO ()
   sync peer = do
      p :: Node <- atomically $ readTVar peer
      signal :: Signal <- atomically $ readTQueue $ line p
      echo (name p) signal peer
      -- alternative immediate flush of TQueue
      -- signals :: [Signal] <- atomically $ flushTQueue $ line peer  -- #flush

   -- send to peer
   -- unsignal and format output
   echo :: Text -> Signal -> TVar Node -> IO ()
   echo w signal target

      -- to server
      | base == target = do
         logs $ Only $ unwords ["echo",w,">",showt $ text signal]

      -- lost signal
      | Pure <- code signal = do
         s :: Node <- atomically $ readTVar $ sent signal
         t :: Node <- atomically $ readTVar target
         logs $ Error $ unwords ["echo pure",name s,"-->",name t,":",showt $ text signal]

      -- to peer
      | Private _ <- code signal , base == sent signal = do
         s :: Node <- atomically $ readTVar $ sent signal
         t :: Node <- atomically $ readTVar target
         sendTextData (conn t) $ clrt Green $ unwords [name s,showt $ text signal]

      | Private _ <- code signal = do
         s :: Node <- atomically $ readTVar $ sent signal
         t :: Node <- atomically $ readTVar target
         sendTextData (conn t) $ clrt Grey $ unwords [clrt Yellow $ [privchar] <> name s,showt $ text signal]

      -- to channel
      | Channel c <- code signal , base == sent signal = do
         s :: Node <- atomically $ readTVar $ sent signal
         t :: Node <- atomically $ readTVar target
         sendTextData (conn t) $ clrt Green $ unwords [clrt Green $ cons chanchar c,clrt Green $ name s,showt $ text signal]

      | Channel c <- code signal = do
         s :: Node <- atomically $ readTVar $ sent signal
         t :: Node <- atomically $ readTVar target
         sendTextData (conn t) $ clrt White $ unwords [clrt Yellow $ cons chanchar c,clrt Yellow $ name s,showt $ text signal]

      -- to all
      | Broadcast <- code signal , base == sent signal = do
         s :: Node <- atomically $ readTVar $ sent signal
         t :: Node <- atomically $ readTVar target
         sendTextData (conn t) $ clrt Green $ unwords [clrt Green $ name s,showt $ text signal]

      | Broadcast <- code signal = do
         s :: Node <- atomically $ readTVar $ sent signal
         t :: Node <- atomically $ readTVar target
         sendTextData (conn t) $ clrt White $ unwords [clrt Yellow $ name s,showt $ text signal]

      -- otherwise
      | otherwise = do
         logs $ Error $ unwords ["echo lost signal",showt signal]

      -- a more performant alternative would be
      -- sendTextDatas :: Connection -> [a] -> IO ()  -- #flush

   -- ACTIONS

   kill :: TVar Node -> IO ()
   kill peer = do
      p :: Node <- atomically $ readTVar peer
      logs $ Warning $ unwords ["kill",name p]
      disconnect peer
      myThreadId >>= killThread

   -- graceful disconnect
   disconnect :: TVar Node -> IO ()
   disconnect peer = do
      p :: Node <- atomically $ readTVar peer
      atomically $ modifyTVar' st (\s -> s { list = filter (/= peer) (list s) , subs = filter (/= peer) <$> subs s })
      sendClose (conn p) $ unwords ["close connection",name p]
      logs $ Info $ unwords ["disconnect",name p]

   -- alter node name
   sign :: TVar Node -> IO ()
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
               l :: [TVar Node] <- atomically $ list <$> readTVar st
               x :: [TVar Node] <- atomically $ filterM ((((== t) . name) <$>) . readTVar) l
               if not $ null x then do
                  tell (Info $ unwords [t,"is already taken!"]) peer
                  sign peer
               else do
                  atomically $ modifyTVar' peer (\n -> n { name = t })
                  tell (Info $ unwords ["you are",t]) peer

         _ -> do
            pipe peer signal

   -- do after every ping
   ping :: TVar Node -> IO ()
   ping peer = do
      p :: Node <- atomically $ readTVar peer
      logs $ Only $ unwords ["ping",name p]
   -- logs $ Noise

   -- UTILITY

   -- send signal from server to peer
   tell :: Flag Text -> TVar Node -> IO ()
   tell t peer = do
      u :: UTCTime <- getCurrentTime
      pipe peer Signal
         { sent = base
         , time = u
         , code = Private $ pure peer
         , text = t
         , self = False
         }

   -- validate name
   valid :: Text -> STM Bool
   valid t = do
      b :: Node <- readTVar base
      pure $ and
         [ t /= mempty
         , t ∉ [name b,anon]
         , all isAlphaNum $ unpack t
         ]

   tune :: TVar Node -> Text -> STM ()
   tune peer c = do
      modifyTVar' st (\s -> s { subs = Map.insertWith (<>) c [peer] $ subs s })

   mute :: TVar Node -> Text -> STM ()
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

