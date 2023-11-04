module Types where

import Network.WebSockets
import Control.Concurrent.STM
import Data.Text
import Data.Time
import Data.Map.Strict

import TextShow

data State = State
   { list :: [TVar Peer]
   , subs :: Map Text [TVar Peer]
-- , mode :: Mode
   }

-- -- TODO: Mark + Mode = modes limit groups of people in a given channel
-- -- roles
-- data Mark
--    = None
--    | Some Word
--
-- data Mode = Mode
--    { sign :: Bool  -- can change nick
--    , mute :: [Mark] -- muted
--    , deaf :: [Mark] -- cannot join the channel
--    }

data Node
   = Base
   | User (TVar Peer)
-- | Chan (Text,[TVar Peer])
   deriving Eq

instance TextShow Node where
   showb Base = ""
   showb (User _) = "<peer>"

data Peer = Peer
   { nick :: Text
   , conn :: Connection
   , line :: TQueue (TVar Peer,Signal)
   , open :: UTCTime
   }

-- payload capsule
data Flag a = Noise | Only a | Info a | Warning a | Error a
   deriving ( Show, Functor )

instance TextShow (Flag Text) where
   showb Noise = "Noise"
   showb (Only a) = showb a
   showb (Info a) = showb a
   showb (Warning a) = showb a
   showb (Error a) = showb a

-- signal code
data Code
   = Pure
   | Internal
   | Private (TVar Peer)
   | Channel Text
   | Broadcast
   | Command Text

instance TextShow Code where
   showb Pure = "___"
   showb Internal = "INT"
   showb (Private _) = "PVT"
   showb (Channel _) = "CHN"
   showb Broadcast = "BDC"
   showb (Command _) = "CMD"

data Signal = Signal
   { base :: Bool
   , time :: UTCTime
   , code :: Code
   , text :: Flag Text
   }

instance TextShow Signal where
   showb s = "Σ" <> showb (code s) <> showb (text s)

