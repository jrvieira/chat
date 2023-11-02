module Main ( main ) where

import Prelude hiding ( getLine, putStrLn, unwords, words )
import Network.WebSockets
import Network.Socket ( withSocketsDo )
import Control.Concurrent.Async  ( withAsync )
import Control.Monad ( forever, unless )
import Control.Monad.IO.Class ( liftIO )
import Data.Text ( Text )
import Data.Text.IO ( getLine, putStrLn )

default ([], Word, Text)

main :: IO ()
main = withSocketsDo $ runClient "127.0.0.1" 8080 "/" app

app :: Connection -> IO ()
app connection = do

   withAsync (forever sync) $ \_ -> forever $ do
      receiveData connection >>= putStrLn

   sendClose connection "app client out"

   where

   sync = do
      line <- getLine
      unless ("" == line) $ sendTextData connection line
