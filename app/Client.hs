module Main ( main ) where

import Prelude hiding ( getLine, putStrLn, unwords, words )
import Network.WebSockets
import Wuss
import System.IO hiding ( putStrLn, getLine )
import System.Console.Readline ( readline, addHistory )
import System.Console.ANSI ( clearScreen, cursorUpLine, clearLine )
import Control.Concurrent.Async  ( withAsync )
import Control.Monad ( forever, unless )
import Data.Text ( Text, unpack, strip, unwords )
import Data.Text.IO ( getLine, putStrLn )
import Data.Aeson

import Zero.Color
import Types

default ([], Word, Text)

main :: IO ()
main = do
   hSetBuffering stdout NoBuffering
   clearScreen
   runSecureClient "chat.jrvieira.com" 443 "/sock" app

app :: Connection -> IO ()
app connection = do

   withAsync (forever send) $ \_ -> forever $ do
      receiveData connection >>= echo

   sendClose connection "app client out"

   where

   send :: IO ()
   send = do
      hSetEcho stdin True
      prompt :: Text <- getLine
      cursorUpLine 1
      clearLine
      hSetEcho stdin False
      unless (strip prompt == mempty) $ do
         addHistory $ unpack prompt
         sendTextData connection prompt

   echo :: Text -> IO ()
   echo = go . decodeStrictText
      where
      go :: Maybe Echo -> IO ()
      go Nothing = error "no parse"
      go (Just e)
         | "chat" <- echo_type e = putStrLn line
         | otherwise = pure ()
         where
         line = strip $ unwords [bold $ strip $ unwords [echo_chan e, echo_nick e],f $ echo_text e]
         f
            | echo_base e = clrt Dim
            | otherwise = id
         bold t
            | "" <- t = ""
            | otherwise = clrt Magenta $ t
