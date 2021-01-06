{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy (ByteString, empty, length)
import Data.ByteString.Lazy.Char8 (lines)
import Data.ByteString.Lazy.UTF8 (toString, fromString)
import Data.List (isPrefixOf)
import GHC.Generics (Generic)
import Network.Run.TCP (runTCPServer)
import Network.Socket.ByteString.Lazy (recv, sendAll)
import Prelude hiding (lines, length, id)
import System.Environment (getEnv)

main :: IO ()
main = do
  let port = "3000"
  putStrLn $ "http://localhost:"<>port
  runTCPServer Nothing port talk
  where
  talk s = do
    msg <- recv s 1024
    let ls = lines msg
    putStrLn $ toString $ head ls
    putStrLn $ toString $ last ls
    tgh <- tgHeader
    let res =
          if tgh == head ls then response $ process $ last ls
          else                   response $ empty
    sendAll s res
    talk s

data In = In
  { message :: String
  , chat :: Chat
  } deriving Generic

data Chat = Chat
  { id :: Int
  } deriving Generic

instance FromJSON Chat
instance FromJSON In

process :: ByteString -> ByteString
process x =
  case decode x of
    Nothing -> empty
    Just In { message="/start", chat=Chat{id=id} } -> tgMsg "Type a keyword, please." id
    Just In { message= message, chat=Chat{id=id} } -> tgMsg message id

data Out = Out
  { chat_id :: Int
  , method :: String
  , text :: String
  } deriving Generic

instance ToJSON Out

msgOut text chat_id = Out { chat_id=chat_id, method="sendMessage", text=text }

tgMsg :: String -> Int -> ByteString
tgMsg msg chat_id = encode $ msgOut msg chat_id

response :: ByteString -> ByteString
response x = fromString (
  "HTTP/1.1 200 OK\r\n\
  \Content-type: application/x-www-form-urlencoded\r\n\
  \Content-Length: "<> (show $ length x) <>"\r\n\
  \\r\n\
  \") <> x

tgHeader :: IO ByteString
tgHeader = do
  secret <- getEnv "botsecret"
  pure $ fromString $ "POST /bot"<>secret<>" HTTP/1.0\r"