{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy (ByteString, empty, length)
import Data.ByteString.Lazy.Char8 (splitWith, split)
import qualified Data.ByteString.UTF8 as BS (toString, fromString)
import Data.ByteString.Lazy.UTF8 (toString, fromString)
import Data.ByteString.Lazy.Search (breakOn, breakAfter)
import Data.List (isPrefixOf)
import GHC.Generics (Generic)
import Network.Run.TCP (runTCPServer)
import Network.Socket.ByteString.Lazy (recv, sendAll)
import Prelude hiding (length, id)
import System.Environment (getEnv)

main :: IO ()
main = do
  let port = "8080"
  putStrLn $ "http://localhost:"<>port
  runTCPServer Nothing port talk
  where
  talk s = do
    msg <- recv s 1024
    secret <- getEnv "botsecret"
    let mp = fst $ breakOn (BS.fromString " HTTP/") msg
    let res =
          if mp == (fromString $ "POST /bot"<>secret)
            then response $ process $ snd $ breakAfter (BS.fromString "\r\n\r\n") msg
            else response $ empty
    sendAll s res
    talk s

data In = In
  { message :: Message
  } deriving Generic

data Message = Message
  { chat :: Chat
  , text :: String
  } deriving Generic

data Chat = Chat
  { id :: Int
  } deriving Generic

instance FromJSON Chat
instance FromJSON Message
instance FromJSON In

process :: ByteString -> ByteString
process x =
  case decode x of
    Nothing -> empty
    Just In { message=Message{text="/start", chat=Chat{id=id}}} -> tgMsg "Type a keyword, please." id
    Just In { message=Message{text=    text, chat=Chat{id=id}}} -> tgMsg text id

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
  \Content-Type: application/json\r\n\
  \Content-Length: "<> (show $ length x) <>"\r\n\
  \\r\n\
  \") <> x
