{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Control.Monad (unless)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy (ByteString, empty, length, null, readFile, writeFile)
import Data.ByteString.Lazy.Char8 (splitWith, split, stripPrefix)
import Data.ByteString.Lazy.Search (breakOn, breakAfter)
import Data.ByteString.Lazy.UTF8 (toString, fromString)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, fromMaybe)
import GHC.Generics (Generic)
import Network.Run.TCP (runTCPServer)
import Network.Socket.ByteString.Lazy (recv, sendAll)
import Prelude hiding (length, id, null, readFile)
import qualified Data.ByteString.UTF8 as BS (toString, fromString)
import System.Environment (getEnv)

main :: IO ()
main = do
  let port = "8080"
  putStrLn $ "http://localhost:"<>port
  runTCPServer Nothing port talk
  where
  talk s = do
    payload <- recv s 4096
    unless (null payload) $ do
      secret <- getEnv "botsecret"
      req <- parseReq payload
      res <- case req of
        Req { method="GET", path="/acpo" } -> do
          html <- acpoHtml
          pure $ response "text/html" html
        Req { method="POST", path="/acpo_link", params=params } -> do
          Just 
          -- Just find (\(k,v)->k=="id")
          -- sequence $ map (\(k, v) -> writeFile ("data/"<>k) v) params
          pure $ response "text/plain" empty
        Req { method="POST", path=path, body=body } ->
          if path == "/bot"<>secret
          then pure $ response "application/json" $ process body
          else pure notfound
        _ ->
          pure notfound
      sendAll s $ res
      talk s

data Req = Req
  { method :: String
  , path :: String
  , params :: [(String, ByteString)]
  , body :: ByteString
  } deriving Show

parseReq :: ByteString -> IO Req
parseReq payload = do
  let body = snd $ breakAfter' "\r\n\r\n" payload
  let mq = breakOn' " " $ fst $ breakOn' " HTTP/" payload
  let method = toString $ fst mq
  let query = snd mq
  let pp = breakOn' "?" query
  let path = stripPrefix' " " $ fst pp
  let params = catMaybes $ map (\x -> case x of
        [a, b] -> Just (toString a, b)
        _ -> Nothing) $ map (split '=') $ split '&' $ stripPrefix'' "?" $ snd pp
  pure $ Req
    { method=method
    , path=path
    , params=params
    , body=body
    }
  where
  breakAfter' :: String -> ByteString -> (ByteString, ByteString)
  breakAfter' sep x = breakAfter (BS.fromString sep) x
  breakOn' :: String -> ByteString -> (ByteString, ByteString)
  breakOn' sep x = breakOn (BS.fromString sep) x
  stripPrefix' :: String -> ByteString -> String
  stripPrefix' prefix x = toString $ fromMaybe empty $ stripPrefix (fromString prefix) x
  stripPrefix'' :: String -> ByteString -> ByteString
  stripPrefix'' prefix x = fromMaybe empty $ stripPrefix (fromString prefix) x

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

acpoHtml :: IO ByteString
acpoHtml = readFile "html/acpo.html"

response :: String -> ByteString -> ByteString
response ct body = fromString (
  "HTTP/1.1 200 OK\r\n\
  \Content-Type: "<>ct<>"\r\n\
  \Content-Length: "<> (show $ length body) <>"\r\n\
  \\r\n\
  \") <> body

notfound :: ByteString
notfound = fromString (
  "HTTP/1.1 404 Not Found\r\n\
  \Content-Length: 0\r\n\
  \\r\n\
  \")
