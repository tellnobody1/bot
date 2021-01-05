module Main where

import Control.Monad (when)
import Data.ByteString (ByteString, empty)
import qualified Data.ByteString as BS (length)
import Network.Socket.ByteString (recv, sendAll)
import Network.Run.TCP (runTCPServer)
import System.Environment (getEnv)
import Data.ByteString.UTF8 (toString, fromString)
import Data.List (isPrefixOf)
import Data.ByteString.Char8 (split)

main :: IO ()
main = do
  let port = "3000"
  putStrLn $ "http://localhost:"<>port
  runTCPServer Nothing port talk
  where
  talk s = do
    msg <- recv s 1024
    let ls = split '\n' msg
    putStrLn $ toString $ head ls
    putStrLn $ toString $ last ls
    tgh <- tgHeader
    let res =
          if tgh == head ls then response $ last ls
          else                   response empty
    sendAll s res
    talk s

response :: ByteString -> ByteString
response x = fromString (
  "HTTP/1.1 200 OK\r\n\
  \Content-type: application/x-www-form-urlencoded\r\n\
  \Content-Length: "<> (show $ BS.length x) <>"\r\n\
  \\r\n\
  \") <> x

tgHeader :: IO ByteString
tgHeader = do
  secret <- getEnv "botsecret"
  pure $ fromString $ "POST /bot"<>secret<>" HTTP/1.1\r"