      res <- case req of
        Req { method="POST", path=path, body=body } ->
          if path == "/bot"<>secret
          then pure $ response "application/json" $ process body
          else pure notfound

data Req = Req
  { method :: String
  , path :: String
  , params :: [(String, ByteString)]
  , body :: ByteString
  } deriving Show

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

msgOut text chat_id = Out { chat_id=chat_id, method="sendMessage", text=text }

tgMsg :: String -> Int -> ByteString
tgMsg msg chat_id = encode $ msgOut msg chat_id
