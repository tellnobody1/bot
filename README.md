# bot

```
export botsecret=123
stack run
echo -ne 'POST /bot123 HTTP/1.0\r\n\r\n{"message":"test","chat":{"id":1}}' | nc localhost 3000
```