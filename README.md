# bot

[![tg](https://img.shields.io/badge/telegram-bot-blue)](https://t.me/tellnobodybot)

```sh
export botsecret=123
sbt> run
export botport=8080
sbt> deploySsh server
```

## tests

```sh
curl -i --request POST http://localhost:8002/bot/123 -d '{"message":{"chat":{"id":1},"text":"/start"}}'
curl -i --request POST http://localhost:8002/bot/123 -d '{"message":{"chat":{"id":1},"text":"acpo/login"}}'
curl -i --request POST http://localhost:8002/bot/123 -d '{"message":{"chat":{"id":1},"text":"acpo/get"}}'
```
