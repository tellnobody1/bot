# bot

```sh
export botsecret=123
sbt run
```

## tests

```sh
curl -i --request POST http://localhost:8002/bot/123 -d '{"message":{"chat":{"id":1},"text":"/start"}}'
curl -i --request POST http://localhost:8002/bot/123 -d '{"message":{"chat":{"id":1},"text":"acpo"}}'
```