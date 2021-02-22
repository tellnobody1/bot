# bot

```sh
export botsecret=123
sbt run
```

## tests

```sh
curl -i --http1.0 --request POST http://localhost:8080/bot/123 -d '{"message":{"chat":{"id":1},"text":"telegram"}}'
```