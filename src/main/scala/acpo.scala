import zio._

val acpoHtml = Chunk.fromArray("""<!doctype html>
<html lang="uk">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>АЦПО</title>
  <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.0.0-beta1/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-giJF6kkoqNQ00vy+HMDP7azOuL0xtbfIcaT9wjKHr8RbDVddVHyTfAAsrekwKmP1" crossorigin="anonymous">
</head>
<body>
  <div class="container">
    <h1>Інтеграція АЦПО з Telegram</h1>
    <div class="row row-cols-lg-auto g-3 align-items-center">
      <div class="col-12">
        <input placeholder="Логін" id="login" class="form-control" autocomplete="off"/>
      </div>
      <div class="col-12">
        <input placeholder="Пароль" id="password" type="password" class="form-control"/>
      </div>
      <div class="col-12">
        <button id="submit" class="btn btn-primary">Зв'язати</button>
      </div>
    </div>
    <script>
      let id = document.getElementById
      id("submit").addEventListener("click", () => {
        fetch("https://portal.acpo.com.ua/account/login", {
          method: "POST"
        , body:
            JSON.stringify({
              web_login: id("login").value
            , web_pass: id("password").value
            })
        }).then(data => data.json().then(res => {
          let id = new URL(document.location).searchParams.get("id")
          fetch(`/acpo_link?id=${id}&fiz_id=${res.fiz_id}&token=${res.token}`, {
            method: "POST"
          }).then(() => location.replace(`https://t.me/tellnobodybot?start=${id}`))
        }))
      })
    </script>
  </div>
</body>
</html>""".getBytes("utf8").nn)