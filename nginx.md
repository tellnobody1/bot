# nginx

```
certbot certonly --standalone
```

```bash
sudo touch /etc/nginx/sites-available/bot.tellnobody.space
sudo vim /etc/nginx/sites-available/bot.tellnobody.space
```

```
server {
  listen 443 ssl;
  server_name bot.tellnobody.space;
  ssl_certificate /etc/letsencrypt/live/bot.tellnobody.space/fullchain.pem;
  ssl_certificate_key /etc/letsencrypt/live/bot.tellnobody.space/privkey.pem;

  location / {
    proxy_pass http://127.0.0.1:8002;
  }
}
```

```bash
sudo ln -s /etc/nginx/sites-available/bot.tellnobody.space /etc/nginx/sites-enabled/
```

```bash
sudo nginx -t
sudo systemctl restart nginx
```
