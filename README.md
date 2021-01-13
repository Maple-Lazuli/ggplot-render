See https://github.com/virtualstaticvoid/heroku-buildpack-r/issues/107

```bash
heroku create --stack heroku-16

heroku buildpacks:add https://github.com/virtualstaticvoid/heroku-buildpack-r.git#heroku-16

git push heroku master

heroku logs # to check

curl -v -L https://your-heroku-app-name.heroku.com/mean
curl -v -d "a=1" -d "b=2" -L https://your-heroku-app-name.herokuapp.com/sum
```
