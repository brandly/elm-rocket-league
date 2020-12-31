# elm-rocket-league

### development

build everything into dist:

```
$ make
```

you'll want some server to deliver static assets:

```
$ python -m SimpleHTTPServer
$ open http://localhost:8000/dist/
```

[Watchman](https://github.com/facebook/watchman) is really good. run this to keep `dist/` up to date as you write code:

```
$ watchman-make -p src/**/* -t all
```

![dependencies](https://brandly.github.io/elm-rocket-league/dependency-graph.svg)
