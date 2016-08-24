# Hexl

## Preps

``` shell
npm install -g webpack gulp
```

## Dev

Run in background:

``` shell
cd hexl
gulp
webpack --progress --colors --watch
```

Build:

``` shell
cd hexl
# Server
./build-server.sh
# Client
./build-client.sh
```

Then go to [localhost:3000](http://localhost:3000).
