# Hexl

## Preps

Install globally

``` shell
npm install -g webpack gulp
```

## Dev

Run in background:

``` shell
cd hexl
npm install
# optional gulp for live css injection
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

Then go to [localhost:3001](http://localhost:3001).
