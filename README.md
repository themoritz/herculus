# Hexl

## Preps

Works with npm v2.15.1 and node v4.4.3.
Use `nvm install v.4.4.3`.

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
