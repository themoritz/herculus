# Herculus PureScript Frontend

This code is still written to work with an outdated version of PureScript. Therefore
the build and deployment process is not very streamlined.

All the Nix build stuff is deprecated.

## Developing

* Download the old binary of the PureScript compiler and install it on your path. For example:

    ```bash
    wget https://github.com/purescript/purescript/releases/download/v0.11.7/linux64.tar.gz
    tar -xvf linux64.tar.gz
    mv purescript/* $HOME/.local/bin
    ```

* Install `npm`.
* Install node and bower dependencies:
    ```bash
    npm install
    npm install
    ./node_modules/bower/bin/bower install
    ```

* Generate the PureScript Json and Rest related code from the Haskell API. There is an
  incompatability between the PureScript dependencies and `purescript-bridge` which is
  used for the code generation. Therefore, the generated code needs to be patched.

    ```bash
    cabal run psc-gen -- -t ./src
    patch src/lib/Api/Rest.purs Rest.purs.patch
    ```

* Now you can use npm/webpack to develop the client:

    ```bash
    npm run watch
    ```

  The command will start a development server at `localhost:3001`. Whenever you change
  the code, the server will automatically rebuild the client and refresh the browser.
  Api requests are proxied to `localhost:3000`, so make sure the server is running with
  that port configured (by running `cabal run server:server-exe`).

## Deployment

If necessary, change the `API_URL` and `WEBSOCKET_URL` environment variables
in the `Dockerfile`.

```bash
cabal run psc-gen -- -t ./src
patch src/lib/Api/Rest.purs Rest.purs.patch

docker build -t herculus-client . --platform linux/x86_64
docker cp $(docker create herculus-client):/opt/build/public/ ./public_docker

scp -r public_docker user@host:/root/code/herculus/app/client
```
