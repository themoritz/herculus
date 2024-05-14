# Herculus Server

We use [Stackage](https://www.stackage.org/) snapshots, but prefer cabal for building.
For that, we take the `cabal.project.freeze` file from Stackage (e.g.,
`https://www.stackage.org/lts-21.19/cabal.config`) and copy it into the `app` directory.

## Development

* Install [ghcup](https://www.haskell.org/ghcup/), use versions:
  * HLS 2.7.0.0
  * cabal 3.10.3.0
  * GHC 9.4.8

* Build project with `cabal build all`, or just a specific target you're working on.

* HLS should work with VSCode.

* You can also build with Stack if you prefer.

## Deployment

At the moment this is not Dockerized.

### Nix

You can build with Nix:

```bash
cd server && nix-build && cd -
```

### Cabal

Or manually with `cabal` on the deployment machine:

```bash
# Example for Debian Bullseye

# GHCUP
apt install build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 pkg-config
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# requires zlib
apt install zlib1g-dev
cabal build server:server-exe
```

Then create a systemd service file and start the server.
