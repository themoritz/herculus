# Herculus

Herculus is a web application for quickly and easily creating custom
data processing apps.

You can play with it at __[www.herculus.io](https://www.herculus.io)__.

## Development

### General Setup

This repository works with the [Nix](https://nixos.org/) package manager
and [direnv](https://direnv.net/), so make sure you have both installed.

* [Install Nix](https://nixos.org/download/)
* [Install direnv](https://direnv.net/docs/installation.html)

#### MongoDB

The server uses MongoDB as a database. You can run it on your machine using Docker:

``` shell
docker volume create mongodbdata
docker run --name mongodb -v mongodbdata:/data/db -p 27017:27017 -d mongo
```

If you need to make manual changes to the database you can do something like this (on MacOs):

``` shell
brew tap mongodb/brew
brew install mongodb-community-shell

mongo --host localhost --port 27017
> show dbs
> use herculus
> db.users.find()
> db.dropDatabase()
```

#### LaTeX

PDF generation requires LaTeX with a recent version of TeX Live to be
installed on the system.
See http://pandoc.org/MANUAL.html for details on the required packages.

PDFs are generated with the Lato font. Is included in the debian package
`texlive-fonts-extra` or directly:
http://www.ctan.org/tex-archive/fonts/lato/

### Components

The project is divided into four main components (each link refers to the
respective README with development instructions):

* [Landing page](./landing-page/client):
  Static site generator for the landing page.

* [Documentation](./doc):
  Static site generator for the documentation.

* [Backend Server](./app):
  Haskell backend server that handles data processing.

* [Frontend Client](./app/client):
  PureScript frontend client.

### Troubleshoting

* _Error_: `server-exe: connect: does not exist (Connection refused)`

  _Solution_: Check if your local `mongodb` has been started before.

## Deployment

Right now there is no automated deployment process (the old
[NixOps](https://github.com/NixOS/nixops) process is bitrotten). Each component
has individual deployment instructions that can then be stiched together with
Nginx, where the server is proxied and the static sites as well as the frontend
assets are served by Nginx. Here is an [example nginx.conf](./nginx.conf).
