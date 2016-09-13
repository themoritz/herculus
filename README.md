# Hexl

## Dependencies

### Stack

``` shell
curl -sSL https://get.haskellstack.org/ | sh
```

### Node

Works with npm v3.10.31 and node v6.5.0.
Execute `nvm install v6.5.0` or `cd hexl && nvm use`.

To tell nvm to always use this version: `nvm alias default v6.5.0`.

### MongoDB

#### Debian Linux

#### Windows

#### OS X (via Homebrew)

``` shell
brew services start mongodb
```

To stop run `brew services start mongodb`

### LaTeX

PDF generation requires LaTeX with a recent version of TeX Live to be
installed on the system.
See http://pandoc.org/MANUAL.html for details on the required packages.

PDFs are generated with the Lato font. Is included in the debian package
`texlive-fonts-extra` or directly:
http://www.ctan.org/tex-archive/fonts/lato/

## Development

### Initial Setup

``` shell
cd hexl
npm install
```

Also needs to be called whenever someone changes the `package.json`...

### Workflow

1. Start gulp to watch for updates to `public/css/bundle.css` and
   `public/js/app.js`: `npm run watch`
2. Start webpack to rebundle foreign dependencies (React, Codemirror, ...)
   and styles: `npm run webpack-dev`

Whenever you made changes to the server, recompile and start it with
`npm run build-server`.

Whenever you made changes to the client, recompile it with
`npm run build-client`. gulp should refresh your browser window when done.

App is available at [localhost:3001](http://localhost:3001).

## Deployment

### Server

Initially,

``` shell
cd hexl
npm install

" hsc2hs
" put executables installed by stack on path
PATH=$PATH:$HOME/.local/bin
" use stack.yaml in server
cd hexl/server
stack install hsc2hs
```

Basically run

``` shell
cd hexl
stack build server
stack exec server
```

### Client and Assets

``` shell
API_URL=http://host:5555 WEBSOCKET_URL=ws://host:5555/websocket npm run webpack-prod
# Note the missing trailing slashes!
```

After that, the `assets/public` folder should contain:

``` shell
cd assets/public && tree
.
├── css
│   └── bundle.css
├── index.html
└── js
    ├── app.js
    └── app.js.gz
```

These can be served by nginx or similar.
