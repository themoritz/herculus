# Hexl

## Preps

Works with npm v3.10.31 and node v6.5.0.
Use `nvm install v6.5.0`.

## Dev

### Run in background:

#### Watchers


``` shell
cd hexl
npm install
# optional gulp for live css injection
npm run watch-styles
npm run watch-client
```

#### MongoDB

- Windows

TODO: How to...?


- OS X (via Homebrew)

``` shell
brew services start mongodb
```

To stop run `brew services start mongodb`


### Build:

``` shell
cd hexl
# Server
npm run run-server
# Client
npm run build-client
```

Then go to [localhost:3001](http://localhost:3001).

## PDF Generation

PDF generation requires LaTeX with a recent version of TeX Live to be
installed on the system.
See http://pandoc.org/MANUAL.html for details on the required packages.

PDFs are generated with the Lato font. Is included in the debian package
`texlive-fonts-extra` or directly:
http://www.ctan.org/tex-archive/fonts/lato/
