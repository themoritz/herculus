var React = require('react')
var ReactDOM = require('react-dom')
var ReactVR = require('react-virtualized')

require('react-virtualized/styles.css')
require('./assets/sass/main.scss')

global.React = React
global.ReactDOM = ReactDOM
global.AutoSizer = ReactVR.AutoSizer
global.Grid = ReactVR.Grid

require('./client/.stack-work/dist/x86_64-linux/Cabal-1.22.8.0_ghcjs/build/client/client.jsexe/all.js')
