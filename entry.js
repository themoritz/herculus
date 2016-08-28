// React
var React = require('react')
var ReactDOM = require('react-dom')

global.React = React
global.ReactDOM = ReactDOM

// react-virtualized
var ReactVR = require('react-virtualized')
require('react-virtualized/styles.css')
require('./assets/sass/main.scss')

global.AutoSizer = ReactVR.AutoSizer
global.Grid = ReactVR.Grid

// react-ace
var AceEditor = require('react-ace')
var brace = require('brace')
require('brace/mode/ocaml')
require('brace/theme/github')

global.AceEditor = AceEditor.default

// Own components
global.OnDidMount = require('./assets/components/OnDidMount.js')

// GHCJS
require('./client/.stack-work/dist/x86_64-linux/Cabal-1.22.8.0_ghcjs/build/client/client.jsexe/all.js')
