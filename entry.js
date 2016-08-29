// React
var React = require('react')
var ReactDOM = require('react-dom')

global.React = React
global.ReactDOM = ReactDOM

// react-virtualized
require('react-virtualized/styles.css')

// react-datepicker
require('react-datepicker/dist/react-datepicker.css');

// react-codemirror
var Codemirror = require('react-codemirror')
require('codemirror/lib/codemirror.css')
require('codemirror/mode/mllike/mllike')
require('codemirror/theme/neat.css')

global.Codemirror = Codemirror

// Own components
global.OnDidMount = require('./assets/components/OnDidMount.js')
global.Grid = require('./assets/components/Grid.js')
global.DatePicker = require('./assets/components/DatePicker.js')

// font-awesome
require('font-awesome-webpack!./font-awesome.config.js')

// Styles
require('./assets/sass/main.scss')

// GHCJS
require('./client/.stack-work/dist/x86_64-linux/Cabal-1.22.8.0_ghcjs/build/client/client.jsexe/all.js')
