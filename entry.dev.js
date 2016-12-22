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
require('codemirror/mode/gfm/gfm')
require('codemirror/mode/stex/stex')
require('codemirror/mode/htmlmixed/htmlmixed')
//require('codemirror/theme/neat.css')

global.Codemirror = Codemirror

// Own components
global.OnDidMount = require('./assets/components/OnDidMount.js')
global.Grid = require('./assets/components/Grid.js')
global.DatePicker = require('./assets/components/DatePicker.js')

// font-awesome
require('font-awesome-webpack!./font-awesome.config.js')

// Styles
require('./assets/sass/main.scss')

// Git rev sanity check
global.hexl$getClientGitRev = function () {
  return process.env.GIT_REV
}

// Basil for local storage
require('basil.js')
global.basil = new window.Basil({expireDays: 60})
