require('./css/main.css')

// Font-awesome
require('font-awesome/css/font-awesome.css')

// Ace editor
var ace = require('brace')
require('brace/mode/haskell')
require('brace/mode/html')
require('brace/mode/latex')
require('brace/mode/markdown')
require('brace/theme/chrome')

// Flatpickr datepicker
global.Flatpickr = require('flatpickr')
require('flatpickr/dist/flatpickr.css')

// Basil for local storage
require('basil.js')
global.basil = new window.Basil({expireDays: 60})

var apiUrl = process.env.API_URL || 'http://localhost:3000/api/'
var webSocketUrl = process.env.WEBSOCKET_URL || 'ws://localhost:3000/websocket'

var app = require('./../src/Main.purs').main(apiUrl)(webSocketUrl)

if (module.hot) {

  module.hot.dispose(function () {
    window.document.body.innerHTML = ""
  })
  module.hot.accept()
  app(true)()

} else {

  app(false)()

}
