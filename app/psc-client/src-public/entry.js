require('./css/main.css')

var ace = require('brace')

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
