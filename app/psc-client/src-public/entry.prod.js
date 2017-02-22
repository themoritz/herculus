require('./css/main.css')

// Basil for local storage
require('basil.js')
global.basil = new window.Basil({expireDays: 60})

var apiUrl = process.env.API_URL || 'http://localhost:3000/api/'
var webSocketUrl = process.env.WEBSOCKET_URL || 'ws://localhost:3000/websocket'

require('./../src/Main.purs').main(apiUrl)(webSocketUrl)(false)()
