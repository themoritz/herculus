var webpack = require('webpack')
var HtmlWebpackPlugin = require('html-webpack-plugin');
var config = require('./webpack.config.js')

var webSocketUrl;
if (process.env.WEBSOCKET_URL) {
  webSocketUrl = process.env.WEBSOCKET_URL
} else {
  throw "Please define the WEBSOCKET_URL env var."
}

var apiUrl;
if (process.env.API_URL) {
  apiUrl = process.env.API_URL
} else {
  throw "Please define the API_URL env var."
}

config.plugins = [
  new webpack.DefinePlugin({
    'process.env': {
      'WEBSOCKET_URL': JSON.stringify(webSocketUrl),
      'API_URL': JSON.stringify(apiUrl),
      'GIT_REV': JSON.stringify(process.env.GIT_REV)
    }
  }),
  new HtmlWebpackPlugin({
    template: './src-public/index.html'
  })
]

module.exports = config
