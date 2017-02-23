var path = require('path')
var webpack = require('webpack')
var HtmlWebpackPlugin = require('html-webpack-plugin');

var loaders = require('./loaders.js')

var webSocketUrl;
if (process.env.WEBSOCKET_URL) {
  webSocketUrl = process.env.WEBSOCKET_URL
} else {
  throw 'Please define the WEBSOCKET_URL env var.'
}

var apiUrl;
if (process.env.API_URL) {
  apiUrl = process.env.API_URL
} else {
  throw 'Please define the API_URL env var.'
}

module.exports = {
  entry: [
    path.join(__dirname, '../src-public/entry.prod.js')
  ],
  output: {
    path: path.resolve('./public'),
    filename: 'bundle-[hash:8].js'
  },
  module: {
    rules: [
      loaders.css,
      loaders.js,
      loaders.pursProd
    ]
  },
  plugins: [
    new webpack.DefinePlugin({
      'process.env': {
        'WEBSOCKET_URL': JSON.stringify(webSocketUrl),
        'API_URL': JSON.stringify(apiUrl),
        'GIT_REV': JSON.stringify(process.env.GIT_REV)
      }
    }),
    new HtmlWebpackPlugin({
      template: './src-public/index.html'
    }),
    new webpack.optimize.UglifyJsPlugin({
      compress: {
        warnings: false
      }
    })
  ],
  resolveLoader: {
    modules: [
      path.join(__dirname, '../node_modules')
    ]
  }
}
