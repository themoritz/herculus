var WebpackShellPlugin = require('webpack-shell-plugin')
var ExtractTextPlugin = require('extract-text-webpack-plugin')
var webpack = require('webpack')
var config = require('./webpack.dev.config.js')

config.entry = './entry.prod.js'

config.plugins.push(
  new WebpackShellPlugin({
    onBuildExit: [
      'cd ./assets/public/js && mv bundle.js app.js',
      'zopfli --i15 ./assets/public/js/app.js'
    ]
  }),
  new webpack.DefinePlugin({
    'process.env': {
      // For React
      'NODE_ENV': JSON.stringify('production'),
      // Config: grabbed in client/jsbits/config.js
      'WEBSOCKET_URL': JSON.stringify(process.env.WEBSOCKET_URL || 'ws://localhost:3000/websocket'),
      'API_URL': JSON.stringify(process.env.API_URL || 'http://localhost:3001')
    }
  }),
  new webpack.optimize.UglifyJsPlugin({
    compress: {
      warnings: false
    }
  })
)

module.exports = config
