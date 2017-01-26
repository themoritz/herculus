var webpack = require('webpack')
var config = require('./webpack.config.js')

config.output.filename = 'js/app.js'

config.plugins.push(
  new webpack.DefinePlugin({
    'process.env': {
      // For React
      'NODE_ENV': JSON.stringify('production'),
      // Config: grabbed in jsbits/config.js
      'WEBSOCKET_URL': JSON.stringify(process.env.WEBSOCKET_URL || 'ws://localhost:3000/websocket'),
      'API_URL': JSON.stringify(process.env.API_URL || 'http://localhost:3001/api')
    }
  }),
  new webpack.optimize.UglifyJsPlugin({
    compress: {
      warnings: false
    }
  })
)

module.exports = config
