var WebpackShellPlugin = require('webpack-shell-plugin')
var ExtractTextPlugin = require('extract-text-webpack-plugin')
var webpack = require('webpack')
var config = require('./webpack.dev.config.js')

config.entry = './entry.prod.js'

config.plugins = [
  new ExtractTextPlugin("css/bundle.css"),
  new WebpackShellPlugin({
    onBuildExit: [
      'cd ./assets/public/js && mv bundle.js app.js',
      './node_modules/.bin/zopfli -i 15 ./assets/public/js/app.js'
    ]
  }),
  new webpack.DefinePlugin({
    // For React
    'process.env': {
      'NODE_ENV': JSON.stringify('production')
    }
  }),
  new webpack.optimize.UglifyJsPlugin({
    compress: {
      warnings: false
    }
  })
]

module.exports = config
