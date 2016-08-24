var ExtractTextPlugin = require('extract-text-webpack-plugin')
var path = require('path')

module.exports = {
  entry: './entry.js',
  output: {
    path: path.join(__dirname, 'assets/public'),
    filename: 'js/bundle.js'
  },
  module: {
    loaders: [
      { test: /\.css$/, loader: "style!css" },
      { test: /\.scss$/, loader: ExtractTextPlugin.extract("style", "css!sass") }
    ]
  },
  plugins: [
    new ExtractTextPlugin("css/bundle.css")
  ]
}
