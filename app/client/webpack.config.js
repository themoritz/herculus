var ExtractTextPlugin = require('extract-text-webpack-plugin')
var path = require('path')
var webpack = require('webpack')
var autoprefixer = require('autoprefixer')

module.exports = {
  entry: './entry.js',
  output: {
    path: path.join(__dirname, 'public'),
    filename: 'js/bundle.js'
  },
  module: {
    loaders: [
      { test: /\.css$/, loader: "style!css" },
      { test: /\.scss$/, loader: ExtractTextPlugin.extract("style", "css!postcss-loader!sass") },
      {
        test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
        loader: 'url-loader?limit=10000&mimetype=application/font-woff'
      },
      { test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/, loader: 'file-loader' }
    ]
  },
  postcss: function () {
    return [autoprefixer]
  },
  plugins: [
    new ExtractTextPlugin("css/bundle.css"),
    new webpack.DefinePlugin({
      'process.env': {
        'GIT_REV': JSON.stringify(process.env.GIT_REV || 'not defined')
      }
    })
  ]
}