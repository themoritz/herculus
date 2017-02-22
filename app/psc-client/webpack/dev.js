var path = require('path')
var webpack = require('webpack')
var HtmlWebpackPlugin = require('html-webpack-plugin')

var loaders = require('./loaders.js')

var port = process.env.PORT || 3001
var proxyPort = process.env.PROXY_PORT || 3000

var config = {
  entry: [
    'webpack-hot-middleware/client?reload=true',
    path.join(__dirname, '../src-public/entry.js'),
  ],
  output: {
    path: path.resolve('./public'),
    filename: 'bundle.js',
    publicPath: ''
  },
  module: {
    rules: [
      loaders.css,
      loaders.js,
      loaders.pursDev
    ]
  },
  plugins: [
    new webpack.DefinePlugin({
      'process.env.API_URL':
        JSON.stringify('http://localhost:'+port+'/proxy/api/'),
      'process.env.WEBSOCKET_URL':
        JSON.stringify('ws://localhost:'+proxyPort+'/websocket')
    }),
    new webpack.SourceMapDevToolPlugin({
      filename: '[file].map',
      moduleFilenameTemplate: '[absolute-resource-path]',
      fallbackModuleFilenameTemplate: '[absolute-resource-path]'
    }),
    new webpack.HotModuleReplacementPlugin(),
    new webpack.NoEmitOnErrorsPlugin(),
    new HtmlWebpackPlugin({
      template: './src-public/index.html'
    }),
  ],
  resolveLoader: {
    modules: [
      path.join(__dirname, '../node_modules')
    ]
  }
};

// If this file is directly run with node, start the development server
// instead of exporting the webpack config.
if (require.main === module) {
  var compiler = webpack(config);
  var express = require('express');
  var app = express();
  var proxy = require('express-http-proxy');

  app
    .use('/proxy', proxy('localhost:'+proxyPort))
    .use(require('connect-history-api-fallback')())
    .use(require('webpack-dev-middleware')(compiler, {
      publicPath: config.output.publicPath,
      stats: {
        hash: false,
        timings: false,
        version: false,
        assets: false,
        errors: true,
        colors: false,
        chunks: false,
        children: false,
        cached: false,
        modules: false,
        chunkModules: false
      }
    }))
    .use(require('webpack-hot-middleware')(compiler))
    .use(express.static('./public'))
    .listen(port);
} else {
  module.exports = config;
}
