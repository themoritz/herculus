exports.css = {
  test: /\.css$/,
  use: [
    'style-loader',
    'css-loader?importLoaders=1',
    {
      loader: 'postcss-loader',
      options: {
        plugins: function () {
          return [
            require('postcss-import'),
            require('postcss-simple-vars'),
            require('postcss-custom-properties'),
            require('postcss-calc'),
            require('postcss-color-function'),
            require('postcss-nested'),
            require('autoprefixer')
          ]
        }
      }
    }
  ]
}

exports.js = {
  test: /\.js$/,
  exclude: /node_modules|bower_components/,
  loader: 'source-map-loader'
}

exports.pursDev = {
  test: /\.purs$/,
  exclude: /node_modules/,
  loader: 'purs-loader',
  query: {
    bundle: false,
    psc: 'psa',
    pscIde: true,
    pscArgs: {
      sourceMaps: true
    }
  }
}

exports.pursProd = {
  test: /\.purs$/,
  exclude: /node_modules/,
  loader: 'purs-loader',
  query: {
    bundle: true
  }
}
