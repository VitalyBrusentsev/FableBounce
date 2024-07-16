var path = require("path");
const isProduction = process.argv.find((v) => v.includes('production'));


module.exports = {
  devtool: isProduction ? false : 'source-map',
  mode: isProduction ? 'production' : 'development',
  entry: {
    'app': './src/App.fs.js'
  },
  output: {
    path: path.join(__dirname, './dist'),
    filename: '[name].js',
  },
  devServer: {
    static: path.join(__dirname, "./dist"),
    port: 8080,
  },
  module: {
      rules: []
  }
}