var path = require('path');
var HtmlWebpackPlugin = require('html-webpack-plugin');
var UpdateVersionPlugin = require('./updateVersionPlugin');

module.exports = {
    entry: path.resolve(__dirname, 'js/main.js'),
    output: {
        path: path.resolve(__dirname, 'build'),
        filename: '[name].[contenthash].js',
        library: 'ivprogCore',
        libraryTarget: 'umd'
    },
    node: {
        fs: 'empty',
    },
    module: {
        rules: [
            {
              test: /\.js$/,
              exclude: /(node_modules)/,
              use: {
                loader: "babel-loader",
                options: {
                  presets: ["@babel/preset-env"]
                }
              }
            },
            {
              test: /\.g4$/,
              exclude: /(node_modules)/,
              use: {
                loader:'antlr4-webpack-loader'
              }
            }
        ]
    },
    stats: {
        colors: true
    },
    plugins: [
      new UpdateVersionPlugin(),
      new HtmlWebpackPlugin({
        template: 'templates/index.html',
        filename: '../index.html'
      }),
      new HtmlWebpackPlugin({
        template: 'templates/runner.html',
        filename: '../runner.html'
      })
    ],
    optimization: {
        splitChunks: {
            chunks: 'all'
        }
    },
    devtool: 'source-map',
    watchOptions: {
        ignored: path.resolve(__dirname, '.ima_version.json')
    }
};