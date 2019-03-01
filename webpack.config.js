var path = require('path');
var webpack = require('webpack');
var UpdateVersionPlugin = require('./updateVersionPlugin');

module.exports = {
    entry: './js/main.js',
    mode: 'development',
    output: {
        path: path.resolve(__dirname, 'build'),
        filename: 'ivprog.bundle.js',
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
    plugins: [new UpdateVersionPlugin()],
    /*optimization: {
        splitChunks: {
            chunks: 'all'
        }
    },*/
    devtool: 'source-map',
    watchOptions: {
        ignored: path.resolve(__dirname, '.ima_version.json')
    }
};