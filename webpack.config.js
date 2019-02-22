var path = require('path');
var webpack = require('webpack');
var UpdateVersionPlugin = require('./updateVersionPlugin');

module.exports = {
    entry: './js/main.js',
    mode: 'development',
    watch: true,
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
        rules: [{
            test: /\.js$/,
            loader: 'babel-loader',
            query: {
                presets: ['env']
            }
        }, {
            test: /\.g4/,
            loader: 'antlr4-webpack-loader'
        }]
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