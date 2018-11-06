var path = require('path');
var webpack = require('webpack');
module.exports = {
    entry: './js/main.js',
    mode: 'development',
    watch: true,
    output: {
        path: path.resolve(__dirname, 'build'),
        filename: 'ivprog.bundle.js',
        library: 'ivprogCore'
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
    /*optimization: {
        splitChunks: {
            chunks: 'all'
        }
    },*/
    devtool: 'source-map'
};