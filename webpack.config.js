var path = require('path');
var webpack = require('webpack');
module.exports = {
    entry: './js/main.js',
    output: {
        path: path.resolve(__dirname, 'build'),
        filename: 'ivprog.bundle.js'
    },
    node: {
        fs: 'empty',
    },
    module: {
        rules: [
            {
                test: /\.js$/,
                loader: 'babel-loader',
                query: {
                    presets: ['env']
                }
            },
            { test: /\.g4/, loader: 'antlr4-webpack-loader' }
        ]
    },
    stats: {
        colors: true
    },
    devtool: 'source-map'
};
