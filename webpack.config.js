var path = require('path');
var HtmlWebpackPlugin = require('html-webpack-plugin');
var UpdateVersionPlugin = require('./updateVersionPlugin');
//var ChangeScriptSourcePlugin = require('./changeScriptSourcePlugin');
var CopyPlugin = require('copy-webpack-plugin');
var CleanWebpackPlugin = require('clean-webpack-plugin');

module.exports = {
    entry: path.resolve(__dirname, 'js/main.js'),
    output: {
        path: path.resolve(__dirname, 'build',"js"),
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
      new CleanWebpackPlugin({
        cleanOnceBeforeBuildPatterns:[path.resolve(__dirname, 'build/**/*')],
        watch: true
      }),
      new UpdateVersionPlugin(),
      new HtmlWebpackPlugin({
        template: 'templates/index.html',
        filename: path.resolve(__dirname, 'build', 'index.html')
      }),
      new HtmlWebpackPlugin({
        template: 'templates/runner.html',
        filename: path.resolve(__dirname, 'build', 'runner.html')
      }),
      /*new ChangeScriptSourcePlugin(),*/
      new CopyPlugin([
        {from:"css/ivprog-visual-1.0.css", to:path.resolve(__dirname, 'build/css')},
        {from:"css/ivprog-term.css", to:path.resolve(__dirname, 'build/css')},
        {from:'js/Sortable.js', to:path.resolve(__dirname, 'build/js')},
        {from:'js/iassign-integration-functions.js', to:path.resolve(__dirname, 'build/js')},
        {from: 'img/trash-icon.png', to:path.resolve(__dirname, 'build/img')},
        {from:'js/jquery.json-editor.min.js', to:path.resolve(__dirname, 'build/js')},
        /*{from:'index.html', to:path.resolve(__dirname, 'build')},
        {from:'runner.html', to:path.resolve(__dirname, 'build')},*/
      ])
    ],
    optimization: {
        splitChunks: {
            chunks: 'all'
        }
    },
    devtool: 'source-map',
    watchOptions: {
        ignored: [
          path.resolve(__dirname, '.ima_version.json'),
          path.resolve(__dirname, 'index.html'),
          path.resolve(__dirname, 'runner.html')
        ]
    }
};