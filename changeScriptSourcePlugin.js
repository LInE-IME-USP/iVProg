// If your plugin is direct dependent to the html webpack plugin:
var HtmlWebpackPlugin = require('html-webpack-plugin');

function ChangeScriptSourcePlugin () {}

ChangeScriptSourcePlugin.prototype.apply = function (compiler) {
  compiler.hooks.compilation.tap('ChangeScriptSourcePlugin', function (compilation) {
    console.log('The compiler is starting a new compilation...')
    // Staic Plugin interface |compilation |HOOK NAME | register listener 
    HtmlWebpackPlugin.getHooks(compilation).alterAssetTags.tapAsync(
      'ChangeScriptSourcePlugin', // <-- Set a meaningful name here for stacktraces
      function (data, cb) {
        // Manipulate the content
        const listSize = data.assetTags.scripts.length;
        for (let i = 0; i < listSize; ++i) {
          const tag = data.assetTags.scripts[i];
          var path = tag.attributes.src;
          // remove build/ from src...
          data.assetTags.scripts[i].attributes.src = path.substring(path.indexOf("/") + 1);
        }

        // Tell webpack to move on
        cb(null, data);
      }
    )
  })
}

module.exports = ChangeScriptSourcePlugin