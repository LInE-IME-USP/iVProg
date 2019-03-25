var fs = require('fs');
var path = require('path');
var versionStringFun = require('./versionFileHelper');

function writeVersionFile () {
  var versionString = versionStringFun();
  var fileData = `{ "version":"${versionString}" }`;
  var filePath = path.join(__dirname, '.ima_version.json');
  fs.writeFileSync(filePath, fileData);
}

function UpdateVersionPlugin () { }

UpdateVersionPlugin.prototype.apply  = function (compiler) {
  compiler.hooks.beforeCompile.tap("UpdateVersionPlugin", function() {
    writeVersionFile();
  });
}

module.exports = UpdateVersionPlugin;