var fs = require('fs');
var path = require('path');

function processDate () {
  var date = new Date();
  var day = date.getUTCDate();
  day = day > 9 ? day : '0' + day;
  var month = date.getMonth() + 1;
  month = month > 9 ? month : '0' + month;
  var minutes = date.getMinutes();
  minutes = minutes > 9 ? minutes : '0' + minutes;
  var hour = date.getHours();
  hour = hour > 9 ? hour : '0' + hour;
  return {
    year: date.getFullYear(),
    month: month,
    day: day,
    hour: hour,
    minutes: minutes
  }
}

function writeVersionFile () {
  var versionInfo = processDate();
  var versionString = `${versionInfo.year}_${versionInfo.month}_${versionInfo.day} ${versionInfo.hour}_${versionInfo.minutes}`;
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