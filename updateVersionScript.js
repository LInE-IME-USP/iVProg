var fs = require('fs');
var path = require('path');

var fileName = '.ivprog_version';
var date = new Date();
var day = date.getUTCDate() > 9 ? date.getUTCDate() : '0' + date.getUTCDate();
var month = date.getMonth() > 9 ? date.getMonth() + 1 : '0' + (date.getMonth() + 1);
var versionString = `${date.getFullYear()}_${month}_${day} ${date.getHours()}_${date.getMinutes()}`;
var versionData = `{ "version":"${versionString}" }`;
var filePath = path.join(__dirname, fileName);
fs.writeFileSync(filePath, versionData);