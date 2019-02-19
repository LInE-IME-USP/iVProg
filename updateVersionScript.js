var fs = require('fs');
var path = require('path');

var fileName = '.ivprog_version.json';
var date = new Date();
var day = date.getUTCDate() > 9 ? date.getUTCDate() : '0' + date.getUTCDate();
var month = date.getMonth() > 9 ? date.getMonth() + 1 : '0' + (date.getMonth() + 1);
var minutes = date.getMinutes() > 9 ? date.getMinutes() : '0' + date.getMinutes();
var hour = date.getHours() > 9 ? date.getHours() : '0' + date.getHours();
var versionString = `${date.getFullYear()}_${month}_${day} ${hour}_${minutes}`;
var versionData = `{ "version":"${versionString}" }`;
var filePath = path.join(__dirname, fileName);
fs.writeFileSync(filePath, versionData);