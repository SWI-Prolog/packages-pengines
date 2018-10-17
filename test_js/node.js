var path = require('path');
var Mocha = require('mocha');
var Pengine = require('../web/js/pengines');
var common = require('./common');

// Wrapper script to run Pengine JavaScript test cases
// in Node.js.

var port = common.getPenginePort();

global.Pengine = Pengine;
global.SERVER = `http://localhost:${port}/pengine`;

var mocha = new Mocha({ui: 'bdd', reporter: 'tap'});
mocha.addFile(path.join(__dirname, 'tests.js'));
mocha.run(function(failures) {
  process.exit(failures > 0 ? 1 : 0);
});
