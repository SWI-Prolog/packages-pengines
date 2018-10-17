var path = require('path');
var util = require('util');
var puppeteer = require('puppeteer');
var common = require('./common');

// Wrapper script to run Pengine JavaScript test cases
// inside a Chromium headless browser instance.

var port = common.getPenginePort();

(async function() {
  var browser = await puppeteer.launch({
    args: ['--no-sandbox'],
    headless: true
  });
  var page = await browser.newPage();
  // Placeholder page for running tests
  await page.goto(`file:${path.join(__dirname, 'browser.html')}`);
  // Set Pengines server location.
  await page.addScriptTag({content: `var SERVER = "http://localhost:${port}/pengine";`});
  // Inject jQuery
  await page.addScriptTag({path: path.join(__dirname, '..', 'node_modules', 'jquery', 'dist', 'jquery.js')});
  // Inject Pengines
  await page.addScriptTag({path: path.join(__dirname, '..', 'web', 'js', 'pengines.js')});
  // Inject Mocha
  await page.addScriptTag({path: path.join(__dirname, '..', 'node_modules', 'mocha', 'mocha.js')});
  // Setup Mocha
  await page.evaluate(async () => mocha.setup({ui: 'bdd', reporter: 'tap'}));
  // Inject test cases
  await page.addScriptTag({path: path.join(__dirname, 'tests.js')});
  // Handle messages from the test runner
  var fail = false;
  page.on('console', async function(msg) {
    // Get whole console message.
    var text;
    var argsCount = msg.args().length;
    if (argsCount === 0) {
      text = msg.text().trim();
    } else {
      var argValues = [];
      for (var arg of msg.args()) {
        argValues.push(await arg.jsonValue());
      }
      text = util.format.apply(util, argValues).trim();
    }
    console.log(text);
    if (text.match(/^not ok/)) {
      // A failing test.
      fail = true;
    }
    if (text.match(/^# fail/)) {
      // Last line.
      await browser.close();
      process.exit(fail ? 1 : 0);
    }
  });
  // Run tests with Mocha
  await page.evaluate(async function() { mocha.run(); });
})().catch(function() {
  console.error(err);
  process.exit(1);
});
