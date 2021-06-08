var Elm = require("../src/Main.elm").Elm;
var packageJson = require("../package.json");

var app = Elm.Main.init({
  node: document.getElementById("main"),
  flags: {
    searchServiceUrl: process.env.SEARCH_SERVICE,
    searchApiKey: process.env.SEARCH_KEY,
    version: packageJson.version,
  },
});