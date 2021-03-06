var Elm = require("../src/Main.elm").Elm;

var app = Elm.Main.init({
  node: document.getElementById("main"),
  flags: {
    searchServiceUrl: process.env.SEARCH_SERVICE,
    searchApiKey: process.env.SEARCH_KEY,
    version: process.env.VERSION ? process.env.VERSION : "1.0.0",
    mapToken: process.env.MAPBOX_TOKEN,
  },
});


app.ports.loadImage.subscribe(function (msg) {
  let img = new Image();
  img.onload = function() {
    app.ports.receiveImageUpdates.send(this.src);
  }
  img.src = msg;
})
