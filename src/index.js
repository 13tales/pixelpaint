"use strict";

var Elm = require("./main.elm");

require("./index.html");

var mountNode = document.getElementById("paint");

var app = Elm.Main.embed(mountNode);
