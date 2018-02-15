"use strict";

var release = require('./config.release');
var config;

function defaultBackend() {
  // var l = window.location;
  // if(typeof(l) != 'undefined')
  //   return ((l.protocol === "https:") ? "wss://" : "ws://") + l.hostname + (((l.port !== 80) && (l.port !== 443)) ? ":" + l.port : "") + "/ws";
  // else
    return "ws://localhost:30533";
}

config = release;

module.exports = config;
