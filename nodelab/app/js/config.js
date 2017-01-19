"use strict";

var release = require('./config.release');
var brunch = require('brunch');
var config;
var customEvent = require('./CustomEvent').customEvent;

try {
  var u = require('underscore');
} catch (e) {
  var u = _;
}
function defaultBackend() {
  // var l = window.location;
  // if(typeof(l) != 'undefined')
  //   return ((l.protocol === "https:") ? "wss://" : "ws://") + l.hostname + (((l.port !== 80) && (l.port !== 443)) ? ":" + l.port : "") + "/ws";
  // else
    return "ws://localhost:8088";
}

if (brunch.env !== "production") {
  var local = {};
  var debug = require('./config.debug');
  try {
    local   = require('./config.local');
  } catch (e) {
    // no local overrides, skipping.
  }

  var browser = {};
  if (localStorage.getItem('logging') !== null) {
    browser.logging = (localStorage.getItem('logging') === "true");
  }
  if (localStorage.getItem("backend") !== null) {
    browser.backend = (localStorage.getItem("backend") === "true");
  }
  if (localStorage.getItem("exportState") !== null) {
    browser.exportState = (localStorage.getItem("exportState") === "true");
  }
  browser.backendAddress = localStorage.getItem("backendAddress") || defaultBackend();

  config = u({}).defaults(browser, local, debug, release);

  console.info("Backend address is " + browser.backendAddress);

  window.setBackendAddress = function (addr) {
    console.log("Backend address set to: " + addr + ". Refresh the page to use your new backend.");
    localStorage.setItem('backendAddress', addr);
  };

  console.info("Logging is " + (config.logging?"enabled":"disabled"));

  window.enableLogging  = function (){
    console.info("Logging enabled! Please reload the page.");
    localStorage.setItem('logging', "true" );
  };
  window.disableLogging = function (){
    console.info("Logging disabled! Please reload the page.");
    localStorage.setItem('logging', "false");
  };

  window.showState = function() {
    customEvent("debug.getState", null);
  };

} else {
  config = release;
}

module.exports = config;
