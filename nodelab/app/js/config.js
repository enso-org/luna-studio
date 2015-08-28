"use strict";

var release = require('config.release');
var brunch = require('brunch');
var config;

if(brunch.env !== "production") {
  var local = {};
  var debug = require('config.debug');
  try {
    local   = require('config.local');
  } catch (e) {
    // no local overrides, skipping.
  }

  var browser = {};
  if(localStorage.getItem('logging') !== null) {
    browser.logging = (localStorage.getItem('logging') === "true");
  }
  if (localStorage.getItem("backend") !== null) {
    browser.backend = (localStorage.getItem("backend") === "true");
  }

  config = _({}).defaults(browser, local, debug, release);

  console.info("Backend connection is " + (config.backend ? "enabled" : "disabled"));
  window.enableBackend = function  () {
    console.info("Backend connection enabled! Please reload the page.");
    localStorage.setItem('backend', "true");
  };
  window.disableBackend = function () {
    console.info("Backend connection disabled! Please reload the page.");
    localStorage.setItem('backend', "false");
  };

  console.info("Logging is " + (config.logging?"enabled":"disabled"));
  window.enableLogging  = function(){
    console.info("Logging enabled! Please reload the page.");
    localStorage.setItem('logging', "true" );
  };
  window.disableLogging = function(){
    console.info("Logging disabled! Please reload the page.");
    localStorage.setItem('logging', "false");
  };
} else {
  config = release;
}

module.exports = config;
