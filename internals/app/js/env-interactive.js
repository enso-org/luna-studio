"use strict";

module.exports = function() {
  // required for interactive
  window.app            = require('./app-internals');
  window.common         = require('./common');
  window.config         = require('./config');
};
