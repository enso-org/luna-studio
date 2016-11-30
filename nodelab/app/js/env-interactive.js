"use strict";

module.exports = function() {
  // required for interactive
  window.app            = require('app');
  window.breadcrumb     = require('breadcrumb');
  window.common         = require('common');
  window.config         = require('config');
  window.connectionPen  = require('connection_pen');
  window.raycaster      = require('raycaster');
  window.textEditor     = require('text_editor');
  window.Button         = require('Widget/Button');
  window.CodeEditor     = require('Widget/CodeEditor');
  window.Connection     = require('Widget/Connection');
  window.DataFrame      = require('Widget/DataFrame');
  window.Graphics       = require('Widget/Graphics');
  window.GraphNode      = require('Widget/Node');
  window.Group          = require('Widget/Group');
  window.Icon           = require('Widget/Icon');
  window.Label          = require('Widget/Label');
  window.LabeledWidget  = require('Widget/LabeledWidget');
  window.LongText       = require('Widget/Text');
  window.PlotImage      = require('Widget/Image');
  window.Port           = require('Widget/Port');
  window.Slider         = require('Widget/Slider');
  window.TextBox        = require('Widget/TextBox');
  window.Toggle         = require('Widget/Toggle');
  window.h$errorMsg     = require("BSOD").appCrashed;
};
