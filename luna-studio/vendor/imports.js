var React = require("react");
var ReactDOM = require("react-dom");
var ResizeObserver = require("resize-observer-polyfill");

function copyToClipboard(txt, meta) { atom.clipboard.write(txt, [meta]); }
