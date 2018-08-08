var React = require("react");
var ReactDOM = require("react-dom");

function copyToClipboard(txt, meta) { atom.clipboard.write(txt, [meta]); }
