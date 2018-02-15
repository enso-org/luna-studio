"use strict";
var knownFrames = {};

var sendToFrame = function (id, data) {
  document.getElementsByName(id)[0].contentWindow.postMessage(data, "*");
};

var flushAll = function (id) {
  var queue = knownFrames[id];
  delete knownFrames[id];
  if (queue) queue.forEach(function (data) { sendToFrame(id, data); });
};

var queueMsg = function (id, data) {
  if (!knownFrames.hasOwnProperty(id)) knownFrames[id] = [];
  knownFrames[id].push(data);
};

var loaded = function (id) {
  var frame = document.getElementsByName(id)[0];
  return (frame && frame.contentWindow.document.readyState === "complete");
};

var register = function (id) {
  if (loaded(id)) flushAll(id);
  else setTimeout(function () { register(id); }, 100);
};

var send = function (id, data) {
  if (loaded(id)) sendToFrame(id, data);
  else queueMsg(id, data);
};

var sendData = function (id, type, data) {
  send(id, { event: "data", type: type, data: data });
};

var sendInternalData = function (id, data) {
  send(id, { event: "internalData", data: data });
};

var sendDatapoint = function (id, data) {
  send(id, { event: "datapoint", data: data });
};

var notifyStreamRestart = function (id, type, backup) {
  send(id, { event: "restart", type: type, data: backup });
};

module.exports = {
  sendData: sendData,
  register: register,
  notifyStreamRestart: notifyStreamRestart,
  sendDatapoint: sendDatapoint,
  sendInternalData: sendInternalData
};
