var knownFrames = {};

var sendToFrame = function (id, data) {
  window.frames[id].window.postMessage(data, "*");
};

var flushAll = function (id) {
  var queue = knownFrames[id];
  delete knownFrames[id];
  queue.forEach(function (data) { sendToFrame(id, data); });
};

var queueMsg = function (id, data) {
  if (!knownFrames.hasOwnProperty(id)) knownFrames[id] = [];
  knownFrames[id].push(data);
};

var loaded = function (id) {
  return (window.frames[id] && window.frames[id].window.document.readyState == "complete");
};

var register = function (id) {
  if (loaded(id)) flushAll(id);
  else setTimeout(function () { register(id); }, 100);
};

var send = function (id, data) {
  if (loaded(id)) sendToFrame(id, data);
  else queueMsg(id, data);
};

var sendData = function (id, data) {
  send(id, { event: "data", data: data });
};

var sendDatapoint = function (id, data) {
  send(id, { event: "datapoint", data: data });
};

var notifyStreamRestart = function (id) {
  send(id, { event: "restart" });
};

module.exports = {
  sendData: sendData,
  register: register,
  notifyStreamRestart: notifyStreamRestart,
  sendDatapoint: sendDatapoint
};
