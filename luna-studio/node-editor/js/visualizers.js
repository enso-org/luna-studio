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

var register = function (id) {
    if (window.frames[id] && window.frames[id].window.document.readyState == "complete") flushAll(id);
    else setTimeout(function () { register(id); }, 100);
};

var sendData = function (id, data) {
    if (window.frames[id]) sendToFrame(id, data);
    else queueMsg(id, data);
};

module.exports = {
    sendData: sendData,
    register: register
};
