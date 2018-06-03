"use strict";

var mockSocket = function () {
  return {
    addEventListener: function () {},
    removeEventListener: function () {},
    send: function () {}
  };
};

module.exports = function () {
  var listenConnection = mockSocket();
  var sendConnection   = mockSocket();
  var isConnOpen = false;
  var listeners = {
    onOpen: [],
    onMessage: [],
    onClose: [],
    onError: []
  };

  var attachListeners = function () {
    listeners.onOpen.forEach(function (listener) {
      listenConnection.addEventListener("open", listener);
    });
    listeners.onMessage.forEach(function (listener) {
      listenConnection.addEventListener("message", listener);
    });
    listeners.onClose.forEach(function (listener) {
      listenConnection.addEventListener("close", listener);
    });
    listeners.onError.forEach(function (listener) {
      listenConnection.addEventListener("error", listener);
    });
  };

  return {
    isOpen: function () {
      return isConnOpen;
    },
    onOpen: function (listener) {
      isConnOpen = true;
      listeners.onOpen.push(listener);
      listenConnection.addEventListener("open", listener);
    },
    onMessage: function (listener) {
      listeners.onMessage.push(listener);
      listenConnection.addEventListener("message", listener);
    },
    onClose: function (listener) {
      listeners.onClose.push(listener);
      listenConnection.addEventListener("close", listener);
    },
    onError: function (listener) {
      listeners.onError.push(listener);
      listenConnection.addEventListener("error", listener);
    },
    connect: function (listenAddr, sendAddr) {
      listenConnection = new WebSocket(listenAddr);
      listenConnection.binaryType = "arraybuffer";
      sendConnection = new WebSocket(sendAddr);
      sendConnection.binaryType = "arraybuffer"
      attachListeners();
    },
    send: function (data) {
      sendConnection.send(data);
    }
  };
};
