"use strict";

var removeFromArray = function (array, elt) {
  var index = array.indexOf(elt);
  array.splice(index, 1);
};

module.exports = function () {

  var listeners = {
    bufferListener: [],
    codeListener: [],
    eventListenerInternal: [],
    textListener: [],
  };

  return {
    codeListener: function (listener) {
      listeners.codeListener.push(listener);
    },
    pushCode: function(uri_send, start_send, end_send, text) {
      listeners.codeListener.forEach(function(listener) {
        listener(uri_send, start_send, end_send, text);
      });
    },

    bufferListener: function (listener) {
      listeners.bufferListener.push(listener);
    },
    pushBuffer: function(data1, data2) {
      listeners.bufferListener.forEach(function(listener) {
        listener(data1, data2);
      });
    },

    subscribeEventListenerInternal: function(listener) {
      listeners.eventListenerInternal.push(listener);
    },
    unsubscribeEventListenerInternal: function(listener) {
      removeFromArray(listeners.eventListenerInternal, listener);
    },
    pushInternalEvent: function(data) {
      listeners.eventListenerInternal.forEach(function(listener) {
          listener(data);
      });
    },

    subscribeText: function(listener) {
      listeners.textListener.push(listener);
    },
    unsubscribeText: function(listener) {
      removeFromArray(listeners.textListener, listener);
    },
    pushText: function(data) {
      listeners.textListener.forEach(function(listener) {
          listener(data);
      });
    },
    getPath: function(data) {
      return data.uri;
    },
    getStart: function(data) {
      return data.start;
    },
    getStop: function(data) {
      return data.end;
    },
    getText: function(data) {
      return data.text;
    },
    getCursor: function(data) {
      return data.cursor;
    },

  };
};
