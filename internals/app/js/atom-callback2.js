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
    pushBuffer: function(data) {
      listeners.bufferListener.forEach(function(listener) {
        listener(data);
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
      var key = "uri";
      for(key in data) {
        if(data.hasOwnProperty(key)) {
          var value = data.key;
          return value;}
        }
    },
    getStart: function(data) {
      var key = "start";
      for(key in data) {
        if(data.hasOwnProperty(key)) {
          var value = data.key;
          return value;}
        }
    },
    getStop: function(data) {
      var key = "end";
      for(key in data) {
        if(data.hasOwnProperty(key)) {
          var value = data.key;
          return value;}
        }
    },
    getText: function(data) {
      var key = "text";
      for(key in data) {
        if(data.hasOwnProperty(key)) {
          var value = data.key;
          return value;}
        }
    },
    getCursor: function(data) {
      var key = "cursor";
      for(key in data) {
        if(data.hasOwnProperty(key)) {
          var value = data.key;
          return value;}
        }
    },

  };
};
