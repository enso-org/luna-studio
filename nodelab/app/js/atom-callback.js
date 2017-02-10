"use strict";

var removeFromArray = function (array, elt) {
  var index = array.indexOf(elt);
  array.splice(index, 1);
};

module.exports = function () {

  var listeners = {
    onEvent: [],
    codeListener: [],
  };

  return {
    codeListener: function (listener) {
      listeners.codeListener.push(listener);
    },
    pushCode: function(data) {
      listeners.codeListener.forEach(function(listener) {
        listener(data);
      });
    },
    onEvent: function (listener) {
      listeners.onEvent.push(listener);
    },
    unOnEvent: function (listener) {
      removeFromArray(listeners.onEvent, listener);
    },
    pushEvent: function(data) {
      listeners.onEvent.forEach(function(listener) {
        listener(data);
      });
    }
  };
};
