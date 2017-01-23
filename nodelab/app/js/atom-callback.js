"use strict";

var removeFromArray = function (array, elt) {
  var index = array.indexOf(elt);
  array.splice(index, 1);
};

module.exports = function () {

  var listeners = {
    onEvent: [],
  };

  return {
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
