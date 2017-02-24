"use strict";

var removeFromArray = function (array, elt) {
  var index = array.indexOf(elt);
  array.splice(index, 1);
};

module.exports = function () {

  var listeners = {
    onEvent: [],
    codeListener: [],
    notificationListener: [],
    eventListenerInternal: [],
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
    notificationListener: function (listener) {
      listeners.notificationListener.push(listener);
    },
    pushNotification: function (data1, data2) {
      listeners.notificationListener.forEach(function (listener) {
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
