"use strict";

var removeFromArray = function (array, elt) {
  var index = array.indexOf(elt);
  array.splice(index, 1);
};

module.exports = function () {

  var listeners = {
    onEvent: [],
    codeListener: [],
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
    pushNotification: function (lvl, error) {
      if (typeof atom == 'undefined')
      {
          console.log(error);
      }
      else
      {
          var notification;
          if (lvl === 0) {
            notification = atom.notifications.addFatalError("Fatal Error", {
              dismissable: true,
              description: error,
              buttons: [
                {
                  text: 'Copy to clipboard',
                  onDidClick: function() {
                    atom.clipboard.write(error);
                    return notification.dismiss();
                  }
                }
              ]
            });
          } else if (lvl === 1) {
            notification = atom.notifications.addError("Error", {
              dismissable: true,
              description: error,
              buttons: [
                {
                  text: 'Copy to clipboard',
                  onDidClick: function() {
                    atom.clipboard.write(error);
                    return notification.dismiss();
                  }
                }
              ]
            });
          } else {
            notification = atom.notifications.addWarning("Warning", {
              dismissable: true,
              description: error,
              buttons: [
                {
                  text: 'Copy to clipboard',
                  onDidClick: function() {
                    atom.clipboard.write(error);
                    return notification.dismiss();
                  }
                }
              ]
            });
          }
      }
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
