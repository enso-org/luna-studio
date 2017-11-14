"use strict";

var removeFromArray = function (array, elt) {
    var index = array.indexOf(elt);
    array.splice(index, 1);
};

module.exports = function () {

    var listeners = {
        setBufferListener: [],
        setClipboardListener: [],
        insertonInsertCode: [],
        eventListenerInternal: [],
        diffsListener: [],
        statusListener: [],
        interpreterUpdateListener: [],
        lexer: null,
    };

    return {
        onInsertCode: function (listener) {
            listeners.insertonInsertCode.push(listener);
        },
        insertCode: function(uri, diffs) {
            listeners.insertonInsertCode.forEach(function(listener) {
                listener(uri, diffs);
            });
        },

        onSetBuffer: function (listener) {
            listeners.setBufferListener.push(listener);
        },
        setBuffer: function(data1, data2) {
            listeners.setBufferListener.forEach(function(listener) {
                listener(data1, data2);
            });
        },

        onSetClipboard: function (listener) {
            listeners.setClipboardListener.push(listener);
        },
        setClipboard: function(data1, data2) {
            listeners.setClipboardListener.forEach(function(listener) {
                listener(data1, data2);
            });
        },

        statusListener: function (listener) {
            listeners.statusListener.push(listener);
        },
        pushStatus: function(data1, data2, data3) {
            listeners.statusListener.forEach(function(listener) {
                listener(data1, data2, data3);
            });
        },

        onInterpreterUpdate: function (listener) {
            listeners.interpreterUpdateListener.push(listener);
        },
        pushInterpreterUpdate: function(data1, data2, data3) {
            listeners.interpreterUpdateListener.forEach(function(listener) {
                listener(data1, data2, data3);
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

        subscribeDiffs: function(listener) {
            listeners.diffsListener.push(listener);
        },
        unsubscribeDiffs: function(listener) {
            removeFromArray(listeners.diffsListener, listener);
        },
        pushDiffs: function(diffs) {
            listeners.diffsListener.forEach(function(listener) {
                listener(diffs);
            });
        },
        setLexer: function(fun) {
            listeners.lexer = fun;
        },
        unsetLexer: function() {
            listeners.lexer = null;
        },
        lex: function(stack, data) {
            return listeners.lexer(stack, data);
        }
    };
};
