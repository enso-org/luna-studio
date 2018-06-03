"use strict";

function defaultBackend() {
    // var l = window.location;
    // if(typeof(l) != "undefined")
    //     return ((l.protocol === "https:") ? "wss://" : "ws://") + l.hostname + (((l.port !== 80) && (l.port !== 443)) ? ":" + l.port : "") + "/ws";
    // else
    return {
        send: "ws://localhost:30533",
        listen: "ws://localhost:30534"
    };
}

module.exports = {
  backend: true,
  backendAddress: (window.backendAddress || defaultBackend()),
  logging:         false,
  exportState:     false,
  backgroundColor: 0x0F0D0C,
  fontSize:        0.45,
  gaTrackingId:    'UA-68596358-1',
  nodeSearcher: {
    scrollAnimationTime: 100,
    scrollbarOptions: {
      alwaysExpandScrollbar: true,
      alwaysShowScrollbar: 2,
      updateOnSelectorChange: "ul li",
      updateOnContentResize: true
    }
  }
};
