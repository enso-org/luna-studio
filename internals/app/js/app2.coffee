$$              = require('./common')
config          = require('./config')
# websocket       = require('./websocket')
atomCallback    = require('./atom-callback2')
GoogleAnalytics = require('./GoogleAnalytics')


# $$.websocket = websocket()

start = ->
  $(document).ready ->
    if window.already_initialized
      console.error 'app already started'
    else
      window.already_initialized = true
      GoogleAnalytics.startGA()
      require('env')().start()

module.exports =
  start: start
  # websocket: $$.websocket

window.processedEvents = []
