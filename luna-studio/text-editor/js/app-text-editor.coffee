$$              = require('./common')
config          = require('./config')
atomCallbackInternals    = require('./atom-callback-text-editor')
GoogleAnalytics = require('./GoogleAnalytics')



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

window.processedEvents = []
