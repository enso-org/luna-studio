$$              = require('./common')
config          = require('./config')

atomCallback            = require('./atom-callback')
GoogleAnalytics         = require('./GoogleAnalytics')
console.log('DUPADUPADUPA');
gzip                    = require('./gzip')
console.log(gzip);
console.log('DUPADUPADUPA');

window.visualizerFramesManager = require('./visualizers')
window.gzip = require('./gzip');

start = ->
  $(document).ready ->
    if window.already_initialized
      console.error 'app already started'
    else
      window.already_initialized = true
      GoogleAnalytics.startGA()
      require('env-node-editor')().start("")

module.exports =
  start: start

window.processedEvents = []
