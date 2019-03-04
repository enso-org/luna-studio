$$              = require('./common')
config          = require('./config')

atomCallback            = require('./atom-callback')

window.visualizerFramesManager = require('./visualizers')
window.searcherEngine = require('fuzzly')

start = ->
  $(document).ready ->
    if window.already_initialized
      console.error 'app already started'
    else
      window.already_initialized = true
      require('env-node-editor')().start("")

module.exports =
  start: start

window.processedEvents = []
