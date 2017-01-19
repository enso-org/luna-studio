$$              = require('./common')
config          = require('./config')
brunch          = require('brunch')
websocket       = require('./websocket')
GoogleAnalytics = require('./GoogleAnalytics')

console.info 'Current version ' + brunch.env + ' ' + brunch.git_commit + ' build  ' + brunch.build_number
console.info 'Build at ' + brunch.date

$$.websocket = websocket()

start = ->
  $(document).ready ->
    if window.already_initialized
      console.error 'app already started'
    else
      window.already_initialized = true
      GoogleAnalytics.startGA()
      require('env')()

module.exports =
  start: start
  websocket: $$.websocket

window.processedEvents = []
