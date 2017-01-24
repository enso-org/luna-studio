cleanupApp = -> $('body > div, body > canvas').remove()

connectionClosed = ->
  if $('#rejected').length == 0
    cleanupApp()
    $('body').append require('templates/connection_closed')()
    ga 'send', 'event', 'Diagnostic', 'ConnectionLost'

appCrashed = (pat) ->
  # poor man's vprintf
  str = pat
  i = 1
  while i < arguments.length
    str = str.replace(/%s/, arguments[i])
    i++
  console.error 'App crashed with error:', str
  $('body').append require('templates/bsod')(message: str)
  ga 'send', 'event', 'Diagnostic', 'BSOD', str

module.exports =
  connectionClosed: connectionClosed
  appCrashed: appCrashed
