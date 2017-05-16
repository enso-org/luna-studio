module.exports =
  websocket: undefined
  isGAEnabled: ->
    !(localStorage.getItem('ga') == '0')
  enableGA: (val) ->
    localStorage.setItem 'ga', if val then 1 else 0
    alert 'Ok, Google Analytics will be ' + (if val then 'enabled' else 'disabled') + ' after you reload the page.'
    return

window.$$ = module.exports
