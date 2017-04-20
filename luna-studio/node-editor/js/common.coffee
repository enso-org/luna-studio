commonUniforms =
  camFactor:     {type: 'f', value: 1.0}
  antialias:     {type: 'i', value: 0  }
  objectMap:     {type: 'i', value: 0  }
  dpr:           {type: 'f', value: 1.0}
  aa:            {type: 'f', value: 0.8}
  zoomScaling:   {type: 'i', value: 0  }
  connectionPen: {type: 'i', value: 0  }
  isConnecting:  {type: 'i', value: 0  }

module.exports =
  commonUniforms: commonUniforms
  camFactor: commonUniforms.camFactor
  currentConnection: null
  scene: undefined
  camera: undefined
  renderer: undefined
  htmlCanvasPan: undefined
  htmlCanvas: undefined
  nodeSearcher: undefined
  websocket: undefined
  lastFactor: 1.0
  registry: {}
  isGAEnabled: ->
    !(localStorage.getItem('ga') == '0')
  enableGA: (val) ->
    localStorage.setItem 'ga', if val then 1 else 0
    alert 'Ok, Google Analytics will be ' + (if val then 'enabled' else 'disabled') + ' after you reload the page.'
    return

window.$$ = module.exports
