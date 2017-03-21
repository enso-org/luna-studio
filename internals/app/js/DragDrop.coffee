$$  = require './common'
app = require './app2'

initDragDrop = ->
  $($$.canvas2D).on 'dragover',  (ev) -> ev.preventDefault()
  $($$.canvas2D).on 'dragenter', (ev) -> ev.preventDefault()
  $($$.canvas2D).on 'drop',      (ev) ->
    CustomEvent = require 'CustomEvent'
    dt = ev.originalEvent.dataTransfer
    files = dt.files
    ev.preventDefault()
    ev.stopPropagation()
    i = 0
    while i < files.length
      reader = new FileReader
      reader.readAsText files[i]
      reader.onload = (e) -> CustomEvent.customEvent 'file.import', reader.result
      i++

module.exports =
  initDragDrop: initDragDrop
