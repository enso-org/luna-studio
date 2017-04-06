path   = require 'path'
{View} = require 'atom-space-pen-views'
i      = require "./gen/ghcjs-code2.js"
internal = i()

module.exports =
class LunaEditorTab extends View
    constructor: (@uri) ->
        uri = @uri
        @element = document.createElement('atom-text-editor')
        console.log(@editor)
        # @buffer = @element.buffer
        # @buffer.setPath(uri)
        internal.pushInternalEvent("GetBuffer " + uri)

        # withoutTrigger = (callback) ->
        #     @triggerPush = false
        #     callback()
        #     @triggerPush = true
        #
        # setBuffer = (uri_send, text) ->
        #     withoutTrigger =>
        #         console.log(uri, uri_send, text)
        #         if uri == uri_send
        #             @element.setText(text)
        # internal.bufferListener setBuffer

    getTitle:     -> @uri
