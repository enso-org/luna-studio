{View} = require 'atom-space-pen-views'

interpreterStartPauseClasses = 'btn icon luna-interpreter-startpause '
interpreterStartClasses = interpreterStartPauseClasses + 'icon-playback-play'
interpreterPauseClasses = interpreterStartPauseClasses + 'icon-playback-pause'

module.exports =
class Statusbar extends View
    constructor: (@codeEditor) ->
        super

    @content: ->
        @div class: 'luna-statusbar inline-block', =>
            @div class: 'btn-group inline-block luna-interpreter-buttons', =>
                @button
                    class: interpreterPauseClasses
                    outlet: 'startPauseButton'
                @button
                    class: 'btn icon icon-sync luna-interpreter-reload'
                    outlet: 'reloadButton'
            @div
                class: 'luna-interpreter-status inline-block'
                outlet: 'statusLabel'
                'Interpreter running'

    initialize: =>
        @startPauseButton.on 'click', @startPause
        @reloadButton.on 'click', @reload
        @codeEditor.onInterpreterUpdate (update, status) =>
            if update == 'Start'
                @unlockPause()
            else if update == 'Pause'
                @unlockStart()
            else if update == 'Reload'
                @unlockReload()
            else if update == 'Update'
                @setStatus(status)

    startPause: =>
        if @startPauseButton[0].className == interpreterStartClasses
            @codeEditor.pushInternalEvent(tag: "InterpreterStart")
        else
            @codeEditor.pushInternalEvent(tag: "InterpreterPause")
        @startPauseButton[0].disabled = true

    reload: =>
        @codeEditor.pushInternalEvent(tag: "InterpreterReload")
        @reloadButton[0].disabled = true

    unlockPause: =>
        @startPauseButton[0].className = interpreterPauseClasses
        @startPauseButton[0].disabled = false

    unlockStart: =>
        @startPauseButton[0].className = interpreterStartClasses
        @startPauseButton[0].disabled = false

    unlockReload: =>
        @reloadButton[0].disabled = false

    setStatus: (status) =>
        @statusLabel[0].innerText = status

    getTitle: -> 'Status bar'
