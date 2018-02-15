{View} = require 'atom-space-pen-views'
logo   = require 'luna-logo'

module.exports =
class LunaToolbar extends View
    constructor: (@codeEditor) ->
        super

    @content: ->
        @div
            class: 'luna-toolbar'
            =>
                @div
                    class: 'luna-toolbar__container'
                    outlet: 'logoContainer'
                    =>
                        @div
                            class: 'luna-toolbar__button luna-toolbar__logo'
                            outlet: 'buttonLogo'
                @div
                    class: 'luna-toolbar__container'
                    outlet: 'buttonContainer'
                    =>
                        @div
                            class: 'luna-toolbar__button icon-plus'
                            outlet: 'buttonNew'
                        @div
                            class: 'luna-toolbar__button icon-search'
                            outlet: 'buttonSearch'
                        @div
                            class: 'luna-toolbar__button icon-gear'
                            outlet: 'buttonSettings'
    initialize: =>
        target = atom.views.getView atom.workspace
        connectButton = (btn, action) =>
            btn.on 'click', ->
                atom.commands.dispatch target, action

        connectButton @buttonLogo    , 'luna-studio:welcome'
        connectButton @buttonNew     , 'application:new-file'
        connectButton @buttonSearch  , 'fuzzy-finder:toggle-file-finder'
        connectButton @buttonSettings, 'application:show-settings'


    attach: =>
        @panel ?= atom.workspace.addLeftPanel({item: this, visible: false})
        @previouslyFocusedElement = document.activeElement
        @buttonLogo[0].innerHTML = logo.generateInAppLogo 48
        @panel.show()

    detach: =>
        if @panel and @panel.isVisible()
            @panel.hide()
            @previouslyFocusedElement?.focus()

    getTitle: -> 'Luna toolbar'
