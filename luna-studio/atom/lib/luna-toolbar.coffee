{View} = require 'atom-space-pen-views'
logo   = require 'luna-logo'
shell  = require 'shell'

docsUrl = 'http://docs.luna-lang.org'
chatUrl = 'http://chat.luna-lang.org'
logoSize = 24

module.exports =
class LunaToolbar extends View
    constructor: (@codeEditor) ->
        super

    @content: ->
        @div
            class: 'luna-toolbar'
            =>
                @div
                    class: 'luna-toolbar__logo-container'
                    outlet: 'logoContainer'
                    =>
                        @div
                            class: 'luna-toolbar__logo'
                            outlet: 'buttonLogo'
                @div
                    class: 'luna-toolbar__buttons-container'
                    outlet: 'buttonContainer'
                    =>
                        @div
                            class: 'luna-toolbar__button luna-toolbar__button-left'
                            outlet: 'buttonDocs'
                            'Documentation'
                        @div
                            class: 'luna-toolbar__button luna-toolbar__button-right'
                            outlet: 'buttonSupport'
                            'Community support'
    initialize: =>
        target = atom.views.getView atom.workspace
        @buttonLogo.on    'click', -> atom.commands.dispatch target, 'luna-studio:welcome'
        @buttonDocs.on    'click', -> shell.openExternal docsUrl
        @buttonSupport.on 'click', -> shell.openExternal chatUrl

    attach: =>
        @panel ?= atom.workspace.addHeaderPanel({item: this, visible: false})
        @previouslyFocusedElement = document.activeElement
        @buttonLogo[0].innerHTML = logo.generateInAppLogo logoSize
        @panel.show()

    detach: =>
        if @panel and @panel.isVisible()
            @panel.hide()
            @previouslyFocusedElement?.focus()

    getTitle: -> 'Luna toolbar'
