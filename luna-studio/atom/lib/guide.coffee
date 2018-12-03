{View}    = require 'atom-space-pen-views'
analytics = require './gen/analytics'
fs        = require 'fs-plus'
path      = require 'path'
yaml      = require 'js-yaml'
{VM}      = require 'vm2'
showdown  = require 'showdown'
converter = new showdown.Converter()
report    = require './report'

vm     = new VM
            timeout: 1000
            sandbox:
                window: window
                storage: {}

welcomeGuide = steps: []

encoding = 'utf8'
highlightClass = 'luna-guide__highlight'
pointerWindowClass = 'luna-guide__pointer--window'

module.exports =
    class VisualGuide extends View
        constructor: (@nodeEditor) ->
            super

        @content: ->
            @div =>
                @div class: 'luna-guide__pointer', outlet: 'pointer'
                @div class: 'luna-guide__message-positioner', =>
                    @div class: 'luna-guide__message', outlet: 'messageBox', =>
                        @div
                            class: 'luna-guide__title'
                            outlet: 'guideTitle'
                        @div
                            class: 'luna-guide__description'
                            outlet: 'guideDescription'
                        @button
                            outlet: 'buttonHide'
                            class: 'luna-guide__close-icon close icon icon-x'
                        @div class: 'luna-guide__buttons', =>
                            @button
                                outlet: 'buttonContinue'
                                class: 'luna-guide__button luna-guide__button--continue'
                                'Next'
                            @button
                                outlet: 'buttonDoIt'
                                class: 'luna-guide__button luna-guide__button--doit'
                                'Next'
                            # @button
                            #     outlet: 'buttonDisable'
                            #     class: 'luna-guide__button luna-guide__button--disable luna-guide__button--link'
                            #     'Do not show again'

        initialize: =>
            @buttonHide.on 'click', @detach
            # @buttonDisable.on 'click', @disable
            @buttonContinue.on 'click', (e) =>
                if e.ctrlKey and e.shiftKey
                    @fastForward()
                @proceed()
            @buttonContinue.hide()
            @buttonDoIt.on 'click', (e) =>
                if e.ctrlKey and e.shiftKey
                    @fastForward()
                @buttonDoIt.hide()
                @doIt()
            @buttonDoIt.hide()

        nextStep: (nextStepNo) =>
            if nextStepNo != @nextStepNo
                return

            if @currentStep? and @currentStep.after?
                try
                    vm.run @currentStep.after
                catch error
                    report.silentError 'Error while running guide "after" step', error

            @unsetHighlightedElem()

            projectPath = atom.project.getPaths()[0]
            projectPath ?= '(None)'
            analytics.track 'LunaStudio.Guide.Step',
                number: @nextStepNo
                name: path.basename projectPath
                path: projectPath
            @currentStep = @guide.steps[@nextStepNo]
            @nextStepNo++

            if @currentStep?
                @target = @currentStep.target
                @target ?= {}
                @target.action ?= 'proceed'

                # msgBoxDefaultOffset = 20
                # @msgBoxOffset = @currentStep.offset
                # @msgBoxOffset ?= {}
                # @msgBoxOffset.left   ?= msgBoxDefaultOffset
                # @msgBoxOffset.right  ?= msgBoxDefaultOffset
                # @msgBoxOffset.top    ?= msgBoxDefaultOffset
                # @msgBoxOffset.bottom ?= msgBoxDefaultOffset
                @displayStep()
            else
                @detach()


        findTargetedElem: =>
            targetedElem = null
            if @target.className
                if typeof @target.className == 'string'
                    targetedElem = document.getElementsByClassName(@target.className)[0]
                else
                    for t in @target.className
                        if targetedElem?
                            targetedElem = targetedElem.getElementsByClassName(t)[0]
                        else
                            targetedElem = document.getElementsByClassName(t)[0]
                        unless targetedElem?
                            break
            else if @target.id
                targetedElem = document.getElementById(@target.id)
            else if @target.custom
                targetedElem = vm.run @target.custom
            return targetedElem

        setHighlightedElem: =>
            @highlightedElem = @findTargetedElem()
            if @highlightedElem?
                @highlightedElem.classList.add highlightClass
            @updatePointer()

        unsetHighlightedElem: =>
            if @highlightedElem?
                @highlightedElem.classList.remove highlightClass
                @highlightedElem = null
            @updatePointer()

        installHandlers: =>
            if @highlightedElem?
                hgElem = @highlightedElem
                nextStepNo = @nextStepNo
                highlightedRect = hgElem.getBoundingClientRect()
                if highlightedRect.width != 0 and highlightedRect.height != 0
                    if @target.action is 'value'
                        @buttonDoIt.show()
                        oldHandlers = hgElem.oninput
                        hgElem.oninput = =>
                            if hgElem? and (hgElem.value is @target.value)
                                hgElem.oninput = oldHandlers
                                if not @currentStep.expected?.length
                                    setTimeout => @nextStep nextStepNo
                    else if @target.action.includes ':'
                        @buttonDoIt.show()
                        handler = {}
                        handler[@target.action] = =>
                            @disposable.dispose()
                            if not @currentStep.expected?.length
                                setTimeout => @nextStep nextStepNo
                        @disposable = atom.commands.add hgElem, handler
                    else if hgElem?
                        @buttonDoIt.show()
                        oldHandlers = hgElem[@target.action]
                        hgElem[@target.action] = =>
                            if hgElem?
                                hgElem[@target.action] = oldHandlers
                                if not @currentStep.expected?.length
                                    setTimeout => @nextStep nextStepNo

        proceed: =>
            @buttonContinue.hide()
            @nextStep @nextStepNo

        doIt: =>
            mkEvent = (name) => new Event name,
                                    view: window
                                    bubbles: true
                                    cancelable: true
            if @target.action is 'proceed'
                @proceed()
            else if @highlightedElem?
                if @target.action is 'value'
                    event = mkEvent 'input',
                    @highlightedElem.value = @target.value
                    event.simulated = true
                    @highlightedElem.dispatchEvent(event)
                else if @target.action.includes ':'
                    view = atom.views.getView @highlightedElem
                    atom.commands.dispatch view, @target.action, @target.payload
                else if @highlightedElem?
                    if @target.action.startsWith 'on'
                        action = @target.action.slice 2
                    else
                        action = @target.action
                    if action is 'click'
                        @highlightedElem.dispatchEvent mkEvent 'mousedown'
                        @highlightedElem.dispatchEvent mkEvent 'mouseup'
                    @highlightedElem.dispatchEvent mkEvent action

        setEventFilter: =>
            prepareRestriction = (entry) ->
                regexp = if entry.regexp? then entry.regexp else entry
                result =
                    regexp:        new RegExp regexp
                    nodeName:      entry.nodeName
                    portId:        entry.portId
                    searcherInput: entry.searcherInput
                return result;

            @currentStep.allow ?= []
            allowed = @currentStep.allow.map prepareRestriction

            @currentStep.block ?= []
            blocked = @currentStep.block.map prepareRestriction

            @currentStep.expected ?= []
            expected = @currentStep.expected.map prepareRestriction

            @nodeEditor.setEventFilter blocked, allowed, expected

        displayStep: (retry = false) =>
            @setHighlightedElem()
            windowRect = document.body.getBoundingClientRect()

            if @target.action is 'proceed'
                @buttonContinue.show()
            else if not @highlightedElem?
                @buttonDoIt.hide()
                unless retry
                    @guideTitle[0].innerText = @currentStep.title
                    @guideDescription[0].innerText = 'Please wait...'
                    msgBoxRect = @messageBox[0].getBoundingClientRect()
                    # msgBoxLeft = (windowRect.width - msgBoxRect.width)/2
                    # msgBoxTop  = (windowRect.height - msgBoxRect.height)/2
                    # @messageBox[0].style.top = msgBoxTop + 'px'
                    # @messageBox[0].style.left = msgBoxLeft + 'px'

                setTimeout (=> @displayStep(true)), 300
                return

            @setEventFilter()
            @installHandlers()

            @guideTitle[0].innerText = @currentStep.title
            @guideDescription[0].innerHTML = converter.makeHtml @currentStep.description
            msgBoxRect = @messageBox[0].getBoundingClientRect()
            # msgBoxLeft = (windowRect.width - msgBoxRect.width)/2
            # msgBoxTop  = (windowRect.height - msgBoxRect.height)/2

            if @highlightedElem?
                highlightedRect = @highlightedElem.getBoundingClientRect()
                # if highlightedRect.width != 0 and highlightedRect.height != 0
                #     if highlightedRect.left > msgBoxRect.width + @msgBoxOffset.left
                #         msgBoxLeft = highlightedRect.left - msgBoxRect.width - @msgBoxOffset.left
                #         msgBoxTop = highlightedRect.top + highlightedRect.height/2 - msgBoxRect.height/2
                #     else if highlightedRect.right + msgBoxRect.width + @msgBoxOffset.right < windowRect.width
                #         msgBoxLeft = highlightedRect.right + @msgBoxOffset.right
                #         msgBoxTop = highlightedRect.top + highlightedRect.height/2 - msgBoxRect.height/2
                #     else if highlightedRect.top > msgBoxRect.height + @msgBoxOffset.top
                #         msgBoxTop = highlightedRect.top - msgBoxRect.height - @msgBoxOffset.top
                #     else if highlightedRect.bottom + msgBoxRect.height + @msgBoxOffset.bottom < windowRect.height
                #         msgBoxTop = highlightedRect.bottom + @msgBoxOffset.bottom


            # @messageBox[0].style.top = msgBoxTop + 'px'
            # @messageBox[0].style.left = msgBoxLeft + 'px'

            if @highlightedElem?
                @retryWhenHighlightedElementIsGone()

        retryWhenHighlightedElementIsGone: =>
            targetedElem = @findTargetedElem()
            if targetedElem? and targetedElem.classList.contains highlightClass
                setTimeout @retryWhenHighlightedElementIsGone, 100
            else
                @displayStep(retry: true)


        updatePointer: =>
            if @highlightedElem?
                if (@highlightedElem.classList.contains 'luna-studio-window') or (@highlightedElem.classList.contains 'luna-studio-mount')
                    @pointer[0].classList.add pointerWindowClass
                else
                    @pointer[0].classList.remove pointerWindowClass
                if @isVisible()
                    highlightedRect = @highlightedElem.getBoundingClientRect()
                    nodeEditorElem = @highlightedElem
                    loop
                        if typeof nodeEditorElem.className is 'string'
                            if nodeEditorElem.className.split(' ').indexOf('luna-studio-window') != -1 or nodeEditorElem.className.split(' ').indexOf('luna-studio-mount') != -1 then break
                        break unless nodeEditorElem = nodeEditorElem.parentNode

                    if nodeEditorElem
                        nodeEditorRect   = nodeEditorElem.getBoundingClientRect()
                        leftNotVisible   = highlightedRect.left   < nodeEditorRect.left
                        rightNotVisible  = highlightedRect.right  > nodeEditorRect.right
                        topNotVisible    = highlightedRect.top    < nodeEditorRect.top
                        bottomNotVisible = highlightedRect.bottom > nodeEditorRect.bottom
                        if leftNotVisible or rightNotVisible or topNotVisible or bottomNotVisible or @isNodeEditorElementCovered()
                            @pointer.hide()
                        else
                            @pointer.show()
                            @pointer[0].style.width  = highlightedRect.width + 'px'
                            @pointer[0].style.height = highlightedRect.height + 'px'
                            @pointer[0].style.top    = highlightedRect.top + 'px'
                            @pointer[0].style.left   = highlightedRect.left + 'px'
                    else
                        @pointer.show()
                        @pointer[0].style.width  = highlightedRect.width + 'px'
                        @pointer[0].style.height = highlightedRect.height + 'px'
                        @pointer[0].style.top    = highlightedRect.top + 'px'
                        @pointer[0].style.left   = highlightedRect.left + 'px'
                else
                    @pointer.hide()
                window.requestAnimationFrame @updatePointer
            else
                @pointer.hide()

        attach: =>
            @panel ?= atom.workspace.addHeaderPanel({item: this, visible: false})
            @panel.show()
            @nodeEditor.onExpectedEvent =>
                @proceed()


        detach: =>
            if @panel.isVisible()
                @nodeEditor.setEventFilter [], [], []
                @panel.hide()
            @nodeEditor.onExpectedEvent null


        disable: =>
            @disableGuide()
            @detach()

        startProject: =>
            projectPath = atom.project.getPaths()[0]
            if projectPath?
                guidePath = path.join projectPath, 'guide.yml'
                fs.readFile guidePath, encoding, (err, data) =>
                    unless err
                        parsed = yaml.safeLoad data
                        if parsed? && not parsed.disabled
                            @start parsed, guidePath
                            return
                    @detach()

        disableGuide: =>
            if @guidePath?
                @guide.disabled = true
                data = yaml.safeDump(@guide)
                fs.writeFile @guidePath, data, encoding, (err) =>
                if err?
                    report.silentError 'Error when disabling guide', err
            else
                atom.config.set('luna-studio.showWelcomeGuide', false)


        start: (@guide, @guidePath) =>
            @guide ?= welcomeGuide
            @nextStepNo = 0
            @attach()
            @nextStep 0


        isNodeEditorElementCovered: =>
            elem = @highlightedElem
            corners = [ {x: elem.getBoundingClientRect().left + 10,  y: elem.getBoundingClientRect().top + 10}
                      , {x: elem.getBoundingClientRect().left + 10,  y: elem.getBoundingClientRect().bottom - 10}
                      , {x: elem.getBoundingClientRect().right - 10, y: elem.getBoundingClientRect().top + 10}
                      , {x: elem.getBoundingClientRect().right - 10, y: elem.getBoundingClientRect().bottom - 10}
                      ]
            for p, i in corners
                pointContainer = document.elementFromPoint p.x, p.y
                nodeEditorFound = false
                loop
                    if typeof pointContainer.className is 'string'
                        if pointContainer.className.indexOf('luna-studio-window') != -1 or pointContainer.className.indexOf('luna-studio-mount') != -1
                            nodeEditorFound = true
                            break
                    break unless pointContainer = pointContainer.parentNode
                if not nodeEditorFound
                    return true
            return false

        isVisible: =>
            elem = @highlightedElem
            style = getComputedStyle elem
            if style.display is 'none' then return false
            if style.visibility isnt 'visible' then return false
            if (elem.offsetWidth + elem.offsetHeight + elem.getBoundingClientRect().height + elem.getBoundingClientRect().width is 0)
                return false
            return true

        fastForward: =>
            @doIt()
            if @nextStepNo < @guide.steps.length
                setTimeout @fastForward, 100