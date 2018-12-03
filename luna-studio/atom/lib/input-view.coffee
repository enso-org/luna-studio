fs   = require 'fs-plus'
path = require 'path'
{TextEditor, CompositeDisposable, Disposable} = require 'atom'

camelize = (string) ->
    if string
        string = string.replace /[ _-]+(\w)/g, (m) -> m.slice(-1).toUpperCase()
        string.charAt(0).toUpperCase() + string.slice(1);
    else
      ''

module.exports =
class InputView
    previouslyFocusedElement: null
    mode: null

    constructor: ->
        @disposables = new CompositeDisposable

        @element = document.createElement('div')
        @element.classList.add('package-generator')

        @miniEditor = new TextEditor({mini: true})
        @element.appendChild(@miniEditor.element)

        @error = document.createElement('div')
        @error.classList.add('error')
        @element.appendChild(@error)

        @message = document.createElement('div')
        @message.classList.add('message')
        @element.appendChild(@message)

        @disposables.add atom.commands.add 'atom-workspace',
            'package-generator:generate-package': => @attach('package')
            'package-generator:generate-syntax-theme': => @attach('theme')

        blurHandler = => @detach()
        @miniEditor.element.addEventListener('blur', blurHandler)
        @disposables.add(new Disposable(=> @miniEditor.element.removeEventListener('blur', blurHandler)))
        @disposables.add atom.commands.add @element,
            'core:confirm': => @confirm()
            'core:cancel': => @detach()

    destroy: ->
        @panel?.destroy()
        @disposables.dispose()

    attach: (@label = '', @valuePrefix = '', @defaultValue = '', @checkValid = (-> true), @invalidMsg = (-> 'Invalid input'), @callback = ->) ->
        @panel ?= atom.workspace.addModalPanel({item: this, visible: false})
        @previouslyFocusedElement = document.activeElement
        @panel.show()
        @message.textContent = @label
        @setInputText(@defaultValue)
        @miniEditor.element.focus()

    detach: ->
        return unless @panel.isVisible()
        @panel.hide()
        @previouslyFocusedElement?.focus()

    confirm: ->
        if @valid()
            @callback(@getInputText())
            @detach()

    valid: ->
        if @checkValid(@getInputText())
            true
        else
            @error.textContent = @invalidMsg(@getInputText())
            @error.style.display = 'block'
            false

    setInputText: (placeholderName, rangeToSelect) ->
        placeholderName ?= ''
        rangeToSelect ?= [0, placeholderName.length]
        @miniEditor.setText(path.join(@valuePrefix, placeholderName))
        pathLength = @miniEditor.getText().length
        endOfDirectoryIndex = pathLength - placeholderName.length
        @miniEditor.setSelectedBufferRange([[0, endOfDirectoryIndex + rangeToSelect[0]], [0, endOfDirectoryIndex + rangeToSelect[1]]])

    getInputText: ->
        packagePath = fs.normalize(@miniEditor.getText().trim())
        packageName = camelize(path.basename(packagePath))
        path.join(path.dirname(packagePath), packageName)
