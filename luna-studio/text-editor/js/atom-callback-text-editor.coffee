
removeFromArray = (array, elt) ->
    index = array.indexOf elt
    array.splice index, 1

listeners =
    setBufferListener: []
    setClipboardListener: []
    insertonInsertCode: []
    eventListenerInternal: []
    diffsListener: []
    statusListener: []
    interpreterUpdateListener: []
    lexer: null

module.exports =
    onInsertCode: (listener) -> listeners.insertonInsertCode.push listener
    insertCode: (uri, diffs) ->
        for listener in listeners.insertonInsertCode
            listener uri, diffs

    onSetBuffer: (listener) -> listeners.setBufferListener.push listener
    setBuffer: (data1, data2) ->
        for listener in listeners.setBufferListener
            listener data1, data2

    onSetClipboard: (listener) -> listeners.setClipboardListener.push listener
    setClipboard: (data1, data2) ->
        for listener in listeners.setClipboardListener
            listener data1, data2

    onStatus: (listener) -> listeners.statusListener.push listener
    pushStatus: (data1, data2, data3) ->
        for listener in listeners.statusListener
            listener data1, data2, data3

    onInterpreterUpdate: (listener) -> listeners.interpreterUpdateListener.push listener
    pushInterpreterUpdate: (data1, data2, data3) ->
        for listener in listeners.interpreterUpdateListener
            listener data1, data2, data3

    subscribeEventListenerInternal: (listener) -> listeners.eventListenerInternal.push listener
    unsubscribeEventListenerInternal: (listener) -> removeFromArray listeners.eventListenerInternal, listener
    pushInternalEvent: (data) ->
        for listener in listeners.eventListenerInternal
            listener(data)

    subscribeDiffs: (listener) -> listeners.diffsListener.push listener
    unsubscribeDiffs: (listener) -> removeFromArray listeners.diffsListener, listener
    pushDiffs: (diffs) ->
        for listener in listeners.diffsListener
            listener diffs

    setLexer: (lexer) -> listeners.lexer = lexer
    unsetLexer: -> listeners.lexer = null
    lex: (stack, data) -> listeners.lexer stack, data
