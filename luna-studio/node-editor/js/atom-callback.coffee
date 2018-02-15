removeFromArray = (array, elt) =>
    index = array.indexOf elt
    array.splice index, 1

listeners =
    onEvent: []
    onNotification: []

globalRegistry = null
eventFilters =
    blockedEvents: []
    allowedEvents: []
    expectedEvents: []

module.exports =
    connector: (otherGlobal) => globalRegistry = otherGlobal
    setActiveLocation: (location) =>  globalRegistry.activeLocation = location
    pushNotification: (lvl, msg) =>
        if listeners.onNotification.length == 0
            switch lvl
                when 0, 1
                    console.error msg
                    break
                when 2
                    console.warn msg
                    break
                else
                    console.log msg
        else
            listeners.onNotification.forEach (callback) ->
                callback
                    level: lvl
                    message: msg
    onNotification: (listener) => listeners.onNotification.push listener
    onEvent: (listener) => listeners.onEvent.push listener
    unOnEvent: (listener) => removeFromArray listeners.onEvent, listener
    pushEvent: (data) =>
        listeners.onEvent.forEach (listener) => listener(data)
    setEventFilter: (blocked, allowed, expected) =>
        eventFilters = { blockedEvents: blocked, allowedEvents: allowed, expectedEvents: expected }
    onExpectedEvent: (callback) => listeners.onExpectedEvent = callback
    acceptEvent: (event) =>
        event = JSON.parse event
        eventMatchesRestriction = (evt, restriction) ->
            nodeInfo = evt.eventInfo.nodeInfo
            nodeName = nodeInfo?.nodeName
            portId   = nodeInfo?.portInfo?.portId
            searcherInput = evt.eventInfo.searcherInfo?.input
            eventNameMatches     = restriction.regexp.test event.name
            nodeNameMatches      = (not restriction.nodeName?)      or (restriction.nodeName      == nodeName)
            portIdMatches        = (not restriction.portId?)        or (restriction.portId        == portId)
            searcherInputMatches = (not restriction.searcherInput?) or (restriction.searcherInput == searcherInput)
            eventNameMatches and nodeNameMatches and portIdMatches and searcherInputMatches
        isExpected = eventFilters.expectedEvents.some((restriction) -> eventMatchesRestriction event, restriction)
        if isExpected
            listeners?.onExpectedEvent?()
            isExpected
        else
            noRestrictions = eventFilters.blockedEvents.length == 0 and eventFilters.allowedEvents.length == 0
            matchesAllowed = eventFilters.allowedEvents.some((restriction) -> eventMatchesRestriction event, restriction)
            matchesBlocked = eventFilters.blockedEvents.some((restriction) -> eventMatchesRestriction event, restriction)
            noRestrictions or matchesAllowed or not (eventFilters.blockedEvents.length == 0 or matchesBlocked)