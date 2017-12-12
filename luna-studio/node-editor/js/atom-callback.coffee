"use strict";

removeFromArray = (array, elt) =>
    index = array.indexOf elt
    array.splice index, 1

listeners =
    onEvent: []

globalRegistry = null
eventFilters = []

module.exports =
    connector: (otherGlobal) => globalRegistry = otherGlobal
    setActiveLocation: (location) =>  globalRegistry.activeLocation = location
    pushNotification: (lvl, msg) =>
        unless atom?
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
            switch lvl
                when 0
                    notification = atom.notifications.addFatalError "Fatal Error",
                        dismissable: true
                        description: msg
                        buttons: [
                            text: 'Copy to clipboard'
                            onDidClick: =>
                                atom.clipboard.write msg
                                return notification.dismiss()
                        ]
                when 1
                    notification = atom.notifications.addError "Error",
                        dismissable: true
                        description: msg
                        buttons: [
                            text: 'Copy to clipboard'
                            onDidClick: =>
                                atom.clipboard.write msg
                                return notification.dismiss()
                        ]
                else
                    notification = atom.notifications.addWarning "Warning",
                        dismissable: true
                        description: msg
                        buttons: [
                            text: 'Copy to clipboard'
                            onDidClick: =>
                                atom.clipboard.write msg
                                return notification.dismiss()
                        ]
    onEvent: (listener) => listeners.onEvent.push listener
    unOnEvent: (listener) => removeFromArray listeners.onEvent, listener
    pushEvent: (data) =>
        listeners.onEvent.forEach (listener) => listener(data)
    setEventFilter: (filters) => eventFilters = filters
    acceptEvent: (eventName) =>
        if eventFilters.length == 0
            return true
        for regexp in eventFilters
            if regexp.test eventName
                return true
        return false
