fs        = require 'fs'
path      = require 'path'

analytics = require './gen/analytics'

trackError = (title, detail) =>
    analytics.track 'LunaStudio.Error', {title: title, detail: detail}

encoding = 'utf8'
logsPath = process.env.LUNA_STUDIO_LOG_PATH

readLogs = =>
    logData = {}
    try
        for fileName in fs.readdirSync logsPath
            console.log fileName
            filePath = path.join logsPath, fileName
            try
                data = fs.readFileSync filePath, {encoding: encoding}
                logData[fileName] = data
            catch error
                console.error error
    catch error
        console.error error
    return logData


module.exports =
    displayError: (title, detail) =>
        trackError title, detail
        atom.confirm
            message: title.toString()
            detailedMessage: detail.toString()
            buttons:
                Ok: ->
    silentError: (title, detail) =>
        trackError title, detail
        console.error title, detail

    onNotification: (notification) =>
        options =
            dismissable: true
            description: msg
            buttons: [
                text: 'Copy to clipboard'
                onDidClick: =>
                    atom.clipboard.write msg
                    return notification.dismiss()
            ]
        switch notification.lvl
            when 0
                atom.notifications.addFatalError "Fatal Error", options
            when 1
                atom.notifications.addError "Error", options
            else
                atom.notifications.addWarning "Warning", options
