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

createErrorReport = (msg) =>
    contents = [msg + '\n\n' + '======= logs: =======']
    logs = readLogs()
    for fileName in Object.keys logs
        contents.push '\n=== ' + fileName + ' ===\n' + logs[fileName]
    contents.join '\n'

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
        errorReport = createErrorReport notification.message
        options =
            dismissable: true
            description: notification.message
            buttons: [
                text: 'Copy to clipboard'
                onDidClick: =>
                    atom.clipboard.write errorReport
            ]
        switch notification.level
            when 0
                atom.notifications.addFatalError "Fatal Error", options
            when 1
                atom.notifications.addError "Error", options
            else
                atom.notifications.addWarning "Warning", options
