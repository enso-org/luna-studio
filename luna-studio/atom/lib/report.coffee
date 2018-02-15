fs        = require 'fs'
path      = require 'path'
shell     = require 'shell'

analytics = require './gen/analytics'

trackError = (title, detail) =>
    analytics.track 'LunaStudio.Error', {title: title, detail: detail}

encoding = 'utf8'
logsPath = process.env.LUNA_STUDIO_LOG_PATH

filePatttern = /.*empire.*/

readLogs = =>
    logData = {}
    try
        for fileName in fs.readdirSync logsPath
            if filePatttern.test fileName
                filePath = path.join logsPath, fileName
                try
                    data = fs.readFileSync filePath, {encoding: encoding}
                    lines = data.split '\n'
                    data = lines.splice(-100).join '\n'
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
        copyOptions =
            dismissable: true
            description: notification.message
            buttons: [
                text: 'Copy to clipboard'
                onDidClick: => atom.clipboard.write errorReport
            ]
        reportOptions =
            dismissable: true
            description: notification.message + '<br>Please report this bug on our github'
            buttons: [
                text: 'Copy to clipboard and report'
                onDidClick: =>
                    atom.clipboard.write errorReport
                    shell.openExternal 'https://github.com/luna/luna-studio/issues'
            ]
        switch notification.level
            when 0
                atom.notifications.addError "Fatal Error", reportOptions
            when 1
                atom.notifications.addError "Error", reportOptions
            else
                atom.notifications.addWarning "Warning", copyOptions
