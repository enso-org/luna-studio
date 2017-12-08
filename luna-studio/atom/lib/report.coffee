analytics = require './gen/analytics'

trackError = (title, detail) =>
    analytics.track 'LunaStudio.Error', {title: title, detail: detail}

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
