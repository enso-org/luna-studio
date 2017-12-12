mixpanel = require 'mixpanel-browser'

mixpanel.init '0d906436719b047c86b7fee8ae550601'

filters = []

test = (title) =>
    for filter in filters
        if filter.test title
            return true
    return false

filtersSet = false
userInfoSet = false
isConfigured = => filtersSet and userInfoSet

cachedEvents = []


cacheEvent = (title, data) =>
    cachedEvents.push
        title: title
        data: data

module.exports =

    isDevMode: ->
        (@userInfo? and @userInfo.userInfoEmail? and @userInfo.userInfoEmail.endsWith '@luna-lang.org') or process.env.LUNA_STUDIO_DEVELOP?

    setFilters: (strings) ->
        filters = []
        for str in strings
            filters.push new RegExp str
        filtersSet = true
        @sendCached()

    setUserInfo: (@userInfo) ->
        userInfoSet = true
        @sendCached()

    sendCached: ->
        if isConfigured()
            @identify()
            for event in cachedEvents
                @track event.title, event.data
            cachedEvents = []

    identify: ->
        if @userInfo.userInfoUUID?
            if atom.config.get('luna-studio.analyticsEnabled')
                if @isDevMode()
                    console.log 'track.indentify: ', @userInfo.userInfoUUID
                else
                    mixpanel.identify @userInfo.userInfoUUID

    track: (title, data) ->
        data ?= {}
        data.user_info = @userInfo
        if isConfigured()
            if atom.config.get('luna-studio.analyticsEnabled') && test(title)
                if @isDevMode()
                    console.log ("track.accept: " + title), data
                else
                    mixpanel.track title, data
            else
                if @isDevMode()
                    console.log ("track.discard: " + title), data
        else
            cacheEvent title, data
