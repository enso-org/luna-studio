analytics = require './gen/analytics'
fs = require 'fs'
yaml = require 'js-yaml'
request = require 'request'
uuidv1 = require 'uuid/v1'
report = require './report'

timeStart = new Date()
runtimeReport = {}
dataPath = if process.env.LUNA_STUDIO_DATA_PATH? then process.env.LUNA_STUDIO_DATA_PATH + '/analytics-data.yml' else './analytics-data.yml'

encoding = 'utf8'

analyticsConfigRequest =
    url: 'https://raw.githubusercontent.com/luna/luna-studio-config/master/analytics.yml'
    headers:
        'User-Agent': 'luna-studio'

frames = []
gatherActive = false
analyseActive = false

gather = =>
    frames.push new Date()
    if gatherActive
        requestAnimationFrame gather

analyse = (callback) =>
    snapshot = frames
    frames = []

    if snapshot.length < 3
        return

    prev = undefined
    min = undefined
    max = undefined
    sum = 0

    for curr in snapshot
        if prev?
            delta = curr - prev
            sum += delta
            if not min? or delta < min
                min = delta
            if not max? or delta > max
                max = delta
        prev = curr

    avg = sum/(snapshot.length - 1)
    avgFps = 1000 / avg
    minFps = 1000 / max
    maxFps = 1000 / min

    callback
        min: minFps
        max: maxFps
        avg: avgFps

startGather = =>
    gatherActive = true
    requestAnimationFrame gather

stopGather = =>
    gatherActive = false

startAnalyse = (interval, callback) =>
    analyseActive = true
    unless gatherActive
        startGather()
    run = =>
        analyse callback
        if analyseActive
            setTimeout run, interval
    run()

stopAnalyse = =>
    analyseActive = false
    stopGather()

isActive = false

document.body.onmouseover = => isActive = true
document.body.onscroll    = => isActive = true
document.body.onkeydown   = => isActive = true

whenActive = (callback) =>
    if isActive
        callback()
        isActive = false

module.exports =
    collect: =>
        first = true

        timeLoaded = new Date()
        runtimeReport.loadingTime = (timeLoaded - timeStart)/1000.0
        timeStart = timeLoaded

        startAnalyse 60000, (fps) =>
            if first
                analytics.track 'Performance.FPS.First', fps
                first = false
            else
                whenActive => analytics.track 'Performance.FPS', fps
                runtimeReport.fps = fps

        fs.readFile dataPath, encoding, (err, data) =>
            if err
                analytics.track 'Stats.FirstRun'
            else
                parsed = yaml.safeLoad data
                if parsed?
                    analytics.track 'Stats.Runtime', parsed, =>
                        fs.writeFileSync dataPath, "", {encoding: encoding}

    readUserInfo: (callback) =>
        userInfoPath = process.env.LUNA_USER_INFO
        unless userInfoPath?
            callback 'LUNA_USER_INFO variable not set', undefined
            return
        fs.readFile userInfoPath, encoding, (err, data) =>
            if err?
                callback err, undefined
            else
                callback undefined, JSON.parse data

    writeUserInfo: (userInfo) =>
        userInfoPath = process.env.LUNA_USER_INFO
        fs.writeFileSync userInfoPath, JSON.stringify userInfo

    readVersionInfo: (callback) =>
        versionInfoPath = process.env.LUNA_VERSION_PATH
        unless versionInfoPath?
            callback 'LUNA_VERSION_PATH variable not set', undefined
            return
        fs.readFile versionInfoPath, encoding, callback

    initialize: ->
        @readUserInfo (error, userInfo) =>
            if error?
                report.displayError 'Cannot read user_info.json', error
            userInfo ?= {}
            unless userInfo.userInfoUUID?
                userInfo.userInfoUUID = uuidv1()
                @writeUserInfo userInfo
            @readVersionInfo (error, version) =>
                if error?
                    report.silentError 'Cannot read version info.', error
                userInfo.version = version
                analytics.setUserInfo userInfo
        try
            request.get analyticsConfigRequest, (err, response, body) =>
                filters = yaml.safeLoad body
                analytics.setFilters filters
                @collect()
        catch error
            report.displayError 'Cannot get analytics configuration', error

    finalize: =>
        timeEnd = new Date()
        runtimeReport.totalTime = (timeEnd - timeStart)/1000.0
        data = yaml.safeDump runtimeReport
        fs.writeFileSync dataPath, data, {encoding:encoding}
