request = require 'request'
yaml    = require 'js-yaml'
stats   = require './stats'


versionRequestOpts =
    url: 'http://packages.luna-lang.org/config.yaml'
    headers:
        'User-Agent': 'luna-studio'


class Version
    constructor: (@major, @minor) ->

parseVersion = (str) ->
    r = /^(\d+)\.(\d+)/.exec str
    new Version r[1], r[2] if r?

compareVersion = (a, b) ->
    if a.major < b.major then return -1
    if a.major > b.major then return 1
    if a.minor < b.minor then return -1
    if a.minor > b.minor then return 1
    return 0

filterArch = (objs, osKey) ->
    filtered = []
    for key in Object.keys objs
        if objs[key][osKey]?
            filtered.push key
    return filtered

checkUpdates = (callback) =>
    try
        fail = (msg) -> callback
            error: msg
        stats.readVersionInfo (err, versionInfo) =>
            if err
                fail 'Cannot read version info: ' + err.message
                return
            appVersion = parseVersion versionInfo
            stats.readUserInfo (err, userInfo) =>
                osType = userInfo?.userInfoOsType
                osArch = userInfo?.userInfoArch
                unless osType? and osArch
                    fail 'Cannot read user info'
                    return
                osKey = osType.toLowerCase() + '.' + osArch.toLowerCase()
                console.log osKey
                request.get versionRequestOpts, (err, response, body) =>
                    parsed = yaml.safeLoad(body)
                    if body?
                        versionsObj = parsed?.packages?['luna-studio']?.versions
                        versionsObj ?= {}
                        versionsList = []
                        for versionStr in filterArch versionsObj, osKey
                            v = parseVersion versionStr
                            versionsList.push v if v?
                        versionsList.sort compareVersion
                        newestAvailable = versionsList[versionsList.length - 1]
                        if newestAvailable? and appVersion? and 1 == compareVersion newestAvailable, appVersion
                            callback newestAvailable
                    else
                        fail 'Cannot parse updates file.'
    catch error
        fail 'Updates check failed: ' + error.message

module.exports =
    checkUpdates: checkUpdates
