path = require 'path'
fs   = require 'fs'

visPath         = path.join __dirname, 'visualizers'
dataVisName     = 'data'
internalVisName = 'internal'

listVisualizers = (visPath) -> (fs.readdirSync visPath).filter((p) -> fs.existsSync(path.join(visPath, p, "config.js")))

listInternalVisualizers = (visPath) -> (fs.readdirSync visPath).filter((p) -> p == internalVisName && fs.existsSync(path.join(visPath, p, "config.js")))

resolveVis = (p, name) ->
    normalizeVis p, name, require(path.join p, name, "config.js")

resolveInternalVis = (p, name) ->
    visConf = require(path.join p, name, "config.js")
    filesToLoad = visConf()
    if filesToLoad?
        f.path = path.join(name, f.path) for f in filesToLoad
        JSON.stringify(filesToLoad)
    else JSON.stringify(null)

normalizeVis = (p, name, visConf) -> (cons) ->
    filesToLoad = visConf (JSON.parse cons)
    if filesToLoad?
        f.path = path.join(dataVisName, name, f.path) for f in filesToLoad
        JSON.stringify(filesToLoad)
    else JSON.stringify(null)

setupConfigMap = (visPath) ->
    visDataPath = path.join visPath, dataVisName
    visualizers = listVisualizers(visDataPath)
    internalVisualizers = listInternalVisualizers(visPath)
    result = {}
    result[n] = resolveVis visDataPath, n for n in visualizers
    internalResult = {}
    for n in internalVisualizers
        entries = JSON.parse(resolveInternalVis visPath, n)
        if entries
            internalResult[entry.name] = entry.path for entry in entries
    window.visualizersPath     = visPath
    window.visualizers         = result
    window.internalVisualizers = JSON.stringify internalResult

module.exports = () -> setupConfigMap visPath
