path = require 'path'
fs   = require 'fs'

visBasePath = path.join __dirname, 'visualizers'
internalVisualizers = []
projectVisualizers  = []

listVisualizers = (visPath) -> 
    if fs.existsSync visPath
        (fs.readdirSync visPath).filter((p) -> fs.existsSync(path.join(visPath, p, "config.js")))
    else []

resolveVis = (p, name) ->
    normalizeVis p, name, require(path.join p, name, "config.js")

normalizeVis = (p, name, visConf) -> (cons) ->
    filesToLoad = visConf (JSON.parse cons)
    if filesToLoad?
        f.path = path.join(name, f.path) for f in filesToLoad
        JSON.stringify(filesToLoad)
    else JSON.stringify(null)

getVisualizersForPath = (path) ->
    visualizers = listVisualizers(path)
    result = {}
    result[n] = resolveVis path, n for n in visualizers
    result

module.exports = () ->
    window.getInternalVisualizersPath = () -> visBasePath
    window.getInternalVisualizers     = () ->
        internalVisualizers = getVisualizersForPath visBasePath
        internalVisualizers
    window.getProjectVisualizers      = (path) ->
        projectVisualizers = getVisualizersForPath path
        projectVisualizers
    window.checkInternalVisualizer    = (name, tpeRep) -> internalVisualizers[name](tpeRep)
    window.checkProjectVisualizer     = (name, tpeRep) -> projectVisualizers[name](tpeRep)
