path = require 'path'
fs   = require 'fs'

lunaBaseVisPath = path.join __dirname, 'visualizers'
internalVisName = 'internal'
lunaVisName     = 'data'
lunaVisPath     = path.join lunaBaseVisPath, lunaVisName

internalVisualizers = []
lunaVisualizers     = []
projectVisualizers  = []
importedVisualizers = {}


listVisualizers = (visPath, name) -> 
    if not fs.existsSync visPath
        []
    else
        dirs = []
        if name?
            dirs = [name]
        else
            dirs = fs.readdirSync visPath
        dirs.filter((p) -> fs.existsSync(path.join(visPath, p, "config.js")))

resolveVis = (p, name) ->
    normalizeVis p, name, require(path.join p, name, "config.js")

normalizeVis = (p, name, visConf) -> (cons) ->
    filesToLoad = if cons? then visConf(JSON.parse cons) else visConf()
    if filesToLoad?
        f.path = path.join(name, f.path) for f in filesToLoad
        JSON.stringify(filesToLoad)
    else JSON.stringify(null)

getVisualizersForPath = (path, name) ->
    visualizers = listVisualizers(path, name)
    result = {}
    result[n] = resolveVis path, n for n in visualizers
    result

module.exports = () ->
    window.getInternalVisualizersPath = () -> lunaBaseVisPath
    window.getInternalVisualizers = () -> 
        internalVisualizers = getVisualizersForPath lunaBaseVisPath, internalVisName
        internalVisualizers
    window.getLunaVisualizersPath = () -> lunaVisPath
    window.getLunaVisualizers     = () ->
        lunaVisualizers = getVisualizersForPath lunaVisPath
        lunaVisualizers
    window.getProjectVisualizers = (path) ->
        projectVisualizers = getVisualizersForPath path
        projectVisualizers
    window.getImportedVisualizers = (libName, path) ->
        importedVisualizers[libName] = getVisualizersForPath path
        importedVisualizers[libName]
    window.checkInternalVisualizer = (name)                  -> internalVisualizers[name]()
    window.checkLunaVisualizer     = (name, tpeRep)          -> lunaVisualizers[name](tpeRep)
    window.checkProjectVisualizer  = (name, tpeRep)          -> projectVisualizers[name](tpeRep)
    window.checkImportedVisualizer = (libName, name, tpeRep) -> importedVisualizers[libName][name](tpeRep)

