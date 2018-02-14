path = require 'path'
fs   = require 'fs'

visBasePath = path.join __dirname, 'visualizers'

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

setupConfigMap = (projectVisPath) ->
    visualizers = listVisualizers(visBasePath)
    result = {}
    result[n] = resolveVis visBasePath, n for n in visualizers
    window.internalVisualizersPath = visBasePath
    window.internalVisualizers = result
    if projectVisPath
        visualizers = listVisualizers(projectVisPath)
        result = {}
        result[n] = resolveVis projectVisPath, n for n in visualizers
        window.projectVisualizersPath = projectVisPath
        window.projectVisualizers = result

module.exports = () -> window.updateVisualizers = setupConfigMap
