path = require 'path'
fs   = require 'fs'

visBasePath = path.join __dirname, 'visualizers'

listVisualizers = (path) -> fs.readdirSync path

resolveVis = (p, name) ->
    normalizeVis p, name, require(path.join p, name, "config.js")

normalizeVis = (p, name, visConf) -> (cons) ->
    filesToLoad = visConf (JSON.parse cons)
    if filesToLoad?
        f.path = path.join(p, name, f.path) for f in filesToLoad
        JSON.stringify(filesToLoad)
    else JSON.stringify(null)

setupConfigMap = (path) ->
    visualizers = listVisualizers(path)
    result = {}
    result[n] = resolveVis path, n for n in visualizers
    window.visualizers = result

module.exports = () -> setupConfigMap visBasePath
