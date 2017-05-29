path = require 'path'
fs   = require 'fs'

visBasePath = path.join __dirname, 'visualizers'

listVisualizers = (path) -> fs.readdirSync path

resolveVis = (p, name) ->
    normalizeVis p, name, require(path.join p, name, "config.js")

normalizeVis = (p, name, visConf) -> (cons) ->
    fileToLoad = visConf (JSON.parse cons)
    if fileToLoad?
        JSON.stringify(path.join p, name, fileToLoad)
    else JSON.stringify(null)

setupConfigMap = (path) ->
    visualizers = listVisualizers(path)
    result = {}
    result[n] = resolveVis path, n for n in visualizers
    window.visualizers = result

module.exports = () -> setupConfigMap visBasePath
