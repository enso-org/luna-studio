shelljs = require 'shelljs'
logger = require 'loggy'

cabalProjectName = "node-editor"

exports.config =
  paths:
    public: 'www'
    watched: ['node-editor', 'vendor', "#{cabalProjectName}.cabal"]

  files:
    javascripts:
      joinTo:
        'javascripts/ghcjs.js' : /^node-editor\/.*\.(ghcjs)$/
        'javascripts/app.js'   : /^node-editor\/(js|shaders|config|brunch\.buildenv)/
        'javascripts/vendor.js': /^(vendor|bower_components)/
      order:
        before: []

    stylesheets:
      joinTo:
        'stylesheets/app.css': /^(node-editor|vendor|bower_components)/
      order:
        before: []
        after : []
    templates:
      joinTo:
        'javascripts/app.js': /^(node-editor|vendor|bower_components)/
      order:
        before: []
        after : []

  conventions:
    assets: /(assets|vendor\/assets)/
    ignored: [
          /[\\/]_/
          /vendor[\\/](node|j?ruby-.*|bundle)[\\/]/
          /ghcjs-live\.js$/
        ]
  modules:
    nameCleaner: (path) ->
      path.replace /^node-editor\/(js\/)?/, ''

  plugins:
    ghcjs:
      placeholder:  'node-editor/env-node-editor.ghcjs'
      projectName:  cabalProjectName
      buildCommand: 'stack build node-editor --install-ghc --ghc-options=-j8 ' + if process.env.CIRCLECI then '--fast' else ''
      clearScreen:  false
      interactive:  false
      ghciCommand:  "./interactive"


    jshint:
      pattern: /^node-editor\/.*\.js$/
      warnOnly: true

    build_env:
      git_commit: ->
        local_changes = (shelljs.exec('git diff-index --quiet HEAD --').code == 1)
        git_hash      = shelljs.exec('git rev-parse HEAD', {silent:true}).output.trim()
        "#{git_hash}#{if local_changes then "-local" else ""}";
      env: "development"
      date: -> new Date()
      build_number: process.env.DRONE_BUILD_NUMBER or "DEV"

  optimize: no

  overrides:
    interactive:
      conventions: ignored: [
          /[\\/]_/
          /vendor[\\/](node|j?ruby-.*|bundle)[\\/]/
        ]
      plugins:
        ghcjs:
          interactive: true
          autoReload: true
        build_env: env: "interactive"
        off: ['auto-reload-brunch']
    production:
      plugins: build_env: env: "production"

try
  c = require("./brunch-config.local").transform(exports.config)
  logger.info "Applying local overrides"
catch
  null # no local overrides
