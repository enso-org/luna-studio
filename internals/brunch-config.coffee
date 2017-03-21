shelljs = require 'shelljs'
logger = require 'loggy'

cabalProjectName = "internals"

exports.config =
  paths:
    public: 'www'
    watched: ['app', 'vendor', "#{cabalProjectName}.cabal"]

  files:
    javascripts:
      joinTo:
        'javascripts/ghcjs.js' : /^app\/.*\.(ghcjs)$/
        'javascripts/app.js'   : /^app\/(js|shaders|config|brunch\.buildenv)/
        'javascripts/vendor.js': /^(vendor|bower_components)/
      order:
        before: []

    stylesheets:
      joinTo:
        'stylesheets/app.css': /^(app|vendor|bower_components)/
      order:
        before: []
        after : []
    templates:
      joinTo:
        'javascripts/app.js': /^(app|vendor|bower_components)/
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
      path.replace /^app\/(js\/)?/, ''

  plugins:
    ghcjs:
      placeholder:  'app/env2.ghcjs'
      projectName:  cabalProjectName
      buildCommand: 'stack build --ghc-options=-j8 --install-ghc ' + if process.env.CIRCLECI then '--fast' else ''
      clearScreen:  false
      interactive:  false
      ghciCommand:  "./interactive"


    jshint:
      pattern: /^app\/.*\.js$/
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
