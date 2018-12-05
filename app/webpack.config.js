const path = require('path');

var distPath = path.resolve(__dirname, 'dist/web');

function baseConfig(entryPath) {
  return {
    mode: "development" // "production" | "development" | "none" // Chosen mode tells webpack to use its built-in optimizations accordingly.

    , devtool: "eval-source-map"
    , entry: entryPath
    , node: {
	    __dirname: false
    }
    , resolve:
      { modules: 
        [ "node_modules"
        , distPath
        ]
      , extensions: [".js", ".coffee"]
      }
    , module: 
      { strictExportPresence: true
      , rules: 
        [ { use: [ 'coffee-loader' ], test: /\.coffee$/ } ]
      }
    , output: 
      { filename: '[name].js'
      , path: distPath
      }
    , devServer: 
      { contentBase: distPath
      , publicPath: '/'
      , compress: true
      , port: 9000
      }
  }
}


serverConfig = baseConfig({index:'./src/index.coffee', main:"./src/main.coffee"});

serverConfig.target = 'electron-main';

module.exports = serverConfig;
