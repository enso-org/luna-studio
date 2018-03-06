var webpack = require('webpack');

const path = require('path');

module.exports =
  { entry:  ['babel-polyfill', './main.coffee']
  , context: path.resolve(__dirname, "src")
  , output:
    { path: path.resolve(__dirname, 'dist', 'js')
    , publicPath: '/js/'
    , filename: 'bundle.js'
    , library: 'node_editor_basegl'
    , libraryTarget: 'umd'
    , strictModuleExceptionHandling: true
    }
  , node: {
      __filename: true,
      __dirname: true,
  },

  devServer: {
    contentBase: path.resolve(__dirname, 'dist')
  },

  resolve: {
      extensions: ['.js', '.coffee'],
      modules: [
        path.resolve(__dirname, "src"),
        "node_modules"
      ],
      alias: {
        'three/CSS3DRenderer': path.join(__dirname, 'node_modules/three/examples/js/renderers/CSS3DRenderer.js')
      }
  },

  module:
    { strictExportPresence: true
    , rules:
      [ { test: /\.(coffee)$/
        , use:
          [ { loader: 'babel-loader'
            , options: { presets: ['env'] }
            }
          , { loader: path.resolve('./basegl-loader.js')}
          , 'coffee-loader'
          ]
        }
      , { test: /\.(glsl|vert|frag)$/
        , use: 'raw-loader'
        , exclude: /node_modules/
        }
      , { test: /\.(glsl|vert|frag)$/
        , use: 'glslify-loader'
        , exclude: /node_modules/
        }
      ]
    }
  , plugins:
    [ new webpack.ProvidePlugin({'THREE': 'three'})
    ]

};
