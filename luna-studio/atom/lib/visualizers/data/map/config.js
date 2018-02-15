var cfgHelper = require("../../visualization-config-helper.js")

module.exports = function (type) {
    var geolocationPattern = { constructor: ["List"]
                             , fields:      [ { constructor: ["Tuple2"]
                                            , fields: [ { constructor: ["Real"]
                                                        , fields: { any: true } }
                                                      , { constructor: ["Real"]
                                                        , fields: { any: true } }
                                                      ]
                                            }
                                          ]
                             };
   var geolocationWithLabelsPattern = { constructor: ["List"]
                                      , fields:      [ { constructor: ["Tuple2"]
                                                       , fields: [ { constructor: ["Tuple2"]
                                                                   , fields: [ { constructor: ["Real"]
                                                                               , fields: { any: true } }
                                                                             , { constructor: ["Real"]
                                                                               , fields: { any: true } }
                                                                             ]
                                                                   }
                                                                 , { constructor: ["Text"]
                                                                   , fields: { any: true } }
                                                                 ]
                                                       }
                                                     ]
                                      };
    var simpleMarkers = (cfgHelper.matchesType(type, geolocationPattern) || cfgHelper.matchesType(type, geolocationWithLabelsPattern)) ? [{name: "markers", path: "map.html"}] : [];
    var generalMap = (type.constructor == "GeoJSONFeatureCollection" || (type.constructor == "Stream" && type.fields[0].constructor == "GeoJSONFeatureCollection")) ? [{name: "GeoJSON", path: "geojson.html"}] : [];
    return [].concat(simpleMarkers, generalMap);
};
