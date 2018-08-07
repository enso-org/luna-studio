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
    var featureTypes = ["GeoJSONFeatureCollection", "GeoJSONFeature", "GeoPoint"];
    var collectionTypes = ["List", "Stream"]
    var isCollectionOfFeatures = function (type) {
       return featureTypes.includes(type.constructor)
              || collectionTypes.includes(type.constructor)
              && isCollectionOfFeatures(type.fields[0]);
    };
    var generalMap = isCollectionOfFeatures(type) ? [{name: "GeoJSON", path: "geojson.html"}] : [];
    return [].concat(simpleMarkers, generalMap);
};
