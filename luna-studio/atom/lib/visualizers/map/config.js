var cfgHelper = require("../visualization-config-helper.js")

module.exports = function (type) {
    var geolocationPattern = { constructor: ["List"]
                             , fields:      [ { constructor: ["Truple2"]
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
    return (cfgHelper.matchesType(type, geolocationPattern) || cfgHelper.matchesType(type, geolocationWithLabelsPattern)) ? [{path: "map.html"}] : [];
};
