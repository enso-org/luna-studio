(function () {
    var map   = null;
    var layer = null;
    var mapOptions = { center: [0,0], zoom: 0.2, minZoom: 0.2, closePopupOnClick: false }; //  maxBounds: [[-90, -Infinity], [180, Infinity]],  maxBoundsViscosity: 1.0};

    window.addEventListener("resize", function (e) {
        map.invalidateSize();
        width = map.getSize().x;
        height = map.getSize().y
        map.setMinZoom(Math.max(width, height)/1024);
    });

    window.addEventListener("load", function (e) {
        map = L.map('chart-container', mapOptions);
        L.tileLayer('https://api.tiles.mapbox.com/v4/{id}/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibWFwYm94IiwiYSI6ImNpejY4NXVycTA2emYycXBndHRqcmZ3N3gifQ.rJcFIG214AriISLbB6B5aw', {
            maxZoom: 18,
            attribution: 'Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors, ' +
                '<a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, ' +
                'Imagery © <a href="http://mapbox.com">Mapbox</a>',
            id: 'mapbox.streets'
        }).addTo(map);
    });

    var currentData = null;
    var getCurrentData = function (success) { success(currentData); }

    var initRealtime = function () {
        if (layer) layer.clearLayers();
        layer = L.realtime(getCurrentData, {
            start: false,
            interval: 1000,
            style: function (f) { return f.properties.style; },
            onEachFeature: function (f, l) {
                if (f.properties && f.properties.popupContent)
                    l.bindPopup(f.properties.popupContent);
            },
            pointToLayer: function (f, l) {
                return L.circleMarker(l, { radius: 6, fillColor: "#e74c3c", color: "white", fillOpacity: 1, weight: 1 });
            }
        });
        layer.addTo(map);
    }

    window.addEventListener("message", function (evt) {
        var data = JSON.parse(evt.data.data);
        if (evt.data.event == "data") {
            if (layer) layer.clearLayers();
            layer = L.geoJSON(data, {
                style: function (f) { return f.properties.style; },
                onEachFeature: function (f, l) {
                    if (f.properties && f.properties.popupContent)
                        l.bindPopup(f.properties.popupContent);
                },
                pointToLayer: function (f, l) {
                    return L.circleMarker(l, { radius: 6, fillColor: "#e74c3c", color: "white", fillOpacity: 1, weight: 1 });
                }
            })
            layer.addTo(map);
        } else if (evt.data.event == "restart") {
            initRealtime();
        } else if (evt.data.event == "datapoint") {
            var oldData = currentData;
            currentData = data;
            if (!layer) initRealtime();
            if (!oldData) layer.start();
        }
    });
}());
