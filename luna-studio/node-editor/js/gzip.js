var pako = require('pako');

var compressBytes = function (bytes) {
    var compressed = pako.gzip(bytes);
    return compressed.buffer;
};

var decompressBytes = function (bytes) {
    var decompressed = pako.ungzip(bytes);
    return decompressed.buffer;
};

module.exports = {
    compressBytes: compressBytes,
    decompressBytes: decompressBytes
};
