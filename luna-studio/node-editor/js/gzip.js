var pako = require('pako');

var compressBytes = function (bytes) {
    console.log('----- compressing ------');
    console.log(bytes);
    var compressed = pako.gzip(bytes);
    console.log(compressed);
    console.log('----- done -------------');
    // return "Dupa"
    return compressed.buffer;
};

var decompressBytes = function (bytes) {
    console.log('----- decompressing ------');
    console.log(bytes);
    var decompressed = pako.ungzip(bytes);
    console.log(decompressed);
    console.log('----- done ------');
    return decompressed.buffer;
};

module.exports = {
    compressBytes: compressBytes,
    decompressBytes: decompressBytes
};

