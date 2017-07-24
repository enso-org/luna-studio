var pako = require('pako');

<<<<<<< HEAD
var compressBytes = function (bytes) {
    console.log('----- compressing ------');
    console.log(bytes);
    var compressed = pako.gzip(bytes);
    console.log(compressed);
    console.log('----- done -------------');
    // return "Dupa"
    return compressed.buffer;
=======
var state = {
  bytes: undefined,
  buffer: undefined
};

var getBytes = function () {
    return state.bytes;
};

var getBuffer = function() {
    return state.buffer;
};

var printCompressed = function () {
    console.log('----- compressed buffer -----');
    console.log(state.buffer);
    console.log('-----');
};

var compressBytes = function (bytes) {
    console.log('----- compressing ------');
    console.log(bytes);
    state.bytes = pako.gzip(bytes);
    state.buffer = Buffer(state.bytes);
    console.log(state.buffer);
    console.log('----- done -------------');
    return state.bytes;
>>>>>>> 051c78a11614f65c22c21040489ceaf50c5e02cc
};

var decompressBytes = function (bytes) {
    console.log('----- decompressing ------');
    console.log(bytes);
<<<<<<< HEAD
    var decompressed = pako.ungzip(bytes);
    console.log(decompressed);
    console.log('----- done ------');
    return decompressed.buffer;
=======
    state.bytes = pako.ungzip(bytes);
    state.buffer = Buffer(state.bytes);
    console.log(state.buffer);
    console.log('----- done ------');
    return state.bytes;
>>>>>>> 051c78a11614f65c22c21040489ceaf50c5e02cc
};

module.exports = {
    compressBytes: compressBytes,
<<<<<<< HEAD
    decompressBytes: decompressBytes
=======
    decompressBytes: decompressBytes,
    state: state,
    printCompressed: printCompressed,
    getBytes,
    getBuffer
>>>>>>> 051c78a11614f65c22c21040489ceaf50c5e02cc
};

