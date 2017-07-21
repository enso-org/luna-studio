var pako = require('pako');

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
};

var decompressBytes = function (bytes) {
    console.log('----- decompressing ------');
    console.log(bytes);
    state.bytes = pako.ungzip(bytes);
    state.buffer = Buffer(state.bytes);
    console.log(state.buffer);
    console.log('----- done ------');
    return state.bytes;
};

module.exports = {
    compressBytes: compressBytes,
    decompressBytes: decompressBytes,
    state: state,
    printCompressed: printCompressed,
    getBytes,
    getBuffer
};

