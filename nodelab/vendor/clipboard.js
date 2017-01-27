// Modified solution from https://gist.github.com/lgarron/d1dee380f4ed9d825ca7
var copyToClipboard = (function() {
    var _dataString = null;
    document.addEventListener("copy", function(e){
        if (_dataString !== null) {
            try {
                e.clipboardData.setData("text/plain", _dataString);
                e.preventDefault();
                e.stopImmediatePropagation();
            } finally {
                _dataString = null;
            }
        }
    });
    return function(data) {
        _dataString = data;
        document.execCommand("copy");
    };
})();
