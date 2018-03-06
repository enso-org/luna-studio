(function () {
  var entityMap = {
    '&': '&amp;',
    '<': '&lt;',
    '>': '&gt;',
    '"': '&quot;',
    "'": '&#39;',
    '/': '&#x2F;',
    '`': '&#x60;',
    '=': '&#x3D;'
  };

  var escapeHtml = function (string) {
    return string.replace(/[&<>"'`=\/]/g, function (s) {
      return entityMap[s];
    });
  }

  var render = function (data) {
    var parsed = JSON.parse(data);

    var errorString = "";

    if (parsed._errorType.tag === "CompileError") {
      var details = parsed._errorType.contents;

      var appendLocation = function(previousValue, currentValue, index, array) {
        return previousValue + "<li>" + escapeHtml(currentValue._mod +
                         (currentValue._klass ? "." + currentValue._klass : "") +
                         "." + currentValue._fun) +
                         "</li>";
      }

      var arisingFrom = "";
      if (Array.isArray(details._arisingFrom) && details._arisingFrom.length) {
        arisingFrom += details._arisingFrom.reduce(appendLocation, "<p>arising from:<ul>");
        arisingFrom += "</ul></p>";
      }

      var requiredBy = "";
      if (Array.isArray(details._requiredBy) && details._requiredBy.length) {
        requiredBy += details._requiredBy.reduce(appendLocation, "<p>required by:<ul>");
        requiredBy += "</ul></p>";
      }

      errorString += "<h3>" + "Compile error:" + "</h3>" +
                     "<p>" + escapeHtml(parsed._errorContent) + "</p>" +
                     arisingFrom + requiredBy;
    }
    else if (parsed._errorType.tag === "RuntimeError") {
      errorString += "<h3>" + "Runtime error:" + "</h3>" +
                     "<p>" + escapeHtml(parsed._errorContent) + "</p>";
    }
    else {
      errorString += "<h3>" + "Unknown error: " + "</h3>" +
                     "<p>" + escapeHtml(data) + "</p3>";
    }

    document.body.innerHTML = errorString;
  };

  window.addEventListener("message", function (evt) {
    render(evt.data.data);
  });
}());
