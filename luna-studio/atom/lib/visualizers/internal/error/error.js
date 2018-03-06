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

    document.body.innerHTML = "<pre>";

    if (parsed._errorType.tag === "CompileError")
    {
      var details = parsed._errorType.contents;

      var appendLocation = function(msg, location) {
        msg.content += "<li>" + escapeHtml(location._mod +
                       (location._klass ? "." + location._klass : "") +
                       "." + location._fun) +
                       "</li>";
      }
      var arisingFrom = {content: "<br><br>arising from:<ul>"};
      details._arisingFrom.forEach(location => appendLocation(arisingFrom, location));
      arisingFrom.content += "</ul>";

      var requiredBy = {content: ""};
      if (Array.isArray(details._requiredBy) && details._requiredBy.length) {
        requiredBy.content = "required by:<ul>";
        details._requiredBy.forEach(location => appendLocation(requiredBy, location));
        requiredBy.content += "</ul>";
      }

      document.body.innerHTML += "Compile error:" + "<br>" + escapeHtml(parsed._errorContent) +
                                arisingFrom.content + requiredBy.content;
    }
    else if (parsed._errorType.tag === "RuntimeError")
    {
      document.body.innerHTML += "Runtime error:" + "<br>" + escapeHtml(parsed._errorContent);
    }
    else
    {
      document.body.innerHTML += "Unknown error: " + escapeHtml(data);
    }

    document.body.innerHTML += "</pre>";
  };

  window.addEventListener("message", function (evt) {
    render(evt.data.data);
  });
}());
