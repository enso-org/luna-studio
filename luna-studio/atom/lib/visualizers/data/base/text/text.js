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

  var render = function (json) {
    var data = JSON.parse(json);
    document.body.innerHTML = "<span>" + escapeHtml(data) + "</span>";
  };

  window.addEventListener("message", function (evt) {
    render(evt.data.data);
  });
}());
