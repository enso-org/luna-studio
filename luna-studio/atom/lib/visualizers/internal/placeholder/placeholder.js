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

  var displayAwaiting = function () {
  }

  var spinner = function () {
    return '<div class="sk-three-bounce">             \
              <div class="sk-child sk-bounce1"></div> \
              <div class="sk-child sk-bounce2"></div> \
              <div class="sk-child sk-bounce3"></div> \
            </div>'
  }

  var noVisIcon = function () {
    return '<img src="images/no_vis.svg" class="no-vis"/>';
  }

  var displayAwaiting = function () {
    document.body.innerHTML = spinner();
  }

  var displayNoVisForType = function () {
    document.body.innerHTML = noVisIcon();
    document.body.innerHTML += "<p>No visualizers available for this type.</p>";
  }

  var displayNoData = function () {
    document.body.innerHTML = noVisIcon();
    document.body.innerHTML += "<p>No data received. The object does not implement a toJSON method.</p>";
  }

  var displayUnknown = function (tag) {
    document.body.innerHTML = "<p>Unknown placeholder type: " + escapeHtml(tag) + ".";
  }

  var render = function (data) {
    if (data == "NO_VIS_FOR_TYPE")
      displayNoVisForType();
    else if (data == "NO_DATA")
      displayNoData();
    else if (data == "AWAITING_DATA")
      displayAwaiting();
    else
      displayUnknown(data);
  };

  window.addEventListener("message", function (evt) {
    render(evt.data.data);
  });
}());
