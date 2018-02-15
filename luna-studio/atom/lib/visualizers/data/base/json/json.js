(function () {
  var JSONFormatter = require('./json-formatter.js').default;
  var render = function (json) {
    var data = JSON.parse(json);
    var content = (new JSONFormatter(data, 1, { theme: 'dark' })).render();
    var container = document.getElementById("json")
    container.innerHTML = "";
    container.appendChild(content);
  };

  window.addEventListener("message", function (evt) {
    if (evt.data.data) render(evt.data.data);
  });
}());
