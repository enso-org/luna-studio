(function () {
  var render = function (json) {
    var data = JSON.parse(json);
    document.body.innerHTML = "<pre>" + jsyaml.dump(data, {flowLevel: 4}) + "</pre>";
  };

  window.addEventListener("message", function (evt) {
    if(evt.data.data) render(evt.data.data);
  });
}());
