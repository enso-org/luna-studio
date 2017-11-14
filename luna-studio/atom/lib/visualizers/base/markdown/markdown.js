(function () {

  var render = function (json) {
    var md = JSON.parse(json);
    var html = new showdown.Converter().makeHtml(md);
    document.body.innerHTML = html;
  };

  window.addEventListener("message", function (evt) {
    render(evt.data.data);
  });
}());
