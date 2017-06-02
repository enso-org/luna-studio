(function () {
  var render = function (json) {
    var data = JSON.parse(json);
    var result = "";
    data.forEach(function (point, ix) {
      result += ("<tr><td>" + ix + "</td><td>" + point + "</td></tr>");
    });
    document.body.innerHTML = "<table>" + result + "</table>";
  };
  window.addEventListener("message", function (evt) {
    render(evt.data.data);
    document.body.addEventListener("click", function () { console.log("Clicked inside table"); });
  });
}());
