(function () {
  window.addEventListener("message", function (evt) {
    var mkCircle = function (circ) {
      var c = new paper.Path.Circle(new paper.Point(circ.center.x, circ.center.y), circ.radius);
      c.fillColor = circ.color;
    };
    var data = JSON.parse(evt.data);
        canvas = document.getElementById("drawing");
    paper.setup(canvas);
    if (Array.isArray(data)) {
      data.reverse();
      data.forEach(mkCircle);
    } else {
      mkCircle(data);
    }
    paper.view.draw();
  });
}());
