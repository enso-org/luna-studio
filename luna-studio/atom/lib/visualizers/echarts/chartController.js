(function () {
  window.addEventListener("message", function (evt) {
    var data   = JSON.parse(evt.data);
    var series = data.map(function (x, i) { return [i, x]; });
    var chart  = echarts.init(document.getElementById("chart-container"));
    chart.setOption({
      textStyle: {
        fontFamily: "monospace",
        fontSize: 10
      },
      xAxis: {
        splitLine: {
          lineStyle: {
            color: "#333"
          }
        },
        type: "value"
      },
      yAxis: {
        splitLine: {
          lineStyle: {
            color: "#333"
          }
        },
        type: "value"
      },
      series: [{
        name: "Data",
        type: "scatter",
        data: series
      }]
    });
  });
}());
