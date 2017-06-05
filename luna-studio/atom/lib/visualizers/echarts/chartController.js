(function () {
  var chart = null;
  window.addEventListener("load", function () {
    chart  = echarts.init(document.getElementById("chart-container"));
  });
  var options = {
    dataZoom: [{
      bottom: 25,
      id: "dataZoomX",
      type: "slider",
      xAxisIndex: [0],
      filterMode: "filter",
      dataBackground: "red"
    }],
    textStyle: {
      fontFamily: "monospace",
      fontSize: 10
    },
    xAxis: {
      position: "top",
      splitLine: {
        lineStyle: {
          color: "#333"
        }
      },
      type: "value"
    },
    yAxis: {
      min: "dataMin",
      axisLabel: {
        textStyle: {
          fontSize: 8
        }
      },
      splitLine: {
        lineStyle: {
          color: "#333"
        }
      },
      type: "value"
    }
  };

  var currentData = null;

  var display = function () {
    options.series = [{ name: "Data", type: "line", symbolSize: 1, data: currentData }];
    chart.setOption(options);
  }

  window.addEventListener("message", function (evt) {
    if (evt.data.event == "restart") {
      currentData = [];
    } else {
      var data = JSON.parse(evt.data.data);
      if (evt.data.event == "data") {
        currentData = data.map(function (x, i) { return [i, x]; });
      } else {
        currentData.push([currentData.length, data]);
      }
    }
    display();
  });
}());
