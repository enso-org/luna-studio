(function () {
  var chart = null;
  window.addEventListener("load", function () {
    chart  = echarts.init(document.getElementById("chart-container"));
  });
  window.addEventListener("resize", function () {
    if (chart) chart.resize();
  });
  var options = {
    color: ['#3398DB'],
    tooltip : {
        trigger: 'item',
        axisPointer:{
             show: true,
             type : 'cross',
             lineStyle: {
                 type : 'dashed',
                 width : 1
             }
         },
         formatter : function (params) {
             return '(' + params.value[0] + ', ' + params.value[1] + ')';
         }
    },
    grid: {
    },
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
      axisPointer: {
        label: {show: false}
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
      axisPointer: {
        label: {show: false}
      },
      type: "value"
    },
    series : [
        {
            name: "Data",
            symbolSize: 3,
            type: "scatter",
            itemStyle: {normal: {color: "red"}}
        }
    ]
  };
  var currentData = null;
  var labels      = null;

  var display = function () {
    options.series[0].data = currentData;
    chart.setOption(options);
  }

  window.addEventListener("message", function (evt) {
    var data = JSON.parse(evt.data.data);
    if (evt.data.event == "data") {
        currentData = data;
        labels = [];
    }
    display();
  });
}());
