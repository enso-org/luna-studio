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
        trigger: 'axis',
        axisPointer : {
            type : 'shadow'
        }
    },
    grid: {
        left: '3%',
        right: '4%',
        bottom: '3%',
        containLabel: true
    },
    xAxis : [
        {
            type : 'category',
            axisTick: {
                alignWithLabel: true
            }
        }
    ],
    yAxis : [
        {
            type : 'value'
        }
    ],
    series : [
        {
            name: "Data",
            type: "bar",
            itemStyle: {normal: {color: "red"}},
            barWidth: "60%"
        }
    ]
  };
  var currentData = null;
  var labels      = null;

  var display = function () {
    options.series[0].data = currentData;
    options.xAxis[0].data = labels
    chart.setOption(options);
  }

  window.addEventListener("message", function (evt) {
    var data = JSON.parse(evt.data.data);
    if (evt.data.event == "data") {
        currentData = [];
        labels = [];
        data.forEach(function (entry) {
            labels.push(entry[0]);
            currentData.push(entry[1]);
        });
    }
    display();
  });
}());
