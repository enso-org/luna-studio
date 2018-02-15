(function () {
  var chart = null;
  var currentData = null;
  window.addEventListener("load", function () {
    chart  = echarts.init(document.getElementById("chart-container"));
    chart.setOption(defaultOptions);
  });
  window.addEventListener("resize", function () {
    if (chart) chart.resize();
  });

  function clearSeries() {
    options = chart.getOption();
    options.series = [];
    chart.setOption(options, { notMerge: true });
    return options;
  }

  function prepareBoxPlotData (rawData) {
      var boxData = [];
      var outliers = [];
      var axisData = [];

      function quantile(ascArr, p) {
        var H = (ascArr.length - 1) * p + 1,
          h = Math.floor(H),
          v = +ascArr[h - 1],
          e = H - h;
        return e ? v + e * (ascArr[h] - v) : v;
      };

      for (var i = 0; i < rawData.length; i++) {
          axisData.push(i + '');
          var ascList = rawData[i].concat().sort();

          var Q1 = quantile(ascList, 0.25);
          var Q2 = quantile(ascList, 0.5);
          var Q3 = quantile(ascList, 0.75);
          var IQR = Q3 - Q1;

          var low = ascList[0];
          var high = ascList[ascList.length - 1];

          boxData.push([low, Q1, Q2, Q3, high]);

          for (var j = 0; j < ascList.length; j++) {
              var dataItem = ascList[j];
              if (dataItem < low || dataItem > high) {
                  var outlier = [i, dataItem];
                  outliers.push(outlier);
              }
          }
      }
      return {
          boxData: boxData,
          outliers: outliers,
          axisData: axisData
      };
  };

  function addLegendData (options) {
    options.legend[0].data = options.series.map(function(s) { return s.name; });
    return options;
  }

  function applyData (options, data) {
    if (!options.series) options.series = [];
    if (options.toolbox[0].feature.myBoxPlot.iconStatus.myBoxPlot == "emphasis") {
      boxData = prepareBoxPlotData(data);
      options.xAxis[0].data = boxData.axisData;
      options.series = [
          {
              name: 'boxplot',
              type: 'boxplot',
              data: boxData.boxData,
              tooltip: {
                  formatter: function (param) {
                      return [
                          'upper: ' + param.data[4],
                          'Q3: ' + param.data[3],
                          'median: ' + param.data[2],
                          'Q1: ' + param.data[1],
                          'lower: ' + param.data[0]
                      ].join('<br/>')
                  }
              }
          },
          {
              name: 'outlier',
              type: 'scatter',
              data: boxData.outliers
          }
      ];
    } else if (options.xAxis[0].type == "value") {
      data.forEach(function (d, i) {
        entryData = d.map(function (x, j) { return [j,x]; });
        if (options.series[i]) {
          options.series[i].data = entryData;
        } else {
          options.series[i] = { name: "Data" + i, type: "line", areaStyle: {empahsis: {}}, symbolSize: 1, symbol: "circle", data: entryData };
        }
      });
    } else if (options.xAxis[0].type == "category") {
      dataLength = Math.max.apply(null, data.map(function (s) { return s.length; }));
      options.xAxis[0].data = Array.apply(null, {length: dataLength}).map(Number.call, Number);
      data.forEach(function (entryData, i) {
        if (options.series[i]) {
          options.series[i].data = entryData;
        } else {
          options.series[i] = { name: "Data" + i, type: "line", areaStyle: {empahsis: {}}, symbolSize: 1, symbol: "circle", data: entryData };
        }
      });
    }
    return addLegendData(options);
  }

  var defaultOptions = {
    legend: [{
      data: [],
      formatter: function () {
        return '';
      },
      left: 35,
      top: 20,
      itemWidth: 15,
      itemHeight: 10,
      itemGap: 5
    }],
    toolbox: {
      show: true,
      right: 30,
      feature: {

        myLinePlot: {
          show: true,
          title: 'line',
          icon: 'path://M21 7l-6.5 6.5-4-4L3 17',
          iconStatus: {myLinePlot: 'emphasis'},
          onclick: function (){
            options = chart.getOption();
            if (options.toolbox[0].feature.myBoxPlot.iconStatus.myBoxPlot == "emphasis") options = clearSeries();
            options.toolbox[0].feature.myLinePlot.iconStatus = {myLinePlot: 'emphasis'};
            options.toolbox[0].feature.myLinePlotWithFacilityField.iconStatus = {myLinePlotWithFacilityField: 'normal'};
            options.toolbox[0].feature.myBarPlot.iconStatus = {myBarPlot: 'normal'};
            options.toolbox[0].feature.myScatterPlot.iconStatus = {myScatterPlot: 'normal'};
            options.toolbox[0].feature.myBoxPlot.iconStatus = {myBoxPlot: 'normal'};
            options.xAxis[0].type = "value";
            options.xAxis[0].boundaryGap = undefined;
            options = applyData(options, currentData, true);
            options.series.map(function(s) {
              s.type = 'line';
              s.areaStyle = undefined;
              s.stack = undefined;
              s.symbolSize = 1;
              return s;
            });
            chart.setOption(options);
          }
        },

        myLinePlotWithFacilityField: {
          show: true,
          title: 'stack',
          icon: 'path://M3 19h18V6l-7.5 7.6-4.2-3.8L3 16.2V19z',
          iconStatus: {myLinePlotWithFacilityField: 'normal'},
          onclick: function (){
            options = chart.getOption();
            if (options.toolbox[0].feature.myBoxPlot.iconStatus.myBoxPlot == "emphasis") options = clearSeries();
            options.toolbox[0].feature.myLinePlot.iconStatus = {myLinePlot: 'normal'};
            options.toolbox[0].feature.myLinePlotWithFacilityField.iconStatus = {myLinePlotWithFacilityField: 'emphasis'};
            options.toolbox[0].feature.myBarPlot.iconStatus = {myBarPlot: 'normal'};
            options.toolbox[0].feature.myScatterPlot.iconStatus = {myScatterPlot: 'normal'};
            options.toolbox[0].feature.myBoxPlot.iconStatus = {myBoxPlot: 'normal'};
            options.xAxis[0].type = "category";
            options.xAxis[0].boundaryGap = false;
            options = applyData(options, currentData);
            options.series.map(function(s) {
              s.type = 'line';
              s.areaStyle = {normal: {}};
              s.stack = 'def';
              s.symbolSize = 1;
              return s;
            });
            chart.setOption(options);
          }
        },


        myBarPlot: {
          show: true,
          title: 'bar',
          icon: 'path://M20 19V5v14zm-4 0V9v10zm-4 0v-7 7zm-4 0V9v10zm-4 0v-7 7z',
          iconStatus: {myBarPlot: 'normal'},
          onclick: function (){
            options = chart.getOption();
            if (options.toolbox[0].feature.myBoxPlot.iconStatus.myBoxPlot == "emphasis") options = clearSeries();
            options.toolbox[0].feature.myLinePlot.iconStatus = {myLinePlot: 'normal'};
            options.toolbox[0].feature.myLinePlotWithFacilityField.iconStatus = {myLinePlotWithFacilityField: 'normal'};
            options.toolbox[0].feature.myBarPlot.iconStatus = {myBarPlot: 'emphasis'};
            options.toolbox[0].feature.myScatterPlot.iconStatus = {myScatterPlot: 'normal'};
            options.toolbox[0].feature.myBoxPlot.iconStatus = {myBoxPlot: 'normal'};
            options.xAxis[0].type = "category";
            options.xAxis[0].boundaryGap = true;
            options = applyData(options, currentData);
            options.series.map(function(s) {
              s.type = 'bar';
              s.areaStyle = undefined;
              s.stack = undefined;
              s.symbolSize = 1;
              return s;
            });
            chart.setOption(options);
          }
        },

        myScatterPlot: {
          show: true,
          title: 'scatter',
          icon: 'path://M19.5,9 C19.7761424,9 20,8.77614237 20,8.5 C20,8.22385763 19.7761424,8 19.5,8 C19.2238576,8 19,8.22385763 19,8.5 C19,8.77614237 19.2238576,9 19.5,9 Z M16.5,12 C16.7761424,12 17,11.7761424 17,11.5 C17,11.2238576 16.7761424,11 16.5,11 C16.2238576,11 16,11.2238576 16,11.5 C16,11.7761424 16.2238576,12 16.5,12 Z M13.5,15 C13.7761424,15 14,14.7761424 14,14.5 C14,14.2238576 13.7761424,14 13.5,14 C13.2238576,14 13,14.2238576 13,14.5 C13,14.7761424 13.2238576,15 13.5,15 Z M10.5,12 C10.7761424,12 11,11.7761424 11,11.5 C11,11.2238576 10.7761424,11 10.5,11 C10.2238576,11 10,11.2238576 10,11.5 C10,11.7761424 10.2238576,12 10.5,12 Z M7.5,15 C7.77614237,15 8,14.7761424 8,14.5 C8,14.2238576 7.77614237,14 7.5,14 C7.22385763,14 7,14.2238576 7,14.5 C7,14.7761424 7.22385763,15 7.5,15 Z M4.5,18 C4.77614237,18 5,17.7761424 5,17.5 C5,17.2238576 4.77614237,17 4.5,17 C4.22385763,17 4,17.2238576 4,17.5 C4,17.7761424 4.22385763,18 4.5,18 Z',
          iconStatus: {myScatterPlot: 'normal'},
          onclick: function (){
            options = chart.getOption();
            if (options.toolbox[0].feature.myBoxPlot.iconStatus.myBoxPlot == "emphasis") options = clearSeries();
            options.toolbox[0].feature.myLinePlot.iconStatus = {myLinePlot: 'normal'};
            options.toolbox[0].feature.myLinePlotWithFacilityField.iconStatus = {myLinePlotWithFacilityField: 'normal'};
            options.toolbox[0].feature.myBarPlot.iconStatus = {myBarPlot: 'normal'};
            options.toolbox[0].feature.myScatterPlot.iconStatus = {myScatterPlot: 'emphasis'};
            options.toolbox[0].feature.myBoxPlot.iconStatus = {myBoxPlot: 'normal'};
            options.xAxis[0].type = "value";
            options.xAxis[0].boundaryGap = undefined;
            options = applyData(options, currentData);
            options.series.map(function(s) {
              s.type = 'scatter';
              s.areaStyle = undefined;
              s.stack = undefined;
              s.symbolSize = 3;
              return s;
            });
            chart.setOption(options);
          }
        },

        myBoxPlot: {
          show: true,
          title: 'box',
          icon: 'path://M14 16h-4V9h4v7zm-7 3H3v-7h4v7zm14-7h-4V5h4v7z',
          iconStatus: {myBoxPlot: 'normal'},
          onclick: function (){
            options = clearSeries(chart.getOption());
            options.toolbox[0].feature.myLinePlot.iconStatus = {myLinePlot: 'normal'};
            options.toolbox[0].feature.myLinePlotWithFacilityField.iconStatus = {myLinePlotWithFacilityField: 'normal'};
            options.toolbox[0].feature.myBarPlot.iconStatus = {myBarPlot: 'normal'};
            options.toolbox[0].feature.myScatterPlot.iconStatus = {myScatterPlot: 'normal'};
            options.toolbox[0].feature.myBoxPlot.iconStatus = {myBoxPlot: 'emphasis'};
            options.xAxis[0].type = "category";
            options.xAxis[0].boundaryGap = true;
            chart.setOption(applyData(options, currentData));
          }
        },

        dataZoom: {
          show: true,
          title: {
            zoom: 'area zooming',
            back: 'restore area zooming'
          }
        },

        myResetView: {
          show: true,
          title: 'reset view',
          icon: 'path://M2 10.2l3 3.3 3.2-3.3m-3 3C4.4 8.6 8 5 12 5s7 3 7 7-3 7-7 7c-1.5 0-3-.5-4-1.3',
          onclick: function (){
            options = chart.getOption();
            options.legend   = defaultOptions.legend;
            options.dataZoom = defaultOptions.dataZoom;
            chart.setOption(addLegendData(options), { notMerge: true });
          }
        }
      }
    },
    grid: {
      top: 75,
      left: 40,
      right: 40
    },

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
    dataZoom: [{
      bottom: 25,
      id: "dataZoomX",
      type: "slider",
      xAxisIndex: [0],
      filterMode: "filter",
      dataBackground: "red",
    },
    {
      type: "inside"
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
    series: []
  };

  window.addEventListener("message", function (evt) {
    mayOptions = chart.getOption();
    newOptions = mayOptions ? mayOptions : defaultOptions;
    var data;
    if (Array.isArray(evt.data.data)) {
      data = evt.data.data.map( function (x) { return JSON.parse(x); });
    } else {
      data = JSON.parse(evt.data.data);
    }
    if (evt.data.event == "restart") {
      currentData = [];
      data.forEach (function (datapoint) {
        datapoint.forEach(function (x, i) {
          currentData[i] = currentData[i] ? currentData[i] : [];
          currentData[i].push(x);
        });
      })
    } else if (evt.data.event == "datapoint") {
      if (!currentData) currentData = [];
      if (!Array.isArray(data)) data = [data];
      data.forEach(function (x, i) {
        currentData[i] = currentData[i] ? currentData[i] : [];
        currentData[i].push(x);
      });
    } else {
      if (!(data[0] && Array.isArray(data[0]))) data = [data];
      newOptions.series = []
      currentData = data;
    }
    chart.setOption(applyData(newOptions, currentData.map( function (s) { return s.slice(-500)})));
  });
}());
