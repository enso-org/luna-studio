module.exports = function (type) {
    if (["Stream", "List"].indexOf(type.constructor) != -1 && type.fields && ["Int", "Real"].indexOf(type.fields[0].constructor) != -1) return [{name: "line plot", path: "echarts.html"}];
};
