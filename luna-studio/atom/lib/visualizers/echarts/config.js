module.exports = function (type) {
    if (type.constructor === "List" && type.fields && ["Int", "Real"].indexOf(type.fields[0].constructor) != -1) return "echarts.html";
};
