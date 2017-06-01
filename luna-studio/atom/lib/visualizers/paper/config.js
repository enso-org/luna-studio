module.exports = function (type) {
    if (type.constructor == "Circle" || (type.constructor == "List" && type.fields[0].constructor == "Circle")) return "paper.html"
};
