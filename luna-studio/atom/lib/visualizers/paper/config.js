module.exports = function (type) {
    if (type.constructor == "Circle" || (type.constructor == "List" && type.fields[0].constructor == "Circle")) return [{name: "circles", path: "paper.html"}];
};
