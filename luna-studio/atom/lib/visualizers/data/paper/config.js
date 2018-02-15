module.exports = function (type) {
    if (type.constructor == "Geo" || (type.constructor == "Stream" && type.fields[0].constructor == "Geo")) return [{ path: "paper.html", name: "standard" }, {path: "paperCenter.html", name: "centered"}];
    else return [];
};
