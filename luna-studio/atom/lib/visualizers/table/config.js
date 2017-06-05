module.exports = function (type) {
    if (type.constructor === "List") return [{path: "table.html"}];
};
