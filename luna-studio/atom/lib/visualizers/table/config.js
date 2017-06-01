module.exports = function (type) {
    if (type.constructor === "List" && type.fields && type.fields[0].constructor == "Text") return "table.html";
};
