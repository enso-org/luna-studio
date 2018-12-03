var matches = function (type, pattern) {
    if (pattern.any)
        return true
    else if (pattern.constructor.any || pattern.constructor.indexOf(type.constructor) != -1)
        if (pattern.fields.any)
            return true;
        else if (pattern.fields.length < type.fields.length)
            return false;
        else {
            for (var i = 0; i < type.fields.length; i++)
                if (!matches(type.fields[i], pattern.fields[i]))
                    return false;
            return true;
        }
    else
        return false;
};

module.exports = { matchesType: matches };
