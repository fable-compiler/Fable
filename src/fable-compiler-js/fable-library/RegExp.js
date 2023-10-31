export function create(pattern, options = 0) {
    // Supported RegexOptions
    // * IgnoreCase:  0x0001
    // * Multiline:   0x0002
    // * Compiled:    0x0008 (ignored)
    // * Singleline:  0x0010
    // * ECMAScript:  0x0100 (ignored)
    if ((options & ~(1 ^ 2 ^ 8 ^ 16 ^ 256)) !== 0) {
        throw new Error("RegexOptions only supports: IgnoreCase, Multiline, Compiled, Singleline and ECMAScript");
    }
    // Set always global and unicode flags for compatibility with dotnet, see #2925
    let flags = "gu";
    flags += options & 1 ? "i" : ""; // 0x0001 RegexOptions.IgnoreCase
    flags += options & 2 ? "m" : "";
    flags += options & 16 ? "s" : "";
    return new RegExp(pattern, flags);
}
// From http://stackoverflow.com/questions/3446170/escape-string-for-use-in-javascript-regex
export function escape(str) {
    return str.replace(/[\-\[\]\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, "\\$&");
}
export function unescape(str) {
    return str.replace(/\\([\-\[\]\/\{\}\(\)\*\+\?\.\\\^\$\|])/g, "$1");
}
export function isMatch(reg, input, startAt = 0) {
    reg.lastIndex = startAt;
    return reg.test(input);
}
export function match(reg, input, startAt = 0) {
    reg.lastIndex = startAt;
    return reg.exec(input);
}
export function matches(reg, input, startAt = 0) {
    if (input == null) {
        throw new Error("Input cannot ve null");
    }
    if (!reg.global) {
        throw new Error("Non-global RegExp"); // Prevent infinite loop
    }
    reg.lastIndex = startAt;
    const matches = [];
    let m;
    let lastMatchIndex = -1;
    // tslint:disable-next-line:no-conditional-assignment
    while ((m = reg.exec(input)) != null) {
        // It can happen even global regex get stuck, see #2845
        if (m.index === lastMatchIndex) {
            reg.lastIndex++;
        }
        else {
            lastMatchIndex = m.index;
            matches.push(m);
        }
    }
    return matches;
}
export function options(reg) {
    let options = 256; // ECMAScript
    options |= reg.ignoreCase ? 1 : 0;
    options |= reg.multiline ? 2 : 0;
    return options;
}
export function replace(reg, input, replacement, limit, offset = 0) {
    function replacer() {
        let res = arguments[0];
        if (limit) {
            limit--;
            const match = [];
            const len = arguments.length;
            // arguments: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace#specifying_a_function_as_a_parameter
            // * match: matched substring
            // * p1, p2, ...: nth capture group string
            // * offset: offset of matched substring
            // * string: whole string examined
            // * groups: named capturing groups
            //           ONLY if regex contains a named capture group AND browser supports named groups
            // -> last element can be groups OR input string
            // -> check if last element is string
            const withGroups = typeof arguments[len - 1] !== "string";
            let pLast = withGroups ? len - 3 : len - 2;
            for (let i = 0; i < pLast; i++) {
                match.push(arguments[i]);
            }
            match.index = arguments[pLast++];
            match.input = arguments[pLast++];
            if (withGroups) {
                match.groups = arguments[pLast];
            }
            res = replacement(match);
        }
        return res;
    }
    if (typeof reg === "string") {
        const tmp = reg;
        reg = create(input, limit ?? 0);
        input = tmp;
        limit = undefined;
    }
    if (typeof replacement === "function") {
        limit = limit == null ? -1 : limit;
        return input.substring(0, offset) + input.substring(offset).replace(reg, replacer);
    }
    else {
        replacement =
            replacement
                // $0 doesn't work with JS regex, see #1155
                .replace(/\$0/g, (_s) => "$&")
                // named groups in replacement are `${name}` in .Net, but `$<name>` in JS (in regex: groups are `(?<name>...)` in both)
                .replace(/\${([^}]+)}/g, "\$<$1>");
        if (limit != null) {
            let m;
            const sub1 = input.substring(offset);
            const _matches = matches(reg, sub1);
            const sub2 = matches.length > limit ? (m = _matches[limit - 1], sub1.substring(0, m.index + m[0].length)) : sub1;
            return input.substring(0, offset) + sub2.replace(reg, replacement)
                + input.substring(offset + sub2.length);
        }
        else {
            return input.replace(reg, replacement);
        }
    }
}
export function split(reg, input, limit, offset = 0) {
    if (typeof reg === "string") {
        const tmp = reg;
        reg = create(input, limit ?? 0);
        input = tmp;
        limit = undefined;
    }
    input = input.substring(offset);
    return input.split(reg, limit);
}
