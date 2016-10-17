type MatchEvaluator = (match: any) => string;

export function create(pattern: string, options: number) {
  let flags = "g";
  flags += options & 1 ? "i" : "";
  flags += options & 2 ? "m" : "";
  return new RegExp(pattern, flags);
}
// From http://stackoverflow.com/questions/3446170/escape-string-for-use-in-javascript-regex

export function escape(str: string) {
  return str.replace(/[\-\[\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, "\\$&");
}

export function unescape(str: string) {
  return str.replace(/\\([\-\[\/\{\}\(\)\*\+\?\.\\\^\$\|])/g, "$1");
}

export function isMatch(str: string | RegExp, pattern: string, options: number = 0) {
  var reg: RegExp = str instanceof RegExp
    ? (reg = <RegExp>str, str = pattern, reg.lastIndex = options, reg)
    : reg = create(pattern, options);
  return reg.test(<string>str);
}

export function match(str: string | RegExp, pattern: string, options: number = 0) {
  var reg: RegExp = str instanceof RegExp
    ? (reg = <RegExp>str, str = pattern, reg.lastIndex = options, reg)
    : reg = create(pattern, options);
  return reg.exec(<string>str);
}

export function matches(str: string | RegExp, pattern: string, options: number = 0) {
  var reg: RegExp = str instanceof RegExp
    ? (reg = <RegExp>str, str = pattern, reg.lastIndex = options, reg)
    : reg = create(pattern, options);
  if (!reg.global)
    throw "Non-global RegExp"; // Prevent infinite loop
  let m: RegExpExecArray;
  const matches: RegExpExecArray[] = [];
  while ((m = reg.exec(<string>str)) !== null)
    matches.push(m);
  return matches;
}

export function options(reg: RegExp) {
  let options = 256; // ECMAScript
  options |= reg.ignoreCase ? 1 : 0;
  options |= reg.multiline ? 2 : 0;
  return options;
}

export function replace(reg: string | RegExp, input: string, replacement: string | MatchEvaluator, limit?: number, offset: number = 0) {
  function replacer() {
    let res = arguments[0];
    if (limit !== 0) {
      limit--;
      const match: any = [];
      const len = arguments.length;
      for (let i = 0; i < len - 2; i++)
        match.push(arguments[i]);
      match.index = arguments[len - 2];
      match.input = arguments[len - 1];
      res = (replacement as MatchEvaluator)(match);
    }
    return res;
  }
  if (typeof reg == "string") {
    const tmp = <string>reg;
    reg = create(input, limit);
    input = tmp;
    limit = undefined;
  }
  if (typeof replacement == "function") {
    limit = limit == null ? -1 : limit;
    return input.substring(0, offset) + input.substring(offset).replace(<RegExp>reg, replacer);
  } else {
    if (limit != null) {
      let m: RegExpExecArray;
      const sub1 = input.substring(offset);
      const _matches = matches(reg, sub1);
      const sub2 = matches.length > limit ? (m = _matches[limit - 1], sub1.substring(0, m.index + m[0].length)) : sub1;
      return input.substring(0, offset) + sub2.replace(<RegExp>reg, <string>replacement) + input.substring(offset + sub2.length);
    } else {
      return input.replace(<RegExp>reg, <string>replacement);
    }
  }
}

export function split(reg: string | RegExp, input: string, limit?: number, offset: number = 0) {
  if (typeof reg == "string") {
    const tmp = <string>reg;
    reg = create(input, limit);
    input = tmp;
    limit = undefined;
  }
  input = input.substring(offset);
  return input.split(<RegExp>reg, limit);
}