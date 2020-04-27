export type MatchEvaluator = (match: any) => string;

export function create(pattern: string, options: number = 0) {
  // Supported RegexOptions
  // * IgnoreCase:  0x0001
  // * Multiline:   0x0002
  // * Singleline:  0x0010
  // * ECMAScript:  0x0100 (ignored)
  if ((options & ~(1 ^ 2 ^ 16 ^ 256)) !== 0) {
    throw new Error("RegexOptions only supports: IgnoreCase, Multiline, Singleline and ECMAScript");
  }
  let flags = "g";
  flags += options & 1 ? "i" : ""; // 0x0001 RegexOptions.IgnoreCase
  flags += options & 2 ? "m" : "";
  flags += options & 16 ? "s" : "";
  return new RegExp(pattern, flags);
}
// From http://stackoverflow.com/questions/3446170/escape-string-for-use-in-javascript-regex

export function escape(str: string) {
  return str.replace(/[\-\[\]\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, "\\$&");
}

export function unescape(str: string) {
  return str.replace(/\\([\-\[\]\/\{\}\(\)\*\+\?\.\\\^\$\|])/g, "$1");
}

export function isMatch(str: string | RegExp, pattern: string, options: number = 0) {
  let reg: RegExp;
  reg = str instanceof RegExp
    ? (reg = str as RegExp, str = pattern, reg.lastIndex = options, reg)
    : reg = create(pattern, options);
  return reg.test(str as string);
}

export function match(str: string | RegExp, pattern: string, options: number = 0) {
  let reg: RegExp;
  reg = str instanceof RegExp
    ? (reg = str as RegExp, str = pattern, reg.lastIndex = options, reg)
    : reg = create(pattern, options);
  return reg.exec(str as string);
}

export function matches(str: string | RegExp, pattern: string, options: number = 0) {
  let reg: RegExp;
  reg = str instanceof RegExp
    ? (reg = str as RegExp, str = pattern, reg.lastIndex = options, reg)
    : reg = create(pattern, options);
  if (!reg.global) {
    throw new Error("Non-global RegExp"); // Prevent infinite loop
  }
  let m = reg.exec(str as string);
  const matches: RegExpExecArray[] = [];
  while (m !== null) {
    matches.push(m);
    m = reg.exec(str as string);
  }
  return matches;
}

export function options(reg: RegExp) {
  let options = 256; // ECMAScript
  options |= reg.ignoreCase ? 1 : 0;
  options |= reg.multiline ? 2 : 0;
  return options;
}

export function replace(
  reg: string | RegExp, input: string,
  replacement: string | MatchEvaluator,
  limit?: number, offset: number = 0): string {
  function replacer() {
    let res = arguments[0];
    if (limit) {
      limit--;
      const match: any = [];
      const len = arguments.length;
      for (let i = 0; i < len - 2; i++) {
        match.push(arguments[i]);
      }
      match.index = arguments[len - 2];
      match.input = arguments[len - 1];
      res = (replacement as MatchEvaluator)(match);
    }
    return res;
  }
  if (typeof reg === "string") {
    const tmp = reg as string;
    reg = create(input, limit ?? 0);
    input = tmp;
    limit = undefined;
  }
  if (typeof replacement === "function") {
    limit = limit == null ? -1 : limit;
    return input.substring(0, offset) + input.substring(offset).replace(reg as RegExp, replacer);
  } else {
    // $0 doesn't work with JS regex, see #1155
    replacement = replacement.replace(/\$0/g, (_s) => "$&");
    if (limit != null) {
      let m: RegExpExecArray;
      const sub1 = input.substring(offset);
      const _matches = matches(reg, sub1);
      const sub2 = matches.length > limit ? (m = _matches[limit - 1], sub1.substring(0, m.index + m[0].length)) : sub1;
      return input.substring(0, offset) + sub2.replace(reg as RegExp, replacement as string)
        + input.substring(offset + sub2.length);
    } else {
      return input.replace(reg as RegExp, replacement as string);
    }
  }
}

export function split(reg: string | RegExp, input: string, limit?: number, offset: number = 0) {
  if (typeof reg === "string") {
    const tmp = reg as string;
    reg = create(input, limit ?? 0);
    input = tmp;
    limit = undefined;
  }
  input = input.substring(offset);
  return input.split(reg as RegExp, limit);
}
