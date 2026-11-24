import { Exception } from "./Util.ts";
import { ArgumentOutOfRangeException_$ctor_Z384F8060, ArgumentOutOfRangeException_$ctor_Z721C83C5, NotSupportedException_$ctor_Z721C83C5 } from "./System.ts";

export type MatchEvaluator = (match: any) => string;

export function create(pattern: string, options = 0) {
  // Supported RegexOptions
  // * IgnoreCase:  0x0001
  // * Multiline:   0x0002
  // * Compiled:    0x0008 (ignored)
  // * Singleline:  0x0010
  // * ECMAScript:  0x0100 (ignored)
  // * NonBacktracking: 0x0400 (ignored)
  if ((options & ~(1 ^ 2 ^ 8 ^ 16 ^ 256 ^ 1024)) !== 0) {
    throw new Exception("RegexOptions only supports: IgnoreCase, Multiline, Compiled, Singleline, ECMAScript and NonBacktracking");
  }
  // Set always global and unicode flags for compatibility with dotnet, see #2925
  let flags = "gu";
  flags += options & 1 ? "i" : ""; // 0x0001 RegexOptions.IgnoreCase
  flags += options & 2 ? "m" : "";
  flags += options & 16 ? "s" : "";
  return new RegExp(pattern, flags);
}
// From http://stackoverflow.com/questions/3446170/escape-string-for-use-in-javascript-regex

export function escape(str: string) {
  // Matches the characters escaped by .NET's Regex.Escape.
  // Note:
  //
  //  .NET also escapes space and # (relevant for IgnorePatternWhitespace mode),
  //  but JS unicode-mode regex rejects \  and \# as invalid escapes, and we don't
  //  support IgnorePatternWhitespace, so we omit them.
  //
  //  .NET does not escape ] and } but JS unicode-mode regex rejects bare ] and }
  //  as invalid, so we escape them too for compatibility.
  return str.replace(/[$()*+.?[\\\^{|}\]]/g, "\\$&");
}

export function unescape(str: string) {
  return str.replace(/\\([$()*+.?[\\\^{|}\]])/g, "$1");
}

// .NET accepts Regex.InfiniteMatchTimeout (-1 ms) and 0 < timeout <= Int32.MaxValue - 1 ms
export function checkMatchTimeout(ms: number) {
  if (ms !== -1 && !(ms > 0 && ms <= 2147483646)) {
    throw ArgumentOutOfRangeException_$ctor_Z721C83C5("matchTimeout");
  }
}

function checkStartAt(input: string, startAt: number) {
  if (startAt !== 0 && (startAt < 0 || startAt > input.length)) {
    throw ArgumentOutOfRangeException_$ctor_Z384F8060("startat", "Start index cannot be less than 0 or greater than input length.");
  }
}

function checkCount(count: number | undefined, min: number) {
  if (count != null && count < min) {
    throw ArgumentOutOfRangeException_$ctor_Z384F8060("count", "Count cannot be less than -1.");
  }
}

function startsInsideSurrogatePair(reg: RegExp, input: string, startAt: number) {
  return startAt > 0 && reg.unicode && (input.charCodeAt(startAt) & 0xFC00) === 0xDC00 && (input.charCodeAt(startAt - 1) & 0xFC00) === 0xD800;
}

function execFrom(reg: RegExp, input: string, startAt: number) {
  if (startsInsideSurrogatePair(reg, input, startAt)) {
    // A unicode regex moves a lastIndex set inside a surrogate pair back to the start of the pair
    reg.lastIndex = startAt;
    const m = reg.exec(input.substring(0, startAt - 1) + "\uFFFD" + input.substring(startAt));
    if (m != null && m.index === startAt) {
      m.input = input;
      return m;
    }
    startAt++;
  }
  reg.lastIndex = startAt;
  return reg.exec(input);
}

export function isMatch(reg: RegExp, input: string, startAt = 0) {
  checkStartAt(input, startAt);
  if (startsInsideSurrogatePair(reg, input, startAt)) {
    return execFrom(reg, input, startAt) != null;
  }
  reg.lastIndex = startAt;
  return reg.test(input);
}

// A match is the array returned by `exec`, which has no reference to its regex
const matchRegex = Symbol("matchRegex");

type MatchWithRegex = RegExpExecArray & { [matchRegex]?: RegExp };

function setMatchRegex<M extends RegExpExecArray | null>(m: M, reg: RegExp): M {
  if (m != null) {
    (m as MatchWithRegex)[matchRegex] = reg;
  }
  return m;
}

interface GroupInfo {
  unnamed: number[];
  names: string[];
  reordered: boolean;
}

const groupInfosByRegex = new WeakMap<RegExp, GroupInfo>();
const groupInfosBySource = new Map<string, GroupInfo>();

function scanGroups(source: string): [GroupInfo, number] {
  const info: GroupInfo = { unnamed: [], names: [], reordered: false };
  let index = 0;
  let inClass = false;
  for (let i = 0; i < source.length; i++) {
    const c = source[i];
    if (c === "\\") {
      i++;
    } else if (inClass) {
      inClass = c !== "]";
    } else if (c === "[") {
      inClass = true;
    } else if (c === "(") {
      if (source[i + 1] !== "?") {
        info.unnamed.push(++index);
        info.reordered ||= info.names.length > 0;
      } else if (source[i + 2] === "<" && source[i + 3] !== "=" && source[i + 3] !== "!") {
        index++;
        const name = source.substring(i + 3, source.indexOf(">", i));
        if (info.names.includes(name)) {
          info.reordered = true;
        } else {
          info.names.push(name);
        }
      }
    }
  }
  return [info, index];
}

function checkedGroupInfo(reg: RegExp): GroupInfo {
  const [scanned, count] = scanGroups(reg.source);
  const probe = new RegExp("(?:" + reg.source + ")|", reg.flags).exec("")!;
  const names = Object.keys(probe.groups ?? {});
  if (count === probe.length - 1 && scanned.names.join(">") === names.join(">")) {
    return scanned;
  }
  return { unnamed: Array.from({ length: probe.length - 1 - names.length }, (_, i) => i + 1), names, reordered: false };
}

function getGroupInfo(reg: RegExp): GroupInfo {
  let info = groupInfosByRegex.get(reg);
  if (info == null) {
    info = groupInfosBySource.get(reg.source);
    if (info == null) {
      info = checkedGroupInfo(reg);
      if (groupInfosBySource.size >= 256) {
        groupInfosBySource.clear();
      }
      groupInfosBySource.set(reg.source, info);
      groupInfosByRegex.set(reg, info);
    }
  }
  return info;
}

// .NET numbers the unnamed groups first and then the named groups, JS numbers all the groups in pattern order
function toNetGroups(m: RegExpExecArray, reg: RegExp): RegExpExecArray {
  // `exec` sets `groups` only when the regex has named groups
  if (m.groups === undefined) {
    return m;
  }
  const info = getGroupInfo(reg);
  if (!info.reordered) {
    return m;
  }
  const { unnamed, names } = info;
  const res = new Array(1 + unnamed.length + names.length) as MatchWithRegex;
  res[0] = m[0];
  for (let k = 0; k < unnamed.length; k++) {
    res[k + 1] = m[unnamed[k]];
  }
  for (let k = 0; k < names.length; k++) {
    res[k + 1 + unnamed.length] = m.groups[names[k]];
  }
  res.index = m.index;
  res.input = m.input;
  res.groups = m.groups;
  res[matchRegex] = reg;
  return res;
}

export function getGroupNames(reg: RegExp): string[] {
  const info = getGroupInfo(reg);
  return ["0", ...info.unnamed.map((_, i) => String(i + 1)), ...info.names];
}

export function getGroupNumbers(reg: RegExp): Int32Array {
  const info = getGroupInfo(reg);
  return Int32Array.from({ length: 1 + info.unnamed.length + info.names.length }, (_, i) => i);
}

export function groupNameFromNumber(reg: RegExp, i: number): string {
  return getGroupNames(reg)[i] ?? "";
}

export function groupNumberFromName(reg: RegExp, name: string): number {
  const info = getGroupInfo(reg);
  if (info.names.length > 0) {
    return getGroupNames(reg).indexOf(name);
  }
  const i = /^\d+$/.test(name) ? Number(name) : -1;
  return i <= info.unnamed.length ? i : -1;
}

// .NET returns the same `Match.Empty` for every failed match
const emptyMatch = Object.assign([undefined], { index: 0, input: "" }) as unknown as RegExpExecArray;

function matchOrNull(reg: RegExp, input: string, startAt: number) {
  return setMatchRegex(execFrom(reg, input, startAt), reg);
}

export function match(reg: RegExp, input: string, startAt = 0): RegExpExecArray {
  checkStartAt(input, startAt);
  return matchOrNull(reg, input, startAt) ?? emptyMatch;
}

function nextStartAt(reg: RegExp, m: RegExpExecArray) {
  const startAt = m.index + m[0].length;
  if (m[0].length > 0) {
    return startAt;
  }
  // A unicode regex moves a lastIndex set inside a surrogate pair back to the start of the pair
  return startAt + (reg.unicode && (m.input.codePointAt(startAt) ?? 0) > 0xFFFF ? 2 : 1);
}

export function nextMatch(m: RegExpExecArray | null): RegExpExecArray {
  if (m == null || m[0] == null) {
    return emptyMatch;
  }
  const reg = (m as MatchWithRegex)[matchRegex];
  if (reg == null) {
    throw new Exception("Match.NextMatch is only supported on a match returned by Regex");
  }
  if (!reg.global) {
    throw new Exception("Non-global RegExp"); // lastIndex is ignored
  }
  return matchOrNull(reg, m.input, nextStartAt(reg, m)) ?? emptyMatch;
}

export function groups(m: RegExpExecArray | null): RegExpExecArray {
  if (m == null) {
    return emptyMatch;
  }
  if (m.groups === undefined) {
    return m;
  }
  const reg = (m as MatchWithRegex)[matchRegex];
  return reg == null ? m : toNetGroups(m, reg);
}

export function groupByName(groups: RegExpExecArray, name: string): string | undefined {
  const value = groups.groups?.[name];
  const reg = (groups as MatchWithRegex)[matchRegex];
  if (value !== undefined || reg == null || !/^\d+$/.test(name)) {
    return value;
  }
  const i = groupNumberFromName(reg, name);
  return i < 0 ? undefined : groups[i];
}

// A `Match` seen as a `Group` is still the array returned by `exec`
export function groupValue(g: string | RegExpExecArray | null | undefined): string {
  return (Array.isArray(g) ? g[0] : g) ?? "";
}

export function groupSuccess(g: string | RegExpExecArray | null | undefined): boolean {
  return (Array.isArray(g) ? g[0] : g) != null;
}

export function matchAt(matches: RegExpExecArray[], i: number): RegExpExecArray {
  if (i < 0 || i >= matches.length) {
    throw ArgumentOutOfRangeException_$ctor_Z721C83C5("i");
  }
  return matches[i];
}

function* matchesFrom(reg: RegExp, input: string, startAt: number) {
  if (!reg.global) {
    throw new Exception("Non-global RegExp"); // lastIndex is ignored
  }
  let m: RegExpExecArray | null;
  while ((m = matchOrNull(reg, input, startAt)) != null) {
    yield m;
    startAt = nextStartAt(reg, m);
  }
}

export function matches(reg: RegExp, input: string, startAt = 0) {
  if (input == null) {
    throw new Exception("Input cannot ve null");
  }
  checkStartAt(input, startAt);
  return Array.from(matchesFrom(reg, input, startAt));
}

export function count(reg: RegExp, input: string): number {
  return matches(reg, input).length;
}

export function options(reg: RegExp) {
  let options = 256; // ECMAScript
  options |= reg.ignoreCase ? 1 : 0;
  options |= reg.multiline ? 2 : 0;
  return options;
}

// .NET replacement patterns cannot be rewritten as JS replacement strings unambiguously
// (e.g. `$$0` must stay literal "$0", `$0` doesn't work with JS regex (see #1155), and
// `${1}` followed by a digit must not become `$1` + digit), so interpret them
// honoring the `$$` escape left-to-right
function expandReplacement(rep: string, match: RegExpExecArray, reg: RegExp): string {
  const groups = toNetGroups(match, reg);
  return rep.replace(/\$(?:(\$)|(&)|(`)|(')|(\d+)|\{(\d+)\}|\{([^}]+)\})/g,
    (s, dollar?: string, and?: string, before?: string, after?: string, num?: string, numBraced?: string, name?: string) => {
      if (dollar != null) { return "$"; }
      if (and != null) { return match[0]; }
      if (before != null) { return match.input.substring(0, match.index); }
      if (after != null) { return match.input.substring(match.index + match[0].length); }
      const n = num ?? numBraced;
      if (n != null) {
        const i = parseInt(n, 10);
        // Same as .NET: an unmatched group is replaced by an empty string,
        // a nonexistent group number keeps the substitution pattern literally
        return i < groups.length ? (groups[i] ?? "") : s;
      }
      // Named groups are `${name}` in .NET (in regex: groups are `(?<name>...)` in both)
      return match.groups != null && (name as string) in match.groups ? (match.groups[name as string] ?? "") : s;
    });
}

export function result(m: RegExpExecArray | null, replacement: string): string {
  const reg = (m as MatchWithRegex | null)?.[matchRegex];
  if (m == null || reg == null) {
    throw NotSupportedException_$ctor_Z721C83C5("Result cannot be called on a failed Match.");
  }
  return expandReplacement(replacement, m, reg);
}

export function replace(
  reg: string | RegExp, input: string,
  replacement: string | MatchEvaluator,
  limit?: number, offset: number = 0): string {
  if (typeof reg === "string") {
    const tmp = reg as string;
    reg = create(input, limit ?? 0);
    input = tmp;
    limit = undefined;
  }
  checkCount(limit, -1);
  checkStartAt(input, offset);
  if (typeof replacement === "string") {
    const rep = replacement as string;
    replacement = (match: any): string => expandReplacement(rep, match, reg as RegExp);
  }
  let result = "";
  let prev = 0;
  let count = limit ?? -1;
  for (const m of matchesFrom(reg as RegExp, input, offset)) {
    if (count === 0) {
      break;
    }
    result += input.substring(prev, m.index) + ((replacement as MatchEvaluator)(m) ?? "");
    prev = m.index + m[0].length;
    count--;
  }
  return result + input.substring(prev);
}

export function split(reg: string | RegExp, input: string, limit?: number, offset: number = 0) {
  if (typeof reg === "string") {
    const tmp = reg as string;
    reg = create(input, limit ?? 0);
    input = tmp;
    limit = undefined;
  }
  checkCount(limit, 0);
  checkStartAt(input, offset);
  // JS String.split(regex, limit) truncates the result and discards the remainder,
  // whereas .NET keeps it in the last element (`limit` is the max number of elements),
  // keeps the text before `offset` in the first element and includes the values of
  // matched capture groups, so iterate the matches manually like .NET Regex.Split does
  const result: string[] = [];
  let prev = 0;
  let count = limit == null ? -1 : limit - 1;
  for (const m of matches(reg as RegExp, input, offset)) {
    if (count === 0) {
      break;
    }
    result.push(input.substring(prev, m.index));
    prev = m.index + m[0].length;
    const groups = toNetGroups(m, reg as RegExp);
    for (let i = 1; i < groups.length; i++) {
      if (groups[i] !== undefined) {
        result.push(groups[i]);
      }
    }
    count--;
  }
  result.push(input.substring(prev));
  return result;
}
