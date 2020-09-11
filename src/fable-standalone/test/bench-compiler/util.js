import * as fs from "fs";
import * as Path from "path";

// JSON.stringify doesn't escape line/paragraph separators, which are not valid in JS.
// https://github.com/expressjs/express/issues/1132
export function escapeJsStringLiteral(str) {
  return JSON.stringify(str).slice(1, -1).replace(/\u2028/g, '\\u2028').replace(/\u2029/g, '\\u2029');
}

export function ensureDirExists(dir, cont) {
  if (fs.existsSync(dir)) {
    if (typeof cont === "function") { cont(); }
  } else {
    ensureDirExists(Path.dirname(dir), () => {
      if (!fs.existsSync(dir)) { fs.mkdirSync(dir); }
      if (typeof cont === "function") { cont(); }
    });
  }
}

export function getDirFiles(dir) {
  if (!fs.existsSync(dir)) return [];
  const files = fs.readdirSync(dir).map((subdir) => {
    const res = Path.resolve(dir, subdir);
    return fs.statSync(res).isDirectory() ? getDirFiles(res) : res;
  });
  return files.reduce((a, f) => a.concat(f), []);
}

export function serializeToJson(data) {
  return JSON.stringify(data, (key, value) => {
    if (value === Infinity) {
      return "Infinity";
    } else if (value === -Infinity) {
      return "-Infinity";
    } else if (value !== value) {
      return "NaN";
    }
    return value;
  });
}
