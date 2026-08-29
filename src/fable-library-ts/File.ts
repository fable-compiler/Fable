// This library doesn't depend on @types/node (to stay browser-compatible),
// so this Node.js built-in import can't be type-checked; the exported
// functions below still declare their own types for callers to rely on.
// @ts-ignore
import { readFileSync, writeFileSync } from "node:fs";
import { MutableArray } from "./Util.ts";

// byte[] is represented as a plain JS/TS array of numbers, not a typed array
// (see e.g. BitConverter.ts), so convert the Buffer returned by readFileSync.
export function readAllBytes(path: string): number[] {
  return Array.from(readFileSync(path));
}

export function readAllLines(path: string): string[] {
  const lines = readFileSync(path, "utf8").split(/\r\n|\r|\n/);

  if (lines[lines.length - 1] === "") {
    lines.pop();
  }

  return lines;
}

export function readAllText(path: string): string {
  return readFileSync(path, "utf8");
}

export function writeAllBytes(path: string, bytes: MutableArray<number>): void {
  // Convert back from the plain array representation to a typed array Node's fs accepts.
  writeFileSync(path, Uint8Array.from(bytes));
}

export function writeAllLines(path: string, lines: MutableArray<string>): void {
  writeFileSync(path, lines.join("\n"));
}
