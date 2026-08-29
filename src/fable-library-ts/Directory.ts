// This library doesn't depend on @types/node (to stay browser-compatible),
// so this Node.js built-in import can't be type-checked; the exported
// functions below still declare their own types for callers to rely on.
// @ts-ignore
import { existsSync, mkdirSync, statSync } from "node:fs";

export function exists(path: string): boolean {
  return existsSync(path) && statSync(path).isDirectory();
}

export function createDirectory(path: string): void {
  mkdirSync(path, { recursive: true });
}
