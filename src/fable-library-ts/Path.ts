// This library doesn't depend on @types/node (to stay browser-compatible),
// so these Node.js built-in imports can't be type-checked; the exported
// functions below still declare their own types for callers to rely on.
// @ts-ignore
import { basename, extname, join } from "node:path";
// @ts-ignore
import { tmpdir } from "node:os";
// @ts-ignore
import { writeFileSync } from "node:fs";

// Mirrors the format of .NET's Path.GetRandomFileName(): 8 random hex chars,
// a dot, then 3 more random hex chars.
export function getRandomFileName(): string {
  const nameBytes = new Uint8Array(4);
  crypto.getRandomValues(nameBytes);
  const name = Array.from(nameBytes, (b) => b.toString(16).padStart(2, "0")).join("");

  const extBytes = new Uint8Array(2);
  crypto.getRandomValues(extBytes);
  const ext = Array.from(extBytes, (b) => b.toString(16).padStart(2, "0")).join("").slice(0, 3);

  return `${name}.${ext}`;
}

export function getFileNameWithoutExtension(path: string): string {
  return basename(path, extname(path));
}

export function hasExtension(path: string): boolean {
  return extname(path) !== "";
}

export function getTempFileName(): string {
  const path = join(tmpdir(), getRandomFileName());
  writeFileSync(path, "");
  return path;
}
