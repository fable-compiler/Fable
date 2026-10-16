import { int64 } from "./BigInt.ts";

// performance.now() is available in browsers, Node.js, Deno and Bun; it is monotonic
// and has sub-millisecond resolution. Date.now() is only a fallback.
const now: () => number = typeof performance !== "undefined" && typeof performance.now === "function"
  ? () => performance.now()
  : () => Date.now();

// Timestamps are in microseconds, the resolution performance.now() can offer
const ticksPerMillisecond = 1000;

export class Stopwatch {
  // Milliseconds measured before the last Start
  public elapsed = 0;
  // now() at the last Start, while running
  public startedAt: number | undefined = undefined;
}

export function create(): Stopwatch {
  return new Stopwatch();
}

export function startNew(): Stopwatch {
  const sw = new Stopwatch();
  start(sw);
  return sw;
}

export function start(sw: Stopwatch): void {
  if (sw.startedAt === undefined) {
    sw.startedAt = now();
  }
}

export function stop(sw: Stopwatch): void {
  if (sw.startedAt !== undefined) {
    sw.elapsed += now() - sw.startedAt;
    sw.startedAt = undefined;
  }
}

export function reset(sw: Stopwatch): void {
  sw.elapsed = 0;
  sw.startedAt = undefined;
}

export function restart(sw: Stopwatch): void {
  sw.elapsed = 0;
  sw.startedAt = now();
}

export function isRunning(sw: Stopwatch): boolean {
  return sw.startedAt !== undefined;
}

// Used for Elapsed, which is converted to a TimeSpan by the compiler
export function elapsedTotalMilliseconds(sw: Stopwatch): number {
  return sw.elapsed + (sw.startedAt === undefined ? 0 : now() - sw.startedAt);
}

export function elapsedMilliseconds(sw: Stopwatch): int64 {
  return BigInt(Math.trunc(elapsedTotalMilliseconds(sw)));
}

// In units of Frequency, like .NET (not TimeSpan ticks)
export function elapsedTicks(sw: Stopwatch): int64 {
  return BigInt(Math.trunc(elapsedTotalMilliseconds(sw) * ticksPerMillisecond));
}

export function frequency(): int64 {
  return BigInt(ticksPerMillisecond * 1000);
}

export function isHighResolution(): boolean {
  return typeof performance !== "undefined" && typeof performance.now === "function";
}

export function getTimestamp(): int64 {
  return BigInt(Math.trunc(now() * ticksPerMillisecond));
}

// Used for GetElapsedTime, which is converted to a TimeSpan by the compiler
export function getElapsedTotalMilliseconds(startingTimestamp: int64, endingTimestamp?: int64): number {
  return Number((endingTimestamp ?? getTimestamp()) - startingTimestamp) / ticksPerMillisecond;
}
