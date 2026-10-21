import { int64 } from "./BigInt.ts";

// Timestamps are in microseconds, the resolution performance.now() can offer
const ticksPerMillisecond = 1000;

export class Stopwatch {
  // Milliseconds measured before the last Start
  public elapsed = 0;
  // performance.now() at the last Start, while running
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
    sw.startedAt = performance.now();
  }
}

export function stop(sw: Stopwatch): void {
  if (sw.startedAt !== undefined) {
    sw.elapsed += performance.now() - sw.startedAt;
    sw.startedAt = undefined;
  }
}

export function reset(sw: Stopwatch): void {
  sw.elapsed = 0;
  sw.startedAt = undefined;
}

export function restart(sw: Stopwatch): void {
  sw.elapsed = 0;
  sw.startedAt = performance.now();
}

export function isRunning(sw: Stopwatch): boolean {
  return sw.startedAt !== undefined;
}

function rawElapsedMilliseconds(sw: Stopwatch): number {
  return sw.elapsed + (sw.startedAt === undefined ? 0 : performance.now() - sw.startedAt);
}

// In units of Frequency, like .NET (not TimeSpan ticks)
export function elapsedTicks(sw: Stopwatch): int64 {
  return BigInt(Math.trunc(rawElapsedMilliseconds(sw) * ticksPerMillisecond));
}

export function elapsedMilliseconds(sw: Stopwatch): int64 {
  return elapsedTicks(sw) / BigInt(ticksPerMillisecond);
}

export function elapsedTotalMilliseconds(sw: Stopwatch): number {
  return Number(elapsedTicks(sw)) / ticksPerMillisecond;
}

export function frequency(): int64 {
  return BigInt(ticksPerMillisecond * 1000);
}

export function isHighResolution(): boolean {
  return true;
}

export function getTimestamp(): int64 {
  return BigInt(Math.trunc(performance.now() * ticksPerMillisecond));
}

export function getElapsedTotalMilliseconds(startingTimestamp: int64, endingTimestamp?: int64): number {
  return Number((endingTimestamp ?? getTimestamp()) - startingTimestamp) / ticksPerMillisecond;
}
