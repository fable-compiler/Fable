import { CancellationToken, OperationCanceledException } from "./AsyncBuilder.ts";
import { ArgumentException_$ctor_Z384F8060, InvalidOperationException_$ctor_Z721C83C5 } from "./System.ts";

export class TaskCompletionSource<T> {
  private _resolve!: (value: T) => void;
  private _reject!: (reason?: unknown) => void;
  private _completed = false;
  public task: Promise<T>;

  constructor() {
    this.task = new Promise<T>((resolve, reject) => {
      this._resolve = resolve;
      this._reject = reject;
    });
  }

  private trySettle(settle: () => void): boolean {
    if (this._completed) { return false; }
    this._completed = true;
    settle();
    return true;
  }

  private settle(settle: () => void): void {
    if (!this.trySettle(settle)) {
      throw InvalidOperationException_$ctor_Z721C83C5("An attempt was made to transition a task to a final state when it had already completed.");
    }
  }

  SetResult(value: T): void { this.settle(() => this._resolve(value)); }
  TrySetResult(value: T): boolean { return this.trySettle(() => this._resolve(value)); }
  SetException(error: unknown): void { this.settle(() => this._reject(error)); }
  TrySetException(error: unknown): boolean { return this.trySettle(() => this._reject(error)); }
  SetExceptions(errors: Iterable<unknown>): void { this.SetException(firstException(errors)); }
  TrySetExceptions(errors: Iterable<unknown>): boolean { return this.TrySetException(firstException(errors)); }
  SetCanceled(): void { this.settle(() => this._reject(new OperationCanceledException())); }
  TrySetCanceled(): boolean { return this.trySettle(() => this._reject(new OperationCanceledException())); }
  // Former spelling of SetCanceled
  SetCancelled(): void { this.SetCanceled(); }
  get_Task(): Promise<T> { return this.task; }
}

// Awaiting a task faulted with several exceptions throws the first one on .NET
function firstException(errors: Iterable<unknown>): unknown {
  const all = Array.from(errors);
  if (all.length === 0) {
    throw ArgumentException_$ctor_Z384F8060("The exceptions collection was empty.", "exceptions");
  }
  if (all.some((error) => error == null)) {
    throw ArgumentException_$ctor_Z384F8060("The exceptions collection included at least one null element.", "exceptions");
  }
  return all[0];
}

export function fromResult<T>(value: T): Promise<T> {
  return Promise.resolve(value);
}

// Like .NET, the function runs after the caller continues and not at all if the token is cancelled by then
export function run<T>(f: () => T | PromiseLike<T>, cancellationToken?: CancellationToken): Promise<T> {
  return Promise.resolve().then(() => {
    if (cancellationToken?.isCancelled) { throw new OperationCanceledException(); }
    return f();
  });
}

export function zero(): Promise<void> {
  return Promise.resolve();
}

// Like .NET, -1 waits until the token is cancelled
export function delay(millisecondsDelay: number, cancellationToken?: CancellationToken): Promise<void> {
  return new Promise<void>((resolve, reject) => {
    if (cancellationToken?.isCancelled) {
      reject(new OperationCanceledException());
      return;
    }
    let tokenId: number | undefined;
    const timeoutId = millisecondsDelay === -1 ? undefined : setTimeout(() => {
      if (tokenId !== undefined) { cancellationToken!.removeListener(tokenId); }
      resolve();
    }, millisecondsDelay);
    if (cancellationToken != null) {
      tokenId = cancellationToken.addListener(() => {
        clearTimeout(timeoutId);
        reject(new OperationCanceledException());
      });
    }
  });
}

// Task<T> = Promise<T> in JS/TS. GetAwaiter/GetResult return the Promise itself;
// callers should use Async.AwaitTask to extract the value in an async context.
export function getAwaiter<T>(t: Promise<T>): Promise<T> {
  return t;
}

export function getResult<T>(t: Promise<T>): Promise<T> {
  return t;
}
