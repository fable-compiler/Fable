import { OperationCanceledException } from "./AsyncBuilder.ts";

export class TaskCompletionSource<T> {
  private _resolve!: (value: T) => void;
  private _reject!: (reason?: unknown) => void;
  public task: Promise<T>;

  constructor() {
    this.task = new Promise<T>((resolve, reject) => {
      this._resolve = resolve;
      this._reject = reject;
    });
  }

  SetResult(value: T): void { this._resolve(value); }
  SetException(error: unknown): void { this._reject(error); }
  SetCancelled(): void { this._reject(new OperationCanceledException()); }
  get_Task(): Promise<T> { return this.task; }
}

export function fromResult<T>(value: T): Promise<T> {
  return Promise.resolve(value);
}

export function zero(): Promise<void> {
  return Promise.resolve();
}

// Task<T> = Promise<T> in JS/TS. GetAwaiter/GetResult return the Promise itself;
// callers should use Async.AwaitTask to extract the value in an async context.
export function getAwaiter<T>(t: Promise<T>): Promise<T> {
  return t;
}

export function getResult<T>(t: Promise<T>): Promise<T> {
  return t;
}
