import { Event } from "./Event.js";
import { IDisposable } from "./Util.js";

export class Timer implements IDisposable {
  public Interval: number;
  public AutoReset: boolean;

  private _elapsed: Event<Date>;
  private _enabled: boolean = false;
  private _isDisposed: boolean = false;
  private _intervalId: number = 0;
  private _timeoutId: number = 0;

  constructor(interval?: number) {
    this.Interval = interval && interval > 0 ? interval : 100;
    this.AutoReset = true;
    this._elapsed = new Event<Date>();
  }

  Elapsed() {
    return this._elapsed.Publish;
  }

  get Enabled() {
    return this._enabled;
  }

  set Enabled(x: boolean) {
    if (!this._isDisposed && this._enabled !== x) {
      this._enabled = x;
      if (this._enabled) {
        if (this.AutoReset) {
          this._intervalId = setInterval(() => {
            if (!this.AutoReset) {
              this.Enabled = false;
            }
            this._elapsed.Trigger(new Date());
          }, this.Interval) as any;
        } else {
          this._timeoutId = setTimeout(() => {
            this.Enabled = false;
            this._timeoutId = 0;
            if (this.AutoReset) {
              this.Enabled = true;
            }
            this._elapsed.Trigger(new Date());
          }, this.Interval) as any;
        }
      } else {
        if (this._timeoutId) {
          clearTimeout(this._timeoutId);
          this._timeoutId = 0;
        }
        if (this._intervalId) {
          clearInterval(this._intervalId);
          this._intervalId = 0;
        }
      }
    }
  }

  public Dispose() {
    this.Enabled = false;
    this._isDisposed = true;
  }

  public Close() {
    this.Dispose();
  }

  public Start() {
    this.Enabled = true;
  }

  public Stop() {
    this.Enabled = false;
  }
}

export default Timer;
