import { IDisposable } from "./Util.js";
export declare class Timer implements IDisposable {
    Interval: number;
    AutoReset: boolean;
    private _elapsed;
    private _enabled;
    private _isDisposed;
    private _intervalId;
    private _timeoutId;
    constructor(interval?: number);
    Elapsed(): import("./Event.js").IEvent$2<import("./Event.js").Handler<Date>, Date>;
    get Enabled(): boolean;
    set Enabled(x: boolean);
    Dispose(): void;
    Close(): void;
    Start(): void;
    Stop(): void;
}
export default Timer;
