import { Exception } from "./Types.js";

export declare class SystemException extends Exception {
    constructor();
}

export declare class TimeoutException extends SystemException {
    constructor();
}
