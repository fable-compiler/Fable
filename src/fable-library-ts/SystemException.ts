import { Exception } from "./Util.js";

export class SystemException extends Exception {
}

export class TimeoutException extends SystemException {
}
