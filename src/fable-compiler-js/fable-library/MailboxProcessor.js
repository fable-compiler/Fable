import { defaultCancellationToken } from "./Async.js";
import { fromContinuations } from "./Async.js";
import { startImmediate } from "./Async.js";
class QueueCell {
    constructor(message) {
        this.value = message;
    }
}
class MailboxQueue {
    add(message) {
        const itCell = new QueueCell(message);
        if (this.firstAndLast) {
            this.firstAndLast[1].next = itCell;
            this.firstAndLast = [this.firstAndLast[0], itCell];
        }
        else {
            this.firstAndLast = [itCell, itCell];
        }
    }
    tryGet() {
        if (this.firstAndLast) {
            const value = this.firstAndLast[0].value;
            if (this.firstAndLast[0].next) {
                this.firstAndLast = [this.firstAndLast[0].next, this.firstAndLast[1]];
            }
            else {
                delete this.firstAndLast;
            }
            return value;
        }
        return void 0;
    }
}
export class MailboxProcessor {
    constructor(body, cancellationToken) {
        this.body = body;
        this.cancellationToken = cancellationToken || defaultCancellationToken;
        this.messages = new MailboxQueue();
    }
}
function __processEvents($this) {
    if ($this.continuation) {
        const value = $this.messages.tryGet();
        if (value) {
            const cont = $this.continuation;
            delete $this.continuation;
            cont(value);
        }
    }
}
export function startInstance($this) {
    startImmediate($this.body($this), $this.cancellationToken);
}
export function receive($this) {
    return fromContinuations((conts) => {
        if ($this.continuation) {
            throw new Error("Receive can only be called once!");
        }
        $this.continuation = conts[0];
        __processEvents($this);
    });
}
export function post($this, message) {
    $this.messages.add(message);
    __processEvents($this);
}
export function postAndAsyncReply($this, buildMessage) {
    let result;
    let continuation;
    function checkCompletion() {
        if (result !== void 0 && continuation !== void 0) {
            continuation(result);
        }
    }
    const reply = {
        reply: (res) => {
            result = res;
            checkCompletion();
        },
    };
    $this.messages.add(buildMessage(reply));
    __processEvents($this);
    return fromContinuations((conts) => {
        continuation = conts[0];
        checkCompletion();
    });
}
export function start(body, cancellationToken) {
    const mbox = new MailboxProcessor(body, cancellationToken);
    startInstance(mbox);
    return mbox;
}
export default MailboxProcessor;
