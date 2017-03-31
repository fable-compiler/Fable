import { IAsync } from "./AsyncBuilder"
import { IAsyncContext } from "./AsyncBuilder"
import { Continuation } from "./AsyncBuilder"
import { CancellationToken } from "./AsyncBuilder"
import { defaultCancellationToken } from "./Async"
import { fromContinuations } from "./Async"
import { startImmediate } from "./Async"

class QueueCell<Msg> {
  value: Msg;
  next: QueueCell<Msg>;

  constructor(message: Msg) {
    this.value = message;
  }
}

class MailboxQueue<Msg> {
  private firstAndLast: [QueueCell<Msg>, QueueCell<Msg>];

  add(message: Msg) {
    const itCell = new QueueCell(message);
    if (this.firstAndLast) {
      this.firstAndLast[1].next = itCell;
      this.firstAndLast = [this.firstAndLast[0], itCell];
    }
    else
      this.firstAndLast = [itCell, itCell];
  }

  tryGet() {
    if (this.firstAndLast) {
      const value = this.firstAndLast[0].value;
      if (this.firstAndLast[0].next)
        this.firstAndLast = [this.firstAndLast[0].next, this.firstAndLast[1]];
      else
        delete this.firstAndLast;
      return value;
    }
    return void 0;
  }
}

export type MailboxBody<Msg> = (m: MailboxProcessor<Msg>) => IAsync<void>;

export interface AsyncReplyChannel<Reply> {
  reply: (r: Reply) => void;
}

export default class MailboxProcessor<Msg> {
  private body: MailboxBody<Msg>;
  private cancellationToken: CancellationToken;
  private messages: MailboxQueue<Msg>;

  private continuation: Continuation<Msg>;

  constructor(body: MailboxBody<Msg>, cancellationToken?: CancellationToken) {
    this.body = body;
    this.cancellationToken = cancellationToken || defaultCancellationToken;
    this.messages = new MailboxQueue<Msg>();
  }

  __processEvents() {
    if (this.continuation) {
      const value = this.messages.tryGet();
      if (value) {
        const cont = this.continuation;
        delete this.continuation;
        cont(value);
      }
    }
  }

  start() {
    startImmediate(this.body(this), this.cancellationToken);
  }

  receive() {
    return fromContinuations((conts: Array<Continuation<Msg>>) => {
      if (this.continuation)
        throw new Error("Receive can only be called once!");

      this.continuation = conts[0];
      this.__processEvents();
    });
  }

  post(message: Msg) {
    this.messages.add(message);
    this.__processEvents();
  }

  postAndAsyncReply<Reply>(buildMessage: (c: AsyncReplyChannel<Reply>) => Msg) {
    let result: Reply;
    let continuation: Continuation<Reply>;
    function checkCompletion() {
      if (result && continuation)
        continuation(result);
    }
    const reply = {
      reply: (res: Reply) => {
        result = res;
        checkCompletion();
      }
    };
    this.messages.add(buildMessage(reply));
    this.__processEvents();
    return fromContinuations((conts: Array<Continuation<Reply>>) => {
      continuation = conts[0];
      checkCompletion();
    });
  }
}

export function start<Msg>(body: MailboxBody<Msg>, cancellationToken?: CancellationToken) {
  const mbox = new MailboxProcessor(body, cancellationToken);
  mbox.start();
  return mbox;
}

