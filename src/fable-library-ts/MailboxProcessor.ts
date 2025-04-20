import { defaultCancellationToken } from "./Async.js";
import { fromContinuations } from "./Async.js";
import { startImmediate } from "./Async.js";
import { Async } from "./AsyncBuilder.js";
import { Continuation, Continuations } from "./AsyncBuilder.js";
import { CancellationToken } from "./AsyncBuilder.js";

class QueueCell<Msg> {
  public value: Msg;
  public next?: QueueCell<Msg>;

  constructor(message: Msg) {
    this.value = message;
  }
}

class MailboxQueue<Msg> {
  private firstAndLast?: [QueueCell<Msg>, QueueCell<Msg>];

  public add(message: Msg) {
    const itCell = new QueueCell(message);
    if (this.firstAndLast) {
      this.firstAndLast[1].next = itCell;
      this.firstAndLast = [this.firstAndLast[0], itCell];
    } else {
      this.firstAndLast = [itCell, itCell];
    }
  }

  public tryGet() {
    if (this.firstAndLast) {
      const value = this.firstAndLast[0].value;
      if (this.firstAndLast[0].next) {
        this.firstAndLast = [this.firstAndLast[0].next, this.firstAndLast[1]];
      } else {
        delete this.firstAndLast;
      }
      return value;
    }
    return void 0;
  }
}

export type MailboxBody<Msg> = (m: MailboxProcessor<Msg>) => Async<void>;

export interface AsyncReplyChannel<Reply> {
  reply: (r: Reply) => void;
}

export class MailboxProcessor<Msg> {
  public body: MailboxBody<Msg>;
  public cancellationToken: CancellationToken;
  public messages: MailboxQueue<Msg>;

  public continuation?: Continuation<Msg>;

  constructor(body: MailboxBody<Msg>, cancellationToken?: CancellationToken) {
    this.body = body;
    this.cancellationToken = cancellationToken || defaultCancellationToken;
    this.messages = new MailboxQueue<Msg>();
  }
}

function __processEvents<Msg>($this: MailboxProcessor<Msg>) {
  if ($this.continuation) {
    const value = $this.messages.tryGet();
    if (value) {
      const cont = $this.continuation;
      delete $this.continuation;
      cont(value);
    }
  }
}

export function startInstance<Msg>($this: MailboxProcessor<Msg>) {
  startImmediate($this.body($this), $this.cancellationToken);
}

export function receive<Msg>($this: MailboxProcessor<Msg>) {
  return fromContinuations((conts: Continuations<Msg>) => {
    if ($this.continuation) {
      throw new Error("Receive can only be called once!");
    }
    $this.continuation = conts[0];
    __processEvents($this);
  });
}

export function post<Msg>($this: MailboxProcessor<Msg>, message: Msg) {
  $this.messages.add(message);
  __processEvents($this);
}

export function postAndAsyncReply<Reply, Msg>(
  $this: MailboxProcessor<Msg>,
  buildMessage: (c: AsyncReplyChannel<Reply>) => Msg,
) {
  let result: Reply;
  let continuation: Continuation<Reply>;
  function checkCompletion() {
    if (result !== void 0 && continuation !== void 0) {
      continuation(result);
    }
  }
  const reply = {
    reply: (res: Reply) => {
      result = res;
      checkCompletion();
    },
  };
  $this.messages.add(buildMessage(reply));
  __processEvents($this);
  return fromContinuations((conts: Continuations<Reply>) => {
    continuation = conts[0];
    checkCompletion();
  });
}

export function start<Msg>(body: MailboxBody<Msg>, cancellationToken?: CancellationToken) {
  const mbox = new MailboxProcessor(body, cancellationToken);
  startInstance(mbox);
  return mbox;
}

export default MailboxProcessor;
