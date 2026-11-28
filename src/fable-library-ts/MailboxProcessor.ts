import { defaultCancellationToken } from "./Async.ts";
import { fromContinuations } from "./Async.ts";
import { startImmediate } from "./Async.ts";
import { Async, IAsyncContext, OperationCanceledException, protectedCont } from "./AsyncBuilder.ts";
import { Continuation, Continuations } from "./AsyncBuilder.ts";
import { CancellationToken } from "./AsyncBuilder.ts";
import type { Option } from "./Option.ts";
import { some } from "./Option.ts";
import { TimeoutException_$ctor_Z721C83C5 } from "./System.ts";
import { Exception } from "./Util.ts";

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

  public tryGet(): { value: Msg } | undefined {
    if (this.firstAndLast) {
      const value = this.firstAndLast[0].value;
      if (this.firstAndLast[0].next) {
        this.firstAndLast = [this.firstAndLast[0].next, this.firstAndLast[1]];
      } else {
        delete this.firstAndLast;
      }
      // Wrap the value so falsy messages (0, false, "", null, undefined/unit)
      // can be distinguished from an empty queue.
      return { value };
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
    const dequeued = $this.messages.tryGet();
    if (dequeued !== void 0) {
      const cont = $this.continuation;
      delete $this.continuation;
      cont(dequeued.value);
    }
  }
}

export function startInstance<Msg>($this: MailboxProcessor<Msg>) {
  startImmediate($this.body($this), $this.cancellationToken);
}

// Waits for the next message; an omitted or negative timeout waits forever, like Timeout.Infinite
function receiveWithin<Msg, T>(
  $this: MailboxProcessor<Msg>,
  timeout: number | undefined,
  onMessage: (message: Msg) => T,
  onTimeout: (ctx: IAsyncContext<T>) => void,
): Async<T> {
  return protectedCont((ctx: IAsyncContext<T>) => {
    if ($this.continuation) {
      throw new Exception("Receive can only be called once!");
    }
    let tokenId: number | undefined;
    let timeoutId: ReturnType<typeof setTimeout> | undefined;
    const release = () => {
      if (timeoutId !== undefined) { clearTimeout(timeoutId); }
      if (tokenId !== undefined) { ctx.cancelToken.removeListener(tokenId); }
    };
    const continuation = (message: Msg) => {
      release();
      ctx.onSuccess(onMessage(message));
    };
    // Gives up the wait so later posts stay queued; false if it already settled
    const detach = () => {
      if ($this.continuation !== continuation) { return false; }
      delete $this.continuation;
      release();
      return true;
    };
    $this.continuation = continuation;
    tokenId = ctx.cancelToken.addListener(() => {
      if (detach()) { ctx.onCancel(new OperationCanceledException()); }
    });
    if ($this.continuation === continuation) {
      __processEvents($this);
    }
    if ($this.continuation !== continuation || timeout == null || timeout < 0) {
      return;
    }
    if (timeout === 0) {
      detach();
      onTimeout(ctx);
    } else {
      timeoutId = setTimeout(() => { if (detach()) { onTimeout(ctx); } }, timeout);
    }
  });
}

export function receive<Msg>($this: MailboxProcessor<Msg>, timeout?: number): Async<Msg> {
  return receiveWithin($this, timeout, (message: Msg) => message,
    (ctx) => ctx.onError(TimeoutException_$ctor_Z721C83C5("Mailbox.Receive timed out.")));
}

export function tryReceive<Msg>($this: MailboxProcessor<Msg>, timeout?: number): Async<Option<Msg>> {
  return receiveWithin($this, timeout, (message: Msg) => some(message),
    (ctx) => ctx.onSuccess(undefined));
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
  // Use an explicit flag because the reply value itself may be undefined
  // (e.g. AsyncReplyChannel<unit>), which must still complete the async.
  let replied = false;
  let continuation: Continuation<Reply>;
  function checkCompletion() {
    if (replied && continuation !== void 0) {
      continuation(result);
    }
  }
  const reply = {
    reply: (res: Reply) => {
      // .NET AsyncReplyChannel.Reply ignores a second reply
      if (replied) { return; }
      result = res;
      replied = true;
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
