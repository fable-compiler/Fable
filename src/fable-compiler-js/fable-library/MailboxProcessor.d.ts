import { IAsync } from "./AsyncBuilder.js";
import { Continuation } from "./AsyncBuilder.js";
import { CancellationToken } from "./AsyncBuilder.js";
declare class MailboxQueue<Msg> {
    private firstAndLast?;
    add(message: Msg): void;
    tryGet(): Msg | undefined;
}
export type MailboxBody<Msg> = (m: MailboxProcessor<Msg>) => IAsync<void>;
export interface AsyncReplyChannel<Reply> {
    reply: (r: Reply) => void;
}
export declare class MailboxProcessor<Msg> {
    body: MailboxBody<Msg>;
    cancellationToken: CancellationToken;
    messages: MailboxQueue<Msg>;
    continuation?: Continuation<Msg>;
    constructor(body: MailboxBody<Msg>, cancellationToken?: CancellationToken);
}
export declare function startInstance<Msg>($this: MailboxProcessor<Msg>): void;
export declare function receive<Msg>($this: MailboxProcessor<Msg>): (ctx: import("./AsyncBuilder.js").IAsyncContext<Msg>) => void;
export declare function post<Msg>($this: MailboxProcessor<Msg>, message: Msg): void;
export declare function postAndAsyncReply<Reply, Msg>($this: MailboxProcessor<Msg>, buildMessage: (c: AsyncReplyChannel<Reply>) => Msg): (ctx: import("./AsyncBuilder.js").IAsyncContext<Reply>) => void;
export declare function start<Msg>(body: MailboxBody<Msg>, cancellationToken?: CancellationToken): MailboxProcessor<Msg>;
export default MailboxProcessor;
