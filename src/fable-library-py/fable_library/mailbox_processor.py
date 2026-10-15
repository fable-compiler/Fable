from __future__ import annotations

import asyncio
from collections.abc import Callable
from queue import SimpleQueue
from threading import Lock, RLock
from typing import Any

from .async_ import call_on_owner_loop, from_continuations, start_immediate
from .async_builder import (
    Async,
    CancellationToken,
    Continuations,
    IAsyncContext,
    OperationCanceledError,
    protected_cont,
)


class AsyncReplyChannel[Reply]:
    def __init__(self, fn: Callable[[Any], None]) -> None:
        self.fn = fn

    def reply(self, r: Any) -> None:
        self.fn(r)


class MailboxProcessor[Msg]:
    def __init__(
        self,
        body: Callable[[MailboxProcessor[Msg]], Async[None]],
        cancellation_token: CancellationToken | None = None,
    ):
        self.messages: SimpleQueue[Msg] = SimpleQueue()
        self.token = cancellation_token or CancellationToken()
        self.lock = RLock()
        self.body = body

        # Holds the continuation i.e the `done` callback of Async.from_continuations returned by `receive`.
        self.continuation: Continuations[Any] | None = None

    def post(self, msg: Msg) -> None:
        """Post a message synchronously to the mailbox processor.

        This method is not asynchronous since it's very fast to execute.
        It simply adds the message to the message queue of the mailbox
        processor and returns.

        Args:
            msg: Message to post.

        Returns:
            None
        """
        self.messages.put(msg)
        self.__process_events()

    def post_and_async_reply[Reply](self, build_message: Callable[[AsyncReplyChannel[Reply]], Msg]) -> Async[Reply]:
        """Post a message asynchronously to the mailbox processor and
        wait for the reply.

        Args:
            build_message: A function that takes a reply channel
            (`AsyncReplyChannel[Reply]`) and returns a message to send
            to the mailbox processor. The message should contain the
            reply channel as e.g a tuple.

        Returns:
            The reply from mailbox processor.
        """

        result: Reply | None = None
        # Track whether `reply` was actually called, since the reply value itself
        # may legitimately be `None` (e.g. `AsyncReplyChannel[None]` for unit).
        replied = False
        gate = Lock()
        continuation: Continuations[Any] | None = (
            None  # This is the continuation for the `done` callback of the awaiting poster.
        )

        def check_completion() -> None:
            nonlocal continuation
            # reply may run on another thread before the poster's continuation is installed
            with gate:
                if not replied or continuation is None:
                    return
                deliver = continuation[0]
                # Consume this waiter once, retaining the reply for later executions.
                continuation = None
            deliver(result)

        def reply_callback(res: Reply):
            nonlocal result, replied
            # .NET AsyncReplyChannel.Reply ignores a second reply
            with gate:
                if replied:
                    return
                result = res
                replied = True
            check_completion()

        reply_channel: AsyncReplyChannel[Reply] = AsyncReplyChannel(reply_callback)
        self.messages.put(build_message(reply_channel))
        self.__process_events()

        def callback(conts: Continuations[Any]) -> None:
            nonlocal continuation
            with gate:
                continuation = conts
            check_completion()

        return from_continuations(callback)

    def receive(self) -> Async[Msg]:
        """Receive message from mailbox.

        Returns:
            An asynchronous computation which will consume the
            first message in arrival order. No thread is blocked while
            waiting for further messages. Cancelling the token of the
            receiving computation cancels a pending receive.
        """

        def cont(ctx: IAsyncContext[Msg]) -> None:
            loop = asyncio.get_running_loop()
            token_id: int | None = None

            def on_message(msg: Msg) -> None:
                if token_id is not None:
                    ctx.cancel_token.remove_listener(token_id)
                ctx.on_success(msg)

            continuation: Continuations[Any] = (on_message, ctx.on_error, ctx.on_cancel)

            def on_cancel() -> None:
                with self.lock:
                    if self.continuation is not continuation:
                        return
                    self.continuation = None
                call_on_owner_loop(loop, lambda: ctx.on_cancel(OperationCanceledError()))

            with self.lock:
                if self.continuation:
                    raise Exception("Receive can only be called once!")
                self.continuation = continuation

            token_id = ctx.cancel_token.add_listener(on_cancel)
            with self.lock:
                if self.continuation is not continuation:
                    return
            self.__process_events()

        return protected_cont(cont)

    def __process_events(self) -> None:
        with self.lock:
            if not self.continuation or self.messages.empty():
                return
            msg = self.messages.get()
            self.continuation, cont = None, self.continuation

        if cont:
            cont[0](msg)

    @classmethod
    def start(
        cls,
        body: Callable[[MailboxProcessor[Msg]], Async[None]],
        cancellation_token: CancellationToken | None = None,
    ) -> MailboxProcessor[Msg]:
        mbox: MailboxProcessor[Msg] = MailboxProcessor(body, cancellation_token)
        start_immediate(body(mbox), mbox.token)
        return mbox


def receive[Msg](mbox: MailboxProcessor[Msg]) -> Async[Msg]:
    return mbox.receive()


def post[Msg](mbox: MailboxProcessor[Msg], msg: Msg):
    return mbox.post(msg)


def post_and_async_reply[Msg, Reply](
    mbox: MailboxProcessor[Msg], build_message: Callable[[AsyncReplyChannel[Reply]], Msg]
) -> Async[Reply]:
    return mbox.post_and_async_reply(build_message)


def start_instance(mbox: MailboxProcessor[Any]) -> None:
    body = mbox.body(mbox)
    return start_immediate(body, mbox.token)


def start[Msg](
    body: Callable[[MailboxProcessor[Msg]], Async[None]],
    cancellationToken: CancellationToken | None = None,
) -> MailboxProcessor[Msg]:
    mbox = MailboxProcessor(body, cancellationToken)
    start_instance(mbox)
    return mbox


__all__ = [
    "AsyncReplyChannel",
    "MailboxProcessor",
    "post",
    "post_and_async_reply",
    "receive",
    "start",
    "start_instance",
]
