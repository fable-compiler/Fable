from __future__ import annotations

from queue import SimpleQueue
from threading import RLock
from typing import Any, Callable, Generic, List, Optional, TypeVar

from .async_ import from_continuations, start_immediate
from .async_builder import Async, CancellationToken, OperationCanceledError


_Msg = TypeVar("_Msg")
_Reply = TypeVar("_Reply")


class AsyncReplyChannel(Generic[_Reply]):
    def __init__(self, fn: Callable[[Any], None]) -> None:
        self.fn = fn

    def reply(self, r: Any) -> None:
        self.fn(r)


class MailboxProcessor(Generic[_Msg]):
    def __init__(
        self,
        body: Callable[[MailboxProcessor[_Msg]], Async[None]],
        cancellation_token: Optional[CancellationToken] = None,
    ):
        self.messages: SimpleQueue[_Msg] = SimpleQueue()
        self.token = cancellation_token or CancellationToken()
        self.lock = RLock()
        self.body = body

        # Holds the continuation i.e the `done` callback of Async.from_continuations returned by `receive`.
        self.continuation: List[Callable[[Any], None]] = []

    def post(self, msg: _Msg) -> None:
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

    def post_and_async_reply(
        self, build_message: Callable[[AsyncReplyChannel[_Reply]], _Msg]
    ) -> Async[_Reply]:
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

        result: Optional[_Reply] = None
        continuation : List[Callable[[Any], None]] = None  # This is the continuation for the `done` callback of the awaiting poster.

        def check_completion() -> None:
            if result is not None and continuation is not None:
                continuation[0](result)

        def reply_callback(res: _Reply):
            nonlocal result
            result = res
            check_completion()

        reply_channel: AsyncReplyChannel[_Reply] = AsyncReplyChannel(reply_callback)
        self.messages.put(build_message(reply_channel))
        self.__process_events()

        def callback(conts: List[Callable[[Any], None]]) -> None:
            nonlocal continuation
            continuation = conts
            check_completion()

        return from_continuations(callback)

    def receive(self) -> Async[_Msg]:
        """Receive message from mailbox.

        Returns:
            An asynchronous computation which will consume the
            first message in arrival order. No thread is blocked while
            waiting for further messages. Raises a TimeoutException if
            the timeout is exceeded.
        """

        def callback(conts: List[Callable[[Any], None]]):
            if self.continuation:
                raise Exception("Receive can only be called once!")

            self.continuation = conts

            self.__process_events()

        return from_continuations(callback)

    def __process_events(self) -> None:
        if not self.continuation:
            return

        # Cancellation of async workflows is more tricky in Python than
        # with F# so we check the cancellation token for each process.
        if self.token.is_cancellation_requested:
            self.continuation, cont = [], self.continuation
            if cont:
                cont[2](OperationCanceledError("Mailbox was cancelled"))
            return

        with self.lock:
            if self.messages.empty():
                return
            msg = self.messages.get()
            self.continuation, cont = [], self.continuation

        if cont:
            cont[0](msg)

    @classmethod
    def start(
        cls,
        body: Callable[[MailboxProcessor[_Msg]], Async[None]],
        cancellation_token: Optional[CancellationToken] = None,
    ) -> MailboxProcessor[_Msg]:
        mbox: MailboxProcessor[_Msg] = MailboxProcessor(body, cancellation_token)
        start_immediate(body(mbox))
        return mbox


def receive(mbox: MailboxProcessor[_Msg]) -> Async[_Msg]:
    return mbox.receive()


def post(mbox: MailboxProcessor[_Msg], msg: _Msg):
    return mbox.post(msg)


def start_instance(mbox: MailboxProcessor[Any]) -> None:
    body = mbox.body(mbox)
    return start_immediate(body)


def start(
    body: Callable[[MailboxProcessor[_Msg]], Async[None]],
    cancellationToken: Optional[CancellationToken] = None,
) -> MailboxProcessor[_Msg]:
    mbox = MailboxProcessor(body, cancellationToken)
    start_instance(mbox)
    return mbox


__all__ = [
    "AsyncReplyChannel",
    "MailboxProcessor",
    "receive",
    "post",
    "start",
    "start_instance",
]
