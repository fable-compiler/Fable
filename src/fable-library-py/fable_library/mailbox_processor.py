from __future__ import annotations

from queue import SimpleQueue
from threading import RLock

from .async_builder import CancellationToken, OperationCanceledError
from .async_ import from_continuations, start_immediate


class AsyncReplyChannel:
    def __init__(self, fn) -> None:
        self.fn = fn

    def reply(self, r) -> None:
        self.fn(r)


class MailboxProcessor:
    def __init__(self, body, cancellation_token=None):
        self.messages = SimpleQueue()
        self.token = cancellation_token or CancellationToken()
        self.lock = RLock()
        self.body = body

        # Holds the continuation i.e the `done` callback of Async.from_continuations returned by `receive`.
        self.continuation = None

    def post(self, msg) -> None:
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

    def post_and_async_reply(self, build_message):
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

        result = None
        continuation = None  # This is the continuation for the `done` callback of the awaiting poster.

        def check_completion() -> None:
            if result is not None and continuation is not None:
                continuation(result)

        def reply_callback(res):
            nonlocal result
            result = res
            check_completion()

        reply_channel = AsyncReplyChannel(reply_callback)
        self.messages.put(build_message(reply_channel))
        self.__process_events()

        def callback(conts):
            nonlocal continuation
            continuation = conts
            check_completion()

        return from_continuations(callback)

    def receive(self):
        """Receive message from mailbox.

        Returns:
            An asynchronous computation which will consume the
            first message in arrival order. No thread is blocked while
            waiting for further messages. Raises a TimeoutException if
            the timeout is exceeded.
        """

        def callback(conts):
            if self.continuation:
                raise Exception("Receive can only be called once!")

            self.continuation = conts

            self.__process_events()

        return from_continuations(callback)

    def __process_events(self):
        # Cancellation of async workflows is more tricky in Python than
        # with F# so we check the cancellation token for each process.
        if self.token.is_cancellation_requested:
            self.continuation, cont = None, self.continuation
            if cont is not None:
                cont[2](OperationCanceledError("Mailbox was cancelled"))
            return

        if self.continuation is None:
            return

        with self.lock:
            if self.messages.empty():
                return
            msg = self.messages.get()
            self.continuation, cont = None, self.continuation

        if cont is not None:
            cont[0](msg)

    @staticmethod
    def start(body, cancellation_token=None):
        mbox = MailboxProcessor(body, cancellation_token)
        start_immediate(body(mbox))
        return mbox


def receive(mbox):
    return mbox.receive()


def post(mbox, msg):
    return mbox.post(msg)


def start_instance(mbox):
    start_immediate(mbox.body(mbox))


def start(body, cancellationToken=None):
    mbox = MailboxProcessor(body, cancellationToken)
    start_instance(mbox)
    return mbox


__all__ = ["AsyncReplyChannel", "MailboxProcessor", "receive", "post", "start", "start_instance"]
