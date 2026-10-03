"""Controlled scheduler and thread fixtures for the generated F# async tests."""

import asyncio
from collections.abc import Callable
from dataclasses import dataclass, field
from threading import Barrier, Event, Thread

from fable_library.array_ import Array
from fable_library.async_builder import Async, CancellationToken, IAsyncContext, Trampoline
from fable_library.mailbox_processor import MailboxProcessor
from fable_library.util import lock


@dataclass
class Observation:
    _terminal: list[str] = field(default_factory=list)
    _errors: list[str] = field(default_factory=list)
    _before: list[str] = field(default_factory=list)
    _outcome: list[str] = field(default_factory=list)
    listeners: int = 0
    scheduled: int = 0
    cancel_count: int = 0
    retired: bool = False
    owner_loop: bool = False
    threads_finished: bool = False
    pending_before: bool = False
    pending_after: bool = False

    @property
    def terminal(self) -> Array[str]:
        return Array(self._terminal)

    @property
    def errors(self) -> Array[str]:
        return Array(self._errors)

    @property
    def before(self) -> Array[str]:
        return Array(self._before)

    @property
    def outcome(self) -> Array[str]:
        return Array(self._outcome)


class Timer(asyncio.TimerHandle):
    def __init__(self, callback: Callable[[], None]):
        super().__init__(0, callback, (), asyncio.get_running_loop())
        self.callback = callback
        self.retired = False
        self.cancel_count = 0

    def cancel(self):
        self.retired = True
        self.cancel_count += 1

    def fire(self):
        # decision: fires retired callbacks to model a timeout already queued when cancellation wins
        self.callback()


class Scheduler(Trampoline):
    def __init__(self, token: CancellationToken, mode: str = ""):
        super().__init__()
        self.token = token
        self.mode = mode
        self.timers: list[Timer] = []

    def run_later(self, action: Callable[[], None], due_time: float = 0.0):
        if self.mode == "install_error":
            raise ValueError("scheduler")
        timer = Timer(action)
        self.timers.append(timer)
        if self.mode == "install_cancel":
            self.token.cancel()
        return timer


def context(scheduler: Trampoline, token: CancellationToken, result: Observation) -> IAsyncContext[None]:
    def failed(error: Exception):
        result._terminal.append("error")
        result._errors.append(str(error))

    return IAsyncContext.create(
        scheduler,
        token,
        lambda _: result._terminal.append("success"),
        failed,
        lambda _: result._terminal.append("cancel"),
    )


def observe_sleep(work: Async[None], token: CancellationToken, mode: str) -> Observation:
    async def run():
        result = Observation()
        scheduler = Scheduler(token, mode)
        if mode == "precancel":
            token.cancel()
        work(context(scheduler, token, result))
        if scheduler.timers:
            timer = scheduler.timers[0]
            if mode == "cancel":
                token.cancel()
                timer.fire()
            elif mode == "timeout":
                timer.fire()
                token.cancel()
            timer.fire()
            result.retired = timer.retired
            result.cancel_count = timer.cancel_count
        result.scheduled = len(scheduler.timers)
        result.listeners = len(token.listeners)
        return result

    return asyncio.run(run())


def observe_mailbox(work: Async[int], mailbox: MailboxProcessor[int], token: CancellationToken) -> Observation:
    async def run():
        result = Observation()
        ctx = IAsyncContext.create(
            Trampoline(),
            token,
            lambda value: result._terminal.append(str(value)),
            lambda _: result._terminal.append("error"),
            lambda _: result._terminal.append("cancel"),
        )
        work(ctx)
        token.cancel()
        result._before = result._terminal.copy()
        result.pending_before = mailbox.continuation is not None
        mailbox.post(42)
        result.pending_after = mailbox.continuation is not None
        result.listeners = len(token.listeners)
        return result

    return asyncio.run(run())


def observe_owner_loop(work: Async[None], token: CancellationToken) -> Observation:
    async def run():
        result = Observation()
        loop = asyncio.get_running_loop()
        finished = loop.create_future()

        def cancelled(_):
            result.owner_loop = asyncio.get_running_loop() is loop
            result._terminal.append("cancel")
            finished.set_result(None)

        ctx = IAsyncContext.create(
            Trampoline(),
            token,
            lambda _: result._terminal.append("success"),
            lambda _: result._terminal.append("error"),
            cancelled,
        )
        work(ctx)
        thread = Thread(target=token.cancel)
        thread.start()
        try:
            await asyncio.wait_for(finished, 5)
        finally:
            thread.join(5)
        result.threads_finished = not thread.is_alive()
        result.listeners = len(token.listeners)
        return result

    return asyncio.run(run())


def observe_settlement(
    make_work: Callable[[Callable[[], None]], Async[None]],
    token: CancellationToken,
    winner: str,
) -> Observation:
    async def run():
        result = Observation()
        scheduler = Scheduler(token)
        key = object()
        gate = Barrier(4)
        winner_done = Event()
        loop = asyncio.get_running_loop()
        finished = loop.create_future()
        thread_errors: list[Exception] = []

        def choose(tag: str):
            def settle():
                if result._outcome:
                    return
                result._outcome.append(tag)
                if tag == "reply":
                    # invariant: the winning reply retires its deadline before competitors proceed
                    token.cancel()
                winner_done.set()
                loop.call_soon_threadsafe(finished.set_result, None)

            lock(key, settle)

        registration = token.register(lambda: choose("cancel"))
        make_work(lambda: choose("timeout"))(context(scheduler, token, result))
        timer = scheduler.timers[0]

        def compete(tag: str):
            try:
                gate.wait(5)
                if tag != winner and not winner_done.wait(5):
                    raise TimeoutError("settlement winner did not signal")
                if tag == "reply":
                    choose("reply")
                elif tag == "cancel":
                    token.cancel()
                else:
                    loop.call_soon_threadsafe(timer.fire)
            except Exception as error:
                thread_errors.append(error)

        threads = [Thread(target=compete, args=(tag,)) for tag in ("reply", "cancel", "timeout")]
        for thread in threads:
            thread.start()
        gate.wait(5)
        try:
            await asyncio.wait_for(finished, 5)
        finally:
            for thread in threads:
                thread.join(5)
            registration.Dispose()
            token.cancel()
        if thread_errors:
            raise ExceptionGroup("fixture worker failed", thread_errors)
        # Exercise a stale callback after every winner, including cancellation's finalizer.
        timer.fire()
        await asyncio.sleep(0)
        result.threads_finished = all(not thread.is_alive() for thread in threads)
        result.listeners = len(token.listeners)
        result.cancel_count = timer.cancel_count
        result.retired = timer.retired
        return result

    return asyncio.run(run())
