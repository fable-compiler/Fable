from expression.core import aiotools
from expression.system import OperationCanceledError, CancellationToken

# class Trampoline:
#     maxTrampolineCallCount = 2000

#     def __init__(self) -> None:
#         self.callCount = 0

#     def incrementAndCheck(self):
#         self.callCount += 1
#         return self.callCount > Trampoline.maxTrampolineCallCount

#     def hijack(self, f: Callable[[], None]):
#         self.callCount = 0
#         setTimeout(f, 0)
#         asyncio.e

Continuation = aiotools.Continuation

sleep = aiotools.sleep
start = aiotools.start
runSynchronously = aiotools.run_synchronously

__all__ = ["sleep", "start", "runSynchronously"]
