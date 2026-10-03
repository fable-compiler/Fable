import assert from "node:assert/strict";
import { CancellationToken, CancellationCallbackError } from "../../../temp/fable-library-js/AsyncBuilder.js";
import { sleep, startWithContinuations } from "../../../temp/fable-library-js/Async.js";
import { singleton } from "../../../temp/fable-library-js/AsyncBuilder.js";
import { MailboxProcessor, receive, post } from "../../../temp/fable-library-js/MailboxProcessor.js";

const token = new CancellationToken();
const calls = [];
const first = token.register(() => calls.push("first"));
const second = token.register(() => { second.Dispose(); first.Dispose(); calls.push("second"); });
token.cancel();
token.register(state => calls.push(state), 0).Dispose();
second.Dispose();
assert.deepEqual(calls, ["second", 0]);
assert.equal(token._listeners.size, 0);

const failing = new CancellationToken();
let remaining = 0;
failing.register(() => remaining++);
failing.register(() => { throw new Error("callback"); });
assert.throws(() => failing.cancel(), CancellationCallbackError);
assert.equal(remaining, 1);
assert.equal(failing._listeners.size, 0);

const originalSetTimeout = globalThis.setTimeout;
const originalClearTimeout = globalThis.clearTimeout;
try {
  for (const cancelFirst of [true, false]) {
    for (const throws of [true, false]) {
      let timeout, retired = 0, finalized = 0;
      globalThis.setTimeout = callback => { timeout = callback; return 123; };
      globalThis.clearTimeout = id => { assert.equal(id, 123); retired++; };
      const source = new CancellationToken();
      const terminal = [];
      const work = singleton.TryFinally(sleep(100), () => {
        finalized++;
        if (throws) { throw new Error("finalizer"); }
      });
      const finish = tag => () => terminal.push(tag);
      startWithContinuations(work, finish("success"), finish("error"), finish("cancel"), source);
      if (cancelFirst) { source.cancel(); timeout(); }
      else {
        timeout();
        source.cancel();
      }
      timeout();
      assert.equal(finalized, 1);
      assert.deepEqual(terminal, cancelFirst ? ["cancel"] : throws ? ["error"] : ["success"]);
      assert.equal(source._listeners.size, 0);
      assert.equal(retired, 1);
    }
  }
} finally {
  globalThis.setTimeout = originalSetTimeout;
  globalThis.clearTimeout = originalClearTimeout;
}

// JS Receive retains its success continuation while idle; token cancellation does not wake it.
const source = new CancellationToken();
const mailbox = new MailboxProcessor(() => singleton.Zero(), source);
const terminal = [];
startWithContinuations(receive(mailbox), x => terminal.push(x), e => { throw e; }, () => terminal.push("cancel"), source);
source.cancel();
assert.deepEqual(terminal, []);
post(mailbox, 42);
assert.deepEqual(terminal, [42]);
assert.equal(source._listeners.size, 0);
console.log("JS cancellation, controlled timers, and idle Receive regressions passed");
