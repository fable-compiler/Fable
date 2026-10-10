// Controlled native fixtures; test cases and assertions live in AsyncTests.fs.
export function listenerCount(token) {
    return token._listeners.size;
}

export function cancellationErrorCount(error) {
    if (error.constructor.name !== "CancellationCallbackError" || !Array.isArray(error.errors)) {
        return -1;
    }
    return error.errors.length;
}

export function withTimers(action) {
    const originalSetTimeout = globalThis.setTimeout;
    const originalClearTimeout = globalThis.clearTimeout;
    let callback;
    const timer = {
        retired: 0,
        invalidHandles: 0,
        fire() {
            // decision: fires retired callbacks to model a timeout already queued at cancellation
            callback();
        },
    };
    globalThis.setTimeout = f => { callback = f; return 123; };
    globalThis.clearTimeout = id => {
        if (id !== 123) { timer.invalidHandles++; }
        timer.retired++;
    };
    try {
        return action(timer);
    } finally {
        globalThis.setTimeout = originalSetTimeout;
        globalThis.clearTimeout = originalClearTimeout;
    }
}
