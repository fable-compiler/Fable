export function startDeferred(computation, token, onSuccess, onError, onCancel) {
    let body;
    computation({
        onSuccess,
        onError,
        onCancel,
        cancelToken: token,
        trampoline: { completed: false, incrementAndCheck: () => true, hijack: f => { body = f; } },
    });
    return () => body();
}

export function handlerCount(event) {
    return event.delegates.length;
}
