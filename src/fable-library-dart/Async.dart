import 'dart:async' as dart_async;

import 'AsyncBuilder.dart' as async_builder;
import 'Choice.dart' as choice;
import 'Types.dart' as types;

void _emptyContinuation<T>(T _value) {}

async_builder.Async<T> _invokeAsyncFunction<U, T>(Function function, U value) {
  if (function is async_builder.Async<T> Function()) {
    return function();
  }

  return (function as async_builder.Async<T> Function(U))(value);
}

async_builder.Async<T> makeAsync<T>(async_builder.Async<T> body) {
  return body;
}

void invoke<T>(
  async_builder.Async<T> computation,
  async_builder.IAsyncContext<T> ctx,
) {
  computation(ctx);
}

void callThenInvoke<T, U>(
  async_builder.IAsyncContext<T> ctx,
  U result1,
  Function part2,
) {
  _invokeAsyncFunction<U, T>(part2, result1)(ctx);
}

void bind<T, U>(
  async_builder.IAsyncContext<T> ctx,
  async_builder.Async<U> part1,
  Function part2,
) {
  async_builder.protectedBind<U, T>(part1, part2)(ctx);
}

Duration _cancellationDelay(Object? value) {
  if (value is Duration) {
    return value;
  }

  if (value is num) {
    return Duration(milliseconds: value.toInt());
  }

  throw ArgumentError.value(
    value,
    'value',
    'Expected Duration or milliseconds.',
  );
}

async_builder.CancellationToken createCancellationToken([Object? arg]) {
  final token = async_builder.CancellationToken(arg is bool ? arg : false);

  if (arg is Duration || arg is num) {
    dart_async.Timer(_cancellationDelay(arg), token.cancel);
  }

  return token;
}

void cancel(async_builder.CancellationToken token) {
  token.cancel();
}

void cancelAfter(async_builder.CancellationToken token, Object delay) {
  dart_async.Timer(_cancellationDelay(delay), token.cancel);
}

bool isCancellationRequested(async_builder.CancellationToken? token) {
  return token?.isCancelled ?? false;
}

void throwIfCancellationRequested(async_builder.CancellationToken? token) {
  if (token != null && token.isCancelled) {
    throw async_builder.OperationCanceledException();
  }
}

async_builder.Async<async_builder.CancellationToken> cancellationToken() {
  return async_builder.protectedCont((ctx) => ctx.onSuccess(ctx.cancelToken));
}

final defaultCancellationToken = async_builder.CancellationToken();

async_builder.Async<choice.FSharpChoice$2<T, dynamic>> catchAsync<T>(
  async_builder.Async<T> work,
) {
  return async_builder.protectedCont<choice.FSharpChoice$2<T, dynamic>>((ctx) {
    work(
      async_builder.IAsyncContext<T>(
        onSuccess: (value) {
          ctx.onSuccess(choice.Choice_makeChoice1Of2<T, dynamic>(value));
        },
        onError: (error) {
          ctx.onSuccess(choice.Choice_makeChoice2Of2<dynamic, T>(error));
        },
        onCancel: ctx.onCancel,
        cancelToken: ctx.cancelToken,
        trampoline: ctx.trampoline,
      ),
    );
  });
}

async_builder.Async<T> fromContinuations<T>(
  void Function(async_builder.Continuations<T>) function,
) {
  return async_builder.protectedCont<T>((ctx) {
    function(
      types.Tuple3<
        async_builder.Continuation<T>,
        async_builder.Continuation<dynamic>,
        async_builder.Continuation<async_builder.OperationCanceledException>
      >(ctx.onSuccess, ctx.onError, ctx.onCancel),
    );
  });
}

async_builder.Async<void> ignore<T>(async_builder.Async<T> computation) {
  return async_builder.protectedBind<T, void>(
    computation,
    (_) => async_builder.protectedReturn<void>(null),
  );
}

void start<T>(
  async_builder.Async<T> computation, [
  types.Some<async_builder.CancellationToken>? cancellationToken,
]) {
  startWithContinuations<T>(
    computation,
    _emptyContinuation<T>,
    (dynamic error) {
      throw error;
    },
    _emptyContinuation<async_builder.OperationCanceledException>,
    cancellationToken,
  );
}

void startImmediate<T>(
  async_builder.Async<T> computation, [
  types.Some<async_builder.CancellationToken>? cancellationToken,
]) {
  start<T>(computation, cancellationToken);
}

void startWithContinuations<T>(
  async_builder.Async<T> computation,
  Function continuation,
  async_builder.Continuation<dynamic> exceptionContinuation,
  async_builder.Continuation<async_builder.OperationCanceledException>
  cancellationContinuation, [
  types.Some<async_builder.CancellationToken>? cancelToken,
]) {
  final trampoline = async_builder.Trampoline();

  async_builder.Continuation<T> doneSuccess(Function cont) {
    return (value) {
      trampoline.completed = true;

      if (cont is void Function()) {
        cont();
      } else {
        (cont as void Function(T))(value);
      }
    };
  }

  async_builder.Continuation<U> done<U>(async_builder.Continuation<U> cont) {
    return (value) {
      trampoline.completed = true;
      cont(value);
    };
  }

  computation(
    async_builder.IAsyncContext<T>(
      onSuccess: doneSuccess(continuation),
      onError: done<dynamic>(exceptionContinuation),
      onCancel: done<async_builder.OperationCanceledException>(
        cancellationContinuation,
      ),
      cancelToken: cancelToken?.value ?? defaultCancellationToken,
      trampoline: trampoline,
    ),
  );
}

async_builder.Async<void> sleep(Object delay) {
  return async_builder.protectedCont<void>((ctx) {
    late final dart_async.Timer timer;
    int? listenerId;
    var completed = false;

    timer = dart_async.Timer(_cancellationDelay(delay), () {
      if (completed) {
        return;
      }

      completed = true;

      if (listenerId != null) {
        ctx.cancelToken.removeListener(listenerId!);
      }

      ctx.onSuccess(null);
    });

    listenerId = ctx.cancelToken.addListener(() {
      if (completed) {
        return;
      }

      completed = true;
      timer.cancel();
      ctx.onCancel(async_builder.OperationCanceledException());
    });
  });
}

async_builder.Async<T> awaitFuture<T>(dart_async.Future<T> future) {
  return async_builder.protectedCont<T>((ctx) {
    future.then<void>(
      (value) {
        ctx.onSuccess(value);
      },
      onError: (Object error, StackTrace stackTrace) {
        if (error is async_builder.OperationCanceledException) {
          ctx.onCancel(error);
        } else {
          ctx.onError(error);
        }
      },
    );
  });
}

dart_async.Future<T> startAsFuture<T>(
  async_builder.Async<T> computation, [
  types.Some<async_builder.CancellationToken>? cancellationToken,
]) {
  final completer = dart_async.Completer<T>();

  startWithContinuations<T>(
    computation,
    (value) {
      completer.complete(value);
    },
    (dynamic error) {
      completer.completeError(error);
    },
    (error) {
      completer.completeError(error);
    },
    cancellationToken,
  );

  return completer.future;
}
