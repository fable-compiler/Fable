import 'dart:async' as dart_async;

import 'Types.dart' as types;

abstract class AsyncReplyChannel<Reply> {
  void reply(Reply value);
}

typedef Continuation<T> = void Function(T value);

class OperationCanceledException extends types.ExceptionBase {
  OperationCanceledException([String? message])
    : super(message ?? 'The operation was canceled');
}

typedef Continuations<T> =
    types.Tuple3<
      Continuation<T>,
      Continuation<dynamic>,
      Continuation<OperationCanceledException>
    >;

class CancellationToken implements types.IDisposable {
  int _id = 0;
  bool _cancelled;
  final Map<int, void Function()> _listeners = {};

  CancellationToken([this._cancelled = false]);

  bool get isCancelled => _cancelled;

  void cancel() {
    if (!_cancelled) {
      _cancelled = true;

      final listeners = _listeners.values.toList(growable: false);

      for (final listener in listeners) {
        listener();
      }
    }
  }

  int addListener(void Function() f) {
    final id = _id;
    _listeners[_id++] = f;
    return id;
  }

  bool removeListener(int id) {
    return _listeners.remove(id) != null;
  }

  types.IDisposable register(dynamic f, [dynamic state]) {
    final id = addListener(state == null ? () => f() : () => f(state));

    return _CancellationRegistration(this, id);
  }

  @override
  void Dispose() {}
}

class _CancellationRegistration implements types.IDisposable {
  final CancellationToken _token;
  final int _id;

  _CancellationRegistration(this._token, this._id);

  @override
  void Dispose() {
    _token.removeListener(_id);
  }
}

class Trampoline {
  static const int maxTrampolineCallCount = 2000;

  int _callCount = 0;

  bool completed = false;

  bool incrementAndCheck() {
    return _callCount++ > maxTrampolineCallCount;
  }

  void hijack(void Function() f) {
    _callCount = 0;
    dart_async.Timer.run(f);
  }
}

class IAsyncContext<T> {
  final Continuation<T> onSuccess;
  final Continuation<dynamic> onError;
  final Continuation<OperationCanceledException> onCancel;

  final CancellationToken cancelToken;
  final Trampoline trampoline;

  const IAsyncContext({
    required this.onSuccess,
    required this.onError,
    required this.onCancel,
    required this.cancelToken,
    required this.trampoline,
  });
}

typedef Async<T> = void Function(IAsyncContext<T> context);

Async<U> _invokeBinder<T, U>(Function binder, T value) {
  if (binder is Async<U> Function()) {
    return binder();
  }

  return (binder as Async<U> Function(T))(value);
}

Async<T> protectedCont<T>(Async<T> f) {
  return (ctx) {
    if (ctx.cancelToken.isCancelled) {
      ctx.onCancel(OperationCanceledException());
    } else if (ctx.trampoline.incrementAndCheck()) {
      ctx.trampoline.hijack(() {
        try {
          f(ctx);
        } catch (error) {
          if (ctx.trampoline.completed) {
            rethrow;
          }

          ctx.onError(error);
        }
      });
    } else {
      try {
        f(ctx);
      } catch (error) {
        if (ctx.trampoline.completed) {
          rethrow;
        }

        ctx.onError(error);
      }
    }
  };
}

Async<U> protectedBind<T, U>(Async<T> computation, Function binder) {
  return protectedCont<U>((ctx) {
    computation(
      IAsyncContext<T>(
        onSuccess: (value) {
          late final Async<U> bound;

          try {
            bound = _invokeBinder<T, U>(binder, value);
          } catch (error) {
            ctx.onError(error);
            return;
          }

          bound(ctx);
        },
        onError: ctx.onError,
        onCancel: ctx.onCancel,
        cancelToken: ctx.cancelToken,
        trampoline: ctx.trampoline,
      ),
    );
  });
}

Async<T> protectedReturn<T>(T value) {
  return protectedCont((ctx) => ctx.onSuccess(value));
}

class AsyncBuilder {
  Async<U> Bind<T, U>(Async<T> computation, Function binder) {
    return protectedBind<T, U>(computation, binder);
  }

  Async<T> Combine<T>(Async<void> computation1, Async<T> computation2) {
    return Bind<void, T>(computation1, () => computation2);
  }

  Async<T> Delay<T>(Async<T> Function() generator) {
    return protectedCont((ctx) => generator()(ctx));
  }

  Async<void> For<T>(Iterable<T> sequence, Function body) {
    final iterator = sequence.iterator;
    var hasCurrent = iterator.moveNext();

    return While(
      () => hasCurrent,
      Delay<void>(() {
        final result = _invokeBinder<T, void>(body, iterator.current);
        hasCurrent = iterator.moveNext();
        return result;
      }),
    );
  }

  Async<T> Return<T>(T value) {
    return protectedReturn(value);
  }

  Async<T> ReturnFrom<T>(Async<T> computation) {
    return computation;
  }

  Async<T> TryFinally<T>(Async<T> computation, void Function() compensation) {
    return protectedCont<T>((ctx) {
      computation(
        IAsyncContext<T>(
          onSuccess: (value) {
            compensation();
            ctx.onSuccess(value);
          },
          onError: (error) {
            compensation();
            ctx.onError(error);
          },
          onCancel: (error) {
            compensation();
            ctx.onCancel(error);
          },
          cancelToken: ctx.cancelToken,
          trampoline: ctx.trampoline,
        ),
      );
    });
  }

  Async<T> TryWith<T>(
    Async<T> computation,
    Async<T> Function(dynamic error) catchHandler,
  ) {
    return protectedCont<T>((ctx) {
      computation(
        IAsyncContext<T>(
          onSuccess: ctx.onSuccess,
          onCancel: ctx.onCancel,
          cancelToken: ctx.cancelToken,
          trampoline: ctx.trampoline,
          onError: (error) {
            late final Async<T> handled;

            try {
              handled = catchHandler(error);
            } catch (handlerError) {
              ctx.onError(handlerError);
              return;
            }

            handled(ctx);
          },
        ),
      );
    });
  }

  Async<U> Using<T extends types.IDisposable, U>(
    T resource,
    Async<U> Function(T value) binder,
  ) {
    return TryFinally(Delay<U>(() => binder(resource)), resource.Dispose);
  }

  Async<void> While(bool Function() guard, Async<void> computation) {
    if (guard()) {
      return Bind<void, void>(computation, () => While(guard, computation));
    } else {
      return Return<void>(null);
    }
  }

  Async<void> Zero() {
    return protectedCont<void>((ctx) => ctx.onSuccess(null));
  }
}

final singleton = AsyncBuilder();
