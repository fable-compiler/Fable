// ignore_for_file: file_names

final _curried = Expando();

TResult Function(T1, T2) uncurry2<T1, T2, TResult>(
    TResult Function(T2) Function(T1) f) {
  f2(T1 a1, T2 a2) => f(a1)(a2);
  _curried[f2] = f;
  return f2;
}

TResult Function(T2) Function(T1) curry2<T1, T2, TResult>(
    TResult Function(T1, T2) f) {
  var c = _curried[f];
  if (c == null) {
    return (T1 a1) => (T2 a2) => f(a1, a2);
  } else {
    return c as TResult Function(T2) Function(T1);
  }
}

TResult Function(T1, T2, T3) uncurry3<T1, T2, T3, TResult>(
    TResult Function(T3) Function(T2) Function(T1) f) {
  f2(T1 a1, T2 a2, T3 a3) => f(a1)(a2)(a3);
  _curried[f2] = f;
  return f2;
}

TResult Function(T3) Function(T2) Function(T1) curry3<T1, T2, T3, TResult>(
    TResult Function(T1, T2, T3) f) {
  var c = _curried[f];
  if (c == null) {
    return (T1 a1) => (T2 a2) => (T3 a3) => f(a1, a2, a3);
  } else {
    return c as TResult Function(T3) Function(T2) Function(T1);
  }
}

TResult Function(T1, T2, T3, T4) uncurry4<T1, T2, T3, T4, TResult>(
    TResult Function(T4) Function(T3) Function(T2) Function(T1) f) {
  f2(T1 a1, T2 a2, T3 a3, T4 a4) => f(a1)(a2)(a3)(a4);
  _curried[f2] = f;
  return f2;
}

TResult Function(T4) Function(T3) Function(T2) Function(T1)
    curry4<T1, T2, T3, T4, TResult>(TResult Function(T1, T2, T3, T4) f) {
  var c = _curried[f];
  if (c == null) {
    return (T1 a1) => (T2 a2) => (T3 a3) => (T4 a4) => f(a1, a2, a3, a4);
  } else {
    return c as TResult Function(T4) Function(T3) Function(T2) Function(T1);
  }
}

TResult Function(T1, T2, T3, T4, T5) uncurry5<T1, T2, T3, T4, T5, TResult>(
    TResult Function(T5) Function(T4) Function(T3) Function(T2) Function(T1)
        f) {
  f2(T1 a1, T2 a2, T3 a3, T4 a4, T5 a5) => f(a1)(a2)(a3)(a4)(a5);
  _curried[f2] = f;
  return f2;
}

TResult Function(T5) Function(T4) Function(T3) Function(T2) Function(T1)
    curry5<T1, T2, T3, T4, T5, TResult>(
        TResult Function(T1, T2, T3, T4, T5) f) {
  var c = _curried[f];
  if (c == null) {
    return (T1 a1) =>
        (T2 a2) => (T3 a3) => (T4 a4) => (T5 a5) => f(a1, a2, a3, a4, a5);
  } else {
    return c as TResult Function(T5) Function(T4) Function(T3) Function(T2)
        Function(T1);
  }
}

TResult Function(T1, T2, T3, T4, T5, T6)
    uncurry6<T1, T2, T3, T4, T5, T6, TResult>(
        TResult Function(T6) Function(T5) Function(T4) Function(T3) Function(T2)
                Function(T1)
            f) {
  f2(T1 a1, T2 a2, T3 a3, T4 a4, T5 a5, T6 a6) => f(a1)(a2)(a3)(a4)(a5)(a6);
  _curried[f2] = f;
  return f2;
}

TResult Function(T6) Function(T5) Function(T4) Function(T3) Function(T2)
        Function(T1)
    curry6<T1, T2, T3, T4, T5, T6, TResult>(
        TResult Function(T1, T2, T3, T4, T5, T6) f) {
  var c = _curried[f];
  if (c == null) {
    return (T1 a1) => (T2 a2) =>
        (T3 a3) => (T4 a4) => (T5 a5) => (T6 a6) => f(a1, a2, a3, a4, a5, a6);
  } else {
    return c as TResult Function(T6) Function(T5) Function(T4) Function(T3)
            Function(T2)
        Function(T1);
  }
}

TResult Function(T1, T2, T3, T4, T5, T6, T7)
    uncurry7<T1, T2, T3, T4, T5, T6, T7, TResult>(
        TResult Function(T7) Function(T6) Function(T5) Function(T4) Function(T3)
                    Function(T2)
                Function(T1)
            f) {
  f2(T1 a1, T2 a2, T3 a3, T4 a4, T5 a5, T6 a6, T7 a7) =>
      f(a1)(a2)(a3)(a4)(a5)(a6)(a7);
  _curried[f2] = f;
  return f2;
}

TResult Function(T7) Function(T6) Function(T5) Function(T4) Function(T3)
            Function(T2)
        Function(T1)
    curry7<T1, T2, T3, T4, T5, T6, T7, TResult>(
        TResult Function(T1, T2, T3, T4, T5, T6, T7) f) {
  var c = _curried[f];
  if (c == null) {
    return (T1 a1) => (T2 a2) => (T3 a3) => (T4 a4) =>
        (T5 a5) => (T6 a6) => (T7 a7) => f(a1, a2, a3, a4, a5, a6, a7);
  } else {
    return c as TResult Function(T7) Function(T6) Function(T5) Function(T4)
                Function(T3)
            Function(T2)
        Function(T1);
  }
}

TResult Function(T1, T2, T3, T4, T5, T6, T7, T8)
    uncurry8<T1, T2, T3, T4, T5, T6, T7, T8, TResult>(
        TResult Function(T8) Function(T7) Function(T6) Function(T5) Function(T4)
                        Function(T3)
                    Function(T2)
                Function(T1)
            f) {
  f2(T1 a1, T2 a2, T3 a3, T4 a4, T5 a5, T6 a6, T7 a7, T8 a8) =>
      f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8);
  _curried[f2] = f;
  return f2;
}

TResult Function(T8) Function(T7) Function(T6) Function(T5) Function(T4)
                Function(T3)
            Function(T2)
        Function(T1)
    curry8<T1, T2, T3, T4, T5, T6, T7, T8, TResult>(
        TResult Function(T1, T2, T3, T4, T5, T6, T7, T8) f) {
  var c = _curried[f];
  if (c == null) {
    return (T1 a1) => (T2 a2) => (T3 a3) => (T4 a4) => (T5 a5) =>
        (T6 a6) => (T7 a7) => (T8 a8) => f(a1, a2, a3, a4, a5, a6, a7, a8);
  } else {
    return c as TResult Function(T8) Function(T7) Function(T6) Function(T5)
                    Function(T4)
                Function(T3)
            Function(T2)
        Function(T1);
  }
}

TResult Function(T1, T2, T3, T4, T5, T6, T7, T8, T9)
    uncurry9<T1, T2, T3, T4, T5, T6, T7, T8, T9, TResult>(
        TResult Function(T9) Function(T8) Function(T7) Function(T6) Function(T5)
                            Function(T4)
                        Function(T3)
                    Function(T2)
                Function(T1)
            f) {
  f2(T1 a1, T2 a2, T3 a3, T4 a4, T5 a5, T6 a6, T7 a7, T8 a8, T9 a9) =>
      f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9);
  _curried[f2] = f;
  return f2;
}

TResult Function(T9) Function(T8) Function(T7) Function(T6) Function(T5)
                    Function(T4)
                Function(T3)
            Function(T2)
        Function(T1)
    curry9<T1, T2, T3, T4, T5, T6, T7, T8, T9, TResult>(
        TResult Function(T1, T2, T3, T4, T5, T6, T7, T8, T9) f) {
  var c = _curried[f];
  if (c == null) {
    return (T1 a1) => (T2 a2) => (T3 a3) => (T4 a4) => (T5 a5) => (T6 a6) =>
        (T7 a7) => (T8 a8) => (T9 a9) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9);
  } else {
    return c as TResult Function(T9) Function(T8) Function(T7) Function(T6)
                        Function(T5)
                    Function(T4)
                Function(T3)
            Function(T2)
        Function(T1);
  }
}

TResult Function(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)
    uncurry10<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, TResult>(
        TResult Function(T10) Function(T9) Function(T8) Function(T7) Function(
                                    T6)
                                Function(T5)
                            Function(T4)
                        Function(T3)
                    Function(T2)
                Function(T1)
            f) {
  f2(T1 a1, T2 a2, T3 a3, T4 a4, T5 a5, T6 a6, T7 a7, T8 a8, T9 a9, T10 a10) =>
      f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10);
  _curried[f2] = f;
  return f2;
}

TResult Function(T10) Function(T9) Function(T8) Function(T7) Function(T6)
                        Function(T5)
                    Function(T4)
                Function(T3)
            Function(T2)
        Function(T1)
    curry10<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, TResult>(
        TResult Function(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) f) {
  var c = _curried[f];
  if (c == null) {
    return (T1 a1) => (T2 a2) => (T3 a3) => (T4 a4) => (T5 a5) => (T6 a6) =>
        (T7 a7) => (T8 a8) =>
            (T9 a9) => (T10 a10) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
  } else {
    return c as TResult Function(T10) Function(T9) Function(T8) Function(T7)
                            Function(T6)
                        Function(T5)
                    Function(T4)
                Function(T3)
            Function(T2)
        Function(T1);
  }
}

TResult Function(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)
    uncurry11<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, TResult>(
        TResult Function(T11) Function(T10) Function(T9) Function(T8) Function(
                                        T7)
                                    Function(T6)
                                Function(T5)
                            Function(T4)
                        Function(T3)
                    Function(T2)
                Function(T1)
            f) {
  f2(T1 a1, T2 a2, T3 a3, T4 a4, T5 a5, T6 a6, T7 a7, T8 a8, T9 a9, T10 a10,
          T11 a11) =>
      f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11);
  _curried[f2] = f;
  return f2;
}

TResult Function(T11) Function(T10) Function(T9) Function(T8) Function(T7)
                            Function(T6)
                        Function(T5)
                    Function(T4)
                Function(T3)
            Function(T2)
        Function(T1)
    curry11<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, TResult>(
        TResult Function(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) f) {
  var c = _curried[f];
  if (c == null) {
    return (T1 a1) => (T2 a2) => (T3 a3) => (T4 a4) => (T5 a5) => (T6 a6) =>
        (T7 a7) => (T8 a8) => (T9 a9) => (T10 a10) =>
            (T11 a11) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11);
  } else {
    return c as TResult Function(T11) Function(T10) Function(T9) Function(T8)
                                Function(T7)
                            Function(T6)
                        Function(T5)
                    Function(T4)
                Function(T3)
            Function(T2)
        Function(T1);
  }
}

TResult Function(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)
    uncurry12<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, TResult>(
        TResult Function(T12) Function(T11) Function(T10) Function(T9) Function(
                                            T8)
                                        Function(T7)
                                    Function(T6)
                                Function(T5)
                            Function(T4)
                        Function(T3)
                    Function(T2)
                Function(T1)
            f) {
  f2(T1 a1, T2 a2, T3 a3, T4 a4, T5 a5, T6 a6, T7 a7, T8 a8, T9 a9, T10 a10,
          T11 a11, T12 a12) =>
      f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12);
  _curried[f2] = f;
  return f2;
}

TResult Function(T12) Function(T11) Function(T10) Function(T9) Function(T8)
                                Function(T7)
                            Function(T6)
                        Function(T5)
                    Function(T4)
                Function(T3)
            Function(T2)
        Function(T1)
    curry12<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, TResult>(
        TResult Function(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) f) {
  var c = _curried[f];
  if (c == null) {
    return (T1 a1) => (T2 a2) => (T3 a3) => (T4 a4) => (T5 a5) => (T6 a6) =>
        (T7 a7) => (T8 a8) => (T9 a9) => (T10 a10) => (T11 a11) =>
            (T12 a12) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12);
  } else {
    return c as TResult Function(T12) Function(T11) Function(T10) Function(T9)
                                    Function(T8)
                                Function(T7)
                            Function(T6)
                        Function(T5)
                    Function(T4)
                Function(T3)
            Function(T2)
        Function(T1);
  }
}

TResult Function(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)
    uncurry13<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, TResult>(
        TResult Function(T13) Function(T12) Function(T11) Function(T10)
                                                Function(T9)
                                            Function(T8)
                                        Function(T7)
                                    Function(T6)
                                Function(T5)
                            Function(T4)
                        Function(T3)
                    Function(T2)
                Function(T1)
            f) {
  f2(T1 a1, T2 a2, T3 a3, T4 a4, T5 a5, T6 a6, T7 a7, T8 a8, T9 a9, T10 a10,
          T11 a11, T12 a12, T13 a13) =>
      f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13);
  _curried[f2] = f;
  return f2;
}

TResult Function(T13) Function(T12) Function(T11) Function(T10) Function(T9)
                                    Function(T8)
                                Function(T7)
                            Function(T6)
                        Function(T5)
                    Function(T4)
                Function(T3)
            Function(T2)
        Function(T1)
    curry13<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, TResult>(
        TResult Function(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)
            f) {
  var c = _curried[f];
  if (c == null) {
    return (T1 a1) => (T2 a2) => (T3 a3) => (T4 a4) => (T5 a5) => (T6 a6) =>
        (T7 a7) => (T8 a8) => (T9 a9) => (T10 a10) => (T11 a11) => (T12 a12) =>
            (T13 a13) =>
                f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13);
  } else {
    return c as TResult Function(T13) Function(T12) Function(T11) Function(T10)
                                        Function(T9)
                                    Function(T8)
                                Function(T7)
                            Function(T6)
                        Function(T5)
                    Function(T4)
                Function(T3)
            Function(T2)
        Function(T1);
  }
}

TResult Function(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)
    uncurry14<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14,
            TResult>(
        TResult Function(T14) Function(T13) Function(T12) Function(T11)
                                                    Function(T10)
                                                Function(T9)
                                            Function(T8)
                                        Function(T7)
                                    Function(T6)
                                Function(T5)
                            Function(T4)
                        Function(T3)
                    Function(T2)
                Function(T1)
            f) {
  f2(T1 a1, T2 a2, T3 a3, T4 a4, T5 a5, T6 a6, T7 a7, T8 a8, T9 a9, T10 a10,
          T11 a11, T12 a12, T13 a13, T14 a14) =>
      f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14);
  _curried[f2] = f;
  return f2;
}

TResult Function(T14) Function(T13) Function(T12) Function(T11) Function(T10)
                                    Function(T9)
                                Function(T8)
                            Function(T7)
                        Function(T6)
                    Function(T5)
                Function(T4)
            Function(T3)
        Function(T2)
    Function(T1) curry14<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13,
        T14, TResult>(
    TResult Function(
            T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)
        f) {
  var c = _curried[f];
  if (c == null) {
    return (T1 a1) => (T2 a2) => (T3 a3) => (T4 a4) => (T5 a5) => (T6 a6) =>
        (T7 a7) => (T8 a8) => (T9 a9) => (T10 a10) => (T11 a11) => (T12 a12) =>
            (T13 a13) => (T14 a14) =>
                f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14);
  } else {
    return c as TResult Function(T14) Function(T13) Function(T12) Function(T11)
                                            Function(T10)
                                        Function(T9)
                                    Function(T8)
                                Function(T7)
                            Function(T6)
                        Function(T5)
                    Function(T4)
                Function(T3)
            Function(T2)
        Function(T1);
  }
}

TResult Function(
    T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) uncurry15<
        T1,
        T2,
        T3,
        T4,
        T5,
        T6,
        T7,
        T8,
        T9,
        T10,
        T11,
        T12,
        T13,
        T14,
        T15,
        TResult>(
    TResult Function(T15) Function(T14) Function(T13) Function(T12) Function(
                                                    T11)
                                                Function(T10)
                                            Function(T9)
                                        Function(T8)
                                    Function(T7)
                                Function(T6)
                            Function(T5)
                        Function(T4)
                    Function(T3)
                Function(T2)
            Function(T1)
        f) {
  f2(T1 a1, T2 a2, T3 a3, T4 a4, T5 a5, T6 a6, T7 a7, T8 a8, T9 a9, T10 a10,
          T11 a11, T12 a12, T13 a13, T14 a14, T15 a15) =>
      f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15);
  _curried[f2] = f;
  return f2;
}

TResult Function(T15) Function(T14) Function(T13) Function(T12) Function(T11)
                                        Function(T10)
                                    Function(T9)
                                Function(T8)
                            Function(T7)
                        Function(T6)
                    Function(T5)
                Function(T4)
            Function(T3)
        Function(T2)
    Function(T1) curry15<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13,
        T14, T15, TResult>(
    TResult Function(
            T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)
        f) {
  var c = _curried[f];
  if (c == null) {
    return (T1 a1) => (T2 a2) => (T3 a3) => (T4 a4) => (T5 a5) => (T6 a6) =>
        (T7 a7) => (T8 a8) => (T9 a9) => (T10 a10) => (T11 a11) => (T12 a12) =>
            (T13 a13) => (T14 a14) => (T15 a15) => f(a1, a2, a3, a4, a5, a6, a7,
                a8, a9, a10, a11, a12, a13, a14, a15);
  } else {
    return c as TResult Function(T15) Function(T14) Function(T13) Function(T12)
                                                Function(T11)
                                            Function(T10)
                                        Function(T9)
                                    Function(T8)
                                Function(T7)
                            Function(T6)
                        Function(T5)
                    Function(T4)
                Function(T3)
            Function(T2)
        Function(T1);
  }
}

TResult Function(
        T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)
    uncurry16<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, TResult>(
        TResult Function(T16) Function(T15) Function(T14) Function(T13)
                                                            Function(T12)
                                                        Function(T11)
                                                    Function(T10)
                                                Function(T9)
                                            Function(T8)
                                        Function(T7)
                                    Function(T6)
                                Function(T5)
                            Function(T4)
                        Function(T3)
                    Function(T2)
                Function(T1)
            f) {
  f2(T1 a1, T2 a2, T3 a3, T4 a4, T5 a5, T6 a6, T7 a7, T8 a8, T9 a9, T10 a10,
          T11 a11, T12 a12, T13 a13, T14 a14, T15 a15, T16 a16) =>
      f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)(a16);
  _curried[f2] = f;
  return f2;
}

TResult Function(T16) Function(T15) Function(T14) Function(T13) Function(T12)
                                            Function(T11)
                                        Function(T10)
                                    Function(T9)
                                Function(T8)
                            Function(T7)
                        Function(T6)
                    Function(T5)
                Function(T4)
            Function(T3)
        Function(T2)
    Function(T1) curry16<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13,
        T14, T15, T16, TResult>(
    TResult Function(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)
        f) {
  var c = _curried[f];
  if (c == null) {
    return (T1 a1) => (T2 a2) => (T3 a3) => (T4 a4) => (T5 a5) => (T6 a6) =>
        (T7 a7) => (T8 a8) => (T9 a9) => (T10 a10) => (T11 a11) => (T12 a12) =>
            (T13 a13) => (T14 a14) => (T15 a15) => (T16 a16) => f(a1, a2, a3,
                a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16);
  } else {
    return c as TResult Function(T16) Function(T15) Function(T14) Function(T13)
                                                    Function(T12)
                                                Function(T11)
                                            Function(T10)
                                        Function(T9)
                                    Function(T8)
                                Function(T7)
                            Function(T6)
                        Function(T5)
                    Function(T4)
                Function(T3)
            Function(T2)
        Function(T1);
  }
}

TResult Function(
        T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)
    uncurry17<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, TResult>(
        TResult Function(T17) Function(T16) Function(T15) Function(T14) Function(T13)
                                                            Function(T12)
                                                        Function(T11)
                                                    Function(T10)
                                                Function(T9)
                                            Function(T8)
                                        Function(T7)
                                    Function(T6)
                                Function(T5)
                            Function(T4)
                        Function(T3)
                    Function(T2)
                Function(T1)
            f) {
  f2(T1 a1, T2 a2, T3 a3, T4 a4, T5 a5, T6 a6, T7 a7, T8 a8, T9 a9, T10 a10,
          T11 a11, T12 a12, T13 a13, T14 a14, T15 a15, T16 a16, T17 a17) =>
      f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)(a16)(
          a17);
  _curried[f2] = f;
  return f2;
}

TResult Function(T17) Function(T16) Function(T15) Function(T14) Function(T13)
                                                Function(T12)
                                            Function(T11)
                                        Function(T10)
                                    Function(T9)
                                Function(T8)
                            Function(T7)
                        Function(T6)
                    Function(T5)
                Function(T4)
            Function(T3)
        Function(T2)
    Function(T1) curry17<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13,
        T14, T15, T16, T17, TResult>(
    TResult Function(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) f) {
  var c = _curried[f];
  if (c == null) {
    return (T1 a1) => (T2 a2) => (T3 a3) => (T4 a4) => (T5 a5) => (T6 a6) =>
        (T7 a7) => (T8 a8) => (T9 a9) => (T10 a10) => (T11 a11) => (T12 a12) =>
            (T13 a13) => (T14 a14) => (T15 a15) => (T16 a16) => (T17 a17) => f(
                a1,
                a2,
                a3,
                a4,
                a5,
                a6,
                a7,
                a8,
                a9,
                a10,
                a11,
                a12,
                a13,
                a14,
                a15,
                a16,
                a17);
  } else {
    return c as TResult Function(T17) Function(T16) Function(T15) Function(T14)
                                                        Function(T13)
                                                    Function(T12)
                                                Function(T11)
                                            Function(T10)
                                        Function(T9)
                                    Function(T8)
                                Function(T7)
                            Function(T6)
                        Function(T5)
                    Function(T4)
                Function(T3)
            Function(T2)
        Function(T1);
  }
}

TResult Function(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)
    uncurry18<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, TResult>(
        TResult Function(T18) Function(T17) Function(T16) Function(T15) Function(T14) Function(T13)
                                                            Function(T12)
                                                        Function(T11)
                                                    Function(T10)
                                                Function(T9)
                                            Function(T8)
                                        Function(T7)
                                    Function(T6)
                                Function(T5)
                            Function(T4)
                        Function(T3)
                    Function(T2)
                Function(T1)
            f) {
  f2(
          T1 a1,
          T2 a2,
          T3 a3,
          T4 a4,
          T5 a5,
          T6 a6,
          T7 a7,
          T8 a8,
          T9 a9,
          T10 a10,
          T11 a11,
          T12 a12,
          T13 a13,
          T14 a14,
          T15 a15,
          T16 a16,
          T17 a17,
          T18 a18) =>
      f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)(a16)(
          a17)(a18);
  _curried[f2] = f;
  return f2;
}

TResult Function(T18) Function(T17) Function(T16) Function(T15) Function(T14) Function(T13)
                                                Function(T12)
                                            Function(T11)
                                        Function(T10)
                                    Function(T9)
                                Function(T8)
                            Function(T7)
                        Function(T6)
                    Function(T5)
                Function(T4)
            Function(T3)
        Function(T2)
    Function(T1) curry18<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13,
        T14, T15, T16, T17, T18, TResult>(
    TResult Function(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) f) {
  var c = _curried[f];
  if (c == null) {
    return (T1 a1) => (T2 a2) => (T3 a3) => (T4 a4) => (T5 a5) => (T6 a6) =>
        (T7 a7) => (T8 a8) => (T9 a9) => (T10 a10) => (T11 a11) => (T12 a12) =>
            (T13 a13) => (T14 a14) => (T15 a15) => (T16 a16) => (T17 a17) =>
                (T18 a18) => f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11,
                    a12, a13, a14, a15, a16, a17, a18);
  } else {
    return c as TResult Function(T18) Function(T17) Function(T16) Function(T15)
                                                            Function(T14)
                                                        Function(T13)
                                                    Function(T12)
                                                Function(T11)
                                            Function(T10)
                                        Function(T9)
                                    Function(T8)
                                Function(T7)
                            Function(T6)
                        Function(T5)
                    Function(T4)
                Function(T3)
            Function(T2)
        Function(T1);
  }
}

TResult Function(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)
    uncurry19<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, TResult>(
        TResult Function(T19) Function(T18) Function(T17) Function(T16) Function(T15) Function(T14) Function(T13) Function(T12)
                                                        Function(T11)
                                                    Function(T10)
                                                Function(T9)
                                            Function(T8)
                                        Function(T7)
                                    Function(T6)
                                Function(T5)
                            Function(T4)
                        Function(T3)
                    Function(T2)
                Function(T1)
            f) {
  f2(
          T1 a1,
          T2 a2,
          T3 a3,
          T4 a4,
          T5 a5,
          T6 a6,
          T7 a7,
          T8 a8,
          T9 a9,
          T10 a10,
          T11 a11,
          T12 a12,
          T13 a13,
          T14 a14,
          T15 a15,
          T16 a16,
          T17 a17,
          T18 a18,
          T19 a19) =>
      f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)(a16)(
          a17)(a18)(a19);
  _curried[f2] = f;
  return f2;
}

TResult Function(T19) Function(T18) Function(T17) Function(T16) Function(T15) Function(T14) Function(T13)
                                                Function(T12)
                                            Function(T11)
                                        Function(T10)
                                    Function(T9)
                                Function(T8)
                            Function(T7)
                        Function(T6)
                    Function(T5)
                Function(T4)
            Function(T3)
        Function(T2)
    Function(T1) curry19<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13,
        T14, T15, T16, T17, T18, T19, TResult>(
    TResult Function(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) f) {
  var c = _curried[f];
  if (c == null) {
    return (T1 a1) => (T2 a2) => (T3 a3) => (T4 a4) => (T5 a5) => (T6 a6) => (T7 a7) =>
        (T8 a8) => (T9 a9) => (T10 a10) => (T11 a11) => (T12 a12) => (T13 a13) =>
            (T14 a14) => (T15 a15) => (T16 a16) => (T17 a17) => (T18 a18) => (T19 a19) => f(
                a1,
                a2,
                a3,
                a4,
                a5,
                a6,
                a7,
                a8,
                a9,
                a10,
                a11,
                a12,
                a13,
                a14,
                a15,
                a16,
                a17,
                a18,
                a19);
  } else {
    return c as TResult Function(T19) Function(T18) Function(T17) Function(T16)
                                                                Function(T15)
                                                            Function(T14)
                                                        Function(T13)
                                                    Function(T12)
                                                Function(T11)
                                            Function(T10)
                                        Function(T9)
                                    Function(T8)
                                Function(T7)
                            Function(T6)
                        Function(T5)
                    Function(T4)
                Function(T3)
            Function(T2)
        Function(T1);
  }
}

TResult Function(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)
    uncurry20<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, TResult>(
        TResult Function(T20) Function(T19) Function(T18) Function(T17) Function(T16) Function(T15) Function(T14) Function(T13) Function(T12) Function(T11)
                                                    Function(T10)
                                                Function(T9)
                                            Function(T8)
                                        Function(T7)
                                    Function(T6)
                                Function(T5)
                            Function(T4)
                        Function(T3)
                    Function(T2)
                Function(T1)
            f) {
  f2(
          T1 a1,
          T2 a2,
          T3 a3,
          T4 a4,
          T5 a5,
          T6 a6,
          T7 a7,
          T8 a8,
          T9 a9,
          T10 a10,
          T11 a11,
          T12 a12,
          T13 a13,
          T14 a14,
          T15 a15,
          T16 a16,
          T17 a17,
          T18 a18,
          T19 a19,
          T20 a20) =>
      f(a1)(a2)(a3)(a4)(a5)(a6)(a7)(a8)(a9)(a10)(a11)(a12)(a13)(a14)(a15)(a16)(
          a17)(a18)(a19)(a20);
  _curried[f2] = f;
  return f2;
}

TResult Function(T20) Function(T19) Function(T18) Function(T17) Function(T16)
                                                                Function(T15)
                                                            Function(T14)
                                                        Function(T13)
                                                    Function(T12)
                                                Function(T11)
                                            Function(T10)
                                        Function(T9)
                                    Function(T8)
                                Function(T7)
                            Function(T6)
                        Function(T5)
                    Function(T4)
                Function(T3)
            Function(T2)
        Function(T1)
    curry20<T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, TResult>(
        TResult Function(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) f) {
  var c = _curried[f];
  if (c == null) {
    return (T1 a1) => (T2 a2) => (T3 a3) => (T4 a4) => (T5 a5) => (T6 a6) =>
        (T7 a7) => (T8 a8) => (T9 a9) => (T10 a10) => (T11 a11) => (T12 a12) =>
            (T13 a13) => (T14 a14) => (T15 a15) => (T16 a16) => (T17 a17) =>
                (T18 a18) => (T19 a19) => (T20 a20) => f(
                    a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20);
  } else {
    return c as TResult Function(T20) Function(T19) Function(T18) Function(T17)
                                                                    Function(T16)
                                                                Function(T15)
                                                            Function(T14)
                                                        Function(T13)
                                                    Function(T12)
                                                Function(T11)
                                            Function(T10)
                                        Function(T9)
                                    Function(T8)
                                Function(T7)
                            Function(T6)
                        Function(T5)
                    Function(T4)
                Function(T3)
            Function(T2)
        Function(T1);
  }
}

void ignore([dynamic _]) {}

T value<T>(T? value) => value!;

// From https://stackoverflow.com/a/37449594
int combineHashCodes(Iterable<int> hashes) =>
    hashes.isEmpty ? 0 : hashes.reduce((h1, h2) => ((h1 << 5) + h1) ^ h2);

bool equalsList<T>(List<T> xs, List<T> ys, [bool Function(T x, T y)? eq]) {
  if (xs.length != ys.length) {
    return false;
  } else {
    if (xs != ys) {
      eq ??= (x, y) => equalsDynamic(x, y);
      for (var i = 0; i < xs.length; i++) {
        if (!eq(xs[i], ys[i])) {
          return false;
        }
      }
    }
    return true;
  }
}

bool equalsDynamic(dynamic x, dynamic y) {
  if (x is List) {
    return equalsList(x, y);
  } else {
    return x == y;
  }
}

int compareList<T>(List<T> xs, List<T> ys, [int Function(T x, T y)? cmp]) {
  if (xs.length != ys.length) {
    return xs.length < ys.length ? -1 : 1;
  }
  cmp ??= (x, y) => compareDynamic(x, y);
  for (var i = 0; i < xs.length; i++) {
    final res = cmp(xs[i], ys[i]);
    if (res != 0) {
      return res;
    }
  }
  return 0;
}

int compareBool(bool a, bool b) {
  return a ? (b ? 0 : 1) : -1;
}

int compareNullable<T>(T? x, T? y, int Function(T, T) comparer) {
  if (x == null) {
    return y == null ? 0 : -1;
  } else {
    return y == null ? 1 : comparer(x, y);
  }
}

int compareDynamic(dynamic a, dynamic b) {
  if (a is Comparable) {
    return a.compareTo(b);
  } else if (a is List) {
    compareList(a, b);
  } else if (a is bool) {
    return compareBool(a, b);
  }
  // Use compareNullable here?
  return a == b ? 0 : -1;
}

T min<T>(int Function(T, T) comparer, T x, T y) {
  return comparer(x, y) < 0 ? x : y;
}

T max<T>(int Function(T, T) comparer, T x, T y) {
  return comparer(x, y) > 0 ? x : y;
}

T clamp<T>(int Function(T, T) comparer, T value, T min, T max) {
  return (comparer(value, min) < 0)
      ? min
      : (comparer(value, max) > 0)
          ? max
          : value;
}

String int16ToString(int i, [int radix = 10]) {
  if (radix == 10) {
    return i.toString();
  } else {
    i = i < 0 ? 0xFFFF + i + 1 : i;
    return i.toRadixString(radix);
  }
}

String int32ToString(int i, [int radix = 10]) {
  if (radix == 10) {
    return i.toString();
  } else {
    i = i < 0 ? 0xFFFFFFFF + i + 1 : i;
    return i.toRadixString(radix);
  }
}

String padWithZeros(int i, int length) {
  var str = i.toRadixString(10);
  while (str.length < length) {
    str = "0" + str;
  }
  return str;
}

String padLeftAndRightWithZeros(int i, int lengthLeft, int lengthRight) {
  var str = i.toRadixString(10);
  while (str.length < lengthLeft) {
    str = "0" + str;
  }
  while (str.length < lengthRight) {
    str = str + "0";
  }
  return str;
}
