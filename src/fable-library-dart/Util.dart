void ignore([Object? _arg]) {
}

T sequenceExpression<T>(List<Object> _exprs, T returnValue) {
  return returnValue;
}

// From https://stackoverflow.com/a/37449594
int combineHashCodes(List<int> hashes) =>
  hashes.isEmpty ? 0 : hashes.reduce((h1, h2) => ((h1 << 5) + h1) ^ h2);

bool equalList<T>(List<T> xs, List<T> ys) {
  if (xs.length != ys.length) {
    return false;
  } else {
    if (xs != ys) {
      for (var i = 0; i < xs.length; i++) {
        if (xs[i] != ys[i]) {
          return false;
        }
      }
    }
    return true;
  }
}

int compareList<T extends Comparable<T>>(List<T> xs, List<T> ys) {
  if (xs.length != ys.length) {
    return xs.length < ys.length ? -1 : 1;
  }
  for (var i = 0, j = 0; i < xs.length; i++) {
    j = xs[i].compareTo(ys[i]);
    if (j != 0) { return j; }
  }
  return 0;
}
