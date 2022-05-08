// ignore_for_file: file_names

// From https://stackoverflow.com/a/37449594
int combineHashCodes(Iterable<int> hashes) =>
    hashes.isEmpty ? 0 : hashes.reduce((h1, h2) => ((h1 << 5) + h1) ^ h2);

bool equalList<T>(List<T> xs, List<T> ys, [bool Function(T x, T y)? eq]) {
  if (xs.length != ys.length) {
    return false;
  } else {
    if (xs != ys) {
      eq ??= (x, y) => x == y;
      for (var i = 0; i < xs.length; i++) {
        if (!eq(xs[i], ys[i])) {
          return false;
        }
      }
    }
    return true;
  }
}

int compareList<T>(List<T> xs, List<T> ys) {
  if (xs.length != ys.length) {
    return xs.length < ys.length ? -1 : 1;
  }
  for (var i = 0; i < xs.length; i++) {
    final res = compareDynamic(xs[i], ys[i]);
    if (res != 0) {
      return res;
    }
  }
  return 0;
}

int compareBool(bool a, bool b) {
  return a ? (b ? 0 : 1) : -1;
}

int compareDynamic(dynamic a, dynamic b) {
  if (a is Comparable) {
    return a.compareTo(b);
  } else if (a is List) {
    compareList(a, b);
  } else if (a is bool) {
    return compareBool(a, b);
  }
  return a == b ? 0 : -1;
}

String int32ToString(int i, [int radix = 10]) {
  if (radix == 10) {
    return i.toString();
  } else {
    i = i < 0 ? 0xFFFFFFFF + i + 1 : i;
    return i.toRadixString(radix);
  }
}
