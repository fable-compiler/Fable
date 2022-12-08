// ignore_for_file: file_names

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
  return (comparer(value, min) < 0) ? min : (comparer(value, max) > 0) ? max : value;
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
