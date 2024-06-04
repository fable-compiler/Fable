// ignore_for_file: file_names, non_constant_identifier_names

import 'dart:collection';
import 'Util.dart' as util;

T value<T>(Some<T>? option) => option!.value;

Some<T>? toOption<T>(T? value) => value != null ? Some(value) : null;
T? ofOption<T>(Some<T>? value) => value?.value;

T defaultValue<T>(T def, T? value) => value ?? def;
T defaultWith<T>(T Function() fn, T? value) => value ?? fn();

class Some<T> implements Comparable<Some<T>> {
  final T value;
  const Some(this.value);

  @override
  String toString() => 'Some($value)';

  @override
  bool operator ==(Object other) =>
      other is Some<T> &&
      other.runtimeType == runtimeType &&
      util.equalsDynamic(other.value, value);

  @override
  int get hashCode => value.hashCode;

  @override
  int compareTo(Some<T> other) => util.compareDynamic(other.value, value);
}

Map<K, V> mapFromTuples<K, V>(Iterable<Tuple2<K, V>> tuples) {
  return Map.fromEntries(
      tuples.map((tuple) => MapEntry(tuple.item1, tuple.item2)));
}

Tuple2<K, V> mapEntryToTuple<K, V>(MapEntry<K, V> kv) {
  return Tuple2(kv.key, kv.value);
}

Map<K, V> mapWith<K, V>(IEqualityComparer<K> comparer,
    [Map<K, V>? initialValues]) {
  final map = LinkedHashMap<K, V>(
      equals: comparer.Equals, hashCode: comparer.GetHashCode);
  if (initialValues != null) {
    map.addAll(initialValues);
  }
  return map;
}

void assertKey<K, V>(Map<K, V> map, K key) {
  if (!map.containsKey(key)) {
    throw Exception("The given key '$key' was not present in the dictionary.");
  }
}

V getValue<K, V>(Map<K, V> map, K key) {
  assertKey(map, key);
  return map[key]!;
}

V? getValueNullable<K, V>(Map<K, V?> map, K key) {
  assertKey(map, key);
  return map[key];
}

void addKeyValue<K, V>(Map<K, V> map, K key, V value) {
  if (map.containsKey(key)) {
    throw Exception(
        "An item with the same key has already been added. Key: $key");
  }
  map[key] = value;
}

bool tryGetValue<K, V>(Map<K, V> map, K key, FSharpRef<V> defaultValue) {
  if (map.containsKey(key)) {
    defaultValue.contents = map[key]!;
    return true;
  } else {
    return false;
  }
}

bool removeKey<K, V>(Map<K, V> map, K key) {
  if (map.containsKey(key)) {
    map.remove(key);
    return true;
  }
  return false;
}

Set<T> setWith<T>(IEqualityComparer<T> comparer, [Iterable<T>? initialValues]) {
  final set =
      LinkedHashSet<T>(equals: comparer.Equals, hashCode: comparer.GetHashCode);
  if (initialValues != null) {
    set.addAll(initialValues);
  }
  return set;
}

abstract class IDisposable {
  void Dispose();
}

void dispose(dynamic d) {
  if (d is IDisposable) {
    d.Dispose();
  }
}

abstract class IComparer<T> {
  int Compare(T a, T b);
}

class Comparer<T> implements IComparer<T> {
  final int Function(T, T) _comparer;

  Comparer(this._comparer);

  @override
  int Compare(T a, T b) {
    return _comparer(a, b);
  }
}

abstract class IEqualityComparer<T> {
  bool Equals(T a, T b);

  int GetHashCode(T a);
}

class EqualityComparer<T> implements IEqualityComparer<T> {
  final bool Function(T, T) _equals;
  final int Function(T) _getHashCode;

  EqualityComparer(this._equals, this._getHashCode);

  @override
  bool Equals(T a, T b) {
    return _equals(a, b);
  }

  @override
  int GetHashCode(T a) {
    return _getHashCode(a);
  }
}

abstract class IGenericAdder<T> {
  T GetZero();

  T Add(T a, T b);
}

class GenericAdder<T> implements IGenericAdder<T> {
  final T Function() _getZero;
  final T Function(T, T) _add;

  GenericAdder(this._getZero, this._add);

  @override
  T GetZero() {
    return _getZero();
  }

  @override
  T Add(T x, T y) {
    return _add(x, y);
  }
}

abstract class IGenericAverager<T> {
  T GetZero();

  T Add(T a, T b);

  T DivideByInt(T a, int b);
}

class GenericAverager<T> implements IGenericAverager<T> {
  final T Function() _getZero;
  final T Function(T, T) _add;
  final T Function(T, int) _divideByInt;

  GenericAverager(this._getZero, this._add, this._divideByInt);

  @override
  T GetZero() {
    return _getZero();
  }

  @override
  T Add(T x, T y) {
    return _add(x, y);
  }

  @override
  T DivideByInt(T x, int y) {
    return _divideByInt(x, y);
  }
}

abstract class Union {}

abstract class Record {}

class FSharpRef<T> {
  final T Function() _getter;
  final Function(T) _setter;

  T get contents => _getter();
  set contents(T value) => _setter(value);

  FSharpRef(this._getter, this._setter);

  static FSharpRef<T> ofValue<T>(T value) {
    return FSharpRef(() => value, (T newValue) {
      value = newValue;
    });
  }
}

class Tuple1<T1> implements Comparable<Tuple1<T1>> {
  final T1 item1;

  const Tuple1(this.item1);

  @override
  String toString() => '($item1)';

  @override
  bool operator ==(Object other) =>
      other is Tuple1<T1> &&
      other.runtimeType == runtimeType &&
      util.equalsDynamic(other.item1, item1);

  @override
  int get hashCode => item1.hashCode;

  @override
  int compareTo(Tuple1<T1> other) => util.compareDynamic(item1, other.item1);
}

class Tuple2<T1, T2> implements Comparable<Tuple2<T1, T2>> {
  final T1 item1;
  final T2 item2;

  const Tuple2(this.item1, this.item2);

  @override
  String toString() => '($item1, $item2)';

  @override
  bool operator ==(Object other) =>
      other is Tuple2<T1, T2> &&
      other.runtimeType == runtimeType &&
      util.equalsDynamic(other.item1, item1) &&
      util.equalsDynamic(other.item2, item2);

  @override
  int get hashCode => util.combineHashCodes([item1.hashCode, item2.hashCode]);

  @override
  int compareTo(Tuple2<T1, T2> other) {
    int r;
    if ((r = util.compareDynamic(item1, other.item1)) == 0) {
      r = util.compareDynamic(item2, other.item2);
    }
    return r;
  }
}

class Tuple3<T1, T2, T3> implements Comparable<Tuple3<T1, T2, T3>> {
  final T1 item1;
  final T2 item2;
  final T3 item3;

  const Tuple3(this.item1, this.item2, this.item3);

  @override
  String toString() => '($item1, $item2, $item3)';

  @override
  bool operator ==(Object other) =>
      other is Tuple3<T1, T2, T3> &&
      other.runtimeType == runtimeType &&
      util.equalsDynamic(other.item1, item1) &&
      util.equalsDynamic(other.item2, item2) &&
      util.equalsDynamic(other.item3, item3);

  @override
  int get hashCode =>
      util.combineHashCodes([item1.hashCode, item2.hashCode, item3.hashCode]);

  @override
  int compareTo(Tuple3<T1, T2, T3> other) {
    int r;
    if ((r = util.compareDynamic(item1, other.item1)) == 0) {
      if ((r = util.compareDynamic(item2, other.item2)) == 0) {
        r = util.compareDynamic(item3, other.item3);
      }
    }
    return r;
  }
}

class Tuple4<T1, T2, T3, T4> implements Comparable<Tuple4<T1, T2, T3, T4>> {
  final T1 item1;
  final T2 item2;
  final T3 item3;
  final T4 item4;

  const Tuple4(this.item1, this.item2, this.item3, this.item4);

  @override
  String toString() => '($item1, $item2, $item3, $item4)';

  @override
  bool operator ==(Object other) =>
      other is Tuple4<T1, T2, T3, T4> &&
      other.runtimeType == runtimeType &&
      util.equalsDynamic(other.item1, item1) &&
      util.equalsDynamic(other.item2, item2) &&
      util.equalsDynamic(other.item3, item3) &&
      util.equalsDynamic(other.item4, item4);

  @override
  int get hashCode => util.combineHashCodes(
      [item1.hashCode, item2.hashCode, item3.hashCode, item4.hashCode]);

  @override
  int compareTo(Tuple4<T1, T2, T3, T4> other) {
    int r;
    if ((r = util.compareDynamic(item1, other.item1)) == 0) {
      if ((r = util.compareDynamic(item2, other.item2)) == 0) {
        if ((r = util.compareDynamic(item3, other.item3)) == 0) {
          r = util.compareDynamic(item4, other.item4);
        }
      }
    }
    return r;
  }
}

class Tuple5<T1, T2, T3, T4, T5>
    implements Comparable<Tuple5<T1, T2, T3, T4, T5>> {
  final T1 item1;
  final T2 item2;
  final T3 item3;
  final T4 item4;
  final T5 item5;

  const Tuple5(this.item1, this.item2, this.item3, this.item4, this.item5);

  @override
  String toString() => '($item1, $item2, $item3, $item4, $item5)';

  @override
  bool operator ==(Object other) =>
      other is Tuple5<T1, T2, T3, T4, T5> &&
      other.runtimeType == runtimeType &&
      util.equalsDynamic(other.item1, item1) &&
      util.equalsDynamic(other.item2, item2) &&
      util.equalsDynamic(other.item3, item3) &&
      util.equalsDynamic(other.item4, item4) &&
      util.equalsDynamic(other.item5, item5);

  @override
  int get hashCode => util.combineHashCodes([
        item1.hashCode,
        item2.hashCode,
        item3.hashCode,
        item4.hashCode,
        item5.hashCode
      ]);

  @override
  int compareTo(Tuple5<T1, T2, T3, T4, T5> other) {
    int r;
    if ((r = util.compareDynamic(item1, other.item1)) == 0) {
      if ((r = util.compareDynamic(item2, other.item2)) == 0) {
        if ((r = util.compareDynamic(item3, other.item3)) == 0) {
          if ((r = util.compareDynamic(item4, other.item4)) == 0) {
            r = util.compareDynamic(item5, other.item5);
          }
        }
      }
    }
    return r;
  }
}

class Tuple6<T1, T2, T3, T4, T5, T6>
    implements Comparable<Tuple6<T1, T2, T3, T4, T5, T6>> {
  final T1 item1;
  final T2 item2;
  final T3 item3;
  final T4 item4;
  final T5 item5;
  final T6 item6;

  const Tuple6(
      this.item1, this.item2, this.item3, this.item4, this.item5, this.item6);

  @override
  String toString() => '($item1, $item2, $item3, $item4, $item5, $item6)';

  @override
  bool operator ==(Object other) =>
      other is Tuple6<T1, T2, T3, T4, T5, T6> &&
      other.runtimeType == runtimeType &&
      util.equalsDynamic(other.item1, item1) &&
      util.equalsDynamic(other.item2, item2) &&
      util.equalsDynamic(other.item3, item3) &&
      util.equalsDynamic(other.item4, item4) &&
      util.equalsDynamic(other.item5, item5) &&
      util.equalsDynamic(other.item6, item6);

  @override
  int get hashCode => util.combineHashCodes([
        item1.hashCode,
        item2.hashCode,
        item3.hashCode,
        item4.hashCode,
        item5.hashCode,
        item6.hashCode
      ]);

  @override
  int compareTo(Tuple6<T1, T2, T3, T4, T5, T6> other) {
    int r;
    if ((r = util.compareDynamic(item1, other.item1)) == 0) {
      if ((r = util.compareDynamic(item2, other.item2)) == 0) {
        if ((r = util.compareDynamic(item3, other.item3)) == 0) {
          if ((r = util.compareDynamic(item4, other.item4)) == 0) {
            if ((r = util.compareDynamic(item5, other.item5)) == 0) {
              r = util.compareDynamic(item6, other.item6);
            }
          }
        }
      }
    }
    return r;
  }
}

class Tuple7<T1, T2, T3, T4, T5, T6, T7>
    implements Comparable<Tuple7<T1, T2, T3, T4, T5, T6, T7>> {
  final T1 item1;
  final T2 item2;
  final T3 item3;
  final T4 item4;
  final T5 item5;
  final T6 item6;
  final T7 item7;

  const Tuple7(this.item1, this.item2, this.item3, this.item4, this.item5,
      this.item6, this.item7);

  @override
  String toString() =>
      '($item1, $item2, $item3, $item4, $item5, $item6, $item7)';

  @override
  bool operator ==(Object other) =>
      other is Tuple7<T1, T2, T3, T4, T5, T6, T7> &&
      other.runtimeType == runtimeType &&
      util.equalsDynamic(other.item1, item1) &&
      util.equalsDynamic(other.item2, item2) &&
      util.equalsDynamic(other.item3, item3) &&
      util.equalsDynamic(other.item4, item4) &&
      util.equalsDynamic(other.item5, item5) &&
      util.equalsDynamic(other.item6, item6) &&
      util.equalsDynamic(other.item7, item7);

  @override
  int get hashCode => util.combineHashCodes([
        item1.hashCode,
        item2.hashCode,
        item3.hashCode,
        item4.hashCode,
        item5.hashCode,
        item6.hashCode,
        item7.hashCode
      ]);

  @override
  int compareTo(Tuple7<T1, T2, T3, T4, T5, T6, T7> other) {
    int r;
    if ((r = util.compareDynamic(item1, other.item1)) == 0) {
      if ((r = util.compareDynamic(item2, other.item2)) == 0) {
        if ((r = util.compareDynamic(item3, other.item3)) == 0) {
          if ((r = util.compareDynamic(item4, other.item4)) == 0) {
            if ((r = util.compareDynamic(item5, other.item5)) == 0) {
              if ((r = util.compareDynamic(item6, other.item6)) == 0) {
                r = util.compareDynamic(item7, other.item7);
              }
            }
          }
        }
      }
    }
    return r;
  }
}
