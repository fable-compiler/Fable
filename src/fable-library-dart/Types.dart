// ignore_for_file: file_names, non_constant_identifier_names

import 'Util.dart' as util;

abstract class IDisposable {
  void Dispose();
}

void dispose(dynamic d) {
  (d as IDisposable).Dispose();
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

abstract class Union {
  final int tag;
  final List<Object> fields;

  int compareTagAndFields(Union other) {
    if (other.tag == tag) {
      // Assume fields have same length
      for (var i = 0, j = 0; i < fields.length; i++) {
        j = util.compareDynamic(fields[i], other.fields[i]);
        if (j != 0) {
          return j;
        }
      }
      return 0;
    } else {
      return tag < other.tag ? -1 : 1;
    }
  }

  const Union(this.tag, this.fields);

  // TODO: implement toString

  @override
  bool operator ==(Object other) =>
      other is Union &&
      other.runtimeType == runtimeType &&
      other.tag == tag &&
      util.equalList(other.fields, fields);

  @override
  int get hashCode =>
      util.combineHashCodes([tag, ...fields.map((e) => e.hashCode)]);
}

// class MyUnion extends Union implements Comparable<MyUnion> {
//   @override
//   int compareTo(MyUnion other) {
//     return super.compareTagAndFields(other);
//   }

//   const MyUnion(tag, fields): super(tag, fields);
// }

abstract class Record {}

class Tuple2<T1, T2> implements Comparable<Tuple2<T1, T2>> {
  final T1 item1;
  final T2 item2;

  Tuple2(this.item1, this.item2);

  @override
  bool operator ==(Object other) =>
      other is Tuple2<T1, T2> &&
      other.runtimeType == runtimeType &&
      other.item1 == item1 &&
      other.item2 == item2;

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

  Tuple3(this.item1, this.item2, this.item3);

  @override
  bool operator ==(Object other) =>
      other is Tuple3<T1, T2, T3> &&
      other.runtimeType == runtimeType &&
      other.item1 == item1 &&
      other.item2 == item2 &&
      other.item3 == item3;

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

  Tuple4(this.item1, this.item2, this.item3, this.item4);

  @override
  bool operator ==(Object other) =>
      other is Tuple4<T1, T2, T3, T4> &&
      other.runtimeType == runtimeType &&
      other.item1 == item1 &&
      other.item2 == item2 &&
      other.item3 == item3 &&
      other.item4 == item4;

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

  Tuple5(this.item1, this.item2, this.item3, this.item4, this.item5);

  @override
  bool operator ==(Object other) =>
      other is Tuple5<T1, T2, T3, T4, T5> &&
      other.runtimeType == runtimeType &&
      other.item1 == item1 &&
      other.item2 == item2 &&
      other.item3 == item3 &&
      other.item4 == item4 &&
      other.item5 == item5;

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

  Tuple6(
      this.item1, this.item2, this.item3, this.item4, this.item5, this.item6);

  @override
  bool operator ==(Object other) =>
      other is Tuple6<T1, T2, T3, T4, T5, T6> &&
      other.runtimeType == runtimeType &&
      other.item1 == item1 &&
      other.item2 == item2 &&
      other.item3 == item3 &&
      other.item4 == item4 &&
      other.item5 == item5 &&
      other.item6 == item6;

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

  Tuple7(this.item1, this.item2, this.item3, this.item4, this.item5, this.item6,
      this.item7);

  @override
  bool operator ==(Object other) =>
      other is Tuple7<T1, T2, T3, T4, T5, T6, T7> &&
      other.runtimeType == runtimeType &&
      other.item1 == item1 &&
      other.item2 == item2 &&
      other.item3 == item3 &&
      other.item4 == item4 &&
      other.item5 == item5 &&
      other.item6 == item6 &&
      other.item7 == item7;

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
