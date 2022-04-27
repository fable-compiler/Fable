// ignore_for_file: file_names, non_constant_identifier_names

import 'Util.dart' as util;

// class Unit {
//   Unit._() {}
// }
//
// final unit = new Unit._();

void ignore([dynamic _arg]) {
  // return unit;
}

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
        var x = fields[i];
        var y = other.fields[i];
        if (x is Comparable) {
          j = x.compareTo(y);
        }
        if (j != 0) { return j; }
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

abstract class Record {
}

// class MyRecord implements Record, Comparable<MyRecord> {
//   final String Foo;
//   final int Bar;
//   final double Baz;

//   const MyRecord(this.Foo, this.Bar, this.Baz);

//   @override
//   bool operator ==(Object other) =>
//       other is MyRecord &&
//       other.Foo == Foo &&
//       other.Bar == Bar &&
//       other.Baz == Baz;

//   @override
//   int get hashCode =>
//     util.combineHashCodes([Foo.hashCode, Bar.hashCode, Baz.hashCode]);

//   @override
//   int compareTo(MyRecord other) {
//     int r;
//     if ((r = Foo.compareTo(other.Foo)) == 0) {
//       if ((r = Bar.compareTo(other.Bar)) == 0) {
//         r = Baz.compareTo(other.Baz);
//       }
//     }
//     return r;
//   }
// }
