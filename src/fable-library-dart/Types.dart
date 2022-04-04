import 'Util.dart' as util;

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

class EmptyIterator<T> implements Iterator<T> {
  @override
  T get current => throw Exception("Empty iterator");

  @override
  bool moveNext() {
    return false;
  }
}

class CustomIterator<T> implements Iterator<T> {
  T? _current;
  final T? Function() _moveNext;

  @override
  T get current => _current ?? (throw Exception("Iterator not initialized"));

  @override
  bool moveNext() {
    final next = _moveNext();
    if (next != null) {
      _current = next;
      return true;
    } else {
      return false;
    }
  }

  CustomIterator(this._moveNext);
}