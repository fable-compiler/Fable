// ignore_for_file: file_names

import 'Util.dart' as util;

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
  T get current => _current ?? (throw Exception("Iterator has not started"));

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

abstract class FSharpList<T> extends Iterable<T> implements Comparable<FSharpList<T>> {
  T get head;
  FSharpList<T> get tail;
  bool get isNil;

  @override
  String toString() {
    return List.from(this).toString();
  }

  @override
  Iterator<T> get iterator {
    FSharpList<T> current = this;
    return CustomIterator(() {
      if (current.isNil) {
        return null;
      } else {
        var tmp = current;
        current = tmp.tail;
        return tmp.head;
      }
    });
  }

  @override
  bool operator ==(Object other) {
    if (other is FSharpList<T>) {
      var iter1 = iterator;
      var iter2 = other.iterator;
      while (iter1.moveNext()) {
        if (!iter2.moveNext() || iter1.current != iter2.current) {
          return false;
        }
      }
      return !iter2.moveNext();
    }
    return false;
  }

  @override
  int get hashCode => util.combineHashCodes(map((e) => e.hashCode));

  @override
  int compareTo(FSharpList<T> other) {
    var iter1 = iterator;
    var iter2 = other.iterator;
    while (iter1.moveNext()) {
      if (!iter2.moveNext()) {
        return -1;
      } else {
        var res = 0;
        var cur = iter1.current;
        if (cur is Comparable<T>) {
          res = cur.compareTo(iter2.current);
        }
        if (res != 0) {
          return res;
        }
      }
    }
    return iter2.moveNext() ? 1 : 0;
  }
}

class Cons<T> extends FSharpList<T> {
  @override
  T head;
  @override
  FSharpList<T> tail;
  @override
  bool get isNil => false;

  Cons(this.head, this.tail);
}

class Nil<T> extends FSharpList<T> {
  @override
  T get head => throw Exception('Empty list');
  @override
  FSharpList<T> get tail => throw Exception('Empty list');
  @override
  bool get isNil => true;
}

FSharpList<T> empty<T>() => Nil<T>();

FSharpList<T> singleton<T>(T x) => Cons(x, Nil<T>());

FSharpList<T> ofArrayWithTail<T>(List<T> xs, FSharpList<T> tail) {
  var li = tail;
  for (var i = xs.length - 1; i >= 0; i--) {
    li = Cons(xs[i], li);
  }
  return li;
}

FSharpList<T> ofArray<T>(List<T> xs) => ofArrayWithTail(xs, Nil<T>());

FSharpList<T> ofSeq<T>(Iterable<T> xs) {
  var li = empty<T>();
  for (final x in xs) {
    li = Cons(x, li);
  }
  return li;
}
