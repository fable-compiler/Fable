// ignore_for_file: file_names

import 'Types.dart' as types;
import 'Util.dart' as util;

abstract class FsList<T> extends Iterable<T> implements Comparable<FsList<T>> {
  T get head;
  FsList<T> get tail;
  bool get isNil;

  @override
  Iterator<T> get iterator {
    FsList<T> current = this;
    return types.CustomIterator(() {
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
    if (other is FsList<T>) {
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
  int compareTo(FsList<T> other) {
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

class Cons<T> extends FsList<T> {
  @override
  T head;
  @override
  FsList<T> tail;
  @override
  bool get isNil => false;

  Cons(this.head, this.tail);
}

class Nil<T> extends FsList<T> {
  @override
  T get head => throw Exception('Empty list');
  @override
  FsList<T> get tail => throw Exception('Empty list');
  @override
  bool get isNil => true;
}

FsList<T> empty<T>() => Nil<T>();

FsList<T> singleton<T>(T x) => Cons(x, Nil<T>());

FsList<T> ofArrayWithTail<T>(List<T> xs, FsList<T> tail) {
  var li = tail;
  for (var i = xs.length - 1; i >= 0; i--) {
    li = Cons(xs[i], li);
  }
  return li;
}

FsList<T> ofArray<T>(List<T> xs) => ofArrayWithTail(xs, Nil<T>());

FsList<T> ofSeq<T>(Iterable<T> xs) {
  var li = empty<T>();
  for (final x in xs) {
    li = Cons(x, li);
  }
  return li;
}
