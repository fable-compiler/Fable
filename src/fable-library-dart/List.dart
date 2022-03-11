abstract class List<T> {
  T get head;
  List<T> get tail;
  bool get isEmpty;

  factory List(T head, List<T> tail) {
    return NonEmptyList(head, tail);
  }

  factory List.empty() {
    return EmptyList();
  }
}

class NonEmptyList<T> implements List<T> {
  @override
  T head;

  @override
  List<T> tail;

  @override
  bool get isEmpty {
    return false;
  }

  NonEmptyList(this.head, this.tail);
}

class EmptyList<T> implements List<T> {
  @override
  T get head {
    throw 'Empty list';
  }

  @override
  List<T> get tail {
    throw 'Empty list';
  }

  @override
  bool get isEmpty {
    return true;
  }

  EmptyList();
}