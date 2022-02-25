class Union {
  int $tag;
  List<Object> $fields;
  Union(this.$tag, this.$fields);
}

class MyUnion extends Union {
  MyUnion(tag, fields) : super(tag, fields);
}