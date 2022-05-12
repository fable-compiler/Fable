// ignore_for_file: file_names, constant_identifier_names

bool isNullOrWhiteSpace(String str) {
  return RegExp(r"^\s*$").hasMatch(str);
}

String initialize(int count, String Function(int) initializer) {
  if (count < 0) {
    throw new Exception("String length must be non-negative");
  }
  final xs = <String>[];
  for (var i = 0; i < count; i++) {
    xs.add(initializer(i));
  }
  return xs.join("");
}

String replicate(int count, String str) {
  return initialize(count, (_) => str);
}

String filter(bool Function(int) predicate, String str) {
  return String.fromCharCodes(str.runes.where(predicate));
}

String collect(String Function(int) mapping, String str) {
  return str.runes.map(mapping).join();
}

String map(int Function(int) mapping, String str) {
  return String.fromCharCodes(str.runes.map(mapping));
}

String mapIndexed(int Function(int, int) mapping, String str) {
  var index = -1;
  return String.fromCharCodes(str.runes.map((int rune) {
    index++;
    return mapping(index, rune);
  }));
}