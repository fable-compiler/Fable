// ignore_for_file: file_names, constant_identifier_names

class StringComparison {
  static const CurrentCulture = 0;
  static const CurrentCultureIgnoreCase = 1;
  static const InvariantCulture = 2;
  static const InvariantCultureIgnoreCase = 3;
  static const Ordinal = 4;
  static const OrdinalIgnoreCase = 5;
}

// Dart doesn't seem to make distinction between cultures or does it implicitly
// Also, apparently Dart comparer lower/uppercase strings differently as how .NET does

int compareWith(String x, String y, int options) {
  final isIgnoreCase = options == StringComparison.CurrentCultureIgnoreCase ||
      options == StringComparison.InvariantCultureIgnoreCase ||
      options == StringComparison.OrdinalIgnoreCase;

  if (isIgnoreCase) {
    return x.toLowerCase().compareTo(y.toLowerCase());
  } else {
    return x.compareTo(y);
  }
}

int compare(String x, String y, [bool? ignoreCase]) {
  return compareWith(
      x,
      y,
      ignoreCase == true
          ? StringComparison.CurrentCultureIgnoreCase
          : StringComparison.CurrentCulture);
}

int compareSubstringsWith(
    String strA, int indexA, String strB, int indexB, int length, int options) {
  final subA = strA.substring(indexA, indexA + length);
  final subB = strB.substring(indexB, indexB + length);
  return compareWith(subA, subB, options);
}

int compareSubstrings(
    String strA, int indexA, String strB, int indexB, int length,
    [bool? ignoreCase]) {
  return compareSubstringsWith(
      strA,
      indexA,
      strB,
      indexB,
      length,
      ignoreCase == true
          ? StringComparison.CurrentCultureIgnoreCase
          : StringComparison.CurrentCulture);
}

int indexOfAny(String str, List<int> anyOf, [int? startIndex, int? length]) {
  if (str == '') {
    return -1;
  }
  startIndex ??= 0;
  if (startIndex < 0) {
    throw Exception('Start index cannot be negative');
  }
  length ??= str.length - startIndex;
  if (length < 0) {
    throw Exception('Length cannot be negative');
  }
  final endIndex = startIndex + length;
  if (endIndex > str.length) {
    throw Exception('Invalid startIndex and length');
  }
  str = str.substring(startIndex, endIndex);
  for (final c in anyOf) {
    final index = str.indexOf(String.fromCharCode(c));
    if (index > -1) {
      return index + startIndex;
    }
  }
  return -1;
}

int getCharAtIndex(String input, int index) {
  if (index < 0 || index >= input.length) {
    throw Exception('Index was outside the bounds of the array.');
  }
  return input.codeUnitAt(index);
}

List<String> splitWithChars(String str,
    [List<int>? splitters, int? count, int? options]) {
  splitters ??= [];
  return split(str, splitters.map((x) => String.fromCharCode(x)).toList(),
      count, options);
}

List<String> splitWithRegExp(String str, RegExp reg,
    [int? count, int? options]) {
  if (count != null && count < 0) {
    throw Exception('Count cannot be less than zero');
  }
  if (count == 0) {
    return [];
  }

  options ??= 0;
  final removeEmpty = (options & 1) == 1;
  final trim = (options & 2) == 2;

  final List<String> splits = [];

  var lastIndex = 0;
  for (var match in reg.allMatches(str).toList()) {
    final candidate = trim
        ? str.substring(lastIndex, match.start).trim()
        : str.substring(lastIndex, match.start);

    if (!removeEmpty || candidate.isNotEmpty) {
      if (count != null && splits.length + 1 == count) {
        splits.add(
            trim ? str.substring(lastIndex).trim() : str.substring(lastIndex));
        break;
      } else {
        splits.add(candidate);
      }
    }
    lastIndex = match.end;
  }

  // Last match
  if (count == null || splits.length < count) {
    final candidate =
        trim ? str.substring(lastIndex).trim() : str.substring(lastIndex);
    if (!removeEmpty || candidate.isNotEmpty) {
      splits.add(candidate);
    }
  }

  return splits;
}

List<String> split(String str,
    [List<String>? splitters, int? count, int? options]) {
  splitters ??= [];
  splitters = splitters
      .where((x) => x.isNotEmpty)
      .map((x) => RegExp.escape(x))
      .toList();
  splitters = splitters.isNotEmpty ? splitters : ['\\s'];

  final reg = RegExp(splitters.join('|'));
  return splitWithRegExp(str, reg, count, options);
}

String patternFromChars(List<int> chars) =>
    RegExp.escape(chars.map((e) => String.fromCharCode(e)).join());

String trim(String str, List<int> chars) {
  if (chars.isEmpty) {
    return str.trim();
  }

  final pattern = '[' + patternFromChars(chars) + ']+';
  return str
      .replaceFirst(RegExp('^' + pattern), '')
      .replaceAll(RegExp(pattern + r'$'), '');
}

String trimStart(String str, List<int> chars) {
  return chars.isEmpty
      ? str.trimLeft()
      : str.replaceAll(RegExp('^[' + patternFromChars(chars) + ']+'), '');
}

String trimEnd(String str, List<int> chars) {
  return chars.isEmpty
      ? str.trimRight()
      : str.replaceAll(RegExp('[' + patternFromChars(chars) + r']+$'), '');
}

bool isNullOrEmpty(String? str) {
  return str == null || str.isEmpty;
}

bool isNullOrWhiteSpace(String? str) {
  return str == null || RegExp(r'^\s*$').hasMatch(str);
}

String insert(String str, int startIndex, String value) {
  if (startIndex < 0 || startIndex > str.length) {
    throw Exception(
        'startIndex is negative or greater than the length of this instance.');
  }
  return str.substring(0, startIndex) + value + str.substring(startIndex);
}

String remove(String str, int startIndex, [int? count]) {
  if (startIndex >= str.length) {
    throw Exception('startIndex must be less than length of string');
  }
  if (count != null) {
    if (startIndex + count > str.length) {
      throw Exception(
          'Index and count must refer to a location within the string.');
    }
    return str.substring(0, startIndex) + str.substring(startIndex + count);
  } else {
    return str.substring(0, startIndex);
  }
}

String initialize(int count, String Function(int) initializer) {
  if (count < 0) {
    throw Exception('String length must be non-negative');
  }
  final xs = <String>[];
  for (var i = 0; i < count; i++) {
    xs.add(initializer(i));
  }
  return xs.join('');
}

String replicate(int count, String str) {
  return initialize(count, (_) => str);
}

String joinWithIndices(
    String delimiter, List<String> xs, int startIndex, int count) {
  final endIndexPlusOne = startIndex + count;
  if (endIndexPlusOne > xs.length) {
    throw Exception(
        'Index and count must refer to a location within the buffer.');
  }
  return xs.sublist(startIndex, endIndexPlusOne).join(delimiter);
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
