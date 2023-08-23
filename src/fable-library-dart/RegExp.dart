// ignore_for_file: file_names
import "./String.dart" as string_mod;

class FailMatch implements Match {
  @override
  String? operator [](int group) => null;

  @override
  int get end => 0;

  @override
  String? group(int group) => null;

  @override
  int get groupCount => 0;

  @override
  List<String?> groups(List<int> groupIndices) => const [];

  @override
  String get input => '';

  @override
  Pattern get pattern => '';

  @override
  int get start => 0;
}

class GroupIterable extends Iterable<String?> {
  final Match match;
  GroupIterable(this.match);

  @override
  get iterator => GroupIterator(match);
}

class GroupIterator implements Iterator<String?> {
  final Match match;
  int _nextIndex = 0;

  GroupIterator(this.match);

  @override
  String? current;

  @override
  bool moveNext() {
    if (_nextIndex > match.groupCount) {
      return false;
    }
    current = match.group(_nextIndex++);
    return true;
  }
}

RegExp create(String pattern, [int options = 0]) {
  // Supported RegexOptions
  // * IgnoreCase:  0x0001
  // * Multiline:   0x0002
  // * Compiled:    0x0008 (ignored)
  // * Singleline:  0x0010
  // * ECMAScript:  0x0100 (ignored)
  if ((options & ~(1 ^ 2 ^ 8 ^ 16 ^ 256)) != 0) {
    throw Exception(
        'RegexOptions only supports: IgnoreCase, Multiline, Compiled, Singleline and ECMAScript');
  }
  return RegExp(pattern,
      unicode: true,
      multiLine: options & 2 > 0,
      caseSensitive: options & 1 == 0,
      dotAll: options & 16 > 0);
}

// From http://stackoverflow.com/questions/3446170/escape-string-for-use-in-javascript-regex

String escape(String str) {
  return RegExp.escape(str);
  // return str.replaceAllMapped(
  //   RegExp(r'[\-\[\]\/\{\}\(\)\*\+\?\.\\\^\$\|]'),
  //   (m) => '\\${m.group(0)}',
  // );
}

String unescape(String str) {
  return str.replaceAllMapped(
    RegExp(r'\\([\-\[\]\/\{\}\(\)\*\+\?\.\\\^\$\|])'),
    (m) => m.group(1)!,
  );
}

bool isMatch(RegExp reg, String input, [int startAt = 0]) {
  return startAt > 0
      ? reg.allMatches(input, startAt).isNotEmpty
      : reg.hasMatch(input);
}

String matchValue(Match value) {
  return value.group(0) ?? '';
}

String groupValue(String? group) {
  return group ?? '';
}

int matchLength(Match match) {
  return match.end - match.start;
}

int groupLength(String? group) {
  return group?.length ?? 0;
}

bool matchSuccess(Match match) {
  return match is! FailMatch;
}

bool groupSuccess(String? group) {
  return group != null;
}

String? matchGroup(Match match, int index) {
  return match.group(index);
}

String? matchNamedGroup(Match match, String index) {
  if (match is RegExpMatch) {
    try {
      return match.namedGroup(index);
    } catch (_) {
      return null;
    }
  } else {
    return null;
  }
}

Match match(RegExp reg, String input, [int? startAt]) {
  if (startAt != null && startAt > 0) {
    // Note that reg.matchAsPrefix behaves as sticky regex in JS
    final iterator = reg.allMatches(input, startAt).iterator;
    return iterator.moveNext() ? iterator.current : FailMatch();
  } else {
    return reg.firstMatch(input) ?? FailMatch();
  }
}

int options(RegExp reg) {
  var options = 256; // ECMAScript
  options |= !reg.isCaseSensitive ? 1 : 0;
  options |= reg.isMultiLine ? 2 : 0;
  options |= reg.isDotAll ? 16 : 0;
  return options;
}

String _replaceWith(
    Pattern pattern, String input, String Function(Match) replace,
    [int? limit]) {
  var count = 0;
  pattern = pattern is String ? create(pattern) : pattern;
  return input.replaceAllMapped(pattern, (match) {
    count++;
    if ((limit != null && count > limit)) {
      return match.group(0)!;
    } else {
      return replace(match);
    }
  });
}

String replaceWith(
        Pattern pattern, String input, String Function(Match) replace,
        [int? limit, int offset = 0]) =>
    offset > 0
        ? input.substring(0, offset) +
            _replaceWith(pattern, input.substring(offset), replace, limit)
        : _replaceWith(pattern, input, replace, limit);

final _replaceMacros = RegExp(r'(?<!\$)\$(\d+|\{\w+\})');

String replace(Pattern pattern, String input, String replace,
    [int? limit, int offset = 0]) {
  if (_replaceMacros.hasMatch(replace)) {
    return replaceWith(pattern, input, (match) {
      return replace.replaceAllMapped(_replaceMacros, (rep) {
        final index = rep.group(1)!;
        if (index.startsWith('{')) {
          final name = index.substring(1, index.length - 1);
          return matchNamedGroup(match, name) ?? rep.group(0)!;
        } else {
          return match.group(int.parse(index)) ?? rep.group(0)!;
        }
      });
    }, limit, offset);
  } else if (limit == null) {
    pattern = pattern is String ? create(pattern) : pattern;
    return offset > 0
        ? input.substring(0, offset) +
            input.substring(offset).replaceAll(pattern, replace)
        : input.replaceAll(pattern, replace);
  } else {
    return replaceWith(pattern, input, (_) => replace, limit, offset);
  }
}

List<String> splitWithPattern(String input, String pattern, [int? options]) {
  final reg = create(pattern, options ?? 0);
  return input.split(reg);
}

List<String> split(RegExp pattern, String input, [int? limit, int? offset]) {
  input = offset == null ? input : input.substring(offset);
  return limit == null
      ? input.split(pattern)
      : string_mod.splitWithRegExp(input, pattern, limit);
}
