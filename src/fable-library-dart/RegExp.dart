RegExp create(String pattern, [int options = 0]) {
  // Supported RegexOptions
  // * IgnoreCase:  0x0001
  // * Multiline:   0x0002
  // * Singleline:  0x0010
  // * ECMAScript:  0x0100 (ignored)
  if ((options & ~(1 ^ 2 ^ 16 ^ 256)) != 0) {
    throw Exception(
        'RegexOptions only supports: IgnoreCase, Multiline, Singleline and ECMAScript');
  }
  return RegExp(pattern,
      multiLine: options & 2 > 0,
      caseSensitive: options & 1 == 0,
      dotAll: options & 16 > 0);
}

bool isMatch(RegExp reg, String input, [int startAt = 0]) {
  return startAt > 0
      ? reg.allMatches(input, startAt).isNotEmpty
      : reg.hasMatch(input);
}

int options(RegExp reg) {
  var options = 256; // ECMAScript
  options |= !reg.isCaseSensitive ? 1 : 0;
  options |= reg.isMultiLine ? 2 : 0;
  options |= reg.isDotAll ? 16 : 0;
  return options;
}

String replace(Pattern reg, String input, String replacement,
    [int? limit, int offset = 0]) {
  // TODO: If replacement uses groups we need to use replaceAllMapped
  // replacement =
  //   replacement
  //   // $0 doesn't work with JS regex, see #1155
  //   .replace(/\$0/g, (_s) => "$&")
  //   // named groups in replacement are `${name}` in .Net, but `$<name>` in JS (in regex: groups are `(?<name>...)` in both)
  //   .replace(/\${([^}]+)}/g, "\$<$1>");

  if (limit != null) {
    throw Exception("Todo: replace with limit");
  } else {
    return input.replaceAll(reg, replacement);
  }
}
