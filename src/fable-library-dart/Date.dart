// We define Unspecified for .NET compatibility but it's not used in Dart DateTime
enum DateTimeKind { Unspecified, Utc, Local }

// Dart DateTime limits, from https://stackoverflow.com/a/67148809
// Use .NET values instead? Problem is we cannot set DateTimeKind.Unspecified

DateTime minValue() => DateTime.utc(-271821, 04, 20);
DateTime maxValue() => DateTime.utc(275760, 09, 13);

int unixEpochMillisecondsToTicks(int ms, int offset) {
  return (ms + 62135596800000 + offset) * 10000;
}

int ticksToUnixEpochMilliseconds(int ticks) {
  return ticks ~/ 10000 - 62135596800000;
}

void _checkKind(DateTimeKind kind) {
  if (kind == DateTimeKind.Unspecified) {
    throw Exception("DateTimeKind.Unspecified is not supported");
  }
}

DateTime create(int year, int month, int day,
    [int h = 0, int m = 0, int s = 0, int ms = 0, DateTimeKind kind = DateTimeKind.Local]) {
  _checkKind(kind);
  return kind == DateTimeKind.Utc
      ? DateTime.utc(year, month, day, h, m, s, ms)
      : DateTime(year, month, day, h, m, s, ms);
}

DateTime fromTicks(int ticks, [DateTimeKind kind = DateTimeKind.Local]) {
  _checkKind(kind);
  final ms = ticksToUnixEpochMilliseconds(ticks);
  return DateTime.fromMillisecondsSinceEpoch(ms,
      isUtc: kind == DateTimeKind.Utc);
}

DateTimeKind kind(DateTime dt) => dt.isUtc ? DateTimeKind.Utc : DateTimeKind.Local;
