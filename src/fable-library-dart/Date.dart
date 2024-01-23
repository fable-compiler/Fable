// ignore_for_file: file_names, constant_identifier_names
import 'dart:math' as math;
import 'TimeSpan.dart' as timespan;
import 'Types.dart' as types;
import 'Util.dart' as util;

// We define Unspecified for .NET compatibility but it's not used in Dart DateTime
class DateTimeKind {
  static const Unspecified = 0;
  static const Utc = 1;
  static const Local = 2;
}

// Dart DateTime limits, from https://stackoverflow.com/a/67148809
// Use .NET values instead? Problem is we cannot set DateTimeKind.Unspecified

DateTime minValue() => DateTime.utc(-271821, 04, 20);
DateTime maxValue() => DateTime.utc(275760, 09, 13);

const _epochMicrosecondsOffset = 62135596800000000;

int unixEpochMicrosecondsToTicks(int ms) {
  return (ms + _epochMicrosecondsOffset) * 10;
}

int ticksToUnixEpochMicroseconds(int ticks) {
  return ticks ~/ 10 - _epochMicrosecondsOffset;
}

void _assertNonUnspecified(int kind) {
  if (kind == DateTimeKind.Unspecified) {
    throw Exception("DateTimeKind.Unspecified is not supported");
  }
}

final _parseFormat = RegExp(
    r'^\+?(?<date>(?<date1>\d+)[\/\-.](?<date2>\d+)(?:[\/\-.](?<date3>\d+))?(?:T|\s+))?(?<time>\d+:\d+(?::\d+(?:\.\d+)?)?)?\s*(?<ampm>[AaPp][Mm])?\s*(?<offset>Z|[+-](?<offsethours>[01]?\d):?(?<offsetminutes>[0-5]?\d)?)?$');

DateTime parse(String input) {
  input = input.trim();

  final m = _parseFormat.firstMatch(input);
  if (m == null) {
    throw Exception("Invalid date format: $input");
  }

  var utc = false;
  int offsetTotalMinutes = 0;
  int year = 0;
  int month = 0;
  int day = 0;
  int hours = 0;
  int minutes = 0;
  double seconds = 0;
  int microSeconds = 0;

  // Assume MDY by default
  final datePart = m.namedGroup("date");
  if (datePart != null) {
    final date3_ = m.namedGroup("date3");
    month = int.parse(m.namedGroup("date1")!);
    day = int.parse(m.namedGroup("date2")!);
    year = date3_ != null ? int.parse(date3_) : DateTime.now().year;
    // YMD
    if (month > 31) {
      final tmp = month;
      month = day;
      day = year;
      year = tmp;
    }
    // .NET Invariant Culture doesn't accept DMY
    // else if (month > 12) {
    //   final tmp = month;
    //   month = day;
    //   day = tmp;
    // }
  } else {
    var d = DateTime.now();
    year = d.year;
    month = d.month;
    day = d.day;
  }

  final time = m.namedGroup("time");
  if (time != null) {
    final timeParts = time.split(":");
    hours = int.parse(timeParts[0]);
    if (timeParts.length > 1) {
      minutes = int.parse(timeParts[1]);
    }
    if (timeParts.length > 2) {
      seconds = double.parse(timeParts[2]);
    }
    final amPm = m.namedGroup("ampm");
    if (amPm != null && amPm.toLowerCase() == 'pm') {
      hours += 12;
    }
  }

  int secondsInt = seconds.floor();
  if (seconds > secondsInt) {
    microSeconds = ((seconds - secondsInt) * 1000000).toInt();
  }

  final offset = m.namedGroup("offset");
  if (offset != null) {
    utc = true;
    if (offset.toUpperCase() != "Z") {
      final offsetMinutes = m.namedGroup("offsetminutes");
      offsetTotalMinutes = int.parse(m.namedGroup("offsethours")!) * 60 +
          (offsetMinutes != null ? int.parse(offsetMinutes) : 0);
      if (offset.startsWith("-")) {
        offsetTotalMinutes = -offsetTotalMinutes;
      }
    }
  }

  final parsedDate = utc
      ? DateTime.utc(
          year, month, day, hours, minutes, secondsInt, 0, microSeconds)
      : DateTime(year, month, day, hours, minutes, secondsInt, 0, microSeconds);

  // DateTime will be UTC in this case so it's safe to add time, see:
  // https://medium.com/pinch-nl/datetime-and-daylight-saving-time-in-dart-9c9468633b5d
  if (offsetTotalMinutes != 0) {
    parsedDate.add(Duration(minutes: offsetTotalMinutes));
  }
  // .NET always parses as local, even if input is UTC
  return parsedDate.isUtc ? parsedDate.toLocal() : parsedDate;
}

bool tryParse(String input, types.FSharpRef<DateTime> defaultValue) {
  try {
    defaultValue.contents = parse(input);
    return true;
  } catch (_) {
    return false;
  }
}

DateTime create(int year, int month, int day,
    [int hour = 0,
    int min = 0,
    int sec = 0,
    int millisec = 0,
    int kind = DateTimeKind.Local]) {
  _assertNonUnspecified(kind);
  return kind == DateTimeKind.Utc
      ? DateTime.utc(year, month, day, hour, min, sec, millisec)
      : DateTime(year, month, day, hour, min, sec, millisec);
}

DateTime specifyKind(DateTime d, int kind) {
  _assertNonUnspecified(kind);
  return kind == DateTimeKind.Utc ? d.toUtc() : d.toLocal();
}

Duration timeOfDay(DateTime d) {
  return Duration(
      hours: d.hour,
      minutes: d.minute,
      seconds: d.second,
      milliseconds: d.millisecond,
      microseconds: d.microsecond);
}

DateTime date(DateTime d) {
  return d.isUtc
      ? DateTime.utc(d.year, d.month, d.day)
      : DateTime(d.year, d.month, d.day);
}

int year(DateTime d) => d.year;
int month(DateTime d) => d.month;
int day(DateTime d) => d.day;
int hour(DateTime d) => d.hour;
int minute(DateTime d) => d.minute;
int millisecond(DateTime d) => d.millisecond;
int dayOfWeek(DateTime d) => d.weekday % 7;

int dayOfYear(DateTime d) {
  final _year = d.year;
  final _month = d.month;
  var _day = d.day;
  for (var i = 1; i < _month; i++) {
    _day += daysInMonth(_year, i);
  }
  return _day;
}

// Don't use Dart Duration because it can cause issues with daylight saving time, see
// https://www.flutterclutter.dev/flutter/troubleshooting/datetime-add-and-subtract-daylight-saving-time/2021/2317/

DateTime addYears(DateTime d, int v) {
  final newMonth = d.month;
  final newYear = d.year + v;
  final _daysInMonth = daysInMonth(newYear, newMonth);
  final newDay = math.min(_daysInMonth, d.day);
  return d.isUtc
      ? DateTime.utc(newYear, newMonth, newDay, d.hour, d.minute, d.second,
          d.millisecond, d.microsecond)
      : DateTime(newYear, newMonth, newDay, d.hour, d.minute, d.second,
          d.millisecond, d.microsecond);
}

DateTime addMonths(DateTime d, int v) {
  var newMonth = d.month + v;
  var newMonth_ = 0;
  var yearOffset = 0;
  if (newMonth > 12) {
    newMonth_ = newMonth.remainder(12);
    // If we use ~/ operator here, it doesn't work with negative numbers
    // See difference between truncate (what ~/ does) and floor
    yearOffset = (newMonth / 12).floor();
    newMonth = newMonth_;
  } else if (newMonth < 1) {
    newMonth_ = 12 + newMonth.remainder(12);
    yearOffset = (newMonth / 12).floor() + (newMonth_ == 12 ? -1 : 0);
    newMonth = newMonth_;
  }
  final newYear = d.year + yearOffset;
  final _daysInMonth = daysInMonth(newYear, newMonth);
  final newDay = math.min(_daysInMonth, d.day);
  return d.isUtc
      ? DateTime.utc(newYear, newMonth, newDay, d.hour, d.minute, d.second,
          d.millisecond, d.microsecond)
      : DateTime(newYear, newMonth, newDay, d.hour, d.minute, d.second,
          d.millisecond, d.microsecond);
}

DateTime add(DateTime d, Duration ts) {
  final newDate = d.add(ts);
  if (!d.isUtc) {
    final oldTzOffset = d.timeZoneOffset;
    final newTzOffset = newDate.timeZoneOffset;
    return oldTzOffset != newTzOffset
        // Maxime Mangel: I am not sure why but the next line needs to have
        // different reversed compared to other languages implementations
        // Without that, I have an error on the test "Adding days to a local date works even if daylight saving time changes"
        // when executing from Europe/Paris timezone
        // If this breaks another timezone, we need to investigate for a more robust detection
        ? newDate.add(oldTzOffset - newTzOffset)
        : newDate;
  } else {
    return newDate;
  }
}

DateTime subtract(DateTime d, Duration ts) {
  return add(d, -ts);
}

Duration subtractDate(DateTime d1, DateTime d2) {
  return Duration(
      microseconds: d1.microsecondsSinceEpoch - d2.microsecondsSinceEpoch);
}

int compare(DateTime dt1, DateTime dt2) {
  return dt1.compareTo(dt2);
}

DateTime addDays(DateTime d, double v) => add(d, timespan.fromDays(v));
DateTime addHours(DateTime d, double v) => add(d, timespan.fromHours(v));
DateTime addMinutes(DateTime d, double v) => add(d, timespan.fromMinutes(v));
DateTime addSeconds(DateTime d, double v) => add(d, timespan.fromSeconds(v));
DateTime addMilliseconds(DateTime d, double v) =>
    add(d, timespan.fromMilliseconds(v));
DateTime addTicks(DateTime d, int v) => add(d, Duration(microseconds: v ~/ 10));

DateTime now() {
  return DateTime.now();
}

DateTime utcNow() {
  return DateTime.now().toUtc();
}

DateTime today() {
  return date(DateTime.now());
}

bool isLeapYear(int year) =>
    year % 4 == 0 && year % 100 != 0 || year % 400 == 0;

int daysInMonth(int year, int month) => month == 2
    ? (isLeapYear(year) ? 29 : 28)
    : (month >= 8 ? (month % 2 == 0 ? 31 : 30) : (month % 2 == 0 ? 30 : 31));

DateTime fromTicks(int ticks, [int kind = DateTimeKind.Local]) {
  _assertNonUnspecified(kind);
  final ms = ticksToUnixEpochMicroseconds(ticks);
  return DateTime.fromMicrosecondsSinceEpoch(ms,
      isUtc: kind == DateTimeKind.Utc);
}

int getTicks(DateTime date) {
  return unixEpochMicrosecondsToTicks(date.microsecondsSinceEpoch);
}

int kind(DateTime dt) => dt.isUtc ? DateTimeKind.Utc : DateTimeKind.Local;

String dateOffsetToString(Duration offset) {
  var offsetMinutes = offset.inMinutes;
  final isMinus = offsetMinutes < 0;
  offsetMinutes = offsetMinutes.abs();
  final hours = offsetMinutes ~/ 3600000;
  final minutes = (offsetMinutes % 3600000) ~/ 60000;
  return (isMinus ? "-" : "+") +
      util.padWithZeros(hours, 2) +
      ":" +
      util.padWithZeros(minutes, 2);
}

String toDateOnlyString(DateTime date) {
  final str = date.toIso8601String();
  return str.substring(0, str.indexOf("T"));
}

String toTimeOnlyString(DateTime date) {
  final str = date.toIso8601String();
  return str.substring(str.indexOf("T") + 1, str.length - (date.isUtc ? 1 : 0));
}

String dateToISOString(DateTime d, [bool? utc]) {
  if (utc == null) {
    utc = d.isUtc;
  } else {
    d = utc ? d.toUtc() : d.toLocal();
  }
  return utc
      ? d.toIso8601String()
      : d.toIso8601String() + dateOffsetToString(d.timeZoneOffset * -60000);
}

String dateToStringWithCustomFormat(DateTime date, String format, [bool? utc]) {
  if (utc != null) {
    date = utc ? date.toUtc() : date.toLocal();
  }
  return format.replaceAllMapped(RegExp(r'(\w)\1*'), (Match match) {
    int? rep;
    final matchValue = match.group(0)!;
    switch (matchValue.substring(0, 1)) {
      case "y":
        final y = date.year;
        rep = matchValue.length < 4 ? y % 100 : y;
        break;
      case "M":
        rep = date.month;
        break;
      case "d":
        rep = date.day;
        break;
      case "H":
        rep = date.hour;
        break;
      case "h":
        final h = date.hour;
        rep = h > 12 ? h % 12 : h;
        break;
      case "m":
        rep = date.minute;
        break;
      case "s":
        rep = date.second;
        break;
      case "f":
        rep = date.millisecond;
        break;
    }
    if (rep == null) {
      return matchValue;
    } else {
      return (rep < 10 && matchValue.length > 1)
          ? "0" + rep.toString()
          : "" + rep.toString();
    }
  });
}

String toString(DateTime date, [String? format]) {
  if (format == null) {
    return date.toString();
  } else if (format.length == 1) {
    switch (format) {
      case "D":
      case "d":
        return toDateOnlyString(date);
      case "T":
      case "t":
        return toTimeOnlyString(date);
      case "O":
      case "o":
        return dateToISOString(date);
      default:
        throw Exception("Unrecognized Date print format");
    }
  } else {
    return dateToStringWithCustomFormat(date, format);
  }
}

// Doesn't seem to work
// bool _isDST(Duration janOffset, Duration julOffset, Duration tOffset) {
//   final min = janOffset > julOffset ? julOffset : janOffset;
//   return min == tOffset;
// }

// bool isDaylightSavingTime(DateTime d) {
//   final year = d.year;
//   final jan = DateTime(year, 0, 1);
//   final jul = DateTime(year, 6, 1);
//   return _isDST(jan.timeZoneOffset, jul.timeZoneOffset, d.timeZoneOffset);
// }
