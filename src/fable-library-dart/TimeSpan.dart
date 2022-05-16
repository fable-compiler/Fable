// ignore_for_file: file_names, constant_identifier_names

Duration fromTicks(int ticks) {
  return Duration(microseconds: ticks ~/ 10);
}

Duration fromMilliseconds(double milliseconds) {
  return Duration(microseconds: (milliseconds * 1000).toInt());
}

// double getFractionalPart(double num) =>
//   num.remainder(num < 0 ? num.ceilToDouble() : num.floorToDouble());

Duration create(double days, double hours, double minutes, double seconds, [double milliseconds = 0]) {
  double totalMicroseconds = days * 86400000000 + hours * 3600000000 + minutes * 60000000 + seconds * 1000000 + milliseconds * 1000;
  return Duration(microseconds: totalMicroseconds.toInt());
}

double totalDays(Duration ts) => ts.inDays.toDouble();
double totalHours(Duration ts) => ts.inHours.toDouble();
double totalMinutes(Duration ts) => ts.inMinutes.toDouble();
double totalSeconds(Duration ts) => ts.inSeconds.toDouble();
double totalMilliseconds(Duration ts) => ts.inMilliseconds.toDouble();

Duration negate(Duration ts) => -ts;
Duration add(Duration ts1, Duration ts2) => ts1 + ts2;
Duration subtract(Duration ts1, Duration ts2) => ts1 - ts2;
Duration multiply(Duration ts, double factor) => ts * factor;
Duration divide(Duration ts, double factor) => ts * (1 / factor);