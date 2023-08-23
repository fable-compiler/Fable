// ignore_for_file: file_names, constant_identifier_names

/// Calls ceil if values is negative and floor if values is positive.
int signedRound(double value) {
  return value < 0 ? value.ceil() : value.floor();
}

// double getFractionalPart(double num) =>
//   num.remainder(signedRound(num));

Duration create(double days, double hours, double minutes, double seconds,
    [double milliseconds = 0]) {
  double totalMicroseconds = days * 86400000000.0 +
      hours * 3600000000.0 +
      minutes * 60000000.0 +
      seconds * 1000000.0 +
      milliseconds * 1000.0;
  return Duration(microseconds: totalMicroseconds.toInt());
}

Duration fromDays(double v) =>
    Duration(microseconds: (v * 86400000000.0).toInt());

Duration fromHours(double v) =>
    Duration(microseconds: (v * 3600000000.0).toInt());

Duration fromMinutes(double v) =>
    Duration(microseconds: (v * 60000000.0).toInt());

Duration fromSeconds(double v) =>
    Duration(microseconds: (v * 1000000.0).toInt());

Duration fromMilliseconds(double milliseconds) =>
    Duration(microseconds: (milliseconds * 1000.0).toInt());

Duration fromTicks(int ticks) => Duration(microseconds: ticks ~/ 10);

int days(Duration ts) => ts.inDays;
int hours(Duration ts) => ts.inHours - (ts.inDays * 24);
int minutes(Duration ts) => ts.inMinutes - (ts.inHours * 60);
int seconds(Duration ts) => ts.inSeconds - (ts.inMinutes * 60);
int milliseconds(Duration ts) => ts.inMilliseconds - (ts.inSeconds * 1000);
int ticks(Duration ts) => ts.inMicroseconds * 10;

double totalDays(Duration ts) => ts.inMicroseconds / 86400000000.0;
double totalHours(Duration ts) => ts.inMicroseconds / 3600000000.0;
double totalMinutes(Duration ts) => ts.inMicroseconds / 60000000.0;
double totalSeconds(Duration ts) => ts.inMicroseconds / 1000000.0;
double totalMilliseconds(Duration ts) => ts.inMicroseconds / 1000.0;

Duration negate(Duration ts) => -ts;
Duration add(Duration ts1, Duration ts2) => ts1 + ts2;
Duration subtract(Duration ts1, Duration ts2) => ts1 - ts2;
Duration multiply(Duration ts, double factor) => ts * factor;
Duration divide(Duration ts, double factor) => ts * (1 / factor);

int compare(Duration ts1, Duration ts2) => ts1.compareTo(ts2);

Duration duration(Duration ts) => ts.abs();

// TODO: toString/parse