// ignore_for_file: file_names
import 'dart:math' as dart_math;

Exception _outOfRange(String paramName) =>
  Exception('Specified argument was out of the range of valid values. (Parameter $paramName)');

Exception _nullArg(String paramName) =>
  Exception('Value cannot be null. (Parameter $paramName)');

class Random {
  final dart_math.Random _random;

  Random([int? seed]) : _random = dart_math.Random(seed);

  int Next0() => _random.nextInt(0x7fffffff);

  int Next1(int maxValue) => Next2(0, maxValue);

  int Next2(int minValue, int maxValue) {
    if (maxValue < minValue) {
      throw _outOfRange('minValue');
    }

    if (maxValue == minValue) {
      return minValue;
    }

    final range = maxValue - minValue;
    return _random.nextInt(range) + minValue;
  }

  double NextDouble() => _random.nextDouble();

  void NextBytes(List<int>? buffer) {
    if (buffer == null) {
      throw _nullArg('buffer');
    }

    for (var i = 0; i < buffer.length; i++) {
      buffer[i] = _random.nextInt(256);
    }
  }
}