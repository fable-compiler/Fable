import 'package:test/test.dart';
import '../ArithmeticTests.dart' as Arithmetic;

void main() {
  test('+ operator works', () {
    var actual = Arithmetic.addPlus2(3,4);
    expect(actual, equals(9));
  });
}