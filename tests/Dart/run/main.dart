import '../ArithmeticTests.fs.dart' as arithmetic;
import '../ComparisonTests.fs.dart' as comparison;
import '../DateTimeTests.fs.dart' as date;
import '../RegexTests.fs.dart' as regex;
import '../UnionTests.fs.dart' as union;

void main() {
  arithmetic.tests();
  comparison.tests();
  date.tests();
  regex.tests();
  union.tests();
}