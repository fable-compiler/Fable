import './src/ArithmeticTests.fs.dart' as arithmetic;
import './src/ArrayTests.fs.dart' as array;
import './src/ComparisonTests.fs.dart' as comparison;
import './src/DateTimeTests.fs.dart' as date;
import './src/ListTests.fs.dart' as list;
import './src/MapTests.fs.dart' as map;
import './src/RegexTests.fs.dart' as regex;
import './src/SeqTests.fs.dart' as seq;
import './src/UnionTests.fs.dart' as union;

void main() {
  arithmetic.tests();
  array.tests();
  comparison.tests();
  date.tests();
  list.tests();
  map.tests();
  regex.tests();
  seq.tests();
  union.tests();
}