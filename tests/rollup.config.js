import commonjs from 'rollup-plugin-commonjs';
import fable from 'rollup-plugin-fable';

export default {
  input: './Main/Fable.Tests.fsproj',
  output: {
    file: '../../build/tests/bundle.js',
    format: 'cjs',
  },
  plugins: [
    fable({ define: "DOTNETCORE" }),
    commonjs({
      namedExports: { './Main/js/foo.js': ['foo','foo2','apply', 'fooOptional', 'MyClass', 'foo_js'] }
    }),
  ]
  //sourceMap: 'inline',
};
