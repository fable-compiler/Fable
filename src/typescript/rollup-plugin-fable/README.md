# rollup-plugin-fable

Rollup plugin for Fable compiler.

## Installation

```dotnet fable add fable-core@next rollup-plugin-fable```

## Usage

Create a `rollup-config.js` like the following:

```
import fable from 'rollup-plugin-fable';

export default {
  entry: './my-app.fsproj',
  dest: './dist/bundle.js',
  plugins: [
    fable()
  ],
  format: 'cjs'
};
```

Add this to your npm scripts.

```
"scripts": {
  "rollup": "rollup -c rollup-config.js"
}
```

You can then bundle your app by running: `dotnet fable npm-run rollup`.

You can alter the output from the compiler by passing in a babel config. Here is a more advanced example that targets the current node version using [babel-preset-env](https://github.com/babel/babel-preset-env)

```
import fable from 'rollup-plugin-fable';

export default {
  plugins: [
    entry: './my-app.fsproj',
    dest: './dist/bundle.js',
    fable({
      babel: {
        presets: [
          ['env', { 
            targets: { 
              node: 'current' 
            }, 
            modules: false 
          }]
        ],
        plugins: [],
        babelrc: false
      }
    })
  ],
  format: 'cjs'
};

```
