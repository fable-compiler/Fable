# Fable.Plugins.NUnit

Fable plugin for NUnit tests. Add `Fable.Plugins.NUnit` to your paket.dependencies to download it.

## Usage

Usually you have to pass the path to the plugin from the JS bootstrapper. For example, if you're using [fable-loader](https://www.npmjs.com/package/fable-loader) you can add the following to the options:

```js
{
    test: /\.fs(x|proj)?$/,
    use: {
        loader: "fable-loader",
        options: {
            plugins: path.join(__dirname,"packages/Fable.Plugins.NUnit/lib/netstandard1.6/Fable.Plugins.NUnit.dll"),
            // Add other options here
        }
}
```

Check [the tutorial](http://fable.io/samples/nunit/) for more info.
