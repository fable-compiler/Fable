# Testing

To make it easier to share code across platforms, a [plugin](plugins.md) is available to make
[NUnit](http://www.nunit.org) tests compatible with [Mocha](https://mochajs.org).
This is used for Fable own tests. To compile and run them, just type:

```
build.cmd MochaTest   // on windows    
./build.sh MochaTest  // on unix
```

To debug the generated JS code, run the command below and **attach** an IDE to the node session. If you use Visual Studio Code, you can find detailed instructions [here](https://code.visualstudio.com/docs/editor/debugging).

```
npm run test-debug
```

You can debug the F# source too but, unfortunately, it seems VS Code doesn't like very much the generated source maps, so for this and other node apps you'll have more luck with [node-inspector](https://github.com/node-inspector/node-inspector). In a different terminal window, type the following and use Chrome to browse to the indicated URL.

```
npm install -g node-inspector  // Only first time, to install node-inspector globally

node-inspector
```

After that, run `npm run test-debug` as shown above. If you go to Chrome debugger, you'll see the JS test files but the F# ones won't be loaded yet. Just set a breakpoint at one of the JS files and, when hit, the debugger will automatically search the original code. From that moment on, the F# files will already be displayed so you can set the breakpoints directly there.


> Note: For now only `TestFixture` and `Test` attributes, and `Assert.AreEqual` are available, but more features will be available soon.

> Note: As attributes are only read by name, it's possible to use custom-defined attributes without the `NUnit` dependency if needed.