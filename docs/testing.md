# Testing

This somehow breaks the _unopinionated_ rule, but as a way to share cross-platform code a feature has been introduced to compile [NUnit](http://www.nunit.org) tests to [Mocha](https://mochajs.org). This is used for Fabel own tests. To compile and run them, type:

```
> build.cmd MochaTest   // on windows    
$ ./build.sh MochaTest  // on unix

node node_modules/mocha/bin/mocha build/test
```

For now only `TestFixture` and `Test` attributes, and `Assert.AreEqual` are available, but more features will be available soon.

> As attributes are only read by name, it's possible to use the attributes without the `NUnit` dependency if needed.