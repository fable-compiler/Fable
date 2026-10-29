---
last_commit_released: ce886964cb4521dc4c01c8da089b97c9d9742814
updaters:
  - package.json:
      file: package.json
  - regex:
      file: ./../Fable.Transforms/Global/Compiler.fs
      pattern: (?<=let JS_LIBRARY_VERSION = ").*(?=")
---

# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 2.11.0 - 2026-10-29

### 🚀 Features

* *(js/ts/python/dart/beam)* Support System.Array.BinarySearch and ResizeArray.BinarySearch (#5172) ([d332e7d4](https://github.com/fable-compiler/Fable/commit/d332e7d42c889f172bb9501f505292abf519752f))

### 🐞 Bug Fixes

* *(all)* Accept RegexOptions.NonBacktracking (#5178) ([a61ec8a1](https://github.com/fable-compiler/Fable/commit/a61ec8a1583830599a7ffb2dfb4d093e99902d4f))
* *(js/ts)* Format DateTimeOffset in its own offset (#5171) ([04ddbd20](https://github.com/fable-compiler/Fable/commit/04ddbd2027e03f4cf7b0b22fe886fdabadf26d93))
* *(js/ts)* Support %o in sprintf and interpolated strings (#5175) ([e13e7b0f](https://github.com/fable-compiler/Fable/commit/e13e7b0fdc16d26793833c77a11d242ccc61109b))
* *(js/ts)* Parse numbers like .NET and raise its exception types (#5200) ([82e606e9](https://github.com/fable-compiler/Fable/commit/82e606e9f8567d9bdad743df1b5de6abbdda89b0))
* *(js/ts)* Honor NumberStyles when parsing floats (#5217) ([82b935f3](https://github.com/fable-compiler/Fable/commit/82b935f3859b36923e5e0eb26771f860eadbe525))
* *(js/ts/python/dart/beam)* Raise typed exceptions from invalidArg and invalidOp ([60f2f5b9](https://github.com/fable-compiler/Fable/commit/60f2f5b9fa0e6a26af0f3b4f3cddc2496f075ba3))
* *(js/ts/python/dart/beam)* Align collection exception types with FSharp.Core (#5153) ([7d7ceb33](https://github.com/fable-compiler/Fable/commit/7d7ceb33afd85a25bf06dc703e9d4a471cbc3292))
* *(js/ts/python/dart/beam)* Add FSharp.Core's argument checks to collection functions (#5154) ([5185e654](https://github.com/fable-compiler/Fable/commit/5185e6547416384fe85fcb0b0c1cf267708879db))

### ⚡ Performance Improvements

* *(js/ts)* Standard library combine numeric hash from snapshot of object instead of mapping (#5151) ([44495104](https://github.com/fable-compiler/Fable/commit/44495104b5c7ac65bfcea184d1305fd29f1db9b4))
* *(js/ts)* Standard library remove .map result array from record hash using snapshot (#5150) ([0ceb5e9e](https://github.com/fable-compiler/Fable/commit/0ceb5e9ec5b470d00295a5cea9ead1c621c7858e))
* *(js/ts)* Standard library one pass List.append builder (#5148) ([2ed9d6dd](https://github.com/fable-compiler/Fable/commit/2ed9d6dd0a372e5bcf53dfe24d786bf9b9256fd3))
* *(js/ts)* List node as a class with a shared empty node (#5185) ([187f8985](https://github.com/fable-compiler/Fable/commit/187f89852413186b52d4ecdde55e1be7cf736785))
* *(js/ts)* Check array bounds only on undefined reads (#5188) ([435fc0bb](https://github.com/fable-compiler/Fable/commit/435fc0bb008ff2ab7c14a08c7218be94f4735029))
* *(js/ts)* Closure-free ref cells and out-arg value cells (#5189) ([91bca925](https://github.com/fable-compiler/Fable/commit/91bca925757356191c7e6e7a71cb3e8a1bc8d52d))
* *(js/ts)* Keep the curried original on its adapter (#5191) ([bfb4a2a4](https://github.com/fable-compiler/Fable/commit/bfb4a2a4e3bb1b5bd4491955a1e1cb90c14050b6))
* *(js/ts)* Walk arrays by index when enumerated as seq (#5192) ([d88621c3](https://github.com/fable-compiler/Fable/commit/d88621c3b9d6ce48a72dc09b86a6805f612ac185))
* *(js/ts)* Hash unions without allocating (#5195) ([2b66a5d2](https://github.com/fable-compiler/Fable/commit/2b66a5d2184f3a35f0d73fb397ab902e300ac4bd))
* *(js/ts)* Probe dictionary buckets without allocating (#5196) ([9517eafd](https://github.com/fable-compiler/Fable/commit/9517eafd93aa7b84061662a421b7a39a7269318b))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/2098f64a25e96385306ef19ed8c289f3b6c84db9..ce886964cb4521dc4c01c8da089b97c9d9742814)</small></strong>

## 2.10.0 - 2026-10-22

### 🚀 Features

* *(js/ts)* Support System.Diagnostics.Stopwatch (#5100) ([eb68225f](https://github.com/fable-compiler/Fable/commit/eb68225ff74d1b455765d99998faa6fd3be32383))

### 🐞 Bug Fixes

* *(js/ts)* Round decimals below 0.1 correctly ([fca4c408](https://github.com/fable-compiler/Fable/commit/fca4c408a64e33c58293f12869975e5b61bcb96a))
* *(js/ts)* Keep decimal zero scale without leading zeros ([3076b45a](https://github.com/fable-compiler/Fable/commit/3076b45acacf39b8c99ec57025560ad355ab74db))
* *(js/ts)* Hash decimals by value in collections ([32a69e0d](https://github.com/fable-compiler/Fable/commit/32a69e0d00f567c9a838307e9ed51b3b1d519473))
* *(js/ts)* Keep .NET decimal scale through arithmetic (#5126) ([bdf0a6c8](https://github.com/fable-compiler/Fable/commit/bdf0a6c8cb56151a5431b5e3703f7ee165b26dba))
* *(js/ts)* Accept decimal integers up to 2^96 ([c9feb339](https://github.com/fable-compiler/Fable/commit/c9feb339fb8a4e98bebf17b3facdc42ecd22d628))
* *(js/ts)* Align decimal conversion, parsing and G format ([6430cdac](https://github.com/fable-compiler/Fable/commit/6430cdacec34e6327434af649eb0c9427d65608c))
* *(js/ts)* Add missing Async and Task members, cancel timed-out StartChild, bind Async in task { } (#5097) ([d063630d](https://github.com/fable-compiler/Fable/commit/d063630d58d62db21603d45a6bf54b172378c66e))
* *(js/ts)* Respect maxDegreeOfParallelism and cancel on failure in Async.Parallel (#5098) ([5de3271d](https://github.com/fable-compiler/Fable/commit/5de3271d8b47f1752c4e74c010e4314cf40bbcbe))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/e33c57870454fea189a3eb33519f583a5003f8c3..2098f64a25e96385306ef19ed8c289f3b6c84db9)</small></strong>

## 2.9.0 - 2026-10-16

### 🚀 Features

* *(js/ts/python/beam)* Support Async.TryCancelled (#5083) ([1643297d](https://github.com/fable-compiler/Fable/commit/1643297d9297fef536c8d10091d5b0d1fad77af7))

### 🐞 Bug Fixes

* *(js/ts)* Avoid full array copy in Encoding.GetString (#5069) ([e6b72ec7](https://github.com/fable-compiler/Fable/commit/e6b72ec73be454bd3b3233167542a65fc6553c8a))
* *(js/ts/python)* Preserve cancelled cleanup and generic unit callbacks (#5062) ([5dd6b497](https://github.com/fable-compiler/Fable/commit/5dd6b4979e79c8f7c4632cbfb5254d6cf0d2e8d2))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/148f989e1b37b3f6560c3dda615b6a3a8efffcb8..e33c57870454fea189a3eb33519f583a5003f8c3)</small></strong>

## 2.8.1 - 2026-10-12

### 🐞 Bug Fixes

* *(js/ts/python)* Synchronize cancellation registrations (#5036) ([07db576c](https://github.com/fable-compiler/Fable/commit/07db576c4ae16dcef9d1773ac4d6c70b2a223f0a))
* *(js/ts/python)* Settle cancelled delays and finalizers once (#5038) ([a9802484](https://github.com/fable-compiler/Fable/commit/a9802484c8cdeadd5203123d76f42e375c1c3159))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/2369fbd24fafe851544c1ed6eb54f0bfc2b75f36..148f989e1b37b3f6560c3dda615b6a3a8efffcb8)</small></strong>

## 2.8.0 - 2026-09-25

### 🐞 Bug Fixes

* *(js/ts)* Fix Dictionary.Remove and HashSet.Remove to remove empty buckets (#4985) ([aea61b56](https://github.com/fable-compiler/Fable/commit/aea61b562c461a9b383e805f06f68a3b21ed45c2))
* *(js/ts)* Clear custom System.Collections.IList implementations ([2369fbd2](https://github.com/fable-compiler/Fable/commit/2369fbd24fafe851544c1ed6eb54f0bfc2b75f36))

### ⚡ Performance Improvements

* *(js/ts)* Faster `resizeArray.Clear()` in JS/TS ([32d85add](https://github.com/fable-compiler/Fable/commit/32d85add01422547dba03b15e0a0787d8afb031b))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/4a441b1926387a42db6a3a4294acd97b02760ca3..2369fbd24fafe851544c1ed6eb54f0bfc2b75f36)</small></strong>

## 2.7.1 - 2026-09-11

### 🐞 Bug Fixes

* *(js/ts)* Keep the sign of negative decimals below 0.1 ([91d80f18](https://github.com/fable-compiler/Fable/commit/91d80f185fb38e4d23fd9e906037a14daab3e3b9))
* *(js/ts)* Respect the precision when formatting decimals below 0.1 ([1c02557e](https://github.com/fable-compiler/Fable/commit/1c02557efd113d8d30a922c74c3fc9d1e7168cfd))
* *(js/ts)* Omit the decimal sign when the rounded result is zero ([ef63087d](https://github.com/fable-compiler/Fable/commit/ef63087d97b61927763023c4b2e5bb16bcd64af2))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/c49d1ec5d1a491bfff65fe4bd6c71014cac9b321..4a441b1926387a42db6a3a4294acd97b02760ca3)</small></strong>

## 2.7.0 - 2026-09-08

### 🚀 Features

* Add Temporal dates and times (opt-in) (#4759) ([3e8f56f7](https://github.com/fable-compiler/Fable/commit/3e8f56f78345fbee8a4394c5452eb212264701f9))

### 🐞 Bug Fixes

* *(js/ts)* Match .NET for G, E and custom numeric formats ([6fb03090](https://github.com/fable-compiler/Fable/commit/6fb03090ce328b8c5d6a36422f120dd641aefe3a))
* *(js/ts)* Allow the E format specifier on Int64 and UInt64 ([c49d1ec5](https://github.com/fable-compiler/Fable/commit/c49d1ec5d1a491bfff65fe4bd6c71014cac9b321))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/bf694309ae5dbc6bb546d9acc4d091f5e35fe330..c49d1ec5d1a491bfff65fe4bd6c71014cac9b321)</small></strong>

## 2.6.0 - 2026-09-03

### 🚀 Features

* *(all)* Quotations: DerivedPatterns, and captured locals as Value nodes (#4919) ([fae5093f](https://github.com/fable-compiler/Fable/commit/fae5093f2801195e865adf1acfaec63f38bbf973))

### 🐞 Bug Fixes

* *(all)* Emit instance calls for virtual methods inherited from Object (#4926) ([2fb535a7](https://github.com/fable-compiler/Fable/commit/2fb535a7cd35cef501c56684f205e2dffa3c36e6))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/619d7afddbaf25108e819abaaa9d8a6503c457d6..bf694309ae5dbc6bb546d9acc4d091f5e35fe330)</small></strong>

## 2.5.1 - 2026-07-24

### 🐞 Bug Fixes

* *(js/ts,python)* Inline Option/ValueOption combinators to avoid closures and allocations (#4836) ([61cac646](https://github.com/fable-compiler/Fable/commit/61cac646e6385d82247082dbb90a98b1646560de))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/6568f35ece227fc30ce84111c8d2e975b09bac00..619d7afddbaf25108e819abaaa9d8a6503c457d6)</small></strong>

## 2.5.0 - 2026-07-15

### 🚀 Features

* *(js/ts)* Add `System.IO.Directory` support (Exists, CreateDirectory) (#4798) ([6568f35e](https://github.com/fable-compiler/Fable/commit/6568f35ece227fc30ce84111c8d2e975b09bac00))
* *(js/ts/python/beam)* Preserve member calls and pattern matches in quotations (#4780) ([55482e6b](https://github.com/fable-compiler/Fable/commit/55482e6b5e6306fd4dbaba5759024313cb26682a))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/a8d9bf5f0b7cc4560bb107963348e765695c22b6..6568f35ece227fc30ce84111c8d2e975b09bac00)</small></strong>

## 2.4.2 - 2026-07-11

### 🐞 Bug Fixes

* *(js/ts)* Async cancellation propagation and continuation double-resolve (#4746) ([c78a9a8e](https://github.com/fable-compiler/Fable/commit/c78a9a8ed20234feddf3375b61e91098f9261bee))
* *(js/ts)* Date arithmetic: PM parsing, AddMonths day clamp, AddYears offset (#4744) ([be0558eb](https://github.com/fable-compiler/Fable/commit/be0558ebe07f78f7a93ec0c34592d7c066b59fc7))
* *(js/ts)* Int64 non-decimal parsing and BitConverter .NET mismatches (#4748) ([2704d9c5](https://github.com/fable-compiler/Fable/commit/2704d9c5daab87eb1791c0ea1b6b8b06b987247c))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/d6ae6bd3790b57b31941a118cdffaeb6a59155c3..a8d9bf5f0b7cc4560bb107963348e765695c22b6)</small></strong>

## 2.4.1 - 2026-07-09

### 🐞 Bug Fixes

* MailboxProcessor dropping falsy messages and unit replies ([8299b2e7](https://github.com/fable-compiler/Fable/commit/8299b2e7f690e9136906653155883a08607aa4a4))
* *(all)* Add `Seq.enumerateTryWith` for try/with in seq, list, array comprehensions (#4750) ([448c90d7](https://github.com/fable-compiler/Fable/commit/448c90d7ccba80ee3d3c38432a2a1d1407f6dc69))
* *(js/ts)* Match .NET NaN semantics in JS comparison, min and max ([7e5106f9](https://github.com/fable-compiler/Fable/commit/7e5106f9d93cb55ae4aca4656868fa0c53493991))
* *(js/ts)* BigInteger byte-array corruption, checked conversions, gcd, log2 (#4743) ([f3cb7f2b](https://github.com/fable-compiler/Fable/commit/f3cb7f2bfc9e46e558763867d6c1ba323bcf281c))
* *(js/ts)* Observable/Event dropping null/undefined/unit values (#4742) ([75bff73b](https://github.com/fable-compiler/Fable/commit/75bff73be3743fe0af37f99c541b10f6978150f3))
* *(js/ts)* EndsWith regression for empty pattern with non-ordinal comparison ([7793ed7b](https://github.com/fable-compiler/Fable/commit/7793ed7b0fc1f33bf3da6c0ea31607e172726267))
* *(js/ts/python)* Quote strings with the %A format specifier (#4749) ([75321f3b](https://github.com/fable-compiler/Fable/commit/75321f3b2cb70ea345a0a45d6343cbb6e0de466f))
* *(ts/js)* `Regex.Replace` count/substitutions and `Regex.Split` semantics (#4741) ([d6ae6bd3](https://github.com/fable-compiler/Fable/commit/d6ae6bd3790b57b31941a118cdffaeb6a59155c3))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/7f915f1dd66b9a5fbbd56f858b07d39b98519b65..d6ae6bd3790b57b31941a118cdffaeb6a59155c3)</small></strong>

## 2.4.0 - 2026-07-05

### 🚀 Features

* *(js/ts)* Add `System.IO.Path` and `System.IO.File` API support ([ec99fac5](https://github.com/fable-compiler/Fable/commit/ec99fac5fd7a9428ee54f66f878ef4edad0904cb))
* *(js/ts/python)* Add Environment and Console.Error support ([6e082f95](https://github.com/fable-compiler/Fable/commit/6e082f955140676da3388a43b55378ea4ad20f1c))

### 🐞 Bug Fixes

* *(js/ts)* DateTime.ToString() and %A are now consistent across all DateTimeKind values (#4714) ([de98b0e4](https://github.com/fable-compiler/Fable/commit/de98b0e466c8824b0ba8b941d20efae70d232abb))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/c977d78b39225a51c7bd051a1fe363ed0ccbe201..7f915f1dd66b9a5fbbd56f858b07d39b98519b65)</small></strong>

## 2.3.0 - 2026-06-30

### 🚀 Features

* *(js/ts)* Map task { } to Promise<T> ([97f54d36](https://github.com/fable-compiler/Fable/commit/97f54d3692e5ba881c249b289c20b4fad5ac27e2))
* *(js/ts/python/beam)* Add support for `Async.AwaitEvent` (#4693) ([71c98179](https://github.com/fable-compiler/Fable/commit/71c981792b6ff99cd417bb2e70a13a9d55ed72cf))

### 🐞 Bug Fixes

* *(js)* Respect StringComparison in String.IndexOf/LastIndexOf (#4681) ([7cb92d52](https://github.com/fable-compiler/Fable/commit/7cb92d522e5dc8dfe51351565504c59a94c2f408))
* *(js/ts)* Int list is not a union type (#4698) ([ecbecbe7](https://github.com/fable-compiler/Fable/commit/ecbecbe756610fd83a621c005e97eda92bbc332a))
* *(js/ts)* Pass TypeInfo to getRecordFields to handle None fields in anonymous records (#4704) ([c977d78b](https://github.com/fable-compiler/Fable/commit/c977d78b39225a51c7bd051a1fe363ed0ccbe201))
* *(ts)* Widen isEnumDefined value parameter to accept any value (#4690) ([0d6498dd](https://github.com/fable-compiler/Fable/commit/0d6498ddf65e0e18918690633da23b04b78479f6))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/c9b3ee2429a4688946c1936e27df730837428070..c977d78b39225a51c7bd051a1fe363ed0ccbe201)</small></strong>

## 2.2.0 - 2026-06-24

### 🚀 Features

* *(all)* Support Exception.InnerException across all targets (#4677) ([c9b3ee24](https://github.com/fable-compiler/Fable/commit/c9b3ee2429a4688946c1936e27df730837428070))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/15eb83ed36657f75073fb0e1b4cac677e24fc9bb..c9b3ee2429a4688946c1936e27df730837428070)</small></strong>

## 2.1.1 - 2026-06-10

### 🐞 Bug Fixes

* *(js/ts)* Reject JS-permissive date strings that .NET TryParse rejects (#4588) ([09003de](https://github.com/fable-compiler/Fable/commit/09003de8bc72e4f73281828d905d49aab7db3f55))
* *(js/ts)* Make Exception.ToString() return the message instead of "Exception" (#4635) ([4c36f76](https://github.com/fable-compiler/Fable/commit/4c36f761bd378da800e30657c58355bd5324a653))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/63bcd3d90f37cb3934edcc59b5f54f49ffab3896..15eb83ed36657f75073fb0e1b4cac677e24fc9bb)</small></strong>

## 2.1.0 - 2026-05-28

### 🚀 Features

* *(js/ts/python)* Add missing standard DateTime format specifiers for JS/TS and Python (#4547) ([90f5eb7](https://github.com/fable-compiler/Fable/commit/90f5eb7f61710bc679f6fdec30ff5d864984ac08))
* *(js/ts/python)* Add missing StringBuilder.Append overloads for numeric types (#4568) ([b402c30](https://github.com/fable-compiler/Fable/commit/b402c30d04bfca8644314ba4b21894abfff6a713))

### 🐞 Bug Fixes

* [JS/TS] Fix DatetimeOffset.ToString("s") (#4596) ([3ce6f3f](https://github.com/fable-compiler/Fable/commit/3ce6f3fd88cc7f67e34e2bbc311e722c55ffbf45))
* *(js/ts)* Fix datetime custom format off by one year (#4558) ([83bdbb5](https://github.com/fable-compiler/Fable/commit/83bdbb5b34e70eae203831a3a442d477d15911e1))
* *(js/ts)* Hex format specifier uses no padding unless precision is specified (#4603) ([ba9857a](https://github.com/fable-compiler/Fable/commit/ba9857a4dcbf7ab1f936241f6619fda43e039040))
* *(js/ts)* Fix Decimal.GetBits returning incorrect mantissa after round arithmetic result (#4561) ([345bcd4](https://github.com/fable-compiler/Fable/commit/345bcd4b99417b304c71059966126fad74cf3a97))
* *(js/ts)* Fix G/g format specifier corrupting exponential notation when trimming trailing zeros (#4587) ([773c098](https://github.com/fable-compiler/Fable/commit/773c098a8f7ac7e8db1d4e490dec1294a80321ee))
* *(js/ts/python)* Fix FSharpOption not recognized as union type in F# reflection (#4529) ([d78a37d](https://github.com/fable-compiler/Fable/commit/d78a37db9f4c25eb51fac8afcd320b4ea36c60a7))
* *(ts)* Enforce browser-only compatibility in fable-library-ts tsconfig (#4563) ([10c81c1](https://github.com/fable-compiler/Fable/commit/10c81c1361208eb61f5fa78c6704e6fccc068fb1))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/b471dc16fc3b5132af77b5974d1669c9b8220cca..63bcd3d90f37cb3934edcc59b5f54f49ffab3896)</small></strong>

## 2.0.0 - 2026-04-21

### Fixed

* [JS/TS] Fix `ResizeArray` (`System.Collections.Generic.List`) equality to use reference equality instead of structural equality (fixes #3718)
* [JS/TS] Fix `String.Contains` ignoring `StringComparison` argument (second argument was silently discarded)

## 2.0.0-rc.6 - 2026-04-07

### Fixed

* [JS/TS] Fix `Async.StartChild` with timeout always timing out even when the computation finishes before the deadline (fixes #4481) (by @MangelMaxime)

## 2.0.0-rc.5 - 2026-03-31

### Fixed

* [JS/TS] Improve `Regex.Escape` and `Regex.Unescape` handling (by @MangelMaxime)
* [JS/TS] Fixed quotation for union string cases (by @MangelMaxime)

## 2.0.0-rc.4 - 2026-03-19

### Fixed

* [JS/TS] Fix `Unchecked.defaultof<char>` being emitted as `null` instead of `'\0'` (by @MangelMaxime)

## 2.0.0-rc.3 - 2026-03-10

### Fixed

* [TS] Expose optional `stack` property on `Exception` (by @MangelMaxime)

## 2.0.0-rc.2 - 2026-03-03

### Fixed

* [JS/TS] Allows compiling `fable-library-ts` for Browser environment (by @goswinr)

## 2.0.0-rc.1 - 2026-02-26

## 2.0.0-beta.7 - 2026-02-03

* [JS/TS] Fix `StringBuilder.Chars` getter and setter (by @MangelMaxime)

## 2.0.0-beta.6 - 2025-12-26

### Fixed

* [JS/TS] Fix #4305 `DateTimeOffset.Now` returns wrong time (by @ncave)

## 2.0.0-beta.5 - 2025-11-19

### Changed

* [JS/TS] Replace the deprecated `substr` method with `slice` (by @Thorium)

## 2.0.0-beta.4 - 2025-07-25

### Added

* [JS/TS] Initial support for Nullable Reference Types (by @ncave)

## 2.0.0-beta.3 - 2025-03-14

### Fixed

* [JS/TS] Make `nullArgCheck` report the same error message as on .NET (by @MangelMaxime)

## 2.0.0-beta.2 - 2025-03-03

* [JS/TS] Fix #4049: decimal/bigint to integer conversion checks (by @ncave)
* [JS/TS] Fix `decimal` to `char` conversion checks (by @ManngelMaxime)
* [JS/TS] Propagate non-captured exception when running `Async.Start` or `Async.StartImmediate` (by @MangelMaxime)
* [JS/TS] Remove `Async.RunSynchronously` (by @MangelMaxime)
* [JS/TS] Change signature of `startWithContinuations` to always require all its arguments (by @MangelMaxime)
* [JS/TS] Fix short `DateTime` and `DateTimeOffset` short format strings (by @MangelMaxime)
* [JS/TS] Add `C` and `c` format for numeric types (by @MangelMaxime)
* [JS/TS] Add `B` and `b` format for numeric types (by @MangelMaxime)
* [JS/TS] Add `n` format for numeric types (by @MangelMaxime)
* [JS/TS] Fix numeric formats (by @MangelMaxime)

## 2.0.0-beta.1 - 2025-02-16

* Compiled with Fable 5.0.0-alpha.10

## 1.10.0 - 2025-01-23

### Added

* [JS/TS] Add `StringBuiler.Append(c: char, repeatCount: int)` overload (by @roboz0r)

## 1.9.0 - 2025-01-09

### Added

* [JS/TS] Add new `TimSpane` overload support coming from .NET 9 (by @MangelMaxime)

### Fixed

* [JS/TS] Fix `DateTimeOffset.ToLocalTime` (by @MangelMaxime)

## 1.8.0 - 2024-11-19

* [JS/TS] Fix `h` in `DateTime.ToString` (@MangelMaxime)
* [JS/TS] Fix `hh` in `DateTime.ToString` (@MangelMaxime)

## 1.7.0 - 2024-11-19

### Fixed

* [JS/TS] Added missing IReadOnlyCollection helpers (#3953)

## 1.6.0 - 2024-10-02

### Removed

* Remove `Async` (from `Async.ts`) class (by @MangelMaxime)

### Changed

* Renamed `IAsync` to `Async` in `AsyncBuilder.ts` (#3906) (by @ncave)

## 1.5.0 - 2024-09-19

### Added

* [JS/TS] Add support for `OrdinalIgnoreCase` overload for `String.EndsWith` (#3892) (by @goswinr)
* [JS/TS] Add `uri.Port`, `uri.IsDefaultPort` (by @MangelMaxime)

### Fixed

* [JS/TS] Fix escaping of `{` and `}` in FormattableString (#3890) (by @roboz0r)
* [JS/TS] Fix `uri.Host` to return the host name without the port (by @MangelMaxime)
* [JS/TS] Fix TypeScript compilation by resolving type of `jsOptions` (#3894) (by @MangelMaxime)

## 1.4.3 - 2024-09-04

* [JS/TS] Fixed Decimal comparisons (#3884) (by @ncave)

## 1.4.2 - 2024-06-13

### Fixed

* [JS/TS] Fixed BigInt.ToDecimal with negative values (#3500) (by @ncave)

## 1.4.1 - 2024-06-13

### Fixed

* [JS/TS] Fixed DateTime.MinValue, DateTime.MaxValue (#3836) (by @ncave)

## 1.4.0 - 2024-03-20

### Added

* [JS/TS] Add `ConditionalWeakTable` (by @chkn)

## 1.3.0 - 2024-03-18

* [JS/TS] `Boolean.tryParse` should not crash on `null` string (@goswinr)

## 1.2.0 - 2024-03-01

### Fixed

* [GH-3772](https://github.com/fable-compiler/Fable/pull/3772) [JS/TS] Re-implement `DateTime.ToString` custom format handling (by @MangelMaxime)

    It now supports all custom format specifiers, and behave as if `CultureInfo.InvariantCulture` was used (Fable does not support Globalization).

## 1.1.0 - 2024-02-20

### Added

#### JavaScript

* [GH-3759](https://github.com/fable-compiler/Fable/issues/3759) Add `StringBuilder.Chars` (by @MangelMaxime)
* Add `StringBuilder.AppendFormat` (by @ncave)
* [GH-3748](https://github.com/fable-compiler/Fable/pull/3748) Add `Array.getItem` and `Array.setItem` (by @MangelMaxime)

## 1.0.0 - 2024-02-13

* Release stable version

## 1.0.0-beta-001 - 2024-02-12

### Changed

* Separate `Result` from `Choice`
* Released as part of Fable 4.12.0
