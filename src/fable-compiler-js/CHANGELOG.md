---
last_commit_released: 619d7afddbaf25108e819abaaa9d8a6503c457d6
include:
  - ../Fable.Core/
  - ../fable-standalone/
  - ../Fable.Transforms/
updaters:
  - package.json:
      file: package.json
---

# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 2.11.0 - 2026-07-24

### 🐞 Bug Fixes

* *(all)* Eliminate dead bindings left by CE/inline lowering (#4837) ([9f694e97](https://github.com/fable-compiler/Fable/commit/9f694e970eed60418264cf4e05054ceae2bbe18e))
* *(beam)* Make reflection metadata agree with record/union codegen (#4849) ([ec6a3df0](https://github.com/fable-compiler/Fable/commit/ec6a3df0cd59d1211d0f35b29ae95dc9c656c4fb))
* *(js/ts)* Resolve generic params when reflecting erased union fields (#4851) ([619d7afd](https://github.com/fable-compiler/Fable/commit/619d7afddbaf25108e819abaaa9d8a6503c457d6))
* *(js/ts,python)* Inline Option/ValueOption combinators to avoid closures and allocations (#4836) ([61cac646](https://github.com/fable-compiler/Fable/commit/61cac646e6385d82247082dbb90a98b1646560de))

### ⚡ Performance Improvements

* *(all)* Lower constant ±1 step ranges to counted for-loops (#4838) ([ab28daec](https://github.com/fable-compiler/Fable/commit/ab28daecc7b35b4932829c9ffc838c1adb8a880f))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/b2ce30674d93152d9dac4331912fe06b62b8a0df..619d7afddbaf25108e819abaaa9d8a6503c457d6)</small></strong>

## 2.10.0 - 2026-07-22

### 🚀 Features

* *(beam)* Render %A in F# syntax instead of dumping Erlang terms (#4814) ([218e0c40](https://github.com/fable-compiler/Fable/commit/218e0c40aee0283bb8c9c74ab36ee0f912b50c51))

### 🐞 Bug Fixes

* *(beam)* Correct char-to-string conversion and console Unicode encoding (#4812) ([df8e01d6](https://github.com/fable-compiler/Fable/commit/df8e01d65a9aa1e4cdf853e3b209eb07c1d979c4))
* *(beam)* Isolate each top-level effect's variables in main/0 (#4815) ([8d708633](https://github.com/fable-compiler/Fable/commit/8d708633e0c0e48d227bf653e2cbf91e203bd539))
* *(beam)* Decode UTF-8 codepoints in ToCharArray and related String functions ([b2ce3067](https://github.com/fable-compiler/Fable/commit/b2ce30674d93152d9dac4331912fe06b62b8a0df))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/2fc9c1415a9abcf729d920ca6c99732b3c894993..b2ce30674d93152d9dac4331912fe06b62b8a0df)</small></strong>

## 2.9.0 - 2026-07-17

### 🚀 Features

* *(dart)* Quotations (#4784) ([019a6ade](https://github.com/fable-compiler/Fable/commit/019a6adeb42b17de24f1d491f227ef56fa4d0cd4))

### 🐞 Bug Fixes

* *(python)* Make class declarations and references agree on type names (#4807) ([ae4b910b](https://github.com/fable-compiler/Fable/commit/ae4b910bd651a9be8b277690b3d617035d7ebbf2))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/7b2a262f6538f25b78feb70b0d4533a2bf298638..2fc9c1415a9abcf729d920ca6c99732b3c894993)</small></strong>

## 2.8.0 - 2026-07-16

### 🚀 Features

* *(beam)* Support `Environment.CurrentDirectory` and `GetEnvironmentVariable` (#4801) ([55b9d560](https://github.com/fable-compiler/Fable/commit/55b9d560f1d55f65af93293bc99fb92121ba4269))
* *(beam)* Support setting `Environment.CurrentDirectory` (#4783) ([7b2a262f](https://github.com/fable-compiler/Fable/commit/7b2a262f6538f25b78feb70b0d4533a2bf298638))
* *(rust)* Various codegen and runtime improvements (#4781) ([ae10cd2c](https://github.com/fable-compiler/Fable/commit/ae10cd2c41524a9d6f9670163a8889e1320ef43f))

    * Cast `bool` to integer types; support `**` via `.powf`
    * Add `Decimal.IsInteger/IsEvenInteger/IsOddInteger/IsCanonical`
    * Fix two-switch decision trees dispatching to the wrong target
    * Fix reference-typed match bindings panicking via `mem::zeroed`
    * Fix string and non-ident option pattern matches emitting invalid Rust
    `match` arms
    * Fix `Convert.ToXxx(bool)`
    * Fix `Char.ToUpper/ToLower` truncating multi-char case mappings
    * Fix `Random.nextDouble` panic message
    * Fix `Map.find/tryFind` panicking on reference-typed values

### 🐞 Bug Fixes

* *(all)* Skip translating unreachable branch of constant-condition if/then/else (#4799) ([53c06de1](https://github.com/fable-compiler/Fable/commit/53c06de1a1e21de7fbf99f416765fe1d0061fbff))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/6568f35ece227fc30ce84111c8d2e975b09bac00..7b2a262f6538f25b78feb70b0d4533a2bf298638)</small></strong>

## 2.7.0 - 2026-07-15

### 🚀 Features

* *(js/ts)* Add `System.IO.Directory` support (Exists, CreateDirectory) (#4798) ([6568f35e](https://github.com/fable-compiler/Fable/commit/6568f35ece227fc30ce84111c8d2e975b09bac00))
* *(js/ts/python/beam)* Preserve member calls and pattern matches in quotations (#4780) ([55482e6b](https://github.com/fable-compiler/Fable/commit/55482e6b5e6306fd4dbaba5759024313cb26682a))

### 🐞 Bug Fixes

* *(beam)* Make Assert.AreEqual/NotEqual raise, and stop emitting reflection calls for erased types (#4775) ([d9711d4c](https://github.com/fable-compiler/Fable/commit/d9711d4c4586e38787bddf7cfa412e3388cb8ab9))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/c6455f4798e8eba6375ea90c8270dd3df11e0bd4..6568f35ece227fc30ce84111c8d2e975b09bac00)</small></strong>

## 2.6.2 - 2026-07-13

### 🐞 Bug Fixes

* *(beam)* Make F# reflection work on the Beam target (#4766) ([b15b280d](https://github.com/fable-compiler/Fable/commit/b15b280d9d5d1084786179d2eb292a2e916e8aee))
* *(beam)* Give sized integers .NET fixed-width semantics (#4769) ([2ed39630](https://github.com/fable-compiler/Fable/commit/2ed396305488efce793efbfca8e4671ada1793fd))
* *(beam)* Qualify Erlang module names by their OTP app (#4770) ([5516c8d8](https://github.com/fable-compiler/Fable/commit/5516c8d8991401e85b3ad08d7f9a6f76c3219ff2))
* *(beam)* Entry point argv and exit code, bigint from float, option reflection (#4772) ([ea915541](https://github.com/fable-compiler/Fable/commit/ea915541d0b07747cc1f08415c4d1f7525e065bd))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/a8d9bf5f0b7cc4560bb107963348e765695c22b6..c6455f4798e8eba6375ea90c8270dd3df11e0bd4)</small></strong>

## 2.6.1 - 2026-07-11

### 🐞 Bug Fixes

* *(beam)* Order discriminated union comparison by declaration order (#4761) ([3597a582](https://github.com/fable-compiler/Fable/commit/3597a5829dbdf276ab9fef9d43e8be52b14e3c88))
* *(beam)* Preserve function-value identity for PhysicalEquality (#4762) ([727673f3](https://github.com/fable-compiler/Fable/commit/727673f343eba9e234dc122153ce50b96abe6b38))
* *(js/ts)* Don't drop unsupported `jsOptions` statements when inlining as POJO (#4754) ([4cbc44fc](https://github.com/fable-compiler/Fable/commit/4cbc44fc4219f34dc0c52752ea2b2049e82b410c))
* *(python)* Propagate [<EntryPoint>] return value as process exit code (#4763) ([a8d9bf5f](https://github.com/fable-compiler/Fable/commit/a8d9bf5f0b7cc4560bb107963348e765695c22b6))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/d6ae6bd3790b57b31941a118cdffaeb6a59155c3..a8d9bf5f0b7cc4560bb107963348e765695c22b6)</small></strong>

## 2.6.0 - 2026-07-09

### 🐞 Bug Fixes

* *(all)* Include original exception message when loading a Fable plugin fails (#4739) ([659e73a4](https://github.com/fable-compiler/Fable/commit/659e73a4c59db397fb0821de371ea5c48731d505))
* *(all)* Add `Seq.enumerateTryWith` for try/with in seq, list, array comprehensions (#4750) ([448c90d7](https://github.com/fable-compiler/Fable/commit/448c90d7ccba80ee3d3c38432a2a1d1407f6dc69))
* *(js/ts)* Represent union cases with no fields as singletons ([3f6e618e](https://github.com/fable-compiler/Fable/commit/3f6e618eb33b7344437476dc5f2f7334a29662aa))
* *(python)* Represent union cases with no fields as singletons ([0211b4cc](https://github.com/fable-compiler/Fable/commit/0211b4ccf19b4eb8ea4677a6734d7e691020cb68))
* *(python)* Emit forward reference for self-referencing static union members (#4752) ([452b85f2](https://github.com/fable-compiler/Fable/commit/452b85f26bcec34bfdc61405162948bf7d8810c0))
* *(python)* Match .NET NaN and signed-zero semantics in comparison, min and max ([383bb93c](https://github.com/fable-compiler/Fable/commit/383bb93ccd35e03dfaa1bbf897ef9ac83ae1ec95))
* *(python)* `LastIndexOf(value, startIndex)` off-by-one on inclusive boundary ([bad6bcac](https://github.com/fable-compiler/Fable/commit/bad6bcacf8d8d17337ae3bdb988da648f23c9640))

### ⚡ Performance Improvements

* *(python)* Use native operators for int64/uint64 arithmetic (#4727) ([f062d668](https://github.com/fable-compiler/Fable/commit/f062d668f3514e5f43d5307e7504e7ca124efcce))
* *(python)* Rewrite hot Seq combinators as native Python generators (#4728) ([73e0eab9](https://github.com/fable-compiler/Fable/commit/73e0eab9c3071f44fd87da38658a8b73acfa3ccc))
* *(python)* Read union case fields directly instead of rebuilding Array (#4725) ([e4eaa80b](https://github.com/fable-compiler/Fable/commit/e4eaa80b1e9390abee0d2d53bcdb026a5633b7c6))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/7f915f1dd66b9a5fbbd56f858b07d39b98519b65..d6ae6bd3790b57b31941a118cdffaeb6a59155c3)</small></strong>

## 2.5.0 - 2026-07-05

### 🚀 Features

* *(all)* Allow `[<Emit>]` on StringEnum cases (#4711) ([0b1f61ee](https://github.com/fable-compiler/Fable/commit/0b1f61eef50d8225602ec4fd1fe641126302983a))
* *(js/ts)* Optimize `printfn` with constant string to `console.log` (#4718) ([2d6d083a](https://github.com/fable-compiler/Fable/commit/2d6d083a0ce552b9ccac5128977cc4390132010a))
* *(js/ts)* Add `Path.Combine` support ([b1b1e273](https://github.com/fable-compiler/Fable/commit/b1b1e273b25c2c50b29a901dc40ec9dd522e1bb1))
* *(js/ts)* Add `System.IO.Path` and `System.IO.File` API support ([ec99fac5](https://github.com/fable-compiler/Fable/commit/ec99fac5fd7a9428ee54f66f878ef4edad0904cb))
* *(js/ts/python)* Warn on duplicate attached member names (#4715) ([fd06aa64](https://github.com/fable-compiler/Fable/commit/fd06aa64c42b9ce28a1195f67bf88e50ca04644c))
* *(js/ts/python)* Add Environment and Console.Error support ([6e082f95](https://github.com/fable-compiler/Fable/commit/6e082f955140676da3388a43b55378ea4ad20f1c))
* *(py)* Add `Path.Combine` support ([4c901c65](https://github.com/fable-compiler/Fable/commit/4c901c6567e9144a9167c9bcb6c749a236076b3d))
* *(python)* Add `Directory.Exists`, `Directory.CreateDirectory` ([f4b4eabc](https://github.com/fable-compiler/Fable/commit/f4b4eabc846d20a91b390f73193d3f0276c70310))
* *(ts)* [<Pojo>] interfaces extend parent [<Pojo>] type (#4717) ([4f4243cd](https://github.com/fable-compiler/Fable/commit/4f4243cd26e3bd10048001500e99f1f7ee92e35a))

### 🐞 Bug Fixes

* Generate an error on erased union case typed as obj (#4710) ([bf2134d5](https://github.com/fable-compiler/Fable/commit/bf2134d5d3715de3025cbeb9e0f98763a2cfff3c))
* *(all)* Include return type in overload suffix for op_Implicit/op_Explicit (#4712) ([a6846010](https://github.com/fable-compiler/Fable/commit/a68460107bb640c04bf88edae935f018d364c9dd))
* *(js/ts)* DateTime.ToString() and %A are now consistent across all DateTimeKind values (#4714) ([de98b0e4](https://github.com/fable-compiler/Fable/commit/de98b0e466c8824b0ba8b941d20efae70d232abb))
* *(python)* Accept allowAccessToPrivateRepresentation in FSharpType.IsUnion/IsRecord/GetUnionCases (#4722) ([7f915f1d](https://github.com/fable-compiler/Fable/commit/7f915f1dd66b9a5fbbd56f858b07d39b98519b65))
* *(ts)* Strip byref annotation from compiler-generated copy-update locals (#4716) ([1e072ed2](https://github.com/fable-compiler/Fable/commit/1e072ed297ab9b8a4623a17bbc2be87af05f81fd))

### ⚡ Performance Improvements

* Split Fable.Transforms into per-target projects for faster incremental builds (#4720) ([260c962f](https://github.com/fable-compiler/Fable/commit/260c962fe754ab9810c2a977223720d2f78dca74))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/c977d78b39225a51c7bd051a1fe363ed0ccbe201..7f915f1dd66b9a5fbbd56f858b07d39b98519b65)</small></strong>

## 2.4.0 - 2026-06-30

### 🚀 Features

* *(all)* Add `Compiler.is*` target detection flags for conditional branching (#4692) ([621dea31](https://github.com/fable-compiler/Fable/commit/621dea31daaef6a9dcf8a2ac5ceb144a063bb873))
* *(js/ts)* Map task { } to Promise<T> ([97f54d36](https://github.com/fable-compiler/Fable/commit/97f54d3692e5ba881c249b289c20b4fad5ac27e2))

### 🐞 Bug Fixes

* *(all)* Pass raw value for optional arguments of native bindings (#4688) ([a4c75483](https://github.com/fable-compiler/Fable/commit/a4c7548313fcbefc0ff0eddc7a95d8d0599d68a6))
* *(all)* Support .NET format specifiers and alignment in interpolated strings (#4695) ([7c50a3af](https://github.com/fable-compiler/Fable/commit/7c50a3af39aee935ce318f44e04247da32b33f0f))
* *(all)* Error on inline function referencing private value (#4701) ([cea6806e](https://github.com/fable-compiler/Fable/commit/cea6806ea388e73cf42fb673960f7862bb497fc2))
* *(beam)* Support module-level mutable variables via process dictionary (#4676) ([535b9af0](https://github.com/fable-compiler/Fable/commit/535b9af06969bcd7bdf118bdfb633ccadd5e4177))
* *(beam)* Namespace module-level mutable state keys by module (#4683) ([1ec28ab1](https://github.com/fable-compiler/Fable/commit/1ec28ab10923623eaf04b90bf8ffbeb54469d454))
* *(js)* Respect StringComparison in String.IndexOf/LastIndexOf (#4681) ([7cb92d52](https://github.com/fable-compiler/Fable/commit/7cb92d522e5dc8dfe51351565504c59a94c2f408))
* *(js/ts)* Implicit DateTime to DateTimeOffset conversion (#4697) ([fcbd9b91](https://github.com/fable-compiler/Fable/commit/fcbd9b91397a41226df4bed25c1bd1f0667efb64))
* *(js/ts)* Hoist emitJsStatement imports with trailing comments (#4702) ([e820eb72](https://github.com/fable-compiler/Fable/commit/e820eb7239b2ffe0f2cb4bf3e6c040a1c7dca57f))
* *(js/ts)* Uncurry mutable module value alias calls (#4703) ([0de0d218](https://github.com/fable-compiler/Fable/commit/0de0d2187a68db0ac35bd2546dd98eb1c67397e5))
* *(js/ts)* Pass TypeInfo to getRecordFields to handle None fields in anonymous records (#4704) ([c977d78b](https://github.com/fable-compiler/Fable/commit/c977d78b39225a51c7bd051a1fe363ed0ccbe201))
* *(js/ts/python)* Drop allowAccessToPrivateRepresentation arg from reflection calls (#4689) ([ec3cded7](https://github.com/fable-compiler/Fable/commit/ec3cded78cc98dc740ce978d3bb078f2603f4846))
* *(ts)* Use declared type-param names in object expression generic methods (#4685) ([1c3e7d99](https://github.com/fable-compiler/Fable/commit/1c3e7d99653c7119c615b6dfd095a45e36add5b3))
* *(ts)* Use signature type-param names when implementing generic interface methods (#4686) ([03e5deee](https://github.com/fable-compiler/Fable/commit/03e5deeedaa35be2974f40e439e520526cf4c1ba))
* *(ts)* Bound enum-constrained type parameters with `extends number` (#4687) ([15daf624](https://github.com/fable-compiler/Fable/commit/15daf6245212ddb8b7ac6296631381711e34c98d))
* *(ts)* Keep enclosing type parameters in scope for nested members (#4691) ([f5723a2d](https://github.com/fable-compiler/Fable/commit/f5723a2d008408e1bca140256a452ac556c69c00))
* *(ts)* Re-declare class type params on generic static attached members (#4696) ([2da50780](https://github.com/fable-compiler/Fable/commit/2da50780b8254cfad05f3a645780f60999cae4ee))
* *(ts)* Filter class type params from attached let-binding methods (#4700) ([d3a6a1dd](https://github.com/fable-compiler/Fable/commit/d3a6a1dd09066b25d876e28446d70fea58162abd))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/c9b3ee2429a4688946c1936e27df730837428070..c977d78b39225a51c7bd051a1fe363ed0ccbe201)</small></strong>

## 2.3.0 - 2026-06-24

### 🚀 Features

* *(all)* Support Exception.InnerException across all targets (#4677) ([c9b3ee24](https://github.com/fable-compiler/Fable/commit/c9b3ee2429a4688946c1936e27df730837428070))

### 🐞 Bug Fixes

* *(beam)* Make immutable class instances process-portable (#4670) ([182eebef](https://github.com/fable-compiler/Fable/commit/182eebefacb58b7c72270c8d651936f87c3cbbcc))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/477b8c693d948f74d8d2d5c38ac4d8e1f9287a51..c9b3ee2429a4688946c1936e27df730837428070)</small></strong>

## 2.2.0 - 2026-06-16

### 🚀 Features

* *(beam)* Add !^ erased-cast operator to BeamInterop (#4659) ([206776b](https://github.com/fable-compiler/Fable/commit/206776bfcc7068f9953bf6e70aafcc24c4082248))

### 🐞 Bug Fixes

* Add type specifiers to interpolated strings (#4646) ([512ae4d](https://github.com/fable-compiler/Fable/commit/512ae4d1901d6ed3c4c10bb5b4f99e73c49a2056))
* *(all)* Align `invalidArg` error message with .NET format (#4662) ([e672eb7](https://github.com/fable-compiler/Fable/commit/e672eb7ff6c38e15bada7520142024456b0dbb61))
* *(beam)* Preserve element type through Option.Value for interface dispatch (#4661) ([e5521b6](https://github.com/fable-compiler/Fable/commit/e5521b63f70ea25c3b42447ef4327f95245002da))
* *(python)* Emit wildcard default for union or-patterns (#4649) (#4653) ([c96f0ec](https://github.com/fable-compiler/Fable/commit/c96f0ecfe40d6be0fe8d95766ba279c3be4f6f7b))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/15eb83ed36657f75073fb0e1b4cac677e24fc9bb..477b8c693d948f74d8d2d5c38ac4d8e1f9287a51)</small></strong>

## 2.1.0 - 2026-06-10

### 🚀 Features

* *(beam)* Support Erlang/BEAM target in standalone compiler (#4644) ([30310e4](https://github.com/fable-compiler/Fable/commit/30310e4f915443524cf5f2c1b326840ee556c7ae))

### 🐞 Bug Fixes

* *(beam)* Collapse array-literal process-dict ref round-trips in FFI/Emit calls (#4626) ([234ee08](https://github.com/fable-compiler/Fable/commit/234ee0846cf848dbad18d409c341755ad8d11da7))
* *(beam)* Make Emit $N substitution a single left-to-right pass (#4631) ([48af8db](https://github.com/fable-compiler/Fable/commit/48af8dba6a92d91115505c62f6a51c6aaa88ac19))
* *(js/ts)* Throw an error when trying to set non-property memeber inside of `jsOptions` (#4624) ([2d9673f](https://github.com/fable-compiler/Fable/commit/2d9673fb609f6ae3273576c913929e0f601d8f5d))
* *(python)* Avoid union case field name collision with Union.name (#4647) ([ff5df24](https://github.com/fable-compiler/Fable/commit/ff5df24191a1b3aef5c86efa25f87fa69b3462d9))
* *(python)* Make [<AttachMembers>] union static members work (#4634) (#4636) ([15eb83e](https://github.com/fable-compiler/Fable/commit/15eb83ed36657f75073fb0e1b4cac677e24fc9bb))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/63bcd3d90f37cb3934edcc59b5f54f49ffab3896..15eb83ed36657f75073fb0e1b4cac677e24fc9bb)</small></strong>

## 2.0.1 - 2026-05-28

### 🐞 Bug Fixes

* [JS/TS/Tests] Remove mocha to resolve package vulnerabilities (#4604) ([092324b](https://github.com/fable-compiler/Fable/commit/092324b55d17973ef8364c00d4ed48eb067c0f23))
* [Rust] Update hashing and datetime tests (#4613) ([9d39037](https://github.com/fable-compiler/Fable/commit/9d390376fd2c81f76fde70c522b59f61a9c9175d))
* *(all)* Duplicate LetRec bindings during inline expansion (#4592) ([62612a5](https://github.com/fable-compiler/Fable/commit/62612a5bb42644934b7573b7cf8f9db930c5dc37))
* *(all)* Fix super call in multi-level generic class hierarchy using wrong mangled name (#4414) ([db1d4d2](https://github.com/fable-compiler/Fable/commit/db1d4d2e8cdbb9d5a16856c91192c1f0fab3f783))
* *(js/ts)* Fix JSX props with long string values causing compile error (fixes #3839) (#4545) ([d828a46](https://github.com/fable-compiler/Fable/commit/d828a461797e3f33bf4ab99b46030d16b29771e6))
* *(js/ts)* Add support for `match` clauses in `JSX.create` (#4620) ([616edca](https://github.com/fable-compiler/Fable/commit/616edca568890549ff81cfa18ab32ca0b807c8eb))
* *(js/ts)* Don't spread last arg in String.Concat call (#4621) ([3de580e](https://github.com/fable-compiler/Fable/commit/3de580e4447ed0090a79648caa8f1e5003bac91d))
* *(js/ts)* Inline AttachMembers members at call sites (#4622) ([63bcd3d](https://github.com/fable-compiler/Fable/commit/63bcd3d90f37cb3934edcc59b5f54f49ffab3896))
* *(python)* Avoid duplicate captured argument in hoisted guards (#4610) (#4611) ([90aec48](https://github.com/fable-compiler/Fable/commit/90aec483296d26549aedf9eaf46f0825d70a4022))

<strong><small>[View changes on Github](https://github.com/fable-compiler/Fable/compare/b471dc16fc3b5132af77b5974d1669c9b8220cca..63bcd3d90f37cb3934edcc59b5f54f49ffab3896)</small></strong>

## 2.0.0 - 2025-04-21

* Fable 5

## 2.0.0-rc.1 - 2025-02-26

* Fable 5.0.0-rc.1

## 2.0.0-beta.1 - 2025-02-16

* Fable 5.0.0-alpha.10
* Replace `FABLE_COMPILER_4` with `FABLE_COMPILER_5` as the compiler directive

## 1.2.2 - 2024-05-24

### Fixed

* Fixed includes to come from this package internals (by @MangelMaxime)

## 1.2.1 - 2024-05-24

### Fixed

* Fixed includes to come from this package internals and to use `fable-library-js` instead of `fable-library-ts` (by @ncave)

## 1.2.0 - 2024-05-23

### Changed

* Use `@fable-org/fable-metadata` package instead of `fable-metadata` (by @MangelMaxime)
* Use `@fable-org/fable-standalone` package instead of `fable-standalone` (by @MangelMaxime)
* Make `GetDirectoryName` return `"."` instead of `""` if the path doesn't contain any directory (by @MangelMaxime)

### Fixed

* Fix initialization of `fable-standalone` with the new package format (by @MangelMaxime)

## 1.1.0 - 2024-02-20

* Add `NPM_PACKAGE_FABLE_COMPILER_JAVASCRIPT` compiler directive (by @MangelMaxime)

## 1.0.0 - 2024-02-12

* Release stable version

## 1.0.0-beta-001 - 2024-02-12

### Added

* First release as part of `@fable-org` scope
