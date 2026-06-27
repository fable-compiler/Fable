---
last_commit_released: c9b3ee2429a4688946c1936e27df730837428070
include:
  - ../Fable.Transforms/
  - ../fcs-fable/
updaters:
  - regex:
      file: ../fable-compiler-js/package.json
      pattern: '(?<="@fable-org/fable-standalone": "\^)\d+\.\d+\.\d+'
  - ../Fable.AST/
---

# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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

## 2.0.0 - 2026-04-21

* Fable 5.0.0

## 2.0.0-rc.7 - 2026-04-07

* Fable 5.0.0-rc.7

## 2.0.0-rc.6 - 2026-03-31

* Fable 5.0.0-rc.6

## 2.0.0-rc.5 - 2026-03-20

* Fable 5.0.0-rc.5

## 2.0.0-rc.4 - 2026-03-19

* Fable 5.0.0-rc.4

## 2.0.0-rc.3 - 2026-03-10

* Fable 5.0.0-rc.3

## 2.0.0-rc.2 - 2026-03-03

* Fable 5.0.0-rc.2

## 2.0.0-rc.1 - 2026-02-26

* Fable 5.0.0-rc.1

## 2.0.0-beta.15 - 2026-02-13

* Fable 5.0.0-alpha.24

## 2.0.0-beta.14 - 2026-02-03

* Fable 5.0.0-alpha.23

## 2.0.0-beta.13 - 2025-12-26

* Fable 5.0.0-alpha.22

## 2.0.0-beta.12 - 2025-12-15

* Fable 5.0.0-alpha.21

## 2.0.0-beta.11 - 2025-12-08

* Fable 5.0.0-alpha.20

## 2.0.0-beta.10 - 2025-12-04

* Fable 5.0.0-alpha.19

## 2.0.0-beta.9 - 2025-12-03

* Fable 5.0.0-alpha.18

## 2.0.0-beta.8 - 2025-11-27

* Fable 5.0.0-alpha.17

## 2.0.0-beta.7 - 2025-11-25

* Fable 5.0.0-alpha.16

## 2.0.0-beta.6 - 2025-11-19

* Fable 5.0.0-alpha.15

## 2.0.0-beta.5 - 2025-07-25

* Fable 5.0.0-alpha.14

## 2.0.0-beta.4 - 2025-05-04

* Fable 5.0.0-alpha.13 - 2025-05-04

## 2.0.0-beta.3 - 2025-03-14

* Fable 5.0.0-alpha.12

## 2.0.0-beta.2 - 2025-03-03

* Fable 5.0.0-alpha.11

## 2.0.0-beta.1 - 2025-02-16

* Fable 5.0.0-alpha.10
* Replace `FABLE_COMPILER_4` with `FABLE_COMPILER_5` as the compiler directive

## 1.12.0 - 2024-11-19

* Fable 4.24.0

## 1.11.0 - 2024-10-28

* Fable 4.23.0

## 1.10.0 - 2024-10-02

* Fable 4.22.0

## 1.9.0 - 2024-09-19

* Fable 4.21.0

## 1.9.0 - 2024-09-04

* Fable 4.20.0

## 1.8.1 - 2024-06-17

* Fable 4.19.3

## 1.8.0 - 2024-06-13

### Changed

* Fable 4.19.1

## 1.7.0 - 2024-06-10

### Changed

* Fable 4.19.0

## 1.6.0 - 2024-05-23

### Fixed

* Make `dirname-filename-esm` a `dependency` instead of a `devDependency`

### Changed

* Fable 4.18.0

## 1.5.0 - 2024-03-20

### Changed

* Fable 4.17.0

## 1.4.0 - 2024-03-20

### Changed

* Fable 4.16.0

## 1.3.0 - 2024-03-18

### Changed

* Fable 4.15.0

## 1.2.0 - 2024-03-01

### Changed

* Fable 4.14.0

## 1.1.0 - 2024-02-20

### Changed

* Fable 4.13.0

## 1.0.0 - 2024-02-12

* Release stable version

## 1.0.0-beta-001 - 2024-02-12

### Changed

* First release as part of `@fable-org` scope
* Released as part of Fable 4.12.0
