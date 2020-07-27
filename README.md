MarkII
======

[![codecov](https://codecov.io/gh/izgzhen/markii/branch/master/graph/badge.svg)](https://codecov.io/gh/izgzhen/markii)
![CI](https://github.com/izgzhen/markii/workflows/CI/badge.svg)

Android/Java Static Analysis Library.

## Dependency

- [SBT](https://www.scala-sbt.org/index.html)
- Java 8

And then inside `lib/`:

```
bash install.sh
```

## Usage

```
./build-run-markii.sh $APK_PATH $OUTPUT_PATH
```

- Input `$APK_PATH`: path to input APK file
- Output `$OUTPUT_PATH`: path to output facts directory

Supported facts (partial, see [`Fact`](https://blog.zhen-zhang.com/markii/api/com/research/nomad/markii/FactsWriter$$Fact$.html) for a complete list):

- `eventHandler(e: Event, cb: Method, v: ViewID)`
  - When event `e` is trigger on view `v`, the handler `cb` will run
- `layoutWidth(dim: Dimension, v: ViewID)`
  - The layout width of view `v` is `dim` (e.g. `fill_parent`)
- `mainActivity(act: Class)`
  - The main activity of the APK is `act`
- `idName(n: String, v: ViewID)`
  - The id-name of view `v` is `n`
- ...
