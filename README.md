MarkII
======

[HTML version](https://dochost.me/github/izgzhen/markii/blob/master/docs/index.html)

Android/Java Static Analysis Library.

## Dependency

- [SBT](https://www.scala-sbt.org/index.html)
- Java 8

## Usage

```
./build-run-markii.sh $APK_PATH $OUTPUT_PATH
```

- Input `$APK_PATH`: path to input APK file
- Output `$OUTPUT_PATH`: path to output facts directory

Supported facts (partial, see [`Fact`](./docs/api/com/research/nomad/markii/FactsWriter$$Fact$.html) for a complete list):

- `eventHandler(e: Event, cb: Method, v: ViewID)`
  - When event `e` is trigger on view `v`, the handler `cb` will run
- `layoutWidth(dim: Dimension, v: ViewID)`
  - The layout width of view `v` is `dim` (e.g. `fill_parent`)
- `mainActivity(act: Class)`
  - The main activity of the APK is `act`
- `idName(n: String, v: ViewID)`
  - The id-name of view `v` is `n`
- ...

## Algorithms

[Tutorial](./docs/algorithm-tutorial.md)

`SPARK` is the default call graph and pointer analysis producer for Soot.
It is better than CHA though flow-insensitive and context-insensitive.
Also, older Soot library's `SPARK` has some problems processing the latest versions of android apps.


## Acknowledgements

- http://web.cse.ohio-state.edu/presto/software/gator/

## Contributors

- Luxi Wang
