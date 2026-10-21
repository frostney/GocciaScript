# Native AOT loading benchmark

`AotLoading.pas` compares the front end for generated module libraries with
1,000 and 5,000 exported functions. It reports the median of 11 measured
samples after three warmups.

From the repository root:

```sh
mkdir -p build/compiled/aot-loading
fpc @config.cfg -FUbuild/compiled/aot-loading -O4 -dPRODUCTION -Xs -CX -XX -B benchmarks/native/AotLoading.pas
./build/AotLoading
```

On Windows, run `build/AotLoading.exe`. Use the same compiler and optimization
flags for both paths; the one executable measures both.

The source measurement includes lexing, parsing, compilation and declaration
plan extraction. The buffer measurement includes the public
`LoadModuleFromBuffer` API, verification, profile comparison and declaration
plan materialization. Serialization runs once outside the timed region.
Neither path includes engine setup, dependency resolution, body execution or
cleanup. The source text and artifact are already in memory, so these numbers
measure front-end work rather than disk-cache effects.

The benchmark fails if the loaded profile differs. Behavioral equivalence is
checked separately by `bun scripts/test-cli-aot.ts` (also run by the CLI CI
suite), including smol-toml, module cycles, mixed source/bytecode graphs and a
large generated library. Memory-buffer ownership and engine registration are
covered by `Goccia.Engine.Bytecode.Test` and `Goccia.Bytecode.Profile.Test`.
