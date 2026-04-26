---
name: pawprint-performance-benchmark
description: Run and interpret the WoofWare.PawPrint BenchmarkDotNet performance harness. Use when measuring PawPrint interpreter performance, establishing allocation/time baselines, comparing before/after optimization work, or explaining how to run the repo's `WoofWare.PawPrint.Performance` benchmarks.
---

# Pawprint Performance Benchmark

## Workflow

Use the repo-local BenchmarkDotNet project for PawPrint performance baselines:

```bash
nix develop -c dotnet run --project WoofWare.PawPrint.Performance/WoofWare.PawPrint.Performance.fsproj -c Release -- --filter "*StackHeavy*"
```

For a quick smoke check of the benchmark wiring, use BDN's dry job:

```bash
nix develop -c dotnet run --project WoofWare.PawPrint.Performance/WoofWare.PawPrint.Performance.fsproj -c Release -- --filter "*StackHeavy*" --job Dry
```

Treat `--job Dry` output only as a path check. Use the non-dry command for before/after numbers.

## Interpretation

The benchmark compiles a stack-heavy C# guest program in `GlobalSetup`, verifies PawPrint's exit code against the real .NET runtime, then measures `Program.run` with `MemoryDiagnoser`.

Compare the BenchmarkDotNet summary columns most relevant to interpreter-state work:

- `Mean`: elapsed time per PawPrint run.
- `Allocated`: managed allocation per PawPrint run.
- `Gen0`/`Gen1`/`Gen2`: collection pressure.

BenchmarkDotNet writes reports under `BenchmarkDotNet.Artifacts/`; the repo ignores that directory.
