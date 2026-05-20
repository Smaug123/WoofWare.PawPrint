# F# Test Cases Infrastructure

## Problem

The test suite compiles C# source files via Roslyn at test time and runs them through PawPrint.
This works well, but C# cannot emit arbitrary IL — Roslyn decides which branch instructions, comparison patterns, etc. to generate.
F# supports inline IL via the `(# ... #)` syntax, which would let us write tests that exercise specific opcodes like `beq.s`.

More importantly, the ultimate goal of PawPrint is to run a specific F# application.
We need the ability to write and run F# test cases through the interpreter.

## Constraints

- **The F# compiler is very slow.** Runtime compilation via F# Compiler Services would significantly increase test time. Build-time compilation is strongly preferred.
- **FSharp.Core is not in the shared runtime.** It ships with the SDK, not in `Microsoft.NETCore.App`. A published F# app includes FSharp.Core in its output; the test harness must supply its directory to PawPrint's assembly loader.
- **PawPrint only runs entry points.** `Program.run` finds the assembly's `MainMethod` and executes it. There is no mechanism to call an arbitrary static method.
- **The real-runtime comparison pattern must be preserved.** Tests run through both PawPrint and the real .NET runtime, comparing exit codes for correctness.

## Design

### Approach: Build-time F# project with argv-based dispatch

A new F# executable project in the solution, compiled at build time by `dotnet build`.
Each test case is a module with a `main` function.
The project's entry point dispatches to the appropriate module based on `argv.[0]`.

```
WoofWare.PawPrint.Test.FSharpPureCases/
  WoofWare.PawPrint.Test.FSharpPureCases.fsproj   (Exe, net9.0, self-contained publish)
  BeqBranch.fs
  ... more test files ...
  Main.fs   (dispatcher entry point — must be last in compilation order)
```

Each test module follows a convention:

```fsharp
module BeqBranch

let main (argv : string array) : int =
    // ... test logic, returns 0 on success ...
    0
```

The dispatcher in `Main.fs`:

```fsharp
module Main

[<EntryPoint>]
let main (argv : string array) : int =
    match argv.[0] with
    | "BeqBranch" -> BeqBranch.main argv.[1..]
    | name -> failwith $"Unknown test case: {name}"
```

### Test harness integration

A new test fixture `TestFSharpPureCases.fs` in the test project:

1. **At test time**, reads the published DLL from a known path (the FSharpPureCases project's publish output directory).
2. **For each test case name**, runs the assembly through both:
   - `RealRuntime.executeWithRealRuntime [| testCaseName |] image` 
   - `Program.run ... [testCaseName]`
3. **Compares exit codes**, same as the C# tests.

The published output directory is needed (not just `bin/Debug`) because self-contained publish bundles FSharp.Core.dll alongside the app, and PawPrint's assembly loader needs that directory in its runtime search paths.

### FSharp.Core resolution

The self-contained publish output contains FSharp.Core.dll alongside the main assembly.
The test harness adds the publish output directory to the `dotnetRuntimeDirs` passed to `Program.run`, so PawPrint's assembly loader finds FSharp.Core.dll when resolving references.

### Test case discovery

Test cases are listed explicitly in the test fixture (like `requiresMocks` and `customExitCodes` in `TestPureCases.fs`). This avoids needing reflection over the FSharpPureCases assembly and keeps the test list reviewable. When adding a new F# test file, you add one line to the dispatcher and one line to the test case list.

### Why not F# Compiler Services?

FCS would mirror the Roslyn pattern exactly but:
- Compilation is much slower than Roslyn (~seconds per file vs ~milliseconds)
- FCS is a large dependency
- The build-time approach compiles once, incrementally, with full caching

### Why not a library with per-method invocation?

This would require changes to `Program.run` and `RealRuntime.executeWithRealRuntime` to support calling arbitrary methods.
That's a useful enhancement but orthogonal to the test infrastructure.
The dispatcher pattern works today with no interpreter changes.

## §1 Project structure

The F# project is a standalone Exe added to the slnx.
It targets net9.0 and is published self-contained for the current RID.
The test project does NOT have a ProjectReference to it (to avoid coupling the build), but the test expects it to be pre-built.

## §2 Test harness

`TestFSharpPureCases.fs` reads the published DLL, runs each test case through both runtimes, and compares results. It adds the publish directory to runtime dirs so FSharp.Core resolves.

## §3 First test case

`BeqBranch.fs` uses inline IL to emit `beq.s` and verifies correct branching for int32, int64, nativeint, float, object ref, and managed pointer operands.

## §4 Unimplemented / expected-failure support

Like the C# tests, maintain an `unimplemented` set of test case names that are known not to pass yet. These run against the real runtime only (to verify the test itself is correct) and are skipped for PawPrint execution.
