Implement this plan with each stage on its own branch, stacked as necessary on previous branches, so that a reviewer can review each branch in isolation.

## Stage 1: F# test cases project skeleton

**Dependencies**: None

**Implements**: DESIGN §1 (Project structure)

Create the project and build infrastructure:

1. Create `WoofWare.PawPrint.Test.FSharpPureCases/WoofWare.PawPrint.Test.FSharpPureCases.fsproj`:
   - Exe, net9.0
   - A trivial `Main.fs` with an `[<EntryPoint>] let main` that dispatches on `argv.[0]`
   - A single placeholder test module `Placeholder.fs` whose `main` returns 0
   - The dispatcher routes `"Placeholder"` to it
2. Add the project to `WoofWare.PawPrint.slnx`.
3. Verify that `nix develop -c dotnet publish --self-contained --configuration Release --runtime osx-arm64 WoofWare.PawPrint.Test.FSharpPureCases/` succeeds and the output directory contains both the app DLL and FSharp.Core.dll.

**Correctness oracle**:
- `dotnet build` of the full solution succeeds
- `dotnet publish --self-contained` produces output containing both the main DLL and FSharp.Core.dll
- Running the published exe with `Placeholder` as argv produces exit code 0

---

## Stage 2: Test harness for F# cases

**Dependencies**: Stage 1

**Implements**: DESIGN §2 (Test harness), §4 (Unimplemented support)

Add `TestFSharpPureCases.fs` to the test project:

1. Locate the published FSharpPureCases DLL. The path can be constructed from the known project layout and RID: `WoofWare.PawPrint.Test.FSharpPureCases/bin/Release/net9.0/{rid}/publish/WoofWare.PawPrint.Test.FSharpPureCases.dll`. If the file doesn't exist, skip with `Assert.Inconclusive("F# test cases not published; run dotnet publish first")`.
2. Read the DLL bytes; construct `dotnetRuntimeDirs` that includes both the standard runtime dirs (from `DotnetRuntime.SelectForDll`) and the publish output directory (for FSharp.Core.dll).
3. For each test case name in a `testCases` list:
   - Run through `RealRuntime.executeWithRealRuntime [| testCaseName |] image`
   - Run through `Program.run ... [testCaseName]`
   - Assert both return exit code 0 (or a custom expected code)
4. Support an `unimplemented` set: these run against the real runtime only and are `Assert.Inconclusive` for PawPrint.
5. Wire up `"Placeholder"` as the first test case.

**Correctness oracle**:
- `dotnet test --filter "Name~FSharpPure"` discovers and passes the Placeholder test
- The Placeholder test exercises the full path: loads the DLL, runs through PawPrint, runs through real runtime, compares exit codes

---

## Stage 3: BeqBranch inline IL test

**Dependencies**: Stage 2

**Implements**: DESIGN §3 (First test case)

Add `BeqBranch.fs` to the FSharpPureCases project:

1. Write helper functions using F# inline IL `(# ... #)` to emit `beq.s` for various operand type pairs (int32/int32, int64/int64, nativeint/nativeint, float/float, object/object).
2. The module's `main` function calls each helper and returns a nonzero error code on mismatch, 0 on success (same pattern as the C# tests).
3. Add `"BeqBranch"` to the dispatcher in Main.fs and to the test case list in `TestFSharpPureCases.fs`.
4. If PawPrint cannot yet run the test (e.g. FSharp.Core resolution issues, missing opcodes), add it to `unimplemented` so it's tracked but doesn't block CI. In this case, verify the real-runtime oracle passes (confirming the test itself is correct), and note what's missing.

**Correctness oracle**:
- The real runtime executes BeqBranch and returns 0
- If PawPrint can execute it: PawPrint also returns 0
- If not yet: test is in `unimplemented`, real-runtime-only check passes, and the blocking issue is documented
