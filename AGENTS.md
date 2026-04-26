# WoofWare.PawPrint

WoofWare.PawPrint is an experimental .NET runtime implementation written in F#. It's an IL interpreter designed to be:
- Fully deterministic (supporting time-travel debugging and fuzzing over thread execution order)
- Fully managed (reimplementing P/Invoke methods to avoid native code)
- Fully in-memory except for explicit filesystem operations

This is NOT a high-performance runtime - it's a very slow IL interpreter prioritizing determinism over speed.

You should find the genuine .NET runtime's source checked out at ../dotnet if you need to check behaviour; shout if it's not.

Standard `dotnet` toolchain is provided by the Nix devshell. Run `dotnet` commands as `nix develop -c dotnet ...` rather than invoking `dotnet` directly.

After changes, `nix develop -c dotnet fantomas .` to format.

The solution file is `WoofWare.PawPrint.slnx` (slnx format).

### Running the Application
A playground C# file is in CSharpExample/Class1.cs.
This environment is convenient for running WoofWare.PawPrint against a standalone DLL.
Interpolate the appropriate platform/config strings as necessary.

```bash
nix develop -c dotnet publish --self-contained --configuration Release --runtime osx-arm64 CSharpExample/
nix develop -c dotnet run --project WoofWare.PawPrint.App/WoofWare.PawPrint.App.fsproj -- CSharpExample/bin/Release/net9.0/osx-arm64/publish/CSharpExample.dll
```

## Architecture

### Core Components

**WoofWare.PawPrint** (Main Library)
- `AbstractMachine.fs`: Core IL interpreter execution engine, knitting together `UnaryConstIlOp.fs`, `UnaryMetadataIlOp.fs`, `UnaryStringTokenIlOp.fs`, and `NullaryIlOp.fs`
- `IlMachineState.fs`: Manages the complete state of the abstract machine
- `MethodState.fs`: Tracks execution state of individual methods
- `ManagedHeap.fs`: Implements the managed memory model
- `Assembly.fs`: Handles reading and parsing .NET assemblies
- `TypeInfo.fs`, `TypeDefn.fs`, `TypeRef.fs`: Type system implementation
- `IlOp.fs`: IL instruction definitions and munging
- `EvalStack.fs`: Evaluation stack implementation
- `Corelib.fs`: Core library type definitions (String, Array, etc.)
- `ExternImplementations/` and `NativeImpls.fs`: the boundary for runtime-provided or host-provided behavior; prefer extending this seam over special-casing host effects elsewhere in the interpreter

**WoofWare.PawPrint.Test**
- Uses NUnit as the test framework
- Test cases are defined in `TestPureCases.fs` and `TestImpureCases.fs`
- C# source files in `sources{Pure,Impure}/` are compiled and executed by the runtime as test cases; files in `sourcesPure` are automatically turned into test cases with no further action (see TestPureCases.fs for the mechanism), while `sourcesImpure` tests must be explicitly registered
- The `unimplemented` set of test files that are not yet expected to pass lives in `WoofWare.PawPrint.Test/TestPureCases.fs` (look for `let unimplemented =` near the top of the `TestPureCases` module); there's a sibling `unimplementedMockTests` map in the same file for unimplemented tests that also need mock registrations
- `TestHarness.fs` provides infrastructure for running test assemblies through the interpreter
- Run all tests with `nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --verbosity normal`
- Run a filtered subset with `nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --no-build --filter "Name~TypeRef" --verbosity normal`
- List adapter-discovered tests with `nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --list-tests`
- The `dotnet run`-based runner (`dotnet run --project ... -- --filter-test-case Foo --no-spinner`) may produce no visible output in non-interactive shells; prefer `dotnet test` with `--filter "Name~..."` instead

**WoofWare.PawPrint.App**
- Entry point application for running the interpreter

**WoofWare.PawPrint.IlDump**
- Small CLI tool for disassembling IL from .NET assemblies, using the same assembly-reading infrastructure as the interpreter
- Usage: `nix develop -c dotnet run --project WoofWare.PawPrint.IlDump -- <dll-path> [TypeName] [MethodName]`
- Filters are case-insensitive substring matches

### Key Design Patterns

1. **Immutable State**: The interpreter uses immutable F# records for all state, with state transitions returning new state objects
2. **Assembly Loading**: Assemblies are loaded on-demand as types are referenced
3. **Thread Management**: Each thread has its own execution state, managed through the `IlMachineState`
4. **Type Initialization**: Classes are initialized lazily when first accessed, following .NET semantics

### Target Frameworks

- `WoofWare.PawPrint` and `WoofWare.PawPrint.Domain` intentionally target `net8.0` for compatibility with future consumers
- `WoofWare.PawPrint.App`, `WoofWare.PawPrint.Test`, and playground/example executables target `net9.0`
- When diagnosing build/runtime issues, keep the cross-target split in mind; it is deliberate, not drift

### Code style

* Functions should be fully type-annotated, to give the most helpful error messages on type mismatches.
* Generally, prefer to fully-qualify discriminated union cases in `match` statements.
* ALWAYS fully-qualify enum cases when constructing them and matching on them (e.g., `PrimitiveType.Int16` not `Int16`).
* When writing a "TODO" `failwith`, specify in the error message what the condition is that triggers the failure, so that a failing run can easily be traced back to its cause.
* If a field name begins with an underscore (like `_LoadedAssemblies`), do not mutate it directly. Only mutate it via whatever intermediate methods have been defined for that purpose (like `WithLoadedAssembly`).
* Recall that in F#, compilation order matters: new functions must go after their dependencies, and later files can only depend on earlier ones from the `.fsproj`.

### Architecture guidelines

* When a lookup fails because a value is not represented in that index, do not broaden the lookup to return a related value. Instead, keep lookup helpers honest: they should return exactly what the index contains, or `None`/an error. Hew tightly to the domain: don't mix concerns, but instead transform canonical data into the right form.
  * For example, preserve the distinction between identity and view/projection. Prefer making walks total rather than adding projection helpers. If a traversal over runtime types fails at a structural/synthetic handle, teach the traversal how to step through the appropriate relationship; do not coerce the handle into a different identity just to reuse metadata code.

### Development Workflow

Use the `/implement-il-instruction` skill when adding support for a new IL opcode.

The project uses deterministic builds and treats warnings as errors to maintain code quality.
It strongly prefers to avoid special-casing to get around problems, but instead to implement general correct solutions; cases where this has failed to happen are considered to be tech debt and at some point in the future we'll be cleaning them up.

When managed BCL code fails because it reaches a runtime intrinsic, InternalCall, P/Invoke, or other host-provided primitive, implement the primitive boundary itself rather than mocking or replacing a higher-level managed method that happens to call it.
For example, add a manual implementation of `System.Type.get_IsGenericType` if `Marshal.SizeOf` needs it; do not mock out `Marshal.SizeOf` just to get past that call path.

You will often find that "obvious" end-to-end tests will fail for annoying reasons, like "something deep in the BCL is calling out to unmanaged code" or "we haven't yet implemented some apparently-unrelated IL opcode".
If this happens, the right strategy is to make incremental progress only: don't head down the rabbithole, but instead try writing a test that specifically captures only what you've just improved, without needing extra implementation work.
We really want to keep changes in small, reviewable chunks; leaving tests in the `unimplemented` category is fine if they're not yet passing, because it means we won't forget about them.
If you find you really do need to implement a dependency, please consider whether we can implement the dependency *first*, getting that PR'ed into main before continuing, because that's greatly preferable; either way, stop and ask me what to do, because I never intend you to implement more than one feature at once.

### Common Gotchas

* I've named several types in such a way as to overlap with built-in types, e.g. MethodInfo is in both WoofWare.PawPrint and System.Reflection.Metadata namespaces. Build errors can usually be fixed by fully-qualifying the type.
* Files under `ExternImplementations/Generated*.fs` are generated by Myriad. Edit the corresponding handwritten interface/source file instead, and let regeneration update the generated file.

## Hosted Type System

For detailed guidance on type concretization, generic resolution, and common patterns in the emulated CLR type system, see .claude/commands/type-concretization.md .

## Instructions for OpenAI Codex agents specifically

When you've completed a change to the point where you think it can be PR'ed, please commit it.
Then invoke Claude for a review: `claude --effort max --print "Please review this branch against main. The branch intends to..."` (for example).
This will take many minutes.
Once Claude has replied, address any of its feedback that you think is correct and worth addressing, then repeat if you made changes.

(end of Codex-specific instructions)
