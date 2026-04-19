# Plan: IL Dump Tool + AGENTS.md Updates + CastClassArray Fix Investigation

## Context

We're investigating why `CastClassArray.cs` fails with "bad generics" at `Intrinsics.fs:640`. To understand the exact IL emitted for `Array.get_Length`, we need an IL disassembly tool. The `dotnet-ildasm` NuGet tool targets netcoreapp3.0 and won't run under our .NET 9 SDK. Since PawPrint already has full IL reading infrastructure (`Assembly.read`, `IlOp.Format`, etc.), we'll write a small in-repo tool instead.

## Step 1: Create `WoofWare.PawPrint.IlDump` project

A minimal F# console app that takes a DLL path + optional type/method filter and prints IL for matching methods.

**New file:** `WoofWare.PawPrint.IlDump/WoofWare.PawPrint.IlDump.fsproj`
- Target `net9.0`, OutputType `Exe`
- Reference only `WoofWare.PawPrint.Domain` (that's where `Assembly.read`, `IlOp`, `MethodInfo`, `TypeInfo` live)
- Reference `Microsoft.Extensions.Logging.Console` for the logger factory that `Assembly.read` requires

**New file:** `WoofWare.PawPrint.IlDump/Program.fs`

Usage: `dotnet run --project WoofWare.PawPrint.IlDump -- <dll-path> [TypeName] [MethodName]`

Implementation:
1. Parse args: DLL path (required), optional type name filter, optional method name filter
2. Open the DLL as a `FileStream`, call `Assembly.read loggerFactory (Some path) stream`
3. Iterate `assembly.TypeDefs` (an `IReadOnlyDictionary<TypeDefinitionHandle, TypeInfo<...>>`)
4. For each type, filter by name if provided (substring match on `typeInfo.Name`)
5. For each method in `typeInfo.Methods`, filter by name if provided (substring match on `method.Name`)
6. Print: type name, method name, signature info, then each IL instruction via `IlOp.Format`

Key APIs to use:
- `Assembly.read` from `WoofWare.PawPrint.Domain/Assembly.fs:316`
- `TypeInfo.Methods` from `WoofWare.PawPrint.Domain/TypeInfo.fs:51`
- `MethodInfo.Instructions` — `Some { Instructions = (IlOp * int) list }` or `None` for native methods
- `IlOp.Format : IlOp -> int -> string` from `WoofWare.PawPrint.Domain/IlOp.fs:669`

## Step 2: Add to solution

Add the new project to `WoofWare.PawPrint.slnx`.

## Step 3: Update AGENTS.md

Add notes about:
- The solution file is `WoofWare.PawPrint.slnx` (slnx format, not sln)
- The IL dump tool is available: `nix develop -c dotnet run --project WoofWare.PawPrint.IlDump -- <dll> [TypeName] [MethodName]`

## Step 4: Build and test

- Build: `nix develop -c dotnet build WoofWare.PawPrint.IlDump/`
- Test on System.Private.CoreLib: `nix develop -c dotnet run --project WoofWare.PawPrint.IlDump -- /nix/store/.../System.Private.CoreLib.dll Array get_Length`
- Verify it dumps the IL for `Array.get_Length` showing the `Unsafe.As<RawArrayData>` call

## Step 5: Format

Run `nix develop -c dotnet fantomas .` to format the new code.

## Verification

1. `nix develop -c dotnet build` — entire solution builds
2. Run the IL dump tool against System.Private.CoreLib to view `Array.get_Length` IL
3. Run existing tests to verify no regressions: `nix develop -c dotnet test WoofWare.PawPrint.Test/ --verbosity normal`
