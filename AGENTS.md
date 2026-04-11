# WoofWare.PawPrint

WoofWare.PawPrint is an experimental .NET runtime implementation written in F#. It's an IL interpreter designed to be:
- Fully deterministic (supporting time-travel debugging and fuzzing over thread execution order)
- Fully managed (reimplementing P/Invoke methods to avoid native code)
- Fully in-memory except for explicit filesystem operations

This is NOT a high-performance runtime - it's a very slow IL interpreter prioritizing determinism over speed.

Standard `dotnet` toolchain is provided by the Nix devshell. Run `dotnet` commands as `nix develop -c dotnet ...` rather than invoking `dotnet` directly.

After changes, `nix develop -c dotnet fantomas .` to format.

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
- C# source files in `sources{Pure,Impure}/` are compiled and executed by the runtime as test cases; files in `sourcesPure` are automatically turned into test cases with no further action (see TestPureCases.fs for the mechanism)
- `TestHarness.fs` provides infrastructure for running test assemblies through the interpreter
- Run all tests with `nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --verbosity normal`
- Run a filtered subset with `nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --no-build --filter "Name~TypeRef" --verbosity normal`
- List adapter-discovered tests with `nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --list-tests`
- The `dotnet run`-based runner (`dotnet run --project ... -- --filter-test-case Foo --no-spinner`) may produce no visible output in non-interactive shells; prefer `dotnet test` with `--filter "Name~..."` instead

**WoofWare.PawPrint.App**
- Entry point application for running the interpreter

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

### Development Workflow

When adding new IL instruction support:
1. Add the instruction to `IlOp.fs`
2. Implement execution logic in `AbstractMachine.fs`
3. Add a test case in `sourcesPure/` or `sourcesImpure/` (C# file) that exercises the instruction
4. Pure cases in `sourcesPure/` are auto-discovered; impure cases in `sourcesImpure/` must also be added to `TestImpureCases.fs`. The `.fsproj` already includes both directories as wildcard `EmbeddedResource`s.
5. Run tests to verify implementation, using `nix develop -c dotnet ...`

The project uses deterministic builds and treats warnings as errors to maintain code quality.
It strongly prefers to avoid special-casing to get around problems, but instead to implement general correct solutions; cases where this has failed to happen are considered to be tech debt and at some point in the future we'll be cleaning them up.

### Common Gotchas

* I've named several types in such a way as to overlap with built-in types, e.g. MethodInfo is in both WoofWare.PawPrint and System.Reflection.Metadata namespaces. Build errors can usually be fixed by fully-qualifying the type.
* Files under `ExternImplementations/Generated*.fs` are generated by Myriad. Edit the corresponding handwritten interface/source file instead, and let regeneration update the generated file.

## Type Concretization System

### Overview

Type concretization converts abstract type definitions (`TypeDefn`) to concrete runtime types (`ConcreteTypeHandle`). This is essential because IL operations need exact types at runtime, including all generic instantiations. The system separates type concretization from IL execution, ensuring types are properly loaded before use.

### Key Concepts

#### Generic Parameters
- **Common error**: "Generic type/method parameter X out of range" probably means you're missing the proper generic context: some caller has passed the wrong list of generics through somewhere.

#### Assembly Context
TypeRefs must be resolved in the context of the assembly where they're defined, not where they're used. When resolving a TypeRef, always use the assembly that contains the TypeRef in its metadata.

### Common Scenarios and Solutions

#### Nested Generic Contexts
When inside `Array.Empty<T>()` calling `AsRef<T>`, the `T` refers to the outer method's generic parameter. Pass the current executing method's generics as context:
```fsharp
let currentMethod = state.ThreadState.[thread].MethodState.ExecutingMethod
concretizeMethodWithTypeGenerics ... currentMethod.Generics state
```

#### Field Access in Generic Contexts
When accessing `EmptyArray<T>.Value` from within `Array.Empty<T>()`, use both type and method generics:
```fsharp
let contextTypeGenerics = currentMethod.DeclaringType.Generics
let contextMethodGenerics = currentMethod.Generics
```

#### Call vs CallMethod
- `callMethodInActiveAssembly` expects unconcretized methods and does concretization internally
- `callMethod` expects already-concretized methods
- The refactoring changed to concretizing before calling to ensure types are loaded

### Common Pitfalls

1. **Don't create new generic parameters when they already exist**. It's *very rarely* correct to instantiate `TypeDefn.Generic{Type,Method}Parameter` yourself:
   ```fsharp
   // Wrong: field.DeclaringType.Generics |> List.mapi (fun i _ -> TypeDefn.GenericTypeParameter i)
   // Right: field.DeclaringType.Generics
   ```

2. **Assembly loading context**: The `loadAssembly` function expects the assembly that contains the reference as the first parameter, not the target assembly

3. **Type forwarding**: Use `Assembly.resolveTypeRef` which handles type forwarding and exported types correctly

### Key Files for Type System

- **TypeConcretisation.fs**: Core type concretization logic
  - `concretizeType`: Main entry point
  - `concretizeGenericInstantiation`: Handles generic instantiations like `List<T>`
  - `ConcretizationContext`: Tracks state during concretization

- **IlMachineState.fs**:
  - `concretizeMethodForExecution`: Prepares methods for execution
  - `concretizeFieldForExecution`: Prepares fields for access
  - Manages the flow of generic contexts through execution

- **Assembly.fs**:
  - `resolveTypeRef`: Resolves type references across assemblies
  - `resolveTopLevelTypeFromName`: Discovers top-level types (by namespace + name), falling back to exported types
  - `resolveTypeFromExport`: Follows type forwarding chains

### Debugging Type Concretization Issues

When encountering errors:
1. Check the generic context (method name, generic parameters)
2. Verify the assembly context being used
3. Identify the TypeDefn variant being concretized
4. Add logging to see generic contexts: `failwithf "Failed to concretize: %A" typeDefn`
5. Check if you're in a generic method calling another generic method
6. Verify TypeRefs are being resolved in the correct assembly

## Common Type System Patterns

### Creating TypeDefn from Type Metadata

When you need to create a `TypeDefn` from type metadata (e.g., from a `TypeInfo`), there's a common pattern that involves:
1. Resolving the base type to determine `SignatureTypeKind`
2. Creating the base `TypeDefn.FromDefinition`
3. For generic types, creating a `GenericInstantiation` with type parameters

This pattern is implemented in `UnaryMetadataIlOp.lookupTypeDefn`. Example usage:
```fsharp
let state, typeDefn =
    UnaryMetadataIlOp.lookupTypeDefn
        baseClassTypes
        state
        activeAssembly
        typeDefHandle
```

### Field Signature Comparison in Generic Contexts

When comparing field signatures in generic contexts (e.g., when resolving member references), signatures must be concretized before comparison. This ensures generic type parameters are properly substituted:

```fsharp
// Concretize both signatures before comparing
let state, concreteFieldSig = concretizeType ... fieldSig
let state, fieldSigConcrete = concretizeType ... fi.Signature
if fieldSigConcrete = concreteFieldSig then ...
```

### Static vs Instance Fields

When constructing objects with `Newobj`:
- Only instance fields should be included in the object
- Static fields belong to the type, not instances
- Filter using: `field.Attributes.HasFlag FieldAttributes.Static`

Example:
```fsharp
let instanceFields =
    ctorType.Fields
    |> List.filter (fun field -> not (field.Attributes.HasFlag FieldAttributes.Static))
```
