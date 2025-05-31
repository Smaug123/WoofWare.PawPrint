# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

WoofWare.PawPrint is an experimental .NET runtime implementation written in F#. It's an IL interpreter designed to be:
- Fully deterministic (supporting time-travel debugging and fuzzing over thread execution order)
- Fully managed (reimplementing P/Invoke methods to avoid native code)
- Fully in-memory except for explicit filesystem operations

This is NOT a high-performance runtime - it's a very slow IL interpreter prioritizing determinism over speed.

## Common Commands

### Building
```bash
# Build the entire solution
dotnet build

# Build a specific project
dotnet build WoofWare.PawPrint/WoofWare.PawPrint.fsproj
```

### Testing
```bash
# Run all tests
dotnet test

# Run tests for a specific project
dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj

# Run a specific test
dotnet test --filter "FullyQualifiedName~TestCases"
```

### Formatting
```bash
# Format F# code using Fantomas
dotnet tool restore
dotnet fantomas .
```

### Running the Application
```bash
dotnet publish --self-contained --runtime-id osx-arm64 CSharpExample/ && dotnet run --project WoofWare.PawPrint.App/WoofWare.PawPrint.App.fsproj -- CSharpExample/bin/Debug/net9.0/osx-arm64/publish/CSharpExample.dll
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

**WoofWare.PawPrint.Test**
- Uses NUnit as the test framework
- Test cases are defined in `TestCases.fs`
- C# source files in `sources/` are compiled and executed by the runtime as test cases
- `TestHarness.fs` provides infrastructure for running test assemblies through the interpreter

**WoofWare.PawPrint.App**
- Entry point application for running the interpreter

### Key Design Patterns

1. **Immutable State**: The interpreter uses immutable F# records for all state, with state transitions returning new state objects
2. **Assembly Loading**: Assemblies are loaded on-demand as types are referenced
3. **Thread Management**: Each thread has its own execution state, managed through the `IlMachineState`
4. **Type Initialization**: Classes are initialized lazily when first accessed, following .NET semantics

### Code style

* Functions should be fully type-annotated, to give the most helpful error messages on type mismatches.
* Generally, prefer to fully-qualify discriminated union cases in `match` statements.
* When writing a "TODO" `failwith`, specify in the error message what the condition is that triggers the failure, so that a failing run can easily be traced back to its cause.

### Development Workflow

When adding new IL instruction support:
1. Add the instruction to `IlOp.fs`
2. Implement execution logic in `AbstractMachine.fs`
3. Add a test case in `sources/` (C# file) that exercises the instruction
4. Add the test case to `TestCases.fs`
5. Run tests to verify implementation

The project uses deterministic builds and treats warnings as errors to maintain code quality.
It strongly prefers to avoid special-casing to get around problems, but instead to implement general correct solutions; cases where this has failed to happen are considered to be tech debt and at some point in the future we'll be cleaning them up.

### Common Gotchas

* I've named several types in such a way as to overlap with built-in types, e.g. MethodInfo is in both WoofWare.PawPrint and System.Reflection.Metadata namespaces. Build errors can usually be fixed by fully-qualifying the type.

## In-Progress Work

### Exception Handling Implementation

**Current branch**: `prepare-for-exceptions`

**Summary**: Initial implementation of CLI exception handling including throw, catch, finally, and leave instructions.

**What's been done**:
- Added exception data structures (`CliException`, `ExceptionContinuation`, `StackFrame`)
- Implemented `leave` and `leave_s` instructions with finally block detection
- Basic `throw` implementation that searches for handlers in current method
- `endfinally` instruction to resume after finally blocks
- Tracking of active exception regions during method execution

**What still needs to be done**:
1. **Type compatibility checking**: Resolve `MetadataToken` in catch blocks and implement proper type assignability checks
2. **Stack unwinding**: When no handler is found in current method, need to:
   - Pop the current method state
   - Build stack trace during unwinding
   - Continue exception search in caller methods
3. **Filter clause support**: Implement filter evaluation (push exception, jump to filter, check boolean result)
4. **Fault blocks**: Implement fault handlers that only execute during exception unwinding
5. **Rethrow instruction**: Track current exception context to enable rethrowing
6. **Multiple finally blocks**: Handle cases where multiple finally blocks need to execute during unwinding
7. **Exception object field access**: Extract Message and other properties from standard exception types

**Key design decisions**:
- Exceptions are stored as heap object references, not extracted data
- Control flow managed through `ExceptionContinuation` discriminated union
- Stack traces built incrementally during unwinding
- Active exception regions tracked per method state
