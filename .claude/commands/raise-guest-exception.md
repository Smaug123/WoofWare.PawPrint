# How to raise a guest exception in the PawPrint runtime

This skill describes how to make the PawPrint IL interpreter throw an exception *inside the guest program* (i.e. a managed .NET exception that the guest's own `try`/`catch` can handle), as opposed to a host `failwith` that crashes the interpreter.

## Overview

There are two tiers of complexity:

1. **You already have an exception object on the managed heap** (e.g. a cached `TypeInitializationException`). This is the simple case.
2. **You need to construct a new exception object** (e.g. `NullReferenceException`, `ArgumentException`). This requires allocating and initialising an object first.

Most call sites in the codebase today still use `failwith "TODO: throw SomeException"` because tier 2 hasn't been generalised yet. When you encounter one of those, upgrading it is welcome but not required for every change.

## Tier 1: Dispatching an existing exception object

If you already have a `ManagedHeapAddress` for the exception object and its `ConcreteTypeHandle`:

```fsharp
match
    ExceptionDispatching.throwExceptionObject
        loggerFactory
        baseClassTypes   // BaseClassTypes<DumpedAssembly>
        state            // IlMachineState
        currentThread    // ThreadId
        exceptionAddr    // ManagedHeapAddress
        exceptionType    // ConcreteTypeHandle
with
| ExceptionDispatchResult.HandlerFound state ->
    // The guest has a handler; execution will resume there.
    // Return `state` (and appropriate WhatWeDid/ExecutionResult).
| ExceptionDispatchResult.ExceptionUnhandled (state, exn) ->
    // No handler anywhere on the call stack.
    // Propagate as ExecutionResult.UnhandledException or similar.
```

**Where this is used today:** `IlMachineStateExecution.loadClass` (line ~524) re-throws a cached `TypeInitializationException` when a `.cctor` previously failed. The `NullaryIlOp.fs` `Throw` opcode implementation (line ~818) also follows this pattern.

## Tier 2: Constructing and throwing a new exception object

Use `IlMachineStateExecution.raiseManagedException`. It takes a `TypeInfo`, e.g. as from `BaseClassTypes` (such as `baseClassTypes.NullReferenceException`) and returns `IlMachineState * WhatWeDid`. Do NOT advance the program counter after calling it — exception dispatch uses the faulting instruction's PC to determine which exception handler regions are active and to build the stack trace.

```fsharp
IlMachineStateExecution.raiseManagedException
    loggerFactory
    baseClassTypes
    baseClassTypes.NullReferenceException
    currentThread
    state
```

Many common exception types are fields on `BaseClassTypes` (see `Corelib.fs`):
`NullReferenceException`, `IndexOutOfRangeException`, `InvalidCastException`, `OverflowException`, `DivideByZeroException`, `MissingFieldException`, `MissingMethodException`, `OutOfMemoryException`, etc.

## Return type considerations

Where you raise an exception affects what you return:

- **In `Intrinsics.call`** (returns `IlMachineState option`): Return `Some state` after dispatch.
- **In `NullaryIlOp.execute`** (returns `ExecutionResult`): Return `ExecutionResult.Stepped (state, WhatWeDid.Executed)` for `HandlerFound`, or `ExecutionResult.UnhandledException` for unhandled.
- **In `UnaryMetadataIlOp.execute`** (returns `IlMachineState * WhatWeDid`): Return `(state, WhatWeDid.Executed)` for `HandlerFound`. For unhandled, the pattern isn't established yet — this would need plumbing.

## Key files

- `ExceptionDispatching.fs` — `throwExceptionObject`, `dispatchException`, handler search
- `NullaryIlOp.fs` — `Throw` opcode, the canonical example of guest exception dispatch
- `IlMachineStateExecution.fs` — `loadClass`, cached TypeInitializationException rethrow example
- `Corelib.fs` — `BaseClassTypes` record with all pre-resolved exception types
- `UnaryMetadataIlOp.fs` — `Newobj`, pattern for allocating + constructing objects
