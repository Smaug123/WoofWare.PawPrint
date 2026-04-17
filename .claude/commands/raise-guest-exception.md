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

## Tier 2: Constructing a new exception object

To throw a *new* exception (e.g. `NullReferenceException`), you need to:

1. **Look up the exception type** from `BaseClassTypes`. All common exception types are already fields on the record (see `Corelib.fs`):
   - `baseClassTypes.NullReferenceException`
   - `baseClassTypes.IndexOutOfRangeException`
   - `baseClassTypes.InvalidCastException`
   - `baseClassTypes.OverflowException`
   - `baseClassTypes.DivideByZeroException`
   - `baseClassTypes.MissingFieldException`
   - `baseClassTypes.MissingMethodException`
   - `baseClassTypes.OutOfMemoryException`
   - etc.

2. **Concretize the type** to get a `ConcreteTypeHandle`, and compute its fields' zero values.

3. **Allocate the object** on the managed heap via `IlMachineState.allocateManagedObject` (or the lower-level `ManagedHeap.allocateNonArray`).

4. **Run the constructor** (`.ctor`) on the allocated object. Most exception types have a parameterless constructor. This would typically be done via `IlMachineStateExecution.callMethod` or `callMethodInActiveAssembly`, pushing the allocated object as `this`.

5. **Dispatch** using `ExceptionDispatching.throwExceptionObject` as in tier 1.

This is currently not factored into a reusable helper. If you need to implement this, the `Newobj` opcode implementation in `UnaryMetadataIlOp.fs` (around line 240) shows the full pattern for allocating + constructing an object. The steps are:
- Resolve the constructor method
- Compute zero values for all instance fields
- Allocate via `IlMachineState.allocateManagedObject`
- Push the object ref and call the constructor
- After construction completes, dispatch the exception

**A reusable `raiseNewException` helper would be very welcome** — it would replace dozens of `failwith "TODO: throw ..."` sites across the codebase.

## Return type considerations

Where you raise an exception affects what you return:

- **In `Intrinsics.call`** (returns `IlMachineState option`): Return `Some state` after dispatch.
- **In `NullaryIlOp.execute`** (returns `ExecutionResult`): Return `ExecutionResult.Stepped (state, WhatWeDid.Executed)` for `HandlerFound`, or `ExecutionResult.UnhandledException` for unhandled.
- **In `UnaryMetadataIlOp.execute`** (returns `IlMachineState * WhatWeDid`): Return `(state, WhatWeDid.Executed)` for `HandlerFound`. For unhandled, the pattern isn't established yet — this would need plumbing.

## Key files

- `ExceptionDispatching.fs` — `throwExceptionObject`, `dispatchException`, handler search
- `NullaryIlOp.fs` — `Throw` opcode (line ~818), the canonical example of guest exception dispatch
- `IlMachineStateExecution.fs` — `loadClass` (line ~520), cached TIE rethrow example
- `Corelib.fs` — `BaseClassTypes` record with all pre-resolved exception types
- `UnaryMetadataIlOp.fs` — `Newobj` (line ~240), pattern for allocating + constructing objects
