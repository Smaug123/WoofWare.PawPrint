---
name: raise-guest-exception
description: How to make the interpreter throw a managed exception inside the guest, catchable by the guest's own try/catch, rather than a host failwith that kills the run. Use when replacing a TODO-throw failwith, or when a modelled syscall must surface an exception to guest code.
---

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
| ExceptionDispatchResult.Dispatched state ->
    // The guest has a handler; execution will resume there.
    // Return `state` (and appropriate WhatWeDid/ExecutionResult).
| ExceptionDispatchResult.ExceptionUnhandled (state, exn) ->
    // No handler anywhere on the call stack.
    // Propagate as ExecutionResult.UnhandledException or similar.
```

**Where this is used today:** `IlMachineStateExecution.loadClass` (line ~524) re-throws a cached `TypeInitializationException` when a `.cctor` previously failed. The `NullaryIlOp.fs` `Throw` opcode implementation (line ~818) also follows this pattern.

## Tier 2: Constructing and throwing a new exception object

Use `IlMachineStateExecution.raiseRuntimeException`. It takes a `RaiseOrigin`, which says where the exception comes from, and the fields to write once the parameterless constructor has run (`[]` almost everywhere; `RuntimeExceptionField.Message` where CoreCLR passes a message), and returns `IlMachineState * WhatWeDid`. Do NOT advance the program counter after calling it — exception dispatch uses the faulting instruction's PC to determine which exception handler regions are active and to build the stack trace.

The origin is the part to get right, because it decides what the raise is checked against:

- `RaiseOrigin.InstructionFault fault` — performing the current instruction faults, as an `OpcodeFault` (a null receiver, an index out of range). `OpcodeFault.resolve` names the exception, and the raise refuses unless the instruction's `OpcodeFaults` entry lists the fault.
- `RaiseOrigin.InstructionResolution (category, exceptionType)` — resolving the current instruction's token fails in a `ResolutionFault` category: the token fails to bind, or the JIT refuses what it binds to (`TokenBinding`), a `DynamicMethod`'s scope entry is unusable (`ScopeEntry`), an access check fails, or dispatch finds no method. The raise refuses unless the entry lists the category and `ResolutionFault.typeNames` names `exceptionType` for it. Dispatch failures go through `raiseDispatchFailure`, which builds this origin.
- `RaiseOrigin.Callee exceptionType` — something the instruction handed control to raises it: a native method, an intrinsic, a delegate's constructor or invocation, an array accessor, an `UnsafeAccessor`, a method that cannot be entered. Nothing is checked, because `OpcodeFaults` says nothing of callees. A native handler returns `NativeHandlerResult.RaiseException` instead, which the dispatcher raises as a callee's.

If the check refuses, either the origin is wrong (often: a callee's failure raised as the instruction's) or the table is missing an entry. Decide which from CoreCLR's source or a measurement; don't widen the origin to make the check pass.

This entry point is intended for exceptions the runtime itself synthesises (e.g. `NullReferenceException` from a null deref, `InvalidCastException` from a failed `castclass`). It deliberately skips the exception type's `.cctor`, which is safe for the BCL exception types but not for arbitrary guest-defined types — so don't use it to dispatch guest-thrown exceptions. Those go through Tier 1 (`throwExceptionObject`) after the guest's own `newobj` has already run the cctor.

```fsharp
IlMachineStateExecution.raiseRuntimeException
    loggerFactory
    baseClassTypes
    (IlMachineStateExecution.RaiseOrigin.InstructionFault OpcodeFault.NullReference)
    []
    currentThread
    state
```

Many common exception types are fields on `BaseClassTypes` (see `Corelib.fs`):
`NullReferenceException`, `IndexOutOfRangeException`, `InvalidCastException`, `OverflowException`, `DivideByZeroException`, `MissingFieldException`, `MissingMethodException`, `OutOfMemoryException`, etc.

## Return type considerations

Where you raise an exception affects what you return:

- **In `Intrinsics.call`** (returns `IlMachineState option`): Return `Some state` after dispatch.
- **In `NullaryIlOp.execute`** (returns `ExecutionResult`): Return `ExecutionResult.Stepped (state, WhatWeDid.Executed)` for `Dispatched`, or `ExecutionResult.UnhandledException` for unhandled.
- **In `UnaryMetadataIlOp.execute`** (returns `IlMachineState * WhatWeDid`): Return `(state, WhatWeDid.Executed)` for `Dispatched`, or `(state, WhatWeDid.UnhandledException exn)` for unhandled. `AbstractMachine.surfaceTerminatingStep` turns the latter into `ExecutionResult.UnhandledException` at the exit from `executeOneStep`, so the scheduler never sees it; the static-field opcodes rethrowing a cached `TypeInitializationException` are the worked example.
- **In a native handler** (returns `NativeHandlerResult`): `NativeHandlerResult.tryEarlyReturn` already forwards `WhatWeDid.UnhandledException` from a managed sub-call as `NativeHandlerResult.Terminating`; if you match on the `WhatWeDid` yourself, `NativeHandlerResult.unhandledException` builds that value.

## Key files

- `ExceptionDispatching.fs` — `throwExceptionObject`, `dispatchException`, handler search
- `NullaryIlOp.fs` — `Throw` opcode, the canonical example of guest exception dispatch
- `IlMachineStateExecution.fs` — `loadClass`, cached TypeInitializationException rethrow example
- `Corelib.fs` — `BaseClassTypes` record with all pre-resolved exception types
- `UnaryMetadataIlOp.fs` — `Newobj`, pattern for allocating + constructing objects
