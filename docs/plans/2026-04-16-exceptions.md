# Plan: CLI Exception Handling

## Context

WoofWare.PawPrint needs complete CLI exception-handling semantics. Two broad classes of exception flow need to be supported:

1. **User-thrown exceptions** via the `throw`/`rethrow` IL opcodes, propagated through try/catch/filter/finally/fault regions.
2. **Runtime-synthesised exceptions** (NullReferenceException, IndexOutOfRangeException, DivideByZeroException, OverflowException, …) raised by ordinary opcodes (`ldfld`, `ldelem`, `div`, `callvirt`, …) when their preconditions fail.

The goal: get `ComplexTryCatch.cs` passing (currently `Assert.Inconclusive`) and replace ~37 `failwith "TODO: throw …"` sites with proper managed exception throws, so programs that rely on exceptions for control flow — which is most real .NET code — can run through the interpreter.

### What already exists

Scaffolding is further along than it looks. In particular:

- `WoofWare.PawPrint/Exceptions.fs` defines `CliException`, `ExceptionStackFrame`, `ExceptionContinuation` (with `ResumeAfterFinally | PropagatingException | ResumeAfterFilter` cases), plus `ExceptionHandling.findFinallyBlocksToRun` and `ExceptionHandling.getActiveRegionsAtOffset`.
- `WoofWare.PawPrint/ExceptionDispatching.fs` has `findExceptionHandler`, returning the first matching catch/finally/fault region in a given frame.
- `MethodInfo.fs:55-108` parses the exception handler table from `System.Reflection.Metadata.MethodBodyBlock.ExceptionRegions` into an `ExceptionRegion` DU (`Catch | Filter | Finally | Fault`) with `ExceptionOffset` (TryOffset/Length + HandlerOffset/Length).
- `MethodState.fs:14-48` stores per-frame `ActiveExceptionRegions` and `ExceptionContinuation`; `setProgramCounter` recomputes active regions on every PC change.
- `Corelib.fs:58-80` pre-resolves all the core exception types into `BaseClassTypes` (`NullReferenceException`, `IndexOutOfRangeException`, `DivideByZeroException`, `OverflowException`, `InvalidCastException`, …).
- `UnaryConstIlOp.fs:6-47` implements `leave`/`leave.s` including queueing finally blocks via `ExceptionContinuation.ResumeAfterFinally`.
- `NullaryIlOp.fs:768-811` implements `endfinally`'s `ResumeAfterFinally` arm (but not its `PropagatingException` arm).
- `NullaryIlOp.fs:813-894` implements a partial `throw` that works only when the handler is in the current frame and is a catch or finally.

### What is missing

1. **Cross-method unwinding.** `throw` at `NullaryIlOp.fs:894` fails with `"TODO: Implement stack unwinding when no handler in current method"`. Nothing today pops frames while searching for a handler.
2. **Endfinally continuing propagation.** `NullaryIlOp.fs:808` fails with `"TODO: Exception type lookup from heap address not yet implemented"`, though `heapObject.ConcreteType` is trivially available (same file, line 855, already uses it).
3. **Type-hierarchy catch matching.** `ExceptionDispatching.isExceptionAssignableTo` returns `state, true` unconditionally (`ExceptionDispatching.fs:11-21`). This makes the *first* catch in source order win regardless of type — wrong whenever multiple typed catches are present.
4. **Runtime-synthesised exceptions.** ~37 call sites throw via `failwith` (host exception) instead of raising a managed exception. No helper exists to allocate and raise one.
5. **Unhandled exceptions reaching the harness.** `ExecutionResult` has only `Terminated | Stepped`. A top-level unhandled exception has nowhere to go — `TestHarness.fs` has no way to observe that the program threw.
6. **`rethrow`, `endfilter`, fault handlers.** All `failwith "TODO"`.

## Approach

A single-frame throw that finds its handler is already handled. Everything else derives from two core pieces that belong together: (a) a general "begin/continue propagation" routine that unwinds frames as needed, running finally/fault blocks along the way; and (b) correct catch-type matching. With those in place, individual bug-fix-sized PRs can follow for each synthesised-exception site and each advanced IL opcode.

### Core design decision: propagation as state, not as a loop

Finally blocks must execute normal IL one instruction at a time between a throw and its ultimate handler. So unwinding can't be a single synchronous routine — it must thread through the dispatch loop.

The existing design already points in the right direction: `ExceptionContinuation.PropagatingException` on a frame says "this frame is currently unwinding; when its finally/fault returns, continue propagation." The missing piece is a single helper

```fsharp
IlMachineState.continuePropagation
    : BaseClassTypes -> CliException -> ThreadId -> IlMachineState -> IlMachineState
```

that, given an in-flight exception and the thread currently propagating it, does **exactly one** of the following, leaving the machine in a state the dispatch loop can resume:

1. Find a catch in the current frame that matches the exception type → clear eval stack, push the exception object, set PC to the handler start, clear `ExceptionContinuation`.
2. Find a finally/fault in the current frame enclosing the current PC → set PC to the handler start, set `ExceptionContinuation = PropagatingException exn`. The handler's terminating `endfinally`/`endfault` will re-enter `continuePropagation` with the same exception.
3. No handler in the current frame → append a stack frame to `exn.StackTrace`, pop the current frame (advancing `ActiveMethodState` to the caller), and recurse into case 1/2/3 on the caller.
4. No caller → the exception is unhandled; transition the thread to an "unhandled exception" terminal state observable by the dispatch loop.

Call sites:
- `throw` opcode in `NullaryIlOp.fs:813-894`: compute `CliException`, then call `continuePropagation` (instead of today's inline single-frame search).
- `endfinally`/`endfault` when `ExceptionContinuation = PropagatingException exn`: call `continuePropagation` with `exn` (replaces the TODO at `NullaryIlOp.fs:808`).
- A new `raiseManagedException baseClassTypes exceptionType thread state` helper used for runtime-synthesised exceptions.

This design keeps the dispatch loop unaware of exceptions — it just keeps stepping. Only the entry points (`throw`, `endfinally`, runtime raises) need to know about propagation.

### Unhandled exceptions

Add a third variant: `ExecutionResult.UnhandledException of IlMachineState * ThreadId * CliException`. `continuePropagation` case 4 returns a state flagged so the next `executeOneStep` returns this variant. `TestHarness.fs` treats this as a runtime failure and compares against whether the real .NET execution also terminated with an unhandled exception (same type; exit code from `dotnet` when a program faults is `-532462766` / `0xE0434352`).

### Runtime-synthesised exceptions

Add a helper:

```fsharp
IlMachineState.raiseManagedException
    : BaseClassTypes -> TypeInfo<TypeDefn, TypeDefn> (* the exception type, e.g. NullReferenceException *)
    -> ThreadId -> IlMachineState -> IlMachineState
```

which:

1. Concretises the exception type via the existing `concretizeType` path.
2. Allocates an instance on the heap with all fields zero-initialised (reusing `IlMachineState.allocateManagedObject`, same path `newobj` uses). **The ctor is NOT run.** This keeps synthesis atomic — no nested frame needed. `Exception._message` will be null; that matches what uncaught-exception printers see as "Exception of type X was thrown." Proper ctor invocation can be retro-fitted later if a test needs `.Message`.
3. Builds a `CliException` with the allocated address and initial stack frame.
4. Delegates to `continuePropagation`.

This lets any opcode raise an exception in one synchronous step, e.g. `ldfld` on null simply returns `state |> raiseManagedException baseClassTypes baseClassTypes.NullReferenceException thread` without advancing the PC.

### Correct catch-type matching

`isExceptionAssignableTo` needs to resolve `catchTypeToken` (a `MetadataToken` in the method's assembly) to a `ConcreteTypeHandle`, then walk the base-class chain of `exceptionType` checking handle equality. Interfaces are irrelevant — exception catches are strictly class-based per ECMA I.12.4.2. The token may be `TypeDef | TypeRef | TypeSpec`; `Assembly.resolveTypeRef` (which handles forwarding) plus `concretizeType` covers all three. Base-type walking has precedent in `IlMachineState` / `TypeConcretisation`; reuse `DumpedAssembly.resolveBaseType` and iterate until `Object`.

## Implementation stages (one PR per stage unless noted)

### Stage 1 — Plumbing: cross-frame unwind + type matching + harness integration

Three tightly-coupled pieces that together make the existing ComplexTryCatch Tests 1-4, 7 pass. Small enough to combine into one PR; splitting would leave intermediate states where throwing still crashes.

**Files**

- `WoofWare.PawPrint/ExceptionDispatching.fs`: implement `isExceptionAssignableTo` (resolve catch token → concretise → walk base-class chain of thrown type looking for handle equality).
- `WoofWare.PawPrint/IlMachineState.fs` (new section near existing exception helpers): add `continuePropagation` per the design above.
- `WoofWare.PawPrint/NullaryIlOp.fs:813-894` (`Throw`): after building `CliException`, call `continuePropagation` and remove the inline handler search.
- `WoofWare.PawPrint/NullaryIlOp.fs:794-811` (`Endfinally` `PropagatingException` arm): call `continuePropagation` with `exn` (replaces the TODO at line 808).
- `WoofWare.PawPrint.Domain/ExecutionResult.fs` (wherever `ExecutionResult` lives — check `IlMachineState.fs` / `AbstractMachine.fs`): add `UnhandledException of IlMachineState * ThreadId * CliException`.
- `WoofWare.PawPrint/AbstractMachine.fs:11-17`: when the active thread is in the unhandled-exception terminal state, return `ExecutionResult.UnhandledException`.
- `WoofWare.PawPrint.App/Program.fs:60-72` (`pumpToReturn`): handle `UnhandledException` — log stack trace, exit with `0xE0434352`.
- `WoofWare.PawPrint.Test/TestHarness.fs`: when PawPrint reports `UnhandledException`, compare against whether the real .NET process also exited with a fault code, and report a pass iff they agree.

**Verification**

- Remove `Assert.Inconclusive` for `ComplexTryCatch.cs` in `TestPureCases.fs:25`.
- Expect Tests 1–7, 14 to pass; later tests may still fail pending Stage 4–5 work — that's fine, the test file runs as a single program so mark it `Assert.Inconclusive` until Stage 5 ships if necessary.
- Add a new tiny pure-source test `UnhandledException.cs` that throws and never catches; expect nonzero exit matching `dotnet`'s.

### Stage 2 — Runtime-synthesised exception helper

Infrastructure only; no behaviour change until call sites are converted.

**Files**

- `WoofWare.PawPrint/IlMachineState.fs`: add `raiseManagedException` as described. Internally concretise via the existing `concretizeType`; allocate via `allocateManagedObject`; build `CliException`; delegate to `continuePropagation`.

**Verification**

- Unit-test via a throwaway call site in a `[<Test>]` or dead-code branch; remove before merge. No user-visible change.

### Stage 3 — NullReferenceException at obvious sites

Convert the cluster of sites where the opcode dereferences a managed pointer that could be null.

**Files / sites**

- `WoofWare.PawPrint/UnaryMetadataIlOp.fs:485, 492` (`Ldfld`, `Stfld`): when the target object is `NullObjectRef` (or managed pointer is null), call `raiseManagedException baseClassTypes baseClassTypes.NullReferenceException`.
- `WoofWare.PawPrint/IlMachineStateExecution.fs:52` (`callvirt` dispatch): NRE on null receiver.
- `WoofWare.PawPrint/NullaryIlOp.fs:55, 85, 110, 139` (`Ldind.*`, `Stind.*`): NRE on null.
- `WoofWare.PawPrint/NullaryIlOp.fs:820` (`Throw` with NullObjectRef on stack): replace with NRE raise.
- `WoofWare.PawPrint/IlMachineState.fs:1469, 1514` (managed pointer deref helpers): NRE on null.

**Verification**

- New test `NullDereferenceTest.cs` covering: null field load, null field store, null virtcall, `throw null`. Expected exit codes from real .NET runtime.

### Stage 4 — Array bounds + array-related NRE

**Files / sites**

- `WoofWare.PawPrint/NullaryIlOp.fs:~1017-1158` (`Ldelem.*`, `Stelem.*`, `Ldlen`): NRE on null array; `IndexOutOfRangeException` on out-of-range index.
- `WoofWare.PawPrint/NullaryIlOp.fs:146` (existing TODO for `Stelem` IOORE).
- `WoofWare.PawPrint/ManagedHeap.fs:125, 149`: replace `failwith` IOORE sites.

**Verification**

- New test `ArrayBoundsTest.cs`: null array access, negative index, too-large index.

### Stage 5 — Arithmetic exceptions

**Files / sites**

- `WoofWare.PawPrint/NullaryIlOp.fs:425, 455, 475` and nearby: `Div`, `Div_Un`, `Rem`, `Rem_Un` throw `DivideByZeroException`; `Add_Ovf`, `Sub_Ovf`, `Mul_Ovf` (signed+unsigned variants) throw `OverflowException`; `ckfinite` throws `ArithmeticException`.

**Verification**

- New tests `DivByZeroTest.cs`, `OverflowTest.cs`. After this stage, restore `ComplexTryCatch.cs` to `Assert.True`-style (if temporarily disabled in Stage 1).

### Stage 6 — `rethrow`

**Files**

- `WoofWare.PawPrint/NullaryIlOp.fs:812` (`Rethrow` TODO).
- `rethrow` is valid only inside a catch handler. The current catch pushes the exception object onto the eval stack (`NullaryIlOp.fs:867`); the frame can remember the "current caught exception" — add a `CurrentCatchException : CliException option` field to `MethodState` set on catch entry and cleared on `leave`. `rethrow` reads it and calls `continuePropagation` with stack trace preserved.

**Verification**

- `ComplexTryCatch.cs` Test 4 / 5 (rethrow preservation). Also a dedicated `RethrowTest.cs`.

### Stage 7 — Filter / `endfilter`

**Files**

- `WoofWare.PawPrint/ExceptionDispatching.fs:62` (TODO in filter branch): set up filter evaluation. Filter region jumps into the filter block with the exception on the stack; its `endfilter` returns 0/1; on 1 the associated handler runs.
- `WoofWare.PawPrint/NullaryIlOp.fs:767` (`Endfilter` TODO): pop int32 from eval stack; use `ExceptionContinuation.ResumeAfterFilter` to decide catch-vs-propagate.

**Verification**

- `ComplexTryCatch.cs` Test 20 (C# `catch (X when P)`).

### Stage 8 — Fault handlers

Low priority — C# doesn't emit them; F# doesn't; only hand-written IL does. Existing code already groups fault with finally in most places, which is almost right (fault runs only on exception, finally runs always); the only difference matters on the normal-completion path of a try region. Audit `leave` handling to confirm faults are skipped on normal exit.

**Files**

- `WoofWare.PawPrint/UnaryConstIlOp.fs` `leave` implementation: do not queue Fault regions, only Finally.
- `WoofWare.PawPrint/NullaryIlOp.fs` `Endfault` (same opcode as Endfinally — already handled).

**Verification**

- Hand-written IL test (place `.il` file compiled via `ilasm` in `sourcesImpure/` if the harness supports it; otherwise `Assert.Inconclusive` documenting the gap).

## Critical files to be modified (summary)

| File | Stages |
|---|---|
| `WoofWare.PawPrint/IlMachineState.fs` | 1 (continuePropagation), 2 (raiseManagedException) |
| `WoofWare.PawPrint/ExceptionDispatching.fs` | 1 (assignability), 7 (filter) |
| `WoofWare.PawPrint/NullaryIlOp.fs` | 1, 3, 4, 5, 6, 7 |
| `WoofWare.PawPrint/UnaryMetadataIlOp.fs` | 3 (ldfld/stfld NRE) |
| `WoofWare.PawPrint/IlMachineStateExecution.fs` | 3 (callvirt NRE) |
| `WoofWare.PawPrint/ManagedHeap.fs` | 4 (IOORE) |
| `WoofWare.PawPrint/MethodState.fs` | 6 (CurrentCatchException field) |
| `WoofWare.PawPrint/AbstractMachine.fs` | 1 (UnhandledException plumbing) |
| `WoofWare.PawPrint.Domain/...` (ExecutionResult) | 1 (new DU case) |
| `WoofWare.PawPrint.App/Program.fs` | 1 (unhandled logging + exit code) |
| `WoofWare.PawPrint.Test/TestHarness.fs` | 1 (unhandled exception comparison) |
| `WoofWare.PawPrint.Test/TestPureCases.fs` | 1 / 5 (un-inconclusive ComplexTryCatch) |
| `WoofWare.PawPrint.Test/sourcesPure/*.cs` | 1–7 (new test cases) |

## Reused helpers

- `IlMachineState.concretizeType` (IlMachineState.fs:219)
- `IlMachineState.allocateManagedObject` (IlMachineState.fs:1146)
- `Assembly.resolveTypeRef` (Assembly.fs)
- `DumpedAssembly.resolveBaseType` (used in AbstractMachine.fs:32)
- `ExceptionHandling.findFinallyBlocksToRun` / `getActiveRegionsAtOffset` (Exceptions.fs)
- `ExceptionDispatching.findExceptionHandler` (ExceptionDispatching.fs, after fixing assignability)
- `ThreadState.appendFrame` / frame-access helpers (ThreadState.fs)
- `MethodState.clearEvalStack`, `setExceptionContinuation`, `clearExceptionContinuation` (MethodState.fs)

## Verification (end-to-end)

- `nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --verbosity normal` — existing tests must stay green through every stage.
- After Stage 1: `nix develop -c dotnet test --filter "Name~ComplexTryCatch"` should exercise Tests 1–7, 14 successfully (later tests may still be skipped by the single-program exit code).
- After Stage 5: `ComplexTryCatch.cs` passes end-to-end.
- After Stage 7: `nix develop -c dotnet test --filter "Name~Exception"` green across the board.
- Spot-check run: `nix develop -c dotnet run --project WoofWare.PawPrint.App -- CSharpExample/.../CSharpExample.dll` with a C# program that throws and catches.

## Open design decisions worth revisiting

- **Constructor invocation for synthesised exceptions.** Current plan skips the ctor. If a test later needs `Exception.Message` populated, we'll need to thread the synthesis through a suspended-ctor frame. Deferred.
- **Stack overflow.** `StackOverflowException` requires a frame-depth budget on `ThreadState.MethodStates`. Out of scope for this plan; call it out as a follow-up.
- **AppDomain unhandled-exception event.** Real .NET raises `AppDomain.UnhandledException` before termination. Out of scope.
