# `StackTrace_GetStackFramesInternal`

## What it is

```c
extern "C" void QCALLTYPE StackTrace_GetStackFramesInternal(
    QCall::ObjectHandleOnStack stackFrameHelper,
    BOOL fNeedFileInfo,
    QCall::ObjectHandleOnStack exception);
```

`debugdebugger.cpp:287`. The primitive under `StackFrameHelper.InitializeSourceInfo`, and so under
every `new StackTrace(...)`, `new StackFrame(...)` and `Environment.StackTrace`.

It fills a caller-allocated `StackFrameHelper` in place: `iFrameCount` is in/out (in =
`NumFramesRequested`, `0` meaning "all"; out = the count actually captured), and array fields are
assigned — fourteen unconditionally when the capture is non-empty, plus
`rgiLastFrameFromForeignExceptionStackTrace` conditionally.

## Measured ground truth

Reachability, from the playground guest `new StackTrace()` (PawPrint, this worktree):

```
Unimplemented native method (PInvokeImpl QCall!StackTrace_GetStackFramesInternal):
  StackTrace::<GetStackFramesInternal>g____PInvoke|0_0(ObjectHandleOnStack, Int32, ObjectHandleOnStack) -> void
```

So `BOOL` arrives as `Int32`, and the entry point is live today.

Real .NET's frame list, read by reflecting into `StackFrameHelper` directly (host probe, net10.0):

| case | frames |
| --- | --- |
| current thread | 10, starting `StackTrace.GetStackFramesInternal`, `StackFrameHelper.InitializeSourceInfo`, then the reflection frames of the probe itself, then `P.Dump`/`Level2`/`Level1`/`Main` |
| from a thrown exception | 3 — exactly the dispatch frames (`Thrower`, `ThrowOuter`, `Main`); **no** `System.Diagnostics` frames |
| from an unthrown exception | **0** |

Two facts fall out of that:

* the current-thread walk **includes CoreLib's own frames**. The guest removes them itself:
  `CalculateFramesToSkip` (StackTrace.CoreCLR.cs:18-44) counts the leading run of frames whose
  declaring type's namespace is **ordinal-equal** to `"System.Diagnostics"` — equality, not a
  prefix, so `System.Diagnostics.Tracing` would stop the run — and is applied only when `e == null`
  (CaptureStackTrace:84).
* an exception that has never been thrown yields zero frames, which is the `data.cElements == 0`
  branch: `iFrameCount = 0` and **not one array is allocated** (debugdebugger.cpp:331-334).

PawPrint's own live chain at the QCall, from a throwaway handler that dumped the frame chain and
failed (innermost first):

```
<GetStackFramesInternal>g____PInvoke|0_0 @ il=0
GetStackFramesInternal                   @ il=13     <- the LibraryImport marshalling wrapper
GetStackFramesInternal                   @ il=20     <- the internal static overload
InitializeSourceInfo                     @ il=8
CaptureStackTrace                        @ il=21
InitializeForCurrentThread               @ il=9
StackTrace..ctor                         @ il=14
Probe.Inner                              @ il=5
Probe.Main                               @ il=5
```

Seven CoreLib frames where real .NET shows fewer, because PawPrint never inlines. **Every one of
them is declared on `StackTrace` or `StackFrameHelper`, both in namespace `System.Diagnostics`**,
so the guest's skip rule absorbs the difference exactly. The frame *count* diverges; the first
guest-visible frame does not. That is a property of the skip rule keying on namespace rather than
count, so it is an argument, not a coincidence.

Scope that claim carefully: it covers only the **leading** run. Nothing absorbs PawPrint's extra
un-inlined frames in the *middle* of a stack (guest → BCL callback → guest) or at the *bottom*
(a spawned thread's `Thread.StartHelper` plumbing). So frame *counts* and full trace *strings* are
not cross-runtime facts here, and no test may assert them.

## Downstream: what this cannot reach yet

`CaptureStackTrace` builds a `StackFrame` for **every** captured frame, before computing any skips
(`StackTrace.CoreCLR.cs:73-85`), and the `StackFrame(StackFrameHelper, ...)` constructor calls
`GetMethodBase(i)` unconditionally (StackFrame.CoreCLR.cs:18). Nothing is lazy. `GetMethodBase`
(StackFrameHelper.cs:148) is

```csharp
RuntimeMethodHandle.GetTypicalMethodDefinition(new RuntimeMethodInfoStub(new RuntimeMethodHandleInternal(mh), this))
```

and `GetTypicalMethodDefinition` (RuntimeHandles.cs:1291) tests the `IsTypicalMethodDefinition`
**InternalCall** first, falling through to the `RuntimeMethodHandle_GetTypicalMethodDefinition`
**QCall** when that says no. Neither is implemented in PawPrint. Measured:

* `MethodBase.GetMethodFromHandle(h)` **works** — so `RuntimeType.GetMethodBase`, the expensive
  managed part, is already functional. The gap is only the two-primitive pair.
* `MethodBase.GetCurrentMethod()` fails on a *third* unimplemented QCall
  (`MethodBase_GetCurrentMethod`), which is unrelated and out of scope.

`IsTypicalMethodDefinition` is small (`method.cpp:1685`): false if there is a method instantiation
and it is not the generic method definition, false if there is a class instantiation and the
declaring type is not the generic type definition, true otherwise. But CoreCLR's `rgMethodHandle`
has already had `StripMethodInstantiation()` applied while the *class* instantiation is left alone
(`debugdebugger.cpp:449-452`), so a frame in a method on `List<int>` answers *false* and genuinely
needs the QCall. Implementing only the predicate would leave the generic-declaring-type case
crashing at a primitive the predicate just promised was unnecessary. The pair goes together.

**Consequence: any capture with one or more frames still cannot complete after this PR.**

### A third blocker, past that one

With `fNeedFileInfo` true, `InitializeSourceInfo` calls `CreateStackTraceSymbols()` — an
`[UnsafeAccessor]` constructor into the `System.Diagnostics.StackTrace` assembly — **before** the
per-frame loop, gated only on `fNeedFileInfo` and not on the frame count
(StackFrameHelper.cs:95-113, verified). PawPrint's `[UnsafeAccessor]` dispatch is a `failwith` TODO
(`AbstractMachine.fs:395-402`). The whole block sits in the guest's own `try { } catch { }`, which
would swallow a *guest* exception but cannot swallow a host `failwith`.

So `new StackTrace(someException, fNeedFileInfo: true)` reaches that crash **in this PR**, with
zero frames — it is not gated behind `GetMethodBase`. Today it fails earlier, at the QCall itself;
after this PR it fails at the UnsafeAccessor instead. Not a behavioural regression (it fails either
way), but it changes which primitive is named, and it means the Stage-1 test must deliberately pick
a `fNeedFileInfo: false` overload.

Note also that `Exception.StackTrace` — which does pass `fNeedFileInfo: true` — cannot reach any of
this under PawPrint: `setExceptionStackTraceString` (`IlMachineRuntimeMetadata.fs:985`) pre-writes
`_stackTraceString` at dispatch, and the property short-circuits on it (Exception.cs:216-218);
an unthrown exception early-outs at `!HasBeenThrown`.

## Design decisions

### D1. Where the current-thread frame walk lives

* **(a) A new guest-observable walk producing `ExceptionStackFrame list`.** The exception branch
  already hands back exactly that type from `IlMachineState.frozenStackTraceFrames`, so both
  branches feed one materialiser and the two frame sources cannot drift in what a frame *is*.
* (b) Reuse `GuestLocation.attributionOffsets`, which already computes per-frame attribution
  offsets over `thread.MethodStates`.
* (c) Share the single-frame constructor with the dispatch machinery, so the call-site offset rule
  exists in exactly one place.

**Choose (a).** Against (b): `GuestLocation` is documented as PawPrint's own developer-facing
diagnostic — "Nothing here may affect execution" — and its walk deliberately degrades ("cost
precision rather than raise") on an unexpected chain shape. Routing guest-observable output through
it would make `StackTrace` depend on a best-effort diagnostic, and would couple two things whose
constraints differ (fidelity here, legibility there) — a coupling that file explicitly warns
against.

(c) is the more tempting rejection, and it is a real cost of (a): the call-site offset rule will
exist in two places (`ExceptionDispatching.appendCallerFrame:683` and the new walk). It is rejected
because dispatch builds frames *incrementally during unwinding*, one per frame left, threading a
`CliException` through; there is no single-frame constructor to share without first inverting that
control flow, which is a much larger change than the duplication it saves. The duplication is
instead guarded by a mutation test (below).

The walk must use the same offset rule dispatch uses: the innermost frame at its own `IlOpIndex`,
every enclosing frame at the `CallSiteIlOpIndex` recorded by the frame it called, not the offset it
will resume at (`GuestLocation.fs:207-218` explains why).

### D2. `rgiMethodToken`

* (a) **Zero for every frame.** `InitializeSourceInfo` skips its `GetSourceLineInfo` call for any
  frame whose token is 0 (StackFrameHelper.cs:119), so the per-frame portable-PDB path becomes a
  no-op and `rgAssembly` / `rgAssemblyPath` / `rgLoadedPeAddress` / `rgiLoadedPeSize` /
  `rgiIsFileLayout` / `rgInMemoryPdbAddress` / `rgiInMemoryPdbSize` are never read.
* (b) Real MethodDef tokens plus a PDB reader and fabricated loaded-PE addresses.

**Choose (a).** PawPrint has no loaded PE image to name and no PDB reader, and it already does not
emit `in File:line` on rendered traces — a documented divergence the parked guests are written
around (`ExceptionDispatchInfoCapture.cs:11-13`). (b) is worse than "a different feature": with
`fNeedFileInfo` true it would call `GetSourceLineInfo`, a *second* `[UnsafeAccessor]`, once per
frame. Zero tokens are required for safety given the UnsafeAccessor gap, not merely convenient.

### D3. `rgiOffset` (the native offset)

* (a) `0`.
* (b) The IL offset — a lie about what the field means.
* (c) `-1`, i.e. `StackFrame.OFFSET_UNKNOWN` (StackFrame.cs:133).

**Choose (c).** PawPrint executes no native code, so there is no native offset, and `-1` is
CoreLib's own word for exactly that. `StackFrame.GetNativeOffset()` is public API, so this is
guest-observable and should be honest rather than plausible; (b) would make a guest's
`GetNativeOffset()` return something that looks like a real address-ish quantity.

`StackTrace.ToString` reads only `GetILOffset()` (StackTrace.cs:335), so nothing in the common
rendering depends on this. `StackFrame.ToString()` does: it renders `-1` as `"<offset unknown>"`
where real .NET prints a number (StackFrame.cs:241-243). That is a guest-observable string
divergence, and the honest one.

### D4. Which arrays to allocate

CoreCLR allocates fourteen whenever `cElements != 0`, plus the conditional foreign-flag array.
One of them — `rgAssembly` — it allocates as `object[]` and stores into an `Assembly?[]?` field
(`AllocateObjectArray(.., g_pObjectClass)`, debugdebugger.cpp:362), a type pun the VM gets away
with and PawPrint's typed heap should not imitate. (`rgAssemblyPath` and `rgFilename` use
`g_pStringClass` and are genuinely `string[]`.)

* (a) Allocate exactly the arrays a guest can read given D2: `rgMethodHandle`, `rgiOffset`,
  `rgiILOffset`, `rgiMethodToken`, and `rgiLastFrameFromForeignExceptionStackTrace`.
* (b) Allocate all fifteen, matching CoreCLR field for field, giving the punned one its declared
  element type rather than `object`.

**Choose (a)**, with each omission justified by D2 rather than by convenience: a null array that
managed code would dereference is a crash, so the set is exactly "what `rgiMethodToken = 0` leaves
reachable". `rgFilename` / `rgiLineNumber` / `rgiColumnNumber` stay null too — their getters
null-check (`GetFilename` uses `rgFilename?[i]`; `GetLineNumber`/`GetColumnNumber` return 0 for
null), so `StackFrame` reads them safely on the `fNeedFileInfo` path. `dynamicMethods` is omitted:
it exists to keep resolvers and collectible `LoaderAllocator`s alive ("Field is not used from
managed"), and PawPrint's registry has no lifetime problem to solve.

`rgiLastFrameFromForeignExceptionStackTrace` follows CoreCLR's optimisation: allocated only if some
frame carries the flag, else explicitly null, which `IsLastFrameFromForeignExceptionStackTrace`
reads as false for every frame (debugdebugger.cpp:400-415). `ExceptionStackFrame` already carries
that bit per frame.

### D5. `rgMethodHandle` cell contents, and synthesised frames

`IntPtr[]`, read back as `new RuntimeMethodHandleInternal(mh)`. PawPrint has two interconvertible
spellings of a method handle: `CliRuntimePointer.MethodRegistryHandle id` and
`NativeIntSource.MethodHandlePtr id`, and `NativeCall.methodHandleIdOfRuntimeMethodHandleInternal`
accepts both. Store `NativeIntSource.MethodHandlePtr id` — the `IntPtr`-shaped spelling, which is
what `EvalStack`'s rewrap rules produce for a value that has travelled through an `IntPtr` cell.

The open question is a frame whose method is runtime-synthesised. It has no MethodDef token, and
`MethodHandleRegistry` refuses to mint a handle for one by design ("reaching here with one means
some path handed a runtime-supplied method to reflection, which is a bug in that path").

* (a) Write a zero handle, so `GetMethodBase` returns null.
* (b) Omit synthesised frames from the walk, as `ExceptionDispatching.isDelegateInvokeStub` already
  does for dispatch traces.
* **(c) Refuse: `failwith` naming the condition.**

**Choose (c).** (a) looked like the safe default and is not: a null `MethodBase` does **not** stop
`CalculateFramesToSkip`, because the namespace checks live inside `if (mb != null)` and the
`iRetVal++` after them is unconditional (StackTrace.CoreCLR.cs:26-41). So a zero handle contiguous
with the CoreLib prefix makes the skip run swallow that frame *silently* — including a synthesised
frame that is the guest's own innermost one. That is a wrong trace presented as a right one. Nor
does CoreCLR do this: it writes a real `MethodDesc*` even for LCG stubs, and asserts
`IsRuntimeMethodHandle()` (debugdebugger.cpp:456), so the zero handle would be a PawPrint invention
riding on a defensive check.

(b) trades the silent skip for a silent omission — better, but still a wrong answer. (c) matches
the project's stated preference for crashing over documented divergence, and is cheap to relax: if
a real guest turns out to put a synthesised frame on a captured stack, the refusal names it, and we
then choose (a) or (b) with an actual example in hand rather than a guess.

If (a) or (b) is ever adopted, the zero cell must be spelled `NativeIntSource.Verbatim 0L`, not
`MethodHandlePtr 0L`: the latter decodes as `Some 0L` in
`methodHandleIdOfRuntimeMethodHandleInternal` (NativeCall.fs:230-240) and would fail lookup, and
the guest's `mh == IntPtr.Zero` comparison must see a plain zero.

### D6. Two renderers of the same truth

Recorded rather than decided, because it is not this PR's to settle. PawPrint currently renders
traces host-side (`renderExceptionStackTrace`, pre-writing `_stackTraceString` at dispatch). Once
this QCall and the `GetTypicalMethodDefinition` pair exist, the guest can render its own via
`StackTrace.ToString`, and the two can disagree guest-observably — `ex.StackTrace` versus
`new StackTrace(ex).ToString()`. The pre-written `_stackTraceString` is itself a divergence: real
.NET leaves it null, and reflection or serialisation can see that.

The canonical destination is to stop pre-writing the string and let the guest build it lazily
through this QCall, leaving one renderer. That is blocked on the pair *and* on UnsafeAccessor
dispatch. Naming it here so the two-truths state is understood as a transition rather than an end
state; a half-finished migration taxes every later change.

## Plan

Stage 1 — this PR. `Native/NativeStackTrace.fs`, registered in `NativeQCall.fs`, plus the
current-thread walk.

1. A guest-observable current-thread walk returning `ExceptionStackFrame<ConcreteTypeHandle, ...>
   list`, innermost first, per D1. `StackFrameHelper` needs no `BaseClassTypes` entry:
   `IlMachineState.setOwnInstanceField` resolves a field against the object's own concrete type,
   and the type is sealed and derives directly from `Object`, so every field the handler touches is
   its own. That leaves `WoofWare.PawPrint.Domain`'s published surface untouched.
2. The handler: read both `ObjectHandleOnStack`s; pick the frame source from whether the exception
   argument is null; write `iFrameCount`; on a zero-frame capture write nothing else.
   `NumFramesRequested` truncation applies **only** to the current-thread branch — CoreCLR consults
   it in `GetStackFrames`' walk callback (debugdebugger.cpp:242) and never in
   `GetStackFramesFromException` ("For StackTraces from an Exception, the EE always captures all
   frames", StackFrameHelper.cs:78-80).
3. Materialise the arrays per D2–D5.

Tests:

* A guest asserting the zero-frame contract end to end, using **`new StackTrace(ex)`** — the
  single-argument overload, chosen because it passes `fNeedFileInfo: false`
  (StackTrace.cs:81-86) and so does not reach the UnsafeAccessor crash. Assertions:
  `FrameCount == 0`, `GetFrames().Length == 0` (it returns `Array.Empty`, never null —
  StackTrace.cs:171-176), `GetFrame(0) == null`. `sourcesPure`, because real .NET agrees on all
  three (measured), which makes it a cross-runtime fact.
* **F#-level tests over the handler's writes**, which the guest test above cannot substitute for:
  `StackFrameHelper`'s constructor already sets `iFrameCount = 0`, so a handler that writes nothing
  — or ignores its exception argument entirely and always answers zero — passes the guest test. The
  F# tests invoke the handler against a state holding a real helper object and assert the field
  writes directly. They are also the *only* Stage-1 observer of the non-zero-frame array
  materialisation (D4/D5), since no guest can read those arrays without `GetMethodBase`.
* F#-level tests over the walk: frame order, the call-site-not-resume-point offset rule, and
  `NumFramesRequested` truncation (whose only observer is these tests — every CoreLib caller passes
  a fresh helper with `iFrameCount = 0`).
* Mutation tests: substitute the resume PC for `CallSiteIlOpIndex` and watch the offset test go
  red. Expected offsets must come from independently disassembled IL (`WoofWare.PawPrint.IlDump`),
  not from reading `CallSiteIlOpIndex` back out of the same structures — otherwise the oracle
  shares the mistake, and `GuestLocation.fs:207-218` records that this particular mutant survives
  at source-line granularity.
* Parked `sourcesPure` guests, each naming its *actual* blocker rather than a predicted one:
  a non-zero-frame capture (blocked at `IsTypicalMethodDefinition`), and
  `new StackTrace(ex, fNeedFileInfo: true)` (blocked at the `[UnsafeAccessor]` TODO,
  `AbstractMachine.fs:402`).

Stage 2 — separate PR, not this one. `RuntimeMethodHandle.IsTypicalMethodDefinition` (InternalCall)
and `RuntimeMethodHandle_GetTypicalMethodDefinition` (QCall), together.

The order is settled by observability, not preference: `GetTypicalMethodDefinition`'s **only**
CoreLib caller is `StackFrameHelper.GetMethodBase` (verified by grep over the pinned CoreLib).
Landing the pair first would land a primitive unreachable in isolation — nothing could observe it —
so AGENTS.md's usual dependency-first preference does not apply; the dependency has no independent
test surface. Stage 1 has a thin but real oracle, and Stage 2 becomes observable the moment it lands
on top.

Stage 2 does **not** un-park `ActivatorCctorThrowsInnerStackTrace.cs` or the
`SetCurrentStackTrace` half of `ExceptionDispatchInfoCapture.cs`. Both need `fNeedFileInfo: true`
(`Exception.GetStackTrace` at Exception.cs:232; `SetCurrentStackTrace` at Exception.cs:247), so
after Stage 2 they march past `GetMethodBase` and die at `CreateStackTraceSymbols()`. The chain to
those green guests is three features long: this QCall, the pair, and then `[UnsafeAccessor]`
dispatch — or, more cheaply, making an unresolvable `[UnsafeAccessor]` raise a *guest* exception,
which the surrounding `try/catch` would swallow exactly as real .NET does when
`System.Diagnostics.StackTrace.dll` is absent.
