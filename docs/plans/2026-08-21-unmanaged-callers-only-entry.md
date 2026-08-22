# Refusing a managed entry into an `[UnmanagedCallersOnly]` method

Two PRs. This document covers both, because the first exists only to make the second possible and
should be read against the measurements that justify it.

- **PR 1 — a fatal error is not a `FailFast`, and an IL step can raise one.** Model CoreCLR's
  fatal-error space faithfully, and give the interpreter's step protocol a way to express "the
  process aborted". No gate, no guest-visible refusal.
- **PR 2 — the gate.** One check at `callMethodWithCommitment`, covering all four measured routes.

## What this is

A method carrying `System.Runtime.InteropServices.UnmanagedCallersOnlyAttribute` may be entered
only from native code. CoreCLR enforces that in the JIT: such a method is compiled with
`CORJIT_FLAG_REVERSE_PINVOKE` (jitinterface.cpp:12917-12934), so its prologue performs a reverse
P/Invoke transition and asserts that the calling thread was in *preemptive* GC mode. Managed code
runs cooperative, so any managed entry lands in `ReversePInvokeBadTransition`
(dllimportcallback.cpp:167-180), which is an *uncatchable* fatal error:

```c
EEPOLICY_HANDLE_FATAL_ERROR_WITH_MESSAGE(
    COR_E_EXECUTIONENGINE,
    W("Invalid Program: attempted to call a UnmanagedCallersOnly method from managed code."));
```

PawPrint has no such gate: it interprets the body like any other method.

## Measured

All rows measured 2026-08-21 against the devshell's .NET 10.0.7, guest `scratchpad/ucoprobe`,
whose target is `[UnmanagedCallersOnly] public static int Doubler (int x) => x * 2;`. Every
"abort" row is stderr `Fatal error.` followed by the message above, exit 134.

| route | real .NET | PawPrint | abort channel today? |
| --- | --- | --- | --- |
| `mi.CreateDelegate(typeof(Func<int,int>))` | binds; `Target` null, `Method` reported | same ✓ | n/a |
| invoking that delegate | abort | returns 42 ✗ | yes |
| `mi.Invoke(null, new object[]{21})` | abort | returns 42 ✗ | yes |
| `AppContext.SetSwitch("Switch.System.Reflection.ForceEmitInvoke", true)` then `mi.Invoke` | abort | returns 42 ✗ | **no** |
| `(delegate*<int,int>)(nint)(delegate* unmanaged<int,int>)&Doubler` then call | abort | returns 42 ✗ | **no** |
| `((delegate* unmanaged<int,int>)&Doubler)(21)` | 42 | 42 ✓ | n/a — legal |
| C# direct call / method-group conversion | Roslyn CS8901 / CS8902 | n/a | n/a |

Four facts fall out, each of which changed the design.

**Binding is not the gate.** `Delegate_BindToMethodInfo` (comdelegate.cpp:1110-1177) and
`COMDelegate::BindToMethod` (:1184-1305) contain no `UnmanagedCallersOnly` check at all, and the
measurement confirms the delegate is built. The only bind-time refusal is in
`COMDelegate::GetDelegateCtor` (comdelegate.cpp:2791-2795), a *catchable* `NotSupportedException`
on the `ldftn`+`newobj` route, which Roslyn refuses to emit (CS8902). Out of scope.

**The abort precedes the class initialiser.** A UCO method whose declaring type has a
`Console.WriteLine`-ing static constructor aborts on real .NET without the constructor ever
running — measured on both the delegate and the reflection path. PawPrint runs it and then runs the
body. The gate therefore belongs *before* the callee's frame is pushed, since PawPrint runs a
`.cctor` from the callee frame's prologue; a gate placed after commitment would emit side effects
real .NET never emits. This is directly testable and is what mutant (g) below exists to catch.

**Two of the four divergent routes are reachable from plain C# and have no abort channel.** An
earlier draft of this plan claimed only hand-authored IL could reach `call`/`callvirt` and that
`calli` was "the legal route". Both claims were wrong, and Fable caught them:

- `MethodInvokerCommon.Initialize` (MethodInvokerCommon.cs:25) selects an emitted stub when
  `Switch.System.Reflection.ForceEmitInvoke` is set — a switch any guest can set with
  `AppContext.SetSwitch` — and the stub enters the target with `OpCodes.Call`
  (InvokerEmitUtil.cs:229). PawPrint executes emitted stubs, so this bypasses any gate on the
  reflection QCall.
- Roslyn blocks only the *direct* conversion. `(delegate*<int,int>)(nint)(delegate* unmanaged<int,int>)&Doubler`
  compiles (the test pipeline sets `AllowUnsafe`, Roslyn.fs:84) and yields a managed-convention
  `calli` to a UCO target, which is a managed entry. `executeCalli`
  (UnaryMetadataCallOps.fs:1517-1720) never inspects the calling convention at all.

**Blast radius in the framework.** Scanning the host shared framework and
`$DOTNET_LINUX_FRAMEWORK_DIR`: 22 UCO methods in `System.Private.CoreLib`, ~20 more across
`System.Console`, `System.Net.*`, `System.IO.FileSystem.*`. All are genuine native callbacks
(`PosixSignalRegistration::OnPosixSignal`, `ConsolePal::InvalidateTerminalSettings`,
`QuicConnection::NativeCallback`, ...). None should be entered from managed code in a correct run,
so the gate should be inert for the existing suite; if it is not, that is a finding. PawPrint's own
`SignalDispatch` builds the `OnPosixSignal` frame directly and so does not go through the
chokepoint — checked, not assumed.

---

# PR 1 — the fatal-error vocabulary and the abort channel

## 1a. A fatal error is not a `FailFast`

`RunOutcome.FailFast` (IlMachineStateModel.fs:535) and `ExecutionResult.FailFast` (:365) mean, per
their docstrings, "a thread called `Environment.FailFast`". CoreCLR routes both this and the UCO
abort through `EEPolicy::HandleFatalError(UINT exitCode, ...)`, and the exit code it is handed is
what distinguishes them:

| | HRESULT | banner | Unix | Windows |
| --- | --- | --- | --- | --- |
| `Environment.FailFast` | `COR_E_FAILFAST` 0x80131623 | `Process terminated.` | 134 | 0x80131623 |
| runtime-raised | `COR_E_EXECUTIONENGINE` 0x80131506 | `Fatal error.` | 134 | 0x80131506 |
| stack overflow (not modelled) | `COR_E_STACKOVERFLOW` | `Fatal error.` | 134 | that code |

The banner is derived, not stored: `if (exitCode == (UINT)COR_E_FAILFAST)` (eepolicy.cpp:374-383).
The process then dies via `CrashDumpAndTerminateProcess(exitCode)` — `abort()` on Unix, hence 134
for every kind; `TerminateProcess(..., exitCode)` on Windows, where they differ.

So the payload carries the HRESULT identity and derives banner and exit code from it, mirroring the
real mechanism, rather than an enum that hard-codes a two-element space. The third row above is why:
the space is open, and a closed `FailFast | ExecutionEngine` enum would have to be reopened the
first time PawPrint models a stack overflow.

```fsharp
/// Which of CoreCLR's fatal errors this is, as the `COR_E_*` HRESULT `EEPolicy::HandleFatalError`
/// is handed. Cases are added as PawPrint gains a producer for one; the HRESULT is the identity,
/// and banner and Windows exit code are derived from it exactly as eepolicy.cpp derives them.
type FatalErrorCode =
    | FailFast          // COR_E_FAILFAST, 0x80131623
    | ExecutionEngine   // COR_E_EXECUTIONENGINE, 0x80131506

type FatalError =
    {
        Code : FatalErrorCode
        Message : string option
    }
```

`ExecutionResult.FailFast` and `RunOutcome.FailFast` are **renamed** to `Aborted` and take one.
(`Aborted` rather than `FatalError`, so that the case does not collide with the type: neither DU is
`RequireQualifiedAccess`. It also matches `WhatWeDid.Aborted` and `CallCommitment.Aborted` below.)
The rename is not cosmetic: after this change the case no longer means "FailFast", and leaving the
old name is the exact "classifier whose contract has stopped being truthful" that AGENTS.md warns
about. ~35 construction and destructuring sites across the library, App, `DebuggerServer`,
`Performance` and ~18 test files become compile errors, which is the point — roughly fifteen of
them print "Guest called Environment.FailFast" in prose that would otherwise silently become false.

`WoofWare.PawPrint.App` derives its exit code and log line from `Code` instead of hard-coding
`COR_E_FAILFAST` (App/Program.fs:236-247).

`RealRuntime` (RealRuntime.fs:29-30, 44, 221) currently knows only the `Process terminated.`
banner, so a real-runtime `Fatal error.` death is classified `NormalExit 134` — silently wrong, and
it would make PR 2's differential test vacuous. It learns the second banner in the same change.

It does *not* learn a `FatalErrorCode` from it. CoreCLR picks the banner with one equality test on
the code, so stderr separates `COR_E_FAILFAST` from everything else and nothing finer, and on Unix
the exit status is 134 whatever the code was — labelling the second banner `ExecutionEngine` would
mislabel a stack overflow, the third code named in the table above. The oracle therefore gets its
own two-valued `ObservedFatalError`, deliberately a *different type* from `FatalErrorCode` so that
what PawPrint knows it raised is never silently compared against what the oracle merely observed. A
test that needs to identify a particular non-FailFast abort reads the report: the runtime's message
is a hardcoded literal and identifies the situation even though it does not identify the code.

## 1b. An IL step can raise one

`callMethodWithCommitment` (IlMachineStateExecution.fs:1652) is the single chokepoint every entry
funnels through, and it cannot currently report an abort. Verified: `CallCommitment` is exactly
`Committed | Raised` (:1633-1650); `WhatWeDid` (IlMachineStateModel.fs:271-317) has no terminating
case; 15 call sites; `WhatWeDid` is consumed across ~25 library files.

The rejected alternative is a `PendingFatalError` field on `IlMachineState` that the pump checks.
It avoids the ripple, and it makes `CallCommitment.Committed` a lie for one step — no frame was
pushed, yet the caller proceeds — which is the failure mode the repo's "keep the classifier
truthful" rule exists to prevent. There is no cheaper *honest* channel.

So: `CallCommitment` gains `Aborted of FatalError`, `WhatWeDid` gains `Aborted of FatalError`, and
`AbstractMachine` maps the latter to `ExecutionResult.FatalError` at the single point where an op's
`WhatWeDid` becomes an `ExecutionResult` — so `Scheduler.onStepOutcome` never sees it, and a
terminated step is never mistaken for a retired one.

Honest limitation, to be stated in the PR body rather than discovered by a reviewer: **PR 1 adds no
producer.** Both new cases are unconstructed until PR 2's gate, so their handling arms are
exercised only by the tests below and not by any guest. That is the cost of the split, taken
deliberately so that a structural change to the step protocol is reviewable on its own.

### What it turned out to cost

The compiler found every site, and the count was much smaller than the raw grep suggested: 4 matches
on `CallCommitment` (the other 11 callers go through the `callMethod` wrapper) and 9 on `WhatWeDid`.

Two things fell out that were not planned.

`callMethod` — `callMethodWithCommitment >> fst` — *silently discards* the commitment. That is
harmless for `Committed` and `Raised`, whose consequences are already in the returned state, but it
would have swallowed an abort and let the caller run on against a state whose process had died. Its
contract is now "for a call site whose target cannot be refused", enforced by a loud failure, and
its remaining callers all name a constructor or a specific BCL method.

The two callers that *could* name a refusable target turned out to be `call` and `callvirt`
themselves (UnaryMetadataCallOps.fs), which already return `IlMachineState * WhatWeDid` and so can
propagate. Switching them to `callMethodWithCommitment`, along with `calli` which already used it,
means **PR 2's single gate reaches all four measured routes with no further plumbing** — including
the emitted invoke stub, whose `OpCodes.Call` is an ordinary `call`.

## PR 1 tests

1. `Environment.FailFast` still reports `FatalErrorCode.FailFast` with its message, end to end
   through the pump — the existing behaviour, now asserting the kind.
2. The App's exit-code and banner derivation, per kind, against the eepolicy.cpp table above.
   Unit-level: both kinds give 134 on Unix and their own HRESULT on Windows.
3. `RealRuntime` classifies a guest that calls `Environment.FailFast` as `Aborted FailFast`, and
   one that dies with `Fatal error.` as `Aborted Other` with a report naming the refusal. The
   second needs a guest that
   provokes a runtime fatal error on real .NET without PawPrint having to run it; the UCO guest is
   exactly that, so it lands here as an oracle-only fixture. Written before the fix and observed
   failing with `NormalExit 134` — the silent misclassification the plan predicted.
4. `Scheduler.onStepOutcome` refuses `WhatWeDid.Aborted` and refuses *only* that, over a table tied
   by reflection to the DU's arity. This is what pins the conversion contract in the absence of a
   producer: a `surfaceAbort` quietly removed upstream surfaces here rather than as a scheduler
   silently treating an abort as forward progress.
5. The two `COR_E_*` values, and that distinct codes have distinct HRESULTs. Pinned rather than
   exercised because on Unix every fatal error aborts to 134, so a wrong constant is invisible on
   the platforms this suite runs on.
6. Whole suite green — the rename must not change any outcome.

---

# PR 2 — the gate

One check in `callMethodWithCommitment`, before the callee's frame is pushed (see "the abort
precedes the class initialiser" above), returning `CallCommitment.Aborted`.

The check needs to distinguish the legal native transition from a managed entry. It is answerable
from metadata PawPrint already has, without modelling GC mode: the `calli` for
`delegate* unmanaged<int,int>` carries a StandaloneSignature whose header is
`SignatureCallingConvention.Unmanaged` (raw 0x09) — measured by decoding the probe's StandAloneSig
table — where every managed route carries `Default`. So `callMethodWithCommitment` takes the call
site's convention as a parameter: `executeCalli` computes it from the standalone signature, and
every other caller passes `Managed`. A DU rather than a bool, since the function already carries
several positional flags and a fourteenth would be easy to pass wrongly.

The gate is deliberately one-directional: an *unmanaged* call site entering a *non*-UCO method is
undefined behaviour in real .NET rather than a diagnosed error, so there is no answer to be
faithful to, and the code says so.

## The classifier

Not `MethodInfo.isJITIntrinsic` (Domain/MethodInfo.fs:866-879), which was the first draft's
choice. It has `| con -> failwith $"TODO: {con}"` for constructor token kinds it does not handle,
and the `getMemberRefParentType` closure it depends on dies on any MemberRef parent that is not a
TypeReference. A `[UnmanagedCallersOnly]` application cannot itself provoke either, but the
classifier scans *every* attribute on the method, so a guest method carrying a C#-11 generic
attribute (MemberRef → TypeSpecification parent) would crash the interpreter.

`CustomAttribute.constructorParentName` (Domain/CustomAttribute.fs:263-300) handles every
encoding, and is what the newer `FieldInfo.hasThreadStaticAttribute` (FieldInfo.fs:141) uses.
Copy that. Note there is already a third attribute-name decoder at parse time
(`tryReadAttributeTypeName`, MethodInfo.fs:1024, used for `[UnsafeAccessor]` at :1146-1157) — do
not add a fourth.

Precomputed onto `MetadataMethodFacts`, as `IsUnmanagedCallersOnly`, rather than scanned at the
point of use. This reverses the earlier draft, on three facts established while reading the code:

- `CustomAttribute.Constructor` is a `MetadataToken`, not the `EntityHandle` that
  `constructorParentName` takes. A point-of-use scan therefore cannot reuse the Domain's decoder
  as-is: it would need either a fifth attribute-name decoder written against `DumpedAssembly`'s
  parsed tables, or a refactor of the public one. Parse time has the `MetadataReader` in hand and
  reuses `constructorParentName` verbatim.
- `FieldInfo.IsThreadStatic` — the precedent the earlier draft named for the *classifier* — turns
  out to precompute, for this reason stated in its own comment: "`[ThreadStatic]` is a custom
  attribute rather than a `FieldAttributes` flag, so it is computed once here at parse time rather
  than re-walking metadata at each access." Following the precedent means following that too.
- `callMethodWithCommitment` is the hottest path in the interpreter, and the gate is unconditional
  on it.

`MetadataMethodFacts` has exactly one construction site (MethodInfo.fs), so the blast radius is a
field addition rather than a sweep. It is a *derived* fact sitting beside the `CustomAttributes` it
is derived from, which is a denormalisation; the same shape is already accepted for
`FieldInfo.IsThreadStatic`, and the field is computed at the one place the array is built, so the
two cannot drift apart. Note `WoofWare.PawPrint.Domain` is a published package, so this is a
source-breaking addition for any external consumer constructing the record.

Measure with `WoofWare.PawPrint.Performance` before and after anyway: precompute should be free on
the call path, and a measurement is what says so.

Accepted risk, precedented on `hasThreadStaticAttribute` (FieldInfo.fs:126-128) and stated in the
code: the match is on namespace + name and does not verify the attribute type resolves to
corelib's, so a guest declaring its own `System.Runtime.InteropServices.UnmanagedCallersOnlyAttribute`
false-positives.

## PR 2 tests

Written first, observed failing, then mutation-tested.

1. `sourcesImpure/UnmanagedCallersOnlyDelegateInvoke.cs` — binds (asserting the bind *succeeds*,
   the half PawPrint already gets right and a too-eager fix would break, observed by printing
   `del.Method.Name` between bind and invoke), then invokes. The standard impure harness cannot
   host this: `TestImpureCases.runTest` hard-fails on a fatal outcome (TestImpureCases.fs:2085).
   It needs a bespoke fixture driving `BoundedRun` + `RealRuntime` directly, as `TestRaces.fs:45-64`
   and `TestBulkMoveCellAccess.fs:377-449` already do.
2. …`ReflectionInvoke.cs` — the same for `MethodInfo.Invoke`.
3. …`ForceEmitInvoke.cs` and …`ManagedCalli.cs` — the two routes the first draft wrongly called
   unreachable, now covered by the chokepoint.
4. `sourcesPure/UnmanagedCallersOnlyFunctionPointer.cs` — the control, exiting 0 on both runtimes:
   the legal `delegate* unmanaged<int,int>` call still returns 42, and a plain non-attributed
   method still works by delegate and by reflection. This will be the suite's first `sourcesPure`
   case that actually *invokes* an unmanaged function pointer (`FnPtrCallConvOverloadDispatch.cs`
   only passes them, `CalliManaged.cs` is managed-convention), so be ready to park it if it
   surfaces an unrelated blocker.
5. Mutants: (a) drop the gate; (b) invert the attribute predicate; (c) match the attribute on name
   only, ignoring namespace — killed only by a decoy `NotInterop.UnmanagedCallersOnlyAttribute` in
   the control guest, so the control must carry one; (d) treat every call site as `Managed`, which
   must break the legal function-pointer control; (e) treat every call site as `Unmanaged`, which
   must break tests 1-3; (f) emit `FailFast` where `ExecutionEngine` is meant, which must be killed
   by the kind assertion rather than the exit code, since both are 134 on Unix; (g) move the gate
   after frame commitment, which must be killed by a guest whose UCO method's declaring type has a
   `Console`-writing static constructor.

## Explicitly out of scope

- `GetDelegateCtor`'s catchable `NotSupportedException` — unreachable from C# (CS8902), and a
  JIT-time refusal PawPrint has no analogue for.
- Modelling cooperative/preemptive GC mode. It is CoreCLR's actual mechanism and would be the
  natural home for future reverse-P/Invoke work, but it is a large piece of new per-thread state
  whose only consumer would be this gate.
- `Marshal.GetDelegateForFunctionPointer` is unimplemented today. When it lands, a delegate
  wrapping a UCO method's *native* pointer is a legal invocation; if that is modelled as
  `FunctionPointerTarget.Managed` the gate would wrongly abort it. The gate's comment says so, and
  the classifier's contract is stated as "a *managed* entry aborts" so that host-initiated entries
  are exempt by construction.

---

# PR 2 — what actually happened

Shipped as planned: one gate in `callMethodWithCommitment`, ahead of class initialisation, keyed on
a `CallSiteConvention` the `calli` handler computes from its StandaloneSignature and every other
caller passes as `Managed`.

## Two departures from the plan

**The classifier precomputes** (see the amended section above). The deciding facts were found while
reading the code, not while planning.

**The gate's reach is narrower than "all four routes" suggested, and mutation testing is what said
so.** Mutating each managed call site's convention to `Unmanaged` in turn:

| mutant | outcome |
| --- | --- |
| the gate never fires | killed — every refusal guest + the ordering guest |
| the attribute predicate inverted | killed — control *and* every refusal guest |
| the attribute classifier ignores the namespace | killed — control, via its decoy attribute |
| `calli` reports its call site as cooperative | killed — control's legal `delegate* unmanaged` call |
| delegate `Invoke` treated as transitioning | killed — delegate guest + ordering guest |
| reflection's invoke treated as transitioning | killed — reflection guest **and ForceEmitInvoke** |
| `call` treated as transitioning | **survived** |
| `callvirt` treated as transitioning | **survived** |
| abort reports `COR_E_FAILFAST` | killed — the kind assertion, not the exit code |
| unmanaged convention classified as cooperative | killed — unit tests + control |
| vararg classified as transitioning | killed — unit tests only |
| the classifier reads the header only | killed — both modifier tests |
| the modifier walk stops at the outermost | killed — the multi-modifier test |
| the suppression modifier matched on name alone | killed — the fabricated decoy |

The reflection mutant killing *both* reflection guests is the interesting one: under PawPrint's
default, `Switch.System.Reflection.ForceEmitInvoke` changes nothing, because dynamic code is off and
CoreLib falls back to the interpreted invoke. So that guest is a distinct arrival on the oracle
only. Turning `IsDynamicCodeSupported` on does send it down the emit path, where it stops at an
unimplemented `ModuleHandle.ResolveMethod` for a method on a generic type — measured, and recorded
in the guest.

## The hole review found

Codex caught one, and it was real. `delegate* unmanaged[SuppressGCTransition]<int, int>` reaches
such a method and real .NET refuses it with the same fatal error as any managed entry — measured —
while the gate let it through. The suppression is *not* in the calling convention: both call sites
carry header `0x09`, and C# puts `SuppressGCTransition` in a `modopt` on the return type, so the
blobs are `09 01 08 08` against `09 01 20 49 08 08`. And the suppression is exactly what makes it
fatal, because the caller never leaves cooperative mode for the callee's prologue to find it in.

So the classifier had been asking the wrong question. `CallSiteConvention` became
`CallSiteTransition = EntersPreemptive | StaysCooperative`. The alternative — keep the convention DU
and add a separate suppression flag — was rejected because `CallSiteConvention.Unmanaged` would then
no longer justify the operation its callers use it for, which is the failure the "keep the
classifier truthful" rule exists to prevent.

Neither survivor is closable here. `callvirt` never can be: the attribute is legal only on a static
method and `callvirt` takes an object reference. `call` needs emitted IL, and the `DynamicMethod`
guest written for it hits a second, unrelated emit gap ("a dynamic method's Call names DynamicScope
entry 2 … which holds a System.RuntimeMethodHandle rather than a method"). Parked against the real
runtime alone; adding it to `cases` is the whole of the work once PawPrint can run an emitted
`call`.

## Cost

`StackHeavyProgramBenchmarks`, 20 iterations, interleaved base/gate/base/gate on one machine:

| round | base | gate |
| --- | --- | --- |
| 1 | 342.7 ms ± 41.7 | 338.3 ms ± 35.6 |
| 2 | 390.2 ms ± 45.4 | 396.8 ms ± 38.0 |

No detectable difference: the within-round gap is ~1.5% either way against ±40 ms error bars and
flips sign, while the *same base code* drifted 342 → 390 ms between rounds. Allocation was identical
in every run (1.04 GB, 133000 Gen0/1000 ops), which is the deterministic quantity and the one that
would move if the change did real per-call work. An earlier probe that *removed* the parse-time scan
measured 432 ms — slower than either, with strictly less work — which is what established that this
box cannot resolve a few percent.
