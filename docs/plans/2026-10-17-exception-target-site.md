# `ExceptionNative_GetMethodFromStackTrace`: `Exception.TargetSite` and `Source`

PR 4 of `2026-08-09-frozen-stack-trace.md`. It is also the first dependency of making .NET 11's
`WrapNonCompliantException` work where the wrapper's constructor raises: there, the runtime runs
`Exception.InternalPreserveStackTrace` on what the constructor raised, and that reads `Source`, which
reads `TargetSite`, which calls this QCall.

## What CoreCLR does

`comutilnative.cpp` (`ExceptionNative_GetMethodFromStackTrace`, identical on .NET 10 and 11): take
the `_stackTrace` array (an `sbyte[]`, or an `object[]` whose element 0 is that array), read
element 0 of the `StackTraceArray`, which is the frame the exception was raised in, and hand back
`pMD->LoadTypicalMethodDefinition()->AllocateStubMethodInfo()`, a fresh `RuntimeMethodInfoStub` for
the typical definition: both the declaring type's and the method's instantiations stripped.
Managed code then runs `RuntimeType.GetMethodBase(IRuntimeMethodInfo)`, and `TargetSite` caches the
result in `_exceptionMethod`. `Source` is `TargetSite?.Module.Assembly.GetName().Name`, latched in
`_source`.

The QCall is only reached when `_stackTrace` is non-null; CoreCLR asserts the array is non-empty.

## Measured (spike, real .NET and PawPrint)

A throwaway arm answering the QCall unconditionally from the head of PawPrint's frozen frame list,
via `MethodHandleRegistry.typicalMethodDefinition` and `allocateFreshStubOfIdentity`, was enough on
.NET 10 for every guest-thrown shape below: PawPrint and real .NET agree. Nothing past the QCall was
missing. On .NET 11 the guest then stops at the known frontier
`Net11Frontier.runtimeMethodInfoValue` (`IRuntimeMethodInfo.GetValue` reads
`Unsafe.As<RuntimeMethodInfoStub>(rmi).m_value`), which `RuntimeType.GetMethodBase` reaches.

Guest-thrown shapes, all agreeing:
* a plain `throw` in a method: `TargetSite` is that method, `Source` its assembly's simple name,
  and a second read returns the same object;
* a `throw` in `Gen<int>.Throw<string>()`: `TargetSite.DeclaringType.IsGenericTypeDefinition` and
  `TargetSite.IsGenericMethodDefinition` both hold (the typical definition);
* `ArgumentNullException.ThrowIfNull((object)null)`: `TargetSite` is the `[StackTraceHidden]`
  `ArgumentNullException.Throw`, and `Source` is `System.Private.CoreLib`; hidden frames count;
* `throw;` from an outer handler keeps the original site;
* `throw e;` of a caught exception names the new site if `TargetSite` was not read before, and the
  old one if it was (the `_exceptionMethod` latch);
* `ExceptionDispatchInfo.Throw` names the captured site;
* a never-thrown exception has a null `TargetSite` and `Source`, without reaching the QCall.

**Runtime-raised exceptions disagree.** Measured on both runtimes (macOS arm64), with
`[MethodImpl(NoInlining)]` on each method:

| raise | real `TargetSite` |
|---|---|
| `a[i]` out of range | `Internal.Runtime.CompilerHelpers.ThrowHelpers.ThrowIndexOutOfRangeException` |
| `(string)o` failing | `CastHelpers.ChkCastAny_NoCacheLookup` |
| `(int)o` failing | `CastHelpers.ThrowInvalidCastExceptionInternal` |
| `a[0] = o` covariance | `CastHelpers.ThrowArrayMismatchException` |
| `a / 0` | `ThrowHelpers.ThrowDivideByZeroException` |
| `checked(a + b)` | `ThrowHelpers.ThrowOverflowException` |
| `.cctor` failure (the TIE) | `InitHelpers.InitClassSlow` |
| field load, `callvirt`, `ldlen` on null | the method itself |
| `new int[-1]` | the method itself |

All of those helpers are `[StackTraceHidden]` managed CoreLib code that CoreCLR's JIT calls, and
PawPrint performs the check itself without such a frame, so its head frame is the method. Which
helper the JIT picks is JIT and CPU dependent (x64 relies on the hardware trap for division by zero,
where arm64's JIT emits an explicit check; the cast helper depends on the cast's shape), so this is
not something to model by a table here (see the memory "refuse rather than model JIT-tier
behaviour"). Answering from PawPrint's head frame would be silently wrong for most runtime raises,
including `Source`, which would name the guest's assembly rather than `System.Private.CoreLib`.

## Design

Answer the QCall only where PawPrint's head frame is the frame CoreCLR's `stackArray[0]` names,
and refuse everywhere else.

**Where the head frame came from** becomes data that travels with the frames, as one value:

```fsharp
/// Which raise put an exception's first frame there, when it was not an IL `throw`.
type RuntimeRaiseSite =
    /// A frame the runtime pushed to construct an exception it raises (`DispatchAsException`):
    /// an instruction's own check, a native, an intrinsic.
    | MadeByRuntime
    /// A failed type initialiser's `TypeInitializationException` (`applyFrameWraps`).
    | TypeInitializerFailed
    /// The cached `TypeInitializationException` raised again on a later access (`loadClass`).
    | TypeInitializerFailedEarlier
    /// A `TargetInvocationException` the runtime wraps around what a constructor or initialiser it
    /// called raised (`applyFrameWraps`, and the reflective field accessors' cached rethrow).
    | TargetInvocationWrap

/// How the first frame of an exception's trace came to be there, which decides whether it is the
/// frame CoreCLR's trace begins with.
type TraceHeadOrigin =
    /// An IL `throw` executing in that frame raised it (guest code or CoreLib's own, such as a
    /// `ThrowHelper`). CoreCLR's `IL_Throw` is native, so its trace begins at the same frame.
    | IlThrow
    /// The runtime raised it on the frame's behalf. CoreCLR often raises these from a managed helper
    /// PawPrint runs no frame for.
    | RuntimeRaise of RuntimeRaiseSite

/// An exception's frames, innermost first, with where the first one came from.
type ExceptionTrace = { Frames : ExceptionStackFrame<...> list ; HeadOrigin : TraceHeadOrigin }
```

* `CliException.StackTrace : ExceptionStackFrame list` becomes `Trace : ExceptionTrace`, and
  `IlMachineState.FrozenStackTraces` maps a token to an `ExceptionTrace`. Renaming the field makes
  the compiler find every reader and writer, and nothing can copy frames without their origin: the
  `rethrow` path re-reads the *current* token's trace (`NullaryIlOp.fs`, the stale-snapshot
  comment), so an origin held anywhere but beside the frames would be the snapshot's while the
  frames were the token's. A sibling of `frozenStackTraceFrames` returns the whole trace;
  `UnhandledExceptionReport` and `TestImpureCases`, which read the map directly, take `.Frames`.
* `throwExceptionObject` takes the origin as a parameter, so no caller can forget it. On `main`:
  the `throw` opcode passes `IlThrow`; the `ret` of a constructor frame the runtime pushed
  (`DispatchAsException`) passes `RuntimeRaise MadeByRuntime` (every such dispatch on `main` is
  the runtime's own; a thrown non-exception is dispatched unwrapped and never gets a trace); the
  cached-TIE rethrow in `loadClass` passes `TypeInitializerFailedEarlier`, and the reflective
  field accessors' rethrow of it, wrapped, `TargetInvocationWrap`.
* The wrappers `applyFrameWraps` makes start a fresh trace with `TypeInitializerFailed` or
  `TargetInvocationWrap`. `appendCallerFrame` appends to the tail and leaves the origin alone.
* `ExceptionDispatchInfo.Throw`'s splice (`consumeForeignExceptionRaise`) restores the captured
  trace in front: if it has frames, its origin is the trace's origin, because its head is the
  trace's head; if it has none (the captured exception was never thrown), the splicing `throw`'s
  own `IlThrow` stands, because its frame is then the head, as it is in CoreCLR.
* A filter is the only guest code that runs between two appends of one raise, and it may raise
  the same object again (an `ExceptionDispatchInfo.Throw` of a capture of it), which changes the
  trace the object holds. CoreCLR re-reads `_stackTrace` at every append, so once a filter has
  run, whether it returned or an exception escaped it, the search takes up the object's trace,
  frames and origin, before continuing or concluding (`takeUpTraceAfterFilter`). A search that
  reached its filter with no frames (a rethrow, from its own frame, of an exception whose trace
  was cleared) minted no token on entry and continues as it was. Found by the Codex reviews;
  pinned by `sourcesPure/FilterSpliceContinuesRestoredTrace.cs`,
  `sourcesPure/FilterOverTracelessRethrow.cs` and three refusal cases.
* PR #1943, when it lands on top of this, adds `RuntimeExceptionMaking.WrappedNonException`,
  whose `ret` must pass `IlThrow` (`IL_Throw` raises the wrapper from the `throw`; the match on the
  DU forces the decision), and a `RaisedAgainFromCaller` re-raise, which re-reads the token and so
  takes the whole `ExceptionTrace` with it.
* The QCall reads the token through `ObjectHandleOnStack`, looks up the trace (failing loudly on
  an unregistered token, as `frozenStackTraceToken` does), and:
  * refuses an empty frame list, which CoreCLR asserts against (unreachable by construction, since
    `recordThrownStackTrace` mints no token for an empty trace; stated defensively);
  * on `IlThrow`, names the head frame: a metadata method through `typicalMethodDefinition` and
    `allocateFreshStubOfIdentity` (fresh, as `AllocateStubMethodInfo` is); a dynamic method is
    refused, because `RuntimeType.GetMethodBase` then needs `RuntimeMethodHandle.GetResolver`,
    which PawPrint does not implement, and the managed side could not be tested; any other
    synthesised kind is refused (no such frame executes an IL `throw`);
  * on `RuntimeRaise site`, refuses with a TODO naming the site, saying that CoreCLR's trace may
    begin in a managed helper PawPrint runs no frame for.

The refusal reaches further than CoreLib-helper raises: a `NullReferenceException` from a field
load, `callvirt` or `ldlen` on null, and `new int[-1]`'s `OverflowException`, are `MadeByRuntime`
too, though real .NET names the method itself there. That is deliberate (the same site raises
both kinds, and which kind depends on the JIT and CPU), and it means the commonest exception a
guest might read `Source` of is refused. A later per-raise table can answer those.

Alternatives considered:
* **Answer unconditionally from the head frame** (the spike). Smallest, and silently wrong for most
  runtime raises (the table above). Rejected.
* **Record the helper frame CoreCLR would have run**, as a hidden head frame for each runtime raise.
  Most faithful, and the managed renderers would skip it through `[StackTraceHidden]`, but which
  helper is JIT and CPU dependent, so it needs a table keyed on the target CPU and the cast/JIT
  shape. A later PR can add it one raise at a time on top of this design: an entry that knows its
  helper turns a `RuntimeRaise` into a recorded frame with `IlThrow`-like status. Deferred.
* **Classify by inspecting the head frame's instruction** at QCall time (is the IL at `IlOffset` a
  `throw`?). No new state, but it fails for the wrapper (the `throw` there raised a different
  object than it threw), cannot tell an EDI splice's restored head apart, and makes the QCall
  depend on re-reading IL. Rejected: the origin is a fact of the raise, so it is recorded there.

Divergence inherited, not introduced: PawPrint runs no JIT inlining, so where CoreCLR inlines a
throwing method into its caller, its trace and `TargetSite` name the caller. That already holds for
rendered traces; the tests use `[MethodImpl(NoInlining)]`. The differential oracle runs guests at
Tier0, which does not inline, so a guest without the attribute would agree with the oracle and
still disagree with Tier1 or ReadyToRun code.

## Tests, first

A test-only commit, with the failing output, comes before the implementation.

1. `sourcesPure/ExceptionTargetSite.cs`: every guest-thrown row above, plus
   `ExceptionDispatchInfo.Capture(new X()).Throw()` (the head is `ExceptionDispatchInfo.Throw`,
   `Source` CoreLib) and `throw e` of a caught *runtime-raised* `e` (answered: the later IL `throw`
   replaces the origin rather than latching it on the object). Exit-code checks against the real
   runtime; fails at the unimplemented QCall before the change.
2. `TestExceptionTargetSiteRefusal`: PawPrint refuses, naming the site, where real .NET's
   `Source` is `System.Private.CoreLib`: an index out of range and a failing cast (`MadeByRuntime`);
   a type initialiser's failure (`TypeInitializerFailed`) and its cached rethrow
   (`TypeInitializerFailedEarlier`); `Activator.CreateInstance<T>` over a throwing constructor and
   the reflective field read of a failed initialiser's field (`TargetInvocationWrap`); a `rethrow`
   of a runtime raise; an `ExceptionDispatchInfo` capture of one, thrown again; and the sequence
   in which a stale origin would answer: a runtime raise captured, thrown again by IL (`IlThrow`),
   caught, spliced by `edi.Throw()` inside the handler, then `rethrow`n.
3. The wrapper case (`RuntimeWrappedException` around a thrown string: `TargetSite` is the method
   containing the `throw`) follows in #1943's branch, since only it wraps.
4. On .NET 11, park 1 on `Net11Frontier.runtimeMethodInfoValue`. 2 runs on both: its reflective
   and `Activator` raises reach the refusal on .NET 11 too (measured).

Mutation checks: flip the `throw` opcode's `IlThrow` to a `RuntimeRaise` and `Ret`'s to `IlThrow`;
take `IlThrow` for every splice, and the captured origin for an empty one; keep the snapshot's origin
in `rethrow`; drop the typical stripping. Each must turn a test red.

Docs: `docs/divergences.md`'s delegate entry says `TargetSite` is unreachable; update it. Add no new
divergence entry: a refusal is the ordinary unimplemented state.

## Out of scope

* The `.NET 11` frontier `runtimeMethodInfoValue`, which the wrap work needs next.
* A `NativeContractTable.tsv` row for this QCall (the escape analysis treats it as opaque today).
