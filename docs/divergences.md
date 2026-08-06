# Known Divergences from CoreCLR

*Authorship: LLM*

PawPrint aims to be behaviourally equivalent to CoreCLR for managed programs. This document catalogues the cases where it is *deliberately* not, and explains why. Each item here is something that:

* a managed program can observe at runtime,
* is not simply unimplemented (those live in the `unimplemented` set in [`WoofWare.PawPrint.Test/TestPureCases.fs`](../WoofWare.PawPrint.Test/TestPureCases.fs) and crash with a TODO `failwith`),
* and is either spec-compliant or marked here as deferred work with a tracking note.

If a program of yours hits one of these, the workaround is usually to rewrite the affected code to not depend on the timing or exception type that CoreCLR's specific implementation chose. If you can't, file an issue.

## Type-initialiser (`.cctor`) timing for `BeforeFieldInit` types

**CoreCLR**: For a type whose `TypeAttributes` includes the `BeforeFieldInit` flag (Roslyn applies this to every type without an explicit `static T()`), the runtime may defer the `.cctor` past instance creation, instance method calls, and static method calls, and only commits to running it before the first *static-field access* on the type. In practice CoreCLR's JIT does defer.

**PawPrint**: Runs `.cctor` eagerly on `newobj`, `Activator.CreateInstance<T>()`, and the other instance-creation paths, regardless of `BeforeFieldInit`. A user program may therefore observe `.cctor` side effects (logging, `Tracker.Flag = true`, an exception thrown out of a field initialiser) one event earlier than CoreCLR would emit them.

**Spec status**: Compliant. ECMA-335 II.10.5.3.2 explicitly allows the runtime to run a `BeforeFieldInit` `.cctor` earlier than the latest legal moment; "eager always" is one of the legal schedules.

**Why we chose this**: PawPrint's headline goal is deterministic simulation. CoreCLR's `BeforeFieldInit` schedule depends on JIT inlining decisions, tier-up timing, and other runtime state that is hard to reproduce. A schedule of the form "always at the first use" is simpler to reason about and trivially reproducible across runs. We also already need a consistent rule because the static-access opcodes do not yet trigger `.cctor` themselves — closing both gaps coherently is a larger refactor than has been done so far.

**Observable example**:

```csharp
static class Tracker { public static bool CctorRan; }

class C {
    public static int N = SetFlag();
    static int SetFlag() { Tracker.CctorRan = true; return 0; }
    // No explicit static C() — Roslyn marks C as BeforeFieldInit.
}

var c = new C();
// CoreCLR:  Tracker.CctorRan is false here — .cctor hasn't fired yet.
// PawPrint: Tracker.CctorRan is true.
```

**Where this lives in code**: The eager triggers are `UnaryMetadataObjectOps.fs` (`newobj`) and the `Activator.CreateInstance<T>()` intrinsic in `IlMachineStateExecution.fs`. A trigger-aware central policy is sketched in the PR conversation that introduced the Activator intrinsic; nobody has implemented it yet.

## Last-bit results of the transcendental `Math` functions

**CoreCLR**: `Math.Pow` and its relatives are `[Intrinsic]` + `MethodImplOptions.InternalCall`, with no IL body; the JIT lowers each to a call into the host platform's C library. The answer is therefore whatever the machine's libm returns.

**PawPrint**: Computes them in-tree, from integer arithmetic only, in `DeterministicMath.fs`. The result depends on nothing but the arguments.

**Spec status**: Compliant, and strictly closer to the ideal than CoreCLR. IEEE 754 mandates correct rounding only for `+ - * /`, `sqrt` and `fma`; `pow` appears in clause 9.2 among the *recommended* operations, where correct rounding is recommended but not required, and C's Annex F does not require it either. Both implementations are therefore free to differ from each other in the last bit, and both do.

**Why we chose this**: Forwarding to the host would make a recorded run replay differently on a machine with a different libm — silently, rarely, and only in the last bit, which is the worst failure mode this project has. It is not a theoretical concern: measured against macOS/Arm's libm over 200 000 random `(base, exponent)` pairs, 25 disagreed. In every one of those 25 PawPrint returned the correctly rounded value and the host did not (independently confirmed with an 80-digit `decimal` computation; the host's error ranged from 0.500004 to 0.5102 ulp). Mainstream libms deliberately budget about 0.5 + ε ulp rather than pay the cost of resolving the table maker's dilemma near midpoints, so this is expected behaviour on their part — but it is not behaviour a deterministic runtime can inherit.

**Observable example**:

```csharp
// PawPrint returns the correctly rounded result; macOS/Arm's libm returns its neighbour.
double d = Math.Pow(667.32139499267623, 24.249516112846091);
```

**A second, coarser divergence, in the same place**: hosts also disagree about `pow` given a *signalling* NaN operand in one of the two cases that override a NaN — `pow(x, ±0)` and `pow(+1, y)`. IEEE 754 clause 9.2.1 grants those overrides against a "quiet NaN" specifically, so a signalling NaN falls back to clause 7.2 and comes back quietened; glibc implements exactly that, and Apple's libm returns 1 regardless. PawPrint specifies glibc's answer, since it is both the standard reading and the behaviour of the linux-x64 host CI differentially tests against. A guest can see the difference only through `BitConverter`, since C# has no way to write a signalling NaN literal.

**Where this lives in code**: `WoofWare.PawPrint/DeterministicMath.fs`, dispatched from the `Math.Pow` arm of `Intrinsics.fs`. Only `Pow` is implemented so far; `Sqrt`, `Log`, `Exp`, `Sin` and the rest of `Math.CoreCLR.cs` remain unimplemented and fail loudly, and should join this module rather than being forwarded to the host.
