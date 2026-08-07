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

**CoreCLR**: `Math.Pow`, `Math.Sin`, `Math.Cos` and their relatives are `[Intrinsic]` + `MethodImplOptions.InternalCall`, with no IL body; the JIT lowers each to a call into the host platform's C library. The answer is therefore whatever the machine's libm returns.

**PawPrint**: Computes them in-tree, from integer arithmetic only, in `DeterministicMath.fs`. The result depends on nothing but the arguments.

**Spec status**: Compliant, and strictly closer to the ideal than CoreCLR.

IEEE 754-2019 *requires* correct rounding only for `+ - * /`, `sqrt` and `fma`. `pow`, `sin` and `cos` sit together in clause 9.2's Table 9.1 of *additional* mathematical operations, which language standards "should define, to be implemented according to this subclause" — an opt-in. The accuracy rule is conditional on taking that option: "A conforming operation shall return results correctly rounded for the applicable rounding direction for all operands in its domain." So the standard's latitude is not "correct rounding is optional" but "providing a clause-9.2 operation at all is optional"; an implementation that does not opt in is simply outside the clause.

Neither C nor CoreCLR opts in. C leaves the accuracy of every `<math.h>` function implementation-defined (C17 §5.2.4.2.2), and explicitly permits an implementation to state that the accuracy is unknown — which is what GCC's documentation does. Annex F pins only special values: F.10.1.5 gives `cos(±0) = 1` and `cos(±∞) = NaN` with the invalid exception, and says nothing about any other argument. CoreCLR's `Math.Cos` is an `InternalCall` straight to that libm; .NET Core 3.0's IEEE-compliance work covered the *required* operations (`nextUp`/`nextDown`, `logB`, `fma`, parsing and formatting), not correct rounding of the transcendentals. Both implementations are therefore free to differ from each other in the last bit, and both do.

What clause 9.2 *does* bind, and PawPrint honours:

* `cos(±0)` is exactly +1 with no exception, and `sin(+0)` is `+0` while `sin(−0)` is `−0` (§9.2.1). The sign of a zero is specified here; the sign of a NaN, below, is not.
* `cos(±∞)` and `sin(±∞)` are quiet NaNs and signal invalid operation (Table 9.1).
* `cos(−x)` is `cos(x)` for every argument and every rounding attribute, because §9.2 requires an operation defined by an even mathematical function to be even. `sin(−x)` is `−sin(x)` under the same clause, but only "for roundTiesToEven, roundTiesToAway, and roundTowardZero" — the narrower scope is deliberate in the standard, since under a directed rounding an odd function need not be exactly odd. We round ties to even throughout, so both bind. These are real constraints rather than courtesies, and each is asserted both as a property in `TestDeterministicMath.fs` and in the end-to-end differential case.

**Why we chose this**: Forwarding to the host would make a recorded run replay differently on a machine with a different libm — silently, rarely, and only in the last bit, which is the worst failure mode this project has. It is not a theoretical concern: measured against macOS/Arm's libm over 200 000 random `(base, exponent)` pairs, 25 `pow` results disagreed. In every one of those 25 PawPrint returned the correctly rounded value and the host did not (independently confirmed with an 80-digit `decimal` computation; the host's error ranged from 0.500004 to 0.5102 ulp). `cos` behaves the same way: over 1 500 random doubles spanning the whole exponent range, 29 disagreed, always by exactly one ulp, and on every one of them PawPrint was the nearer to the exact value — 0.4996 ulp at worst against the host's 0.5004 to 0.6206, measured against a 1400-bit reference. `sin` likewise: 25 of another 1 500 disagreed, again all by one ulp, again with PawPrint nearer on every one — 0.4996 ulp at worst against 0.5004 to 0.6554. Mainstream libms deliberately budget about 0.5 + ε ulp rather than pay the cost of resolving the table maker's dilemma near midpoints, so this is expected behaviour on their part — but it is not behaviour a deterministic runtime can inherit.

**Observable example**:

```csharp
// PawPrint returns the correctly rounded result; macOS/Arm's libm returns its neighbour.
double d = Math.Pow(667.32139499267623, 24.249516112846091);
```

**A second, coarser divergence, in the same place**: hosts also disagree about NaN operands, where IEEE 754 leaves both the payload and, in places, the choice of answer open.

* `pow` given a *signalling* NaN operand in one of the two cases that override a NaN. Clause 9.2.1 words those two rules so as to exclude it: "pow(x, ±0) is 1 **if x is not a signaling NaN**" and "pow(+1, y) is 1 for any y (**even a quiet NaN**)". A signalling NaN therefore falls back to the general rule and comes back quietened; glibc implements exactly that, and Apple's libm returns 1 regardless. PawPrint specifies glibc's answer, since it is both the standard reading and the behaviour of the linux-x64 host CI differentially tests against.
* `sin` or `cos` given any NaN: PawPrint returns the input NaN quieted, payload and sign intact, while macOS/Arm's libm clears the sign. Here the standard declines to choose. §6.2.3 makes propagating the *payload* a recommendation, which both satisfy; the sign it does not specify at all — §6.3 says the standard "does not specify the sign bit of a NaN result, even when there is only one input NaN, or when the NaN is produced from an invalid operation". So both answers conform and PawPrint is picking one, for consistency with its own `pow` rather than because the standard prefers it. The same clause covers the NaN *generated* for `cos(±∞)`, on which x86 and Arm hardware differ.

A guest can see any of these only through `BitConverter`, since C# has no way to write a signalling NaN literal or to compare NaN payloads directly.

**Where this lives in code**: `WoofWare.PawPrint/DeterministicMath.fs`, dispatched from the `Math.Pow`, `Math.Cos` and `Math.Sin` arms of `Intrinsics.fs`. Only those three are implemented so far; `Tan`, `Log`, `Exp` and the rest of `Math.CoreCLR.cs` remain unimplemented and fail loudly, and should join this module rather than being forwarded to the host. (`Math.SinCos` is a different kind of thing and is also unimplemented: it has an IL body of its own, and bottoms out in a separate `SinCos(double, double*, double*)` InternalCall.)

`Math.Sqrt` is the exception to all of the above, and will not belong in this document when it lands: `sqrt` is one of the operations IEEE 754 *requires* to be correctly rounded, every platform implements it as a hardware instruction that is, and so a deterministic in-tree implementation and the host will agree bit-for-bit by construction. It is also, as of this change, the next primitive a guest hits — `PortableThreadPool`'s hill-climbing controller reaches it through the `Complex` arithmetic it performs on the wave components that `Math.Sin` and `Math.Cos` produce.
