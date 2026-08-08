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
* `sqrt` given a negative argument. This is the *only* way `Math.Sqrt` can diverge — see below — and it is the generated-NaN case again, straight from the hardware: x86's `sqrtsd` delivers the negative quiet NaN (the "indefinite" value, which is what `Double.NaN` is) and Arm's `fsqrt` the positive one. PawPrint returns the positive one, as it does everywhere else. A NaN *argument* to `sqrt` is not divergent at all: both architectures propagate it with sign and payload intact, quietening it if it was signalling, which is exactly what PawPrint does.

A guest can see any of these only through `BitConverter`, since C# has no way to write a signalling NaN literal or to compare NaN payloads directly.

**Where this lives in code**: `WoofWare.PawPrint/DeterministicMath.fs`, dispatched from the `Math.Pow`, `Math.Cos`, `Math.Sin` and `Math.Sqrt` arms of `Intrinsics.fs`. Only those four are implemented so far; `Tan`, `Log`, `Exp` and the rest of `Math.CoreCLR.cs` remain unimplemented and fail loudly, and should join this module rather than being forwarded to the host. (`Math.SinCos` is a different kind of thing and is also unimplemented: it has an IL body of its own, and bottoms out in a separate `SinCos(double, double*, double*)` InternalCall.)

`Math.Sqrt` is the exception to everything above, and appears in this section only for the NaN bullet. `squareRoot` is one of the operations IEEE 754 *requires* to be correctly rounded (§5.4.1, not the recommendation of §9.2), the JIT lowers it to a hardware instruction rather than to a libm call, and every platform's instruction obeys — so there is exactly one right answer for every argument and PawPrint returns it. `TestDeterministicMath.fs` asserts bit-for-bit agreement with the host as a property, which is an assertion the other three functions cannot make; `sourcesPure/MathSqrt.cs` correspondingly pins irrational roots exactly, where its `Pow`/`Cos`/`Sin` siblings can only assert identities and slack bounds.

Computing it in-tree anyway is not about changing the answer, then, but about where the guarantee comes from: the interpreter promises correct rounding on its own account rather than inheriting it from whatever machine the recording happened on, and the tests get an exact oracle out of it.

## `calli` through a null function pointer

**CoreCLR**: Faults. On osx-arm64 the process dies with SIGSEGV (exit 139); the null address is called directly, and nothing converts that into a managed exception the guest could catch. Because the fault is a hardware trap rather than a runtime check, exactly what a program observes depends on the platform's signal handling rather than on any CLI rule.

**PawPrint**: Raises a catchable `NullReferenceException` at the `calli` site, before consuming the function pointer or any arguments from the evaluation stack. A guest `try`/`catch (NullReferenceException)` around the call therefore runs.

**Spec status**: Compliant, and strictly closer to the specification than CoreCLR. ECMA-335 III.3.20 lists `NullReferenceException` as the exception `calli` throws when the function pointer is null.

**Why we chose this**: PawPrint has no host address space to fault in — a null function pointer is just a value we can recognise, so we are free to implement the specified behaviour rather than emulate a segfault. Reproducing the fault would mean tearing down the simulated process in a way that carries no information, and would make the interpreter's behaviour depend on the host platform's signal handling, which is precisely the kind of nondeterminism PawPrint exists to eliminate.

**Observable example**:

```csharp
delegate*<int, int> nil = null;
try { nil(1); }
catch (NullReferenceException) { /* PawPrint: reached. CoreCLR: process is already dead. */ }
```

**Testing note**: This cannot be a `sourcesPure` comparison test, because the two runtimes genuinely disagree about the outcome. The oracle cannot describe the fault: a process killed by a signal is reported by `Process.ExitCode` as `128 + signo`, indistinguishable from a guest that simply returned that number, so the oracle would call SIGSEGV a normal exit with code 139. It is covered by a PawPrint-only test, `calli through a null function pointer throws NullReferenceException` in `TestPureCases.fs`.

**Where this lives in code**: `UnaryMetadataCallOps.executeCalli`.

## `calli` through a punned function-pointer signature

**CoreCLR**: Runs the call. ECMA-335 III.3.20 defines `calli`'s marshalling by the call-site StandaloneSignature, and C# lets a guest cast a function pointer to a different signature of the same arity, so `((delegate*<int, long>)p)(3)` — where `p` is a `delegate*<int, int>` — is accepted and returns `3`. The argument direction behaves likewise: calling a `long`-taking target through an `int`-declaring call site returns `7` for input `7`. (Both verified standalone on osx-arm64.) Note that CoreCLR is relying on the platform ABI here: whether a narrower return value arrives with its upper bits in a usable state is an ABI property, not something the CLI specifies.

**PawPrint**: Refuses the call at the `calli` instruction, with an error naming the instruction, both types, and the fact that call-site marshalling is unimplemented.

**Spec status**: Non-compliant, deliberately and detectably. The specified behaviour is to marshal arguments and the result through the call-site signature; PawPrint drives invocation from the target's own `MethodInfo` instead, so it has no way to produce the widened result.

**Why we chose this**: Doing it properly means coercing arguments to the call-site parameter types on the way in and the result to the call-site return type on the way out, which requires carrying the call-site signature onto the frame and applying it in `returnStackFrame` — the return path shared by every call in the interpreter. That was out of scope for the change that introduced `calli`. The alternative to refusing was to let the call proceed, which is what the first implementation did: it died afterwards in `toCliTypeCoerced` with `TODO: Int32(3)`, at the `stloc` rather than at the `calli`, with nothing in the message connecting it to a function pointer. Failing at the faulting instruction with a message that says what is unimplemented is strictly more useful, and keeps the limitation visible rather than latent.

**Scope of the check**: The comparison starts from evaluation-stack representation (ECMA-335 III.1.1) rather than exact type, so signedness and sub-`int32` width may legitimately differ between the two signatures without being rejected — but it is not purely that model, because a `calli` also crosses a method boundary where the ABI footprint matters. `float32` and `float64` are therefore treated as conflicting even though both are `F` on the stack: reading a `float32` return slot as `float64` gives garbage on CoreCLR rather than the target's value, so permitting that pun would make PawPrint silently return the plausible answer where the real runtime returns nonsense.

Which conflations are safe was measured, not assumed. A bitmask probe over five puns (`short`, `byte`, `uint` and `float` returns, plus a signedness-punned parameter) on osx-arm64 gave CoreCLR 23 and PawPrint 31 — identical except for the float bit. That is why the integer widths and signedness are deliberately *not* split: there the two runtimes agree, and splitting them would reject calls that work today.

The check is a source of refusals only: non-primitive types and unsubstituted generic parameters are not classified and so are not compared, and the parameter lists are only compared element-wise when both signatures agree on which of them supplies `this`. Agreement therefore does not assert the call is well-typed — only that it is not one of the mismatches that can be detected cheaply.

**Observable example**:

```csharp
static int Id(int x) => x;
delegate*<int, int> p = &Id;
long r = ((delegate*<int, long>)p)(3);  // CoreCLR: r == 3. PawPrint: refused at the calli.
```

**Testing note**: Cannot be a `sourcesPure` comparison test, since CoreCLR succeeds and PawPrint deliberately does not. Covered by the PawPrint-only tests `calli refuses a punned return type at the faulting instruction` and `calli refuses a punned parameter type at the faulting instruction` in `TestPureCases.fs`.

**Where this lives in code**: `UnaryMetadataCallOps.executeCalli`, and the `CalliStackKind` classifier above it.

## The host-populated `AppContext` properties are absent

**CoreCLR**: Before `hostpolicy` looks at `runtimeOptions.configProperties` at all, it populates eight properties of its own and passes them to `AppContext.Setup` in the same arrays: `TRUSTED_PLATFORM_ASSEMBLIES`, `NATIVE_DLL_SEARCH_DIRECTORIES`, `PLATFORM_RESOURCE_ROOTS`, `APP_CONTEXT_BASE_DIRECTORY`, `APP_CONTEXT_DEPS_FILES`, `FX_DEPS_FILE`, `PROBING_DIRECTORIES` and `RUNTIME_IDENTIFIER` (`hostpolicy_context.cpp`), plus `HOST_RUNTIME_CONTRACT`, and conditionally `APP_PATHS` and `STARTUP_HOOKS`. They come from deps resolution and the host's filesystem layout, never from the config file — a `configProperties` entry that reuses one of those names is a fatal `LibHostDuplicateProperty` rather than an override, so the two sets are disjoint by construction. Every .NET process therefore starts with them, whatever its `runtimeconfig.json` says, and a config with no `configProperties` section still yields nine.

**PawPrint**: Populates none of them. `AppContext` contains exactly the `configProperties` the host passed in `HostConfig.AppContext`, and nothing else; with no properties at all, `AppContext.Setup` is never called and `s_dataStore` stays null (which is indistinguishable from an empty store through the public API, since `GetData` returns null for a null store and `SetData` lazily installs one).

**Spec status**: Outside ECMA-335, which says nothing about host properties — this is the hosting contract rather than the CLI. Non-compliant with that contract, deliberately.

**Why we chose this**: These properties describe a host PawPrint does not have. There is no deps resolution, no probing, no runtime identifier and no assembly directory layout to derive them from: the interpreter is handed a list of framework directories directly, and binds by simple name against the first hit. Synthesising plausible-looking values would be worse than omitting them, because a guest that branches on a TPA entry would then take a path justified by a path list that describes nothing real. Omission at least fails in the direction the guest can detect.

Note that this is *not* the same as "what a guest sees when there is no `runtimeconfig.json`". A real host treats a missing config as a self-contained app, fails to find `hostpolicy` beside the assembly, and exits before any managed code runs (verified: exit 131 on osx-arm64). "No config file, so no properties" exists only in PawPrint, and is likewise deliberate — the test harness compiles guests to a `MemoryStream` where no sidecar file can exist.

**Observable example**:

```csharp
var tpa = AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES");
// CoreCLR:  a ';'-separated list of every framework and app assembly path.
// PawPrint: null.

Console.WriteLine(AppContext.BaseDirectory);
// CoreCLR:  the APP_CONTEXT_BASE_DIRECTORY property, i.e. the app's directory.
// PawPrint: falls through to AppContext's GetBaseDirectoryCore() fallback.
```

**Where this lives in code**: `AppContextProperties.empty` in `RuntimeConfig.fs` documents the gap; `HostConfig.AppContext` is where a host would supply values if it had any. Closing this would mean deciding what a simulated app's filesystem layout *is*, which is a larger question than the seeding change that surfaced it.
