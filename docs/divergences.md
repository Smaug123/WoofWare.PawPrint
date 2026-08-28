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
* `truncate` given a *signalling* NaN. This one is not a disagreement between platforms but a disagreement within a single CoreCLR: `Math.Truncate` has two implementations and they differ. The JIT expands the call to `frintz` (or `roundsd` with an immediate of 11), which quietens a signalling NaN as IEEE 754 §7.2 requires; but the expansion is conditional — on x64 it needs SSE4.2, and under MinOpts or at tier 0 it does not happen at all — and the managed body that runs instead is `ModF(d, &d); return d;`, reaching the platform C library's `modf`. Apple's `modf` hands a signalling NaN straight back unquietened. So real .NET's answer for that argument depends on how the *call site* was compiled rather than on the argument. Measured on osx-arm64 over the twelve NaN shapes, twenty-eight named special values, 28 007 quarter-integers and two million pseudorandom bit patterns, the two routes differ on the six signalling NaNs and on nothing else. PawPrint follows the instruction, which is also what `ceiling` and `round` do on every route.

A guest can see any of these only through `BitConverter`, since C# has no way to write a signalling NaN literal or to compare NaN payloads directly.

**Where this lives in code**: `WoofWare.PawPrint/DeterministicMath.fs`, dispatched from the `Math.Pow`, `Math.Cos`, `Math.Sin`, `Math.Sqrt`, `Math.Ceiling`, `Math.Truncate` and `Math.Round` arms of `Intrinsics.fs`. Only those seven are implemented so far; `Floor`, `Tan`, `Log`, `Exp` and the rest of `Math.CoreCLR.cs` remain unimplemented and fail loudly, and should join this module rather than being forwarded to the host. (`Math.SinCos` is a different kind of thing and is also unimplemented: it has an IL body of its own, and bottoms out in a separate `SinCos(double, double*, double*)` InternalCall.)

`Math.Sqrt`, `Math.Ceiling`, `Math.Truncate` and `Math.Round` are the exceptions to everything above, and `Math.Sqrt` and `Math.Truncate` appear in this section only for their NaN bullets. `squareRoot` is one of the operations IEEE 754 *requires* to be correctly rounded (§5.4.1, not the recommendation of §9.2), the JIT lowers it to a hardware instruction rather than to a libm call, and every platform's instruction obeys — so there is exactly one right answer for every argument and PawPrint returns it. `TestDeterministicMath.fs` asserts bit-for-bit agreement with the host as a property, which is an assertion the transcendental functions cannot make; `sourcesPure/MathSqrt.cs` correspondingly pins irrational roots exactly, where its `Pow`/`Cos`/`Sin` siblings can only assert identities and slack bounds.

`Math.Ceiling` and `Math.Round` are further out still: `roundToIntegralTowardPositive` and `roundToIntegralTiesToEven` (both §5.9) are not merely correctly rounded but *exact*, and neither generates a NaN of its own, so they cannot diverge in any way at all — not even in the one NaN bullet that `Math.Sqrt` appears here for. Their host-agreement tests in `TestDeterministicMath.fs` therefore carry no table of permitted alternatives, unlike their `pow`, `sin` and `sqrt` counterparts, and `sourcesPure/MathCeiling.cs` and `sourcesPure/MathRound.cs` pin every result exactly — including the negative zero that IEEE 754 requires from `ceiling` for an argument strictly between -1 and 0, and from `round` for an argument in [-1/2, 0).

`Math.Truncate` is `roundToIntegralTowardZero` (§5.9), so it is exact in the same way and generates no NaN of its own either — and yet it *is* divergent, for a reason that has nothing to do with the operation. Real .NET implements it twice, and the two implementations disagree about a signalling NaN; the bullet above gives the measurement. So its host-agreement test does carry an exemption, though a much narrower one than `pow`'s or `sin`'s: it admits the un-quietened answer for a signalling NaN and nothing else, which is exactly the set the measurement found. Every finite argument is pinned exactly, and `sourcesPure/MathTruncate.cs` pins the whole of what a guest can reach — including the negative zero IEEE 754 requires for an argument strictly between -1 and 0, and the positive zero it requires between 0 and 1, which is the one sign rule that distinguishes this operation from `ceiling`.

`Math.Round` and `Math.Truncate` differ from the other five in one further respect, which is why they are worth naming here rather than being allowlisted in `safeIntrinsics` and run: CoreCLR gives both an IL body. Neither body is a definition. `Round`'s is a managed emulation of the instruction the JIT actually emits (`roundsd` with mode 0, or `frintn`), obtaining ties-to-even from the ambient rounding mode by computing `(a + 2^52) - 2^52`; executing it would make the answer a property of whatever performed that addition rather than of this runtime. `Truncate`'s cannot be executed at all: it is `ModF(d, &d); return d;`, and `ModF` is itself `InternalCall` with no IL, so allowlisting `Truncate` would only move the failure one frame down.

Computing these four in-tree anyway is not mainly about changing the answer, then, but about where the guarantee comes from: the interpreter promises the specified result on its own account rather than inheriting it from whatever machine — or whatever optimisation tier — the recording happened on, and the tests get an exact oracle out of it.

## `calli` through a null function pointer

**CoreCLR**: Faults. On osx-arm64 the process dies with SIGSEGV (exit 139); the null address is called directly, and nothing converts that into a managed exception the guest could catch. Because the fault is a hardware trap rather than a runtime check, exactly what a program observes depends on the platform's signal handling rather than on any CLI rule.

**PawPrint**: Raises a catchable `NullReferenceException` at the `calli` site, before consuming the function pointer or any arguments from the evaluation stack. A guest `try`/`catch (NullReferenceException)` around the call therefore runs.

**Spec status**: Unspecified, so both behaviours are permitted and neither is closer to the specification than the other. ECMA-335 III.3.20's "Exceptions" clause lists `System.SecurityException` and nothing else — contrast III.4.2 for `callvirt`, which says in as many words that "System.NullReferenceException is thrown if obj is null". What III.3.20 does say is in its "Correctness" clause: `ftn` must be "a method pointer to a method that can be legitimately called with the arguments described by callsitedescr". A null pointer is therefore not correct CIL, and the CLI does not say what an implementation does when handed incorrect CIL. The choice here is ours to make on other grounds, and the grounds are below.

**Why we chose this**: PawPrint has no host address space to fault in — a null function pointer is just a value we can recognise, so we are free to choose a deterministic and catchable answer rather than emulate a segfault. Reproducing the fault would mean tearing down the simulated process in a way that carries no information, and would make the interpreter's behaviour depend on the host platform's signal handling, which is precisely the kind of nondeterminism PawPrint exists to eliminate. `NullReferenceException` is the exception the CLI gives to the analogous case it *does* specify — `callvirt` on a null receiver — so it is the answer a guest author is least surprised by.

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

## `Assembly.Location` is empty for every assembly

**CoreCLR**: Returns `pAssembly->GetPEAssembly()->GetPath()` (`assemblynative.cpp`, `AssemblyNative_GetLocation`) — the path the assembly was loaded from. For an app launched as `dotnet app.dll`, both the app's own assembly and every framework assembly report a real absolute path. The empty string is reserved for assemblies with no file backing: byte-array loads (`Assembly.Load(byte[])`), dynamic assemblies, and single-file-published apps.

**PawPrint**: Returns the empty string for *every* assembly, which is that no-file-backing state.

**Spec status**: Compliant. `Location` returning an empty string is a documented, first-class CoreCLR state that the BCL itself handles — see the suppression justification above `AppContext.GetBaseDirectoryCore`, "Single File apps should always set APP_CONTEXT_BASE_DIRECTORY therefore code handles Assembly.Location equals null". PawPrint is structurally exactly that shape of app.

**Why we chose this**: A guest under PawPrint has no filesystem, so there is no path it could open. The two alternatives are both worse:

* *Report the host's real path.* The framework assemblies resolve from `HostConfig.DotnetRuntimeDirs`, so a guest would observe the developer's `/nix/store/...` layout, and a recorded trace would depend on the machine that produced it. That is the determinism leak the whole `EmulatedKernel` design exists to prevent.
* *Synthesise a plausible path.* Deterministic, but a fiction: nothing is there, the guest cannot act on it, and it would become actively wrong once an emulated filesystem exists and does not contain that file.

The empty string is simply true, and it is what the runtime already reports for an app with no assembly files to point at.

**Knock-on effect**: `AppContext.BaseDirectory` is likewise empty. Its fallback `GetBaseDirectoryCore()` computes `Path.GetDirectoryName(Assembly.GetEntryAssembly()?.Location)`, and `GetDirectoryName("")` returns `null`, so the fallback returns `string.Empty`. Note this is only the *fallback*: on a real host `BaseDirectory` never reaches it, because `hostpolicy` always supplies the `APP_CONTEXT_BASE_DIRECTORY` property (see CoreCLR's own comment in `gcheaputilities.cpp`, "The APP_CONTEXT_BASE_DIRECTORY is always set by the host"). PawPrint does not yet populate that property; when it does, guests will see a real directory there by the same route a real host uses, and this entry will cover only `Location` itself.

**Observable example**:

```csharp
// dotnet app.dll:  "/path/to/app.dll", then "/path/to/"
// PawPrint:        "",                 then ""
Console.WriteLine(typeof(Program).Assembly.Location);
Console.WriteLine(AppContext.BaseDirectory);
```

**Testing note**: Cannot be a `sourcesPure` comparison test, since the real runtime is launched from a real `.dll` and reports its path — there is no cross-runtime fact to assert. Covered by the PawPrint-only `sourcesImpure/AssemblyLocationEmpty.cs`, which asserts the empty `Location` for both the guest's own assembly and a framework assembly, the resulting empty `AppContext.BaseDirectory`, and `ReferenceEquals(asm.Location, string.Empty)` — CoreCLR's `StringObject::NewString` hands back the shared empty-string instance for a zero-length string, so allocating a fresh empty string here would be observably wrong.

**Where this lives in code**: `NativeRuntimeAssembly.tryExecuteQCall`, the `AssemblyNative_GetLocation` case.

## `Assembly.CodeBase` throws, and `AssemblyName.CodeBase` is null

**CoreCLR**: `AssemblyNative_GetCodeBase` returns the assembly's path as a `file://` URL, and returns `TRUE`. It takes its other branch — set the empty string, return `FALSE` — only for an image that `IsInBundle` or `IsExternalData` (`PEAssembly::GetCodeBase`, `peassembly.cpp`). The managed wrapper turns a `FALSE` return into `null`, which makes `AssemblyName.CodeBase` null and the public `Assembly.CodeBase` throw `NotSupportedException` (`SR.NotSupported_CodeBase`).

**PawPrint**: reports every assembly the bundle/external way — empty string, `FALSE`.

**This is a narrower claim than the empty `Location` above, not the same one restated.** CoreCLR's pathless images do not all behave alike here, so `Location == ""` does not determine what `CodeBase` does:

| shape | `Location` | `GetCodeBase` | `AssemblyName.CodeBase` | `Assembly.CodeBase` |
| --- | --- | --- | --- | --- |
| loaded from a file | the path | `TRUE`, a `file://` URL | that URL | that URL |
| `Assembly.Load(byte[])` | `""` | `TRUE`, `""` | `""` | CoreLib's code base¹ |
| single-file / bundled | `""` | `FALSE` | `null` | throws `NotSupportedException` |

¹ `Assembly.CodeBase` substitutes `typeof(object).Assembly.CodeBase` when the answer is empty — "for backward compatibility, return CoreLib codebase for assemblies loaded from memory".

A byte-array image is built by `PEImage::CreateFromByteArray` with a null path but no probe extension, so it is neither bundled nor external and takes the *first* branch. PawPrint therefore had to pick between the last two rows, and picks the single-file one.

**Spec status**: Compliant. This is the state a single-file-published app is in, which the BCL handles as a first-class case — `Assembly.CodeBase` is `[Obsolete]` precisely because it cannot be answered for such apps.

**Why we chose this**: the guest has no filesystem at all, so "there is no code base, and asking for one is not supported" is the truthful answer; `NotSupportedException` says exactly that. The byte-array row would instead answer "the code base is the empty string", and then route callers through the CoreLib-substituting back-compat branch — which under PawPrint yields the empty string anyway, so the guest learns nothing truer and the fiction is longer. It also keeps `Location` and `CodeBase` telling one consistent story about what kind of app this is.

**Note the empty string is still written.** CoreCLR's `retString.Set(codebase)` sits *outside* the `if`, so both branches write; only the returned `BOOL` distinguishes them. PawPrint writes it too. Skipping the write happens to reach the same managed answer — the wrapper discards the string when the bool is false — but it is not what the primitive does, and would be wrong the moment a caller reads the string on a true return.

**Observable example**:

```csharp
// dotnet app.dll:  "file:///path/to/app.dll", then "file:///path/to/app.dll"
// PawPrint:        null,                      then a NotSupportedException
// (GetName() first: reading Assembly.CodeBase throws, so it has to come last.)
Console.WriteLine(typeof(Program).Assembly.GetName().CodeBase ?? "<null>");
Console.WriteLine(typeof(Program).Assembly.CodeBase);
```

**Testing note**: Cannot be a `sourcesPure` comparison test, for the same reason as the entry above. Covered by the PawPrint-only tests `GetCodeBase reports no code base, and still writes the string` and `GetCodeBase reports no code base for a framework assembly too` in `TestAssemblyNativeQCalls.fs`, which pin the `FALSE` return, the written canonical empty string, and both an ordinary and a framework assembly.

**Where this lives in code**: `NativeRuntimeAssembly.tryExecuteQCall`, the `AssemblyNative_GetCodeBase` case.

## `Environment.ProcessPath` reports no executable, and is never resolved against the filesystem

**CoreCLR**: `SystemNative_GetProcessPath` (`pal_process.c:898-901`) is `return minipal_getexepath();`, and both of the arms PawPrint models in `minipal_getexepath` (`src/native/minipal/getexepath.h`) end in `realpath(..., NULL)` — macOS on the buffer `_NSGetExecutablePath` filled, Linux on `/proc/self/exe` and then on `AT_EXECFN`. So the answer is a `malloc`'d canonical path, and it exists only if every component resolved. Measured on .NET 10.0.7: `dotnet app.dll` reports the *muxer* (`/usr/share/dotnet/dotnet`), an apphost-launched app reports the apphost. The app's own `.dll` is never the answer — that is `GetCommandLineArgs()[0]`.

**PawPrint**: reports whatever `KernelConfig.ProcessPath` says. That defaults to `None`, which the handler answers as the C does for a process whose executable no longer resolves: a NULL return with errno `ENOENT`, which CoreLib turns into a null `Environment.ProcessPath`. A host that names a path gets that path back verbatim — **it is not resolved against the emulated filesystem**, so `File.Exists(Environment.ProcessPath)` can be false where every real Unix makes it true.

**Spec status**: Outside ECMA-335, which says nothing about how a process learns its own image path — this is the PAL rather than the CLI. Compliant with the *API* contract in both halves: `Environment.ProcessPath` is documented to "return null when the path is not available", `Interop.Sys.GetProcessPath()` is declared `string?` precisely to carry that, and `Environment.ProcessPath` caches `GetProcessPath() ?? ""` to handle it. The API also documents that if the executable "is renamed or deleted before this property is first accessed, the return value is undefined", which is the licence the non-resolution half sits under.

**Why we chose this**: two separate decisions.

*The default is `None` because it is true.* PawPrint models no `exec(2)`: no file started this process, and the emulated filesystem holds no image of one. The alternatives are both worse:

* *Synthesise a plausible path* (say `/pawprint`). This is the same fiction rejected under "`Assembly.Location` is empty for every assembly" above, for the same reason — nothing would be there, so the guest could not act on it — and it would leave `File.Exists(Environment.ProcessPath)` false, which is the one thing `realpath` guarantees on a real Unix. Upstream does have a fixed-path arm (`TARGET_WASM` returns `"/managed"`), but only because wasm's packaging convention guarantees a file is there; that is this option *plus* seeding, not this option.
* *Refuse — `failwith` naming the unset knob.* The house move when PawPrint would otherwise invent something, but its precondition does not hold here: the flavours agree on an answer, and the distinguishing state is modelled (it is the config field). Refusing would trade a first-class BCL state for a host crash.

Answering NULL is not an invention: it is the state both flavours report for a live process whose executable has been unlinked. Measured, by having a guest delete its own executable before its first read — macOS arm64 and Linux arm64 both give NULL, errno 2, and a null `Environment.ProcessPath`.

*The configured path is answered verbatim* because resolving it would buy a whole errno surface (`ENAMETOOLONG`, `ELOOP`, `EACCES`, plus a symlink-resolving walk) for a knob nothing yet exercises, and because `SystemNative_GetCwd` already answers `KernelConfig.CurrentDirectory` the same way — that field's configured directory need not exist in the seeded filesystem either, and `getcwd(2)` fails `ENOENT` on an unlinked cwd on both flavours just as `realpath` does. This entry is therefore the record for both syscalls' non-resolution. A host that wants the file to be there seeds it.

**Knock-on effect**: guests that assume a non-null `Environment.ProcessPath` fault under the default. Microsoft.Testing.Platform is one: it throws `ArgumentNullException`. The fix is one line of host configuration, not a code change.

**Observable example**:

```csharp
// dotnet app.dll:  "/usr/share/dotnet/dotnet"
// PawPrint:        null   (KernelConfig.ProcessPath = None, the default)
Console.WriteLine(Environment.ProcessPath ?? "<null>");

// With KernelConfig.ProcessPath = Some "/opt/app/Guest", and nothing seeded there:
// dotnet app.dll:  True   (realpath only succeeds if the path resolves)
// PawPrint:        False
Console.WriteLine(File.Exists(Environment.ProcessPath));
```

**Testing note**: Cannot be a `sourcesPure` comparison test. The real runtime reports whatever launched the test host, so there is no cross-runtime value to assert, and under the `None` default the PawPrint side would skip the whole non-null spine — leaving a differential that was green without checking anything. Covered instead by the PawPrint-only `sourcesImpure/ProcessPathConfigured.cs` (the exact configured bytes, plus the entry point's allocation contract: two live pointers distinct, equal bytes, both freeable through `NativeMemory.Free`, and a further call after the frees) and `sourcesImpure/ProcessPathAbsent.cs` (NULL, errno 2, and a null `Environment.ProcessPath`). Both were run on real .NET on both flavours — the latter with a self-delete preamble, to put a real runtime in the state it describes — so their expected exit codes are measured rather than assumed. `TestProcessPath.fs` pins the config-to-kernel wiring, which no guest can distinguish from a write that `applyTo` discards. Note that no test may assert errno on a *successful* call: measured, macOS leaves a pre-set errno alone while Linux clobbers it with `EINVAL`, and CoreLib cannot see either, because its `SetLastError = true` stub zeroes the slot before the call and rewrites it after.

**Where this lives in code**: the `SystemNative_GetProcessPath` case in `NativeSystemNative.fs`; `EmulatedKernel.ProcessPath` and `defaultProcessPath` for the state and the default; `KernelConfig.ProcessPath` is where a host names one.

## `GetCommandLineArgs()[0]` is a bare file name unless the host names a path

**CoreCLR**: `CorHost2::ExecuteAssembly` calls `SetCommandLineArgs(pwzAssemblyPath, argc, argv)`, which forwards to the managed `Environment.InitializeCommandLineArgs(char* exePath, int argc, char** argv)` (`Environment.CoreCLR.cs`). `exePath` is `pwzAssemblyPath` verbatim — only a single-file bundle substitutes `Bundle::AppBundle->Path()` — and the managed body builds `commandLineArgs` (the program name followed by every argument) and `mainMethodArgs` (the arguments alone) in one pass, assigns the former to `s_commandLineArgs`, and returns the latter. So the two arrays cannot disagree, and element 0 is whatever path the host passed. Under `dotnet app.dll` that is the app's own `.dll`, which is *not* `Environment.ProcessPath` — see the entry above.

**PawPrint**: runs that same CoreLib method during startup, so the relationship between the two arrays holds by construction. What differs is only element 0's value: it is `GuestConfig.AssemblyPath`, and when the host names none it falls back to the file name the compiler stamped into the image (`DumpedAssembly.ScopeName`, from the `Module` row — ECMA-335 II.22.30). That is a bare name such as `"Guest.dll"`, where a real launch reports an absolute path.

**Spec status**: Outside ECMA-335, which says nothing about how a process learns its command line. Compliant with what the runtime's own source says the slot holds. CoreCLR's comment on `SetCommandLineArgs` (`corhost.cpp`) records that the answer "might not always return the exact same identity as the cmdLine used to invoke the method", with the worked example of `Foo arg1 arg2` reported as `Full_path_to_Foo arg1 arg2` — so the identity in element 0 is explicitly not pinned to any one rendering. NativeAOT's startup path describes the slot only as "the executable name" (`StartupCodeHelpers.Extensions.cs`, on why `Main`'s arguments are the tail rather than the whole). A non-path element 0 is therefore a shape upstream contemplates rather than one PawPrint invented.

**Why we chose this**: the alternatives are worse in the two ways this project already rejects.

* *Read the host's path.* `Program.prepare`'s `originalPath` is where the host read the image from, used to find a sidecar PDB; it is not part of `GuestConfig` and so not part of the replay contract, and the test harness passes a `.cs` source name there. Letting it reach the guest would make guest control flow depend on the machine that produced the run.
* *Synthesise a path* (say `/app/Guest.dll`). The fiction rejected under `Environment.ProcessPath` above. `ScopeName` is not that: it is a fact recorded in the image, so it is the same on every machine, and it is not claimed to be a path at all.
* *Decline to install a command line when the host names nothing.* This is the one that looks most principled and is in fact unreachable upstream: `ExecuteAssembly` refuses a null assembly path with `E_POINTER`, and it is the only route to `Main`. A guest running `Main` while `GetCommandLineArgs()` reports nothing is a state no real runtime is ever in — and it is worse than a wrong string, because `Main` would receive arguments that `GetCommandLineArgs()` then denied. CoreLib's empty-array fallback (`GetCommandLineArgsNative`, which on Unix is `return Array.Empty<string>()`) serves a *library* hosted from native code, not an executed assembly.

**Observable example**:

```csharp
// dotnet app.dll:  "/path/to/app.dll"
// PawPrint:        "app.dll"   (GuestConfig.AssemblyPath = None)
Console.WriteLine(Environment.GetCommandLineArgs()[0]);

// Unaffected, and true on both: element 0 then Main's arguments, from one shared pass.
static int Main(string[] args) =>
    Environment.GetCommandLineArgs().Length == args.Length + 1 ? 0 : 1;
```

**Testing note**: the *relationship* between the two arrays is a cross-runtime fact and is a `sourcesPure` comparison, `CommandLineArgs.cs`; because the pure harness launches every guest with no arguments, its tail comparison is vacuous there, so `TestCommandLineArgs.fs` runs the same shape under both runtimes with three arguments — and asserts the tail elements are the *same string objects* as `Main`'s, which only one shared pass produces. Element 0's *value* has no cross-runtime oracle (the real runtime reports whatever launched the test host), so it is pinned PawPrint-only: the exact bytes a configured `AssemblyPath` produces, and the exact bytes its absence falls back to.

**Where this lives in code**: `CommandLineArgsInit.fs` builds the call and `Program.beginStartup` pumps it as its own startup phase; `GuestConfig.AssemblyPath` is where a host names one.

## A `runtimeconfig.json` is validated only where PawPrint reads it

**CoreCLR**: `hostpolicy` parses the whole file with rapidjson, which rejects the *entire document* for faults anywhere in it — a numeric token too large to store in a double (`kParseErrorNumberTooBig`, so `1e400`), an unpaired `\uD800` surrogate escape (`kParseErrorStringUnicodeSurrogateInvalid`), and the rest of its error surface. A fault in a section nobody reads is still fatal: for the main config the app does not launch, and for `runtimeconfig.dev.json` the whole sidecar is ignored.

**PawPrint**: parses with `System.Text.Json`, whose accepted grammar is not rapidjson's. It takes `1e400` (yielding an infinity) and an unpaired surrogate escape (throwing only if you ask for the text), so a document faulty *only* in a part PawPrint never inspects is accepted here and refused there. Where the fault is in a `configProperties` value, it is caught: an overflowing number is classified `HostWouldReject`, so it behaves as a real host's rejection does. That check asks every *occurrence* of a property, including one a later duplicate shadows, because this fault stops rapidjson before it reaches the question of which duplicate wins — `{ "P": 1e400, "P": "final" }` does not launch on CoreCLR, where `{ "P": 1.5, "P": "final" }`, whose shadowed occurrence is merely unrenderable, launches with `P="final"`.

**Spec status**: Both parsers implement RFC 8259, which constrains neither the range an implementation must accept for a number nor what it does with a lone surrogate escape. Two conforming parsers may differ here, and these two do.

**Why we chose this**: closing it means validating rapidjson's exact accept/reject surface across every token in the document, including the ones we have no other reason to look at — reimplementing another parser's error behaviour, and taking on the job of tracking it. The configurations involved are ones no build tool emits, and the failure is confined to accepting a file a real host would refuse. The narrower version, where the fault is in a property we actually read, is implemented, because that one feeds the `HostWouldReject`/`NotReproducible` classification that decides whether a dev sidecar may be ignored.

One consequence is visible in that classification. An unpaired surrogate escape in a value reaches us as the same `InvalidOperationException` that invalid UTF-8 *bytes* do, and we report both as `NotReproducible`. For the bytes that is right — a real host reads them and substitutes U+FFFD. For the escape it is too strict: a real host rejects the document, so a dev sidecar containing one would be ignored and the app would launch, where PawPrint fails. Telling them apart means inspecting the raw token, and cannot be done at all for a property *name*, where materialising the text is the operation that failed.

**Observable example**:

```jsonc
// App.runtimeconfig.json
{
  "unread": 1e400,
  "runtimeOptions": { "configProperties": { "Switch": "on" } }
}
// CoreCLR:  the app does not launch (the document fails to parse).
// PawPrint: launches, with Switch seeded to "on".
```

**Where this lives in code**: `parseRootValue` and `renderValue` in `RuntimeConfig.fs`; the overflow check is `rejectUnparseableNumbers`, run over every occurrence before duplicate resolution, and `classificationCases` in `TestRuntimeConfig.fs` pins which faults land in which case.

## The host-populated `AppContext` properties are absent

**CoreCLR**: Before `hostpolicy` looks at `runtimeOptions.configProperties` at all, it populates eight properties of its own and passes them to `AppContext.Setup` in the same arrays: `TRUSTED_PLATFORM_ASSEMBLIES`, `NATIVE_DLL_SEARCH_DIRECTORIES`, `PLATFORM_RESOURCE_ROOTS`, `APP_CONTEXT_BASE_DIRECTORY`, `APP_CONTEXT_DEPS_FILES`, `FX_DEPS_FILE`, `PROBING_DIRECTORIES` and `RUNTIME_IDENTIFIER` (`hostpolicy_context.cpp`), plus `HOST_RUNTIME_CONTRACT`, and conditionally `APP_PATHS` and `STARTUP_HOOKS`. They come from deps resolution and the host's filesystem layout, never from the config file — a `configProperties` entry that reuses one of those names is a fatal `LibHostDuplicateProperty` rather than an override, so the two sets are disjoint by construction. Every .NET process therefore starts with them, whatever its `runtimeconfig.json` says, and a config with no `configProperties` section still yields nine.

**PawPrint**: Populates none of them. `AppContext` contains exactly the `configProperties` the host passed in `HostConfig.AppContext`, plus `AppContextProperties.runtimeBaseline` beneath them — which today is the single dynamic-code switch described under "Dynamic code is declared unsupported" below, and is a claim about the runtime rather than one of the host-populated properties this entry is about. None of the eleven names above is ever among them.

That baseline is why `AppContext.Setup` now runs on every guest and `s_dataStore` is always non-null. Before it existed, a host supplying no properties meant `Setup` was never called and `s_dataStore` stayed null — indistinguishable from an empty store through the public API, since `GetData` returns null for a null store and `SetData` lazily installs one, so nothing observable turned on it.

**Spec status**: Outside ECMA-335, which says nothing about host properties — this is the hosting contract rather than the CLI. Non-compliant with that contract, deliberately.

**Why we chose this**: These properties describe a host PawPrint does not have. There is no deps resolution, no probing, no runtime identifier and no assembly directory layout to derive them from: the interpreter is handed a list of framework directories directly, and binds by simple name against the first hit. Synthesising plausible-looking values would be worse than omitting them, because a guest that branches on a TPA entry would then take a path justified by a path list that describes nothing real. Omission at least fails in the direction the guest can detect.

The same gap covers framework-supplied properties. `hostfxr` builds its property bag by walking every resolved framework's own `runtimeconfig.json` and merging each one's `configProperties` (`runtime_config_t::combine_properties`, first writer wins, and the app is walked first so the app's value survives). PawPrint reads only the app's `runtimeconfig.json` and its `.dev.json` sidecar, because it has no framework-resolution chain to walk — it is handed runtime directories, not a framework graph. In practice this merges nothing today: the shipped `Microsoft.NETCore.App.runtimeconfig.json` for 10.0.7 declares only `tfm` and no `configProperties` at all, so the set being dropped is currently empty. It is recorded here because that is a fact about today's framework, not a guarantee.

Note that this is *not* the same as "what a guest sees when there is no `runtimeconfig.json`". A real host treats a missing config as a self-contained app, fails to find `hostpolicy` beside the assembly, and exits before any managed code runs (verified: exit 131 on osx-arm64). "No config file, so no *host* properties" exists only in PawPrint, and is likewise deliberate — the test harness compiles guests to a `MemoryStream` where no sidecar file can exist. Such a guest still receives the runtime baseline, which does not come from a config file at all.

**Observable example**:

```csharp
var tpa = AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES");
// CoreCLR:  a ';'-separated list of every framework and app assembly path.
// PawPrint: null.

Console.WriteLine(AppContext.BaseDirectory);
// CoreCLR:  the APP_CONTEXT_BASE_DIRECTORY property, i.e. the app's directory.
// PawPrint: falls through to AppContext's GetBaseDirectoryCore() fallback.
```

What PawPrint does *not* do is let a config file fill the hole itself: `RuntimeConfig.parse` refuses a `configProperties` entry that claims one of these names, exactly as a real host refuses to launch such a file. The absence above is a gap in what PawPrint can tell a guest; a forged `TRUSTED_PLATFORM_ASSEMBLIES` that the guest could not tell from the real thing would be worse than the gap.

**Where this lives in code**: `AppContextProperties.empty` in `RuntimeConfig.fs` documents the gap; `hostOwnedNames` in the same file is the refusal; `HostConfig.AppContext` is where a host would supply values if it had any. Closing this would mean deciding what a simulated app's filesystem layout *is*, which is a larger question than the seeding change that surfaced it.

## Dynamic code is declared unsupported

**CoreCLR**: `RuntimeFeature.IsDynamicCodeSupported` reads the AppContext switch `System.Runtime.CompilerServices.RuntimeFeature.IsDynamicCodeSupported` and **defaults to true** when no such switch is present (`RuntimeFeature.NonNativeAot.cs:17`). A stock `dotnet` launch of an app whose `runtimeconfig.json` says nothing about it therefore reports dynamic code as supported, and the BCL reaches for `System.Reflection.Emit` accordingly: `Expression.Compile()` emits a `DynamicMethod` rather than interpreting, and `MethodInvokerCommon.DetermineStrategy_*` abandons the interpreted `RuntimeMethodHandle.InvokeMethod` path after a `MethodBase`'s first invocation in favour of an emitted invoke stub (`MethodInvokerCommon.cs:124,153`).

**PawPrint**: Seeds that switch to `false` on every run, beneath whatever the host supplies, so a guest that says nothing about it reports dynamic code as **unsupported**. NativeAOT reports the same profile, so the BCL fallbacks this selects are well travelled rather than exotic.

**Spec status**: Outside ECMA-335 — a hosting/feature-switch contract rather than the CLI. Divergent from a *stock host's default*, deliberately. Not divergent from the real runtime *in the same configuration*.

That last sentence is measured, not inferred. The same program built three ways and run on net10.0/osx-arm64:

| `runtimeconfig.json` | `IsDynamicCodeSupported` | `IsDynamicCodeCompiled` | `IsSupported("IsDynamicCodeSupported")` | `new DynamicMethod(...)` |
| --- | --- | --- | --- | --- |
| absent | True | True | True | succeeds |
| `"...IsDynamicCodeSupported": false` | False | False | False | `PlatformNotSupportedException: Dynamic code generation is not supported on this platform.` |
| `"...IsDynamicCodeSupported": true` | True | True | True | succeeds |

So setting the switch is a supported configuration that CoreCLR honours from the app's own config, and `false` yields exactly the semantics PawPrint adopts — down to the exception type and message that `sourcesImpure/DynamicCodeUnsupportedByDefault.cs` asserts. The name is not one of the eleven the hosting layer populates for itself, so a guest's `runtimeconfig.json` may legally carry it (`hostOwnedNames` in `RuntimeConfig.fs` does not list it), and the shipped `Microsoft.NETCore.App.runtimeconfig.json` declares no `configProperties` at all, so no framework config competes for it.

**Why we chose this**: it is the truthful claim. PawPrint has no JIT, and its `System.Reflection.Emit` support is partial — enough to mint and run a `DynamicMethod` a guest builds itself, not enough for the stubs the BCL emits on its own behalf, and not at all for `AssemblyBuilder`/`TypeBuilder`. That switch is exactly the question "can this runtime produce code at runtime?", and the honest answer to it is still no. Leaving it at the stock default meant the BCL confidently taking Emit paths that PawPrint cannot execute, so a guest that invoked the same `MethodInfo` twice — or called `Expression.Compile()` — died on an unimplemented native primitive with a message naming a QCall, rather than on anything a guest author could act on. With the switch off, those paths are simply not taken, and the ones that have no fallback raise the `PlatformNotSupportedException` a real host raises in the same configuration. Correctness over availability: PawPrint answering "no" is a guarantee it can meet, and answering "yes" was one it could not.

The direction of precedence is part of the choice. The baseline sits *beneath* the host's properties, so a guest whose `runtimeconfig.json` declares the switch true observes true. Forcing the value would make `AppContextSeed` stop being a faithful reproduction of what `hostpolicy` installs — the guest would read back something its own configuration did not say — and would not even buy immutability, since `AppContext.SetSwitch` remains available to the guest at any moment. What it *would* cost is the only way to ask PawPrint to exercise the dynamic-code paths it does implement, which is how the `DynamicMethod` cases in `sourcesImpure` are registered.

The wart in that choice, stated plainly: a guest that explicitly declares the switch **true** is believed, and PawPrint then asserts a capability it has only in part. This is deliberate rather than overlooked, and it is tolerable for a narrow reason — it cannot produce a *wrong answer*, only a crash. A guest told "yes" runs the Emit paths PawPrint implements, and dies on an unimplemented primitive, with a message naming it, on the ones it does not. "Correctness over availability" is a rule about not returning wrong results, and it is not engaged when the alternative to crashing is crashing. The two other policies are worse: forcing the value breaks the seeding mechanism's faithfulness for every property, not just this one, and refusing to launch punishes a guest that declares the switch and never exercises it.

Only the *silent* case therefore diverges from CoreCLR. A guest that sets the switch either way is reproduced exactly.

One consequence worth stating: this is the first property PawPrint seeds unconditionally, so `AppContext.Setup` now runs on every guest, where previously an empty property set skipped it entirely. That is a behaviour change in its own right — `s_dataStore` is now always non-null — and it interacts with the entry in this document about absent host-populated properties, which remains true of the other nine.

What the seed is worth is measured rather than asserted. Removing it on `69e9fc29` and running the suite regresses ten guests, every one of them on one of three blockers: `ModuleHandle.ResolveMethod` for a MethodDef declared on an open generic type, a `RuntimeMethodHandle` where a dynamic scope wants a method, and `AssemblyNative_InitializeAssemblyLoadContext`. Five of the ten are not reflection tests at all — they reach an emitted invoke stub because something in their BCL path invokes a `MethodBase` twice. Issue #849 carries the full measurement and the staging that would let this entry be deleted.

**Observable example**:

```csharp
Console.WriteLine(RuntimeFeature.IsDynamicCodeSupported);
// CoreCLR (stock launch): True
// PawPrint:               False

var m = typeof(C).GetMethod("M");
m.Invoke(null, args);   // both: interpreted invoke path
m.Invoke(null, args);   // CoreCLR: emitted invoke stub. PawPrint: interpreted again.

new DynamicMethod("f", typeof(int), Type.EmptyTypes, typeof(C));
// CoreCLR:  succeeds.
// PawPrint: PlatformNotSupportedException, from AssemblyBuilder.EnsureDynamicCodeSupported.
```

**Where this lives in code**: `AppContextProperties.runtimeBaseline` and `withRuntimeBaseline` in `RuntimeConfig.fs`; applied in `Program.prepare` immediately before `AppContextSeed.prepareCall`. Pinned by `TestDynamicCodeSupport.fs` and by `sourcesImpure/DynamicCodeUnsupportedByDefault.cs` (the default) and `sourcesImpure/DynamicCodeSupportedOverride.cs` (the precedence).

Reflection.Emit support has since begun, which sharpens rather than changes the entry above. `ModuleHandle_GetDynamicMethod` — the QCall behind `DynamicMethod.GetMethodDescriptor()` — is implemented (`NativeModuleHandle.fs`, pinned by `TestNativeGetDynamicMethod.fs` and `sourcesImpure/DynamicMethodStubFromModule.cs`), so a guest that overrides the switch to true can mint a dynamic method, and that QCall reads the method's IL back out of its `DynamicResolver` (`DynamicMethodBody.fs`) so the minted method carries a decoded body. Such a method now also *runs*: bound to a delegate or called from another dynamic method, with string, type, field and dynamic-method scope operands, with `ldtoken` naming a type, and with `try`/`catch`/`filter`/`finally`/`fault` regions. `ldtoken` is the one type-shaped opcode that does *not* narrow its operand to a closed type — measured on real .NET, `ldtoken` of an open generic definition, of a bare generic parameter, of `System.Void`, of a byref and of a pointer all run and all round-trip through `Type.GetTypeFromHandle`, where `newarr`/`sizeof`/`box` and the rest refuse the first three — so it reads the scope entry through `DynamicScopeOperand.typeHandleTarget` rather than `closedType`. What is still refused by name at mint, rather than stored: a resolver built through `DynamicILInfo` (whose exception clauses arrive as a raw EH blob, not as `__ExceptionInfo` records), and a body naming a scope entry PawPrint cannot resolve — reflected method entries, `ldtoken` naming a field or a method, and `calli` signature blobs, each refused with its own measured reason in `IlDecoding.scopeOperandKind`. `ldtoken` of a field is a gap rather than an error: real .NET resolves it, including for a field of an *open* generic definition where `ldfld` of the same field is an `InvalidProgramException`, so it needs a field walk that does not narrow the declaring type the way `DynamicScopeOperand.field` does.

The baseline nonetheless stays `false`, and stays truthful: "can this runtime produce code at runtime?" is answered for the whole of Reflection.Emit, and `TypeBuilder`/`AssemblyBuilder` remain unimplemented. Revisit this entry and the baseline together when that changes.

Three divergences within `DynamicMethod` support are worth recording, all narrow.

**When a scope entry is read.** CoreCLR resolves each token in a body exactly once, when it first JITs the method, and a guest that rewrites `m_scope.m_tokens` between two invocations is not heard (measured: 4 then 4). PawPrint reads an *instruction operand* out of the live scope each time the instruction executes, so it answers 4 then 8. `catch` clause types are not affected — those are resolved and latched when the method is first prepared (`DynamicMethodExecution.concretize`), matching CoreCLR exactly, because a clause is not attached to an instruction and real .NET refuses an unresolvable one whether or not anything ever throws. Closing the operand case means latching the whole scope at first preparation, in the same place. Unreachable from a guest today: rewriting `m_tokens` needs private reflection PawPrint does not implement. The one missing piece is the `RuntimeFieldHandle_GetValue` QCall; the rest of that route — reading `m_scope` and `m_tokens` reflectively, then replacing a slot through `IList`'s public indexer — is interpreted managed code that already works, and was measured running end to end on real .NET. Implementing that QCall therefore makes this divergence guest-visible.

**The stack trace of a method that cannot be compiled.** A `catch` clause naming something that cannot be a clause type — `typeof(List<>)`, say, which `BeginCatchBlock` accepts because it is a perfectly good `RuntimeType` — is an `InvalidProgramException` raised by the method's first invocation. PawPrint raises the same exception at the same moment, and the caller's own handlers see it; but real .NET's trace carries a frame for the dynamic method itself above the caller's, because the failure happens as the JIT compiles that method, and PawPrint has no frame to name, having refused to build one. Closing that means pushing the frame anyway and failing in its prologue, as a failed `.cctor` does — `MethodState.PendingTypeInit` and `hasNotStarted` are the existing machinery for a frame that is on the stack, has executed nothing, and whose own clauses are therefore out of scope. `sourcesImpure/DynamicMethodInvalidCatchClause.cs` pins the exception and its timing; nothing pins the trace.

**An op whose staticness disagrees with its field's.** `ldsfld`/`ldsflda`/`stsfld` name a static
field and `ldfld`/`ldflda`/`stfld` an instance one, and no compiler emits the mismatch — but
`ILGenerator.Emit` takes whatever `FieldInfo` it is handed without checking it against the opcode,
so a *scope* operand makes both mismatches reachable in three lines of guest code. Measured on real
.NET, against controls that run: a static op on an instance field is a catchable
`InvalidProgramException`; an instance op on a static field *runs*, with the receiver evaluated for
its side effects and discarded (CoreCLR's importer says so in as many words). PawPrint refuses both,
loudly, in `UnaryMetadataFieldOps.checkFieldStaticness` — which is shared with the metadata
universe, where neither is reachable at all.

Neither refusal is wrong in the "produces a wrong answer" sense; both stop the run. The second is
the honest answer to legal IL PawPrint has never implemented — no field op has a path from an
instance receiver to static storage — and raising `InvalidProgramException` there would be a wrong
answer rather than a missing one. Closing the first means threading a raise out of
`resolveFieldToken` and through all six of its callers, which is a change to the field ops rather
than to operand resolution.

## `Activator.CreateInstance` rejection messages

**CoreCLR**: When `Activator.CreateInstance(Type)` refuses a type, the exception the guest catches carries a two-part message: `RuntimeType.ActivatorCache` catches whatever `RuntimeTypeHandle_GetActivationInfo` threw and rethrows the same exception *type* with `SR.Activator_CannotCreateInstance` formatted around the original message — so `Activator.CreateInstance(typeof(SomeAbstract))` reads "Cannot dynamically create an instance of type 'SomeAbstract'. Reason: Cannot create an abstract class.", the tail coming from the unmanaged layer's `Acc_CreateAbst`.

**PawPrint**: The exception *type* is identical, and so is the outer sentence — that half is produced by ordinary managed code PawPrint interprets. The inner reason is not: PawPrint raises runtime-synthesised exceptions by allocating the type and calling its *parameterless* constructor (`IlMachineStateExecution.raiseRuntimeException`), so the inner message is the framework's default for that exception type rather than the specific reason. The guest sees "Reason: " followed by the wrong sentence.

**Spec status**: Outside ECMA-335, which does not specify exception message text.

**Why we chose this**: The exception type is what a `catch` clause selects on, and it is what a program can act on; the message is diagnostic prose. Carrying the reason across would mean teaching the native-handler boundary to construct exceptions through a message-taking constructor, and then reproducing CoreCLR's resource strings — a general change to the raise path plus a corpus of English text to keep in sync with a runtime we pin but do not build. This is a general property of every PawPrint-synthesised exception; it is recorded here rather than under `raiseRuntimeException` because `ActivatorCache`'s rewrap makes it unusually visible: the wrong sentence is embedded in a message the guest is likely to print, instead of merely being the message of an exception it caught.

**Observable example**:

```csharp
try { Activator.CreateInstance(typeof(System.IDisposable)); }
catch (MissingMethodException e) { Console.WriteLine(e.Message); }
// CoreCLR:  "Cannot dynamically create an instance of type 'System.IDisposable'.
//            Reason: Cannot create an instance of an interface."
// PawPrint: same sentence, same exception type, different text after "Reason: ".
```

**Where this lives in code**: `ActivationInfo.classify` in `Native/NativeRuntimeTypeHelpers.fs` picks the exception type per CoreCLR's `ValidateTypeAbleToBeInstantiated`; `NativeHandlerResult.raiseException` is the boundary that cannot carry a message. `WoofWare.PawPrint.Test/sourcesPure/ActivatorCreateInstanceNonGeneric.cs` deliberately compares exception *types* only, for this reason.

## The `newobj` allocation helper has one address for every type

**CoreCLR**: `RuntimeTypeHandle_GetActivationInfo` hands `RuntimeType.ActivatorCache` the address of a JIT allocation helper, chosen by `CEEInfo::getNewHelperStatic` (`jitinterface.cpp`). That choice is not constant: `NEWFAST` is used for a type with a finalizer, for one whose base size reaches `LARGE_OBJECT_SIZE`, and for a COM object type, while ordinary small types get the `NEWSFAST` family (with `ALIGN8` variants where 64-bit alignment is required). So two activation caches can legitimately hold different `_pfnAllocator` values.

**PawPrint**: One address for every type. `FunctionPointerTarget.RuntimeAllocator` is nullary, so any two allocator pointers compare equal and hash identically.

**Spec status**: Outside ECMA-335 entirely — the identity of a JIT helper is an implementation detail of the CoreCLR/JIT pairing, not part of the CLI.

**Why we chose this**: Primarily because PawPrint cannot compute CoreCLR's partition honestly. The default-configuration split depends on three things PawPrint does not model: whether the type has a finalizer (there is no finalization machinery anywhere in the interpreter), its base size measured against `LARGE_OBJECT_SIZE` (PawPrint models a heap object as named field cells, not a byte block of a definite size), and whether it requires 8-byte alignment. A partition emitted from guesses at those would be wrong under *every* real configuration.

Collapsing them is at least right under some. The same `getNewHelperStatic` also consults `GCStress<cfg_alloc>::IsEnabled()` and `TrackAllocationsEnabled()`, and under either of those every type — finalizable or not, large or small — takes the slow helper and they all share one address. That is a diagnostics-on configuration rather than the default, so this is a real divergence and not a free lunch; but it is a divergence towards an answer some real runtime gives, rather than towards one none does. The observable is also close to nil: reaching `_pfnAllocator` at all requires reflecting into `RuntimeType`'s private activation caches and comparing the results as `IntPtr`s.

The same reasoning is why the case carries no `MethodTable` payload. That would make every type's allocator distinct, which inverts the common CoreCLR answer rather than approximating it.

**Observable example**:

```csharp
// Reachable only by reflecting into RuntimeType's private activation caches and reading
// ActivatorCache._pfnAllocator as an IntPtr for two types.
// CoreCLR:  a finalizable class and a small ordinary class can report different addresses.
// PawPrint: always equal.
```

**Where this lives in code**: `FunctionPointerTarget.RuntimeAllocator` in `NativeIntSource.fs` carries the reasoning; `NativeRuntimeTypeQCall.fs` is the only producer, and `UnaryMetadataCallOps.executeAllocatorCalli` the only consumer. `CanonicalPointerKey.RuntimeAllocatorFunctionPointer` gives it a single synthesised hash-bit identity to match.

## An open delegate stores no shuffle thunk, and `_methodPtrAux` is always zero

**CoreCLR**: a delegate's three code-related fields carry different things depending on whether the delegate is open (its `Invoke` supplies every argument the target takes) or closed (it supplies one fewer, and the missing first argument was bound at creation time). `COMDelegate::BindToMethod` (`comdelegate.cpp:1184`) writes, for a *closed* delegate, the bound object into `_target` and the target's code address into `_methodPtr`, leaving `_methodPtrAux` null; and for an *open* one, the delegate **itself** into `_target`, the address of a generated *shuffle thunk* into `_methodPtr`, and the target's real code address into `_methodPtrAux`. The thunk exists because the calling convention puts `this` in the first argument register, and an open delegate's first `Invoke` argument is not a receiver, so the arguments have to be moved down before the target is entered.

**PawPrint**: `_target` holds the bound object for a closed delegate and null for an open one; `_methodPtr` names the target method directly; `_methodPtrAux` is never written, so it stays at the zero `Delegate.InternalAlloc` left.

**Spec status**: Outside ECMA-335. II.14.6 describes delegates in terms of their observable behaviour and says nothing about the layout of `System.Delegate`'s private fields — which is why the divergence is representational rather than semantic.

**Why we chose this**: there are no shuffle thunks to point at. PawPrint does not have a calling convention in the sense that makes one necessary: `AbstractMachine.dispatchDelegateInvoke` rebuilds the callee's evaluation stack explicitly, pushing the receiver first only when there is one, so argument positions are decided at the call rather than baked into a stub. Synthesising a thunk to hold a place in a layout nothing reads would be inventing a runtime artefact to imitate its own shadow.

Note this is not a convention introduced for `Reflection.Emit`. `IlMachineRuntimeMetadata.executeDelegateConstructor` — the ordinary `newobj` path every C# `Func<int, int> f = SomeStatic;` takes — has always written the target into `_target` and the method into `_methodPtr` without regard to open-versus-closed. `Delegate_BindToMethodInfo` applies that same convention rather than adding a second one.

The managed observables agree with CoreCLR for every shape a dynamic method can produce, which is what makes this safe to do:

* `Delegate.Target` is `_methodPtrAux == IntPtr.Zero ? _target : null` (`Delegate.CoreCLR.cs:553`). Open: CoreCLR returns null because the aux field is set; PawPrint returns null because `_target` is. Closed: both return the bound object.
* `Delegate.GetHashCode` (`Delegate.CoreCLR.cs:152`) branches on the same field, and both branches reduce to `GetType().GetHashCode()` for an open delegate under either representation.
* `Delegate.Equals` (`Delegate.CoreCLR.cs:88`) compares all three fields optimistically and then falls back to `_methodBase`. Two delegates over the same dynamic method agree in every field under both representations; two over different dynamic methods disagree in `_methodPtr` under both.
* `Delegate.Method` reaches the runtime through the QCall `Delegate_FindMethodHandle`, whose `COMDelegate::GetMethodDesc` (`comdelegate.cpp:1815`) reads `_methodPtrAux` for an open delegate and `_methodPtr` for a closed one. Both name the target method and PawPrint's `_methodPtr` names it for either shape, so the two agree. `sourcesPure/DelegateMethodInfo.cs` exercises both halves: every static method group in it (`Func<int, int> f = Twice`) is an *open* delegate, because `Invoke` supplies each of the target's arguments, and each instance method group is a closed one. The shape it cannot reach is an open delegate over an *instance* method, where `Invoke` supplies the receiver — C# has no method-group syntax for it, so the routes are raw `ldnull; ldftn; newobj` IL, which the C# test harness cannot emit, and `Delegate.CreateDelegate(Type, MethodInfo)`. That second route is now live: `Delegate_BindToMethodInfo` binds a metadata method, so the shape is buildable and `sourcesPure/DelegateBindToMetadataMethod.cs` builds and invokes it. It is the one place the missing `_methodPtrAux` is not merely representational: `Delegate.GetMethodImpl` reads a zero one as "closed" and dereferences `_target` to walk the base chain when the declaring type is generic (`Delegate.CoreCLR.cs:189`), so `Delegate_FindMethodHandle` refuses that combination by name rather than letting CoreLib raise a `NullReferenceException` — parked as `sourcesPure/DelegateFindMethodHandleOpenInstanceGeneric.cs`, with the non-generic declaring type served. The *open over a virtual method* shape is refused at binding for the same reason, since CoreCLR resolves it at invocation through the aux field (`sourcesPure/DelegateBindOpenVirtual.cs`).

**What this costs later**: two things, both of which have to be paid by the slices that make them reachable rather than here.

1. **Openness is no longer recoverable from the fields.** A delegate closed over `null` and an open delegate are both `(_target = null, _methodPtrAux = 0)` here, where CoreCLR distinguishes them by the aux field. So whoever makes a dynamic method *executable* must derive the shuffle from the arity — the delegate's `Invoke` parameter count against the target's — and not from whether `_target` is null. `sourcesImpure/DynamicMethodDelegateBinding.cs` pins the closed-over-null case existing.
2. **Multicast (issue #959) must revisit this.** `MulticastDelegate.Equals` has a whole branch keyed on `_invocationCount != 0` for wrapper delegates and unmanaged function pointers, both of which read `_methodPtrAux`; and `Delegate.GetMulticastInvoke`/`GetInvokeMethod` are what would populate it. That work has to decide what `_methodPtrAux` means before it can use it.
3. **Two shapes are refused at binding rather than served**, both because the aux field is where CoreCLR would have put the answer: an open delegate over a virtual method on a reference type, and one over a static abstract interface method. Each is parked with its measured refusal.

**Observable example**:

```csharp
// Reachable only by reflecting on System.Delegate's private fields.
// CoreCLR:  for `Func<int,int> f = SomeStaticIntToInt;`, _target is f itself and _methodPtrAux is non-zero.
// PawPrint: _target is null and _methodPtrAux is zero.
// Every public accessor (Target, Method, Equals, GetHashCode) agrees between the two.
```

**Where this lives in code**: `IlMachineRuntimeMetadata.executeDelegateConstructor` for the `newobj` path, `NativeDelegate.tryExecuteQCall` for the `CreateDelegate` path, and `AbstractMachine.dispatchDelegateInvoke` for the consumer that makes the convention work.

## A method handle is per-instantiation, where CoreCLR shares one per canonical form

This predates any one change and is not specific to delegates: it is what PawPrint modelling no `__Canon` sharing amounts to, wherever a method handle is asked about. `Delegate::GetInvokeMethod` is the worked example below because that is where it was measured, but `MethodInfo.MethodHandle` on any generic type shows the same thing.

**CoreCLR**: a `MethodDesc` belongs to an `EEClass`, and an `EEClass` is shared between instantiations that canonicalise alike. Canonicalisation is per *position*: each reference-type argument is replaced by `__Canon` and each value-type argument is kept exact, and two instantiations share exactly when their canonical forms are equal. Measured on .NET 10 by comparing handles for `Func<_, _>.Invoke` — through `Delegate.GetInvokeMethod`, which reads `((DelegateEEClass*)pDelegateMT->GetClass())->GetInvokeMethod()` (`comdelegate.cpp:2156`), and through the public `MethodInfo.MethodHandle`, which agrees:

| pair | canonical forms | shares |
| --- | --- | --- |
| `Func<string, int>`, `Func<object, int>` | `Func<__Canon, int>` both | yes |
| `Func<string, string>`, `Func<object, object>` | `Func<__Canon, __Canon>` both | yes |
| `Func<string, int>`, `Func<string, string>` | `Func<__Canon, int>`, `Func<__Canon, __Canon>` | no |
| `Func<string, int>`, `Func<string, long>` | differ in the value position | no |
| `Func<int, int>`, `Func<int, string>` | `Func<int, int>`, `Func<int, __Canon>` | no |

So a wholly value-type instantiation such as `Func<int, int>` is its own canonical form and shares with nothing.

**PawPrint**: models no canonical sharing anywhere. `MethodHandleRegistry` keys a handle on a fully closed `ConcreteTypeHandle` declaring type, so each instantiation is minted its own registry id. PawPrint therefore answers "no" to every row of the table above — agreeing on the three that CoreCLR does not share, and disagreeing on the two it does.

**Spec status**: legal, checked against ECMA-335 6th edition (June 2012). `System.RuntimeMethodHandle` appears in the standard's five partitions exactly once: III.4.17 `ldtoken`, which says the instruction "pushes a `RuntimeMethodHandle`" for a `methoddef`, `methodref` or `methodspec`, and that "the value pushed on the stack can be used in calls to reflection methods in the system class library". That is the whole of it — no identity semantics, no equality contract, nothing relating two handles to the methods they denote. `GetMethodFromHandle` does not appear at all, and neither does any notion of canonical sharing; `__Canon` is a CoreCLR code-sharing strategy, not a standard one.

Two further reasons the standard does not decide this. Reflection is an *optional* Library — part of the Compact Profile, not of the minimal Kernel Profile (IV.5.5) — so a conforming implementation need not answer the question at all. And per-member semantics live in the companion `CLILibrary.xml`, "considered to be part of this Partition, but distributed in XML format" (Partition IV), which is not part of the PDF; the claim above is therefore about the five partitions, and does not rule out that the XML constrains `RuntimeMethodHandle` further.

`ldtoken`'s "runtime representation of a metadata token" does not favour CoreCLR's answer either, though it reads that way at first: a method on a generic instantiation is named by a `MemberRef` whose parent is a `TypeSpec` (II.24.2.6; II.22.25's rule 11 requires exactly that), so `List<string>::Add` and `List<object>::Add` are *different* tokens, and distinct runtime representations of them are as consistent with III.4.17 as a shared one.

Measured rather than derived, and where the two runtimes stand: `MethodInfo.Equals` is false on both, `DeclaringType` differs on both, `MetadataToken` is equal on both — only the handle disagrees. The platform's own API says a bare handle is not a method identity: `MethodBase.GetMethodFromHandle(RuntimeMethodHandle)` throws `ArgumentException` for a method whose declaring type is generic, on both runtimes. On CoreCLR the cost of sharing is visible directly — two `MethodInfo`s that are `.Equals`-unequal, with different `DeclaringType`s, nonetheless report equal `MethodHandle`s — whereas PawPrint's finer handle keeps handle equality tracking method equality.

**Why we chose this**: exact instantiations are the interpreter's only currency, and answering exactly is what makes the *consumer* right rather than merely convenient. `Delegate.DynamicInvokeImpl` hands the handle to `RuntimeType.GetMethodBase` together with the delegate's exact `RuntimeType` (`Delegate.CoreCLR.cs:82`), and CoreCLR's `GetMethodBase` walks that reflected type's base chain for a type sharing the declaring type's generic definition and then rebinds the handle onto it, via `RuntimeMethodHandle.GetMethodFromCanonical` (`RuntimeType.CoreCLR.cs:1871-1911`). PawPrint hands over the answer that rebind would arrive at, and skips a step it has no shared form to perform. Answering the *open definition* instead would not merely be less exact: it would not run at all. `GetMethodBase` consumes the handle before any invocation, and an open declaring type sends it down the remap branch above, where it reaches `RuntimeMethodHandle.GetMethodFromCanonical` — an InternalCall PawPrint does not implement. So the guest would die there, loudly, and never reach the invoke path.

Reproducing the sharing would mean giving PawPrint a canonical method identity and a remap on every consumer of a method handle — a change to the whole type system in order to make one private field of one InternalCall agree, and one whose only visible effect would be to make two distinct methods answer as one.

**Observable example**: public, and with no delegate in sight.

```csharp
// CoreCLR:  True  — one MethodDesc, List<__Canon>.Add's.
// PawPrint: False — two registry ids, one per exact instantiation.
Console.WriteLine (typeof (List<string>).GetMethod ("Add").MethodHandle
    .Equals (typeof (List<object>).GetMethod ("Add").MethodHandle));

// Both runtimes: False, False, True respectively. Only the handle disagrees.
Console.WriteLine (typeof (List<string>).GetMethod ("Add")
    .Equals (typeof (List<object>).GetMethod ("Add")));
```

**What this costs**: one reachable consequence beyond the comparison itself. The two-argument `MethodBase.GetMethodFromHandle(handle, declaringTypeHandle)` exists to re-attach a canonical handle to an instantiation, and on CoreCLR it accepts `List<string>.Add`'s handle together with `typeof(List<object>).TypeHandle` and answers `List<object>.Add`. That presupposes the sharing. Under PawPrint the handle is already exact, and the call reaches the unimplemented `RuntimeMethodHandle::GetMethodFromCanonical` and fails loudly rather than answering — measured, and the same InternalCall named above. Whoever wants that overload has to decide what rebinding an exact handle onto a different instantiation should mean.

`sourcesPure/DelegateDynamicInvoke.cs` asserts handle distinctness only for pairs CoreCLR shares nothing between — a value-type instantiation against a reference-type one, and two unrelated delegate definitions — and says at the assertion why the reference/reference pair cannot join it. `sourcesImpure/DelegateInvokeHandlePerInstantiation.cs` pins PawPrint's own answer for that pair, which is what keeps this entry from being a claim nothing checks.

**Where this lives in code**: `MethodHandleRegistry`, whose `MethodHandle` keys on a closed `ConcreteTypeHandle` declaring type — every minting path inherits the choice. `NativeDelegate.tryExecute`'s `Delegate.GetInvokeMethod` arm is one such path.

## A delegate invocation that fails before entering its target names no frame for it

**CoreCLR**: when invoking a delegate fails *because of the target itself*, the failure happens
inside the machinery that is preparing to enter that target, so the target is on the stack when the
exception is raised and appears as the top frame of its `StackTrace` and as its `TargetSite`.
Measured on .NET 10 for an abstract target closed over a null receiver: `StackTrace` begins
`at Ab4.M()` and `TargetSite` is `System.String M()`.

**PawPrint**: the delegate's synthetic `Invoke` frame is popped first and the exception is then
raised into the *caller*, so the trace begins at whatever called `Invoke` and the target is named
nowhere. `TargetSite` is unreachable for a different reason — `ExceptionNative_GetMethodFromStackTrace`
is unimplemented, so reading it stops the guest rather than answering wrongly.

**Spec status**: outside ECMA-335, which does not specify stack-trace contents.

**Why we chose this**: the frame ordering is deliberate and is the *other* half of a fidelity
trade. A stub frame still on the stack when the exception is raised lands in the guest's trace as a
`System.Action.Invoke` frame that real .NET never shows, which
`sourcesPure/DelegateCctorFailureTraceHasNoStubFrame.cs` pins the absence of. So the choice is
between one frame too many (the stub) and one too few (the target); popping first picks the latter,
which is the smaller lie because the missing frame is a method that genuinely never ran.

Both delegate-invocation failures have this shape: a `Reflection.Emit` target that could not be
compiled, and an abstract target. Neither has a frame available to name — the first because PawPrint
refused to build the method, the second because an abstract method has no body to enter.

**What it would take to close**: push the target's frame anyway and fail in its prologue, which is
what a failed `.cctor` already does. The machinery is `MethodState.PendingTypeInit` and
`ExceptionDispatching.hasNotStarted`: a frame that is on the stack, has executed nothing, and whose
own clauses are therefore out of scope. It cannot be reused as it stands, because `PendingTypeInit`
carries the handle of a type to initialise and `AbstractMachine`'s driver runs that initialiser on
the next step — so a frame parked there for another reason would run a `.cctor` that nothing asked
for. Closing this means generalising that field into a reason DU, which is a change to
`MethodState` and to exception dispatch and so is its own slice; it would fix both failures at once.

**Observable example**:

```csharp
abstract class A { public abstract string M(); }
var f = (Func<string>)typeof(A).GetMethod("M").CreateDelegate(typeof(Func<string>), null);
try { f(); }
catch (BadImageFormatException e) { Console.WriteLine(e.StackTrace); }
// CoreCLR:  first line is "   at A.M()"
// PawPrint: first line is the caller of Invoke; A.M() appears nowhere.
```

**Where this lives in code**: `AbstractMachine.dispatchDelegateInvoke`, whose `raiseFromPoppedStub`
is the shared ordering both failures use. `sourcesPure/DelegateToAbstractMethodOverNull.cs` pins the
exception itself, which is faithful; only the trace is not.

## Simulated time advances per retired instruction

**CoreCLR**: time is what the OS says it is. `Environment.TickCount64`, `Stopwatch` and
`DateTime.UtcNow` read the kernel's clocks, which advance with the wall regardless of how much
code the process runs. `Thread.Sleep(n)` blocks against an absolute deadline computed from
`CLOCK_MONOTONIC` (`GetAbsoluteTimeout(..., fPreferMonotonicClock: TRUE)` in
`pal/src/synchmgr/synchmanager.cpp`), and the OS wakes the thread some time at or after it —
in practice noticeably after, since the sleep is quantised to the platform's timer granularity.

**PawPrint**: the virtual clock advances `KernelConfig.InstructionCostTicks` — one 100 ns tick
by default — for each IL instruction any thread retires, and by nothing else. A thread that is
blocked, and a process in which nothing is runnable, experience no time at all except via the
driver's jump to the next outstanding deadline. A `Thread.Sleep` wakes at *exactly* its
deadline, never later.

**Spec status**: outside ECMA-335, which says nothing about clocks. The .NET contract for
`Thread.Sleep` is one-sided — "at least this long" — so waking exactly at the deadline is
conformant. It is simply the most optimistic instant the contract permits, where a real
scheduler is routinely far from it.

**Why we chose this**: determinism is the whole point, and a clock that a replay can reproduce
cannot be read from the host. Deriving time from retired instructions makes it a pure function
of the execution, which is what lets a recorded trace replay bit-for-bit.

The *rate* is a calibration, and the quantity it calibrates is the ratio between the shortest
sleep a guest can express (1 ms) and the cost of one instruction — because that ratio decides
whether the BCL's spin-then-sleep backoff does anything. Until #844 the ratio was 1:1, and
`Thread.Sleep(1)` parked its caller for *zero* scheduling decisions: measured on that issue's
repro, 81,886 parks and not one tick out of 800,000 at which any thread was observably
asleep. One tick per instruction — a self-consistent 10 MIPS machine — puts the ratio at
10,000:1, at which sleeping costs the sleeper a realistic share of the machine.

Two consequences worth knowing. A guest that busy-polls a clock while another thread is
runnable pays 10,000 interpreted instructions per simulated millisecond, and the driver's
jump-to-deadline shortcut cannot help, because the poller is runnable. And relative timings
between code paths are not modelled at all: every instruction costs the same, so a `call` and
a `nop` take equally long.

**Observable example**:

```csharp
// CoreCLR:  prints a number that is large and varies between runs; on Windows, a Sleep(1)
//           typically takes ~15ms, so this is nowhere near 1.
// PawPrint: prints exactly 1, every run, on every machine.
long before = Environment.TickCount64;
Thread.Sleep(1);
Console.WriteLine(Environment.TickCount64 - before);
```

**Where this lives in code**: `EmulatedKernel.VirtualClockTicks` is the clock and
`EmulatedKernel.InstructionCostTicks` the rate; `Program.stepPrepared` is its only writer, via
the validating `EmulatedKernel.withVirtualClockTicks`. The three projections the guest sees are
`systemTimeAsTicks`, `monotonicTimestampNanos` and `lowResolutionTimestampMs`, all in
`EmulatedKernel.fs`. `TestSchedulerSleepFairness` pins that a sleeping thread is observably
asleep.

## Rendered stack traces apply none of `StackTrace.ToString`'s display policy

**CoreCLR**: `Exception.StackTrace` renders through `System.Diagnostics.StackTrace.ToString`
(`StackTrace.cs:216`), which is not a straight dump of the frames the runtime recorded. It applies
three display rules on top:

* `ShowInStackTrace` (`StackTrace.cs:375`) drops frames carrying `[StackTraceHidden]` or
  `AggressiveInlining`, unless the frame is the last one. This is why an
  `ExceptionDispatchInfo.Throw()` rethrow shows the *caller's* frame and not
  `ExceptionDispatchInfo.Throw` itself, and why `TaskAwaiter.ThrowForNonSuccess` never appears.
* `TryResolveStateMachineMethod` (`StackTrace.cs:249`) rewrites a compiler-generated async or
  iterator `MoveNext` frame back to the method the guest wrote, so `<Inner>d__8.MoveNext()` prints
  as `Inner()`.
* the `--- End of stack trace from previous location ---` annotation is suppressed when the frame
  carrying `IsLastFrameFromForeignExceptionStackTrace` is an async state machine
  (`StackTrace.cs:361`, `&& !isAsync`) — so an `await` of a faulted async method records the
  boundary but does not print it.

**PawPrint**: `IlMachineRuntimeMetadata.renderExceptionStackTrace` renders every recorded frame,
in order, with none of the above. Traces therefore contain frames real .NET hides, spell async
frames as their state-machine `MoveNext`, and print a boundary annotation in the one place real
.NET suppresses it.

**Spec status**: Outside ECMA-335, which says nothing about stack-trace text.

**Why we chose this**: These are three parts of one rule set, and implementing any of them alone
makes the rendered trace differently wrong rather than less wrong — a trace with the async
suppression but without state-machine name resolution, say, still does not match .NET on any async
frame. They also share machinery PawPrint does not yet have at render time: attribute lookup on
the frame's method and declaring type, and interface-assignability against `IAsyncStateMachine`.
Doing them together, once, is the honest ordering. The data side is already complete: the
per-frame `IsLastFrameFromForeignExceptionStackTrace` that decides the annotation is recorded
faithfully, so this is a rendering gap and not an information loss.

**Observable example**:

```csharp
static void Boom() => throw new InvalidOperationException("x");

try { Boom(); } catch (Exception e) { captured = e; }
try { ExceptionDispatchInfo.Capture(captured).Throw(); }
catch (Exception e) { Console.WriteLine(e.StackTrace); }

// CoreCLR:  at Program.Boom()
//           at Program.Main()
//           --- End of stack trace from previous location ---
//           at Program.Main()
// PawPrint: the same, plus an `at System.Runtime.ExceptionServices.ExceptionDispatchInfo.Throw()`
//           frame that CoreCLR hides.
```

**Where this lives in code**: `IlMachineRuntimeMetadata.renderExceptionStackFrame` and
`renderExceptionStackTrace`. `sourcesPure/ExceptionDispatchInfoThrowPreservesTrace.cs` asserts on
substrings and counts precisely because of this: trace text is not comparable across the two
runtimes, but the presence, count and ordering of the boundary annotation is.

**A second, narrower gap in the same renderer**: a *synthesised* method's frame prints its
parameter list as `(…)` where real .NET prints the types — so a `DynamicMethod` called `Thrower`
taking an `int` renders `at Thrower(…)` against CoreCLR's `at Thrower(Int32)`. The reason is that
the renderer walks the *raw* (`TypeDefn`) signature rather than the concretised one, deliberately,
so that generic parameters survive as their formal names (`T`, `TResult`) rather than as whatever
they were instantiated to — and a synthesised method has no raw signature, only a concretised one.
Rendering from the concretised signature for this case alone would be straightforward and is the
obvious fix whenever the parameter text starts mattering; it is not done here because the two
halves of that decision (formal names for metadata methods, actual types for synthesised ones)
should be made together. Note the *name* half of such a frame is faithful, including the absence
of a declaring type — see "An open delegate stores no shuffle thunk" for why a `Reflection.Emit`
method has none, and `sourcesImpure/DynamicMethodStackTrace.cs`, which asserts only the
cross-runtime facts for exactly this reason.

## A captured stack frame has no native offset

**CoreCLR**: `StackFrame.GetNativeOffset()` answers the byte offset of the frame's return address
within the JITted machine code of its method — a real quantity, and `StackTrace_GetStackFramesInternal`
fills `StackFrameHelper.rgiOffset` with it for every captured frame (`debugdebugger.cpp:461-463`).
Measured on .NET 10, a three-frame capture reported 136, 52 and 96.

**PawPrint**: there is no machine code, so there is no such offset, and every frame reports
`StackFrame.OFFSET_UNKNOWN` (`-1`, `StackFrame.cs:133`) instead. `StackFrame.ToString()` renders that
as the literal `<offset unknown>` (`StackFrame.cs:241-243`) where real .NET prints a number, so the
divergence is visible in text as well as through the accessor.

**Spec status**: Outside ECMA-335, which has no notion of a native code offset.

**Why we chose this**: the alternatives were `0` and the frame's IL offset. `-1` is CoreLib's own
word for "this offset is not known", so a guest that checks for it — as `StackFrame.ToString` does
— takes the branch written for exactly this situation. Reporting the IL offset instead would answer
a different question from the one asked, and would be indistinguishable from a real native offset
to a guest that only reads the number; the IL offset is separately and faithfully available through
`GetILOffset()`. Nothing in the common rendering path depends on this: `StackTrace.ToString` reads
only `GetILOffset()` (`StackTrace.cs:335`).

**A related case in the same handler**: a frame whose method has *no IL body* — an InternalCall,
QCall or P/Invoke — reports `OFFSET_UNKNOWN` for its **IL** offset too, because it has no IL to be
at an offset within. That matches CoreCLR, which distinguishes the two ways an IL offset can be
missing (`InitPass2`, `debugdebugger.cpp:1543-1607`): a valid jitted method whose debug info yields
no mapping reports `0`, but a frame with no managed code information at all falls through to
`(DWORD)-1`. PawPrint keeps frames for such methods (a real trace does name them), and a
`MethodState` for one carries the synthetic program counter `0`, so reporting that would present a
placeholder as a position in the first instruction. Not academic: the innermost frame of every
current-thread capture is the P/Invoke stub of this very QCall.

**Where this lives in code**: `NativeStackTrace.tryExecuteQCall`, the `rgiOffset` and `rgiILOffset`
arrays.

## An unhandled exception is reported after its cleanup runs, not before

**CoreCLR**: when the first pass of exception dispatch finds no handler on the thread, the runtime
reports the exception *immediately* — the `Unhandled exception.` banner and the complete stack
trace go to stderr — and only then does the second pass unwind, running every `finally` and
`fault` clause between the throw point and the base of the stack. Measured on .NET 10: the banner
appears first, then each cleanup clause in innermost-first order, then the process aborts (exit
134). A clause that calls `Environment.Exit` during that unwind wins, and the process exits with
its code rather than aborting.

**PawPrint**: the same cleanup runs, in the same order, with the same complete trace visible to it
— but nothing is reported until the unwind has finished, because the report *is* the terminating
`ExceptionUnhandled` outcome the second pass returns when it reaches the outermost frame. So a
guest that writes to stderr from such a `finally` sees its output ordered before the report rather
than after it.

**Spec status**: Outside ECMA-335, which specifies neither the report nor its timing.

**Why we chose this**: PawPrint has no equivalent of CoreCLR's out-of-band report. Termination is
a value returned up the interpreter loop, and the host decides what to print; emitting a report
mid-unwind would mean either giving the dispatcher a side channel to stderr — the one thing the
`ExecutionResult` design exists to avoid — or returning a terminating outcome while guest code is
still to run, which no caller could act on. The facts a guest can *observe* are unaffected: the
trace it reads inside the clause is the completed one either way, and the exit code agrees.

**Observable example**:

```csharp
static void Cleaner()
{
    try { throw new NotSupportedException(); }
    finally { Console.Error.WriteLine("cleanup"); }
}

// CoreCLR:  Unhandled exception. System.NotSupportedException ... then "cleanup"
// PawPrint: "cleanup", then the host's report
```

**Where this lives in code**: `ExceptionDispatching.secondPass`, whose `NoHandler` arm returns
`ExceptionDispatchResult.ExceptionUnhandled` once it reaches a frame with no caller.
`sourcesPure/UnhandledExceptionRunsFinally.cs` pins the parts both runtimes agree on: that the
clause runs at all, that the trace it reads names every frame including `Main`'s, and that an
`Environment.Exit` from inside it decides the exit code.

## `GC.AllocateUninitializedArray` returns a zeroed array

**CoreCLR**: `GC.AllocateUninitializedArray<T>(int, bool)` passes `GC_ALLOC_ZEROING_OPTIONAL` to
`GCInterface_AllocateNewArray`, which lets `AllocateSzArray` hand back memory that still holds
whatever the previous occupant of that heap range left behind. The whole point of the API is to
skip the zeroing pass, and for a large enough unpinned array of a reference-free `T` it really
does skip it, so a read before a write yields arbitrary bytes.

**PawPrint**: Always zeroed, exactly as `GC.AllocateArray<T>` is. A read before a write yields
`default(T)`.

**Spec status**: Compliant with the API's own contract, which states the contents are
unspecified. The flag *permits* the runtime to skip zeroing; it does not require it, and a
runtime that always zeroes is a legal implementation (the NativeAOT path already skips the
`GC_ALLOC_ZEROING_OPTIONAL` request entirely for reference-containing types).

**Why we chose this**: It is the only content PawPrint can produce. `IlMachineState.allocateArray`
fills each element from a `CliType` template, and the heap model has no representation for unset
storage — a cell either holds a value or does not exist. Modelling "uninitialized" would mean
adding a poison state to the heap and teaching every reader about it, which is a change to the
memory model rather than to this QCall. It is also the deterministic answer, which is the
project's headline goal: real garbage varies per run, so a PawPrint that reproduced it would not
be reproducible.

**Observable example**:

```csharp
byte[] a = GC.AllocateUninitializedArray<byte>(2048);
// CoreCLR:  a[0] is arbitrary.
// PawPrint: a[0] is 0.
```

**Where this lives in code**: `NativeGc.tryExecuteQCall` handles `GCInterface_AllocateNewArray`
and documents the flag handling; `sourcesPure/GcAllocateArray.cs` covers the API while
deliberately never reading an uninitialized element before writing it.

## A negative-length `newarr` always reports the `AllocateSzArray` message

**CoreCLR**: `newarr` with a negative length raises `OverflowException` either way, but with one
of two different messages, chosen by which allocation helper the JIT emitted for the element
type. `CEEInfo::getNewArrHelperStatic` (`vm/jitinterface.cpp:5752-5806`) picks
`CORINFO_HELP_NEWARR_1_PTR` when the element is exactly pointer-sized and
`CORINFO_HELP_NEWARR_1_VC` otherwise. The pointer helper's slow path is
`RhpGcAlloc(MethodTable*, GC_ALLOC_FLAGS, uintptr_t numElements, …)`
(`vm/gchelpers.cpp:58-100`), where `numElements` is *unsigned*: the `numElements < 0` test there
is dead, and a negative length instead trips `numElements > INT_MAX`, which throws
`EEMessageException(kOverflowException, IDS_EE_ARRAY_DIMENSIONS_EXCEEDED)` — "Array dimensions
exceeded supported range.". Every other element type reaches `AllocateSzArray`'s bare
`COMPlusThrow(kOverflowException)` (`vm/gchelpers.cpp:637-638`) and so carries the parameterless
constructor's own message.

**PawPrint**: Always the `AllocateSzArray` message, for every element type.

**Spec status**: Compliant. ECMA-335 III.4.13 says only that `OverflowException` is thrown when
`numElems` is negative; it says nothing about the message, and both runtimes agree on the type.

**Why we chose this**: The CoreCLR split is not a property of the program, it is a property of
the code the JIT happened to emit. It moves with the target pointer size (on a 32-bit target
`int[]` would be the pointer-sized case), and `getNewArrHelperStatic` also falls back to the
slow helper whenever `LoggingOn(LF_GCALLOC, …)` or `TrackAllocationsEnabled()` — so the message
can change with ETW state at run time, for a fixed program on a fixed machine. PawPrint has no
JIT and no notion of a selected allocation helper, so there is nothing here to be faithful to.
Reproducing the split would mean hard-coding a rule about a compilation strategy PawPrint does
not have.

The `> MaxArrayLength()` rejection is *not* affected: it is raised inside `AllocateSzArray`
itself on every helper path, so `OutOfMemoryException` with "Array dimensions exceeded supported
range." is uniform upstream and PawPrint reproduces it exactly.

**Observable example**:

```csharp
static int Neg() => -1;

try { var a = new int[Neg()]; }    catch (OverflowException e) { Console.WriteLine(e.Message); }
try { var a = new string[Neg()]; } catch (OverflowException e) { Console.WriteLine(e.Message); }

// CoreCLR (64-bit):  Arithmetic operation resulted in an overflow.
//                    Array dimensions exceeded supported range.
// PawPrint:          Arithmetic operation resulted in an overflow.
//                    Arithmetic operation resulted in an overflow.
```

**Where this lives in code**: `SzArrayAllocation.faultFor` chooses the fault and message
for both routes into a single-dimensional allocation (`UnaryMetadataArrayOps.executeNewarr` and
`NativeGc`'s `GCInterface_AllocateNewArray`). `sourcesPure/NewarrLengthValidation.cs` asserts the
exception *type* differentially across element types on both sides of the split, and
`sourcesImpure/NewarrNegativeLengthMessage.cs` pins PawPrint's choice of message for the case
where CoreCLR would have used the other one.

## A narrow auto-layout `[InlineArray(N)]` runs instead of failing to compile

**CoreCLR**: `[StructLayout(LayoutKind.Auto)] [InlineArray(N)] struct` gets its alignment from
`MethodTable::GetFieldAlignmentRequirement` (`vm/methodtable.cpp:8853`). An auto-layout type has
no layout metadata, so that reads the *class*: the custom field alignment if one was recorded, and
otherwise `min(GetNumInstanceFieldBytes(), TARGET_POINTER_SIZE)`. The recording test is
`minAlign != min(elementSize, TARGET_POINTER_SIZE)` (`vm/methodtablebuilder.cpp:8598`) and runs
*before* the repeat count is applied, while the fallback reads the size *after* it — so a type
whose element needs no custom alignment and whose multiplied size is below the pointer size and is
not a power of two ends up with a non-power-of-two alignment. Three `byte`s give 3; three `short`s
give 6. The type loads: this is a computed alignment, not a rejection. The JIT then refuses to
compile any method that mentions it, and the program dies with
`InvalidProgramException: The metadata is corrupt.` before reaching `Main`.

**PawPrint**: Computes the same alignment, and runs the program.

**Spec status**: Neither behaviour is specified. ECMA-335 says nothing about `[InlineArray]`, which
is a .NET 8 runtime feature; and a runtime that cannot execute a type it agreed to load is a bug
rather than a contract. Reported upstream behaviour, not a documented one.

**Why we chose this**: PawPrint has no JIT, so there is no component that could refuse the program
— the refusal is not a property of the type or of the layout algorithm, it is a property of a
compilation step PawPrint does not perform. The alternative would be to reject such a type at
layout time, which would mean refusing a type CoreCLR loads on the strength of a guess about what
a JIT we do not have would have done with it. Every *observable size* here matches CoreCLR; what
differs is only whether the program starts.

**Observable example**:

```csharp
[StructLayout(LayoutKind.Auto)] [InlineArray(3)] struct ThreeBytes { private byte _item; }

Console.WriteLine(Unsafe.SizeOf<ThreeBytes>());

// CoreCLR:  System.InvalidProgramException: The metadata is corrupt.
// PawPrint: 3
```

Note the neighbours that are *not* affected, which is what makes this narrow: `[InlineArray(2)]`
over `byte` is 2 bytes and 2-aligned, and anything reaching 8 bytes or more is capped at the
pointer size. Only a total of 3, 5, 6 or 7 bytes can trip it, and only on the auto route — a
sequential type has layout metadata, so `GetFieldAlignmentRequirement` never reaches the fallback.

**Where this lives in code**: `CliValueType.InlineArraySize` in `CliType.fs` transcribes both
halves of the rule. `TestInlineArrayLayout.unrunnableCases` holds the two shapes the differential
sweep therefore cannot check, and asserts that the host runtime still refuses them — so a runtime
that fixed the upstream bug would fail that test rather than leave a stale carve-out in place.

## A failing class initialiser escapes `FieldInfo.SetValue` unwrapped

**CoreCLR**: `InvokeUtil::SetValidField` runs the declaring type's initialiser inside an `EX_TRY`
(`vm/invokeutil.cpp:786-794`) and, if it threw, discards the propagating exception in favour of a
freshly constructed `TargetInvocationException` wrapping it (`CreateTargetExcept`, `:803`). So a
guest reflectively setting a field of a type whose `.cctor` throws catches a
`TargetInvocationException` whose `InnerException` is the `TypeInitializationException`.

Note this is *not* what the neighbouring `ReflectionInvocation_RunClassConstructor` QCall does:
that one lets `CheckRunClassInitThrowing` throw straight through unwrapped
(`vm/reflectioninvocation.cpp:1226-1231`). The two QCalls share the shape "trigger a class
initialiser from native code" and differ on what escapes, so the resemblance is a trap rather than
a template.

**PawPrint**: The bare `TypeInitializationException` propagates, so a `catch
(TargetInvocationException)` does not fire.

**Spec status**: Unspecified. ECMA-335 I.8.9.5 governs when an initialiser runs and that its
failure is reported as a type-initialisation failure, but says nothing about how a reflection API
re-presents that failure to its caller. This is CoreCLR's reflection contract, not the CLI's.

**Why we chose this**: it is not a choice so much as the current shape of the native-frame
boundary. `RuntimeFieldHandle_SetValue` asks for the initialiser by returning
`suspendedForClassInit`; the initialiser frame then runs, throws, and the exception unwinds
*through* the native frame without the handler being re-entered, so there is no point at which the
handler could observe the failure and substitute a different exception. Wrapping it needs a native
frame that can intercept an exception propagating through it, which is a change to exception
dispatch rather than to this handler.

The half that *is* interceptable is handled: a declaring type already in `TypeInitState.Failed`
when the QCall is entered is caught before `ensureTypeInitialised` is called at all — that helper
dispatches the cached exception itself — and the handler refuses loudly there rather than
diverging silently in a second place.

**Observable example**:

```csharp
static class Boom
{
    public static int Value;
    static Boom() { throw new InvalidOperationException("boom"); }
}

try { typeof(Boom).GetField("Value").SetValue(null, 1); }
catch (TargetInvocationException) { /* CoreCLR arrives here */ }

// CoreCLR:  TargetInvocationException, InnerException = TypeInitializationException
// PawPrint: TypeInitializationException, uncaught by the handler above
```

**Where this lives in code**: the class-initialisation block of the `RuntimeFieldHandle_SetValue`
arm in `Native/NativeRuntimeFieldHandle.fs`.
`WoofWare.PawPrint.Test/sourcesPure/ReflectionFieldSetValueFailingCctor.cs` is the measured
example, parked in `TestPureCases.unimplemented` so the real-runtime side keeps asserting what the
answer should be.

## `[UnmanagedCallersOnly]` declarations and unmanaged call sites are not validated

**CoreCLR**: `COMDelegate::ThrowIfInvalidUnmanagedCallersOnlyUsage` (`vm/comdelegate.cpp:2029-2051`)
validates the *declaration* before the reverse-P/Invoke prologue is ever installed, and throws a
**catchable** `InvalidProgramException` for each way it can be wrong: a non-static method
(`InvalidProgram_NonStaticMethod`), a generic one (`InvalidProgram_GenericMethod`), and one whose
signature is not blittable (`InvalidProgram_NonBlittableTypes`). Only a *valid* declaration goes on
to get the prologue whose failed transition is the uncatchable
`EEPOLICY_HANDLE_FATAL_ERROR_WITH_MESSAGE` that `ReversePInvokeBadTransition` raises.

**PawPrint**: no such validation. The gate in `callMethodWithCommitment` asks only whether the
method carries the attribute and whether the call site leaves cooperative mode, so an *invalid*
declaration entered from managed code is reported as the transition failure — the wrong error, and
uncatchable where CoreCLR's is catchable.

**Spec status**: Unspecified. `UnmanagedCallersOnlyAttribute` is a runtime contract with no CLI
counterpart, and ECMA-335 says nothing about it.

The same gap has two further cases, both of which a guest *can* compile, and both measured:

- A method carrying both `[DllImport]` and `[UnmanagedCallersOnly]`. Reached through a function
  pointer (a direct call is CS8901), real .NET throws a catchable `NotSupportedException`: "Method
  'Program.getpid' cannot be marked with both DllImportAttribute and UnmanagedCallersOnlyAttribute."
  PawPrint dispatches the P/Invoke.
- A call site naming two base calling conventions, `delegate* unmanaged[Cdecl, Stdcall]<int, int>`.
  Real .NET throws a catchable `InvalidProgramException`: "Multiple unmanaged calling conventions
  are specified. Only a single calling convention is supported." (`CallConvBuilder::AddTypeName`
  refuses the second, callconvbuilder.cpp.) PawPrint runs the call. This one is not really about
  the attribute at all — it is `calli` call-site validation, and it diverges the same way for an
  ordinary target.

**Why we chose this**: validating these is a coherent piece of work, and a different one from the
transition rule. Every case here is a *catchable* exception raised before the transition is ever
attempted, so the fix is declaration- and call-site-time checking that raises into the guest — the
`raise-guest-exception` machinery — plus blittability analysis and calling-convention conflict
detection. None of it shares code with the gate, which asks only whether a thread that has arrived
at a valid target is still in cooperative mode.

Doing *part* of it would be worse than none: a check covering staticness and genericity but not
blittability, or one convention conflict but not another, leaves the divergence in place behind
something that reads as complete. The one thing that is not deferred is honesty at the boundary —
`callMethod`'s refusal says what it does not know rather than blaming the call site.

Two further entries sit in the same bucket, both reachable only by an image PawPrint is handed
rather than one compiled from C#:

- An **entry point** carrying the attribute. `Program` installs `Main`'s frame directly, so the
  refusal is not applied and PawPrint runs the entry type's static constructor and the method body.
  Roslyn rejects the source (CS8899), so this needs hand-authored or post-processed metadata, and
  CoreCLR's behaviour for such an image is unmeasured — which is why nothing is asserted here
  rather than a guess being encoded.
- The **timing** of the thread-entry refusal. `Thread.StartInternal` refuses while the QCall is
  still executing, so the guest's `Start()` never returns; CoreCLR raises the fatal error when the
  *worker* reaches the target prologue, which permits the parent to run first. PawPrint's choice is
  *a* legal interleaving — the worker being scheduled immediately — but it is the only one PawPrint
  will produce, so the schedules in which the parent proceeds before the abort are not explored.
  Making that a pending worker-entry check, aborting when the worker is first scheduled, would
  restore them.

## A directory stream sees mutations by name, where a real kernel's answer depends on its buffer

**CoreCLR (and any Unix)**: POSIX leaves it unspecified whether an entry added to or removed from a
directory after `opendir(3)` is returned by a subsequent `readdir(3)`. Both modelled kernels were
measured, and their answers are artefacts of when `getdents` happened to run rather than rules:
removing the whole directory *before* the first `readdir` gives end-of-stream at once, with no `.`
or `..`, while reading one entry *first* and then removing it yields the entire listing, dots
included. The same shape governs individual names.

What both kernels *do* guarantee in practice is that an entry already returned can be removed
without disturbing the ones after it: measured at 5000 entries — well past glibc's 32 KB `readdir`
buffer — deleting each name as it is returned skips nothing and leaves the directory empty. That is
a stable per-entry cookie, not a position.

**PawPrint**: the stream remembers the last *name* it returned, and each `readdir` yields the least
name strictly greater than it in the directory's current state. So a name removed after being
returned is invisible; a name removed before being reached is gone from the listing; a name added
ahead of the cursor appears, and one added behind it does not. A stream over a directory `rmdir` has
since removed is at end-of-stream at once, dots included, whatever the cursor had reached.

**Spec status**: unspecified by POSIX, so this is a lawful implementation rather than a divergence
from a rule. It is written down because it is a *choice*, and because two of its consequences —
insertion visibility, and the orphan answering end-of-stream from any cursor position — are places
where a real kernel might legitimately answer otherwise.

**Why we chose this**: among the lawful models it is the least convenient one that a guest could
actually meet on a real kernel, which is the standing preference here: relying on unspecified
behaviour is almost always a bug in the workload, and a model that hides mutations would let such a
bug pass under PawPrint and fail in production. The two rejected alternatives fail that test in
opposite directions. A *position*-indexed cursor is less forgiving than either real kernel and
breaks correct code: CoreLib's own `FileSystem.RemoveDirectoryRecursive` deletes each child inside
the `foreach` over the live enumerator and then `rmdir`s the parent, so an enumeration that skipped
entries would make `Directory.Delete(recursive: true)` throw ENOTEMPTY. A *snapshot* taken at
`opendir` is the most forgiving: it hides every mutation, so nothing a guest did could ever be
caught here.

**Observable example**:

```csharp
Directory.CreateDirectory("d");
File.Create("d/b").Dispose();

foreach (string entry in Directory.EnumerateFileSystemEntries("d"))
{
    File.Create("d/a").Dispose();  // sorts *before* "b", so behind the cursor
    File.Create("d/c").Dispose();  // sorts after it
    break;
}

// PawPrint: a subsequent step of the same enumeration yields "d/c" and never "d/a".
// A real kernel: unspecified; in practice it depends on whether the directory
// still fit in the buffer readdir had already filled.
```

**Where this lives in code**: `VirtualFileSystem.nextDirectoryEntry` and the `DirectoryCursor` type
beside it. `WoofWare.PawPrint.Test/TestDirectoryEnumeration.fs` pins every case above, including
the property that deleting each name as it is returned always empties the directory.

## Directory enumeration order is the model's own, not any kernel's

**CoreCLR (and any Unix)**: the order `readdir(3)` returns names in is arbitrary and machine-
specific, and *no* entry has a fixed position — the dots included. Measured, the same seven names
come back as `z é a sub ls C b` on APFS and `b a C é z sub ls` on a Linux overlay; and a directory
holding the single name `z` enumerates as:

| machine | order |
| --- | --- |
| macOS 26.6, APFS | `. .. z` |
| Linux 6.x arm64 container, ext4 (`/tmp` and `/var/tmp`) | `. .. z` |
| Linux 6.x arm64 container, fuse bind mount | `. .. z` |
| GitHub Actions `ubuntu-latest`, ext4 (`/home/runner/work/_temp`) | `z .. .` |
| GitHub Actions `ubuntu-latest`, ext4 (`/build`, the Nix sandbox on the same filesystem) | `z .. .` |

The last two rows are why this is written down. `EnumerateSeeded.cs` originally asserted that `.`
and `..` came first, in that order; it passed on macOS and in a Linux container and failed on CI,
where the dots come *last* and in the other order. What separates the machines was not measured —
the two that disagree are both ext4, so it is a property of the individual filesystem rather than
of the flavour, and no claim is made here about which property.

**PawPrint**: the directory's names in F# ordinal (UTF-16) order, which is the order
`DirectoryContent.Entries` already holds them in, and *then* `..` and `.` — the dots last.
Deterministic across runs and machines, which is the whole point of the interpreter.

**Spec status**: unspecified. No portable program may depend on enumeration order, and any that does
is already broken on a real system.

**Why we chose this**: two decisions, and they have different reasons.

*Where the dots go* is the least convenient of the two orders measured above. Both are lawful, and
`readdir(3)` fixes no position for anything; putting them last refuses a guest that consumes two
entries to skip them, or that expects the first entry to be one. Such a guest is already broken on
CI's ext4, and a deterministic interpreter should say so on every machine rather than on whichever
one happens to run it. Putting them first was the original choice and was rejected once the table
above existed: it is the *more* convenient order, and it made exactly this mistake pass — the guest
that asserted dots-first was `EnumerateSeeded.cs` itself. Interleaving them among the names, which
is what an ext4 htree hash order effectively does, was also considered and rejected: it is no more
lawful, it catches only the additional assumption that the dots are *adjacent*, and it costs a
cursor that must track which dots it has already emitted alongside which name it stopped at.

*The order among the names* has no such argument, because no real order can be reproduced — there
are several and they disagree — so the criteria are determinism and cost, and the map's own order is
free. Sorting by UTF-8 bytes was considered; it is arguably more principled, since a Unix name *is*
bytes, and it is no more expensive, but it differs only above the BMP and buys nothing a test could
observe.

**Where this lives in code**: `VirtualFileSystem.nextDirectoryEntry`. Every test that compares a
listing sorts it first, and `TestVirtualFileSystemAgainstHost`'s
`the model lists exactly the names this kernel lists` compares sets rather than sequences for
exactly this reason. `sourcesPure/EnumerateSeeded.cs` sorts the shim's own walk too, dots included,
since it is the differential tier and may claim only what holds on every machine either runtime
might run on.

## A directory stream's descriptor keeps an offset of zero

**CoreCLR (and any Unix)**: `opendir(3)` consumes a file descriptor, and `readdir(3)` advances that
descriptor's offset — so `lseek(dirfd, 0, SEEK_CUR)` answers non-zero once enumeration has begun.
The value is a *cookie*, not a count, and it is not derivable from anything PawPrint models.
Measured on both, one entry at a time through libc:

| directory | after `readdir` #1 | #2 | #3 |
| --- | --- | --- | --- |
| 3 entries, Linux/ext4 | `4096` | `4096` | `4096` |
| 300 entries, Linux/ext4 | `8192` | `8192` | `8192` |
| 3 entries, macOS/APFS | `2147483647` | `2147483647` | `2147483647` |
| 300 entries, macOS/APFS | `4294967551` | `4294967551` | `4294967551` |

Two things that table settles. The offset moves **once**, when libc's `getdents` buffer is filled,
and then not at all as the guest consumes entries out of that buffer — so it does not track
enumeration progress even in shape. And its value is the filesystem's own cookie scheme: a block
boundary on ext4, something else entirely on APFS.

**PawPrint**: the descriptor `opendir` opens is an ordinary `OpenFileTarget.File`, and its offset
stays at zero for the stream's whole life. The enumeration position lives beside it, in
`EmulatedKernel.DirectoryStreams`, as a *name* rather than an offset.

**Spec status**: POSIX specifies neither the value nor that `readdir` moves the offset at all; it
gives `telldir`/`seekdir` as the portable interface and says their values are meaningful only to
`seekdir` on the same stream.

**Why we chose this**: there is no value to give. An entry index is a number neither kernel
produces and whose shape is wrong besides — it would move on every `readdir`, where a real one moves
once. A block boundary would be inventing ext4's scheme on a filesystem that has no blocks. The
alternative to inventing is refusing, and refusing costs more than it buys here: `lseek` on a
directory descriptor is otherwise a legal operation that PawPrint answers correctly (it is zero at
`opendir` on both kernels, which PawPrint matches), and refusing it would break the correct case to
avoid a wrong answer in a case no managed code reaches.

Nothing in CoreLib or the PAL calls `dirfd(3)`, so no managed caller can obtain this descriptor at
all. A guest can only reach it by *inferring* the number — descriptors are handed out lowest-free,
so an `open` immediately after an `opendir` returns one above it — which
`WoofWare.PawPrint.Test/sourcesImpure/EnumerateClosedFdSeeded.cs` does deliberately, to check that
the interpreter's own bookkeeping survives a guest closing it.

**Where this lives in code**: the `SystemNative_OpenDir` arm of `Native/NativeSystemNative.fs`
opens the descriptor; `SystemNative_ReadDir` advances `DirectoryStream.Cursor` and deliberately
leaves the descriptor's offset alone.

## A failing `getcwd` leaves the caller's buffer untouched

**Darwin**: `getcwd(3)` writes to the destination *before* it knows whether it will succeed, so
several of its failure paths leave bytes behind and still return NULL. Measured on macOS 26.6 with
the destination prefilled `0xAA`, exactly sized by `mmap`, sweeping the capacity and reporting every
byte that changed:

| state | capacity | returns | what changed |
| --- | --- | --- | --- |
| current directory removed | 1 | ERANGE | nothing |
| current directory removed | 2 ≤ cap < 1024 | ENOENT | one NUL, at `buf[cap-1]` |
| current directory removed | ≥ 1024 (PATH_MAX) | ENOENT | that NUL, **and** the stale path at offset 0 |
| path 1418 bytes, buffer 1024 | 1024 | ERANGE | 976 bytes at offsets 48..1023 — a *suffix* of the path |
| path 1418 bytes, buffer 1400 | 1400 | ERANGE | 1342 bytes at offsets 58..1399 |

**Linux**: writes nothing on any failure path, at any capacity, in either state. Its `getcwd` is a
syscall that assembles the path in kernel memory and copies it out only on success.

**PawPrint** reports the errno — which is exact, and is the only thing any caller in the BCL reads —
and leaves the destination alone, matching the Linux flavour on both.

There is a second consequence, and it is not about residue. Because Darwin stores *before* it
decides which answer to give, a destination it cannot write kills the process on calls that would
otherwise report ERANGE or ENOENT, not only on the ones that would succeed. Whether it has stored
yet turns on the current directory's length against a libc threshold, measured at capacity 8 with an
unmapped destination on macOS 26.6:

| cwd path length | outcome |
| --- | --- |
| 80 … 1015 bytes | ERANGE, destination never touched |
| 1016 … 1418 bytes | SIGSEGV |

1016 is neither PATH_MAX (1024) nor any documented constant — it is one libc build's internal slack,
selecting between the `__getcwd` syscall and the user-space backward assembly. PawPrint models
kernels, not that route selection, so `UnixSystem.getcwd` **refuses** an unwritable destination for
any capacity of two or more, on that flavour, whatever the path length. This deliberately
over-refuses the top row, where the real call answers ERANGE: a refusal says "this library cannot
tell you", where encoding 1016 would answer ERANGE for calls that really die. At capacity 0 and 1 it
answers normally, Darwin having been measured to write nothing there on either side of the
threshold.

*Why not model it*: the last two rows are BSD `getcwd(3)` building the path **backwards** from the
end of the buffer and moving it to the front once it is known to fit. The residue is therefore a
function of libc's internal progress — how far the climb got, which of its paths the capacity
selected, what a `memmove` left behind — rather than of anything a kernel decides. Reproducing it
faithfully means reproducing that algorithm; reproducing it approximately means inventing bytes a
guest can read back, which is worse than writing none. Two successive attempts to model it from
partial measurements were wrong in different ways, one of them writing past the caller's capacity.

*Reachability*: `Interop.Sys.GetCwd` tests the return against NULL and decodes the buffer only on
success, so no BCL caller can see this. It takes a hand-rolled P/Invoke that ignores a NULL return
and reads its buffer anyway.

**Where this lives in code**: `UnixSystem.getcwd` in `WoofWare.PosixKernel/UnixSystem.fs` returns
`GetCwdAnswer.Failed` carrying an errno and nothing else; the measurements are recorded on
`GetCwdOrphanAnswer.ShortestPathFirst` in `SimulatedUnixPlatform.fs`.
## A faulting `getsockname` copies out no partial address

**Both flavours** copy the address towards the caller before they discover that the destination
cannot take all of it, so a destination that is writable for only its first few bytes keeps whatever
got there. Measured against a destination writable for its leading `prefix` bytes and `PROT_NONE`
after, prefilled `0xAA`, declaring 13 bytes to a socket bound on loopback:

| prefix | macOS 26.6 leaves | Linux 6.18.5 leaves |
| --- | --- | --- |
| 0 | nothing | nothing |
| 4 | all four bytes of the address | its first byte only |
| 8 | all eight | all eight |
| 12 | all twelve | all twelve |

The two disagree at 4 because `copy_to_user` and Darwin's equivalent fault at different granularities,
which is a property of the copy routine rather than of the syscall.

**PawPrint** writes nothing when the call faults. Its buffer vocabulary has no partially-writable
destination in it — a `BufferPointer.Storage` names storage that is wholly there and a
`BufferPointer.RawAddress` names none at all — so the state these rows measure is not one a guest can
put PawPrint in, and the residue has no representable case to land in.

*Contrast the length cell*, which **is** modelled: on a fault Linux has already stored the
untruncated length where Darwin has stored nothing, and `GetSockNameAnswer.Failed` carries that as
`lengthOverwritten`. The difference is that the length is one value a kernel decides, where the
residue above is a partial copy's leftovers. This is the distinction `getcwd`'s entry above draws
between what a kernel decides and what an algorithm leaves behind, and `getsockname` is the syscall
that has one of each.

*Reachability*: `SystemNative_GetSockName` returns the PAL error and its managed callers decode the
blob only on success, so no BCL caller reads either. The length store is unreachable even to a
hand-rolled P/Invoke: the shim passes `getsockname(2)` a local `socklen_t` and copies it back to the
caller only when the call succeeded.

**Where this lives in code**: `UnixSystem.getsockname` in `WoofWare.PosixKernel/UnixSystem.fs`; the
length divergence is recorded on `GetSockNameFaultLength` in `SimulatedUnixPlatform.fs`.
