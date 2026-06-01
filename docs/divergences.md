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
