# Class-level `[Intrinsic]` dispatch

Implement this plan with each stage on its own branch, stacked as necessary on previous branches, so that a reviewer can review each branch in isolation.

## Design

CoreLib sometimes marks a whole type `[Intrinsic]` rather than marking every member that has
placeholder IL. Hardware intrinsic provider classes are the motivating example:

```csharp
[Intrinsic]
public abstract class Rdm : AdvSimd
{
    public static new bool IsSupported { get => IsSupported; }
}
```

The getter body is intentionally not ordinary executable IL. The real JIT recognizes the
intrinsic and substitutes a runtime capability value. PawPrint currently recognizes
method-level `[Intrinsic]`, but not declaring-type `[Intrinsic]`, so it may accidentally
run recursive placeholder IL.

The desired taxonomy is:

- Non-intrinsic methods: run ordinary IL/native dispatch as today.
- Intrinsic methods whose IL has been reviewed and is safe: list explicitly in
  `safeIntrinsics`, then run ordinary IL.
- Every other intrinsic method: require an explicit PawPrint implementation. If none exists,
  fail loudly with the exact method name.

For this purpose, a method is intrinsic if either:

- the method itself has `[Intrinsic]`; or
- its declaring type has `[Intrinsic]`.

No `IntrinsicSource` discriminated union is needed. The source of the attribute is not part
of the dispatch model; it is only relevant to optional diagnostics.

Class-level `[Intrinsic]` must not affect type shape. Field layout still comes from metadata.
Any special representation PawPrint already gives to strings, arrays, runtime handles, spans,
vectors, etc. stays an ordinary runtime-modeling decision, not a consequence of the intrinsic
attribute.

## Stage 1: Precise intrinsic method keys

**Dependencies**: None

**Implements**: Design section "Core taxonomy" and prepares exact reviewed lists.

**Goal**: Make the safe/implemented intrinsic lists key off a precise declaring type identity,
so future class-level intrinsics do not accidentally match unrelated nested types with the same
simple name.

**Changes**:

- Introduce an `IntrinsicMethodKey` representation used by `Intrinsics.fs`.
- The key should include:
  - assembly simple name, e.g. `System.Private.CoreLib`;
  - full metadata type name, including namespace and nesting, e.g.
    `System.Runtime.Intrinsics.Arm.Rdm.Arm64`;
  - method name;
  - enough signature shape to avoid obvious overload collisions where needed. Parameter count is
    probably sufficient for the existing safe list, but if the implementation already has easy
    access to concrete parameter/return types, prefer including them.
- Build the key from the method's declaring assembly and declaring type handle, not from the
  caller's active assembly.
- Migrate the existing `safeIntrinsics` entries in
  `WoofWare.PawPrint/Intrinsics.fs` to the new key format.
- Keep behavior otherwise unchanged: this stage should still only recognize method-level
  `[Intrinsic]`.

**Correctness oracle**:

- Existing tests pass.
- Add a focused unit test proving two nested intrinsic provider types with the same simple name
  produce distinct keys, for example `System.Runtime.Intrinsics.Arm.Rdm.Arm64` versus another
  `*.Arm64` provider if both exist in the loaded CoreLib.
- Add a regression test for one existing safe intrinsic, e.g. `System.String.get_Length`, proving
  the migrated safe list still allows its IL path.

## Stage 2: Shared `[Intrinsic]` metadata predicate

**Dependencies**: Stage 1

**Implements**: Design section "For this purpose, a method is intrinsic if either..."

**Goal**: Centralize attribute detection for method and type metadata, without yet changing
call dispatch.

**Changes**:

- Add a helper that recognizes `System.Runtime.CompilerServices.IntrinsicAttribute` from a
  `CustomAttribute`.
- Use the method's declaring assembly metadata when resolving attribute constructors. Do not use
  the caller's active assembly.
- Expose two predicates:
  - `methodHasIntrinsicAttribute`;
  - `typeHasIntrinsicAttribute`.
- Preserve the old method-level behavior through a compatibility wrapper if that keeps the diff
  smaller.
- Do not add a public `IntrinsicSource` type.

**Correctness oracle**:

- Metadata tests over the loaded CoreLib:
  - a known method-level intrinsic is detected;
  - `System.Runtime.Intrinsics.Arm.Rdm` or another available hardware provider class is detected
    as type-level intrinsic;
  - a normal type such as `System.Object` is not detected as intrinsic;
  - a type-level intrinsic that we intend to keep running through reviewed IL, such as
    `System.String` or `System.Span<T>`, is detected as intrinsic metadata. This locks in that
    detection and safe execution are separate concepts.

## Stage 3: Route class-level intrinsics into the existing policy

**Dependencies**: Stage 2

**Implements**: Design section "Every other intrinsic method: require an explicit PawPrint
implementation."

**Goal**: Stop executing unreviewed IL for methods whose declaring type has `[Intrinsic]`.

**Changes**:

- In `IlMachineStateExecution.callMethod`, compute:
  - method-level intrinsic;
  - declaring-type-level intrinsic;
  - combined `isIntrinsic = methodIntrinsic || declaringTypeIntrinsic`.
- Apply the policy in this order:
  1. If not intrinsic, continue with ordinary execution.
  2. If intrinsic and `safeIntrinsics` contains the precise key, continue with ordinary IL.
  3. Otherwise call `Intrinsics.call`.
  4. If `Intrinsics.call` returns `None`, fail with a message like:
     `TODO: implement JIT intrinsic System.Private.CoreLib System.Runtime.Intrinsics.Arm.Rdm.get_IsSupported, or add it to safeIntrinsics after reviewing its IL`.
- Keep the failure before any attempt to enter the method body.

**Correctness oracle**:

- Existing tests pass except for tests that were only passing by accidentally executing
  unreviewed type-level intrinsic IL. Those should now fail loudly, not hang.
- Add a focused test that invokes a currently unimplemented class-level intrinsic and asserts
  PawPrint fails with the explicit "implement JIT intrinsic" message. Prefer a direct F# harness
  test over a broad pure C# source test, so the pre-fix version cannot hang the full suite.
- Add a negative test proving a class-level intrinsic listed in `safeIntrinsics` still runs its IL.
  `String.get_Length` is a good candidate if CoreLib marks `String` intrinsic in the test runtime.

## Stage 4: Implement `Rdm.IsSupported` explicitly

**Dependencies**: Stage 3

**Implements**: The first concrete explicit implementation under the new policy.

**Goal**: Fix the motivating infinite loop by explicitly implementing the `IsSupported`
capability query that CoreLib expects the JIT to replace.

**Changes**:

- Add exact reviewed entries for:
  - `System.Runtime.Intrinsics.Arm.Rdm.get_IsSupported`;
  - `System.Runtime.Intrinsics.Arm.Rdm.Arm64.get_IsSupported`, if reachable in the failing test or
    easy to cover in the same narrow implementation.
- Do not use a namespace wildcard. Each entry should be an explicit type/method key.
- In `Intrinsics.call`, verify the signature is `static bool get_IsSupported()` before pushing a
  value.
- For the default `HardwareIntrinsicsProfile.ScalarOnly`, return `false`.
- If a future profile supports hardware ISA flags, model them as explicit fields or an explicit
  capability map on `HardwareIntrinsicsProfile`; do not read host CPU features.

**Correctness oracle**:

- End-to-end C# test:

  ```csharp
  using System.Runtime.Intrinsics.Arm;

  class Program
  {
      static int Main()
      {
          return Rdm.IsSupported ? 1 : 0;
      }
  }
  ```

  PawPrint should terminate and return `0` under `ScalarOnly`.

- If `Rdm.Arm64` is implemented in this stage, add the equivalent nested-class test.
- Existing `TestHardwareIntrinsicsProfile` tests still pass.

## Stage 5: Move the user's uncommented test only as far as this PR justifies

**Dependencies**: Stage 4

**Implements**: Integration with the motivating test.

**Goal**: Re-enable only the test coverage unblocked by explicit class-level intrinsic dispatch.

**Changes**:

- Run the user's uncommented test under `dotnet test --filter` and capture the next failure.
- If the test now passes, leave it enabled.
- If it now fails for a different missing primitive, keep the test in the `unimplemented` set with
  a new comment naming the next precise blocker.
- Do not implement the next missing opcode/native primitive in this PR unless it is part of this
  class-level intrinsic work.

**Correctness oracle**:

- The filtered motivating test no longer infinite-loops.
- Either it passes, or its `unimplemented` comment names a new finite blocker.
- The full test project passes, or any remaining failure is the documented uncommented test being
  deliberately kept unimplemented.

## Follow-up pattern

After these stages, each new class-level intrinsic discovered by the test suite should follow the
same small loop:

1. Add a focused failing test or metadata assertion that reaches exactly that intrinsic.
2. Inspect the CoreLib body.
3. Either add the exact key to `safeIntrinsics` with a comment explaining why the IL is safe, or add
   an explicit implementation in `Intrinsics.call`.
4. If the next failure is unrelated, stop and record it as the next blocker rather than broadening
   the current PR.
