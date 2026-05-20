Plan: Implementing the constrained. prefix opcode

What I learned

- Constrained is defined in WoofWare.PawPrint.Domain/IlOp.fs:567 (in UnaryMetadataTokenIlOp) and
correctly decoded in MethodInfo.fs:620 (FE 16 + 4-byte type token → UnaryMetadataToken (Constrained,
token)). It just needs an executor.
- The execution stub is at UnaryMetadataIlOp.fs:1634 — currently failwith "TODO: Constrained
unimplemented".
- No prefix infrastructure exists yet. The other prefix stubs (Volatile, Tail, Readonly, Unaligned) are
all still failwith too. Each instruction is dispatched statelessly from AbstractMachine.fs:813–823; there
is no mechanism to carry state from a prefix to the next instruction.
- callvirt lives at UnaryMetadataIlOp.fs:135–262. It already does interface dispatch via
performInterfaceResolution = true → IlMachineStateExecution.callMethod
(IlMachineStateExecution.fs:118–268), using the runtime type peeked from the eval stack.
- Box at UnaryMetadataIlOp.fs:481–660 shows exactly how to resolve a type token, concretize with generic
context, construct a CliValueType, and allocateManagedObject — we'll reuse this pattern.
- Managed pointers are EvalStackValue.ManagedPointer of ManagedPointerSource.
IlMachineState.readManagedByref (IlMachineState.fs:1317) dereferences one to a CliType.
- DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies defn is the existing check for
value-type-ness.
- EnumSemantics.cs (currently in the unimplemented set at TestPureCases.fs:21) is blocked on this opcode;
Threads.cs:24 is also blocked. Comment at EnumSemantics.cs:25 explicitly says value.ToString() goes
through constrained. callvirt.

Design

Where to store the prefix: add a PendingConstrainedType : ConcreteTypeHandle option field to MethodState.
Rationale:

- The prefix modifies the next instruction, so it must persist across the dispatch-loop tick. MethodState
is the natural per-frame bucket — it's already what the PC lives in.
- Scoping is free: returning from a method discards its MethodState, and the prefix goes with it. A
thread interleave during the one-instruction gap is safe because the prefix is per-thread-per-frame, not
global.
- If callvirt suspends for class init and re-enters, the prefix persists naturally (it's state, not
control flow).
- I'm intentionally not introducing a general PrefixState DU yet — per "no speculative generality," a
single option field is enough for this PR. Other prefixes are separate tickets and can add their own
state or generalize this field when they land.

Eager vs lazy type resolution: resolve the type metadata token immediately when Constrained fires
(following Box's pattern) and store the ConcreteTypeHandle. This avoids stashing the raw token +
assembly/generics context separately.

Three semantic cases (ECMA-335 III.2.1): I'll branch inside the Callvirt handler, not at decode time,
because the logic is tied to callvirt specifically and fusing-at-decode would complicate PC accounting.

Scope: recommend a single, narrow PR

Implement cases 1 and 3 only in this PR (enough to unblock EnumSemantics.cs). Leave case 2 as a failwith
with a specific message so it's a visible next step.

- Case 1 (T is a reference type): pop the byref, readManagedByref to get the ObjectRef, push it, continue
with standard callvirt logic.
- Case 3 (T is a value type, method on Object/ValueType/Enum, not overridden by T): pop the byref,
readManagedByref to get the value, box it using the same logic as Box (construct CliValueType from
instance fields, allocateManagedObject), push the ObjectRef, continue with standard callvirt.
- Case 2 (T is a value type that implements the method directly — typically interface methods on structs,
or generic T.Method() where T is a struct): requires resolving the interface method to T's
implementation by-hand (the existing resolution in callMethod keys off the stack-peeked receiver type,
which is wrong here — we need to dispatch against T, not the byref). Defer to a follow-up PR with a
dedicated test (e.g. a generic method calling IEquatable<T>.Equals on a struct).

This matches the project guidance — "make incremental progress only" — and keeps the review small.
EnumSemantics.cs only needs case 3 for value.ToString(). The boxing of the enum value happens via case 3,
then Enum::ToString dispatches virtually on the boxed Enum.

Implementation steps

1. WoofWare.PawPrint/MethodState.fs
- Add field PendingConstrainedType : ConcreteTypeHandle option to the MethodState record (line 23–41).
- Initialize to None in MethodState.Empty (line 191–202).
- Add two small helpers: setPendingConstrainedType / clearPendingConstrainedType, or just inline record
updates at the two call sites — probably cleaner to inline.
2. WoofWare.PawPrint/UnaryMetadataIlOp.fs — replace the Constrained stub at line 1634:
- Resolve the type token using the same three-way match as Box (TypeDefinition / TypeReference /
TypeSpecification).
- Concretize with currentMethod.DeclaringType.Generics and currentMethod.Generics (mirrors Box at lines
498–506).
- Store the resulting ConcreteTypeHandle in the thread's MethodState.PendingConstrainedType.
- advanceProgramCounter, return WhatWeDid.Executed.
- Do not touch the eval stack.
3. WoofWare.PawPrint/UnaryMetadataIlOp.fs — modify Callvirt at line 135:
- Right after computing methodToCall and the null check (before the
WithThreadSwitchedToAssembly/callMethod call at line 241), check
threadState.MethodState.PendingConstrainedType.
- If Some tHandle:
    - Clear PendingConstrainedType (single record-update).
  - Look up tHandle in state.ConcreteTypes, find the TypeDef, use DumpedAssembly.isValueType to
classify.
  - Case 1 (reference type): pop the ManagedPointer, IlMachineState.readManagedByref state src to get a
CliType.ObjectRef, push it. Fall through to normal callvirt logic.
  - Case 3 (value type): pop the ManagedPointer, readManagedByref to get the underlying value, then box
using the same construction as Box (lines 602–652): build CliValueType.OfFields from instance fields,
allocateManagedObject, push the resulting ObjectRef. Fall through to normal callvirt logic.
  - Case 2 (value type that implements method directly): failwith with a specific message —
"constrained.callvirt case 2 (value-type direct implementation) for type X method Y not yet implemented".
  - Deciding case 2 vs case 3 cleanly: a safe proxy for "T overrides this method" is to walk T's
TypeDef.Methods and look for a signature-matching entry whose declaring type is tHandle. For case 3 we
need "method defined on Object/ValueType/Enum and not overridden by T"; equivalently, "T's method table
resolves this method to a slot owned by Object/ValueType/Enum." The cheap approximation: if
methodToCall.DeclaringType is one of those three and T has no method with matching name+signature in its
own TypeDef.Methods, it's case 3.
- If None: current code path unchanged.
4. WoofWare.PawPrint.Test/TestPureCases.fs
- Remove "EnumSemantics.cs" from the unimplemented set (line 21).
- The test is already in sourcesPure/ and auto-discovered, so no other wiring needed.
5. New test file to specifically exercise cases 1 and 3 in isolation, per AGENTS.md's "test that
specifically captures only what you've just improved" rule. E.g., sourcesPure/ConstrainedCallvirt.cs with
a tiny generic static string Describe<T>(T x) => x.ToString() called both with a reference type (case 1)
and with a struct whose ToString isn't overridden (case 3). Small, focused, would fail today.
6. Format + commit: nix develop -c dotnet fantomas ., then commit to a new branch (not main), then codex
review --base main per CLAUDE.md.

Risks and open questions

- Case 2 detection: the "owned by Object/ValueType/Enum" check needs care — value types can hide a base
method with new rather than override. I'll lean on signature match at T's own method table: if T has any
entry for (name, param types, return type), prefer case 2. That may be slightly wrong for exotic corner
cases but matches how the existing interface resolution at callMethod works. Since case 2 will failwith
in this PR, the worst outcome is a clear error; we'll tighten the check when implementing case 2.
- Readonly preceding constrained: readonly. can precede ldelema, not relevant here. Other prefix
combinations aren't an issue — ECMA forbids them before callvirt.
- Debug assert opportunity: after Constrained fires, the next Locations entry should be a Callvirt. I
could add a debug-mode sanity check (either at decode time or at dispatch). Probably worth a one-line
Debug.Assert in the Constrained handler's "what's next" expectation.

Questions for you before I start

1. Happy with the narrow scope (cases 1 + 3 only, case 2 as an explicit failwith)? That unblocks
EnumSemantics.cs cleanly; Threads.cs likely needs case 2 and would be a separate PR.
2. The PendingConstrainedType field is an option on MethodState. OK with that, or would you prefer I
introduce a PrefixState DU now anticipating the other prefixes?
3. Preference on the new targeted test file — worth adding, or is the EnumSemantics.cs un-ignore enough
for this PR?
