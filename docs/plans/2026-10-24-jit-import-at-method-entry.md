# Binding a method's tokens when it is first entered, as CoreCLR's JIT does

## The problem

CoreCLR compiles a method before it runs any of it. Its importer (`importer.cpp`) walks the method's
basic blocks from the entry and resolves every token it imports. A token that binds to nothing, or
to something the JIT refuses, fails the compilation, which raises before the method has run
anything. No handler in that method can catch it; the caller's can. The compilation is retried, and
fails again, on every later call.

PawPrint has no compilation step. Each opcode binds its token when it executes, so for these
failures, everything before the instruction has already run, a path that never reaches the
instruction runs normally, and the method's own `try` catches the failure. `docs/divergences.md`
records four instances:

- "A `call` or `ldftn` naming an abstract method fails when it is reached";
- "A `box` or `newarr` of a byref-like type fails when it is reached";
- "A field reference that binds nothing throws when it is reached";
- the `DynamicScope` operand refusals, which "A `call` or `ldftn` naming an abstract method" calls
  "the same timing trade".

The same timing decides when an assembly is first loaded, and so `AppDomain.AssemblyLoad`'s
timing ("`AppDomain.AssemblyLoad` is raised later for an image than on CoreCLR").

The goal: when a method instantiation is first entered, bind every token in the code the JIT would
compile, and raise the compilation's failure there, before the method runs anything. Instructions
then use what was bound at entry rather than binding again.

## What real .NET does (measured)

Probes on fabricated IL, run on .NET 10 out of process, tiered compilation at its default. The full
table is in the session notes. These are the facts the design rests on:

1. **Timing.** A static method of a class with a precise `.cctor`, holding a `newarr Span<int>`
   behind `ldarg.0; brfalse`, called twice with `false`: `TypeLoadException` both times, and the
   `.cctor` never runs. So the compilation precedes the type-initialisation check, and a failed
   compilation is retried on each call. `TestEscapeAnalysis.fabricateRefusals` measures the rest:
   the method's own handlers never see the failure (`BoxInTry`), a run need not reach the
   instruction (`BoxIfArg`, `NewarrIfArg`, `CallAbstractIfArg`), and a branch that no run takes is
   still compiled unless the JIT folds it (`BoxPastNewobjBranch`).
2. **What is compiled.** The importer drops a conditional branch's dead successor whenever
   `gtFoldExpr` folds the condition to a constant (`importer.cpp`, `COND_JUMP`), and never imports
   a block that nothing imported leads to. Measured, each folds and skips the refusal behind it:
   `ldc.i4.0; brtrue`; `ldc.i4.1; ldc.i4.1; sub; brtrue`; `ldc.i4.1; ldc.i4.2; bgt`;
   `typeof(int) == typeof(string)`; `X86Base.IsSupported` (false on arm64); `box !!T; brtrue` for a
   value-type `T`. A value stored to a local and loaded back is not folded. A `NoOptimization`
   method folds nothing, not even the capability query: `gtFoldExpr` returns at once unless
   `Tier0OptimizationEnabled()`, which is `!compDbgCode && !JIT_FLAG_MIN_OPT` (`compiler.h`).
   `AggressiveOptimization` folds as tier 0 does. `gtFoldExprCompare` also folds a comparison of two
   identical side-effect-free trees, such as `ldarg.0; ldarg.0; beq`.
3. **Handlers.** A handler of a reachable protected block is compiled (`NewarrInCatch`); a
   protected block that nothing reaches, and its handler, are not. The importer queues a block's
   handlers when it begins importing a block inside the protected region (`impVerifyEHBlock`).
4. **Order.** Within a block, the importer fails at the first failing instruction in IL order.
   Across blocks it follows its own worklist, not IL order: `br L2; L1: newarr; …; L2: call
   Abs::A; br L1` raises `BadImageFormatException`, for the abstract call.

## Design

### The compiled code: two bounds

The importer's folding is not something PawPrint models exactly: it depends on what `gtFoldExpr`
recognises, on intrinsic expansion, and on the optimisation level; PR #1445 tried to model the
importer's folding of literal branches and replaced it with a refusal. Instead, two sets of offsets bound it:

- **maybe compiled**: what control reaches from the entry when the only branches folded are those
  on a capability query whose answer the CPU profile gives (`IntrinsicBody.constantResult`, as
  `EscapeAnalysis` folds them), in a method the JIT optimises; and with no branch folded in one it
  does not optimise. A method it does not optimise is one marked `NoOptimization`, or one in an
  assembly whose `DebuggableAttribute` disables optimisation (`Debuggable.allowsJitOptimisation`).
  This never omits an offset the JIT imports.
- **surely compiled**: what control reaches from the entry crossing only conditional branches the
  JIT cannot fold. In a method it does not optimise, every branch: measured, even `box !!T; brtrue`
  for a value-type `T`, whose box the importer still replaces by a constant, compiles both
  successors there. In one it does optimise, a `brtrue`, `brfalse` or `switch` whose operand the
  instruction straight before it pushes, where that instruction is `ldarg`, `ldloc`, a `newobj` of a
  class, or a `call` or `callvirt` of a method that returns a value and that the JIT does not expand
  as an intrinsic (the importer gives each a local or a call node, which `gtFoldExpr` never reduces to
  a constant; calls are inlined only after import). Not a call returning nothing, after which the
  branch pops whatever was beneath it, nor a `newobj` of an array, whose helper's result the JIT may
  know is not null (both measured to fold, under `AggressiveOptimization` for the array); and a two-operand branch straight after two `ldarg`s or `ldloc`s that are not the same
  argument or local (`ldarg.0; ldarg.0; bne.un` folds, measured). No branch may land on the branch
  or between it and its operands. Every other conditional branch may fold, and contributes neither
  successor. The list is short on purpose: `ldarga; brfalse` is measured not to fold, but nothing
  depends on it yet, and an entry costs a measurement. Handler entries are added as for
  `mayExecute`, from the protected blocks reached. This never holds an offset the JIT does not
  import.

Both are computed in `WoofWare.PawPrint.Semantics`, beside `ControlFlow.mayExecute`, as pure
functions of the body, the optimisation flag and the capability answers.

### Order: which failure the importer reaches first

A block that dominates another in the maybe-compiled flow graph (with an edge from every block of a
protected region to each of its handlers) is imported before it: the importer queues a block only
from one it has imported, and every queued path to the dominated block passes through the
dominator. Within a block, IL order decides. So site `a` **precedes** site `b` when they share a
block and `a` comes first, or `a`'s block dominates `b`'s. This is a partial order that holds for
whatever the importer's worklist does; sites it does not order are left unordered rather than
guessed.

### Binding

Each token-bearing instruction in the maybe-compiled code is bound as the JIT binds it, giving one
of:

- **Bound**: what the instruction will use, which it reads instead of resolving the token itself;
- **Raises**: the exception the compilation raises, with its fields: a reference that names
  nothing, a parent that does not load, or one of the JIT's own refusals over what the token names
  (an abstract method under `call` or `ldftn`, a byref-like type under `box` it does not fold or
  under `newarr`);
- **Unmodelled**: a binding PawPrint does not implement, which the resolvers report as a result
  rather than a `failwith` (the stage before Stage 3 makes them). The diagnostic is kept, and the
  instruction fails with it when it executes, which is where such a token stops a run today.

### The compilation's outcome

With the failing sites `F` (those that raise) and the unknown sites `U` (unmodelled) in the maybe
compiled code:

- `F` is empty: the method compiles. Its bindings are kept for its instructions.
- Some `w` in `F` is surely compiled, and every site of `F` and `U` that `w` does not precede raises
  exactly what `w` raises: the compilation raises `w`'s exception.
- Otherwise PawPrint stops, naming the sites it cannot order. Neither picking one nor running the
  method would be a guarantee it can state.

The outcome is memoised per method instantiation. A compilation that raises is raised again, as a
fresh exception, on each entry.

### Where it runs

A new first stage of the callee frame's prologue, before the type-initialisation check
(`MethodState.PendingTypeInit`). The frame is established first, so the exception's trace names the
method, as CoreCLR's does; and the frame has started nothing, so none of its handlers is in scope
for either pass (`ExceptionDispatching.hasNotStarted` widens to cover it). Every path that pushes a
frame with an IL body goes through it: calls of every kind, delegates, reflection, `.cctor`s and the
entry point. A `DynamicMethod`'s body is compiled too, binding its `DynamicScope` entries. The
interpreter's own synthesised bodies (the marshal stub, the multicast `Invoke` stub, the entry
placeholder) are not.

Binding may load assemblies. The prologue is part of a step, so `AssemblyLoadEvent` discards and
reruns it as it does any step that loads, which is sound because binding writes nothing but the
load context and the memo.

### Consequences

- An assembly is loaded, and announced, when the first method that names it in compiled code is
  entered, rather than when an instruction first needs it. That is CoreCLR's timing except where
  the JIT inlines, which only adds earlier loads of what an inlinee names. The `AssemblyLoad`
  divergence narrows to that; its observable example (`Touch`, which prints 11 on CoreCLR) should
  then agree.
- Tokens on paths a run never takes are bound. Where PawPrint cannot bind one, it still stops only
  if the instruction executes, as today, unless that token makes the compilation's outcome
  undecidable.
- Binding runs once per instantiation instead of once per execution, which should be cheaper for
  any instruction executed more than once. Measure with `WoofWare.PawPrint.Performance`.

### Not in scope

- Access checks. The JIT raises `MethodAccessException` and `FieldAccessException` when it binds;
  PawPrint checks no access at all today.
- Other invalid IL that the importer refuses (`BADCODE`: stack depth mismatches, an empty stack at
  a pop). `StackShape` checks some of it in Debug builds. The outcome rule above assumes the rest of
  the body is valid.
- The JIT initialising a `beforefieldinit` type early ("Type-initialiser (`.cctor`) timing").

## Binding today, by opcode family

Surveyed 2026-10-24 at `f61af8672`. Every family resolves its token inside the opcode's own
execution, through `IlMachineState.resolveTypeMetadataToken`, `concretizeType`, `resolveMember`
(memoised in `TypeSystemState._MemberResolutions`) and `ExecutionConcretization`. Nothing caches a
binding per method or per offset. `ResolvedMetadataOperand` re-tags the operand and resolves nothing,
except a `DynamicScope` operand, which is read from the guest heap before the opcode runs.

| family | guest exception on a failed binding today | runtime values the binding reads |
| --- | --- | --- |
| `call`, `ldftn` | `BadImageFormatException` for an abstract target; dispatch failure under `constrained.` | the pending `constrained.` prefix, which is static IL |
| `callvirt` | none | the receiver, for a MethodSpec over a MethodDef of a generic type, whose instantiation it takes |
| `newobj`, `ldvirtftn`, `ldtoken` | `ldtoken` of a field: as fields | none |
| fields (six opcodes) | `MissingFieldException`, `TypeLoadException` (`IlMachineMemberResolution.bindingFailureException`) | none |
| `box` | `InvalidProgramException` where the JIT does not fold a byref-like box | the IL after the box, and the tokens there |
| `newarr` | `TypeLoadException` for a byref-like element | none |
| other type tokens | none | `ldelema` resolves only without `readonly.`, which is static IL |
| `ldstr` | scope entries only | none; it materialises and interns the string on every execution |
| `DynamicScope` operands | `ResolutionFault.ScopeEntry` refusals | the guest's `m_tokens`; minting a method descriptor is a managed call |

Every other failure is a host `failwith`: a reference that binds no method ("binds to no method, so
CoreCLR throws MissingMethodException; that is not modelled"), a parent that does not load, and
the many shapes PawPrint does not implement. No binding path catches anything. Binding's side
effects are loading and binding assemblies, registering concrete types, and its memos; none runs a
`.cctor` (the static-field opcodes arm it afterwards), though `ldtoken` mints handles and `ldstr`
allocates.

`cpobj`, `mkrefany`, `refanyval` and `jmp` are unimplemented: today they stop only when executed,
and under this design their bindings are unmodelled.

### An unmodelled binding is a result

The entry binder must keep going past a binding PawPrint cannot do, which today is a `failwith`
from deep in resolution. Decided 2026-10-24: each such `failwith` reachable from binding becomes a
result first, rather than being caught at the token, so that what a binding may fail with is in
its type. That is a stage of its own, before Stage 3.

## Stages

Implement this plan with each stage on its own branch, stacked as necessary on previous branches, so
that a reviewer can review each branch in isolation.

Every stage that moves a family's binding to method entry moves it completely: the opcode reads its
binding from the method's compiled bindings and no longer resolves its token, so no family is bound
in two places. A family not yet moved is unmodelled to the outcome rule, which therefore stops
rather than raise where one of its sites is not preceded by the failure. Each such stage also
records, over the Guest suite, which runs load an assembly earlier than before (a test-only trace
of the load sequence, diffed), and checks that no test's outcome changes.

### Stage 1: the compiled code's bounds and order

**Dependencies**: none.

**Implements**: "The compiled code: two bounds", "Order".

`WoofWare.PawPrint.Semantics`, pure: maybe-compiled and surely-compiled offsets of a body, given
whether the JIT optimises it and the capability queries' answers, and the precedence order over
sites. Nothing consumes it yet.

**Correctness oracle**:
- Property, over generated bodies: surely ⊆ maybe ⊆ `ControlFlow.mayExecute body Map.empty`.
- Property: in a body the JIT does not optimise, surely = maybe = `mayExecute body Map.empty`.
- Property: precedence agrees with a reference implementation that enumerates paths in the
  maybe-compiled graph of small generated bodies: `a` precedes `b` exactly when every path from the
  entry to `b` passes `a` (or they share a block and `a` is first).
- Differential, against real .NET out of process: generated bodies built from argument and literal
  branches, two-operand branches and protected regions, each with a `newarr Span<int>` in one block.
  Wherever the model decides (the site is surely compiled, or not maybe compiled), real .NET raises
  or runs accordingly. The probe's shapes are fixed examples.

### Stage 2: the escape analysis compiles what the JIT compiles in an unoptimised method

**Dependencies**: Stage 1. Independent of Stages 3 onwards.

`EscapeAnalysis`'s `compiled` becomes Stage 1's maybe-compiled set, so a `NoOptimization` method's
capability branches are no longer folded.

**Correctness oracle**: new `fabricateRefusals` runners (a refusal behind a false capability query,
with and without `NoOptimization`), observed failing before the change, with the real-runtime check
the fixture already makes.

### Stage 2b: the shared resolvers report what they do not model

**Dependencies**: none. Several PRs, one per layer.

**Implements**: "An unmodelled binding is a result".

The resolution layers the opcodes share return a result whose failure is either a modelled guest
exception or an unmodelled diagnostic, instead of a `failwith`: `TypeResolution` and
`MemberReferenceParent` (Loader), `TypeConcretisation`, `MemberReferenceInstantiation` and
`FieldReferenceResolution`'s callers (TypeSystem), then `IlMachineMemberResolution`,
`ExecutionConcretization` and `IlMachineState.resolveTypeMetadataToken`. Today's callers turn an
unmodelled result back into the same `failwith`, so behaviour does not change. Loader and TypeSystem
are published packages, so each PR lists the public signatures it changes. A `failwith` that is a
`BUG:` (an invariant of PawPrint's own) stays one: only what a guest's metadata can provoke becomes
a result.

**Correctness oracle**: both suites unchanged, and each converted shape that a test reaches today
still fails with its old message. The compiler finds each caller.

### Stage 3: method entry compiles, binding `call` and `ldftn`

**Dependencies**: Stages 1 and 2b.

**Implements**: "Binding", "The compilation's outcome", "Where it runs", for method tokens under
`call` and `ldftn`, with their `constrained.` prefix.

The compile prologue, its memo, the widened `hasNotStarted`, the outcome rule, and the binder for
these two opcodes. Retires "A `call` or `ldftn` naming an abstract method fails when it is reached".
Measures the change in load order and in run time (`WoofWare.PawPrint.Performance`).

**Correctness oracle**:
- A differential fabricated guest, written and observed failing first: the abstract-call shapes
  of `fabricateRefusals` (`CallAbstractIfArg`, an abstract call in a `try` of the same method, behind
  a folded literal branch, under `NoOptimization`), a precise `.cctor` that must not run, and a
  second call that raises again.
- `TestFabricatedAbstractMethodCall` unchanged.
- Both test suites, run separately.
- Mutation: dropping the prologue, letting the method's own handlers catch, and raising for a
  maybe-but-not-surely site each turn a test red.

### Stage 4: `box` and `newarr`

**Dependencies**: Stage 3.

Retires "A `box` or `newarr` of a byref-like type fails when it is reached". The box pattern's
follower tokens are bound with it, as `impBoxPatternMatch` binds them.

**Correctness oracle**: as Stage 3, over `BoxIfArg`, `BoxInTry`, `BoxPastNewobjBranch` (which needs
`newobj` bound, so is parked until Stage 7 or stops as undecidable until then), `NewarrIfArg`, and
`TestFabricatedByRefLikeBox` unchanged.

### Stage 5: field tokens

**Dependencies**: Stage 3.

Retires "A field reference that binds nothing throws when it is reached".

**Correctness oracle**: `TestFabricatedFieldReference`'s shapes with the reference behind an argument
branch and inside the method's own `try`, observed failing first.

### Stage 6: the other type tokens

**Dependencies**: Stage 3.

`castclass`, `isinst`, `unbox`, `unbox.any`, `ldelem`, `stelem`, `ldelema` (without `readonly.`),
`ldobj`, `stobj`, `initobj`, `sizeof`.

**Correctness oracle**: a fabricated reference to a type that does not exist, behind an argument
branch, under each opcode: `TypeLoadException` at entry on both runtimes. Where PawPrint does not model
that failure today, this stage makes it so first, and that is its own PR.

### Stage 7: `callvirt`, `newobj`, `ldvirtftn`, `ldtoken`

**Dependencies**: Stage 3.

The receiver-dependent `callvirt` shape stays at the instruction, as dispatch: its binding is the
MethodDef and the method generics, and the receiver supplies the type's instantiation when it runs.
`ldtoken`'s `typeof(X).IsValueType` fold (`TypeofIntrinsicFold`) is the JIT's, so it is decided here
too. Updates the `AppDomain.AssemblyLoad` divergence, whose observable example should now agree with
CoreCLR (pinned by a test).

**Correctness oracle**: that example as a differential test, observed failing first, and
`AssemblyLoadEventImage.cs` and `TestCrossAssemblyAssemblyLoadEvent` unchanged.

### Stage 8: `ldstr` and `DynamicScope` operands

**Dependencies**: Stage 3.

A string literal is materialised when the method is compiled, as CoreCLR's JIT does, and a
`DynamicScope` entry is read then. Minting a method descriptor is a managed call, which CoreCLR also
makes while compiling a dynamic method (`DynamicResolver`); whether the prologue can push it, or that
part stays at the instruction, is decided when this stage starts.

**Correctness oracle**: the scope-entry refusal tests with the operand behind an argument branch,
observed failing first; `ldstr`'s documented divergence retired with a test of interning order.
