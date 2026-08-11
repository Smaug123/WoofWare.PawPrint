namespace WoofWare.PawPrint

/// Scheduler prior for an IL opcode under Probabilistic Concurrency Testing
/// (PCT). The classifier is a pure function of `IlOp`; the scheduler treats
/// the returned band as a hint about how likely it is that interleaving this
/// op with other threads' steps reveals a guest-visible behavioural
/// difference. Unlike a partial-order-reduction (POR) classifier, this is
/// *not* a one-sided contract: false-low-priors only reduce the probability
/// of catching a bug on any one seed, they don't break the design.
///
/// The motivation is that under a strict one-sided POR contract, nearly
/// every op in this interpreter ends up classified as visible (because the
/// interpreter mutates internal bookkeeping — `PointerHashState`,
/// `ConcreteTypes`, the runtime-handle cache — from ops the guest can't
/// directly address). PCT decouples "could in principle have a cross-thread
/// effect" from "almost always has one", and lets the scheduler spend its
/// exploration budget where it matters.
///
/// The bands are coarse enough to assign by hand from CIL semantics but
/// fine enough to discriminate. A future adaptive layer can keep the same
/// classifier and substitute its own band-to-weight mapping (or refine
/// per-PC based on observed effects) without touching this file — the band
/// is the stable interface, the weight is the policy.
type ContextSwitchPrior =
    /// No observable effect anywhere — not on guest state, not on
    /// interpreter bookkeeping. The scheduler can freely coalesce runs of
    /// these without considering interleavings. Examples: `Nop`, `LdcI4_*`,
    /// pure comparisons, non-trapping non-pointer conversions, branches.
    | Never
    /// Touches only interpreter-internal mutable bookkeeping
    /// (`PointerHashState`, the concretisation cache, the interning
    /// table). Guest IL has no opcode that directly addresses these
    /// structures; the only path to guest visibility is through opaque
    /// hash bits / handle identities, which are usually deterministic
    /// after the first fill. Low default weight: the scheduler should
    /// occasionally schedule here so we explore bookkeeping-order
    /// variations, but not every step.
    | InterpreterOnly
    /// Could cause a guest-visible effect, but usually doesn't — the
    /// observable case requires a precondition that holds only some of
    /// the time. Examples: `Add_ovf` and `Div` (trap only on specific
    /// inputs), `Throw` of an exception that almost never escapes the
    /// thread, `Ldloc` of a slot whose address has been published
    /// (rare). Medium default weight.
    | RarelyGuestVisible
    /// Every execution reads or writes shared guest-addressable state.
    /// Examples: `Stfld`, `Stsfld`, `Stind_*`, `Stelem_*`, `Newobj`,
    /// `Call` (target may do anything), heap byref dereferences.
    /// Highest default weight — the scheduler should treat these as
    /// likely context-switch points.
    | AlwaysGuestVisible

[<RequireQualifiedAccess>]
module ContextSwitchPrior =
    /// Default band-to-weight mapping. Returns a value in [0, 1] where
    /// higher means "the scheduler should be more likely to interleave
    /// another thread's step here". These are starter values, picked by
    /// hand to give an order-of-magnitude separation between the bands.
    /// A scheduler is free to ignore these and supply its own mapping —
    /// e.g., an adaptive policy that raises a band's weight after
    /// observing the band-typical effect in a recent run.
    let weight (p : ContextSwitchPrior) : float =
        match p with
        | ContextSwitchPrior.Never -> 0.0
        | ContextSwitchPrior.InterpreterOnly -> 0.01
        | ContextSwitchPrior.RarelyGuestVisible -> 0.1
        | ContextSwitchPrior.AlwaysGuestVisible -> 1.0

    /// Classify a `NullaryIlOp`. The match is exhaustive so any new
    /// constructor will fail to compile here until banded.
    let ofNullary (op : NullaryIlOp) : ContextSwitchPrior =
        match op with
        // ---- Never: no observable effect ----
        // Pure eval-stack manipulation, constant pushes, and frame-local
        // miscellany. None of these touches a heap location, interpreter
        // bookkeeping, or any state a sibling thread can name.
        | NullaryIlOp.Nop
        | NullaryIlOp.Pop
        | NullaryIlOp.Dup
        | NullaryIlOp.LdcI4_0
        | NullaryIlOp.LdcI4_1
        | NullaryIlOp.LdcI4_2
        | NullaryIlOp.LdcI4_3
        | NullaryIlOp.LdcI4_4
        | NullaryIlOp.LdcI4_5
        | NullaryIlOp.LdcI4_6
        | NullaryIlOp.LdcI4_7
        | NullaryIlOp.LdcI4_8
        | NullaryIlOp.LdcI4_m1
        | NullaryIlOp.LdNull
        // Comparisons *read* `PointerHashState` — that is how `ceq` decides whether
        // synthesised hash bits are a given handle's address — but they never assign, so
        // they still cannot mutate it, which is what this banding depends on.
        | NullaryIlOp.Ceq
        | NullaryIlOp.Cgt
        | NullaryIlOp.Cgt_un
        | NullaryIlOp.Clt
        | NullaryIlOp.Clt_un
        // Truncating / widening numeric conversions that preserve
        // `WidenedNativeInt` / `OpaqueHashBits` provenance rather than
        // materialising; don't read the heap; don't trap. (`Conv_I` and
        // `Conv_U` are banded separately below — they may anchor a
        // byte-view on a plain-array byref.)
        | NullaryIlOp.Conv_I1
        | NullaryIlOp.Conv_I2
        | NullaryIlOp.Conv_I4
        | NullaryIlOp.Conv_I8
        | NullaryIlOp.Conv_R4
        | NullaryIlOp.Conv_R8
        | NullaryIlOp.Conv_U1
        | NullaryIlOp.Conv_U2
        | NullaryIlOp.Conv_U4
        | NullaryIlOp.Conv_U8
        | NullaryIlOp.Conv_r_un
        // Prefixes: the modified op that follows carries any effect.
        | NullaryIlOp.Volatile
        | NullaryIlOp.Tail
        | NullaryIlOp.Readonly
        // Allocates on the thread's own stack (not the shared managed
        // heap), so no cross-thread effect.
        | NullaryIlOp.Localloc
        // Read the type field of a typed-reference struct already sitting
        // on the eval stack.
        | NullaryIlOp.Refanytype
        // Pulls the current frame's vararg pointer.
        | NullaryIlOp.Arglist
        // Debugger trap. No semantic effect on the guest program.
        | NullaryIlOp.Break -> ContextSwitchPrior.Never

        // ---- InterpreterOnly: bookkeeping mutation ----
        // Arithmetic on a `WidenedNativeInt` operand materialises hash bits
        // via `PointerHashSynthesis.materialiseHashBits`, which mutates
        // `state.PointerHashState`. The counter assignment is observable
        // by another thread that subsequently consumes a `OpaqueHashBits`
        // value derived from the same pointer, but only through that
        // indirect channel. In typical numeric code these operands aren't
        // `WidenedNativeInt`, so most executions don't even mutate the
        // counter.
        | NullaryIlOp.Add
        | NullaryIlOp.Sub
        | NullaryIlOp.Mul
        | NullaryIlOp.Neg
        | NullaryIlOp.Not
        | NullaryIlOp.Shl
        | NullaryIlOp.Shr
        | NullaryIlOp.Shr_un
        | NullaryIlOp.And
        | NullaryIlOp.Or
        | NullaryIlOp.Xor
        // `Conv_I` and `Conv_U` may call
        // `ManagedPointerByteView.anchorByteViewIfPlainArrayByref`, which
        // reads `state.ManagedHeap.Arrays` to decide whether to anchor a
        // byte-view projection. Reading the (post-publication, immutable)
        // array spine is bookkeeping rather than a guest sync event.
        | NullaryIlOp.Conv_I
        | NullaryIlOp.Conv_U -> ContextSwitchPrior.InterpreterOnly

        // ---- RarelyGuestVisible: conditional / data-dependent effects ----
        // Direct loads/stores of local-variable or argument slots. CIL
        // semantics make these frame-private *unless* the slot's address
        // has been published via `ldloca`/`ldarga` + an escape (e.g.
        // `Unsafe.AsPointer` writing the byref into shared storage).
        // Published-local escape is uncommon, but when it happens these
        // ops genuinely race against another thread's load/store through
        // the escaped byref. The classifier is context-free and can't see
        // whether escape has occurred; under PCT we get to call it
        // "rarely visible" without losing soundness.
        | NullaryIlOp.LdArg0
        | NullaryIlOp.LdArg1
        | NullaryIlOp.LdArg2
        | NullaryIlOp.LdArg3
        | NullaryIlOp.Ldloc_0
        | NullaryIlOp.Ldloc_1
        | NullaryIlOp.Ldloc_2
        | NullaryIlOp.Ldloc_3
        | NullaryIlOp.Stloc_0
        | NullaryIlOp.Stloc_1
        | NullaryIlOp.Stloc_2
        | NullaryIlOp.Stloc_3
        // Trapping arithmetic / overflow-checked conversions / Ckfinite:
        // each may construct a CLR-defined runtime exception
        // (OverflowException, DivideByZeroException, ArithmeticException)
        // on guest-supplied values. The exception object is heap-allocated,
        // but unless the guest deliberately publishes it (rare) the
        // identity stays thread-local.
        | NullaryIlOp.Add_ovf
        | NullaryIlOp.Add_ovf_un
        | NullaryIlOp.Sub_ovf
        | NullaryIlOp.Sub_ovf_un
        | NullaryIlOp.Mul_ovf
        | NullaryIlOp.Mul_ovf_un
        | NullaryIlOp.Div
        | NullaryIlOp.Div_un
        | NullaryIlOp.Rem
        | NullaryIlOp.Rem_un
        | NullaryIlOp.Conv_ovf_i
        | NullaryIlOp.Conv_ovf_u
        | NullaryIlOp.Conv_ovf_i1
        | NullaryIlOp.Conv_ovf_i2
        | NullaryIlOp.Conv_ovf_i4
        | NullaryIlOp.Conv_ovf_i8
        | NullaryIlOp.Conv_ovf_u1
        | NullaryIlOp.Conv_ovf_u2
        | NullaryIlOp.Conv_ovf_u4
        | NullaryIlOp.Conv_ovf_u8
        | NullaryIlOp.Conv_ovf_i_un
        | NullaryIlOp.Conv_ovf_u_un
        | NullaryIlOp.Conv_ovf_i1_un
        | NullaryIlOp.Conv_ovf_i2_un
        | NullaryIlOp.Conv_ovf_i4_un
        | NullaryIlOp.Conv_ovf_i8_un
        | NullaryIlOp.Conv_ovf_u1_un
        | NullaryIlOp.Conv_ovf_u2_un
        | NullaryIlOp.Conv_ovf_u4_un
        | NullaryIlOp.Conv_ovf_u8_un
        | NullaryIlOp.Ckfinite
        // Explicit raise / re-raise: allocates / re-reads an exception
        // object. Like the trapping family, the exception is normally
        // thread-local.
        | NullaryIlOp.Throw
        | NullaryIlOp.Rethrow
        // `Ret` is frame-local for nested returns but bottom-frame `Ret`
        // terminates the thread (mutates `ThreadState`, wakes joiners).
        // Most Rets are nested, so the cross-thread effect is rare per
        // execution.
        | NullaryIlOp.Ret
        // Exception-dispatch end ops: only executed in exception paths,
        // and the active exception object is thread-local unless the
        // guest published it.
        | NullaryIlOp.Endfilter
        | NullaryIlOp.Endfinally -> ContextSwitchPrior.RarelyGuestVisible

        // ---- AlwaysGuestVisible: every execution reads/writes shared state ----
        // Indirect access through an arbitrary managed pointer: the pointer
        // may target the shared heap and we can't tell otherwise.
        | NullaryIlOp.Ldind_ref
        | NullaryIlOp.Stind_ref
        | NullaryIlOp.Stind_I
        | NullaryIlOp.Stind_I1
        | NullaryIlOp.Stind_I2
        | NullaryIlOp.Stind_I4
        | NullaryIlOp.Stind_I8
        | NullaryIlOp.Stind_R4
        | NullaryIlOp.Stind_R8
        | NullaryIlOp.Ldind_i
        | NullaryIlOp.Ldind_i1
        | NullaryIlOp.Ldind_i2
        | NullaryIlOp.Ldind_i4
        | NullaryIlOp.Ldind_i8
        | NullaryIlOp.Ldind_u1
        | NullaryIlOp.Ldind_u2
        | NullaryIlOp.Ldind_u4
        | NullaryIlOp.Ldind_u8
        | NullaryIlOp.Ldind_r4
        | NullaryIlOp.Ldind_r8
        // Array length / element access on the shared heap.
        | NullaryIlOp.LdLen
        | NullaryIlOp.Ldelem_i
        | NullaryIlOp.Ldelem_i1
        | NullaryIlOp.Ldelem_u1
        | NullaryIlOp.Ldelem_i2
        | NullaryIlOp.Ldelem_u2
        | NullaryIlOp.Ldelem_i4
        | NullaryIlOp.Ldelem_u4
        | NullaryIlOp.Ldelem_i8
        | NullaryIlOp.Ldelem_u8
        | NullaryIlOp.Ldelem_r4
        | NullaryIlOp.Ldelem_r8
        | NullaryIlOp.Ldelem_ref
        | NullaryIlOp.Stelem_i
        | NullaryIlOp.Stelem_i1
        | NullaryIlOp.Stelem_u1
        | NullaryIlOp.Stelem_i2
        | NullaryIlOp.Stelem_u2
        | NullaryIlOp.Stelem_i4
        | NullaryIlOp.Stelem_u4
        | NullaryIlOp.Stelem_i8
        | NullaryIlOp.Stelem_u8
        | NullaryIlOp.Stelem_r4
        | NullaryIlOp.Stelem_r8
        | NullaryIlOp.Stelem_ref
        // Block copy / init through arbitrary pointers.
        | NullaryIlOp.Cpblk
        | NullaryIlOp.Initblk -> ContextSwitchPrior.AlwaysGuestVisible

    /// Classify a `UnaryConstIlOp`.
    let ofUnaryConst (op : UnaryConstIlOp) : ContextSwitchPrior =
        match op with
        // Constants and pure control flow: nothing observable.
        | UnaryConstIlOp.Ldc_I4 _
        | UnaryConstIlOp.Ldc_I4_s _
        | UnaryConstIlOp.Ldc_I8 _
        | UnaryConstIlOp.Ldc_R4 _
        | UnaryConstIlOp.Ldc_R8 _
        | UnaryConstIlOp.Br _
        | UnaryConstIlOp.Br_s _
        | UnaryConstIlOp.Brfalse _
        | UnaryConstIlOp.Brfalse_s _
        | UnaryConstIlOp.Brtrue _
        | UnaryConstIlOp.Brtrue_s _
        | UnaryConstIlOp.Beq _
        | UnaryConstIlOp.Beq_s _
        | UnaryConstIlOp.Blt _
        | UnaryConstIlOp.Blt_s _
        | UnaryConstIlOp.Ble _
        | UnaryConstIlOp.Ble_s _
        | UnaryConstIlOp.Bgt _
        | UnaryConstIlOp.Bgt_s _
        | UnaryConstIlOp.Bge _
        | UnaryConstIlOp.Bge_s _
        | UnaryConstIlOp.Bne_un _
        | UnaryConstIlOp.Bne_un_s _
        | UnaryConstIlOp.Bge_un _
        | UnaryConstIlOp.Bge_un_s _
        | UnaryConstIlOp.Bgt_un _
        | UnaryConstIlOp.Bgt_un_s _
        | UnaryConstIlOp.Ble_un _
        | UnaryConstIlOp.Ble_un_s _
        | UnaryConstIlOp.Blt_un _
        | UnaryConstIlOp.Blt_un_s _
        | UnaryConstIlOp.Leave _
        | UnaryConstIlOp.Leave_s _
        // Prefix: the next op (a load/store) carries any effect.
        | UnaryConstIlOp.Unaligned _ -> ContextSwitchPrior.Never

        // Local-variable / argument access. As for the nullary
        // `Ldloc_*`/`Stloc_*`/`LdArg*` group: frame-private unless the
        // slot's address has escaped. `Ldloca`/`Ldarga` produce the byref
        // that enables escape — treating the address-take as visible too
        // gives the scheduler a chance to switch right at the escape
        // boundary.
        | UnaryConstIlOp.Stloc _
        | UnaryConstIlOp.Stloc_s _
        | UnaryConstIlOp.Ldloc _
        | UnaryConstIlOp.Ldloc_s _
        | UnaryConstIlOp.Ldloca _
        | UnaryConstIlOp.Ldloca_s _
        | UnaryConstIlOp.Ldarg _
        | UnaryConstIlOp.Ldarg_s _
        | UnaryConstIlOp.Ldarga _
        | UnaryConstIlOp.Ldarga_s _
        | UnaryConstIlOp.Starg _
        | UnaryConstIlOp.Starg_s _ -> ContextSwitchPrior.RarelyGuestVisible

    /// Classify a `UnaryMetadataTokenIlOp`.
    let ofUnaryMetadata (op : UnaryMetadataTokenIlOp) : ContextSwitchPrior =
        match op with
        // Calls — callee may touch arbitrary shared state. Also subsumes
        // calls to runtime intrinsics (`Monitor.Enter`, `Volatile.Read`,
        // `Interlocked.*`) which are explicit sync points.
        | UnaryMetadataTokenIlOp.Call
        | UnaryMetadataTokenIlOp.Calli
        | UnaryMetadataTokenIlOp.Callvirt
        | UnaryMetadataTokenIlOp.Jmp
        // Allocations: pull addresses out of the shared allocator, then
        // `Newobj` invokes the constructor (call into unknown code).
        // Almost every allocation site is followed by publication.
        | UnaryMetadataTokenIlOp.Newobj
        | UnaryMetadataTokenIlOp.Newarr
        // Field access (instance + static).
        | UnaryMetadataTokenIlOp.Stfld
        | UnaryMetadataTokenIlOp.Ldfld
        | UnaryMetadataTokenIlOp.Ldflda
        | UnaryMetadataTokenIlOp.Stsfld
        | UnaryMetadataTokenIlOp.Ldsfld
        | UnaryMetadataTokenIlOp.Ldsflda
        // Array element ops on the shared heap.
        | UnaryMetadataTokenIlOp.Stelem
        | UnaryMetadataTokenIlOp.Ldelem
        | UnaryMetadataTokenIlOp.Ldelema
        // Indirection through an arbitrary managed pointer.
        | UnaryMetadataTokenIlOp.Stobj
        | UnaryMetadataTokenIlOp.Ldobj
        | UnaryMetadataTokenIlOp.Cpobj
        | UnaryMetadataTokenIlOp.Initobj
        // Boxing allocates and publishes; unboxing dereferences.
        | UnaryMetadataTokenIlOp.Box
        | UnaryMetadataTokenIlOp.Unbox
        | UnaryMetadataTokenIlOp.Unbox_Any
        // Read the heap object's type tag.
        | UnaryMetadataTokenIlOp.Castclass
        | UnaryMetadataTokenIlOp.Isinst
        // Read the heap object's vtable.
        | UnaryMetadataTokenIlOp.Ldvirtftn -> ContextSwitchPrior.AlwaysGuestVisible

        // `Refanyval` reads the typed-reference's embedded type tag; on
        // mismatch it raises `InvalidCastException`. Match is data-
        // dependent, so the exception path is rare.
        | UnaryMetadataTokenIlOp.Refanyval -> ContextSwitchPrior.RarelyGuestVisible

        // Token-resolution ops. Each resolves a type / method / field
        // token through `IlMachineState.resolveTypeMetadataToken` /
        // `ExecutionConcretizationModule.concretizeMethodForExecution`,
        // both of which lazily populate `state.ConcreteTypes` and may
        // load assemblies into `_LoadedAssemblies`. The caches are keyed
        // by stable token identity, so the *values* are deterministic
        // after fill; only the *insertion order* is schedule-dependent.
        // That's interpreter-bookkeeping leakage rather than direct
        // guest semantic effect.
        //   - `Ldftn`: `concretizeMethodForExecution` + `resolveMember`.
        //   - `Sizeof`: `resolveTypeMetadataToken` + `concretizeType`.
        //   - `Constrained`: prefix that resolves the constrained type.
        //   - `Mkrefany`: packages a typed reference; materialising the
        //     embedded type handle uses the same resolution machinery.
        //   - `Ldtoken`: pushes a `RuntimeXHandle`; first touch allocates
        //     the handle on the shared heap, later touches return the
        //     cached identity.
        | UnaryMetadataTokenIlOp.Ldftn
        | UnaryMetadataTokenIlOp.Sizeof
        | UnaryMetadataTokenIlOp.Constrained
        | UnaryMetadataTokenIlOp.Mkrefany
        | UnaryMetadataTokenIlOp.Ldtoken -> ContextSwitchPrior.InterpreterOnly

    /// Classify a `UnaryStringTokenIlOp`.
    let ofUnaryString (op : UnaryStringTokenIlOp) : ContextSwitchPrior =
        match op with
        // `Ldstr` resolves a literal to a managed `String`. The interning
        // table is keyed by (assembly, token), so the resulting identity
        // is deterministic once filled; only the first-touch allocation
        // order varies with schedule.
        | UnaryStringTokenIlOp.Ldstr -> ContextSwitchPrior.InterpreterOnly

    /// Top-level classifier: dispatch on the `IlOp` shape.
    let ofIlOp (op : IlOp) : ContextSwitchPrior =
        match op with
        | IlOp.Nullary op -> ofNullary op
        | IlOp.UnaryConst op -> ofUnaryConst op
        | IlOp.UnaryMetadataToken (op, _) -> ofUnaryMetadata op
        | IlOp.UnaryStringToken (op, _) -> ofUnaryString op
        // Multi-target branch on the integer at the top of the eval
        // stack; pure frame-local control flow.
        | IlOp.Switch _ -> ContextSwitchPrior.Never
