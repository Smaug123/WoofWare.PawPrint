namespace WoofWare.PawPrint

/// Coarse classification of an IL opcode by whether its step can be observed by,
/// or could observe an effect from, another thread.
///
/// This is the foundation for schedule fuzzing (Antithesis-style partial-order
/// reduction): the scheduler only needs to consider context switches at
/// `GloballyVisible` points, because by definition no other thread can tell when
/// a `ThreadLocal` step occurs. Wherever we are unsure, we err on the side of
/// `GloballyVisible`. The contract is one-sided: a true global effect must
/// classify as `GloballyVisible`, but classifying a strictly-local op as
/// `GloballyVisible` only costs schedule-space, not correctness.
///
/// The rule we apply: an op is `GloballyVisible` if its IL semantics include
/// any of
///  (a) reading or writing the shared managed heap, statics, sync primitives
///      or arbitrary indirection;
///  (b) reading or writing shared interpreter/scheduler state — for example
///      mutating `state.PointerHashCounters` from `WidenedNativeInt` operands
///      (the counter-assignment order is guest-observable through
///      `OpaqueHashBits`), or transitioning a thread out of `Runnable` /
///      out of existence and waking joiners;
///  (c) raising a CLR-defined runtime exception as a normal outcome on
///      guest-supplied values — constructing the exception object is itself
///      a heap allocation, and its identity is observable by other threads.
/// Anything else is `ThreadLocal`.
///
/// The classifier deliberately ignores runtime context (what's on the eval
/// stack, where a pointer points, what a `Call` target does). That makes it a
/// pure function of `IlOp` — cheap, easy to test exhaustively, and free of
/// cycles against the machine state. Sharpening (e.g. "this call only touches
/// thread-local state") can layer on later as a separate analysis.
type Visibility =
    /// The op only touches frame-local state (eval stack, locals, args, return
    /// address, the thread's own stack). No other thread can observe its
    /// occurrence or its effects directly. The scheduler is free to run a
    /// contiguous run of `ThreadLocal` ops without checking for interleaving.
    | ThreadLocal
    /// The op may read or write state shared with other threads (managed heap
    /// fields, statics, array elements, indirection through pointers, sync
    /// primitives), or transfers control into a callee whose effects we cannot
    /// see from the opcode alone. A schedule-fuzzing scheduler must treat this
    /// op as a potential context-switch boundary.
    | GloballyVisible

[<RequireQualifiedAccess>]
module OpVisibility =

    /// See `Visibility` for the contract. The match is exhaustive so any new
    /// `NullaryIlOp` constructor will fail to compile here until classified.
    let classifyNullary (op : NullaryIlOp) : Visibility =
        match op with
        // ---- Frame-local stack / register operations ----
        | NullaryIlOp.Nop
        | NullaryIlOp.LdArg0
        | NullaryIlOp.LdArg1
        | NullaryIlOp.LdArg2
        | NullaryIlOp.LdArg3
        | NullaryIlOp.Ldloc_0
        | NullaryIlOp.Ldloc_1
        | NullaryIlOp.Ldloc_2
        | NullaryIlOp.Ldloc_3
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
        | NullaryIlOp.Stloc_0
        | NullaryIlOp.Stloc_1
        | NullaryIlOp.Stloc_2
        | NullaryIlOp.Stloc_3 -> Visibility.ThreadLocal

        // ---- Pure eval-stack comparisons ----
        // Comparisons read both operands but don't materialise any pointer-hash
        // bits (see `EvalStackValueComparisons.ceq`/etc.), so they don't
        // mutate `PointerHashCounters` and aren't a publication point.
        | NullaryIlOp.Ceq
        | NullaryIlOp.Cgt
        | NullaryIlOp.Cgt_un
        | NullaryIlOp.Clt
        | NullaryIlOp.Clt_un -> Visibility.ThreadLocal

        // ---- Conversions ----
        // Conv_* preserve `WidenedNativeInt` / `OpaqueHashBits` provenance
        // rather than materialising hash bits, so they don't touch
        // `PointerHashCounters`. The non-checked Conv_* variants don't raise
        // (they truncate), so they're frame-local.
        | NullaryIlOp.Conv_I
        | NullaryIlOp.Conv_I1
        | NullaryIlOp.Conv_I2
        | NullaryIlOp.Conv_I4
        | NullaryIlOp.Conv_I8
        | NullaryIlOp.Conv_R4
        | NullaryIlOp.Conv_R8
        | NullaryIlOp.Conv_U
        | NullaryIlOp.Conv_U1
        | NullaryIlOp.Conv_U2
        | NullaryIlOp.Conv_U4
        | NullaryIlOp.Conv_U8
        | NullaryIlOp.Conv_r_un -> Visibility.ThreadLocal

        // ---- Thread-local control / exception machinery that does not allocate ----
        // Endfilter/Endfinally just transfer control within the thread's own
        // exception dispatch state. Localloc allocates on the thread's own
        // stack, not the shared managed heap.
        | NullaryIlOp.Endfilter
        | NullaryIlOp.Endfinally
        | NullaryIlOp.Localloc -> Visibility.ThreadLocal

        // ---- Prefixes; the modified op that follows is what carries effects ----
        | NullaryIlOp.Volatile
        | NullaryIlOp.Tail
        | NullaryIlOp.Readonly -> Visibility.ThreadLocal

        // ---- Frame-local miscellany ----
        // Break is a debugger trap; Arglist pulls the current frame's vararg
        // pointer; Refanytype reads the type field of a typed-reference struct
        // already on the eval stack.
        | NullaryIlOp.Break
        | NullaryIlOp.Arglist
        | NullaryIlOp.Refanytype -> Visibility.ThreadLocal

        // ---- Thread lifecycle ----
        // `Ret` is frame-local for nested returns, but bottom-frame `Ret`
        // terminates the thread, which mutates `state.ThreadState` to
        // `Terminated` and wakes any `Join`-blocked thread. This classifier
        // is context-free and cannot distinguish nested returns from
        // termination returns, so it has to assume the visible case.
        | NullaryIlOp.Ret -> Visibility.GloballyVisible

        // ---- Arithmetic that may materialise pointer-hash bits ----
        // When either operand is an `Int64Source.WidenedNativeInt`, the
        // arithmetic path in `BinaryArithmetic.execute` (and the unary
        // helpers in `NullaryIlOp` for `Neg` / `Not` / shifts) materialises
        // the hash bits via `PointerHashSynthesis.materialiseHashBits`,
        // which mutates `state.PointerHashCounters`. The order in which two
        // threads consume that counter is guest-observable through the
        // resulting `OpaqueHashBits`, so these ops are publication points.
        // The classifier is context-free and cannot tell which `Int64Source`
        // variant is on the stack, so by the conservative-when-in-doubt
        // contract all of these classify visible.
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
        | NullaryIlOp.Xor -> Visibility.GloballyVisible

        // ---- Trapping arithmetic / conversions / explicit raise ----
        // Each of these may raise a CLR-defined runtime exception
        // (OverflowException, DivideByZeroException, ArithmeticException,
        // NullReferenceException) on guest-supplied values. Constructing the
        // exception object is a heap allocation — observable allocation order
        // and object identity are shared state, so by the rule stated at the
        // top of this file these are GloballyVisible. Treating the op itself
        // (rather than the exception-dispatch step) as the visible boundary
        // means the scheduler doesn't need to know that an exception will be
        // raised in order to schedule correctly.
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
        | NullaryIlOp.Ckfinite -> Visibility.GloballyVisible

        // Throw allocates a NullReferenceException when the operand is null
        // (CLI I.12.4.2). Rethrow reads through the active exception object,
        // which lives on the shared heap. Either way the op is a publication
        // point.
        | NullaryIlOp.Throw
        | NullaryIlOp.Rethrow -> Visibility.GloballyVisible

        // ---- Heap / indirect access ----
        // Ldind_* and Stind_* dereference an arbitrary managed pointer; that
        // pointer may target the shared heap.
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
        | NullaryIlOp.Ldind_r8 -> Visibility.GloballyVisible

        // Array length: arrays live on the shared heap and although their
        // length is immutable post-construction, observing the length implies
        // observing the allocation, which is a synchronisation event under any
        // weak memory model worth modelling. Conservative.
        | NullaryIlOp.LdLen -> Visibility.GloballyVisible

        // Array element load/store on the shared heap.
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
        | NullaryIlOp.Stelem_ref -> Visibility.GloballyVisible

        // Block copy / init through arbitrary pointers.
        | NullaryIlOp.Cpblk
        | NullaryIlOp.Initblk -> Visibility.GloballyVisible

    /// See `Visibility` for the contract.
    let classifyUnaryConst (op : UnaryConstIlOp) : Visibility =
        match op with
        // Locals / args: frame-private storage.
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
        | UnaryConstIlOp.Starg_s _ -> Visibility.ThreadLocal

        // Constant pushes.
        | UnaryConstIlOp.Ldc_I4 _
        | UnaryConstIlOp.Ldc_I4_s _
        | UnaryConstIlOp.Ldc_I8 _
        | UnaryConstIlOp.Ldc_R4 _
        | UnaryConstIlOp.Ldc_R8 _ -> Visibility.ThreadLocal

        // Branches and frame-local control flow.
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
        | UnaryConstIlOp.Leave_s _ -> Visibility.ThreadLocal

        // Prefix; the next op (a load/store) is the visible one.
        | UnaryConstIlOp.Unaligned _ -> Visibility.ThreadLocal

    /// See `Visibility` for the contract.
    let classifyUnaryMetadata (op : UnaryMetadataTokenIlOp) : Visibility =
        match op with
        // Calls. The callee may touch arbitrary shared state; without
        // resolving the target we must treat the boundary as visible. This
        // also subsumes calls to runtime intrinsics like `Monitor.Enter`,
        // `Volatile.Read`, `Interlocked.*`, etc., which is exactly correct.
        | UnaryMetadataTokenIlOp.Call
        | UnaryMetadataTokenIlOp.Calli
        | UnaryMetadataTokenIlOp.Callvirt
        | UnaryMetadataTokenIlOp.Jmp -> Visibility.GloballyVisible

        // Allocations are observable: they pull addresses out of the shared
        // heap, mutate allocation state, and crucially `Newobj` then calls a
        // constructor (which is itself visible territory).
        | UnaryMetadataTokenIlOp.Newobj
        | UnaryMetadataTokenIlOp.Newarr -> Visibility.GloballyVisible

        // Instance / static field access.
        | UnaryMetadataTokenIlOp.Stfld
        | UnaryMetadataTokenIlOp.Ldfld
        | UnaryMetadataTokenIlOp.Ldflda
        | UnaryMetadataTokenIlOp.Stsfld
        | UnaryMetadataTokenIlOp.Ldsfld
        | UnaryMetadataTokenIlOp.Ldsflda -> Visibility.GloballyVisible

        // Array element ops on the shared heap.
        | UnaryMetadataTokenIlOp.Stelem
        | UnaryMetadataTokenIlOp.Ldelem
        | UnaryMetadataTokenIlOp.Ldelema -> Visibility.GloballyVisible

        // Indirection through an arbitrary managed pointer.
        | UnaryMetadataTokenIlOp.Stobj
        | UnaryMetadataTokenIlOp.Ldobj
        | UnaryMetadataTokenIlOp.Cpobj
        | UnaryMetadataTokenIlOp.Initobj -> Visibility.GloballyVisible

        // Boxing allocates; unboxing dereferences.
        | UnaryMetadataTokenIlOp.Box
        | UnaryMetadataTokenIlOp.Unbox
        | UnaryMetadataTokenIlOp.Unbox_Any -> Visibility.GloballyVisible

        // Read the heap object's type tag. The tag is logically immutable,
        // but observing it implies the object has been published; under any
        // memory model weaker than SC that publication is a synchronisation
        // event. Conservative.
        | UnaryMetadataTokenIlOp.Castclass
        | UnaryMetadataTokenIlOp.Isinst -> Visibility.GloballyVisible

        // Reads the vtable of the object on the stack.
        | UnaryMetadataTokenIlOp.Ldvirtftn -> Visibility.GloballyVisible

        // Ldtoken pushes a RuntimeTypeHandle / RuntimeFieldHandle /
        // RuntimeMethodHandle. In this runtime those handles are realised as
        // managed objects on the shared heap, allocated and cached the first
        // time the token is encountered (see executeLdtoken). First touch
        // mutates shared state, and later touches observe the cached
        // identity, so this is a heap-publication point either way.
        | UnaryMetadataTokenIlOp.Ldtoken -> Visibility.GloballyVisible

        // Refanyval reads the typed-reference struct's embedded type tag and
        // throws InvalidCastException if it doesn't match the metadata token
        // (CLI III.4.20). By the same rule that makes Throw / the _ovf family
        // visible, allocating an InvalidCastException is heap publication.
        | UnaryMetadataTokenIlOp.Refanyval -> Visibility.GloballyVisible

        // Ldftn picks a method pointer from metadata; the load itself is
        // pure (no object dereference). Sizeof is a constant push from
        // immutable metadata. Constrained is a prefix on a following
        // call/callvirt, which will classify visible. Mkrefany packages a
        // typed-reference struct on the stack from a managed pointer +
        // type token, without dereferencing the address it carries; the
        // next op that uses the address will be visible.
        | UnaryMetadataTokenIlOp.Ldftn
        | UnaryMetadataTokenIlOp.Sizeof
        | UnaryMetadataTokenIlOp.Constrained
        | UnaryMetadataTokenIlOp.Mkrefany -> Visibility.ThreadLocal

    /// See `Visibility` for the contract.
    let classifyUnaryString (op : UnaryStringTokenIlOp) : Visibility =
        match op with
        // Ldstr resolves a literal to a managed String object. The interning
        // table is logically immutable from the guest's perspective, but the
        // first time a literal is loaded the String object itself has to be
        // allocated and the interning table mutated (see state.InternedStrings
        // updates in executeLdstr). Subsequent loads observe that allocation's
        // object identity, which is shared-heap state. GloballyVisible.
        | UnaryStringTokenIlOp.Ldstr -> Visibility.GloballyVisible

    /// See `Visibility` for the contract.
    let classify (op : IlOp) : Visibility =
        match op with
        | IlOp.Nullary op -> classifyNullary op
        | IlOp.UnaryConst op -> classifyUnaryConst op
        | IlOp.UnaryMetadataToken (op, _) -> classifyUnaryMetadata op
        | IlOp.UnaryStringToken (op, _) -> classifyUnaryString op
        // Switch is a multi-target branch based on the integer at the top of
        // the eval stack. Pure frame-local control flow.
        | IlOp.Switch _ -> Visibility.ThreadLocal
