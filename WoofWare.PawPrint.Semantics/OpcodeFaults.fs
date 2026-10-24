namespace WoofWare.PawPrint

/// A fault the execution engine raises from an instruction itself, as opposed to one that reaches
/// the instruction from a callee.
///
/// Named abstractly rather than as a resolved type, because this library has no assemblies loaded
/// and so has no type to name. A consumer that does — the interpreter, an analyser holding a
/// corelib — maps these onto its own representation.
[<RequireQualifiedAccess>]
type OpcodeFault =
    /// `System.NullReferenceException`.
    | NullReference
    /// `System.IndexOutOfRangeException`.
    | IndexOutOfRange
    /// `System.ArrayTypeMismatchException`.
    | ArrayTypeMismatch
    /// `System.InvalidCastException`.
    | InvalidCast
    /// `System.OverflowException`. This is also what CoreCLR raises for `ckfinite`, where ECMA-335
    /// III.3.19 names its base class `System.ArithmeticException`: the JIT's throw block for a
    /// non-finite operand is the one it uses for overflow.
    | Overflow
    /// `System.DivideByZeroException`.
    | DivideByZero
    /// `System.OutOfMemoryException`.
    | OutOfMemory
    /// `System.StackOverflowException`.
    | StackOverflow
    /// `System.TypeInitializationException`, from a `.cctor` that threw. Triggered by a
    /// static-field access or by an invocation, per ECMA-335 I.8.9.5 — so it is not confined to the
    /// instructions that name a static field.
    ///
    /// Every instruction that can raise this can also raise <see cref="StackOverflow"/>, because
    /// running an initializer means entering a frame. `TestOpcodeFaults` checks that over the whole
    /// table rather than leaving it to each entry.
    | TypeInitialization

/// A fault the execution engine raises at an instruction while resolving its metadata token against
/// the method the instruction is in, as opposed to one from performing the operation: a category of
/// faults rather than one, because which of them happens depends on that referencing context as
/// much as on the token, which a table keyed on the opcode cannot see. An entry carrying one says
/// that CoreCLR resolves the instruction's token that way, so it can fail there; a consumer that
/// can see the context decides whether it does.
[<RequireQualifiedAccess>]
type ResolutionFault =
    /// The token fails to bind to a definition, or the JIT refuses the instruction for what it binds
    /// to. The assembly it refers into may be missing or unloadable (`FileNotFoundException`,
    /// `FileLoadException`, `BadImageFormatException`), and the token may fail to resolve against
    /// one that loaded (`TypeLoadException`, `MissingFieldException`, `MissingMethodException`,
    /// `InvalidProgramException`). Or it binds, and the JIT rejects the instruction for what it
    /// names, with one of the same: `call` or `ldftn` of an abstract method
    /// (`BadImageFormatException`), `box` of a byref-like type that it does not fold away
    /// (`InvalidProgramException`; `BoxPattern`), or `newarr` of one (`TypeLoadException`). CoreCLR's JIT resolves every token of a method as it
    /// compiles it, so it raises most of these where the method is called, before any of its
    /// instructions runs.
    | TokenBinding
    /// The method is a `DynamicMethod`, and the token names an entry of its scope that the
    /// instruction cannot use: past the scope's end (`ArgumentOutOfRangeException`, from the
    /// scope's own list), null or of the wrong kind or shape (`InvalidProgramException`,
    /// `BadImageFormatException`), not a string for `ldstr` (`NullReferenceException`), or a static
    /// method for `callvirt` (`MissingMethodException`). Only a `DynamicMethod`'s body has scope
    /// entries, so no method in metadata raises these.
    | ScopeEntry
    /// The method may not access the method the token names, the type declaring it, or a type
    /// argument of either, by CoreCLR's visibility rules, which `InternalsVisibleTo` and `IgnoresAccessChecksTo` relax:
    /// `MethodAccessException`. The JIT compiles a failed check to a call that raises it at the
    /// instruction. Code that a compiler checked against the assemblies it runs with passes every
    /// check; code that runs against a later version of an assembly that hides a member, or IL no
    /// compiler checked, need not.
    | MethodAccess
    /// The same as `MethodAccess`, for a field the token names: `FieldAccessException`.
    | FieldAccess
    /// The same as `MethodAccess`, for a type the token names, or one of its type arguments:
    /// `TypeAccessException`.
    | TypeAccess
    /// Dispatching on the receiver's class finds no single method to run: the most specific
    /// implementation is a reabstraction (`EntryPointNotFoundException`), or no default interface
    /// body is most specific (`System.Runtime.AmbiguousImplementationException`). Neither arises
    /// within one C# compilation: it takes an interface changed after its implementer was compiled.
    | DispatchOnReceiver
    /// The same as `DispatchOnReceiver`, dispatching on the type a `constrained.` prefix names. A
    /// table keyed on the opcode cannot see the prefix, so every instruction that can take one
    /// carries this. In code CoreCLR shares between instantiations, a `constrained. ldftn` may
    /// instead hand out a pointer that raises when called.
    | DispatchOnConstrainedType

/// What performing one instruction can raise by itself.
[<RequireQualifiedAccess>]
type OpcodeFaults =
    /// Performing the operation raises exactly `own` and nothing else, and resolving the
    /// instruction's metadata token can raise exactly the faults of the categories in
    /// `resolution`. Empty is therefore a positive claim that the instruction cannot fault, not an
    /// absence of information.
    ///
    /// **Three things this deliberately excludes**, because none is a fact about the opcode:
    ///
    /// * What a callee raises. `call`, `callvirt`, `newobj`, `calli` and `jmp` transfer control,
    ///   and whatever the target does reaches the caller; that belongs to the call graph. The
    ///   entries here are only what the transfer itself can fault on — a null receiver, say.
    /// * Whatever a real runtime does with an *invalid but non-null* address, which the raw-memory
    ///   instructions (`cpblk`, `initblk`, the `ldind`/`stind` family, `ldobj`/`stobj`) can be
    ///   handed by unverifiable IL. ECMA-335's answer is that
    ///   "System.NullReferenceException *can* be thrown if an invalid address is detected"
    ///   (III.3.30) — permissive, and naming the fault these entries already carry.
    ///   `AccessViolationException` appears nowhere in ECMA-335: it is an artefact of hosting the
    ///   CLI on an OS with virtual memory. CoreCLR raises it from the hardware fault, and it is
    ///   *uncatchable* there — a corrupted-state exception that ends the process rather than
    ///   unwinding, as `sourcesPure/UnsafeCopyBlockNullEndpoint.cs` records. So it is not an
    ///   escaping exception at all, and a table read to answer "what can leave this method" would
    ///   be made worse, not better, by carrying it on every pointer dereference.
    /// * Faults from CLI *security policy*: `System.Security.SecurityException`, which ECMA-335
    ///   lists against `calli` (III.3.20) and the `ldftn` family. Code Access Security was removed
    ///   in .NET Core, so on any runtime an analyser will meet there is no policy to reject a call
    ///   and no such exception to raise. This one is excluded because it cannot happen rather than
    ///   because it is uninteresting — if a consumer ever targets a CLI that does implement policy,
    ///   the entries for those instructions are wrong and not merely imprecise.
    | Raises of own : OpcodeFault list * resolution : Set<ResolutionFault>
    /// Not classified, and not classifiable from the opcode alone. A consumer must treat this as
    /// "may raise anything" and must never read it as "cannot raise".
    | Unmodelled

/// What a fault depends on, which is what decides whether a reader wants to see it.
///
/// Not a distinction the CLI draws — .NET has no `Error` hierarchy separating these, as Java does —
/// but the one that matters to somebody reading a report. It exists so a consumer can *choose*;
/// nothing here lets the analysis quietly drop anything.
[<RequireQualifiedAccess>]
type FaultKind =
    /// Determined by the values the program computes and the control flow it takes. A null
    /// dereference, an array bound, a division by zero: each is in principle preventable by the
    /// program, and each is something a reader can act on.
    | Logic
    /// Determined by the environment the program runs in rather than by the program. These attach
    /// to almost every allocation and every call, so reporting them beside logic faults buries the
    /// latter — but they are genuinely possible, so the model keeps them and the *reporting* is
    /// what filters.
    | ResourceExhaustion

/// The bridge from a fault named abstractly to the corelib type that stands for it.
[<RequireQualifiedAccess>]
module OpcodeFault =

    /// The fully-qualified name of the corelib type this fault stands for.
    ///
    /// For a consumer with no assemblies loaded, which cannot use `resolve`: an analyser reporting
    /// what a method may throw wants to name the type without having resolved it. The two are kept
    /// in step by `TestOpcodeFaults`, which resolves every fault against a real corelib and checks
    /// the type it gets back is the one named here.
    let typeName (fault : OpcodeFault) : string =
        match fault with
        | OpcodeFault.NullReference -> "System.NullReferenceException"
        | OpcodeFault.IndexOutOfRange -> "System.IndexOutOfRangeException"
        | OpcodeFault.ArrayTypeMismatch -> "System.ArrayTypeMismatchException"
        | OpcodeFault.InvalidCast -> "System.InvalidCastException"
        | OpcodeFault.Overflow -> "System.OverflowException"
        | OpcodeFault.DivideByZero -> "System.DivideByZeroException"
        | OpcodeFault.OutOfMemory -> "System.OutOfMemoryException"
        | OpcodeFault.StackOverflow -> "System.StackOverflowException"
        | OpcodeFault.TypeInitialization -> "System.TypeInitializationException"

    /// What this fault depends on. Exhaustive with no wildcard: a fault added to `OpcodeFault` has
    /// to be classified here rather than defaulting into either class.
    let kind (fault : OpcodeFault) : FaultKind =
        match fault with
        // Neither is about anything the program computed. `OutOfMemory` attaches to every
        // allocation and `StackOverflow` to `localloc`, so a report carrying them everywhere says
        // nothing a reader can act on — which is the whole reason `FaultKind` exists.
        | OpcodeFault.OutOfMemory
        | OpcodeFault.StackOverflow -> FaultKind.ResourceExhaustion
        // `TypeInitialization` is a logic fault despite being a wrapper around another method's
        // failure: whether it can arise at all is decided by whether the initializer's own code can
        // fail, and that is a fact about the program. Note the consequence, which is measured in
        // `docs/plans/2026-08-26-exception-escape-analysis.md`: an initializer that fails *only*
        // because it allocates still contributes one of these, so filtering
        // `ResourceExhaustion` does not remove every fault whose cause is resource exhaustion.
        | OpcodeFault.TypeInitialization
        | OpcodeFault.NullReference
        | OpcodeFault.IndexOutOfRange
        | OpcodeFault.ArrayTypeMismatch
        | OpcodeFault.InvalidCast
        | OpcodeFault.Overflow
        | OpcodeFault.DivideByZero -> FaultKind.Logic

    /// The corelib type an execution engine raises for this fault.
    ///
    /// Total, and the only place the correspondence is written down: a consumer that wants the
    /// type for a fault must come through here rather than naming a `BaseClassTypes` field
    /// itself, which is what keeps the table and the interpreter from drifting apart.
    let resolve
        (baseClassTypes : BaseClassTypes<'corelib>)
        (fault : OpcodeFault)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        match fault with
        | OpcodeFault.NullReference -> baseClassTypes.NullReferenceException
        | OpcodeFault.IndexOutOfRange -> baseClassTypes.IndexOutOfRangeException
        | OpcodeFault.ArrayTypeMismatch -> baseClassTypes.ArrayTypeMismatchException
        | OpcodeFault.InvalidCast -> baseClassTypes.InvalidCastException
        | OpcodeFault.Overflow -> baseClassTypes.OverflowException
        | OpcodeFault.DivideByZero -> baseClassTypes.DivideByZeroException
        | OpcodeFault.OutOfMemory -> baseClassTypes.OutOfMemoryException
        | OpcodeFault.StackOverflow -> baseClassTypes.StackOverflowException
        | OpcodeFault.TypeInitialization -> baseClassTypes.TypeInitializationException

/// The bridge from a category of resolution faults to the corelib types that stand for it.
[<RequireQualifiedAccess>]
module ResolutionFault =

    /// The fully-qualified names of the corelib types CoreCLR raises for this category.
    /// `TestOpcodeFaults` checks each names a type in a real corelib.
    let typeNames (fault : ResolutionFault) : string list =
        match fault with
        | ResolutionFault.TokenBinding ->
            [
                "System.IO.FileNotFoundException"
                "System.IO.FileLoadException"
                "System.BadImageFormatException"
                "System.TypeLoadException"
                "System.MissingFieldException"
                "System.MissingMethodException"
                "System.InvalidProgramException"
            ]
        | ResolutionFault.ScopeEntry ->
            [
                "System.ArgumentOutOfRangeException"
                "System.InvalidProgramException"
                "System.BadImageFormatException"
                "System.NullReferenceException"
                "System.MissingMethodException"
            ]
        | ResolutionFault.MethodAccess -> [ "System.MethodAccessException" ]
        | ResolutionFault.FieldAccess -> [ "System.FieldAccessException" ]
        | ResolutionFault.TypeAccess -> [ "System.TypeAccessException" ]
        | ResolutionFault.DispatchOnReceiver
        | ResolutionFault.DispatchOnConstrainedType ->
            [
                "System.EntryPointNotFoundException"
                "System.Runtime.AmbiguousImplementationException"
            ]

/// Which faults each IL instruction can raise by itself.
///
/// This is a table rather than an interpreter, which is the point: the same fact serves the
/// concrete interpreter, which decides *whether* a fault happens on this execution, and an analyser
/// that never executes anything and needs only to know *which* are possible.
///
/// A value an integer operand of an instruction may have, which an instruction may need one to
/// have before it can raise a fault (`OpcodeFaults.requiredOperands`).
[<RequireQualifiedAccess>]
type IntegerValue =
    | Zero
    | MinusOne
    /// The least value of the operand's own type: `Int32.MinValue` of an int32, `Int64.MinValue`
    /// of an int64. Where the other operand is a native int, the instruction works at that width,
    /// sign-extending an int32; an int32 then reaches the least value of the instruction's width
    /// only if it is the least of its own.
    | Least

[<RequireQualifiedAccess>]
module IntegerValue =

    /// Whether an int32 operand of the value `operand` has this value.
    let holdsOfInt32 (value : IntegerValue) (operand : int32) : bool =
        match value with
        | IntegerValue.Zero -> operand = 0
        | IntegerValue.MinusOne -> operand = -1
        | IntegerValue.Least -> operand = System.Int32.MinValue

    /// Whether an int64 operand of the value `operand` has this value.
    let holdsOfInt64 (value : IntegerValue) (operand : int64) : bool =
        match value with
        | IntegerValue.Zero -> operand = 0L
        | IntegerValue.MinusOne -> operand = -1L
        | IntegerValue.Least -> operand = System.Int64.MinValue

/// That one operand of an instruction is an integer of a value.
type OperandRequirement =
    {
        /// The operand's depth on the evaluation stack before the instruction runs: 0 for the top.
        FromTop : int
        Is : IntegerValue
    }

/// Every match below is exhaustive with no wildcard, so an opcode added to `IlOp` fails the build
/// here rather than silently acquiring `Raises []`. That distinction is the whole value of the
/// table: `Raises []` is a claim, and a wrong one is a false negative in an analysis that is
/// supposed to be sound.
[<RequireQualifiedAccess>]
module OpcodeFaults =

    /// The entry of an instruction that bears no metadata token, so resolves nothing.
    let private performs (own : OpcodeFault list) : OpcodeFaults = OpcodeFaults.Raises (own, Set.empty)

    let private none = performs []

    let private nullDeref : OpcodeFault list = [ OpcodeFault.NullReference ]

    /// An access to an array element that takes no covariance check: a load, or a store of a
    /// primitive.
    let private uncheckedElementAccess : OpcodeFault list =
        [ OpcodeFault.NullReference ; OpcodeFault.IndexOutOfRange ]

    /// An access to an array element that can also fail the covariance check: a store that may be
    /// of a reference, or the writable address of an element that may be one.
    let private covariantElementAccess : OpcodeFault list =
        [
            OpcodeFault.NullReference
            OpcodeFault.IndexOutOfRange
            OpcodeFault.ArrayTypeMismatch
        ]

    let ofNullary (op : NullaryIlOp) : OpcodeFaults =
        match op with
        // Signed division and remainder fault twice over: on a zero divisor, and on
        // MinValue / -1, whose quotient is not representable.
        | NullaryIlOp.Div
        | NullaryIlOp.Rem -> performs [ OpcodeFault.DivideByZero ; OpcodeFault.Overflow ]
        // The unsigned forms have no unrepresentable quotient, so a zero divisor is the whole of it.
        | NullaryIlOp.Div_un
        | NullaryIlOp.Rem_un -> performs [ OpcodeFault.DivideByZero ]
        | NullaryIlOp.Add_ovf
        | NullaryIlOp.Add_ovf_un
        | NullaryIlOp.Sub_ovf
        | NullaryIlOp.Sub_ovf_un
        | NullaryIlOp.Mul_ovf
        | NullaryIlOp.Mul_ovf_un
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
        | NullaryIlOp.Conv_ovf_u1_un
        | NullaryIlOp.Conv_ovf_i2_un
        | NullaryIlOp.Conv_ovf_u2_un
        | NullaryIlOp.Conv_ovf_i4_un
        | NullaryIlOp.Conv_ovf_u4_un
        | NullaryIlOp.Conv_ovf_i8_un
        | NullaryIlOp.Conv_ovf_u8_un
        | NullaryIlOp.Ckfinite -> performs [ OpcodeFault.Overflow ]
        | NullaryIlOp.Localloc -> performs [ OpcodeFault.StackOverflow ]
        | NullaryIlOp.LdLen -> performs nullDeref
        | NullaryIlOp.Ldind_ref
        | NullaryIlOp.Ldind_i
        | NullaryIlOp.Ldind_i1
        | NullaryIlOp.Ldind_i2
        | NullaryIlOp.Ldind_i4
        | NullaryIlOp.Ldind_i8
        | NullaryIlOp.Ldind_u1
        | NullaryIlOp.Ldind_u2
        | NullaryIlOp.Ldind_u4
        | NullaryIlOp.Ldind_r4
        | NullaryIlOp.Ldind_r8
        | NullaryIlOp.Stind_ref
        | NullaryIlOp.Stind_I
        | NullaryIlOp.Stind_I1
        | NullaryIlOp.Stind_I2
        | NullaryIlOp.Stind_I4
        | NullaryIlOp.Stind_I8
        | NullaryIlOp.Stind_R4
        | NullaryIlOp.Stind_R8
        | NullaryIlOp.Cpblk
        | NullaryIlOp.Initblk -> performs nullDeref
        | NullaryIlOp.Ldelem_i
        | NullaryIlOp.Ldelem_i1
        | NullaryIlOp.Ldelem_u1
        | NullaryIlOp.Ldelem_i2
        | NullaryIlOp.Ldelem_u2
        | NullaryIlOp.Ldelem_i4
        | NullaryIlOp.Ldelem_u4
        | NullaryIlOp.Ldelem_i8
        | NullaryIlOp.Ldelem_r4
        | NullaryIlOp.Ldelem_r8
        | NullaryIlOp.Ldelem_ref
        // A store of a primitive takes no covariance check. ECMA-335 III.4.27 lists
        // `ArrayTypeMismatchException` against every `stelem.<type>`, but its correctness rule
        // requires the instruction's type to be array-element-compatible with the array's own
        // element type, which leaves correct CIL nothing to mismatch; and CoreCLR checks nothing.
        | NullaryIlOp.Stelem_i
        | NullaryIlOp.Stelem_i1
        | NullaryIlOp.Stelem_i2
        | NullaryIlOp.Stelem_i4
        | NullaryIlOp.Stelem_i8
        | NullaryIlOp.Stelem_r4
        | NullaryIlOp.Stelem_r8 -> performs uncheckedElementAccess
        | NullaryIlOp.Stelem_ref -> performs covariantElementAccess
        // A null operand makes `throw` raise instead of throwing what it was handed. What it
        // throws when the operand is *not* null is a fact about the operand, which a consumer
        // reads off the evaluation stack; it is not a fact about the opcode.
        | NullaryIlOp.Throw -> performs nullDeref
        // `rethrow` re-raises whatever the enclosing handler caught. That is a fact about the
        // handler, not about the instruction, so there is nothing this table can say.
        | NullaryIlOp.Rethrow -> OpcodeFaults.Unmodelled
        | NullaryIlOp.Nop
        | NullaryIlOp.Break
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
        | NullaryIlOp.Pop
        | NullaryIlOp.Dup
        | NullaryIlOp.Ret
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
        | NullaryIlOp.Ceq
        | NullaryIlOp.Cgt
        | NullaryIlOp.Cgt_un
        | NullaryIlOp.Clt
        | NullaryIlOp.Clt_un
        | NullaryIlOp.Sub
        | NullaryIlOp.Add
        | NullaryIlOp.Mul
        | NullaryIlOp.Neg
        | NullaryIlOp.Not
        | NullaryIlOp.Shr
        | NullaryIlOp.Shr_un
        | NullaryIlOp.Shl
        | NullaryIlOp.And
        | NullaryIlOp.Or
        | NullaryIlOp.Xor
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
        | NullaryIlOp.Conv_r_un
        | NullaryIlOp.Endfilter
        | NullaryIlOp.Endfinally
        | NullaryIlOp.Volatile
        | NullaryIlOp.Tail
        | NullaryIlOp.Readonly
        | NullaryIlOp.Arglist
        | NullaryIlOp.Refanytype -> none

    let ofUnaryMetadata (op : UnaryMetadataTokenIlOp) : OpcodeFaults =
        // Every one of these bears a token, which the JIT resolves as it compiles the method, so
        // each can fail to bind it, and in a `DynamicMethod` can name a scope entry it cannot use.
        let resolves (own : OpcodeFault list) (resolution : ResolutionFault list) : OpcodeFaults =
            OpcodeFaults.Raises (
                own,
                Set.ofList (ResolutionFault.TokenBinding :: ResolutionFault.ScopeEntry :: resolution)
            )

        // CoreCLR's JIT asks the VM whether the method may access what an instruction's token
        // names, and compiles a refusal to a call of the `ThrowHelpers` method `jithelpers.h` binds
        // to the matching helper, at the instruction (`impHandleAccessAllowed`, importer.cpp). It
        // asks at `call`, `callvirt`, `newobj`, `ldftn` and `ldvirtftn` (`getCallInfo`), at the six
        // field instructions (`getFieldInfo`), and at `newarr`, `box`, `isinst`, `castclass`,
        // `unbox`, `unbox.any` and `mkrefany` (`canAccessClass`); nowhere else.
        match op with
        // "System.InvalidCastException is thrown if obj cannot be cast to typeTok" (ECMA-335
        // III.4.3), as PawPrint's interpreter throws it. CoreCLR's JIT compiles `castclass`, and
        // `unbox.any` of a reference type, to a call of one of CoreLib's `CastHelpers` methods
        // (`impCastClassOrIsInstToTree`, importer.cpp), which raises it there, and whatever else
        // that callee raises reaches the instruction too.
        | UnaryMetadataTokenIlOp.Castclass -> resolves [ OpcodeFault.InvalidCast ] [ ResolutionFault.TypeAccess ]
        // CoreCLR's JIT compiles `unbox`, and `unbox.any` of a value type, to a call of
        // `CastHelpers.Unbox`, or of `Unbox_Nullable` for a `Nullable<T>` (`getUnBoxHelper`,
        // jitinterface.cpp), which raises these there.
        | UnaryMetadataTokenIlOp.Unbox
        | UnaryMetadataTokenIlOp.Unbox_Any ->
            resolves [ OpcodeFault.InvalidCast ; OpcodeFault.NullReference ] [ ResolutionFault.TypeAccess ]
        // `refanyval` compares the requested type against the one the `TypedRef` was built with:
        // "System.InvalidCastException is thrown if type is not identical to the type stored in the
        // TypedRef" (ECMA-335 III.4.28). `mkrefany` is *not* its mirror — it packages an address
        // and a type handle and performs no runtime check, so III.4.16 lists only
        // `TypeLoadException`, which is `ResolutionFault.TokenBinding`. The JIT checks that the
        // method may access the type `mkrefany` names, and not the one `refanyval` names.
        | UnaryMetadataTokenIlOp.Refanyval -> resolves [ OpcodeFault.InvalidCast ] []
        | UnaryMetadataTokenIlOp.Mkrefany -> resolves [] [ ResolutionFault.TypeAccess ]
        // A negative length is the overflow; the allocation itself is the OOM.
        | UnaryMetadataTokenIlOp.Newarr ->
            resolves [ OpcodeFault.Overflow ; OpcodeFault.OutOfMemory ] [ ResolutionFault.TypeAccess ]
        // CoreCLR's JIT boxes a primitive type or an enum inline, and compiles a `box` of another
        // struct to a call of `CastHelpers.Box`, or of `Box_Nullable` for a `Nullable<T>`
        // (`impImportAndPushBox`, importer.cpp), which allocates there.
        | UnaryMetadataTokenIlOp.Box -> resolves [ OpcodeFault.OutOfMemory ] [ ResolutionFault.TypeAccess ]
        // What the constructor itself raises travels by the call edge, not by this entry; running
        // the declaring type's `.cctor` first does not, for the reason given against `call` below.
        | UnaryMetadataTokenIlOp.Newobj ->
            resolves
                [
                    OpcodeFault.OutOfMemory
                    OpcodeFault.StackOverflow
                    OpcodeFault.TypeInitialization
                ]
                [ ResolutionFault.MethodAccess ]
        // The three field instructions that take a receiver may name a *static* field, and then
        // they are a static-field access like `ldsfld`, with a `.cctor` to trigger. ECMA-335
        // III.4.10, III.4.11 and III.4.28 each say the field "can be either an instance field ...
        // or a static field", and each conditions the null check on it: "NullReferenceException is
        // thrown if obj is null *and the field is not static*". `stfld`'s verifiability clause
        // admits the shape too, so it is not merely correct CIL but verifiable CIL.
        //
        // PawPrint's interpreter refuses this shape rather than implementing it
        // (`UnaryMetadataFieldOps.checkFieldStaticness`), so it never raises this itself. That does
        // not licence dropping the entry: this table says what the *instruction* can raise, and it
        // is read by analyses over assemblies PawPrint has never run.
        | UnaryMetadataTokenIlOp.Ldfld
        | UnaryMetadataTokenIlOp.Ldflda
        | UnaryMetadataTokenIlOp.Stfld ->
            resolves
                [
                    OpcodeFault.NullReference
                    OpcodeFault.StackOverflow
                    OpcodeFault.TypeInitialization
                ]
                [ ResolutionFault.FieldAccess ]
        // `ldvirtftn` dispatches on its receiver's class, as `callvirt` does, and raises what that
        // dispatch fails with as it resolves the slot, so no delegate is made.
        | UnaryMetadataTokenIlOp.Ldvirtftn ->
            resolves nullDeref [ ResolutionFault.MethodAccess ; ResolutionFault.DispatchOnReceiver ]
        | UnaryMetadataTokenIlOp.Initobj
        | UnaryMetadataTokenIlOp.Stobj
        | UnaryMetadataTokenIlOp.Cpobj
        | UnaryMetadataTokenIlOp.Ldobj -> resolves nullDeref []
        // The two invoking instructions that also dereference: a receiver for `callvirt`, a
        // function pointer for `calli`. Both can trigger a `.cctor` as well, for the reason given
        // against `call` below.
        //
        // `calli`'s null-dereference entry does not come from the specification. ECMA-335 (6th
        // edition) III.3.20 lists only `System.SecurityException` under `calli`'s "Exceptions",
        // where III.4.2's `callvirt` says in as many words that "System.NullReferenceException is
        // thrown if obj is null"; and III.3.20's "Correctness" requires the pointer to hold the
        // address of a method, so a null one is incorrect CIL whose behaviour the specification
        // does not fix. It comes instead from what PawPrint chooses to do there — a deliberate
        // divergence from CoreCLR, which segfaults — recorded in `docs/divergences.md` and pinned
        // by `TestPureCases`' "calli through a null function pointer throws
        // NullReferenceException".
        // `callvirt` additionally *allocates*, which `calli` does not. Under a `constrained.`
        // prefix, "if thisType is a value type and thisType does not implement method then ptr is
        // dereferenced, boxed, and passed as the 'this' pointer" (ECMA-335 III.2.1) — reachable
        // whenever a generic calls a method it inherited from `Object`, `ValueType` or `Enum`,
        // `t.ToString()` being the everyday case. `executeCallvirt` performs exactly that box, so
        // the allocation is the `callvirt`'s own: were it ever to fail, the instruction in the
        // frame at that moment is this one, and the entry has to say so.
        //
        // `callvirt` dispatches on its receiver's class, or under `constrained.` on the type the
        // prefix names. `calli`'s token is a signature, which names no method to access or
        // dispatch.
        | UnaryMetadataTokenIlOp.Callvirt ->
            resolves
                [
                    OpcodeFault.NullReference
                    OpcodeFault.OutOfMemory
                    OpcodeFault.StackOverflow
                    OpcodeFault.TypeInitialization
                ]
                [
                    ResolutionFault.MethodAccess
                    ResolutionFault.DispatchOnReceiver
                    ResolutionFault.DispatchOnConstrainedType
                ]
        | UnaryMetadataTokenIlOp.Calli ->
            resolves
                [
                    OpcodeFault.NullReference
                    OpcodeFault.StackOverflow
                    OpcodeFault.TypeInitialization
                ]
                []
        | UnaryMetadataTokenIlOp.Ldelem -> resolves uncheckedElementAccess []
        // `ldelema` takes the covariance check too: handing out a writable address to an element
        // of an array whose element type is not the one named would defeat the check `stelem`
        // makes. CoreCLR's JIT compiles an `ldelema` of a reference type with no `readonly.` prefix,
        // and a `stelem` of one, to a call of `CastHelpers.LdelemaRef` or `StelemRef` (importer.cpp),
        // which raises these there.
        | UnaryMetadataTokenIlOp.Stelem
        | UnaryMetadataTokenIlOp.Ldelema -> resolves covariantElementAccess []
        // A static-field access runs the declaring type's `.cctor`, and a `.cctor` that threw
        // surfaces here on this and every later access.
        | UnaryMetadataTokenIlOp.Ldsfld
        | UnaryMetadataTokenIlOp.Ldsflda
        | UnaryMetadataTokenIlOp.Stsfld ->
            resolves [ OpcodeFault.StackOverflow ; OpcodeFault.TypeInitialization ] [ ResolutionFault.FieldAccess ]
        // A `.cctor` is triggered by "first invocation of any static method of that type", "any
        // instance or virtual method of that type if it is a value type", and "any constructor for
        // that type" (ECMA-335 I.8.9.5), so every instruction that invokes can surface one that
        // threw. The `.cctor` is a *different* method from the named callee, so this is not
        // something the call edge carries: an analyser that only followed the named target would
        // miss it entirely, which is why it belongs here.
        //
        // Pushing a frame onto a finite stack is the other thing an invoking instruction does by
        // itself, and it is why these carry `StackOverflow` — the same reason `localloc` does. It
        // matters most where the call edge carries nothing: an unbounded recursion has an empty
        // fixpoint over its callees, so without this entry a consumer would call it provably
        // harmless. `FaultKind.ResourceExhaustion` is what keeps this from swamping a report.
        //
        // `call` dispatches only under `constrained.`, on the type the prefix names, which calls
        // a static virtual method.
        | UnaryMetadataTokenIlOp.Call ->
            resolves
                [ OpcodeFault.StackOverflow ; OpcodeFault.TypeInitialization ]
                [ ResolutionFault.MethodAccess ; ResolutionFault.DispatchOnConstrainedType ]
        // `jmp` alone among the invoking instructions does not stack a frame of its own: it
        // "exit[s] current method and jump[s] to the specified method" (ECMA-335 III.3.37),
        // transferring the current arguments rather than pushing a new activation, and its
        // "Exceptions" clause is "None". It still carries `StackOverflow`, because the `.cctor` it
        // can trigger is itself a frame — which is the rule stated against `OpcodeFault`. The JIT
        // checks no access at it.
        | UnaryMetadataTokenIlOp.Jmp -> resolves [ OpcodeFault.StackOverflow ; OpcodeFault.TypeInitialization ] []
        // `ldftn` faults on nothing itself and invokes nothing, but under `constrained.` it
        // dispatches a static virtual method on the type the prefix names.
        | UnaryMetadataTokenIlOp.Ldftn ->
            resolves [] [ ResolutionFault.MethodAccess ; ResolutionFault.DispatchOnConstrainedType ]
        // These fault on nothing themselves and invoke nothing. `constrained.` only names the type
        // the instruction it prefixes dispatches on, which that instruction's entry carries.
        | UnaryMetadataTokenIlOp.Constrained
        | UnaryMetadataTokenIlOp.Sizeof -> resolves [] []
        // `isinst` faults on nothing itself either, but CoreCLR's JIT compiles it to a call of
        // CoreLib (`impCastClassOrIsInstToTree`, importer.cpp): one of the `IsInstanceOf` methods
        // of `CastHelpers` that `jithelpers.h` binds, whose fallback asks the runtime, which can
        // allocate and call the object's `IDynamicInterfaceCastable.IsInterfaceImplemented`. What
        // those raise reaches it from that callee, so it is not this table's.
        | UnaryMetadataTokenIlOp.Isinst -> resolves [] [ ResolutionFault.TypeAccess ]
        // `ldtoken` faults on nothing itself either, but CoreCLR's JIT compiles it to a call of
        // CoreLib (`CEE_LDTOKEN`, importer.cpp): for a method or a field, the `FromPtr` of
        // `RuntimeMethodInfoStub` or `RuntimeFieldInfoStub`, which makes the stub its handle holds,
        // and for a type whose `RuntimeType` the code cannot embed,
        // `RuntimeTypeHandle.GetRuntimeTypeFromHandle`. What those raise reaches it from that
        // callee, so it is not this table's.
        | UnaryMetadataTokenIlOp.Ldtoken -> resolves [] []

    let ofUnaryConst (op : UnaryConstIlOp) : OpcodeFaults =
        match op with
        | UnaryConstIlOp.Stloc _
        | UnaryConstIlOp.Stloc_s _
        | UnaryConstIlOp.Ldc_I8 _
        | UnaryConstIlOp.Ldc_I4 _
        | UnaryConstIlOp.Ldc_R4 _
        | UnaryConstIlOp.Ldc_R8 _
        | UnaryConstIlOp.Ldc_I4_s _
        | UnaryConstIlOp.Br _
        | UnaryConstIlOp.Br_s _
        | UnaryConstIlOp.Brfalse_s _
        | UnaryConstIlOp.Brtrue_s _
        | UnaryConstIlOp.Brfalse _
        | UnaryConstIlOp.Brtrue _
        | UnaryConstIlOp.Beq_s _
        | UnaryConstIlOp.Blt_s _
        | UnaryConstIlOp.Ble_s _
        | UnaryConstIlOp.Bgt_s _
        | UnaryConstIlOp.Bge_s _
        | UnaryConstIlOp.Beq _
        | UnaryConstIlOp.Blt _
        | UnaryConstIlOp.Ble _
        | UnaryConstIlOp.Bgt _
        | UnaryConstIlOp.Bge _
        | UnaryConstIlOp.Bne_un_s _
        | UnaryConstIlOp.Bge_un_s _
        | UnaryConstIlOp.Bgt_un_s _
        | UnaryConstIlOp.Ble_un_s _
        | UnaryConstIlOp.Blt_un_s _
        | UnaryConstIlOp.Bne_un _
        | UnaryConstIlOp.Bge_un _
        | UnaryConstIlOp.Bgt_un _
        | UnaryConstIlOp.Ble_un _
        | UnaryConstIlOp.Blt_un _
        | UnaryConstIlOp.Ldloc_s _
        | UnaryConstIlOp.Ldloca_s _
        | UnaryConstIlOp.Ldarga _
        | UnaryConstIlOp.Ldarg_s _
        | UnaryConstIlOp.Ldarga_s _
        | UnaryConstIlOp.Leave _
        | UnaryConstIlOp.Leave_s _
        | UnaryConstIlOp.Starg_s _
        | UnaryConstIlOp.Starg _
        | UnaryConstIlOp.Unaligned _
        | UnaryConstIlOp.Ldloc _
        | UnaryConstIlOp.Ldloca _
        | UnaryConstIlOp.Ldarg _ -> none

    let ofUnaryStringToken (op : UnaryStringTokenIlOp) : OpcodeFaults =
        match op with
        // Interning makes this free on every execution *but the first*: the literal has no object
        // until one is made for it, and making it allocates
        // (`UnaryStringTokenIlOp.execute` -> `IlMachineState.allocateManagedString`, mirroring
        // CoreCLR's `AddStringLiteral`). ECMA-335 III.4.15 says "Exceptions: None", which is true
        // of the steady state and not of first materialisation.
        // Its token indexes the method's own module's string heap, so nothing binds; but in a
        // `DynamicMethod` it indexes the method's scope, whose entry may not be a string.
        | UnaryStringTokenIlOp.Ldstr ->
            OpcodeFaults.Raises ([ OpcodeFault.OutOfMemory ], set [ ResolutionFault.ScopeEntry ])

    let ofIlOp (op : IlOp) : OpcodeFaults =
        match op with
        | IlOp.Nullary op -> ofNullary op
        | IlOp.UnaryConst op -> ofUnaryConst op
        | IlOp.UnaryMetadataToken (op, _) -> ofUnaryMetadata op
        | IlOp.UnaryStringToken (op, _) -> ofUnaryStringToken op
        // A multi-target branch on the integer at the top of the stack; the out-of-range case
        // falls through rather than faulting.
        | IlOp.Switch _ -> none

    /// What `op` needs of its operands before it can raise `fault`, one of the faults `ofIlOp`
    /// lists for it: it raises the fault only where every one of these holds. A float operand
    /// meets none of them, being no integer. Empty where the table states nothing, so that any
    /// operands may raise the fault.
    let requiredOperands (op : IlOp) (fault : OpcodeFault) : OperandRequirement list =
        let divisorIs (value : IntegerValue) : OperandRequirement =
            {
                FromTop = 0
                Is = value
            }

        match op, fault with
        | IlOp.Nullary (NullaryIlOp.Div | NullaryIlOp.Rem | NullaryIlOp.Div_un | NullaryIlOp.Rem_un),
          OpcodeFault.DivideByZero -> [ divisorIs IntegerValue.Zero ]
        | IlOp.Nullary (NullaryIlOp.Div | NullaryIlOp.Rem), OpcodeFault.Overflow ->
            [
                divisorIs IntegerValue.MinusOne
                {
                    FromTop = 1
                    Is = IntegerValue.Least
                }
            ]
        // A requirement is a narrowing, so stating none is never wrong.
        | _ -> []

    /// Can an instruction with these faults raise `fault`? `Unmodelled` answers yes to everything,
    /// which is what makes it safe to meet: a consumer that asks this question can never read an
    /// unclassified instruction as harmless.
    let mayRaise (fault : OpcodeFault) (faults : OpcodeFaults) : bool =
        match faults with
        | OpcodeFaults.Unmodelled -> true
        | OpcodeFaults.Raises (own, _) -> List.contains fault own

    /// Can resolving the token of an instruction with these faults raise one of `fault`'s? Like
    /// `mayRaise`, `Unmodelled` answers yes to everything.
    let mayFailResolving (fault : ResolutionFault) (faults : OpcodeFaults) : bool =
        match faults with
        | OpcodeFaults.Unmodelled -> true
        | OpcodeFaults.Raises (_, resolution) -> resolution.Contains fault

    /// Drop the faults of one kind, for a consumer that has decided it does not want to see them.
    /// Resolution faults are kept.
    ///
    /// **Deliberately unsound: a reporting policy, not an analysis.** What comes back is no longer
    /// an over-approximation of what the instruction can do, so a result derived through it may not
    /// be read as a proof that the dropped faults cannot happen. They can — which is why the table
    /// carries them. The interpreter must never use this: its check in `raiseOpcodeFault` is a
    /// soundness check and wants the whole table.
    ///
    /// `Unmodelled` comes back unchanged. An instruction this table declines to classify might
    /// raise a fault of any kind, including one the caller did not ask to drop, so there is nothing
    /// in it that filtering could honestly remove.
    let excludingKind (kind : FaultKind) (faults : OpcodeFaults) : OpcodeFaults =
        match faults with
        | OpcodeFaults.Unmodelled -> OpcodeFaults.Unmodelled
        | OpcodeFaults.Raises (own, resolution) ->
            OpcodeFaults.Raises (own |> List.filter (fun f -> OpcodeFault.kind f <> kind), resolution)
