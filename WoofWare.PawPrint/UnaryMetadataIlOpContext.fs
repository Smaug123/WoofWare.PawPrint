namespace WoofWare.PawPrint

open System.Reflection.Metadata
open Microsoft.Extensions.Logging

/// <summary>
/// The operand of a <see cref="UnaryMetadataTokenIlOp"/>, as the op executing it sees it.
/// </summary>
/// <remarks>
/// <para>
/// This is <see cref="MetadataOperand"/> after the one step that has to happen before dispatch: a
/// `DynamicScope` operand has been read off the guest heap and narrowed. Which of the scope cases to
/// read it as is decided by <c>IlDecoding.scopeOperandKind</c>, the same function the decoder
/// consulted when it accepted the body — so the narrowing is sound without knowing which op is about
/// to run, and the two sites cannot drift because there is only one of them.
/// </para>
/// <para>
/// Separate cases rather than one resolved answer because the metadata path is *not* pre-resolved:
/// the ops disagree today about which generic context and which assembly they feed the metadata
/// pipeline, so normalising them would be a silent behaviour change. Each op keeps its own metadata
/// resolution and gains one arm for each scope case it accepts.
/// </para>
/// </remarks>
[<RequireQualifiedAccess>]
[<NoEquality ; NoComparison>]
type internal ResolvedMetadataOperand =
    /// A token into the metadata tables of the assembly that owns it.
    | FromMetadata of assembly : DumpedAssembly * token : MetadataToken
    /// A closed type read from entry <c>scopeIndex</c> of the executing dynamic method's
    /// `DynamicScope`. Non-closed targets never get here: they are an invalid program, and
    /// <c>UnaryMetadataIlOp.execute</c> raises before dispatching.
    | ScopeType of ConcreteTypeHandle
    /// A dynamic method read from entry <c>scopeIndex</c> of the executing dynamic method's
    /// `DynamicScope`.
    ///
    /// The handle, and not a concretised <see cref="MethodInfo"/>: turning one into the other threads
    /// interpreter state and latches the callee's <c>initLocals</c> as a side effect, neither of
    /// which belongs in a step whose whole job is to read the operand. The op does that, alongside
    /// everything else it does to state.
    | ScopeMethod of DynamicMethodHandle
    /// A field read from entry <c>scopeIndex</c> of the executing dynamic method's `DynamicScope`,
    /// as the identity the field-handle registry holds rather than as a projected
    /// <see cref="FieldInfo"/>. Declaring types that are not closed never get here: they are an
    /// invalid program, and <c>UnaryMetadataIlOp.execute</c> raises before dispatching.
    ///
    /// The identity rather than the projection: a <see cref="FieldHandle"/> is what the field-handle
    /// registry recorded, and a <see cref="FieldInfo"/> is one view of it, which
    /// <c>UnaryMetadataFieldOps.resolveFieldToken</c> builds by the same table read the metadata
    /// path performs.
    | ScopeField of FieldHandle

/// <summary>
/// <see cref="ResolvedMetadataOperand"/> as an op whose operand names a *type* sees it.
/// </summary>
/// <remarks>
/// A separate two-case type rather than a third arm at each of the eleven such ops, so that the one
/// question "is this operand even a type?" is asked in one place (<c>UnaryMetadataIlOpContext.TypeOperand</c>)
/// and every op keeps a total match. Adding a further type-flavoured case therefore breaks all
/// eleven, which is the point; adding a method- or field-flavoured one breaks none of them, which is
/// also the point.
/// </remarks>
[<RequireQualifiedAccess>]
[<NoEquality ; NoComparison>]
type internal ResolvedTypeOperand =
    /// A token into the metadata tables of the assembly that owns it.
    | FromMetadata of assembly : DumpedAssembly * token : MetadataToken
    /// A closed type read from the executing dynamic method's `DynamicScope`.
    | FromScope of ConcreteTypeHandle

/// <summary>
/// <see cref="ResolvedMetadataOperand"/> as an op whose operand names a *field* sees it.
/// </summary>
/// <remarks>
/// A separate two-case type for the reason <see cref="ResolvedTypeOperand"/> is one: the six field
/// ops share <c>UnaryMetadataFieldOps.resolveFieldToken</c>, so the question "is this operand even a
/// field?" is asked once there, and all six keep a total match without knowing that a second token
/// universe exists.
/// </remarks>
[<RequireQualifiedAccess>]
[<NoEquality ; NoComparison>]
type internal ResolvedFieldOperand =
    /// A token into the metadata tables of the assembly that owns it.
    | FromMetadata of assembly : DumpedAssembly * token : MetadataToken
    /// A field read from the executing dynamic method's `DynamicScope`.
    | FromScope of FieldHandle

type internal UnaryMetadataIlOpContext =
    {
        LoggerFactory : ILoggerFactory
        BaseClassTypes : BaseClassTypes<DumpedAssembly>
        Op : UnaryMetadataTokenIlOp
        Operand : ResolvedMetadataOperand
        CurrentMethod : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        Thread : ThreadId
        Logger : ILogger
    }

    /// <summary>
    /// The assembly whose metadata tables this operand indexes.
    /// </summary>
    /// <remarks>
    /// Partial, and loudly so. An op that reaches for this while executing a body minted by
    /// <c>Reflection.Emit</c> has no such assembly to reach for — the operand names a
    /// <c>DynamicScope</c> entry, which is not metadata — and the failure names the fix.
    /// </remarks>
    member this.ActiveAssembly : DumpedAssembly =
        match this.Operand with
        | ResolvedMetadataOperand.FromMetadata (assembly, _) -> assembly
        | ResolvedMetadataOperand.ScopeType _
        | ResolvedMetadataOperand.ScopeMethod _
        | ResolvedMetadataOperand.ScopeField _ ->
            failwith
                $"TODO: %O{this.Op} read its operand as a metadata token, but this operand names a DynamicScope entry. IlDecoding.scopeOperandKind lists this opcode as scope-resolvable, so the op needs a matching ResolvedMetadataOperand arm; the two have gone out of step."

    /// <summary>
    /// This operand, for an op whose operand names a type.
    /// </summary>
    /// <remarks>
    /// Partial in the same way <see cref="ActiveAssembly"/> is: an op that asks for this is one
    /// <c>IlDecoding.scopeOperandKind</c> classifies as <c>Type</c>, so a scope operand reaching it
    /// as anything else means those two have gone out of step.
    /// </remarks>
    member this.TypeOperand : ResolvedTypeOperand =
        match this.Operand with
        | ResolvedMetadataOperand.FromMetadata (assembly, token) -> ResolvedTypeOperand.FromMetadata (assembly, token)
        | ResolvedMetadataOperand.ScopeType handle -> ResolvedTypeOperand.FromScope handle
        | ResolvedMetadataOperand.ScopeMethod handle ->
            failwith
                $"BUG: %O{this.Op} reads its operand as a type, but this one names the DynamicScope's %O{handle}. IlDecoding.scopeOperandKind decides which kind an opcode's scope operand is read as, and it does not say Type for this opcode."
        | ResolvedMetadataOperand.ScopeField _ ->
            failwith
                $"BUG: %O{this.Op} reads its operand as a type, but this one names a field in the DynamicScope. IlDecoding.scopeOperandKind decides which kind an opcode's scope operand is read as, and it does not say Type for this opcode."

    /// <summary>
    /// This operand, for an op whose operand names a field.
    /// </summary>
    /// <remarks>
    /// Partial in the same way <see cref="TypeOperand"/> is, and for the same reason.
    /// </remarks>
    member this.FieldOperand : ResolvedFieldOperand =
        match this.Operand with
        | ResolvedMetadataOperand.FromMetadata (assembly, token) -> ResolvedFieldOperand.FromMetadata (assembly, token)
        | ResolvedMetadataOperand.ScopeField handle -> ResolvedFieldOperand.FromScope handle
        | ResolvedMetadataOperand.ScopeType _
        | ResolvedMetadataOperand.ScopeMethod _ ->
            failwith
                $"BUG: %O{this.Op} reads its operand as a field, but this one names a type or a method in the DynamicScope. IlDecoding.scopeOperandKind decides which kind an opcode's scope operand is read as, and it does not say Field for this opcode."

    /// The metadata token this operand is. Partial in exactly the way <see cref="ActiveAssembly"/>
    /// is, and for the same reason.
    member this.MetadataToken : MetadataToken =
        match this.Operand with
        | ResolvedMetadataOperand.FromMetadata (_, token) -> token
        | ResolvedMetadataOperand.ScopeType _
        | ResolvedMetadataOperand.ScopeMethod _
        | ResolvedMetadataOperand.ScopeField _ ->
            failwith
                $"TODO: %O{this.Op} read its operand as a metadata token, but this operand names a DynamicScope entry. IlDecoding.scopeOperandKind lists this opcode as scope-resolvable, so the op needs a matching ResolvedMetadataOperand arm; the two have gone out of step."
