namespace WoofWare.PawPrint

open System.Reflection.Metadata
open Microsoft.Extensions.Logging

/// <summary>
/// The operand of a <see cref="UnaryMetadataTokenIlOp"/>, as the op executing it sees it.
/// </summary>
/// <remarks>
/// <para>
/// This is <see cref="MetadataOperand"/> after the one step that has to happen before dispatch: a
/// `DynamicScope` operand has been read off the guest heap and narrowed. The decoder guarantees
/// that a `ScopeType` reaches only those ops it listed as scope-resolvable
/// (<c>IlDecoding.scopeOperandKind</c>), which is what makes it sound to resolve a scope operand as
/// a type without knowing which op is about to run.
/// </para>
/// <para>
/// Two cases rather than one resolved answer because the metadata path is *not* pre-resolved: the
/// ops disagree today about which generic context and which assembly they feed the metadata
/// pipeline, so normalising them would be a silent behaviour change. Each op keeps its own metadata
/// resolution and gains one arm for this.
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
        | ResolvedMetadataOperand.ScopeType _ ->
            failwith
                $"TODO: %O{this.Op} read its operand as a metadata token, but this operand names a DynamicScope entry. IlDecoding.scopeOperandKind lists this opcode as scope-resolvable, so the op needs a ResolvedMetadataOperand.ScopeType arm; the two have gone out of step."

    /// The metadata token this operand is. Partial in exactly the way <see cref="ActiveAssembly"/>
    /// is, and for the same reason.
    member this.MetadataToken : MetadataToken =
        match this.Operand with
        | ResolvedMetadataOperand.FromMetadata (_, token) -> token
        | ResolvedMetadataOperand.ScopeType _ ->
            failwith
                $"TODO: %O{this.Op} read its operand as a metadata token, but this operand names a DynamicScope entry. IlDecoding.scopeOperandKind lists this opcode as scope-resolvable, so the op needs a ResolvedMetadataOperand.ScopeType arm; the two have gone out of step."
