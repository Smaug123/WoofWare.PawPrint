namespace WoofWare.PawPrint

/// Why CoreCLR refuses a closed type as the operand of an instruction that consumes a type.
[<RequireQualifiedAccess>]
type TypeOperandRefusal =
    /// The operand is this byref type.
    | Byref of ConcreteTypeHandle
    /// The operand is CoreLib's `System.Void`.
    | Void

    override this.ToString () =
        match this with
        | TypeOperandRefusal.Byref handle -> $"the byref type %O{handle}"
        | TypeOperandRefusal.Void -> "System.Void"

[<RequireQualifiedAccess>]
module TypeOperand =

    /// <summary>
    /// Why CoreCLR refuses <paramref name="handle"/> as the type operand of an instruction that
    /// consumes it, or <c>None</c> where it accepts it there. Its refusal is an
    /// <c>InvalidProgramException</c> with that type's default message.
    /// </summary>
    /// <remarks>
    /// <para>
    /// This is the tail of <c>CEEInfo::resolveToken</c> (jitinterface.cpp), which refuses a type whose
    /// element type is <c>ELEMENT_TYPE_BYREF</c> or <c>ELEMENT_TYPE_VOID</c> for every kind of token
    /// but <c>ldtoken</c>'s, whichever universe the token came from: metadata, or the
    /// <c>DynamicScope</c> of a <c>DynamicMethod</c>. The JIT resolves every type-consuming
    /// instruction's token with one of those kinds, and a <c>catch</c> clause's too. Measured, on a
    /// metadata operand of each of <c>isinst</c>, <c>castclass</c>, <c>box</c>, <c>unbox</c>,
    /// <c>unbox.any</c>, <c>newarr</c>, <c>sizeof</c>, <c>initobj</c>, <c>ldobj</c>, <c>stobj</c>,
    /// <c>cpobj</c>, <c>ldelema</c> (with and without <c>readonly.</c>), <c>ldelem</c>, <c>stelem</c>,
    /// <c>mkrefany</c>, <c>refanyval</c> and <c>constrained.</c>, against an <c>int</c> control that
    /// runs, while <c>ldtoken</c> runs over both.
    /// </para>
    /// <para>
    /// A pointer, an array or a function pointer is accepted here: <c>sizeof int*</c> answers 8
    /// (measured). Only CoreLib's own <c>System.Void</c> has <c>ELEMENT_TYPE_VOID</c>, so a type of
    /// that name in another assembly is accepted as well.
    /// </para>
    /// <para>
    /// This answers only what <c>resolveToken</c> refuses of every type operand. What an opcode
    /// refuses of its own operand is that opcode's question: <c>sizeof Span&lt;int&gt;</c> answers
    /// 16, where <c>newarr Span&lt;int&gt;</c> is a <c>TypeLoadException</c> (both measured).
    /// </para>
    /// </remarks>
    let refusal
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (concreteTypes : AllConcreteTypes)
        (handle : ConcreteTypeHandle)
        : TypeOperandRefusal option
        =
        match handle with
        | ConcreteTypeHandle.Byref _ -> Some (TypeOperandRefusal.Byref handle)
        | ConcreteTypeHandle.Concrete _ ->
            // By identity rather than by name, for the reason given above.
            match AllConcreteTypes.lookup handle concreteTypes with
            | Some ty when ty.Identity = baseClassTypes.Void.Identity -> Some TypeOperandRefusal.Void
            | Some _ -> None
            | None -> failwith $"BUG: type operand %O{handle} has no row in AllConcreteTypes"
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _
        | ConcreteTypeHandle.FunctionPointer _ -> None
