namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata

/// Why CoreCLR refuses a closed type as the operand of an instruction that consumes a type.
[<RequireQualifiedAccess>]
type TypeOperandRefusal =
    /// The operand is a byref type.
    | Byref
    /// The operand is CoreLib's `System.Void`.
    | Void

    override this.ToString () =
        match this with
        | TypeOperandRefusal.Byref -> "a byref type"
        | TypeOperandRefusal.Void -> "System.Void"

[<RequireQualifiedAccess>]
module TypeOperand =

    /// <summary>
    /// Why CoreCLR refuses a token naming the type <paramref name="identity"/> defines, as
    /// <see cref="refusal"/> describes, or <c>None</c>: <c>System.Void</c> is refused, and is
    /// recognised by identity rather than by name, since only CoreLib's own has
    /// <c>ELEMENT_TYPE_VOID</c>.
    /// </summary>
    /// <remarks>
    /// For a member's token, the type is the parent its reference names, which need not declare the
    /// member: a method reference through <c>System.Void</c> finds the virtual methods it inherits
    /// from <c>ValueType</c> and <c>Object</c>, and is then refused (measured, for <c>call</c>,
    /// <c>callvirt</c>, <c>ldftn</c> and <c>ldvirtftn</c>; <c>ldtoken</c> runs).
    /// </remarks>
    let ofDefinition
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (identity : ResolvedTypeIdentity)
        : TypeOperandRefusal option
        =
        if identity = baseClassTypes.Void.Identity then
            Some TypeOperandRefusal.Void
        else
            None

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
    /// (measured). A nominal type is refused as <see cref="ofDefinition"/> says.
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
        | ConcreteTypeHandle.Byref _ -> Some TypeOperandRefusal.Byref
        | ConcreteTypeHandle.Concrete _ ->
            match AllConcreteTypes.lookup handle concreteTypes with
            | Some ty -> ofDefinition baseClassTypes ty.Identity
            | None -> failwith $"BUG: type operand %O{handle} has no row in AllConcreteTypes"
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _
        | ConcreteTypeHandle.FunctionPointer _ -> None

    /// <summary>
    /// Why CoreCLR refuses a token whose type <paramref name="spelling"/> spells, in
    /// <paramref name="spellingAssembly"/>, as <see cref="refusal"/> describes, or <c>None</c>: a
    /// byref, <c>ELEMENT_TYPE_VOID</c>, or a type <see cref="ofDefinition"/> refuses, however it is
    /// named (a TypeSpec of <c>valuetype System.Void</c> included). Custom modifiers do not change
    /// the type a signature names, nor does <c>pinned</c>, which CoreCLR strips from a TypeSpec too
    /// (measured: <c>castclass</c> of <c>pinned int32&amp;</c> is refused).
    /// </summary>
    /// <remarks>
    /// <para>
    /// <c>resolveToken</c> refuses the type only once it has loaded it. A type reference the
    /// spelling names at its top level, under the full name <c>System.Void</c>, is resolved against
    /// <paramref name="assemblies"/>, and is the <c>Error</c> where it does not resolve there: only
    /// loading what it names can tell whether it is CoreLib's. A reference under any other name is
    /// not, since a reference names a type of its own name through any forwarders.
    /// </para>
    /// <para>
    /// A type variable is never refused: no instantiation makes one a byref or <c>System.Void</c>,
    /// neither being a valid type argument (ECMA-335 II.9.4).
    /// </para>
    /// </remarks>
    let rec ofSpelling
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assemblies : LoadedAssemblies)
        (spellingAssembly : DumpedAssembly)
        (spelling : TypeDefn)
        : Result<TypeOperandRefusal option, TypeRef>
        =
        match spelling with
        | TypeDefn.Byref _ -> Ok (Some TypeOperandRefusal.Byref)
        | TypeDefn.Void -> Ok (Some TypeOperandRefusal.Void)
        | TypeDefn.Modified modified -> ofSpelling baseClassTypes assemblies spellingAssembly modified.Unmodified
        | TypeDefn.Pinned element -> ofSpelling baseClassTypes assemblies spellingAssembly element
        | TypeDefn.FromDefinition (identity, _) -> Ok (ofDefinition baseClassTypes identity)
        | TypeDefn.FromReference (typeRef, _) when typeRef.Namespace <> "System" || typeRef.Name <> "Void" -> Ok None
        | TypeDefn.FromReference (typeRef, _) ->
            match LoadedTypeResolution.resolveTypeRef assemblies spellingAssembly ImmutableArray.Empty typeRef with
            | TypeResolutionResult.Resolved (definedIn, _, resolved) ->
                Ok (ofDefinition baseClassTypes definedIn.TypeDefs.[resolved.TypeDefHandle].Identity)
            | _ -> Error typeRef
        | TypeDefn.PrimitiveType _
        | TypeDefn.GenericInstantiation _
        | TypeDefn.Array _
        | TypeDefn.OneDimensionalArrayLowerBoundZero _
        | TypeDefn.Pointer _
        | TypeDefn.FunctionPointer _
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _ -> Ok None

    /// <summary>
    /// Why CoreCLR refuses a token whose type is the one <paramref name="token"/>, a TypeDef,
    /// TypeRef or TypeSpec of <paramref name="assembly"/>, names, as <see cref="ofSpelling"/> says,
    /// with its <c>Error</c>; <c>None</c> for a token naming no type. For a member's token, that type
    /// is the parent its reference names, and <c>resolveToken</c> refuses it only once it has found
    /// the member.
    /// </summary>
    let ofTypeToken
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assemblies : LoadedAssemblies)
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        : Result<TypeOperandRefusal option, TypeRef>
        =
        match token with
        | MetadataToken.TypeDefinition handle -> Ok (ofDefinition baseClassTypes assembly.TypeDefs.[handle].Identity)
        | MetadataToken.TypeReference handle ->
            ofSpelling
                baseClassTypes
                assemblies
                assembly
                (TypeDefn.FromReference (assembly.TypeRefs.[handle], SignatureTypeKind.Unknown))
        | MetadataToken.TypeSpecification handle ->
            ofSpelling baseClassTypes assemblies assembly assembly.TypeSpecs.[handle].Signature
        | _ -> Ok None
