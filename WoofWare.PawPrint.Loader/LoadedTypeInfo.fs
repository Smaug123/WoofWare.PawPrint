namespace WoofWare.PawPrint

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Collections.Immutable
open System.Diagnostics
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core

/// <summary>
/// <c>TypeInfo</c>'s classifications of a type (value type, enum, byref-like, signature kind),
/// answered by walking its base chain through the assemblies already loaded.
/// </summary>
/// <remarks>
/// Every assembly on the base chain must already be loaded; these functions fail rather than load
/// one. <c>Concretization.tryEnsureTypeDefinitionBaseAssembliesLoaded</c> is how a caller arranges that.
/// </remarks>
[<RequireQualifiedAccess>]
module LoadedTypeInfo =
    let private getName (a : DumpedAssembly) : string = a.DefinitionFullName

    let private getTypeDef (a : DumpedAssembly) (h : TypeDefinitionHandle) : TypeInfo<TypeDefn, TypeDefn> =
        a.TypeDefs.[h]
        |> TypeInfo.mapGeneric (fun (par, _) -> TypeDefn.GenericTypeParameter par.SequenceNumber)

    let private getTypeRef
        (loadedAssemblies : LoadedAssemblies)
        (a : DumpedAssembly)
        (h : TypeReferenceHandle)
        : DumpedAssembly * TypeInfo<TypeDefn, TypeDefn>
        =
        match LoadedTypeResolution.resolveTypeRef loadedAssemblies a ImmutableArray.Empty a.TypeRefs.[h] with
        | TypeResolutionResult.Resolved (resultAssy, _, typeInfo) -> resultAssy, typeInfo
        | TypeResolutionResult.FirstLoadAssy _ ->
            failwith "seems pretty unlikely that we could have constructed this object without loading its base type"
        | TypeResolutionResult.NotFound miss ->
            failwithf "Base type reference from %s does not resolve: %O" a.DefinitionFullName miss

    let private getTypeSpec
        (loadedAssemblies : LoadedAssemblies)
        (a : DumpedAssembly)
        (h : TypeSpecificationHandle)
        : DumpedAssembly * TypeDefinitionHandle
        =
        let signature = a.TypeSpecs.[h].Signature

        let rec go (currentAssembly : DumpedAssembly) (ty : TypeDefn) =
            match ty with
            | TypeDefn.GenericInstantiation (generic, _) -> go currentAssembly generic
            // A custom modifier annotates the signature; the type definition being named is the
            // unmodified one. Stepping into `Modifier` would resolve `InAttribute`/`IsVolatile`/etc.
            | TypeDefn.Modified m -> go currentAssembly m.Unmodified
            | TypeDefn.FromDefinition (identity, _) ->
                let resolvedAssembly = loadedAssemblies.ByDefinitionName identity.AssemblyFullName
                let resolvedType = resolvedAssembly.TypeDefs.[identity.TypeDefinition.Get]
                resolvedAssembly, resolvedType.TypeDefHandle
            | TypeDefn.FromReference (typeRef, _) ->
                match
                    LoadedTypeResolution.resolveTypeRef loadedAssemblies currentAssembly ImmutableArray.Empty typeRef
                with
                | TypeResolutionResult.FirstLoadAssy assyRef ->
                    failwithf
                        "Base type traversal unexpectedly needed to load assembly %s while resolving %O from %s"
                        assyRef.FullName
                        signature
                        a.DefinitionFullName
                | TypeResolutionResult.NotFound miss ->
                    failwithf "Base type traversal could not resolve %O from %s: %O" signature a.DefinitionFullName miss
                | TypeResolutionResult.Resolved (resolvedAssembly, _, resolvedType) ->
                    resolvedAssembly, resolvedType.TypeDefHandle
            | unexpected ->
                failwithf
                    "Unexpected TypeSpec base type shape while resolving %O from %s: %O"
                    signature
                    a.DefinitionFullName
                    unexpected

        go a signature

    let private assemblies (loadedAssemblies : LoadedAssemblies) (identity : string) : DumpedAssembly =
        loadedAssemblies.ByDefinitionName identity

    /// ECMA "value type": transitively inherits from System.ValueType (possibly via System.Enum),
    /// but is NOT exactly System.ValueType or System.Enum themselves.
    let isValueType
        (bct : BaseClassTypes<DumpedAssembly>)
        (loadedAssemblies : LoadedAssemblies)
        (ty : TypeInfo<'generic, 'field>)
        : bool
        =
        TypeInfo.isValueType
            bct
            (assemblies loadedAssemblies)
            getName
            getTypeDef
            (getTypeRef loadedAssemblies)
            (getTypeSpec loadedAssemblies)
            ty

    /// CoreCLR's `MethodTable::IsEnum`: derives from System.Enum, and is not System.Enum itself.
    /// See <see cref="TypeInfo.isEnum"/> for why this decides the MethodTable category.
    let isEnum
        (bct : BaseClassTypes<DumpedAssembly>)
        (loadedAssemblies : LoadedAssemblies)
        (ty : TypeInfo<'generic, 'field>)
        : bool
        =
        TypeInfo.isEnum
            bct
            (assemblies loadedAssemblies)
            getName
            getTypeDef
            (getTypeRef loadedAssemblies)
            (getTypeSpec loadedAssemblies)
            ty

    /// Convenience: not a value type.
    let isReferenceType
        (bct : BaseClassTypes<DumpedAssembly>)
        (loadedAssemblies : LoadedAssemblies)
        (ty : TypeInfo<'generic, 'field>)
        : bool
        =
        TypeInfo.isReferenceType
            bct
            (assemblies loadedAssemblies)
            getName
            getTypeDef
            (getTypeRef loadedAssemblies)
            (getTypeSpec loadedAssemblies)
            ty

    /// ECMA "byref-like": a value type that may not appear on the heap (a C# <c>ref struct</c>).
    ///
    /// CoreCLR derives this from the <c>IsByRefLikeAttribute</c> application, but only for types it
    /// has already classified as value classes: the attribute read sits inside the
    /// <c>fIsValueClass = true</c> branch of <c>MethodTableBuilder::BuildMethodTableThrowing</c>
    /// (methodtablebuilder.cpp:1449). So a *class* carrying the attribute — which C# cannot emit but
    /// IL can, since <c>AttributeUsage</c> binds only the compiler — is not byref-like, and this
    /// gate is what makes <see cref="TypeInfo.HasIsByRefLikeAttribute"/> into the classification.
    ///
    /// Note this is a question about a *nominal* type. CoreCLR's <c>TypeHandle::IsByRefLike</c>
    /// (typehandle.cpp:1061) answers <c>false</c> for every TypeDesc, so byrefs, pointers, function
    /// pointers and arrays are never byref-like however their element type is declared.
    let isByRefLike
        (bct : BaseClassTypes<DumpedAssembly>)
        (loadedAssemblies : LoadedAssemblies)
        (ty : TypeInfo<'generic, 'field>)
        : bool
        =
        ty.HasIsByRefLikeAttribute && isValueType bct loadedAssemblies ty

    /// Metadata layout kind: ValueType for value types, Class otherwise. Note that System.Enum and
    /// System.ValueType themselves encode as Class, matching real CLR signature encoding.
    let signatureTypeKind
        (bct : BaseClassTypes<DumpedAssembly>)
        (loadedAssemblies : LoadedAssemblies)
        (ty : TypeInfo<'generic, 'field>)
        : SignatureTypeKind
        =
        TypeInfo.signatureTypeKind
            bct
            (assemblies loadedAssemblies)
            getName
            getTypeDef
            (getTypeRef loadedAssemblies)
            (getTypeSpec loadedAssemblies)
            ty

    let typeInfoToTypeDefn
        (bct : BaseClassTypes<DumpedAssembly>)
        (loadedAssemblies : LoadedAssemblies)
        (ti : TypeInfo<TypeDefn, TypeDefn>)
        : TypeDefn
        =
        TypeInfo.toTypeDefn
            bct
            (assemblies loadedAssemblies)
            getName
            getTypeDef
            (getTypeRef loadedAssemblies)
            (getTypeSpec loadedAssemblies)
            ti

    let typeInfoToTypeDefn'
        (bct : BaseClassTypes<DumpedAssembly>)
        (assemblies : LoadedAssemblies)
        (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        =
        ti
        |> TypeInfo.mapGeneric (fun (par, _) -> TypeDefn.GenericTypeParameter par.SequenceNumber)
        |> typeInfoToTypeDefn bct assemblies
