namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

/// Functions for resolving type metadata (TypeRefs, TypeDefs, TypeSpecs) to concrete TypeInfo values.
/// Operates on the loaded-assemblies dictionary directly, without requiring IlMachineState.
[<RequireQualifiedAccess>]
module TypeResolution =

    type private Dummy = class end

    /// <summary>
    /// Bind an AssemblyReference to an assembly, loading it from the runtime dirs if this is the
    /// first time we have needed it. Returns the updated load context, the assembly, and the
    /// assembly's own *definition* identity — which is in general NOT the reference's identity,
    /// so callers must not assume the returned name is what they asked for.
    /// </summary>
    /// <remarks>
    /// Binding is by simple name: we look for <c>&lt;SimpleName&gt;.dll</c> in each runtime dir in
    /// turn. Version, culture and public key token in the reference are therefore not honoured,
    /// which is exactly why the reference's identity so often disagrees with the definition
    /// identity of what it binds to (the .NET Framework compatibility facades reference
    /// implementation assemblies as <c>Version=0.0.0.0</c>).
    /// </remarks>
    let internal loadAssembly
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (referencedInAssembly : DumpedAssembly)
        (r : AssemblyReferenceHandle)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * DumpedAssembly * AssemblyName
        =
        let assemblyRef = referencedInAssembly.AssemblyReferences.[r]

        match assemblies.TryResolveReference assemblyRef with
        | Some v -> assemblies, v, v.Name
        | None ->
            let assemblyName = assemblyRef.Name
            let logger = loggerFactory.CreateLogger typeof<Dummy>.DeclaringType

            // `tryPick`, not `choose |> tryHead`: the first hit is the binding, so every
            // later dir must go unread. Reading them anyway is not merely wasted parsing
            // (though it is that too, and a runtime dir list often holds a whole second
            // framework) — it lets a directory we were never going to bind against fail
            // the load, because anything but FileNotFoundException escapes.
            let assy =
                dotnetRuntimeDirs
                |> Seq.tryPick (fun dir ->
                    let file = Path.Combine (dir, assemblyName.Name + ".dll")

                    try
                        logger.LogInformation ("Loading assembly from file {AssemblyFileLoadPath}", file)
                        Assembly.readFile loggerFactory file |> Some
                    with :? FileNotFoundException ->
                        None
                )

            match assy with
            | None -> failwith $"Could not find a readable DLL in any runtime dir with name %s{assemblyName.Name}.dll"
            | Some assy ->
                // Record both the assembly (under its own definition identity) and the binding
                // that got us here, so the next probe with this reference identity is a hit.
                let assemblies, canonical = assemblies.WithBoundReference assemblyRef assy
                assemblies, canonical, canonical.Name

    /// <summary>
    /// The interpreter's assembly loader: binds AssemblyReferences by simple name against
    /// <paramref name="dotnetRuntimeDirs"/>, in order.
    /// </summary>
    /// <remarks>
    /// This is the one loader. Tests must use it too rather than hand-rolling an
    /// <c>IAssemblyLoad</c>: the bug this exists to prevent was invisible to the test suite for
    /// precisely as long as the test fakes keyed their dictionaries differently from production.
    /// </remarks>
    let directoryLoader (loggerFactory : ILoggerFactory) (dotnetRuntimeDirs : string seq) : IAssemblyLoad =
        { new IAssemblyLoad with
            member _.LoadAssembly loaded referencedIn ref =
                let assemblies, targetAssy, _name =
                    loadAssembly loggerFactory dotnetRuntimeDirs loaded.[referencedIn] ref loaded

                assemblies, targetAssy
        }

    let rec internal resolveTopLevelTypeFromName
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (ns : string option)
        (name : string)
        (genericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        match Assembly.resolveTopLevelTypeFromName assy assemblies ns name genericArgs with
        | TypeResolutionResult.Resolved (assy, _, typeDef) -> assemblies, assy, typeDef
        | TypeResolutionResult.FirstLoadAssy loadFirst ->
            let assemblies, _, _ =
                loadAssembly
                    loggerFactory
                    dotnetRuntimeDirs
                    assemblies.[snd loadFirst.Handle]
                    (fst loadFirst.Handle)
                    assemblies

            let assemblies =
                LoadedAssemblies.assertReferenceBound $"top-level type %s{name}" loadFirst assemblies

            resolveTopLevelTypeFromName loggerFactory dotnetRuntimeDirs ns name genericArgs assy assemblies

    let rec internal resolveTypeFromExport
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (fromAssembly : DumpedAssembly)
        (ty : WoofWare.PawPrint.ExportedType)
        (genericArgs : ImmutableArray<TypeDefn>)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        match Assembly.resolveTypeFromExport fromAssembly assemblies genericArgs ty with
        | TypeResolutionResult.Resolved (assy, _, typeDef) -> assemblies, assy, typeDef
        | TypeResolutionResult.FirstLoadAssy loadFirst ->
            let assemblies, _, _ =
                loadAssembly
                    loggerFactory
                    dotnetRuntimeDirs
                    assemblies.[snd loadFirst.Handle]
                    (fst loadFirst.Handle)
                    assemblies

            let assemblies =
                LoadedAssemblies.assertReferenceBound $"exported type %s{ty.Name}" loadFirst assemblies

            resolveTypeFromExport loggerFactory dotnetRuntimeDirs fromAssembly ty genericArgs assemblies

    let rec internal resolveTypeFromRef
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (referencedInAssembly : DumpedAssembly)
        (target : TypeRef)
        (typeGenericArgs : ImmutableArray<TypeDefn>)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        match Assembly.resolveTypeRef assemblies referencedInAssembly typeGenericArgs target with
        | TypeResolutionResult.Resolved (assy, _, typeDef) -> assemblies, assy, typeDef
        | TypeResolutionResult.FirstLoadAssy loadFirst ->
            let assemblies, _, _ =
                loadAssembly
                    loggerFactory
                    dotnetRuntimeDirs
                    assemblies.[snd loadFirst.Handle]
                    (fst loadFirst.Handle)
                    assemblies

            let assemblies =
                LoadedAssemblies.assertReferenceBound $"type reference %s{target.Name}" loadFirst assemblies

            resolveTypeFromRef loggerFactory dotnetRuntimeDirs referencedInAssembly target typeGenericArgs assemblies

    let internal resolveType
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (ty : TypeReferenceHandle)
        (genericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        let target = assy.TypeRefs.[ty]
        resolveTypeFromRef loggerFactory dotnetRuntimeDirs assy target genericArgs assemblies

    /// Substitute generic parameters in a TypeDefn while preserving the structure of
    /// constructed types (arrays, pointers, byrefs). For "leaf" types (FromReference,
    /// FromDefinition, PrimitiveType), falls through to resolveTypeFromDefn and converts
    /// back via typeInfoToTypeDefn, which is lossless for those cases. For constructed
    /// types, recurses structurally so that e.g. OneDimensionalArrayLowerBoundZero is
    /// preserved rather than being collapsed to System.Array.
    let rec private substituteGenericsInTypeDefn
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (ty : TypeDefn)
        (typeGenericArgs : ImmutableArray<TypeDefn>)
        (methodGenericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * TypeDefn
        =
        match ty with
        | TypeDefn.GenericTypeParameter idx ->
            substituteGenericsInTypeDefn
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                typeGenericArgs.[idx]
                typeGenericArgs
                methodGenericArgs
                assy
                assemblies
        | TypeDefn.GenericMethodParameter idx ->
            substituteGenericsInTypeDefn
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                methodGenericArgs.[idx]
                typeGenericArgs
                methodGenericArgs
                assy
                assemblies
        | TypeDefn.OneDimensionalArrayLowerBoundZero elementType ->
            let assemblies, resolved =
                substituteGenericsInTypeDefn
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    elementType
                    typeGenericArgs
                    methodGenericArgs
                    assy
                    assemblies

            assemblies, TypeDefn.OneDimensionalArrayLowerBoundZero resolved
        | TypeDefn.Array (elementType, rank) ->
            let assemblies, resolved =
                substituteGenericsInTypeDefn
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    elementType
                    typeGenericArgs
                    methodGenericArgs
                    assy
                    assemblies

            assemblies, TypeDefn.Array (resolved, rank)
        | TypeDefn.Pointer elementType ->
            let assemblies, resolved =
                substituteGenericsInTypeDefn
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    elementType
                    typeGenericArgs
                    methodGenericArgs
                    assy
                    assemblies

            assemblies, TypeDefn.Pointer resolved
        | TypeDefn.Byref elementType ->
            let assemblies, resolved =
                substituteGenericsInTypeDefn
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    elementType
                    typeGenericArgs
                    methodGenericArgs
                    assy
                    assemblies

            assemblies, TypeDefn.Byref resolved
        | TypeDefn.GenericInstantiation (generic, args) ->
            // Substitute generics in the args, then delegate the whole GenericInstantiation
            // to resolveTypeFromDefn + typeInfoToTypeDefn. This ensures proper assembly
            // resolution for the generic def while preserving constructed types in the args.
            // The re-entry into resolveTypeFromDefn's GenericInstantiation case will call
            // substituteGenericsInTypeDefn on the already-substituted args, which will
            // go through the leaf cases (no-op for concrete types).
            let builder = ImmutableArray.CreateBuilder args.Length

            let assemblies =
                (assemblies, args)
                ||> Seq.fold (fun assemblies arg ->
                    let assemblies, resolved =
                        substituteGenericsInTypeDefn
                            loggerFactory
                            dotnetRuntimeDirs
                            baseClassTypes
                            arg
                            typeGenericArgs
                            methodGenericArgs
                            assy
                            assemblies

                    builder.Add resolved
                    assemblies
                )

            let substituted = TypeDefn.GenericInstantiation (generic, builder.ToImmutable ())

            let assemblies, _assy, resolvedInfo =
                resolveTypeFromDefn
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    substituted
                    typeGenericArgs
                    methodGenericArgs
                    assy
                    assemblies

            let preserved =
                DumpedAssembly.typeInfoToTypeDefn baseClassTypes assemblies resolvedInfo

            assemblies, preserved
        | TypeDefn.FromReference _
        | TypeDefn.FromDefinition _
        | TypeDefn.PrimitiveType _
        | TypeDefn.Void ->
            // Leaf types: resolve for side effects (assembly loading) and convert back.
            // The round-trip through TypeInfo is lossless for these cases.
            let assemblies, _assy, resolvedInfo =
                resolveTypeFromDefn
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    ty
                    typeGenericArgs
                    methodGenericArgs
                    assy
                    assemblies

            let preserved =
                DumpedAssembly.typeInfoToTypeDefn baseClassTypes assemblies resolvedInfo

            assemblies, preserved
        | other ->
            // For any other TypeDefn variant, resolve and convert back.
            let assemblies, _assy, resolvedInfo =
                resolveTypeFromDefn
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    other
                    typeGenericArgs
                    methodGenericArgs
                    assy
                    assemblies

            let preserved =
                DumpedAssembly.typeInfoToTypeDefn baseClassTypes assemblies resolvedInfo

            assemblies, preserved

    and resolveTypeFromDefn
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (ty : TypeDefn)
        (typeGenericArgs : ImmutableArray<TypeDefn>)
        (methodGenericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        match ty with
        | TypeDefn.GenericInstantiation (generic, args) ->
            let builder = ImmutableArray.CreateBuilder args.Length

            let assemblies =
                (assemblies, args)
                ||> Seq.fold (fun assemblies arg ->
                    let assemblies, preservedArg =
                        substituteGenericsInTypeDefn
                            loggerFactory
                            dotnetRuntimeDirs
                            baseClassTypes
                            arg
                            typeGenericArgs
                            methodGenericArgs
                            assy
                            assemblies

                    builder.Add preservedArg

                    assemblies
                )

            let args' = builder.ToImmutable ()

            resolveTypeFromDefn
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                generic
                args'
                methodGenericArgs
                assy
                assemblies
        | TypeDefn.FromDefinition (identity, _typeKind) ->
            let assy = assemblies.ByDefinitionName identity.AssemblyFullName

            let defn =
                assy.TypeDefs.[identity.TypeDefinition.Get]
                |> TypeInfo.mapGeneric (fun (param, _) -> typeGenericArgs.[param.SequenceNumber])

            assemblies, assy, defn
        | TypeDefn.FromReference (ref, _typeKind) ->
            let assemblies, assy, ty =
                resolveTypeFromRef loggerFactory dotnetRuntimeDirs assy ref typeGenericArgs assemblies

            assemblies, assy, ty
        | TypeDefn.PrimitiveType prim ->
            let ty =
                match prim with
                | PrimitiveType.Boolean -> baseClassTypes.Boolean
                | PrimitiveType.Char -> baseClassTypes.Char
                | PrimitiveType.SByte -> baseClassTypes.SByte
                | PrimitiveType.Byte -> baseClassTypes.Byte
                | PrimitiveType.Int16 -> baseClassTypes.Int16
                | PrimitiveType.UInt16 -> baseClassTypes.UInt16
                | PrimitiveType.Int32 -> baseClassTypes.Int32
                | PrimitiveType.UInt32 -> baseClassTypes.UInt32
                | PrimitiveType.Int64 -> baseClassTypes.Int64
                | PrimitiveType.UInt64 -> baseClassTypes.UInt64
                | PrimitiveType.Single -> baseClassTypes.Single
                | PrimitiveType.Double -> baseClassTypes.Double
                | PrimitiveType.String -> baseClassTypes.String
                | PrimitiveType.TypedReference -> failwith "todo"
                | PrimitiveType.IntPtr -> baseClassTypes.IntPtr
                | PrimitiveType.UIntPtr -> baseClassTypes.UIntPtr
                | PrimitiveType.Object -> baseClassTypes.Object
                |> TypeInfo.mapGeneric (fun _ -> failwith "none of these types are generic")

            assemblies, baseClassTypes.Corelib, ty
        | TypeDefn.GenericTypeParameter param ->
            let arg = typeGenericArgs.[param]
            // TODO: this assembly is probably wrong?
            resolveTypeFromDefn
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                arg
                typeGenericArgs
                methodGenericArgs
                assy
                assemblies
        | TypeDefn.GenericMethodParameter param ->
            let arg = methodGenericArgs.[param]
            // TODO: this assembly is probably wrong?
            resolveTypeFromDefn
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                arg
                typeGenericArgs
                methodGenericArgs
                assy
                assemblies
        | TypeDefn.OneDimensionalArrayLowerBoundZero _
        | TypeDefn.Array _ ->
            // This is lossy: we return System.Array's TypeInfo, discarding the element type.
            // Callers that need precise array type identity (e.g. Ldtoken) should use
            // concretizeType directly instead of going through this function.
            let arrayTy =
                baseClassTypes.Array
                |> TypeInfo.mapGeneric (fun _ -> failwith "System.Array is not generic")

            assemblies, baseClassTypes.Corelib, arrayTy
        | TypeDefn.Pointer _
        | TypeDefn.Byref _
        | TypeDefn.Pinned _ ->
            failwith
                $"TODO: resolveTypeFromDefn cannot faithfully represent pointer/byref/pinned types as TypeInfo. Caller should handle these wrapper types before calling resolveTypeFromDefn. Got: {ty}"
        | s -> failwith $"TODO: resolveTypeFromDefn unimplemented for {s}"

    let resolveTypeFromSpec
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (ty : TypeSpecificationHandle)
        (assy : DumpedAssembly)
        (typeGenericArgs : TypeDefn ImmutableArray)
        (methodGenericArgs : TypeDefn ImmutableArray)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        let sign = assy.TypeSpecs.[ty].Signature

        resolveTypeFromDefn
            loggerFactory
            dotnetRuntimeDirs
            baseClassTypes
            sign
            typeGenericArgs
            methodGenericArgs
            assy
            assemblies
