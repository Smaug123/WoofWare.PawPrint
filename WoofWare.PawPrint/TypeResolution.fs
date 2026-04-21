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

    /// Load an assembly referenced by another assembly. Returns the updated assemblies dictionary,
    /// the loaded assembly, and its name. If the assembly is already loaded, returns the dictionary
    /// unchanged.
    let internal loadAssembly
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (referencedInAssembly : DumpedAssembly)
        (r : AssemblyReferenceHandle)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        : ImmutableDictionary<string, DumpedAssembly> * DumpedAssembly * AssemblyName
        =
        let assemblyRef = referencedInAssembly.AssemblyReferences.[r]
        let assemblyName = assemblyRef.Name

        match assemblies.TryGetValue assemblyName.FullName with
        | true, v -> assemblies, v, assemblyName
        | false, _ ->
            let logger = loggerFactory.CreateLogger typeof<Dummy>.DeclaringType

            let assy =
                dotnetRuntimeDirs
                |> Seq.choose (fun dir ->
                    let file = Path.Combine (dir, assemblyName.Name + ".dll")

                    try
                        use f = File.OpenRead file
                        logger.LogInformation ("Loading assembly from file {AssemblyFileLoadPath}", file)
                        Assembly.read loggerFactory (Some file) f |> Some
                    with :? FileNotFoundException ->
                        None
                )
                |> Seq.toList

            match assy |> List.tryHead with
            | None -> failwith $"Could not find a readable DLL in any runtime dir with name %s{assemblyName.Name}.dll"
            | Some assy ->
                let assemblies = assemblies.SetItem (assemblyName.FullName, assy)
                assemblies, assy, assemblyName

    let rec internal resolveTopLevelTypeFromName
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (ns : string option)
        (name : string)
        (genericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        : ImmutableDictionary<string, DumpedAssembly> * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        match Assembly.resolveTopLevelTypeFromName assy assemblies ns name genericArgs with
        | TypeResolutionResult.Resolved (assy, _, typeDef) -> assemblies, assy, typeDef
        | TypeResolutionResult.FirstLoadAssy loadFirst ->
            let assemblies, _, _ =
                loadAssembly
                    loggerFactory
                    dotnetRuntimeDirs
                    assemblies.[snd(loadFirst.Handle).FullName]
                    (fst loadFirst.Handle)
                    assemblies

            resolveTopLevelTypeFromName loggerFactory dotnetRuntimeDirs ns name genericArgs assy assemblies

    let rec internal resolveTypeFromExport
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (fromAssembly : DumpedAssembly)
        (ty : WoofWare.PawPrint.ExportedType)
        (genericArgs : ImmutableArray<TypeDefn>)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        : ImmutableDictionary<string, DumpedAssembly> * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        match Assembly.resolveTypeFromExport fromAssembly assemblies genericArgs ty with
        | TypeResolutionResult.Resolved (assy, _, typeDef) -> assemblies, assy, typeDef
        | TypeResolutionResult.FirstLoadAssy loadFirst ->
            let assemblies, _, _ =
                loadAssembly
                    loggerFactory
                    dotnetRuntimeDirs
                    assemblies.[snd(loadFirst.Handle).FullName]
                    (fst loadFirst.Handle)
                    assemblies

            resolveTypeFromExport loggerFactory dotnetRuntimeDirs fromAssembly ty genericArgs assemblies

    let rec internal resolveTypeFromRef
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (referencedInAssembly : DumpedAssembly)
        (target : TypeRef)
        (typeGenericArgs : ImmutableArray<TypeDefn>)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        : ImmutableDictionary<string, DumpedAssembly> * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
        =
        match Assembly.resolveTypeRef assemblies referencedInAssembly typeGenericArgs target with
        | TypeResolutionResult.Resolved (assy, _, typeDef) -> assemblies, assy, typeDef
        | TypeResolutionResult.FirstLoadAssy loadFirst ->
            let assemblies, _, _ =
                loadAssembly
                    loggerFactory
                    dotnetRuntimeDirs
                    assemblies.[snd(loadFirst.Handle).FullName]
                    (fst loadFirst.Handle)
                    assemblies

            resolveTypeFromRef loggerFactory dotnetRuntimeDirs referencedInAssembly target typeGenericArgs assemblies

    let internal resolveType
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (ty : TypeReferenceHandle)
        (genericArgs : ImmutableArray<TypeDefn>)
        (assy : DumpedAssembly)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        : ImmutableDictionary<string, DumpedAssembly> * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
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
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        : ImmutableDictionary<string, DumpedAssembly> * TypeDefn
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
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        : ImmutableDictionary<string, DumpedAssembly> * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
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
            let assy = assemblies.[identity.AssemblyFullName]

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
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        : ImmutableDictionary<string, DumpedAssembly> * DumpedAssembly * WoofWare.PawPrint.TypeInfo<TypeDefn, TypeDefn>
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
