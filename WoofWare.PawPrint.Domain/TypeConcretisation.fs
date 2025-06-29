namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata

type ConcreteTypeHandle = | ConcreteTypeHandle of int

type AllConcreteTypes =
    {
        Mapping : Map<ConcreteTypeHandle, ConcreteType<ConcreteTypeHandle>>
        NextHandle : int
    }

    static member Empty =
        {
            Mapping = Map.empty
            NextHandle = 0
        }

[<RequireQualifiedAccess>]
module AllConcreteTypes =
    let lookup (cth : ConcreteTypeHandle) (this : AllConcreteTypes) : ConcreteType<ConcreteTypeHandle> option =
        this.Mapping |> Map.tryFind cth

    let lookup' (ct : ConcreteType<ConcreteTypeHandle>) (this : AllConcreteTypes) : ConcreteTypeHandle option =
        this.Mapping
        |> Map.tryPick (fun handle existingCt ->
            if
                existingCt._AssemblyName = ct._AssemblyName
                && existingCt._Namespace = ct._Namespace
                && existingCt._Name = ct._Name
                && existingCt._Definition = ct._Definition
                && existingCt._Generics = ct._Generics
            then
                Some handle
            else
                None
        )

    let findExistingConcreteType
        (concreteTypes : AllConcreteTypes)
        (asm : AssemblyName, ns : string, name : string, generics : ConcreteTypeHandle list as key)
        : ConcreteTypeHandle option
        =

        concreteTypes.Mapping
        |> Map.tryPick (fun handle ct ->
            if
                ct._AssemblyName = asm
                && ct._Namespace = ns
                && ct._Name = name
                && ct._Generics = generics
            then
                Some handle
            else
                None
        )

    /// `source` is AssemblyName * Namespace * Name
    let add (ct : ConcreteType<ConcreteTypeHandle>) (this : AllConcreteTypes) : ConcreteTypeHandle * AllConcreteTypes =
        let toRet = ConcreteTypeHandle this.NextHandle

        let newState =
            {
                NextHandle = this.NextHandle + 1
                Mapping = this.Mapping |> Map.add toRet ct
            }

        toRet, newState

[<RequireQualifiedAccess>]
module TypeConcretization =

    type ConcretizationContext =
        {
            /// Types currently being processed (to detect cycles)
            InProgress : ImmutableDictionary<AssemblyName * TypeDefn, ConcreteTypeHandle>
            /// All concrete types created so far
            ConcreteTypes : AllConcreteTypes
            /// For resolving type references
            LoadedAssemblies : ImmutableDictionary<string, DumpedAssembly>
            BaseTypes : BaseClassTypes<DumpedAssembly>
        }

    // Helper function to find existing primitive types
    let private findExistingPrimitiveType
        (concreteTypes : AllConcreteTypes)
        (key : AssemblyName * string * string)
        : ConcreteTypeHandle option
        =
        concreteTypes.Mapping
        |> Map.tryPick (fun handle ct ->
            if
                ct._AssemblyName = (let (asm, _, _) = key in asm)
                && ct._Namespace = (let (_, ns, _) = key in ns)
                && ct._Name = (let (_, _, name) = key in name)
                && ct._Generics.IsEmpty
            then
                Some handle
            else
                None
        )

    let private concretizePrimitive
        (ctx : ConcretizationContext)
        (prim : PrimitiveType)
        : ConcreteTypeHandle * ConcretizationContext
        =

        // Get the TypeInfo for this primitive from BaseClassTypes
        let typeInfo =
            match prim with
            | PrimitiveType.Boolean -> ctx.BaseTypes.Boolean
            | PrimitiveType.Char -> ctx.BaseTypes.Char
            | PrimitiveType.SByte -> ctx.BaseTypes.SByte
            | PrimitiveType.Byte -> ctx.BaseTypes.Byte
            | PrimitiveType.Int16 -> ctx.BaseTypes.Int16
            | PrimitiveType.UInt16 -> ctx.BaseTypes.UInt16
            | PrimitiveType.Int32 -> ctx.BaseTypes.Int32
            | PrimitiveType.UInt32 -> ctx.BaseTypes.UInt32
            | PrimitiveType.Int64 -> ctx.BaseTypes.Int64
            | PrimitiveType.UInt64 -> ctx.BaseTypes.UInt64
            | PrimitiveType.Single -> ctx.BaseTypes.Single
            | PrimitiveType.Double -> ctx.BaseTypes.Double
            | PrimitiveType.String -> ctx.BaseTypes.String
            | PrimitiveType.Object -> ctx.BaseTypes.Object
            | PrimitiveType.TypedReference -> failwith "TypedReference not supported in BaseClassTypes"
            | PrimitiveType.IntPtr -> failwith "IntPtr not supported in BaseClassTypes"
            | PrimitiveType.UIntPtr -> failwith "UIntPtr not supported in BaseClassTypes"

        // Check if we've already concretized this primitive type
        let key = (typeInfo.Assembly, typeInfo.Namespace, typeInfo.Name)

        match findExistingPrimitiveType ctx.ConcreteTypes key with
        | Some handle -> handle, ctx
        | None ->
            // Create the concrete type (primitives have no generic arguments)
            let concreteType =
                {
                    _AssemblyName = typeInfo.Assembly
                    _Definition = ComparableTypeDefinitionHandle.Make typeInfo.TypeDefHandle
                    _Namespace = typeInfo.Namespace
                    _Name = typeInfo.Name
                    _Generics = [] // Primitives have no generic parameters
                }

            // Add to the concrete types
            let handle, newConcreteTypes = AllConcreteTypes.add concreteType ctx.ConcreteTypes

            let newCtx =
                { ctx with
                    ConcreteTypes = newConcreteTypes
                }

            handle, newCtx

    let private concretizeArray
        (ctx : ConcretizationContext)
        (elementHandle : ConcreteTypeHandle)
        (shape : 'a)
        : ConcreteTypeHandle * ConcretizationContext
        =

        // Arrays are System.Array<T> where T is the element type
        let arrayTypeInfo = ctx.BaseTypes.Array

        // Check if we've already concretized this array type
        let key =
            (arrayTypeInfo.Assembly, arrayTypeInfo.Namespace, arrayTypeInfo.Name, [ elementHandle ])

        match AllConcreteTypes.findExistingConcreteType ctx.ConcreteTypes key with
        | Some handle -> handle, ctx
        | None ->
            // Create the concrete array type
            let concreteType =
                {
                    _AssemblyName = arrayTypeInfo.Assembly
                    _Definition = ComparableTypeDefinitionHandle.Make arrayTypeInfo.TypeDefHandle
                    _Namespace = arrayTypeInfo.Namespace
                    _Name = arrayTypeInfo.Name
                    _Generics = [ elementHandle ] // Array<T> has one generic parameter
                }

            // Add to the concrete types
            let handle, newConcreteTypes = AllConcreteTypes.add concreteType ctx.ConcreteTypes

            let newCtx =
                { ctx with
                    ConcreteTypes = newConcreteTypes
                }

            handle, newCtx

    let private concretizeOneDimArray
        (ctx : ConcretizationContext)
        (elementHandle : ConcreteTypeHandle)
        : ConcreteTypeHandle * ConcretizationContext
        =

        // One-dimensional arrays with lower bound 0 are also System.Array<T>
        // They just have different IL instructions for access
        let arrayTypeInfo = ctx.BaseTypes.Array

        // Check if we've already concretized this array type
        let key =
            (arrayTypeInfo.Assembly, arrayTypeInfo.Namespace, arrayTypeInfo.Name, [ elementHandle ])

        match AllConcreteTypes.findExistingConcreteType ctx.ConcreteTypes key with
        | Some handle -> handle, ctx
        | None ->
            // Create the concrete array type
            let concreteType =
                {
                    _AssemblyName = arrayTypeInfo.Assembly
                    _Definition = ComparableTypeDefinitionHandle.Make arrayTypeInfo.TypeDefHandle
                    _Namespace = arrayTypeInfo.Namespace
                    _Name = arrayTypeInfo.Name
                    _Generics = [ elementHandle ] // Array<T> has one generic parameter
                }

            // Add to the concrete types
            let handle, newConcreteTypes = AllConcreteTypes.add concreteType ctx.ConcreteTypes

            let newCtx =
                { ctx with
                    ConcreteTypes = newConcreteTypes
                }

            handle, newCtx

    let private concretizeTypeDefinition
        (ctx : ConcretizationContext)
        (assemblyName : AssemblyName)
        (typeDefHandle : ComparableTypeDefinitionHandle)
        : ConcreteTypeHandle * ConcretizationContext
        =

        // Look up the type definition in the assembly
        let assembly = ctx.LoadedAssemblies.[assemblyName.FullName]
        let typeInfo = assembly.TypeDefs.[typeDefHandle.Get]

        // Check if this type has generic parameters
        if not typeInfo.Generics.IsEmpty then
            failwithf
                "Cannot concretize open generic type %s.%s - it has %d generic parameters"
                typeInfo.Namespace
                typeInfo.Name
                typeInfo.Generics.Length

        // Check if we've already concretized this type
        let key = (assemblyName, typeInfo.Namespace, typeInfo.Name, [])

        match AllConcreteTypes.findExistingConcreteType ctx.ConcreteTypes key with
        | Some handle -> handle, ctx
        | None ->
            // Create the concrete type (no generic arguments since it's not generic)
            let concreteType =
                {
                    _AssemblyName = assemblyName
                    _Definition = typeDefHandle
                    _Namespace = typeInfo.Namespace
                    _Name = typeInfo.Name
                    _Generics = [] // No generic parameters
                }

            // Add to the concrete types
            let handle, newConcreteTypes = AllConcreteTypes.add concreteType ctx.ConcreteTypes

            let newCtx =
                { ctx with
                    ConcreteTypes = newConcreteTypes
                }

            handle, newCtx

    let private concretizeTypeReference
        (getAssembly : AssemblyName -> AssemblyReferenceHandle -> DumpedAssembly)
        (ctx : ConcretizationContext)
        (currentAssembly : AssemblyName)
        (typeRef : TypeRef)
        : ConcreteTypeHandle * ConcretizationContext
        =

        // First we need to resolve where this type lives
        match typeRef.ResolutionScope with
        | TypeRefResolutionScope.Assembly assyRef ->
            let assy = getAssembly currentAssembly assyRef
            // Type is in another assembly
            let targetAssyName = assy.Name

            match ctx.LoadedAssemblies.TryGetValue targetAssyName.FullName with
            | false, _ ->
                failwithf
                    "Cannot concretize type reference %s.%s - assembly %s not loaded"
                    typeRef.Namespace
                    typeRef.Name
                    targetAssyName.FullName

            | true, targetAssy ->
                // Find the type in the target assembly
                match targetAssy.TypeDef typeRef.Namespace typeRef.Name with
                | None ->
                    failwithf
                        "Type %s.%s not found in assembly %s"
                        typeRef.Namespace
                        typeRef.Name
                        targetAssyName.FullName

                | Some typeInfo ->
                    // Check if this type has generic parameters
                    if not typeInfo.Generics.IsEmpty then
                        failwithf
                            "Cannot concretize type reference to open generic type %s.%s - it has %d generic parameters"
                            typeInfo.Namespace
                            typeInfo.Name
                            typeInfo.Generics.Length

                    // Create or find the concrete type
                    concretizeTypeDefinition
                        ctx
                        targetAssyName
                        (ComparableTypeDefinitionHandle.Make typeInfo.TypeDefHandle)

        | TypeRefResolutionScope.TypeRef parentTypeRef ->
            // This is a nested type - need to resolve parent first
            // For now, let's fail - nested type handling is complex
            failwith "TODO: Nested type resolution not implemented in concretization"

        | TypeRefResolutionScope.ModuleRef _ ->
            // Type is in the current module
            let currentAssy = ctx.LoadedAssemblies.[currentAssembly.FullName]

            match currentAssy.TypeDef typeRef.Namespace typeRef.Name with
            | None ->
                failwithf
                    "Type %s.%s not found in current assembly %s"
                    typeRef.Namespace
                    typeRef.Name
                    currentAssembly.FullName

            | Some typeInfo ->
                concretizeTypeDefinition
                    ctx
                    currentAssembly
                    (ComparableTypeDefinitionHandle.Make typeInfo.TypeDefHandle)

    /// Concretize a type in a specific generic context
    let rec concretizeType
        (ctx : ConcretizationContext)
        (getAssembly : AssemblyName -> AssemblyReferenceHandle -> DumpedAssembly)
        (assembly : AssemblyName)
        (typeGenerics : ConcreteTypeHandle array)
        (methodGenerics : ConcreteTypeHandle array)
        (typeDefn : TypeDefn)
        : ConcreteTypeHandle * ConcretizationContext
        =

        let key = (assembly, typeDefn)

        // Check if we're already processing this type (cycle detection)
        match ctx.InProgress.TryGetValue key with
        | true, handle -> handle, ctx
        | false, _ ->

        // Check if already concretized
        match AllConcreteTypes.findExistingConcreteType ctx.ConcreteTypes (failwith "TODO") with
        | Some handle -> handle, ctx
        | None ->

        match typeDefn with
        | PrimitiveType prim -> concretizePrimitive ctx prim

        | Array (elementType, shape) ->
            let elementHandle, ctx =
                concretizeType ctx getAssembly assembly typeGenerics methodGenerics elementType

            concretizeArray ctx elementHandle shape

        | GenericTypeParameter index ->
            if index < typeGenerics.Length then
                typeGenerics.[index], ctx
            else
                failwithf "Generic type parameter %d out of range" index

        | GenericMethodParameter index ->
            if index < methodGenerics.Length then
                methodGenerics.[index], ctx
            else
                failwithf "Generic method parameter %d out of range" index

        | GenericInstantiation (genericDef, args) ->
            concretizeGenericInstantiation ctx getAssembly assembly typeGenerics methodGenerics genericDef args

        | FromDefinition (typeDefHandle, targetAssembly, _) ->
            concretizeTypeDefinition ctx (AssemblyName targetAssembly) typeDefHandle

        | FromReference (typeRef, _) -> concretizeTypeReference getAssembly ctx assembly typeRef

        | _ -> failwithf "TODO: Concretization of %A not implemented" typeDefn

    and private concretizeGenericInstantiation
        (ctx : ConcretizationContext)
        (getAssembly : _ -> _ -> _)
        (assembly : AssemblyName)
        (typeGenerics : ConcreteTypeHandle array)
        (methodGenerics : ConcreteTypeHandle array)
        (genericDef : TypeDefn)
        (args : ImmutableArray<TypeDefn>)
        : ConcreteTypeHandle * ConcretizationContext
        =
        // Pre-allocate a handle for this type to handle cycles
        let tempHandle = ConcreteTypeHandle ctx.ConcreteTypes.NextHandle

        let ctx =
            { ctx with
                ConcreteTypes =
                    { ctx.ConcreteTypes with
                        NextHandle = ctx.ConcreteTypes.NextHandle + 1
                    }
                InProgress = ctx.InProgress.SetItem ((assembly, GenericInstantiation (genericDef, args)), tempHandle)
            }

        // Concretize all type arguments
        let argHandles, ctx =
            args
            |> Seq.fold
                (fun (handles, ctx) arg ->
                    let handle, ctx =
                        concretizeType ctx getAssembly assembly typeGenerics methodGenerics arg

                    handle :: handles, ctx
                )
                ([], ctx)

        let argHandles = argHandles |> List.rev

        // Get the base type definition
        let baseAssembly, baseTypeDefHandle =
            match genericDef with
            | FromDefinition (handle, assy, _) -> AssemblyName assy, handle
            | FromReference (typeRef, _) ->
                match
                    Assembly.resolveTypeRef
                        ctx.LoadedAssemblies
                        ctx.LoadedAssemblies.[assembly.FullName]
                        typeRef
                        (failwith "generic args")
                with
                | TypeResolutionResult.Resolved (assy, typeDef) ->
                    assy.Name, ComparableTypeDefinitionHandle.Make typeDef.TypeDefHandle
                | _ -> failwith "TODO: handle unresolved type refs"
            | _ -> failwithf "Generic instantiation of %A not supported" genericDef

        // Create the concrete type
        let concreteType =
            {
                _AssemblyName = baseAssembly
                _Definition = ComparableTypeDefinitionHandle.Make baseTypeDefHandle.Get
                _Namespace = "TODO" // Look up from type def
                _Name = "TODO" // Look up from type def
                _Generics = argHandles
            }

        // Update the pre-allocated entry
        let ctx =
            { ctx with
                ConcreteTypes =
                    { ctx.ConcreteTypes with
                        Mapping = ctx.ConcreteTypes.Mapping |> Map.add tempHandle concreteType
                    }
                InProgress = ctx.InProgress.Remove (assembly, GenericInstantiation (genericDef, args))
            }

        tempHandle, ctx

/// High-level API for concretizing types
[<RequireQualifiedAccess>]
module Concretization =

    /// Helper to concretize an array of types
    let private concretizeTypeArray
        (ctx : TypeConcretization.ConcretizationContext)
        (getAssembly : AssemblyName -> AssemblyReferenceHandle -> DumpedAssembly)
        (assembly : AssemblyName)
        (typeArgs : ConcreteTypeHandle array)
        (methodArgs : ConcreteTypeHandle array)
        (types : ImmutableArray<TypeDefn>)
        : ImmutableArray<ConcreteTypeHandle> * TypeConcretization.ConcretizationContext
        =

        let handles = ImmutableArray.CreateBuilder (types.Length)
        let mutable ctx = ctx

        for i = 0 to types.Length - 1 do
            let handle, newCtx =
                TypeConcretization.concretizeType ctx getAssembly assembly typeArgs methodArgs types.[i]

            handles.Add (handle)
            ctx <- newCtx

        handles.ToImmutable (), ctx

    /// Helper to concretize a method signature
    let private concretizeMethodSignature
        (ctx : TypeConcretization.ConcretizationContext)
        (getAssembly : AssemblyName -> AssemblyReferenceHandle -> DumpedAssembly)
        (assembly : AssemblyName)
        (typeArgs : ConcreteTypeHandle array)
        (methodArgs : ConcreteTypeHandle array)
        (signature : TypeMethodSignature<TypeDefn>)
        : TypeMethodSignature<ConcreteTypeHandle> * TypeConcretization.ConcretizationContext
        =

        // Concretize return type
        let returnHandle, ctx =
            TypeConcretization.concretizeType ctx getAssembly assembly typeArgs methodArgs signature.ReturnType

        // Concretize parameter types
        let paramHandles = ResizeArray<ConcreteTypeHandle> ()
        let mutable ctx = ctx

        for paramType in signature.ParameterTypes do
            let handle, newCtx =
                TypeConcretization.concretizeType ctx getAssembly assembly typeArgs methodArgs paramType

            paramHandles.Add (handle)
            ctx <- newCtx

        let newSignature =
            {
                Header = signature.Header
                ReturnType = returnHandle
                ParameterTypes = paramHandles |> Seq.toList
                GenericParameterCount = signature.GenericParameterCount
                RequiredParameterCount = signature.RequiredParameterCount
            }

        newSignature, ctx

    /// Concretize a method's signature and body
    let concretizeMethod
        (ctx : AllConcreteTypes)
        (getAssembly : AssemblyName -> AssemblyReferenceHandle -> DumpedAssembly)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (baseTypes : BaseClassTypes<DumpedAssembly>)
        (method : WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParameter, TypeDefn>)
        (typeArgs : ConcreteTypeHandle array)
        (methodArgs : ConcreteTypeHandle array)
        : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> * AllConcreteTypes
        =

        let concCtx =
            {
                TypeConcretization.ConcretizationContext.InProgress = ImmutableDictionary.Empty
                TypeConcretization.ConcretizationContext.ConcreteTypes = ctx
                TypeConcretization.ConcretizationContext.LoadedAssemblies = assemblies
                TypeConcretization.ConcretizationContext.BaseTypes = baseTypes
            }

        // First, we need to create a TypeDefn for the declaring type with its generics instantiated
        let declaringTypeDefn =
            if method.DeclaringType._Generics.IsEmpty then
                // Non-generic type
                TypeDefn.FromDefinition (
                    method.DeclaringType._Definition,
                    method.DeclaringType._AssemblyName.FullName,
                    failwith "TODO: determine kind"
                )
            else
                // Generic type - create a GenericInstantiation
                let baseType =
                    TypeDefn.FromDefinition (
                        method.DeclaringType._Definition,
                        method.DeclaringType._AssemblyName.FullName,
                        failwith "TODO: determine kind"
                    )

                let genericArgs =
                    typeArgs
                    |> Array.take method.DeclaringType._Generics.Length
                    |> Array.mapi (fun i _ -> TypeDefn.GenericTypeParameter i)
                    |> ImmutableArray.CreateRange

                TypeDefn.GenericInstantiation (baseType, genericArgs)

        // Concretize the declaring type
        let declaringHandle, concCtx =
            TypeConcretization.concretizeType
                concCtx
                getAssembly
                method.DeclaringType._AssemblyName
                typeArgs
                methodArgs
                declaringTypeDefn

        // Look up the concretized declaring type
        let concretizedDeclaringType =
            AllConcreteTypes.lookup declaringHandle concCtx.ConcreteTypes |> Option.get

        // Concretize signature
        let signature, concCtx =
            concretizeMethodSignature
                concCtx
                getAssembly
                method.DeclaringType._AssemblyName
                typeArgs
                methodArgs
                method.Signature

        // Concretize local variables
        let instructions, concCtx =
            match method.Instructions with
            | None -> None, concCtx
            | Some instr ->
                let locals, concCtx =
                    match instr.LocalVars with
                    | None -> None, concCtx
                    | Some vars ->
                        let handles, ctx =
                            concretizeTypeArray
                                concCtx
                                getAssembly
                                method.DeclaringType._AssemblyName
                                typeArgs
                                methodArgs
                                vars

                        Some handles, ctx

                Some (MethodInstructions.setLocalVars locals instr), concCtx

        // Map generics to handles
        let genericHandles =
            method.Generics
            |> Seq.mapi (fun i _ -> methodArgs.[i])
            |> ImmutableArray.CreateRange

        let concretizedMethod : MethodInfo<_, _, ConcreteTypeHandle> =
            {
                DeclaringType = concretizedDeclaringType
                Handle = method.Handle
                Name = method.Name
                Instructions = instructions
                Parameters = method.Parameters
                Generics = genericHandles
                Signature = signature
                CustomAttributes = method.CustomAttributes
                MethodAttributes = method.MethodAttributes
                ImplAttributes = method.ImplAttributes
                IsStatic = method.IsStatic
            }

        concretizedMethod, concCtx.ConcreteTypes
