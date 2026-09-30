namespace WoofWare.PawPrint

open System
open System.IO
open System.Reflection
open WoofWare.PosixKernel

open NativeRuntimeTypeHelpers

/// QCalls declared on `System.Reflection.Emit.RuntimeAssemblyBuilder`, which create the runtime half
/// of an `AssemblyBuilder`.
[<RequireQualifiedAccess>]
module NativeRuntimeAssemblyBuilder =
    /// CoreCLR's `ASSEMBLY_ACCESS_COLLECT` bit of `AssemblyBuilderAccess`, which asks for the assembly
    /// to live in a collectible loader allocator of its own.
    let private accessCollect : int = 8

    /// `AssemblyBuilderAccess.Run`, the only access the public factory admits besides `RunAndCollect`.
    let private accessRun : int = 1

    /// `CALG_SHA1`, which `Assembly::CreateDynamic` stores when the caller passes no hash algorithm.
    let private calgSha1 : int = 0x8004

    /// A version-4 GUID over bytes drawn from the kernel's entropy pool, as `minipal_guid_v4_create`
    /// makes one over secure random bytes: the metadata emitter stamps a dynamic module's version ID
    /// that way, so it differs between runs of the real runtime.
    let private freshModuleVersionId (state : IlMachineState) : Guid * IlMachineState =
        let bytes, pool = EntropyPool.draw 16 state.Kernel.Machine.EntropyPool
        let bytes = Seq.toArray bytes
        // `Data3` is bytes 6 and 7, little-endian, and its top nibble is the version.
        bytes.[7] <- (bytes.[7] &&& 0x0Fuy) ||| 0x40uy
        // The top two bits of `Data4[0]` are the RFC 4122 variant.
        bytes.[8] <- (bytes.[8] &&& 0x3Fuy) ||| 0x80uy

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    Machine =
                        { kernel.Machine with
                            EntropyPool = pool
                        }
                }
            )

        Guid bytes, state

    /// The binder behind a managed `AssemblyLoadContext`: its `_nativeAssemblyLoadContext` field,
    /// which `AssemblyNative_InitializeAssemblyLoadContext` filled.
    let private binderOfContext
        (operation : string)
        (ctx : NativeCallContext)
        (context : ManagedHeapAddress)
        (state : IlMachineState)
        : AssemblyBinder * IlMachineState
        =
        let state, _, contextType =
            concretizeNonGenericCorelibType
                ctx.LoggerFactory
                ctx.BaseClassTypes
                state
                "System.Runtime.Loader"
                "AssemblyLoadContext"

        let field =
            IlMachineState.requiredOwnInstanceFieldId state contextType "_nativeAssemblyLoadContext"

        let heapObj = ManagedHeap.get context state.ManagedHeap

        match
            AllocatedNonArrayObject.DereferenceFieldById field heapObj
            |> CliType.unwrapPrimitiveLikeDeep
        with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.AssemblyBinderPtr binder)) -> binder, state
        | other -> failwith $"%s{operation}: expected the load context's binder, got %O{other}"

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "AppDomain_CreateDynamicAssembly",
          "System.Private.CoreLib",
          "System.Reflection.Emit",
          "RuntimeAssemblyBuilder",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "ObjectHandleOnStack", contextGenerics)
            ConcretePointer (CorelibType state.ConcreteTypes ("System.Reflection",
                                                              "NativeAssemblyNameParts",
                                                              partsGenerics))
            CorelibType state.ConcreteTypes ("System.Configuration.Assemblies", "AssemblyHashAlgorithm", hashGenerics)
            CorelibType state.ConcreteTypes ("System.Reflection.Emit", "AssemblyBuilderAccess", accessGenerics)
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "ObjectHandleOnStack", retGenerics) ],
          MethodReturnType.Void when
            contextGenerics.IsEmpty
            && partsGenerics.IsEmpty
            && hashGenerics.IsEmpty
            && accessGenerics.IsEmpty
            && retGenerics.IsEmpty
            ->
            let operation = "AppDomain_CreateDynamicAssembly"

            if instruction.Arguments.Length <> 5 then
                failwith $"%s{operation}: expected five native arguments, got %d{instruction.Arguments.Length}"

            let contextPtr =
                NativeCall.objectHandleOnStackTarget operation state "assemblyLoadContext" instruction.Arguments.[0]

            let context =
                match
                    IlMachineState.readManagedByref
                        ctx.BaseClassTypes
                        state
                        (ManagedPointerSource.requireAddressed contextPtr)
                with
                | CliType.ObjectRef (Some context) -> context
                | other ->
                    failwith
                        $"%s{operation}: expected a load context behind assemblyLoadContext, got %O{other}; every caller passes one"

            let binder, state = binderOfContext operation ctx context state

            match binder with
            | AssemblyBinder.Default -> ()

            // `RuntimeAssemblyBuilder.CreateDynamicAssembly` fills a `NativeAssemblyNameParts` local
            // from the `AssemblyName` and passes its address.
            let parts =
                let partsPtr =
                    NativeCall.managedPointerOfPointerArgument operation "pAssemblyName" instruction.Arguments.[1]

                match
                    IlMachineState.readManagedByref
                        ctx.BaseClassTypes
                        state
                        (ManagedPointerSource.requireAddressed partsPtr)
                with
                | CliType.ValueType parts -> parts
                | other ->
                    failwith $"%s{operation}: expected NativeAssemblyNameParts behind pAssemblyName, got %O{other}"

            let field (name : string) : CliType =
                let fieldId = IlMachineState.requiredOwnInstanceFieldId state parts.Declared name
                CliValueType.DereferenceFieldById fieldId parts

            let simpleName =
                match NativeCall.managedPointerOfPointerArgument operation "_pName" (field "_pName") with
                | ManagedPointerSource.Null -> ""
                | namePtr -> NativeCall.readNullTerminatedUtf16 operation ctx.BaseClassTypes state namePtr

            // `Assembly::CreateDynamic` refuses a null or empty name before defining anything.
            if simpleName = "" then
                NativeHandlerResult.raiseExceptionWithMessage
                    ctx.BaseClassTypes.ArgumentException
                    (Some "AssemblyName.Name cannot be null or an empty string.")
                    state
                |> Some
            else

            // Checked after the name, as `Assembly::CreateDynamic` does.
            let access = NativeCall.int32Argument operation instruction.Arguments.[3]

            if access &&& accessCollect <> 0 then
                failwith
                    $"TODO: %s{operation}: AssemblyBuilderAccess 0x%x{access} asks for a collectible dynamic assembly, which needs a second loader allocator, and PawPrint has one"

            if access <> accessRun then
                failwith
                    $"%s{operation}: AssemblyBuilderAccess 0x%x{access} is neither Run nor RunAndCollect, which AssemblyBuilder.DefineDynamicAssembly admits alone"

            if NativeCall.int32Argument operation (field "_cbPublicKeyOrToken") <> 0 then
                failwith
                    $"TODO: %s{operation}: the dynamic assembly '%s{simpleName}' has a public key; CoreCLR raises SecurityException (Invalid assembly public key.) for one StrongNameIsValidPublicKey rejects, and PawPrint does not yet validate keys"

            // A null culture pointer and an empty culture both define a neutral assembly.
            let culture =
                match NativeCall.managedPointerOfPointerArgument operation "_pCultureName" (field "_pCultureName") with
                | ManagedPointerSource.Null -> ""
                | culturePtr -> NativeCall.readNullTerminatedUtf16 operation ctx.BaseClassTypes state culturePtr

            let hashAlgorithm =
                match NativeCall.int32Argument operation instruction.Arguments.[2] with
                | 0 -> calgSha1
                | algorithm -> algorithm

            let name : DynamicAssemblyName =
                {
                    SimpleName = simpleName
                    Version =
                        Version (
                            int (NativeCall.uint16Argument operation (field "_major")),
                            int (NativeCall.uint16Argument operation (field "_minor")),
                            int (NativeCall.uint16Argument operation (field "_build")),
                            int (NativeCall.uint16Argument operation (field "_revision"))
                        )
                    Culture = culture
                    PublicKey = System.Collections.Immutable.ImmutableArray<byte>.Empty
                    Flags = enum<AssemblyFlags> (NativeCall.int32Argument operation (field "_flags"))
                    HashAlgorithm = enum<System.Reflection.AssemblyHashAlgorithm> hashAlgorithm
                }

            let moduleVersionId, state = freshModuleVersionId state

            let assembly =
                use image = new MemoryStream (DynamicAssemblyImage.build name moduleVersionId)
                Assembly.read ctx.LoggerFactory None image

            let state =
                match state.WithDynamicAssembly assembly with
                | Ok state -> state
                | Error existing ->
                    failwith
                        $"TODO: %s{operation}: a dynamic assembly named %s{assembly.DefinitionFullName} would share its identity with the assembly already loaded as %s{existing.DefinitionFullName}; CoreCLR admits both, but PawPrint identifies an assembly by that name"

            let runtimeAssembly, state =
                NativeRuntimeType.getOrAllocateRuntimeAssembly
                    ctx.LoggerFactory
                    ctx.BaseClassTypes
                    assembly.DefinitionFullName
                    state

            let retAssembly =
                NativeCall.objectHandleOnStackTarget operation state "retAssembly" instruction.Arguments.[4]

            IlMachineState.writeManagedByrefWithBase
                ctx.BaseClassTypes
                state
                (ManagedPointerSource.requireAddressed retAssembly)
                (CliType.ObjectRef (Some runtimeAssembly))
            |> NativeHandlerResult.completed
            |> Some
        | _ -> None
