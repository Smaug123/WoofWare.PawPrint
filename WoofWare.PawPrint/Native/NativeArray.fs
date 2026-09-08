namespace WoofWare.PawPrint

open System.Collections.Immutable

[<RequireQualifiedAccess>]
module NativeArray =
    let private int32OfCliType (operation : string) (argName : string) (arg : CliType) : int =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.Int32 i) -> i
        | other -> failwith $"%s{operation}: expected %s{argName} as Int32, got %O{other}"

    let private requiredInt32ConcreteType
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : ConcreteType<ConcreteTypeHandle>
        =
        let handle =
            AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes baseClassTypes.Int32.Identity
            |> Option.defaultWith (fun () -> failwith $"%s{operation}: System.Int32 is not concretized")

        AllConcreteTypes.lookup handle state.ConcreteTypes
        |> Option.defaultWith (fun () -> failwith $"%s{operation}: concrete System.Int32 handle %O{handle} not found")

    /// Read `buffer[index]` of an `int*` argument. The native side indexes the pointer in
    /// bytes at the Int32 stride whatever the buffer's provenance, so this does the same:
    /// CoreLib hands over a `stackalloc int[]`, an `int[]` pinned by `fixed`, or the address
    /// of a single `int` local, and only the last of those is limited to index 0.
    let private readInt32Element
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (int32ConcreteType : ConcreteType<ConcreteTypeHandle>)
        (argName : string)
        (buffer : ManagedPointerSource)
        (index : int)
        : int
        =
        match buffer with
        | ManagedPointerSource.Null -> failwith $"%s{operation}: expected non-null %s{argName} pointer"
        | ManagedPointerSource.NativeIntPlaceholder bits ->
            failwith
                $"%s{operation}: cannot read %s{argName} through fake non-null byref @ 0x%x{bits}; the placeholder must never be dereferenced"
        | ManagedPointerSource.Byref _ ->
            let ptr =
                ManagedPointerByteView.addByteOffset state int32ConcreteType (index * sizeof<int32>) buffer

            IlMachineState.readManagedByref baseClassTypes state ptr
            |> int32OfCliType operation $"%s{argName}[%d{index}]"

    /// CoreCLR's `MAX_RANK` (vm/array.h): the most dimensions an array type can have.
    let private maxRank = 32

    /// CoreCLR's `CheckElementType` (classlibnative/bcltype/arraynative.cpp): the element
    /// types no array can be made of, each carrying the message the `NotSupportedException`
    /// CoreCLR throws renders from its resource string. `None` means the element is allowed.
    ///
    /// Pointers and function pointers are `TypeDesc`s this screen deliberately lets through:
    /// only a byref or a generic variable is refused there, and `int*[,]` is a legal array
    /// type (measured on real .NET). Open generic types, generic parameters and open
    /// constructed types never reach here — each is its own `RuntimeTypeHandleTarget` arm,
    /// refused before the element is in hand.
    let private forbiddenElementTypeMessage
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (element : ConcreteTypeHandle)
        : string option
        =
        match element with
        | ConcreteTypeHandle.Byref _ -> Some "Type is not supported."
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> None
        | ConcreteTypeHandle.Concrete _
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            // An array-typed element (`int[][,]`) has no nominal TypeDef to ask, and is
            // neither byref-like nor `System.Void`, so `None` is the right answer for it.
            match AllConcreteTypes.tryTypeInfo state._LoadedAssemblies state.ConcreteTypes element with
            | None -> None
            | Some (concreteType, typeInfo) ->
                if DumpedAssembly.isByRefLike baseClassTypes state._LoadedAssemblies typeInfo then
                    Some "Cannot create arrays of ByRef-like values."
                elif concreteType.Identity = baseClassTypes.Void.Identity then
                    Some "Arrays of System.Void are not supported."
                else
                    None

    /// The array type to allocate and its element type, or the message of the
    /// `NotSupportedException` CoreCLR raises for an element type no array can hold.
    let private arrayTypeForCreateInstance
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (operation : string)
        (fromArrayType : bool)
        (rank : int)
        (target : RuntimeTypeHandleTarget)
        : Result<ConcreteTypeHandle * ConcreteTypeHandle, string>
        =
        match target with
        | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
            RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
        | RuntimeTypeHandleTarget.OpenConstructed _ as openConstructed ->
            failwith
                $"TODO: open constructed types are not handled at Native/NativeArray.fs:%s{__LINE__}; got %O{openConstructed}"
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            failwith $"TODO: %s{operation} for open generic type definition %O{identity}"
        | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
            failwith $"TODO: %s{operation} for generic parameter #%i{position} of %O{declaringType.TypeDefinition.Get}"
        | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
            failwith
                $"TODO: %s{operation} for method generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}"
        | RuntimeTypeHandleTarget.Composite _
        | RuntimeTypeHandleTarget.FunctionPointer _ -> RuntimeTypeHandleTarget.refuseComposite operation target
        | RuntimeTypeHandleTarget.Closed typeHandle ->
            if fromArrayType then
                // No element-type screen on this path, as CoreCLR has none: the array type is
                // already loaded, and an array type with a forbidden element could not have
                // been formed to be passed in.
                match typeHandle with
                | ConcreteTypeHandle.OneDimArrayZero element when rank = 1 -> Ok (typeHandle, element)
                | ConcreteTypeHandle.Array (_, 1) ->
                    // A rank-1 ELEMENT_TYPE_ARRAY (`T[*]`) is a distinct runtime type from the
                    // szarray `T[]`, and PawPrint has no allocation for it: the multi-dim
                    // constructor refuses rank 1 for the same reason.
                    failwith
                        $"TODO: %s{operation} from rank-1 multidimensional array type %O{typeHandle}; PawPrint does not model T[*]"
                | ConcreteTypeHandle.Array (element, arrayRank) when rank = arrayRank -> Ok (typeHandle, element)
                | ConcreteTypeHandle.Array _ ->
                    failwith $"%s{operation}: requested rank %d{rank} does not match array type %O{typeHandle}"
                | other -> failwith $"%s{operation}: fromArrayType=true expected array RuntimeType, got %O{other}"
            else

            match forbiddenElementTypeMessage baseClassTypes state typeHandle with
            | Some message -> Error message
            | None ->

            if rank = 1 then
                Ok (ConcreteTypeHandle.OneDimArrayZero typeHandle, typeHandle)
            elif rank > maxRank then
                // `ClassLoader::LoadArrayTypeThrowing` refuses the type itself with
                // IDS_CLASSLOAD_RANK_TOOLARGE, so the guest sees a TypeLoadException naming
                // the array type and its assembly.
                failwith
                    $"TODO: %s{operation} for rank %d{rank}, above CoreCLR's MAX_RANK of %d{maxRank}; should raise TypeLoadException"
            else
                Ok (ConcreteTypeHandle.Array (typeHandle, rank), typeHandle)

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
        | "Array_CreateInstance",
          "System.Private.CoreLib",
          "System",
          "Array",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallTypeHandle", qCallGenerics)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "ObjectHandleOnStack",
                                             objectHandleGenerics) ],
          MethodReturnType.Void when qCallGenerics.IsEmpty && objectHandleGenerics.IsEmpty ->
            let operation = "Array.CreateInstance"

            if instruction.Arguments.Length <> 6 then
                failwith $"%s{operation}: expected six native arguments, got %d{instruction.Arguments.Length}"

            let typeHandle =
                NativeCall.qCallTypeHandleToRuntimeTypeHandleTarget
                    operation
                    state
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let rank = int32OfCliType operation "rank" instruction.Arguments.[1]

            let lengths =
                NativeCall.managedPointerOfPointerArgument operation "lengths" instruction.Arguments.[2]

            let lowerBounds =
                NativeCall.managedPointerOfPointerArgument operation "lowerBounds" instruction.Arguments.[3]

            let fromArrayType =
                match int32OfCliType operation "fromArrayType" instruction.Arguments.[4] with
                | 0 -> false
                | _ -> true

            let retArray =
                NativeCall.objectHandleOnStackTarget operation state "retArray" instruction.Arguments.[5]

            if rank < 1 then
                failwith $"%s{operation}: rank %d{rank} violates the QCall's precondition that it is positive"

            // CoreCLR screens the element type and loads the array type before it reads any
            // length, so a forbidden element type is reported even where a length would
            // independently have failed.
            match arrayTypeForCreateInstance ctx.BaseClassTypes state operation fromArrayType rank typeHandle with
            | Error message ->
                NativeHandlerResult.raiseExceptionWithMessage
                    ctx.BaseClassTypes.NotSupportedException
                    (Some message)
                    state
                |> Some
            | Ok (arrayType, elementType) ->

            let int32ConcreteType = requiredInt32ConcreteType operation ctx.BaseClassTypes state

            let dimensionLengths =
                Array.init
                    rank
                    (readInt32Element ctx.BaseClassTypes operation state int32ConcreteType "lengths" lengths)

            // CoreCLR validates the dimensions inside `AllocateArrayEx`, after the array type is
            // in hand, and answers a violation with an exception the guest can catch. The
            // szarray path shares the rule for its single dimension, since `AllocateArrayEx`
            // forwards a rank-1 zero-lower-bound request to `AllocateSzArray`.
            let dimensionLengths = ImmutableArray.CreateRange dimensionLengths

            let totalLength =
                match MultiDimArrayAllocation.totalElements dimensionLengths with
                | Error err -> Error (MultiDimArrayAllocation.exceptionFor ctx.BaseClassTypes err)
                | Ok totalLength -> Ok totalLength

            match totalLength with
            | Error (exnType, message) -> NativeHandlerResult.raiseExceptionWithMessage exnType message state |> Some
            | Ok totalLength ->

            match lowerBounds with
            | ManagedPointerSource.Null -> ()
            | ManagedPointerSource.NativeIntPlaceholder bits ->
                failwith
                    $"%s{operation}: cannot read lowerBounds through fake non-null byref @ 0x%x{bits}; the placeholder must never be dereferenced"
            | ManagedPointerSource.Byref _ ->
                for i in 0 .. rank - 1 do
                    let lowerBound =
                        readInt32Element
                            ctx.BaseClassTypes
                            operation
                            state
                            int32ConcreteType
                            "lowerBounds"
                            lowerBounds
                            i

                    if lowerBound <> 0 then
                        failwith
                            $"TODO: %s{operation} with non-zero lower bound %d{lowerBound} at dimension %d{i}; PawPrint only models zero lower bounds"

            let zero, state =
                IlMachineState.cliTypeZeroOfHandle state ctx.BaseClassTypes elementType

            let arrayAddr, state =
                match arrayType with
                | ConcreteTypeHandle.OneDimArrayZero _ ->
                    // A szarray's own length *is* the total, rank being 1 here.
                    IlMachineState.allocateArray arrayType (fun () -> zero) totalLength state
                | ConcreteTypeHandle.Array _ ->
                    IlMachineState.allocateMultiDimArray arrayType (fun () -> zero) dimensionLengths state
                | other -> failwith $"%s{operation}: arrayTypeForCreateInstance answered non-array type %O{other}"

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    retArray
                    (CliType.ObjectRef (Some arrayAddr))

            NativeHandlerResult.completed state |> Some
        | _ -> None
