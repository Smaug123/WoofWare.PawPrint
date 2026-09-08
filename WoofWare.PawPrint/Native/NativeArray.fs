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

    let private arrayTypeForCreateInstance
        (operation : string)
        (fromArrayType : bool)
        (rank : int)
        (target : RuntimeTypeHandleTarget)
        : ConcreteTypeHandle * ConcreteTypeHandle
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
                match typeHandle with
                | ConcreteTypeHandle.OneDimArrayZero element when rank = 1 -> typeHandle, element
                | ConcreteTypeHandle.Array (_, 1) ->
                    // A rank-1 ELEMENT_TYPE_ARRAY (`T[*]`) is a distinct runtime type from the
                    // szarray `T[]`, and PawPrint has no allocation for it: the multi-dim
                    // constructor refuses rank 1 for the same reason.
                    failwith
                        $"TODO: %s{operation} from rank-1 multidimensional array type %O{typeHandle}; PawPrint does not model T[*]"
                | ConcreteTypeHandle.Array (element, arrayRank) when rank = arrayRank -> typeHandle, element
                | ConcreteTypeHandle.Array _ ->
                    failwith $"%s{operation}: requested rank %d{rank} does not match array type %O{typeHandle}"
                | other -> failwith $"%s{operation}: fromArrayType=true expected array RuntimeType, got %O{other}"
            else if rank = 1 then
                ConcreteTypeHandle.OneDimArrayZero typeHandle, typeHandle
            else if rank > maxRank then
                // `ClassLoader::LoadArrayTypeThrowing` refuses the type itself with
                // IDS_CLASSLOAD_RANK_TOOLARGE, so the guest sees a TypeLoadException naming
                // the array type and its assembly.
                failwith
                    $"TODO: %s{operation} for rank %d{rank}, above CoreCLR's MAX_RANK of %d{maxRank}; should raise TypeLoadException"
            else
                ConcreteTypeHandle.Array (typeHandle, rank), typeHandle

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

            let int32ConcreteType = requiredInt32ConcreteType operation ctx.BaseClassTypes state

            let dimensionLengths =
                Array.init
                    rank
                    (readInt32Element ctx.BaseClassTypes operation state int32ConcreteType "lengths" lengths)

            for i in 0 .. rank - 1 do
                if dimensionLengths.[i] < 0 then
                    failwith
                        $"TODO: %s{operation} with negative length %d{dimensionLengths.[i]} at dimension %d{i} should throw ArgumentOutOfRangeException"

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

            let arrayType, elementType =
                arrayTypeForCreateInstance operation fromArrayType rank typeHandle

            let zero, state =
                IlMachineState.cliTypeZeroOfHandle state ctx.BaseClassTypes elementType

            let arrayAddr, state =
                match arrayType with
                | ConcreteTypeHandle.OneDimArrayZero _ ->
                    IlMachineState.allocateArray arrayType (fun () -> zero) dimensionLengths.[0] state
                | ConcreteTypeHandle.Array _ ->
                    IlMachineState.allocateMultiDimArray
                        arrayType
                        (fun () -> zero)
                        (ImmutableArray.CreateRange dimensionLengths)
                        state
                | other -> failwith $"%s{operation}: arrayTypeForCreateInstance answered non-array type %O{other}"

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    retArray
                    (CliType.ObjectRef (Some arrayAddr))

            NativeHandlerResult.completed state |> Some
        | _ -> None
