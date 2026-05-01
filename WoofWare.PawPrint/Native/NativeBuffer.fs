namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeBuffer =
    let private byteTemplate : CliType = CliType.Numeric (CliNumericType.UInt8 0uy)

    let private byteType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : ConcreteType<ConcreteTypeHandle>
        =
        let handle =
            AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes baseClassTypes.Byte.Identity
            |> Option.defaultWith (fun () -> failwith "Buffer_MemMove: System.Byte is not concretized")

        AllConcreteTypes.lookup handle state.ConcreteTypes
        |> Option.defaultWith (fun () -> failwith $"Buffer_MemMove: concrete System.Byte handle %O{handle} not found")

    let private readByte (state : IlMachineState) (ptr : ManagedPointerSource) : byte =
        match IlMachineManagedByref.readManagedByrefBytesAs state ptr byteTemplate with
        | CliType.Numeric (CliNumericType.UInt8 b) -> b
        | other -> failwith $"Buffer_MemMove: byte-view read returned non-byte value %O{other}"

    let private writeByte (state : IlMachineState) (ptr : ManagedPointerSource) (value : byte) : IlMachineState =
        IlMachineManagedByref.writeManagedByrefBytes state ptr (CliType.Numeric (CliNumericType.UInt8 value))

    let private checkedByteCount (operation : string) (count : int64) : int =
        if count < 0L then
            failwith $"%s{operation}: byte count %d{count} is negative"

        if count > int64 System.Int32.MaxValue then
            failwith $"%s{operation}: byte count %d{count} exceeds the interpreter Int32 byte-offset model"

        int count

    let private byteCountOfArgument (operation : string) (arg : CliType) : int =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim count)) ->
            checkedByteCount operation count
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.SyntheticCrossArrayOffset count)) ->
            failwith
                $"%s{operation}: byte count came from synthetic cross-storage pointer subtraction %d{count}, which is not a valid UIntPtr length"
        | CliType.Numeric (CliNumericType.Int64 count) -> checkedByteCount operation count
        | CliType.Numeric (CliNumericType.Int32 count) -> checkedByteCount operation (int64 count)
        | other -> failwith $"%s{operation}: expected UIntPtr byte count, got %O{other}"

    let private projectionByteOffset (projs : ByrefProjection list) : int64 option =
        let rec loop (byteOffset : int64) (projs : ByrefProjection list) : int64 option =
            match projs with
            | [] -> Some byteOffset
            | ByrefProjection.ReinterpretAs _ :: rest -> loop byteOffset rest
            | ByrefProjection.ByteOffset offset :: rest -> loop (byteOffset + int64 offset) rest
            | ByrefProjection.Field _ :: _ -> None

        loop 0L projs

    let private byteLocation
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : (ByteStorageIdentity * int64) option
        =
        match ptr with
        | ManagedPointerSource.Null -> None
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, index), projs) ->
            projectionByteOffset projs
            |> Option.map (fun byteOffset ->
                ByteStorageIdentity.Array arr,
                ManagedPointerByteView.arrayBytePosition baseClassTypes state arr index byteOffset
            )
        | ManagedPointerSource.Byref (ByrefRoot.StringCharAt (str, charIndex), projs) ->
            projectionByteOffset projs
            |> Option.map (fun byteOffset -> ByteStorageIdentity.String str, int64 charIndex * 2L + byteOffset)
        | ManagedPointerSource.Byref (ByrefRoot.PeByteRange peByteRange, projs) ->
            projectionByteOffset projs
            |> Option.map (fun byteOffset -> ByteStorageIdentity.PeByteRange peByteRange, byteOffset)
        | ManagedPointerSource.Byref (ByrefRoot.LocalMemoryByte (thread, frame, block, rootByteOffset), projs) ->
            projectionByteOffset projs
            |> Option.map (fun byteOffset ->
                ByteStorageIdentity.LocalMemory (thread, frame, block), int64 rootByteOffset + byteOffset
            )
        | ManagedPointerSource.Byref (ByrefRoot.LocalVariable (thread, frame, local), projs) ->
            projectionByteOffset projs
            |> Option.map (fun byteOffset -> ByteStorageIdentity.StackLocal (thread, frame, local), byteOffset)
        | ManagedPointerSource.Byref (ByrefRoot.Argument (thread, frame, arg), projs) ->
            projectionByteOffset projs
            |> Option.map (fun byteOffset -> ByteStorageIdentity.StackArgument (thread, frame, arg), byteOffset)
        | ManagedPointerSource.Byref (ByrefRoot.StaticField (declaringType, field), projs) ->
            projectionByteOffset projs
            |> Option.map (fun byteOffset -> ByteStorageIdentity.StaticField (declaringType, field), byteOffset)
        // These roots do not expose a stable flat byte coordinate here. The
        // supported Buffer_MemMove overlap paths are flat byte-storage-backed;
        // if aliased overlap on these roots appears, extend this model rather
        // than guessing a projection.
        | ManagedPointerSource.Byref (ByrefRoot.HeapValue _, _)
        | ManagedPointerSource.Byref (ByrefRoot.HeapObjectField _, _) -> None

    let private shouldCopyBackwards
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (dest : ManagedPointerSource)
        (byteCount : int)
        : bool
        =
        match byteLocation baseClassTypes state src, byteLocation baseClassTypes state dest with
        | Some (srcStorage, srcOffset), Some (destStorage, destOffset) when srcStorage = destStorage ->
            srcOffset < destOffset && destOffset < srcOffset + int64 byteCount
        | _ -> false

    let private copy
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (dest : ManagedPointerSource)
        (src : ManagedPointerSource)
        (byteCount : int)
        : IlMachineState
        =
        let byteConcreteType = byteType baseClassTypes state
        let mutable state = state

        if shouldCopyBackwards baseClassTypes state src dest byteCount then
            for i = byteCount - 1 downto 0 do
                let src =
                    ManagedPointerByteView.addByteOffset baseClassTypes state byteConcreteType i src

                let dest =
                    ManagedPointerByteView.addByteOffset baseClassTypes state byteConcreteType i dest

                let value = readByte state src
                state <- writeByte state dest value
        else
            for i = 0 to byteCount - 1 do
                let src =
                    ManagedPointerByteView.addByteOffset baseClassTypes state byteConcreteType i src

                let dest =
                    ManagedPointerByteView.addByteOffset baseClassTypes state byteConcreteType i dest

                let value = readByte state src
                state <- writeByte state dest value

        state

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : ExecutionResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "Buffer_MemMove",
          "System.Private.CoreLib",
          "System",
          "Buffer",
          "__Memmove",
          [ ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
            ConcreteUIntPtr state.ConcreteTypes ],
          MethodReturnType.Void
        | "Buffer_MemMove",
          "System.Private.CoreLib",
          "System",
          "Buffer",
          "MemmoveInternal",
          [ ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
            ConcreteUIntPtr state.ConcreteTypes ],
          MethodReturnType.Void ->
            let operation = "Buffer_MemMove"

            if instruction.Arguments.Length <> 3 then
                failwith
                    $"%s{operation}: expected three native arguments after matching signature, got %d{instruction.Arguments.Length}"

            let dest =
                NativeCall.managedPointerOfPointerArgument operation "dest" instruction.Arguments.[0]

            let src =
                NativeCall.managedPointerOfPointerArgument operation "src" instruction.Arguments.[1]

            let byteCount = byteCountOfArgument operation instruction.Arguments.[2]

            let state =
                if byteCount = 0 then
                    state
                else
                    copy ctx.BaseClassTypes state dest src byteCount

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | _ -> None
