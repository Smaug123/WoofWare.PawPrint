namespace WoofWare.PawPrint

open WoofWare.PawPrint.ExternImplementations

[<RequireQualifiedAccess>]
module NativeKernel32 =
    let private errorEnvVarNotFound : int = 203

    let private arrayElementHandle (arrObj : AllocatedArray) : ConcreteTypeHandle =
        match arrObj.ConcreteType with
        | ConcreteTypeHandle.OneDimArrayZero element -> element
        | ConcreteTypeHandle.Array (element, _) -> element
        | ConcreteTypeHandle.Concrete _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _ -> failwith $"array object has non-array concrete type: %O{arrObj.ConcreteType}"

    let private arrayElementSize
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (arr : ManagedHeapAddress)
        : int
        =
        let obj = state.ManagedHeap.Arrays.[arr]

        if obj.Length > 0 then
            CliType.sizeOf obj.Elements.[0]
        else
            let zero, _ =
                CliType.zeroOf state.ConcreteTypes state._LoadedAssemblies baseClassTypes (arrayElementHandle obj)

            CliType.sizeOf zero

    let private requiredCharConcreteType
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : ConcreteType<ConcreteTypeHandle>
        =
        let handle =
            AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes baseClassTypes.Char.Identity
            |> Option.defaultWith (fun () -> failwith $"%s{operation}: System.Char is not concretized")

        AllConcreteTypes.lookup handle state.ConcreteTypes
        |> Option.defaultWith (fun () -> failwith $"%s{operation}: concrete System.Char handle %O{handle} not found")

    let private addByteOffset
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (byteOffset : int)
        (ptr : ManagedPointerSource)
        : ManagedPointerSource
        =
        let charConcreteType = requiredCharConcreteType operation baseClassTypes state

        ptr
        |> ManagedPointerSource.appendProjection (ByrefProjection.ReinterpretAs charConcreteType)
        |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset byteOffset)
        |> ManagedPointerSource.normaliseLocalMemoryByteOffset
        |> ManagedPointerSource.normaliseArrayByteOffset (arrayElementSize baseClassTypes state)
        |> ManagedPointerSource.normaliseStringByteOffset

    let private readUtf16Char
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        (charIndex : int)
        : char
        =
        let ptr = addByteOffset operation baseClassTypes state (charIndex * 2) ptr

        match IlMachineState.readManagedByrefBytesAs state ptr (CliType.ofChar (char 0)) with
        | CliType.Char (high, low) -> char (int high * 256 + int low)
        | other -> failwith $"%s{operation}: UTF-16 char read returned non-char value %O{other}"

    let private readNullTerminatedUtf16
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : string
        =
        let rec loop (charIndex : int) (chars : char list) : string =
            if charIndex > 32767 then
                failwith $"%s{operation}: unterminated UTF-16 string exceeded 32767 chars"

            let c = readUtf16Char operation baseClassTypes state ptr charIndex

            if c = char 0 then
                chars |> List.rev |> Array.ofList |> System.String
            else
                loop (charIndex + 1) (c :: chars)

        loop 0 []

    let private writeUtf16Char
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        (charIndex : int)
        (value : char)
        : IlMachineState
        =
        let ptr = addByteOffset operation baseClassTypes state (charIndex * 2) ptr
        IlMachineState.writeManagedByrefBytes state ptr (CliType.ofChar value)

    let private writeNullTerminatedUtf16
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        (value : string)
        : IlMachineState
        =
        let state =
            ((state, 0), value)
            ||> Seq.fold (fun (state, charIndex) c ->
                writeUtf16Char operation baseClassTypes state ptr charIndex c, charIndex + 1
            )
            |> fst

        writeUtf16Char operation baseClassTypes state ptr value.Length (char 0)

    let private uint32OfArgument (operation : string) (argName : string) (arg : CliType) : uint32 =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.Int32 i) -> uint32 i
        | CliType.Numeric (CliNumericType.Int64 i) when i >= 0L && i <= int64 System.UInt32.MaxValue -> uint32 i
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim i)) when
            i >= 0L && i <= int64 System.UInt32.MaxValue
            ->
            uint32 i
        | other -> failwith $"%s{operation}: expected %s{argName} to be UInt32, got %O{other}"

    let private checkedBufferSize (operation : string) (value : uint32) : int =
        if value > uint32 System.Int32.MaxValue then
            failwith $"%s{operation}: buffer size %d{value} exceeds PawPrint's int32 allocation model"

        int value

    let private pushUInt32 (value : uint32) (thread : ThreadId) (state : IlMachineState) : ExecutionResult =
        state
        |> IlMachineState.pushToEvalStack (NativeCall.cliUInt32 value) thread
        |> Tuple.withRight WhatWeDid.Executed
        |> ExecutionResult.Stepped

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : ExecutionResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "GetEnvironmentVariableW",
          "System.Private.CoreLib",
          "Kernel32",
          [ ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt16)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Char)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32) ->
            let operation = "GetEnvironmentVariableW"

            let namePtr =
                NativeCall.managedPointerOfPointerArgument operation "lpName" instruction.Arguments.[0]

            let bufferSize =
                instruction.Arguments.[2]
                |> uint32OfArgument operation "nSize"
                |> checkedBufferSize operation

            let name = readNullTerminatedUtf16 operation ctx.BaseClassTypes state namePtr
            let env = ISystem_Environment_Env.get ctx.Implementations

            match env.TryGetEnvironmentVariable name with
            | None ->
                { state with
                    LastPInvokeError = errorEnvVarNotFound
                    LastSystemError = errorEnvVarNotFound
                }
                |> pushUInt32 0u ctx.Thread
                |> Some
            | Some value ->
                let requiredSize = value.Length + 1

                if bufferSize < requiredSize then
                    { state with
                        LastPInvokeError = 0
                        LastSystemError = 0
                    }
                    |> pushUInt32 (uint32 requiredSize) ctx.Thread
                    |> Some
                else
                    let bufferPtr =
                        NativeCall.managedPointerOfPointerArgument operation "lpBuffer" instruction.Arguments.[1]

                    let state =
                        writeNullTerminatedUtf16 operation ctx.BaseClassTypes state bufferPtr value

                    { state with
                        LastPInvokeError = 0
                        LastSystemError = 0
                    }
                    |> pushUInt32 (uint32 value.Length) ctx.Thread
                    |> Some
        | _ -> None
