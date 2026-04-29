namespace WoofWare.PawPrint

open WoofWare.PawPrint.ExternImplementations

[<RequireQualifiedAccess>]
module NativeKernel32 =
    let private errorEnvVarNotFound : int = 203

    type internal GetEnvironmentVariableWPlan =
        {
            ReturnLength : uint32
            LastError : int
            ValueToWrite : string option
        }

    let internal planGetEnvironmentVariableW (bufferSize : int) (value : string option) : GetEnvironmentVariableWPlan =
        match value with
        | None ->
            {
                ReturnLength = 0u
                LastError = errorEnvVarNotFound
                ValueToWrite = None
            }
        | Some value ->
            let requiredSize = value.Length + 1

            if bufferSize < requiredSize then
                {
                    ReturnLength = uint32 requiredSize
                    LastError = 0
                    ValueToWrite = None
                }
            else
                {
                    ReturnLength = uint32 value.Length
                    LastError = 0
                    ValueToWrite = Some value
                }

    let private withKernel32LastSystemError (error : int) (state : IlMachineState) : IlMachineState =
        // CoreLib's generated P/Invoke wrapper clears and reads this
        // GetLastError slot, then writes LastPInvokeError itself.
        { state with
            LastSystemError = error
        }

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

    let private readUtf16Char
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (charConcreteType : ConcreteType<ConcreteTypeHandle>)
        (ptr : ManagedPointerSource)
        (charIndex : int)
        : char
        =
        let ptr =
            ManagedPointerByteView.addByteOffset baseClassTypes state charConcreteType (charIndex * 2) ptr

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
        let charConcreteType = requiredCharConcreteType operation baseClassTypes state

        let rec loop (charIndex : int) (chars : char list) : string =
            if charIndex > 32767 then
                // Match the Win32 environment-variable name limit; beyond this,
                // PawPrint treats the guest string as unterminated.
                failwith $"%s{operation}: unterminated UTF-16 string exceeded PawPrint's 32767-char scan limit"

            let c = readUtf16Char operation baseClassTypes state charConcreteType ptr charIndex

            if c = char 0 then
                chars |> List.rev |> Array.ofList |> System.String
            else
                loop (charIndex + 1) (c :: chars)

        loop 0 []

    let private writeUtf16Char
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (charConcreteType : ConcreteType<ConcreteTypeHandle>)
        (ptr : ManagedPointerSource)
        (charIndex : int)
        (value : char)
        : IlMachineState
        =
        let ptr =
            ManagedPointerByteView.addByteOffset baseClassTypes state charConcreteType (charIndex * 2) ptr

        IlMachineState.writeManagedByrefBytes state ptr (CliType.ofChar value)

    let private writeNullTerminatedUtf16
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        (value : string)
        : IlMachineState
        =
        let charConcreteType = requiredCharConcreteType operation baseClassTypes state

        // Caller must already have checked capacity; this writes value plus
        // the null terminator unconditionally.
        let state =
            ((state, 0), value)
            ||> Seq.fold (fun (state, charIndex) c ->
                writeUtf16Char operation baseClassTypes state charConcreteType ptr charIndex c, charIndex + 1
            )
            |> fst

        writeUtf16Char operation baseClassTypes state charConcreteType ptr value.Length (char 0)

    let private uint32OfArgument (operation : string) (argName : string) (arg : CliType) : uint32 =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.Int32 i) when i >= 0 -> uint32 i
        | CliType.Numeric (CliNumericType.Int32 i) ->
            failwith
                $"%s{operation}: %s{argName} was Int32 %d{i}, i.e. UInt32 %u{uint32 i}, which exceeds PawPrint's int32 allocation model"
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

            let plan =
                planGetEnvironmentVariableW bufferSize (env.TryGetEnvironmentVariable name)

            let state =
                match plan.ValueToWrite with
                | None -> state
                | Some value ->
                    let bufferPtr =
                        NativeCall.managedPointerOfPointerArgument operation "lpBuffer" instruction.Arguments.[1]

                    writeNullTerminatedUtf16 operation ctx.BaseClassTypes state bufferPtr value

            state
            |> withKernel32LastSystemError plan.LastError
            |> pushUInt32 plan.ReturnLength ctx.Thread
            |> Some
        | _ -> None
