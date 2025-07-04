namespace WoofWare.PawPrint

open System

[<RequireQualifiedAccess>]
module Intrinsics =
    let private safeIntrinsics =
        [
            // The IL implementation is fine: https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/Unsafe.cs#L677
            "System.Private.CoreLib", "Unsafe", "AsRef"
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/String.cs#L739-L750
            "System.Private.CoreLib", "String", "get_Length"
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/ArgumentNullException.cs#L54
            "System.Private.CoreLib", "ArgumentNullException", "ThrowIfNull"
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/coreclr/System.Private.CoreLib/src/System/Type.CoreCLR.cs#L82
            "System.Private.CoreLib", "Type", "GetTypeFromHandle"
        ]
        |> Set.ofList

    let call
        (baseClassTypes : BaseClassTypes<_>)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : IlMachineState option
        =
        let callerAssy =
            state.ThreadState.[currentThread].MethodState.ExecutingMethod.DeclaringType.Assembly

        if
            methodToCall.DeclaringType.Assembly.Name = "System.Private.CoreLib"
            && methodToCall.DeclaringType.Name = "Volatile"
        then
            // These are all safely implemented in IL, just inefficient.
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/Threading/Volatile.cs#L13
            None
        elif
            Set.contains
                (methodToCall.DeclaringType.Assembly.Name, methodToCall.DeclaringType.Name, methodToCall.Name)
                safeIntrinsics
        then
            None
        else

        match methodToCall.DeclaringType.Assembly.Name, methodToCall.DeclaringType.Name, methodToCall.Name with
        | "System.Private.CoreLib", "Type", "get_TypeHandle" ->
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/Type.cs#L470
            // no args, returns RuntimeTypeHandle, a struct with a single field (a RuntimeType class)

            // The thing on top of the stack will be a RuntimeType.
            let arg, state = IlMachineState.popEvalStack currentThread state

            let arg =
                let rec go (arg : EvalStackValue) =
                    match arg with
                    | EvalStackValue.UserDefinedValueType [ _, s ] -> go s
                    | EvalStackValue.ManagedPointer ManagedPointerSource.Null -> failwith "TODO: throw NRE"
                    | EvalStackValue.ManagedPointer (ManagedPointerSource.Heap addr) -> Some addr
                    | s -> failwith $"TODO: called with unrecognised arg %O{s}"

                go arg

            let state =
                let vt =
                    {
                        Fields = [ "m_type", CliType.ObjectRef arg ]
                    }

                IlMachineState.pushToEvalStack (CliType.ValueType vt) currentThread state
                |> IlMachineState.advanceProgramCounter currentThread

            Some state
        | "System.Private.CoreLib", "Unsafe", "AsPointer" ->
            // Method signature: 1 generic parameter, we take a Byref of that parameter, and return a TypeDefn.Pointer(Void)
            let arg, state = IlMachineState.popEvalStack currentThread state

            let toPush =
                match arg with
                | EvalStackValue.ManagedPointer ptr ->
                    match ptr with
                    | ManagedPointerSource.LocalVariable (sourceThread, methodFrame, whichVar) ->
                        CliRuntimePointer.Managed (
                            CliRuntimePointerSource.LocalVariable (sourceThread, methodFrame, whichVar)
                        )
                    | ManagedPointerSource.Argument (sourceThread, methodFrame, whichVar) ->
                        CliRuntimePointer.Managed (
                            CliRuntimePointerSource.Argument (sourceThread, methodFrame, whichVar)
                        )
                    | ManagedPointerSource.Heap managedHeapAddress ->
                        CliRuntimePointer.Managed (CliRuntimePointerSource.Heap managedHeapAddress)
                    | ManagedPointerSource.Null -> failwith "todo"
                    | ManagedPointerSource.ArrayIndex _ -> failwith "TODO"
                | x -> failwith $"TODO: Unsafe.AsPointer(%O{x})"

            IlMachineState.pushToEvalStack (CliType.RuntimePointer toPush) currentThread state
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "BitConverter", "SingleToInt32Bits" ->
            let arg, state = IlMachineState.popEvalStack currentThread state

            let result =
                match arg with
                | EvalStackValue.Float f -> BitConverter.SingleToInt32Bits (float32<float> f) |> EvalStackValue.Int32
                | _ -> failwith "TODO"

            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "BitConverter", "Int32BitsToSingle" ->
            let arg, state = IlMachineState.popEvalStack currentThread state

            let arg =
                match arg with
                | EvalStackValue.Int32 i -> i
                | _ -> failwith "$TODO: {arr}"

            let result =
                BitConverter.Int32BitsToSingle arg |> CliNumericType.Float32 |> CliType.Numeric

            state
            |> IlMachineState.pushToEvalStack result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "BitConverter", "Int64BitsToDouble" ->
            let arg, state = IlMachineState.popEvalStack currentThread state

            let arg =
                match arg with
                | EvalStackValue.Int64 i -> i
                | _ -> failwith "$TODO: {arr}"

            let result =
                BitConverter.Int64BitsToDouble arg |> CliNumericType.Float64 |> CliType.Numeric

            state
            |> IlMachineState.pushToEvalStack result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "BitConverter", "DoubleToInt64Bits" ->
            let arg, state = IlMachineState.popEvalStack currentThread state

            let result =
                match arg with
                | EvalStackValue.Float f -> BitConverter.DoubleToInt64Bits f |> EvalStackValue.Int64
                | _ -> failwith "TODO"

            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Some
        | "System.Private.CoreLib", "String", "Equals" ->
            let arg1, state = IlMachineState.popEvalStack currentThread state

            let arg1 =
                match arg1 with
                | EvalStackValue.ManagedPointer (ManagedPointerSource.Heap h) -> h
                | EvalStackValue.Int32 _
                | EvalStackValue.Int64 _
                | EvalStackValue.Float _ -> failwith $"this isn't a string! {arg1}"
                | _ -> failwith $"TODO: %O{arg1}"

            let arg2, state = IlMachineState.popEvalStack currentThread state

            let arg2 =
                match arg2 with
                | EvalStackValue.ManagedPointer (ManagedPointerSource.Heap h) -> h
                | EvalStackValue.Int32 _
                | EvalStackValue.Int64 _
                | EvalStackValue.Float _ -> failwith $"this isn't a string! {arg2}"
                | _ -> failwith $"TODO: %O{arg2}"

            if arg1 = arg2 then
                state
                |> IlMachineState.pushToEvalStack (CliType.ofBool true) currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> Some
            else
                failwith "TODO"
        | "System.Private.CoreLib", "Unsafe", "ReadUnaligned" ->
            let ptr, state = IlMachineState.popEvalStack currentThread state

            let v : CliType =
                let rec go ptr =
                    match ptr with
                    | EvalStackValue.ManagedPointer src ->
                        match src with
                        | ManagedPointerSource.LocalVariable (sourceThread, methodFrame, whichVar) -> failwith "todo"
                        | ManagedPointerSource.Argument (sourceThread, methodFrame, whichVar) -> failwith "todo"
                        | ManagedPointerSource.Heap managedHeapAddress -> failwith "todo"
                        | ManagedPointerSource.ArrayIndex (arr, index) ->
                            state |> IlMachineState.getArrayValue arr index
                        | ManagedPointerSource.Null -> failwith "TODO: throw NRE"
                    | EvalStackValue.NativeInt src -> failwith "TODO"
                    | EvalStackValue.ObjectRef ptr -> failwith "TODO"
                    | EvalStackValue.UserDefinedValueType [ _, field ] -> go field
                    | EvalStackValue.UserDefinedValueType []
                    | EvalStackValue.UserDefinedValueType (_ :: _ :: _)
                    | EvalStackValue.Int32 _
                    | EvalStackValue.Int64 _
                    | EvalStackValue.Float _ -> failwith $"this isn't a pointer! {ptr}"

                go ptr

            let state =
                state
                |> IlMachineState.pushToEvalStack v currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            Some state
        | "System.Private.CoreLib", "String", "op_Implicit" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ par ], ret ->
                let par = state.ConcreteTypes |> AllConcreteTypes.lookup par |> Option.get
                let ret = state.ConcreteTypes |> AllConcreteTypes.lookup ret |> Option.get

                if
                    par.Namespace = "System"
                    && par.Name = "String"
                    && ret.Namespace = "System"
                    && ret.Name = "ReadOnlySpan`1"
                then
                    match ret.Generics |> Seq.toList with
                    | [ gen ] ->
                        let gen = state.ConcreteTypes |> AllConcreteTypes.lookup gen |> Option.get

                        if gen.Namespace = "System" && gen.Name = "Char" then
                            // This is just an optimisation
                            // https://github.com/dotnet/runtime/blob/ab105b51f8b50ec5567d7cfe9001ca54dd6f64c3/src/libraries/System.Private.CoreLib/src/System/String.cs#L363-L366
                            None
                        else
                            failwith "TODO: unexpected params to String.op_Implicit"
                    | _ -> failwith "TODO: unexpected params to String.op_Implicit"
                else
                    failwith "TODO: unexpected params to String.op_Implicit"
            | _ -> failwith "TODO: unexpected params to String.op_Implicit"
        | a, b, c -> failwith $"TODO: implement JIT intrinsic {a}.{b}.{c}"
        |> Option.map (fun s -> s.WithThreadSwitchedToAssembly callerAssy currentThread |> fst)
