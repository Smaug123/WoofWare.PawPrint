namespace WoofWare.PawPrint

open WoofWare.PawPrint.ExternImplementations

[<RequireQualifiedAccess>]
module NativeMonitor =
    /// Match Monitor's nested int32 enums (`EnterHelperResult`, `LeaveHelperAction`).
    /// They live in CoreLib with empty namespace (nested types) and matching simple name.
    let private (|MonitorNestedEnum|_|) (concreteTypes : AllConcreteTypes) (enumName : string) handle =
        match handle with
        | ConcreteType concreteTypes (asm, "", name, generics) when
            asm = "System.Private.CoreLib" && name = enumName && generics.IsEmpty
            ->
            Some ()
        | _ -> None

    let tryExecute (ctx : NativeCallContext) : ExecutionResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "System.Private.CoreLib",
          "System.Threading",
          "Monitor",
          "TryEnter_FastPath",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Object ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) ->
            System_Threading_Monitor.TryEnter_FastPath ctx.BaseClassTypes ctx.Thread state
            |> Some
        | "System.Private.CoreLib",
          "System.Threading",
          "Monitor",
          "TryEnter_FastPath_WithTimeout",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Object
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (MonitorNestedEnum state.ConcreteTypes "EnterHelperResult") ->
            System_Threading_Monitor.TryEnter_FastPath_WithTimeout ctx.BaseClassTypes ctx.Thread state
            |> Some
        | "System.Private.CoreLib",
          "System.Threading",
          "Monitor",
          "Exit_FastPath",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Object ],
          MethodReturnType.Returns (MonitorNestedEnum state.ConcreteTypes "LeaveHelperAction") ->
            System_Threading_Monitor.Exit_FastPath ctx.BaseClassTypes ctx.Thread state
            |> Some
        | "System.Private.CoreLib",
          "System.Threading",
          "Monitor",
          "IsEnteredNative",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Object ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) ->
            System_Threading_Monitor.IsEnteredNative ctx.BaseClassTypes ctx.Thread state
            |> Some
        | _ -> None

    /// Decode the ManagedHeapAddress carried by an `ObjectHandleOnStack` QCall
    /// argument. Fails loud if the handle points at a null reference or anything
    /// other than an object ref.
    let private addressFromObjectHandle
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (arg : CliType)
        : ManagedHeapAddress
        =
        let ptr = NativeCall.objectHandleOnStackTarget operation state "obj" arg

        let value = IlMachineState.readManagedByref baseClassTypes state ptr

        match value with
        | CliType.ObjectRef (Some addr) -> addr
        | CliType.ObjectRef None -> failwith $"%s{operation}: ObjectHandleOnStack pointed to a null object reference"
        | other -> failwith $"%s{operation}: expected ObjectRef in ObjectHandleOnStack, got %O{other}"

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
        | "Monitor_Wait",
          "System.Private.CoreLib",
          "System.Threading",
          "Monitor",
          _,
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              objectHandleGenerics)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            objectHandleGenerics.IsEmpty
            ->
            // .NET 10 QCall: Monitor.Wait(ObjectHandleOnStack obj, int millisecondsTimeout) -> bool.
            // Caller has already null-checked `obj` and clamped `timeout >= -1` in the managed
            // wrapper. The return bool is `true` if a Pulse/PulseAll signalled us, `false` on
            // timeout. We have no virtual clock, so finite non-zero timeouts fail loud; only
            // -1 (Infinite) is supported. We always push `true` because in PawPrint a waiter
            // can only be woken by a Pulse/PulseAll/spurious wake — never by a timeout.
            let operation = "Monitor_Wait"

            if instruction.Arguments.Length <> 2 then
                failwith $"%s{operation}: expected two native arguments, got %d{instruction.Arguments.Length}"

            let addr =
                addressFromObjectHandle operation ctx.BaseClassTypes state instruction.Arguments.[0]

            let timeout =
                match instruction.Arguments.[1] |> CliType.unwrapPrimitiveLike with
                | CliType.Numeric (CliNumericType.Int32 i) -> i
                | other -> failwith $"%s{operation}: expected int32 timeout, got %O{other}"

            if timeout <> System.Threading.Timeout.Infinite then
                // CoreCLR honours finite timeouts; PawPrint has no virtual clock yet. Silently
                // treating finite timeouts as Infinite would hide guest bugs that depend on
                // timeout-based wakeups. Same envelope as `LowLevelMonitor.timedWait` and
                // `TryEnter_FastPath_WithTimeout`.
                failwith
                    $"TODO: Monitor_Wait with non-Infinite timeout %d{timeout}ms requires a virtual clock; not yet implemented"

            let state = SyncBlockMonitor.wait ctx.Thread addr state

            // LibraryImport's `[return: MarshalAs(UnmanagedType.Bool)]` produces a 4-byte BOOL
            // in the PInvoke signature, so the QCall returns int32 (not the managed bool ABI).
            // Push 1: when the scheduler resumes this thread, the IL pointer is already past
            // this call site and the return value is consumed by the caller as "signal received".
            // Spurious wakeups also return 1 (matches CoreCLR's contract: spurious wakeups are
            // documented; the boolean return is `signalReceived`, which is `true` for any
            // non-timeout exit).
            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 1)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some

        | "Monitor_Pulse",
          "System.Private.CoreLib",
          "System.Threading",
          "Monitor",
          _,
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              objectHandleGenerics) ],
          MethodReturnType.Void when objectHandleGenerics.IsEmpty ->
            // .NET 10 QCall: Monitor.Pulse(ObjectHandleOnStack obj) -> void.
            let operation = "Monitor_Pulse"

            if instruction.Arguments.Length <> 1 then
                failwith $"%s{operation}: expected one native argument, got %d{instruction.Arguments.Length}"

            let addr =
                addressFromObjectHandle operation ctx.BaseClassTypes state instruction.Arguments.[0]

            let state = SyncBlockMonitor.pulse ctx.Thread addr state
            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some

        | "Monitor_PulseAll",
          "System.Private.CoreLib",
          "System.Threading",
          "Monitor",
          _,
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              objectHandleGenerics) ],
          MethodReturnType.Void when objectHandleGenerics.IsEmpty ->
            // .NET 10 QCall: Monitor.PulseAll(ObjectHandleOnStack obj) -> void.
            let operation = "Monitor_PulseAll"

            if instruction.Arguments.Length <> 1 then
                failwith $"%s{operation}: expected one native argument, got %d{instruction.Arguments.Length}"

            let addr =
                addressFromObjectHandle operation ctx.BaseClassTypes state instruction.Arguments.[0]

            let state = SyncBlockMonitor.pulseAll ctx.Thread addr state
            (state, WhatWeDid.Executed) |> ExecutionResult.stepped |> Some

        | _ -> None
