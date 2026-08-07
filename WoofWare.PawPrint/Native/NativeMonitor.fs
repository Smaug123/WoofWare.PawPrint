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

    let tryExecute (ctx : NativeCallContext) : NativeHandlerResult option =
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
            |> NativeHandlerResult.ofExecutionResult
            |> Some
        | "System.Private.CoreLib",
          "System.Threading",
          "Monitor",
          "TryEnter_FastPath_WithTimeout",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Object
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (MonitorNestedEnum state.ConcreteTypes "EnterHelperResult") ->
            System_Threading_Monitor.TryEnter_FastPath_WithTimeout ctx.BaseClassTypes ctx.Thread state
            |> NativeHandlerResult.ofExecutionResult
            |> Some
        | "System.Private.CoreLib",
          "System.Threading",
          "Monitor",
          "Exit_FastPath",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Object ],
          MethodReturnType.Returns (MonitorNestedEnum state.ConcreteTypes "LeaveHelperAction") ->
            System_Threading_Monitor.Exit_FastPath ctx.BaseClassTypes ctx.Thread state
            |> NativeHandlerResult.ofExecutionResult
            |> Some
        | "System.Private.CoreLib",
          "System.Threading",
          "Monitor",
          "IsEnteredNative",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Object ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) ->
            System_Threading_Monitor.IsEnteredNative ctx.BaseClassTypes ctx.Thread state
            |> NativeHandlerResult.ofExecutionResult
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

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
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
            // wrapper. LibraryImport's `[return: MarshalAs(UnmanagedType.Bool)]` produces a
            // 4-byte BOOL in the PInvoke signature, so the QCall returns int32 (not the
            // managed bool ABI): non-zero means signalled, zero means timed out.
            //
            // The call always parks the thread on the SyncBlock's WaitQueue; there is no
            // fast path. Push the optimistic `Int32 1` (signalled) *before* parking, mirroring
            // `SystemNative_LowLevelMonitor_TimedWait`: when the scheduler resumes this
            // thread, the IL site is already past this call and the pushed value is consumed
            // as the return. If the deadline fires first, `SyncBlockMonitor.fireWaitTimeout`
            // rewrites the slot to `Int32 0` (timed out). Pulse/PulseAll/spurious wakes leave
            // the optimistic `1` in place — matching CoreCLR's contract that the boolean
            // return is `signalReceived`, which is `true` for any non-timeout exit.
            let operation = "Monitor_Wait"

            if instruction.Arguments.Length <> 2 then
                failwith $"%s{operation}: expected two native arguments, got %d{instruction.Arguments.Length}"

            let addr =
                addressFromObjectHandle operation ctx.BaseClassTypes state instruction.Arguments.[0]

            let timeout =
                match instruction.Arguments.[1] |> CliType.unwrapPrimitiveLike with
                | CliType.Numeric (CliNumericType.Int32 i) -> i
                | other -> failwith $"%s{operation}: expected int32 timeout, got %O{other}"

            let deadlineMs =
                if timeout = System.Threading.Timeout.Infinite then
                    // The managed `Monitor.Wait(obj)` overload routes through the same QCall
                    // with `millisecondsTimeout = -1`; an infinite wait has no deadline and
                    // wakes only via Pulse/PulseAll/spurious-wake.
                    None
                elif timeout < 0 then
                    // `< -1` is rejected by the BCL wrapper (`Monitor.Wait(obj, int)`)
                    // before reaching the QCall, so reaching here means the wrapper was
                    // bypassed and the caller meant something we cannot infer. A silent
                    // treat-as-infinite or treat-as-zero would turn a guest bug into a
                    // different bug elsewhere.
                    failwith
                        $"%s{operation}: negative timeout %d{timeout} ms is not Infinite (-1); the BCL's Monitor.Wait(obj, int) validates this argument before the QCall, so reaching here means the wrapper was bypassed."
                else
                    // `timeout = 0` is legal: it parks then immediately fires on the next
                    // driver tick's `fireExpiredDeadlines` pass — observably an immediate
                    // timeout. `int64` keeps the addition safe for `Int32.MaxValue`
                    // timeouts against a long-running clock.
                    Some (state.Kernel.VirtualClockMs + int64 timeout)

            // Push the optimistic `Int32 1` (signalled) onto the calling thread's eval stack
            // *before* parking. Park flips the thread's status; the IL site advances past
            // Monitor_Wait when the native handler returns Stepped/Executed, so the pushed
            // value sits on the parked thread's frame stack until it's eventually woken —
            // at which point either the value is correct as-is (signal/spurious wake) or it
            // was rewritten to `Int32 0` by `SyncBlockMonitor.fireWaitTimeout` (deadline wake).
            let state =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 1)) ctx.Thread

            let state = SyncBlockMonitor.wait ctx.Thread addr deadlineMs state

            NativeHandlerResult.completed state |> Some

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
            NativeHandlerResult.completed state |> Some

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
            NativeHandlerResult.completed state |> Some

        | "Monitor_TryEnter_Slowpath",
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
            // .NET 10 QCall: Monitor.TryEnter_Slowpath(ObjectHandleOnStack obj, int millisecondsTimeout) -> int.
            // BCL wrapper treats the int return as bool (`0 = false`, anything else = true).
            // Reached when the fast-path returns `UseSlowPath` (positive finite timeout on a
            // contended lock). The slowpath parks with `Some deadline` and pushes the
            // optimistic `Int32 1`; deadline fire rewrites it to `Int32 0`.
            let operation = "Monitor_TryEnter_Slowpath"

            if instruction.Arguments.Length <> 2 then
                failwith $"%s{operation}: expected two native arguments, got %d{instruction.Arguments.Length}"

            let addr =
                addressFromObjectHandle operation ctx.BaseClassTypes state instruction.Arguments.[0]

            let timeout =
                match instruction.Arguments.[1] |> CliType.unwrapPrimitiveLike with
                | CliType.Numeric (CliNumericType.Int32 i) -> i
                | other -> failwith $"%s{operation}: expected int32 timeout, got %O{other}"

            let state =
                System_Threading_Monitor.TryEnter_Slowpath ctx.BaseClassTypes ctx.Thread addr timeout state

            NativeHandlerResult.completed state |> Some

        | _ -> None
