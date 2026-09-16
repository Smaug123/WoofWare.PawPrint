namespace WoofWare.PawPrint

open System

[<RequireQualifiedAccess>]
module NativeString =
    /// Allocate a blank `String` of the given length and push the heap reference.
    /// `length` must be non-negative; the caller is responsible for rejecting the lengths on
    /// which CoreCLR raises.
    let private allocateAndPushBlankString
        (ctx : NativeCallContext)
        (length : int)
        (state : IlMachineState)
        : IlMachineState
        =
        if length < 0 then
            failwith $"BUG: allocateAndPushBlankString reached with negative length %d{length}"

        let contents = String (char 0, length)

        let addr, state =
            IlMachineState.allocateManagedString ctx.LoggerFactory ctx.BaseClassTypes contents state

        state
        |> IlMachineState.pushToEvalStack (CliType.ObjectRef (Some addr)) ctx.Thread

    /// Decode an `nint`-typed length argument. Reject negatives and
    /// values that exceed `Int32.MaxValue`; CoreCLR throws OOM in both cases, but
    /// we don't yet plumb guest exceptions through here.
    let private nintLengthOfArgument (operation : string) (arg : CliType) : int =
        let checkedLength (count : int64) : int =
            if count < 0L then
                failwith $"TODO: %s{operation} with negative length %d{count} should throw OutOfMemoryException"

            if count > int64 System.Int32.MaxValue then
                failwith
                    $"TODO: %s{operation} with length %d{count} exceeding Int32.MaxValue should throw OutOfMemoryException"

            int count

        // CoreLib's callers widen an `int` length to `nint` with `Conv.I`, which materialises as
        // `NativeInt.Verbatim`, but accept the broader set of representations that other
        // nint-length sites already produce so a future BCL refactor doesn't silently degrade.
        // Mirrors `NativeBuffer.byteCountOfArgument`.
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim count)) -> checkedLength count
        | CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim count)) -> checkedLength count
        | CliType.Numeric (CliNumericType.Int32 count) -> checkedLength (int64 count)
        | other -> failwith $"%s{operation}: expected nint length, got %O{other}"

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
          "System",
          "String",
          "FastAllocateString",
          [ ConcretePointer (CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                                              "MethodTable",
                                                              methodTableGenerics))
            ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.String) when
            methodTableGenerics.IsEmpty
            ->
            // .NET 10 InternalCall: `FastAllocateString(MethodTable* pMT, nint length)`. This is
            // the only `FastAllocateString` that CoreLib declares extern; the `nint`-only overload
            // is a managed wrapper which calls it with `TypeHandleOf<string>().AsMethodTable()`.
            // CoreCLR uses pMT as the allocation type; we only ever expect System.String here,
            // since that's what the wrapper passes — fail loudly if anything else surfaces.
            // https://github.com/dotnet/runtime/blob/v10.0.7/src/coreclr/System.Private.CoreLib/src/System/String.CoreCLR.cs#L27-L34
            let operation = "String.FastAllocateString(MethodTable*, nint)"

            if instruction.Arguments.Length <> 2 then
                failwith
                    $"%s{operation}: expected two native arguments after matching signature, got %d{instruction.Arguments.Length}"

            let methodTableHandle =
                NativeCall.methodTableOfEvalStackValue operation (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            match methodTableHandle with
            | ConcretePrimitive state.ConcreteTypes PrimitiveType.String -> ()
            | other -> failwith $"%s{operation}: expected MethodTable for System.String, got %O{other}"

            let length = nintLengthOfArgument operation instruction.Arguments.[1]

            state
            |> allocateAndPushBlankString ctx length
            |> fun state -> NativeHandlerResult.completed state
            |> Some
        | _ -> None
