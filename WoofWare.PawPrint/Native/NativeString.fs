namespace WoofWare.PawPrint

open System

[<RequireQualifiedAccess>]
module NativeString =
    /// Allocate a blank `String` of the given length and push the heap reference.
    let private allocateAndPushBlankString
        (ctx : NativeCallContext)
        (length : int)
        (state : IlMachineState)
        : IlMachineState
        =
        if length < 0 then
            failwith "TODO: String.FastAllocateString with negative length should throw OutOfMemoryException"

        let contents = String (char 0, length)

        let addr, state =
            IlMachineState.allocateManagedString ctx.LoggerFactory ctx.BaseClassTypes contents state

        state
        |> IlMachineState.pushToEvalStack (CliType.ObjectRef (Some addr)) ctx.Thread

    /// Decode an `nint`-typed length argument. CoreLib's .NET 10 `FastAllocateString`
    /// wrapper widens `int` to `nint` via `Conv.I`, which materialises as
    /// `NativeInt.Verbatim`, but accept the broader set of representations that other
    /// nint-length sites already produce so a future BCL refactor doesn't silently
    /// degrade. Mirrors `NativeBuffer.byteCountOfArgument`. Reject negatives and
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
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.String) ->
            if instruction.Arguments.Length <> 1 then
                failwith
                    $"String.FastAllocateString(int): expected one native argument after matching signature, got %d{instruction.Arguments.Length}"

            let length =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[0] with
                | CliType.Numeric (CliNumericType.Int32 i) -> i
                | other -> failwith $"String.FastAllocateString(int): expected int32 length, got %O{other}"

            state
            |> allocateAndPushBlankString ctx length
            |> fun state -> NativeHandlerResult.completed state
            |> Some
        | "System.Private.CoreLib",
          "System",
          "String",
          "FastAllocateString",
          [ ConcretePointer (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                               "System.Runtime.CompilerServices",
                                                               "MethodTable",
                                                               methodTableGenerics))
            ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.String) when
            methodTableGenerics.IsEmpty
            ->
            // .NET 10 InternalCall: `FastAllocateString(MethodTable* pMT, nint length)`.
            // The legacy `FastAllocateString(int)` overload is now a managed wrapper that
            // calls this one with `TypeHandleOf<string>().AsMethodTable()`. CoreCLR uses
            // pMT as the allocation type; we only ever expect System.String here, since
            // that's what the wrapper passes — fail loudly if anything else surfaces.
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
        | "System.Private.CoreLib",
          "System",
          "String",
          ".ctor",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "ReadOnlySpan`1", spanGenerics) ],
          MethodReturnType.Void when
            spanGenerics.Length = 1
            && (
                match spanGenerics.[0] with
                | ConcretePrimitive state.ConcreteTypes PrimitiveType.Char -> true
                | _ -> false
            )
            ->
            // .NET 10 InternalCall: `String..ctor(ReadOnlySpan<char> value)`. CoreCLR
            // allocates a fresh string of `value.Length` and copies the span's chars.
            // ReadOnlySpan<char> is `(ref char _reference, int _length)`; we read both
            // fields directly off the value-type argument, mirroring how Span.get_Item
            // unpacks its receiver.
            // https://github.com/dotnet/runtime/blob/v10.0.7/src/coreclr/System.Private.CoreLib/src/System/String.CoreCLR.cs
            let operation = "String..ctor(ReadOnlySpan<char>)"

            // String is a variable-size object, so `executeNewobj` allocated nothing and
            // passed no `this` (see ConstructionState.ConstructingVariableSize). The frame's
            // only argument is the user-visible ReadOnlySpan<char>.
            if instruction.Arguments.Length <> 1 then
                failwith
                    $"%s{operation}: expected 1 argument (ReadOnlySpan<char>) after matching signature, got %d{instruction.Arguments.Length}"

            let span : CliValueType =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[0] with
                | CliType.ValueType vt -> vt
                | other -> failwith $"%s{operation}: expected ReadOnlySpan<char> value type, got %O{other}"

            let length : int =
                let lengthField =
                    IlMachineState.requiredOwnInstanceFieldId state span.Declared "_length"

                match
                    CliValueType.DereferenceFieldById lengthField span
                    |> CliType.unwrapPrimitiveLikeDeep
                with
                | CliType.Numeric (CliNumericType.Int32 i) -> i
                | other -> failwith $"%s{operation}: expected _length to be int32, got %O{other}"

            let reference : ManagedPointerSource =
                let referenceField =
                    IlMachineState.requiredOwnInstanceFieldId state span.Declared "_reference"

                match
                    CliValueType.DereferenceFieldById referenceField span
                    |> CliType.unwrapPrimitiveLikeDeep
                with
                | CliType.RuntimePointer (CliRuntimePointer.Managed src) -> src
                | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer src)) -> src
                | CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L) -> ManagedPointerSource.Null
                | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)) -> ManagedPointerSource.Null
                | other -> failwith $"%s{operation}: expected _reference to be a managed byref, got %O{other}"

            // Walk per-element via `offsetManagedPointerByElements` so byte-view
            // byrefs (e.g. `stackalloc char[N]` rooted at `StackMemoryByte`) advance
            // through their canonical byte-view shape; gluing a fresh `ReinterpretAs`
            // on top would defeat the byte-view path in `readManagedByrefBytesAs`.
            let charType = spanGenerics.[0]

            let contents, state =
                if length = 0 then
                    "", state
                else
                    match reference with
                    | ManagedPointerSource.Null ->
                        failwith
                            $"TODO: %s{operation} with null _reference and non-zero length %d{length} should throw ArgumentNullException"
                    | _ ->
                        let chars = Array.zeroCreate<char> length
                        let basePtr = EvalStackValue.ManagedPointer reference

                        let mutable state = state

                        for i = 0 to length - 1 do
                            let elementPtr, state' =
                                IntrinsicHelpers.offsetManagedPointerByElements
                                    ctx.BaseClassTypes
                                    state
                                    charType
                                    i
                                    basePtr

                            state <- state'

                            let elementSrc =
                                match elementPtr with
                                | EvalStackValue.ManagedPointer src -> src
                                | other ->
                                    failwith
                                        $"%s{operation}: offsetManagedPointerByElements produced non-byref %O{other}"

                            match
                                IlMachineState.readManagedByrefBytesAs
                                    ctx.BaseClassTypes
                                    state
                                    elementSrc
                                    (CliType.ofChar (char 0))
                            with
                            | CliType.Char (high, low) -> chars.[i] <- char (int high * 256 + int low)
                            | other -> failwith $"%s{operation}: char[%d{i}] read returned non-char value %O{other}"

                        System.String chars, state

            // CoreCLR's String..ctor(ReadOnlySpan<char>) does not intern the empty
            // string the way `String..ctor(char*)` does, but PawPrint already collapses
            // empty-string allocations through `InternedStrings` so that
            // `(object)"" == (object)""` holds across the runtime; preserve that here.
            let newAddr, state =
                if contents = "" then
                    match state.InternedStrings.TryGetValue "" with
                    | true, addr -> addr, state
                    | false, _ ->
                        let addr, state =
                            IlMachineState.allocateManagedString ctx.LoggerFactory ctx.BaseClassTypes "" state

                        addr,
                        { state with
                            InternedStrings = state.InternedStrings.Add ("", addr)
                        }
                else
                    IlMachineState.allocateManagedString ctx.LoggerFactory ctx.BaseClassTypes contents state

            // Hand the freshly-allocated string back to the pending newobj: under the
            // variable-size convention the constructor is the only party that knows the
            // object's address, and `returnStackFrame` pushes it on our behalf.
            state
            |> IlMachineState.withSuppliedConstructedObject newAddr ctx.Thread
            |> fun state -> NativeHandlerResult.completed state
            |> Some
        | "System.Private.CoreLib",
          "System",
          "String",
          ".ctor",
          [ ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Char) ],
          MethodReturnType.Void ->
            let operation = "String..ctor(char*)"

            // String is a variable-size object, so `executeNewobj` allocated nothing and
            // passed no `this` (see ConstructionState.ConstructingVariableSize). The frame's
            // only argument is the user-visible char*.
            if instruction.Arguments.Length <> 1 then
                failwith
                    $"%s{operation}: expected 1 argument (char*) after matching signature, got %d{instruction.Arguments.Length}"

            let ptr =
                NativeCall.managedPointerOfPointerArgument operation "value" instruction.Arguments.[0]

            // CoreCLR's String.Ctor(char*) returns String.Empty when ptr == null
            // or the first char is NUL, rather than throwing or allocating fresh.
            // The returned reference must be the canonical empty string so that
            // `(object)new string((char*)null) == (object)""` holds, matching
            // .NET. We thread through `InternedStrings` so the address is shared
            // with `Ldstr ""` regardless of which site materialises it first.
            let contents =
                match ptr with
                | ManagedPointerSource.Null -> ""
                | _ -> NativeCall.readNullTerminatedUtf16 operation ctx.BaseClassTypes state ptr

            let newAddr, state =
                if contents = "" then
                    match state.InternedStrings.TryGetValue "" with
                    | true, addr -> addr, state
                    | false, _ ->
                        let addr, state =
                            IlMachineState.allocateManagedString ctx.LoggerFactory ctx.BaseClassTypes "" state

                        addr,
                        { state with
                            InternedStrings = state.InternedStrings.Add ("", addr)
                        }
                else
                    IlMachineState.allocateManagedString ctx.LoggerFactory ctx.BaseClassTypes contents state

            // Hand the freshly-allocated string back to the pending newobj: under the
            // variable-size convention the constructor is the only party that knows the
            // object's address, and `returnStackFrame` pushes it on our behalf.
            state
            |> IlMachineState.withSuppliedConstructedObject newAddr ctx.Thread
            |> fun state -> NativeHandlerResult.completed state
            |> Some
        | _ -> None
