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
          [ ConcretePointer (CorelibType state.TypeSystem.ConcreteTypes ("System.Runtime.CompilerServices",
                                                                         "MethodTable",
                                                                         methodTableGenerics))
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.IntPtr ],
          MethodReturnType.Returns (ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.String) when
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
            | ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.String -> ()
            | other -> failwith $"%s{operation}: expected MethodTable for System.String, got %O{other}"

            let length = nintLengthOfArgument operation instruction.Arguments.[1]

            state
            |> allocateAndPushBlankString ctx length
            |> fun state -> NativeHandlerResult.completed state
            |> Some
        | _ -> None

    /// What a lookup in the intern table does when no string of the value is interned.
    [<RequireQualifiedAccess>]
    type private OnInternMiss =
        /// Intern the object that was looked up, and answer it (`String_Intern`).
        | InternIt
        /// Answer null and leave the table alone (`String_IsInterned`).
        | AnswerNull

    /// <c>String_Intern</c> and <c>String_IsInterned</c> (coreclr/vm/appdomainnative.cpp), behind
    /// <c>string.Intern</c> and <c>string.IsInterned</c>, once the caller has checked the call's
    /// shape: the one native argument is the <c>StringHandleOnStack</c> through which the string
    /// arrives and the answer is returned. The table is the one <c>ldstr</c> interns literals into,
    /// so a literal and an interned non-literal of the same value are one object.
    let private lookUpInterned
        (operation : string)
        (onMiss : OnInternMiss)
        (ctx : NativeCallContext)
        : NativeHandlerResult
        =
        let state = ctx.State
        let instruction = ctx.Instruction

        if instruction.Arguments.Length <> 1 then
            failwith $"%s{operation}: expected one native argument, got %d{instruction.Arguments.Length}"

        let handle =
            NativeCall.stringHandleOnStackTarget operation state "str" instruction.Arguments.[0]
            |> ManagedPointerSource.requireAddressed

        let addr =
            match IlMachineState.readManagedByref ctx.BaseClassTypes state handle with
            | CliType.ObjectRef (Some addr) -> addr
            | CliType.ObjectRef None ->
                // CoreCLR dereferences the string unchecked; it is the managed wrapper's
                // `ArgumentNullException.ThrowIfNull` that keeps null out.
                failwith $"%s{operation}: the string handle holds null, which the managed wrapper rejects first"
            | other -> failwith $"%s{operation}: expected the string handle to hold a string reference, got %O{other}"

        // Read now rather than trusted from anywhere earlier: a guest can rewrite a string's
        // characters in place through an unsafe pointer, and CoreCLR hashes the characters the
        // object holds when it is asked.
        let value =
            match ManagedHeap.getStringContents addr state.ManagedHeap with
            | Some value -> value
            | None ->
                failwith
                    $"%s{operation}: %O{addr} has no recorded string contents; every string reaching the heap goes through allocateManagedString, which records them"

        let answer, state =
            if value.Length = 0 then
                // CoreCLR interns `string.Empty` under "" when it starts, before any guest code runs,
                // so "" is never a miss and its object is always `string.Empty`. PawPrint makes that
                // object on first use.
                let empty, state =
                    IlMachineState.internCanonicalEmptyString ctx.LoggerFactory ctx.BaseClassTypes state

                Some empty, state
            else
                match ManagedHeap.tryGetInterned value state.ManagedHeap, onMiss with
                | Some interned, _ -> Some interned, state
                | None, OnInternMiss.InternIt ->
                    // The object itself, not a copy: `GlobalStringLiteralMap::AddInternedString`
                    // (vm/stringliteralmap.cpp) stores the `STRINGREF` it is handed.
                    Some addr,
                    { state with
                        ManagedHeap = ManagedHeap.intern addr state.ManagedHeap
                    }
                | None, OnInternMiss.AnswerNull -> None, state

        let state =
            IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state handle (CliType.ObjectRef answer)

        NativeHandlerResult.completed state

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            ctx.Instruction.ExecutingMethod.Signature.ParameterTypes,
            ctx.Instruction.ExecutingMethod.Signature.ReturnType
        with
        | "String_Intern",
          "System.Private.CoreLib",
          "System",
          "String",
          [ CorelibType state.TypeSystem.ConcreteTypes ("System.Runtime.CompilerServices",
                                                        "StringHandleOnStack",
                                                        stringHandleGenerics) ],
          MethodReturnType.Void when stringHandleGenerics.IsEmpty ->
            lookUpInterned "String_Intern" OnInternMiss.InternIt ctx |> Some
        | "String_IsInterned",
          "System.Private.CoreLib",
          "System",
          "String",
          [ CorelibType state.TypeSystem.ConcreteTypes ("System.Runtime.CompilerServices",
                                                        "StringHandleOnStack",
                                                        stringHandleGenerics) ],
          MethodReturnType.Void when stringHandleGenerics.IsEmpty ->
            lookUpInterned "String_IsInterned" OnInternMiss.AnswerNull ctx |> Some
        | _ -> None
