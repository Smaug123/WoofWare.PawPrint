namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata

/// <summary>
/// Reading the IL body of a method minted by <c>Reflection.Emit</c> back out of the managed
/// <c>DynamicResolver</c> that holds it.
/// </summary>
/// <remarks>
/// <para>
/// CoreCLR reaches this data through <c>DynamicResolver</c>'s <c>GetCodeInfo</c>,
/// <c>GetLocalsSignature</c> and <c>GetEHInfo</c>, which the JIT calls back into managed code to
/// run. PawPrint reads the fields those methods return instead. That is not a shortcut around a
/// managed call: <c>DynamicResolver</c>'s constructor stores every one of them
/// (<c>DynamicILGenerator.cs</c>, <c>m_code</c>/<c>m_localSignature</c>/<c>m_exceptions</c>/
/// <c>m_stackSize</c>), and the accessors are projections of those fields and nothing else, so the
/// fields *are* the data. Reading them keeps this a pure function of the heap, where calling the
/// accessors would mean suspending the interpreter mid-QCall for a managed re-entry.
/// </para>
/// <para>
/// Safe to do eagerly at <c>ModuleHandle_GetDynamicMethod</c> because the resolver reaching that
/// QCall is already complete: <c>DynamicMethod.GetMethodDescriptor</c> constructs the resolver —
/// which is what bakes the IL, via <c>DynamicILGenerator.BakeByteArray</c> — and only then calls
/// the QCall with it.
/// </para>
/// <para>
/// With exactly one exception, which is why <see cref="readInitLocals"/> is separate from
/// <see cref="read"/>: <c>initLocals</c> does not live on the resolver at all. It is read back off
/// the <c>DynamicMethod</c> whenever <c>GetCodeInfo</c> is called, which is at first JIT, and its
/// setter goes on working until then. Reading it here would capture a value the guest is still
/// entitled to change.
/// </para>
/// </remarks>
[<RequireQualifiedAccess>]
module internal DynamicMethodBody =

    /// The type whose fields this module knows how to read. Checked rather than assumed: the
    /// QCall's `resolver` argument arrives as a bare `ObjectHandleOnStack`, so nothing upstream
    /// has established what is in it, and reading `m_code` off some other type would either fail
    /// obscurely or — for a type that happens to have a field of that name — succeed wrongly.
    let private resolverTypeName = "System.Reflection.Emit", "DynamicResolver"

    let private requireObject
        (operation : string)
        (what : string)
        (state : IlMachineState)
        (value : CliType)
        : ManagedHeapAddress option
        =
        match CliType.unwrapPrimitiveLikeDeep value with
        | CliType.ObjectRef target -> target
        | other -> failwith $"%s{operation}: expected %s{what} to be an object reference, got %O{other}"

    /// The contents of a managed `byte[]`, in order.
    let private readByteArray
        (operation : string)
        (what : string)
        (state : IlMachineState)
        (addr : ManagedHeapAddress)
        : byte[]
        =
        let shape = ManagedHeap.getArrayShape addr state.ManagedHeap

        if shape.Lengths.Length <> 1 then
            failwith
                $"%s{operation}: expected %s{what} to be a single-dimensional array, got rank %d{shape.Lengths.Length}"

        Array.init
            shape.Length
            (fun i ->
                match
                    ManagedHeap.getArrayValue addr i state.ManagedHeap
                    |> CliType.unwrapPrimitiveLikeDeep
                with
                | CliType.Numeric (CliNumericType.UInt8 b) -> b
                | CliType.Numeric (CliNumericType.Int8 b) -> byte b
                | other -> failwith $"%s{operation}: expected %s{what}[%d{i}] to be a byte, got %O{other}"
            )

    /// Whether this instruction carries an operand drawn from the method's token universe.
    ///
    /// For a dynamic method that universe is the `DynamicScope` attached to the resolver — a
    /// `List<object?>` handing out operands of the form `index | tokenTypeTag` — and *not* the
    /// metadata of the assembly the method is scoped to. The two are indistinguishable by
    /// inspection: a scope operand is a perfectly well-formed `MethodDef`/`TypeDef`/`String`
    /// token that names an unrelated real row. So a body carrying one cannot be stored until
    /// those operands can be resolved against the scope, and until then it is refused here
    /// rather than decoded into something that would silently execute against the wrong rows.
    let private carriesToken (op : IlOp) : bool =
        match op with
        | IlOp.UnaryMetadataToken _
        | IlOp.UnaryStringToken _ -> true
        | IlOp.Nullary _
        | IlOp.UnaryConst _
        | IlOp.Switch _ -> false

    /// <summary>
    /// The `initLocals` a dynamic method would be compiled with if it were compiled *now*, read
    /// off the `DynamicMethod` the resolver was built for.
    /// </summary>
    /// <remarks>
    /// <para>
    /// This is `DynamicResolver.GetCodeInfo`'s `initLocals = (m_method.InitLocals) ? 1 : 0;`
    /// (`DynamicILGenerator.cs:729`) and nothing else. Everything else that method reports was
    /// frozen when the resolver was constructed, which is why the rest of this module can run at
    /// mint time and this cannot: `DynamicMethod.InitLocals`' setter never latches, so the value
    /// is whatever the guest last assigned.
    /// </para>
    /// <para>
    /// The caller is responsible for latching what this returns
    /// (`MethodHandleRegistry.latchInitLocals`); on its own this is a plain read of current state,
    /// and calling it twice around a guest assignment will give two different answers.
    /// </para>
    /// </remarks>
    let readInitLocals (operation : string) (state : IlMachineState) (resolver : ManagedHeapAddress) : bool =
        let obj = ManagedHeap.get resolver state.ManagedHeap

        let method =
            AllocatedNonArrayObject.DereferenceField "m_method" obj
            |> requireObject operation "m_method" state
            |> Option.defaultWith (fun () ->
                failwith
                    $"%s{operation}: the resolver's m_method is null, but DynamicResolver's constructor assigns it from the ILGenerator's method builder"
            )

        let methodObj = ManagedHeap.get method state.ManagedHeap

        match
            AllocatedNonArrayObject.DereferenceField "_initLocals" methodObj
            |> CliType.unwrapPrimitiveLikeDeep
        with
        | CliType.Numeric (CliNumericType.Int32 v) -> v <> 0
        | CliType.Bool v -> v <> 0uy
        | other -> failwith $"%s{operation}: expected DynamicMethod._initLocals to be a bool, got %O{other}"

    /// <summary>
    /// The body held by the <c>DynamicResolver</c> at <paramref name="resolver" />.
    /// </summary>
    /// <param name="scopeAssembly">
    /// The assembly the dynamic method is scoped to, used to resolve the local signature's type
    /// references. This is the module `DynamicScope` belongs to, which is the module
    /// `SignatureHelper` would have spelled a type against had it had one.
    /// </param>
    let read
        (operation : string)
        (state : IlMachineState)
        (scopeAssembly : DumpedAssembly)
        (resolver : ManagedHeapAddress)
        : MintedDynamicMethodBody
        =
        let obj = ManagedHeap.get resolver state.ManagedHeap

        let resolverType =
            AllConcreteTypes.lookup obj.ConcreteType state.ConcreteTypes
            |> Option.defaultWith (fun () ->
                failwith $"%s{operation}: the resolver's type %O{obj.ConcreteType} is not concretized"
            )

        let expectedNamespace, expectedName = resolverTypeName

        if resolverType.Namespace <> expectedNamespace || resolverType.Name <> expectedName then
            failwith
                $"%s{operation}: expected the resolver to be a %s{expectedNamespace}.%s{expectedName}, got %s{resolverType.Namespace}.%s{resolverType.Name}"

        let field (name : string) : CliType =
            AllocatedNonArrayObject.DereferenceField name obj

        // `m_exceptionHeader` is non-null only on the `DynamicILInfo` path, where the caller
        // supplied a fat/thin EH *blob* rather than building clauses through `ILGenerator`.
        // `GetCodeInfo` reads the clause count out of that blob and `GetEHInfo` reads the clauses
        // from `m_exceptions`, which that path leaves null — so the two are genuinely different
        // sources and an implementation that read only one would silently lose every clause of
        // the other. Only the `ILGenerator` path is supported here; refuse the other by name.
        match field "m_exceptionHeader" |> requireObject operation "m_exceptionHeader" state with
        | None -> ()
        | Some _ ->
            failwith
                $"TODO: %s{operation} was given a resolver built through DynamicILInfo, whose exception clauses arrive as a fat/thin EH blob in m_exceptionHeader rather than as __ExceptionInfo records; PawPrint reads only the ILGenerator path"

        // Exception regions are refused wholesale for now rather than decoded: a `catch` clause's
        // type arrives as a `DynamicScope` index in `ClassTokenOrFilterOffset`, so it has exactly
        // the token problem `carriesToken` refuses bodies for, and `ExceptionRegion.Catch` has
        // nowhere to put anything else. `m_exceptions` is null when `ILGenerator` saw no `try`.
        match field "m_exceptions" |> requireObject operation "m_exceptions" state with
        | None -> ()
        | Some addr ->
            let shape = ManagedHeap.getArrayShape addr state.ManagedHeap

            if shape.Length <> 0 then
                failwith
                    $"TODO: %s{operation} was given a dynamic method with %d{shape.Length} exception region(s); a catch clause's type is a DynamicScope index, which PawPrint cannot yet resolve"

        let code =
            match field "m_code" |> requireObject operation "m_code" state with
            | Some addr -> readByteArray operation "m_code" state addr
            | None ->
                failwith
                    $"%s{operation}: the resolver's m_code is null, but DynamicResolver's constructor assigns it from BakeByteArray and DynamicMethod.GetMethodDescriptor refuses an empty ILGenerator before reaching this QCall"

        // Decoded against the scope assembly's name, which is a placeholder and is why the very
        // next thing done with the result is to refuse any instruction that kept it. Nothing that
        // claims that universe escapes this function.
        let instructions = IlDecoding.decodeInstructions scopeAssembly.Name code

        match instructions |> List.tryFind (fst >> carriesToken) with
        | Some (op, offset) ->
            failwith
                $"TODO: %s{operation} was given a dynamic method whose IL carries a token operand (%O{op} at IL_%04x{offset}); those name entries in the method's DynamicScope, not rows in %s{scopeAssembly.Name.Name}, and PawPrint cannot yet resolve them"
        | None -> ()

        let localVars =
            match field "m_localSignature" |> requireObject operation "m_localSignature" state with
            | Some addr ->
                let blob = readByteArray operation "m_localSignature" state addr

                LocalSignatureDecoding.decode scopeAssembly.Name (scopeAssembly.PeReader.GetMetadataReader ()) blob
                |> Some
            | None ->
                failwith
                    $"%s{operation}: the resolver's m_localSignature is null, but DynamicResolver's constructor assigns it from SignatureHelper.InternalGetSignatureArray, which always returns at least the calling-convention byte"

        // No `initLocals` here: it is not knowable yet. `readInitLocals` above answers it, at
        // first execution, and `MethodHandleRegistry.latchInitLocals` fixes it there.
        MintedDynamicMethodBody.make instructions localVars ImmutableArray.Empty
