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
/// run. PawPrint reads the fields those methods return instead: <c>DynamicResolver</c>'s
/// constructor stores every one of them
/// (<c>DynamicILGenerator.cs</c>, <c>m_code</c>/<c>m_localSignature</c>/<c>m_exceptions</c>/
/// <c>m_stackSize</c>), and the accessors are projections of those fields and nothing
/// else. Reading them keeps this a pure function of the heap, where calling the
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

    /// The contents of a managed `byte[]`, in order, each as the guest wrote it: a signature blob
    /// really can hold bytes that name a type handle rather than holding a number, because
    /// `SignatureHelper.InternalAddRuntimeType` writes eight of them for any type it cannot spell as
    /// a token.
    let private readNamedByteArray
        (operation : string)
        (what : string)
        (state : IlMachineState)
        (addr : ManagedHeapAddress)
        : UInt8Source[]
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
                | CliType.Numeric (CliNumericType.Int8 b) -> UInt8Source.Verbatim (byte b)
                | other -> failwith $"%s{operation}: expected %s{what}[%d{i}] to be a byte, got %O{other}"
            )

    /// The contents of a managed `byte[]` that must hold only numbers, in order.
    let private readByteArray
        (operation : string)
        (what : string)
        (state : IlMachineState)
        (addr : ManagedHeapAddress)
        : byte[]
        =
        readNamedByteArray operation what state addr
        |> Array.mapi (fun i b -> UInt8Source.value $"%s{operation}: %s{what}[%d{i}]" b)

    /// An `int` field of a guest object.
    let private readInt32Field
        (operation : string)
        (what : string)
        (state : IlMachineState)
        (owner : ManagedHeapAddress)
        (field : string)
        : int
        =
        match
            AllocatedNonArrayObject.DereferenceField field (ManagedHeap.get owner state.ManagedHeap)
            |> CliType.unwrapPrimitiveLikeDeep
        with
        | CliType.Numeric (CliNumericType.Int32 v) -> v
        | other -> failwith $"%s{operation}: expected %s{what}.%s{field} to be an int32, got %O{other}"

    /// The contents of an `int[]` field of a guest object, in order.
    let private readInt32ArrayField
        (operation : string)
        (what : string)
        (state : IlMachineState)
        (owner : ManagedHeapAddress)
        (field : string)
        : int[]
        =
        let addr =
            AllocatedNonArrayObject.DereferenceField field (ManagedHeap.get owner state.ManagedHeap)
            |> requireObject operation $"%s{what}.%s{field}" state
            |> Option.defaultWith (fun () ->
                failwith
                    $"%s{operation}: %s{what}.%s{field} is null, but __ExceptionInfo's constructor allocates all five of its arrays"
            )

        let shape = ManagedHeap.getArrayShape addr state.ManagedHeap

        if shape.Lengths.Length <> 1 then
            failwith
                $"%s{operation}: expected %s{what}.%s{field} to be a single-dimensional array, got rank %d{shape.Lengths.Length}"

        Array.init
            shape.Length
            (fun i ->
                match
                    ManagedHeap.getArrayValue addr i state.ManagedHeap
                    |> CliType.unwrapPrimitiveLikeDeep
                with
                | CliType.Numeric (CliNumericType.Int32 v) -> v
                | other -> failwith $"%s{operation}: expected %s{what}.%s{field}[%d{i}] to be an int32, got %O{other}"
            )

    /// <summary>
    /// The exception-handling clauses of a body built through <c>ILGenerator</c>, read out of the
    /// resolver's <c>m_exceptions</c>.
    /// </summary>
    /// <remarks>
    /// <para>
    /// This is <c>DynamicResolver.GetEHInfo</c> (<c>DynamicILGenerator.cs:743-767</c>) run over
    /// every clause instead of one at a time: the JIT asks for clause <c>n</c> and that method walks
    /// the <c>__ExceptionInfo</c> array subtracting <c>GetNumberOfCatches()</c> until it lands, so
    /// the clause sequence is the concatenation, in array order, of each info's first
    /// <c>m_currentCatch</c> clauses.
    /// </para>
    /// <para>
    /// The array arrives already sorted innermost-first, by <c>GetExceptions</c>'s call to
    /// <c>SortExceptions</c> (<c>RuntimeILGenerator.cs:336</c>), which is what ECMA-335 II.25.4.6
    /// requires and what <c>ExceptionDispatching.findAcceptingClause</c>'s tie-break expects. That
    /// sort's own comment notes its <c>IsInner</c> comparison gives an arbitrary answer for two
    /// clauses that do not nest; harmless here, because dispatch re-sorts candidates by try length
    /// and non-nested try ranges are disjoint, so no pair of them is ever both candidates.
    /// </para>
    /// </remarks>
    let private readExceptionRegions
        (operation : string)
        (state : IlMachineState)
        (exceptions : ManagedHeapAddress)
        : ImmutableArray<WoofWare.PawPrint.ExceptionRegion>
        =
        // __ExceptionInfo's clause kinds (`RuntimeILGenerator.cs:1251-1255`).
        let COR_ILEXCEPTION_CLAUSE_NONE = 0x0000
        let COR_ILEXCEPTION_CLAUSE_FILTER = 0x0001
        let COR_ILEXCEPTION_CLAUSE_FINALLY = 0x0002
        let COR_ILEXCEPTION_CLAUSE_FAULT = 0x0004

        let shape = ManagedHeap.getArrayShape exceptions state.ManagedHeap

        if shape.Lengths.Length <> 1 then
            failwith
                $"%s{operation}: expected m_exceptions to be a single-dimensional array, got rank %d{shape.Lengths.Length}"

        let builder = ImmutableArray.CreateBuilder<WoofWare.PawPrint.ExceptionRegion> ()

        for i in 0 .. shape.Length - 1 do
            let what = $"m_exceptions[%d{i}]"

            let info =
                ManagedHeap.getArrayValue exceptions i state.ManagedHeap
                |> requireObject operation what state
                |> Option.defaultWith (fun () ->
                    failwith
                        $"%s{operation}: %s{what} is null, but GetExceptions copies exactly m_exceptionCount non-null entries"
                )

            let int32Field = readInt32Field operation what state info
            let int32ArrayField = readInt32ArrayField operation what state info

            let startAddr = int32Field "m_startAddr"
            let endAddr = int32Field "m_endAddr"
            let endFinally = int32Field "m_endFinally"
            // m_currentCatch and not the arrays' length: __ExceptionInfo allocates its five
            // parallel arrays four entries at a time and doubles them
            // (RuntimeILGenerator.cs:1282-1307), so the tail holds zeroes that would decode as
            // catch clauses covering [0, 0) and naming scope entry 0 — well-formed nonsense.
            let numberOfCatches = int32Field "m_currentCatch"
            let filterAddr = int32ArrayField "m_filterAddr"
            let catchAddr = int32ArrayField "m_catchAddr"
            let catchEndAddr = int32ArrayField "m_catchEndAddr"
            let clauseType = int32ArrayField "m_type"

            let bound =
                min (min filterAddr.Length catchAddr.Length) (min catchEndAddr.Length clauseType.Length)

            if numberOfCatches < 0 || numberOfCatches > bound then
                failwith
                    $"%s{operation}: %s{what} claims %d{numberOfCatches} clause(s) but its parallel arrays hold %d{bound}"

            for c in 0 .. numberOfCatches - 1 do
                let flags = clauseType.[c]

                // A finally clause takes TryLength from m_endFinally where every other kind takes
                // it from m_endAddr. Measured on a try/catch/finally, where one __ExceptionInfo
                // yields a catch covering [0,+11) and a finally covering [0,+25): the two clauses
                // of one region have different try ranges, so a projection hoisting the length out
                // of the loop is wrong.
                let tryLength =
                    if flags &&& COR_ILEXCEPTION_CLAUSE_FINALLY <> COR_ILEXCEPTION_CLAUSE_FINALLY then
                        endAddr - startAddr
                    else
                        endFinally - startAddr

                let offset =
                    {
                        TryOffset = startAddr
                        TryLength = tryLength
                        HandlerOffset = catchAddr.[c]
                        HandlerLength = catchEndAddr.[c] - catchAddr.[c]
                    }

                let region =
                    if flags = COR_ILEXCEPTION_CLAUSE_NONE then
                        // `m_filterAddr` holds this clause's *type*, as a DynamicScope token:
                        // `BeginCatchBlock` writes `GetTokenFor(rtType)` into it directly
                        // (`DynamicILGenerator.cs:371`), over the top of the slot every other clause
                        // kind uses for a filter's IL offset.
                        ExceptionRegion.Catch (
                            ExceptionCatchType.FromDynamicScope (IlDecoding.scopeIndexOf filterAddr.[c]),
                            offset
                        )
                    elif flags = COR_ILEXCEPTION_CLAUSE_FILTER then
                        ExceptionRegion.Filter (filterAddr.[c], offset)
                    elif flags = COR_ILEXCEPTION_CLAUSE_FINALLY then
                        ExceptionRegion.Finally offset
                    // Fault and PreserveStack are both 0x0004, so reading that value as Fault is
                    // only safe because MarkHelper writes nothing but None/Filter/Finally/Fault
                    // into m_type. A PreserveStack flag could only arrive through DynamicILInfo's
                    // raw EH blob, which `read` refuses by name before reaching here.
                    elif flags = COR_ILEXCEPTION_CLAUSE_FAULT then
                        ExceptionRegion.Fault offset
                    else
                        failwith
                            $"%s{operation}: %s{what} clause %d{c} has flags 0x%08x{flags}, which is none of the four __ExceptionInfo writes"

                builder.Add region

        builder.ToImmutable ()

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
    /// (`MethodHandleRegistry.latchPreparation`); on its own this is a plain read of current state,
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
    let read
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (resolver : ManagedHeapAddress)
        : MintedDynamicMethodBody
        =
        let obj = ManagedHeap.get resolver state.ManagedHeap

        let resolverType =
            AllConcreteTypes.lookup obj.ConcreteType state.TypeSystem.ConcreteTypes
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
        // from `m_exceptions`, which that path leaves null — two different sources; reading only
        // one would silently lose the other's clauses. Only the `ILGenerator` path is supported
        // here; refuse the other by name.
        match field "m_exceptionHeader" |> requireObject operation "m_exceptionHeader" state with
        | None -> ()
        | Some _ ->
            failwith
                $"TODO: %s{operation} was given a resolver built through DynamicILInfo, whose exception clauses arrive as a fat/thin EH blob in m_exceptionHeader rather than as __ExceptionInfo records; PawPrint reads only the ILGenerator path"

        // `m_exceptions` is null when `ILGenerator` saw no `try` at all: `GetExceptions` returns
        // null rather than an empty array for `m_exceptionCount = 0`.
        let exceptionRegions =
            match field "m_exceptions" |> requireObject operation "m_exceptions" state with
            | None -> ImmutableArray.Empty
            | Some addr -> readExceptionRegions operation state addr

        let code =
            match field "m_code" |> requireObject operation "m_code" state with
            | Some addr -> readByteArray operation "m_code" state addr
            | None ->
                failwith
                    $"%s{operation}: the resolver's m_code is null, but DynamicResolver's constructor assigns it from BakeByteArray and DynamicMethod.GetMethodDescriptor refuses an empty ILGenerator before reaching this QCall"

        // Decoded against the scope, not against `scopeAssembly`. A token here names an entry in
        // `m_tokens`; decoding it against the assembly's tables would silently resolve it to an
        // unrelated real row, because the bit patterns are the same. The entries themselves are not
        // read now, for an instruction's operand or for a catch clause's type: CoreCLR reads both
        // when it compiles the method at its first invocation, and a guest can rewrite the scope
        // before then. `DynamicMethodExecution.concretize` resolves the clause types at that point.
        let instructions = IlDecoding.decodeInstructions IlTokenUniverse.DynamicScope code

        for region in exceptionRegions do
            match region with
            | ExceptionRegion.Catch (ExceptionCatchType.FromMetadata token, _) ->
                failwith
                    $"BUG: %s{operation} decoded a catch clause carrying the metadata token %O{token}; a dynamic method's clause types come from its DynamicScope"
            | ExceptionRegion.Catch (ExceptionCatchType.FromDynamicScope _, _)
            | ExceptionRegion.Filter _
            | ExceptionRegion.Finally _
            | ExceptionRegion.Fault _ -> ()

        let localVars =
            match field "m_localSignature" |> requireObject operation "m_localSignature" state with
            | Some addr ->
                readNamedByteArray operation "m_localSignature" state addr
                |> ImmutableArray.CreateRange
                |> DynamicSignatureDecoding.decodeLocals (
                    InternalSignatureTypeResolution.ofHandle operation baseClassTypes state
                )
                |> Some
            | None ->
                failwith
                    $"%s{operation}: the resolver's m_localSignature is null, but DynamicResolver's constructor assigns it from SignatureHelper.InternalGetSignatureArray, which always returns at least the calling-convention byte"

        // No `initLocals` here: it is not knowable yet. `readInitLocals` above answers it, at
        // first execution, and `MethodHandleRegistry.latchPreparation` fixes it there.
        MintedDynamicMethodBody.make instructions localVars exceptionRegions
