namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging

/// <summary>
/// <c>ldstr</c>.
/// </summary>
/// <remarks>
/// <para>
/// PawPrint materialises a string literal — and so interns it — when the instruction *executes*.
/// Real .NET materialises every literal in a method when it *JITs* that method. The two agree on
/// value, and until dynamic methods they agreed on identity too, because a metadata literal has no
/// object until one is made for it and it made no difference when that happened. A dynamic method's
/// literal arrives with an object already attached, so which literal wins an intern slot is now
/// visible to <c>ReferenceEquals</c>, and the timing difference shows through in two places.
/// Measured on real .NET, stable across tiered/untiered/minopts:
/// </para>
/// <para>
/// A caller whose own body contains an equal literal *later* than the call has already interned it,
/// because the caller was JITted before it ran; PawPrint has not, because that instruction has not
/// executed, so the dynamic method's object wins the slot instead. And an <c>ldstr</c> whose value
/// is discarded (<c>ldstr; pop</c>) is never materialised by real .NET, where an interpreter has no
/// choice but to execute it, so PawPrint interns a literal real .NET does not.
/// </para>
/// <para>
/// Both follow from the single modelling choice above, and closing either means materialising every
/// literal of a method when the method is prepared — a change to how *all* <c>ldstr</c> works, not
/// something local to dynamic methods, and one which would make the second case worse rather than
/// better. Left as a known divergence rather than papered over; a guest can only detect it by
/// comparing references against a string it emitted itself.
/// </para>
/// </remarks>
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal UnaryStringTokenIlOp =

    let execute
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (op : UnaryStringTokenIlOp)
        (operand : StringOperand)
        (state : IlMachineState)
        (thread : ThreadId)
        : IlMachineState * WhatWeDid
        =
        match op with
        | UnaryStringTokenIlOp.Ldstr ->
            // What to intern the value *to* if nothing has interned it yet. A metadata literal has
            // no object until one is made for it, so it allocates; a dynamic method's literal
            // already has one — the object the emitting guest handed to `ILGenerator.Emit` — and
            // real .NET interns that very object. `GlobalStringLiteralMap::GetInternedString`
            // (`vm/stringliteralmap.cpp:403`) takes a `STRINGREF*` and stores it on a miss via
            // `AddInternedString` (line 431), where the metadata path beside it
            // (`AddStringLiteral(EEStringData*)`, line 396) allocates. So `ReferenceEquals` in the
            // guest can tell the two apart, and this is the difference between them.
            let resolved =
                match operand with
                | StringOperand.FromMetadata sh ->
                    let value =
                        match state.LoadedAssembly sh.SourceAssembly with
                        | Some assy -> assy.Strings sh.Token
                        | None ->
                            let available = state._LoadedAssemblies.DefinitionNames |> String.concat " ; "

                            failwith
                                $"Tried to resolve ldstr token %O{sh.Token} from assembly {sh.SourceAssembly.FullName}, but only had the following available: {available}"

                    Ok (value, None)
                | StringOperand.FromDynamicScope scopeIndex ->
                    // The decoder established at mint that this index held a string, so anything
                    // else here means the guest rewrote `m_scope.m_tokens` afterwards, and real
                    // .NET's answer is a *catchable* exception rather than a rejected method.
                    //
                    // Which exception is measured for `ldstr` specifically, and it is *not* the
                    // type path's answer. `DynamicScope.GetString` is `this[token] as string`
                    // (`DynamicILGenerator.cs:994`), so a null slot and a slot holding something
                    // that is not a string are indistinguishable — both come back null — and the
                    // JIT then embeds an indirection through a null handle. Measured, against an
                    // untouched control that returns "abcd": a null slot, a `byte[]` and a
                    // `RuntimeTypeHandle` all give NullReferenceException, and only an index
                    // exactly at the list's length gives ArgumentOutOfRangeException.
                    //
                    // The literal has to be *used* to measure this at all: `ldstr; pop` is never
                    // materialised by real .NET (the divergence documented above), so a probe that
                    // discards the value reads every rewrite as a pass.
                    let scope = DynamicScopeOperand.executingScope "ldstr" state thread

                    match DynamicScopeOperand.entryObject "ldstr" scopeIndex state scope with
                    | ScopeEntryLookup.PastEnd ->
                        Error (
                            baseClassTypes.ArgumentOutOfRangeException,
                            $"DynamicScope entry %d{scopeIndex} is exactly at the end of the scope's token list"
                        )
                    | ScopeEntryLookup.Absent ->
                        Error (
                            baseClassTypes.NullReferenceException,
                            $"DynamicScope entry %d{scopeIndex} is null, so it names no string"
                        )
                    | ScopeEntryLookup.Found addr ->

                    match ManagedHeap.tryGetObjectConcreteType addr state.ManagedHeap with
                    | None ->
                        // A live slot pointing off the heap is interpreter corruption, not a state
                        // any guest can arrange.
                        failwith $"ldstr: DynamicScope entry %d{scopeIndex} is at %O{addr}, which is not on the heap"
                    | Some concreteType when
                        not (DynamicScopeOperand.isCorelibType baseClassTypes.String state concreteType)
                        ->
                        Error (
                            baseClassTypes.NullReferenceException,
                            $"DynamicScope entry %d{scopeIndex} is not a string"
                        )
                    | Some _ ->

                    // Read now, not when the method was minted. Real .NET reads the scope entry's
                    // characters when it materialises the literal, and a guest can mutate a
                    // `System.String`'s data in place through an unsafe pointer between emitting it
                    // and running the method; measured on real .NET, the mutated value is the one
                    // that gets interned.
                    match ManagedHeap.getStringContents addr state.ManagedHeap with
                    | None ->
                        // The entry *is* a `System.String` but has no recorded contents, which
                        // breaches the invariant `allocateManagedString` maintains. Kept distinct
                        // from "not a string" above, which is a state a guest can arrange.
                        failwith
                            $"ldstr: DynamicScope entry %d{scopeIndex} is a System.String at %O{addr} whose contents were never recorded; every string reaching the heap goes through allocateManagedString, which records them"
                    | Some value -> Ok (value, Some addr)

            match resolved with
            | Error (exceptionType, why) ->
                // Don't advance the PC: exception dispatch needs the faulting instruction's offset.
                // As in `UnaryMetadataIlOp`: `why` is a PawPrint diagnostic for the log, and the
                // guest's exception carries CoreCLR's own message.
                loggerFactory.CreateLogger("Ldstr").LogWarning ("ldstr refused a DynamicScope operand: {Reason}", why)

                IlMachineStateExecution.raiseRuntimeExceptionWithMessage
                    loggerFactory
                    baseClassTypes
                    exceptionType
                    (DynamicScopeOperand.clrMessageFor baseClassTypes exceptionType)
                    thread
                    state
            | Ok (value, candidate) ->

            let addressToLoad, state =
                match state.InternedStrings.TryGetValue value with
                | true, v -> v, state
                | false, _ ->
                    let addr, state =
                        match candidate with
                        | Some addr -> addr, state
                        | None -> IlMachineState.allocateManagedString loggerFactory baseClassTypes value state

                    addr,
                    { state with
                        InternedStrings = state.InternedStrings.Add (value, addr)
                    }

            let state =
                IlMachineState.pushToEvalStack (CliType.ObjectRef (Some addressToLoad)) thread state

            state
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed
