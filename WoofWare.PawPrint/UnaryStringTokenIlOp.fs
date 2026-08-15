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

    /// <summary>
    /// The guest <c>System.String</c> object that entry <paramref name="scopeIndex"/> of the
    /// executing method's <c>DynamicScope</c> holds.
    /// </summary>
    /// <remarks>
    /// Reached through the executing method rather than through the operand, because the operand is
    /// an <see cref="IlOp"/> and must not carry a heap address. A dynamic method's
    /// <c>MethodInfo</c> is <c>Synthesised</c> with a <c>SynthesisedMethod.DynamicMethod</c> kind
    /// carrying its <c>DynamicMethodHandle</c> — that handle is the method's identity precisely
    /// because every dynamic method in a module shares one owner — so the body, and with it the
    /// scope's string objects, is one registry lookup away.
    /// </remarks>
    let private scopeStringObject (scopeIndex : int) (state : IlMachineState) (thread : ThreadId) : ManagedHeapAddress =
        let executing = state.ThreadState.[thread].MethodState.ExecutingMethod

        let handle =
            match executing.SynthesisedKind with
            | Some (SynthesisedMethod.DynamicMethod handle) -> handle
            | _ ->
                failwith
                    $"ldstr names DynamicScope entry %d{scopeIndex}, but the executing method %s{executing.Name} is not a dynamic method; only a body read off a DynamicResolver can carry a scope operand"

        let definition =
            MethodHandleRegistry.resolveDynamicMethod handle state.MethodHandles
            |> Option.defaultWith (fun () ->
                failwith $"ldstr: %O{handle} is executing but is not registered in the method-handle registry"
            )

        (definition.GetBody ()).ScopeStrings
        |> Map.tryFind scopeIndex
        |> Option.defaultWith (fun () ->
            failwith
                $"ldstr: %O{handle} names DynamicScope entry %d{scopeIndex}, but no string object was recorded for it; the decoder builds this operand only for entries the scope reader classified as strings, so the two have gone out of step"
        )

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
            let value, candidate =
                match operand with
                | StringOperand.FromMetadata sh ->
                    let value =
                        match state.LoadedAssembly sh.SourceAssembly with
                        | Some assy -> assy.Strings sh.Token
                        | None ->
                            let available = state._LoadedAssemblies.DefinitionNames |> String.concat " ; "

                            failwith
                                $"Tried to resolve ldstr token %O{sh.Token} from assembly {sh.SourceAssembly.FullName}, but only had the following available: {available}"

                    value, None
                | StringOperand.FromDynamicScope scopeIndex ->
                    let addr = scopeStringObject scopeIndex state thread

                    // Read now, not when the method was minted. Real .NET reads the scope entry's
                    // characters when it materialises the literal, and a guest can mutate a
                    // `System.String`'s data in place through an unsafe pointer between emitting it
                    // and running the method; measured on real .NET, the mutated value is the one
                    // that gets interned.
                    let value =
                        ManagedHeap.getStringContents addr state.ManagedHeap
                        |> Option.defaultWith (fun () ->
                            failwith
                                $"ldstr: the DynamicScope string at %O{addr} has no recorded contents; every string a guest can hand to ILGenerator.Emit was allocated through allocateManagedString, which records them"
                        )

                    value, Some addr

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
