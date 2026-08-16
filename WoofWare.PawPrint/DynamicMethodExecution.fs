namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging

/// <summary>
/// Preparing a method minted by <c>Reflection.Emit</c> for execution.
/// </summary>
/// <remarks>
/// This exists as its own step, rather than as part of
/// <see cref="ExecutionConcretization.concretizeDynamicMethod"/>, for two reasons.
///
/// The first is ordering: reading <c>initLocals</c> means reading a field off the guest's
/// <c>DynamicMethod</c> through its <c>DynamicResolver</c>, which is <c>DynamicMethodBody</c>'s
/// job, and that module is compiled long after <c>ExecutionConcretization</c>.
///
/// The second is that the read and the latch must not come apart. A caller that read
/// <c>_initLocals</c> and concretised with it, without latching, would recompile the method with
/// whatever the guest most recently assigned -- which is exactly the bug CoreCLR's
/// <c>if (!m_Code)</c> guard prevents. Exposing one function that does both means the next caller
/// to need a dynamic method frame (<c>MethodBase.Invoke</c>,
/// <c>RuntimeMethodHandle.InvokeMethod</c>) cannot get that wrong by omission.
///
/// The same is true of a <c>catch</c> clause's type, which is why that is settled here as well:
/// the clause names a <c>DynamicScope</c> index, the JIT resolves it while compiling, and a guest
/// that rewrites the slot between two invocations is measured not to be heard. Resolving it at
/// dispatch instead would hear that rewrite, and — worse — would let a body whose clause names
/// something unresolvable run happily so long as nothing ever threw, where real .NET refuses to
/// compile it at all.
///
/// "First execution" stands in for CoreCLR's "first JIT". Everything that reaches
/// <c>LCGMethodResolver::GetCodeInfo</c> does so as part of a JIT — the JIT's own
/// <c>getMethodInfo</c>, a profiler callback immediately before it, PGO hashing — and tier-up
/// re-JIT reads the cached values rather than the guest's field, so "first JIT wins" is the whole
/// of the rule. The one way to reach a first JIT without invoking anything is
/// <c>RuntimeHelpers.PrepareMethod</c>, which PawPrint does not implement for dynamic methods.
/// Whoever implements it must latch here too, or a guest could prepare a method, assign
/// <c>InitLocals</c> or rewrite a clause's scope slot, and see that take effect where real .NET
/// would ignore it.
/// </remarks>
[<RequireQualifiedAccess>]
module internal DynamicMethodExecution =

    /// <summary>
    /// The method <paramref name="handle" /> names, in the form a frame can be pushed for, latching
    /// its <c>initLocals</c> and its <c>catch</c> clause types if this is the first time it has been
    /// prepared.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The <c>Error</c> is a clause type the scope does not name — measured on real .NET as an
    /// <c>InvalidProgramException</c> raised by the first invocation, so it belongs to the *caller*
    /// and not to the method being prepared. Its payload is <c>DynamicScopeOperand.closedType</c>'s:
    /// the exception type to raise, and a diagnostic for the log.
    /// </para>
    /// <para>
    /// Nothing is latched on that path. Measured: after a first invocation that failed this way, a
    /// guest that repairs the scope slot and invokes again gets a method that compiles and runs — so
    /// a failed preparation must leave the method entirely unprepared, <c>initLocals</c> included.
    /// That is why the clause types are resolved *before* anything is written, and why
    /// <see cref="PreparedDynamicMethod"/> latches as one value.
    /// </para>
    /// </remarks>
    let concretize
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (handle : DynamicMethodHandle)
        (state : IlMachineState)
        : IlMachineState *
          Result<
              MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>,
              TypeInfo<GenericParamFromMetadata, TypeDefn> * string
           >
        =
        let definition =
            MethodHandleRegistry.resolveDynamicMethod handle state.MethodHandles
            |> Option.defaultWith (fun () ->
                failwith $"%s{operation}: %O{handle} is not registered in the method-handle registry"
            )

        // Only read the guest's state if the answer is not already fixed. Not an optimisation:
        // once latched, the resolver is no longer consulted at all, so a method whose resolver had
        // somehow gone is still executable on its second invocation, exactly as a JITted one is.
        let prepared, state =
            match definition.GetPreparation () with
            | Some latched -> Ok latched, state
            | None ->
                // Every `catch` clause of this body, resolved against the scope as it stands now.
                // This is where CoreCLR resolves them too: `DynamicResolver.GetEHInfo` hands the JIT
                // a clause's `ClassTokenOrFilterOffset` and the JIT resolves it, once, while
                // compiling — measured, by rewriting the slot between `CreateDelegate` and the first
                // call (the new type wins) and again between two calls (it does not).
                //
                // Resolving every clause, not only those an exception reaches: a body whose `catch`
                // names an open generic is an InvalidProgramException on real .NET even when it
                // never throws at all (measured). Doing this lazily, at dispatch, would let such a
                // body run.
                let clauseTypes =
                    definition.GetBody().ExceptionRegions
                    |> Seq.choose (fun region ->
                        match region with
                        | ExceptionRegion.Catch (ExceptionCatchType.FromDynamicScope index, _) -> Some index
                        | ExceptionRegion.Catch (ExceptionCatchType.FromMetadata _, _)
                        | ExceptionRegion.Filter _
                        | ExceptionRegion.Finally _
                        | ExceptionRegion.Fault _ -> None
                    )
                    |> Seq.distinct
                    |> Seq.sort
                    |> Seq.fold
                        (fun acc index ->
                            match acc with
                            | Error e -> Error e
                            | Ok acc ->
                                DynamicScopeOperand.closedType
                                    baseClassTypes
                                    $"%s{operation}: catch clause"
                                    index
                                    state
                                    handle
                                |> Result.map (fun ty -> Map.add index ty acc)
                        )
                        (Ok Map.empty)

                match clauseTypes with
                | Error e -> Error e, state
                | Ok clauseTypes ->

                let resolver =
                    definition.GetResolver ()
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"%s{operation}: %O{handle} has no DynamicResolver, so its initLocals cannot be read; ModuleHandle_GetDynamicMethod refuses a null resolver, so a registered method should always have one"
                    )

                let observed =
                    {
                        LocalsInit = DynamicMethodBody.readInitLocals operation state resolver
                        CatchTypes = clauseTypes
                    }

                let prepared, methodHandles =
                    MethodHandleRegistry.latchPreparation handle observed state.MethodHandles

                let state =
                    { state with
                        MethodHandles = methodHandles
                    }

                Ok prepared, state

        match prepared with
        | Error e -> state, Error e
        | Ok prepared ->

        let state, method =
            ExecutionConcretization.concretizeDynamicMethod
                loggerFactory
                baseClassTypes
                operation
                handle
                prepared.LocalsInit
                state

        state, Ok method
