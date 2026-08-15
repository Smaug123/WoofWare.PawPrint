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
/// "First execution" stands in for CoreCLR's "first JIT". Everything that reaches
/// <c>LCGMethodResolver::GetCodeInfo</c> does so as part of a JIT — the JIT's own
/// <c>getMethodInfo</c>, a profiler callback immediately before it, PGO hashing — and tier-up
/// re-JIT reads the cached values rather than the guest's field, so "first JIT wins" is the whole
/// of the rule. The one way to reach a first JIT without invoking anything is
/// <c>RuntimeHelpers.PrepareMethod</c>, which PawPrint does not implement for dynamic methods.
/// Whoever implements it must latch here too, or a guest could prepare a method, assign
/// <c>InitLocals</c>, and see the assignment take effect where real .NET would ignore it.
/// </remarks>
[<RequireQualifiedAccess>]
module internal DynamicMethodExecution =

    /// <summary>
    /// The method <paramref name="handle" /> names, in the form a frame can be pushed for,
    /// latching its <c>initLocals</c> if this is the first time it has been prepared.
    /// </summary>
    let concretize
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (handle : DynamicMethodHandle)
        (state : IlMachineState)
        : IlMachineState * MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let definition =
            MethodHandleRegistry.resolveDynamicMethod handle state.MethodHandles
            |> Option.defaultWith (fun () ->
                failwith $"%s{operation}: %O{handle} is not registered in the method-handle registry"
            )

        // Only read the guest's field if the answer is not already fixed. Not an optimisation:
        // once latched, the resolver is no longer consulted at all, so a method whose resolver had
        // somehow gone is still executable on its second invocation, exactly as a JITted one is.
        let localsInit, state =
            match definition.GetLatchedLocalsInit () with
            | Some latched -> latched, state
            | None ->
                let resolver =
                    definition.GetResolver ()
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"%s{operation}: %O{handle} has no DynamicResolver, so its initLocals cannot be read; ModuleHandle_GetDynamicMethod refuses a null resolver, so a registered method should always have one"
                    )

                let observed = DynamicMethodBody.readInitLocals operation state resolver

                let localsInit, methodHandles =
                    MethodHandleRegistry.latchInitLocals handle observed state.MethodHandles

                let state =
                    { state with
                        MethodHandles = methodHandles
                    }

                localsInit, state

        ExecutionConcretization.concretizeDynamicMethod loggerFactory baseClassTypes operation handle localsInit state
