namespace WoofWare.PawPrint

/// <summary>
/// Resolving an operand that names an entry in the executing method's <c>DynamicScope</c>, at the
/// moment the instruction runs.
/// </summary>
/// <remarks>
/// <para>
/// A <c>DynamicScope</c> is a <c>List&lt;object&gt;</c> of already-resolved runtime handles hanging
/// off a dynamic method's <c>DynamicResolver</c>; an operand into it is an index, and nothing about
/// the token's bits distinguishes it from a metadata token naming a real row (see
/// <see cref="MetadataOperand"/>). The decoder settles which universe an operand belongs to and
/// records the index; this module turns that index into a value, which is deliberately a separate
/// step and a later one.
/// </para>
/// <para>
/// Later, because CoreCLR reads the scope at JIT rather than at emit —
/// <c>DynamicResolver.ResolveToken</c> (<c>DynamicILGenerator.cs:772</c>) — and that is
/// guest-visible. Measured on real .NET: replace the boxed <c>RuntimeTypeHandle</c> in
/// <c>m_scope.m_tokens</c> after <c>CreateDelegate</c> but before the first invocation and the
/// method compiles against the *new* type; a string's characters can likewise be mutated in place
/// after emitting it. Resolving at mint would bake a snapshot that both cases can invalidate.
/// </para>
/// <para>
/// The known divergence that remains: CoreCLR resolves each token exactly once, at first JIT, and a
/// mutation between two invocations is invisible to it (measured: 4 then 4, where PawPrint answers
/// 4 then 8). Closing that means resolving a method's whole scope at first execution and latching
/// it, as <c>MethodHandleRegistry.latchInitLocals</c> does for <c>initLocals</c> — the same
/// "materialise when the method is prepared rather than when the instruction runs" change that
/// <c>UnaryStringTokenIlOp</c> already documents for <c>ldstr</c>, and deferred with it.
/// </para>
/// </remarks>
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal DynamicScopeOperand =

    /// <summary>
    /// The type a guest <c>System.RuntimeType</c> object stands for.
    /// </summary>
    /// <remarks>
    /// <c>RuntimeType.m_handle</c> is an <c>IntPtr</c> whose provenance is the target itself
    /// (<c>TypeHandleRegistry.getOrAllocate</c> plants a <c>NativeIntSource.TypeHandlePtr</c> there),
    /// so this is a read rather than a registry search. Lives here rather than beside its
    /// <c>EvalStackValue</c>-flavoured wrapper in <c>NativeCall</c> because the ops need it and
    /// <c>NativeCall</c> compiles after them.
    /// </remarks>
    let runtimeTypeHandleTargetOfRuntimeType
        (operation : string)
        (state : IlMachineState)
        (runtimeType : ManagedHeapAddress)
        : RuntimeTypeHandleTarget
        =
        let heapObj = ManagedHeap.get runtimeType state.ManagedHeap

        let handleField =
            IlMachineState.requiredOwnInstanceFieldId state heapObj.ConcreteType "m_handle"

        match
            AllocatedNonArrayObject.DereferenceFieldById handleField heapObj
            |> CliType.unwrapPrimitiveLike
        with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.TypeHandlePtr target)) -> target
        | other -> failwith $"%s{operation}: expected TypeHandlePtr in RuntimeType.m_handle, got %O{other}"

    /// <summary>
    /// The guest object that entry <paramref name="scopeIndex"/> of the executing method's
    /// <c>DynamicScope</c> holds.
    /// </summary>
    /// <remarks>
    /// Reached through the executing method rather than through the operand, because the operand is
    /// an <see cref="IlOp"/> and must not carry a heap address. A dynamic method's
    /// <c>MethodInfo</c> is <c>Synthesised</c> with a <c>SynthesisedMethod.DynamicMethod</c> kind
    /// carrying its <c>DynamicMethodHandle</c> — that handle is the method's identity precisely
    /// because every dynamic method in a module shares one owner — so the body, and with it the
    /// scope's objects, is one registry lookup away.
    /// </remarks>
    let entryObject
        (operation : string)
        (scopeIndex : int)
        (state : IlMachineState)
        (thread : ThreadId)
        : ManagedHeapAddress
        =
        let executing = state.ThreadState.[thread].MethodState.ExecutingMethod

        let handle =
            match executing.SynthesisedKind with
            | Some (SynthesisedMethod.DynamicMethod handle) -> handle
            | _ ->
                failwith
                    $"%s{operation} names DynamicScope entry %d{scopeIndex}, but the executing method %s{executing.Name} is not a dynamic method; only a body read off a DynamicResolver can carry a scope operand"

        let definition =
            MethodHandleRegistry.resolveDynamicMethod handle state.MethodHandles
            |> Option.defaultWith (fun () ->
                failwith $"%s{operation}: %O{handle} is executing but is not registered in the method-handle registry"
            )

        (definition.GetBody ()).ScopeObjects
        |> Map.tryFind scopeIndex
        |> Option.defaultWith (fun () ->
            failwith
                $"%s{operation}: %O{handle} names DynamicScope entry %d{scopeIndex}, but no object was recorded for it; the scope reader records every non-null entry, so either the index is the seed null at 0 or the decoder and the reader have gone out of step"
        )

    /// <summary>
    /// The type named by entry <paramref name="scopeIndex"/>, which the decoder has already
    /// established is a boxed <c>RuntimeTypeHandle</c>.
    /// </summary>
    /// <remarks>
    /// This is CoreCLR's <c>ResolveToken</c> arm <c>((RuntimeTypeHandle)handle).Value</c>
    /// (<c>DynamicILGenerator.cs:779-782</c>), followed by the walk from the guest
    /// <c>RuntimeType</c> to the target it stands for.
    /// </remarks>
    let typeTarget
        (operation : string)
        (scopeIndex : int)
        (state : IlMachineState)
        (thread : ThreadId)
        : RuntimeTypeHandleTarget
        =
        let entry = entryObject operation scopeIndex state thread
        let boxed = ManagedHeap.get entry state.ManagedHeap

        let mTypeField =
            IlMachineState.requiredOwnInstanceFieldId state boxed.ConcreteType "m_type"

        match
            AllocatedNonArrayObject.DereferenceFieldById mTypeField boxed
            |> CliType.unwrapPrimitiveLikeDeep
        with
        | CliType.ObjectRef (Some runtimeType) -> runtimeTypeHandleTargetOfRuntimeType operation state runtimeType
        | CliType.ObjectRef None ->
            // `default(RuntimeTypeHandle)`. `DynamicILGenerator.Emit(OpCode, Type)` rejects a null
            // `Type` and demands a `RuntimeType`, so no supported emit path produces this; it is
            // reachable only by writing the scope directly.
            failwith
                $"%s{operation}: DynamicScope entry %d{scopeIndex} is a RuntimeTypeHandle whose m_type is null, so it names no type at all"
        | other ->
            failwith
                $"%s{operation}: expected DynamicScope entry %d{scopeIndex}'s RuntimeTypeHandle.m_type to be a reference to a RuntimeType, got %O{other}"
