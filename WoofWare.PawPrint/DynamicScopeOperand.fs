namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging

/// <summary>
/// What looking an index up in a live <c>DynamicScope</c> found, at the granularity the guest can
/// tell apart. Every distinction here is measured on real .NET, by rewriting the scope through
/// private reflection after <c>CreateDelegate</c> and before the first invocation.
/// </summary>
/// <remarks>
/// Which *exception* each case produces is deliberately not recorded here, because it is a property
/// of the consumer rather than of the lookup: an <c>Absent</c> entry is
/// <c>InvalidProgramException</c> for a type operand, whose <c>ResolveToken</c> throws on null, and
/// <c>NullReferenceException</c> for <c>ldstr</c>, whose <c>GetString</c> returns null and leaves
/// the JIT to indirect through it. Both are measured, and an earlier revision of this branch got
/// <c>ldstr</c> wrong precisely by reading the mapping off the type path.
/// </remarks>
[<RequireQualifiedAccess>]
type internal ScopeEntryLookup =
    | Found of ManagedHeapAddress
    /// The slot is null, or the index is past the end and so reads as null — `DynamicScope`'s
    /// indexer returns null for both, so no consumer can tell them apart.
    | Absent
    /// The index is exactly the list's length, which `DynamicScope`'s indexer lets through its own
    /// bound check and then faults on. Every consumer surfaces <c>ArgumentOutOfRangeException</c>.
    ///
    /// The *type* is reproduced; `ParamName` is not, because the channel that raises a
    /// runtime-synthesised exception calls a parameterless constructor and cannot set
    /// <c>_paramName</c>. That is a general property of that channel rather than anything about
    /// this case — `Intrinsics.fs` declines to fake the same field twice, on the grounds that
    /// writing the name into `_message` alone would leave `.Message` and `.ParamName` disagreeing
    /// about whether it is known — and the same reasoning applies here.
    | PastEnd

/// <summary>
/// What resolving a method-position <c>DynamicScope</c> entry found.
/// </summary>
/// <remarks>
/// The two cases are the two halves of CoreCLR's <c>dm.GetMethodDescriptor()</c>: it hands back the
/// existing <c>_methodHandle</c> if there is one, and otherwise *mints* the method first, which is
/// managed guest code. Reading an operand cannot run guest code, so the second half comes back to
/// the caller to arrange — see <c>DynamicScopeOperand.mintDynamicMethod</c>.
/// </remarks>
[<RequireQualifiedAccess>]
type internal DynamicMethodResolution =
    /// The entry names a <c>DynamicMethod</c> that has been minted; this is its identity.
    | Resolved of DynamicMethodHandle
    /// The entry names a <c>DynamicMethod</c> whose <c>_methodHandle</c> is still null, at this heap
    /// address. It has to be minted before the instruction naming it can run.
    | NeedsMinting of ManagedHeapAddress

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

    let private requireObject (operation : string) (what : string) (value : CliType) : ManagedHeapAddress option =
        match CliType.unwrapPrimitiveLikeDeep value with
        | CliType.ObjectRef target -> target
        | other -> failwith $"%s{operation}: expected %s{what} to be an object reference, got %O{other}"

    let private requireField
        (operation : string)
        (what : string)
        (state : IlMachineState)
        (owner : ManagedHeapAddress)
        (field : string)
        : ManagedHeapAddress
        =
        AllocatedNonArrayObject.DereferenceField field (ManagedHeap.get owner state.ManagedHeap)
        |> requireObject operation what
        |> Option.defaultWith (fun () -> failwith $"%s{operation}: %s{what} is null")

    /// <summary>
    /// The backing array and live length of a <c>DynamicResolver</c>'s
    /// <c>m_scope.m_tokens</c>, which is a <c>List&lt;object?&gt;</c>.
    /// </summary>
    /// <remarks>
    /// <c>_size</c> and not <c>_items.Length</c>: <c>List&lt;T&gt;</c> over-allocates, and the
    /// slots past <c>_size</c> hold whatever was last there.
    /// </remarks>
    let tokenList
        (operation : string)
        (state : IlMachineState)
        (resolver : ManagedHeapAddress)
        : ManagedHeapAddress * int
        =
        let scope = requireField operation "the resolver's m_scope" state resolver "m_scope"
        let tokens = requireField operation "the scope's m_tokens" state scope "m_tokens"
        let items = requireField operation "the token list's _items" state tokens "_items"

        let size =
            match
                AllocatedNonArrayObject.DereferenceField "_size" (ManagedHeap.get tokens state.ManagedHeap)
                |> CliType.unwrapPrimitiveLikeDeep
            with
            | CliType.Numeric (CliNumericType.Int32 v) -> v
            | other -> failwith $"%s{operation}: expected List<object>._size to be an int32, got %O{other}"

        let itemsShape = ManagedHeap.getArrayShape items state.ManagedHeap

        if size < 0 || size > itemsShape.Length then
            failwith
                $"%s{operation}: the token list claims %d{size} entries but its backing array holds %d{itemsShape.Length}"

        items, size

    /// <summary>
    /// The guest object that entry <paramref name="scopeIndex"/> of the executing method's
    /// <c>DynamicScope</c> holds *right now*. <c>None</c> if that slot is null or does not exist.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Read out of the live <c>m_tokens</c> rather than out of anything captured when the method was
    /// minted, and that is the whole point: a guest can replace a slot through private reflection
    /// after <c>CreateDelegate</c> and before the first invocation, and real .NET compiles against
    /// the replacement (measured — swap `typeof(int).TypeHandle` for `typeof(long).TypeHandle` and
    /// `sizeof` answers 8). Capturing the entry's address at mint would follow a *mutation of the
    /// box* but miss a *replacement of the slot*, which is the case the measurement exercised.
    /// </para>
    /// <para>
    /// Reached through the executing method rather than through the operand, because the operand is
    /// an <see cref="IlOp"/> and must not carry a heap address. A dynamic method's
    /// <c>MethodInfo</c> is <c>Synthesised</c> with a <c>SynthesisedMethod.DynamicMethod</c> kind
    /// carrying its <c>DynamicMethodHandle</c> — that handle is the method's identity precisely
    /// because every dynamic method in a module shares one owner — so the resolver, and with it the
    /// scope, is one registry lookup away.
    /// </para>
    /// </remarks>
    let entryObject
        (operation : string)
        (scopeIndex : int)
        (state : IlMachineState)
        (thread : ThreadId)
        : ScopeEntryLookup
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

        let resolver =
            definition.GetResolver ()
            |> Option.defaultWith (fun () ->
                failwith
                    $"%s{operation}: %O{handle} names DynamicScope entry %d{scopeIndex}, but the method was minted with no resolver, so it has no scope to name"
            )

        let items, size = tokenList operation state resolver

        // `DynamicScope`'s indexer is
        //     if (token < 0 || token > m_tokens.Count) return null; return m_tokens[token];
        // (`DynamicILGenerator.cs:976-987`), whose bound check is off by one against its own
        // indexing. So an index *past* the end reads as null, and an index exactly *at* the end
        // gets through the check and faults the list. Mirrored rather than tidied, because the
        // difference is guest-visible: measured on real .NET, truncating the list so the operand
        // index equals `Count` gives ArgumentOutOfRangeException where a larger index gives
        // InvalidProgramException. This is the same "mirror the indexer, do not second-guess it"
        // rule that makes the token's tag non-authoritative.
        if scopeIndex = size then
            ScopeEntryLookup.PastEnd
        elif scopeIndex < 0 || scopeIndex > size then
            ScopeEntryLookup.Absent
        else

        match
            ManagedHeap.getArrayValue items scopeIndex state.ManagedHeap
            |> requireObject operation $"DynamicScope entry %d{scopeIndex}"
        with
        | None -> ScopeEntryLookup.Absent
        | Some addr -> ScopeEntryLookup.Found addr

    /// <summary>
    /// The message CoreCLR attaches to the exception it raises for a bad scope operand, or
    /// <c>None</c> where that is the exception type's own default message.
    /// </summary>
    /// <remarks>
    /// Measured, by rewriting a scope after <c>CreateDelegate</c>: <c>InvalidProgramException</c>
    /// (null type slot, non-closed, byref, <c>System.Void</c>) and <c>NullReferenceException</c>
    /// (<c>ldstr</c>) both carry their type's default message, so <c>None</c> reproduces them
    /// exactly; <c>BadImageFormatException</c> carries the fixed string below, with no token detail
    /// in it, so that too can be reproduced exactly.
    ///
    /// The exception is <c>ArgumentOutOfRangeException</c>, whose real message is
    /// <c>List&lt;T&gt;</c>'s "Index was out of range… (Parameter 'index')". The suffix comes from
    /// <c>_paramName</c>, which this channel cannot set, so the message is left at its default and
    /// the gap is the one already recorded on <see cref="ScopeEntryLookup.PastEnd"/>.
    ///
    /// Note this is a property of the *exception*, not of what went wrong: the reason a particular
    /// operand was refused is a PawPrint diagnostic and belongs in the log, not in a field the guest
    /// reads.
    /// </remarks>
    let clrMessageFor
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (exceptionType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : string option
        =
        if exceptionType.Identity = baseClassTypes.BadImageFormatException.Identity then
            Some "Bad class token."
        else
            None

    /// True when <paramref name="handle"/> is the given corelib type, by identity rather than by
    /// displayed name: a guest can define its own `System.RuntimeTypeHandle`, and CoreCLR's own test
    /// is `handle is RuntimeTypeHandle`, which a lookalike does not satisfy.
    let isCorelibType
        (corelibType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (state : IlMachineState)
        (handle : ConcreteTypeHandle)
        : bool
        =
        match handle with
        | ConcreteTypeHandle.Concrete _ ->
            match AllConcreteTypes.lookup handle state.ConcreteTypes with
            | Some ty -> ty.Identity = corelibType.Identity
            | None -> false
        | _ -> false

    /// <summary>
    /// The closed type named by entry <paramref name="scopeIndex"/>, or a description of why the
    /// entry does not name one — which the caller turns into the guest's
    /// <c>InvalidProgramException</c>.
    /// </summary>
    /// <remarks>
    /// <para>
    /// This is CoreCLR's <c>ResolveToken</c> arm <c>((RuntimeTypeHandle)handle).Value</c>
    /// (<c>DynamicILGenerator.cs:779-782</c>), followed by the walk from the guest
    /// <c>RuntimeType</c> to the target it stands for.
    /// </para>
    /// <para>
    /// Everything the decoder established about this entry is re-established here rather than
    /// assumed, because the decoder looked at the scope as it was when the method was minted and
    /// this reads it as it is now; a guest that replaced the slot in between gets what real .NET
    /// gives it, which is a refusal rather than a resolution against the stale entry.
    /// </para>
    /// <para>
    /// A closed target is not automatically a legal operand. Measured on real .NET, against a
    /// closed control that runs: a byref (<c>typeof(int).MakeByRefType()</c>) and <c>System.Void</c>
    /// are <c>InvalidProgramException</c> for <c>sizeof</c>, <c>newarr</c> and <c>box</c> alike,
    /// whereas a *pointer* type is perfectly legal and answers 8 — so pointers, arrays and function
    /// pointers are deliberately not refused here. The equivalent metadata operands are not
    /// validated either; that is pre-existing and unreachable from any compiler, where this is newly
    /// reachable because `ILGenerator.Emit` accepts any `RuntimeType`.
    /// </para>
    /// <para>
    /// Of those, only the <c>System.Void</c> refusal has a test: a guest cannot yet build a byref or
    /// pointer <c>Type</c> at all, because <c>Type.MakeByRefType</c> and <c>Type.MakePointerType</c>
    /// bottom out in the unimplemented <c>RuntimeTypeHandle_MakeByRef</c> QCall. The byref arm is
    /// kept rather than deferred because it is measured rather than guessed, and because it becomes
    /// reachable the moment that unrelated QCall lands, at which point nothing would prompt anyone
    /// to add it.
    /// </para>
    /// <para>
    /// Where this deliberately stops: it answers the questions that are *about the scope* — is the
    /// entry a type handle, and is the target a closed type — plus the two shapes above. It does
    /// not answer "is this type a legal operand of this opcode", which is a per-opcode question
    /// (<c>sizeof Span&lt;int&gt;</c> is legal and answers 16; <c>newarr Span&lt;int&gt;</c> is a
    /// <c>TypeLoadException</c> — both measured) and one PawPrint does not answer for metadata
    /// operands either. Answering it belongs with the ops, for both universes at once; adding
    /// shapes here one at a time would make the same IL behave differently depending on which
    /// universe its operand came from.
    /// </para>
    /// </remarks>
    let closedType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (scopeIndex : int)
        (state : IlMachineState)
        (thread : ThreadId)
        : Result<ConcreteTypeHandle, TypeInfo<GenericParamFromMetadata, TypeDefn> * string>
        =
        // Which exception each refusal carries is measured, not chosen: rewrite a type slot after
        // `CreateDelegate` and before the first invocation, and real .NET answers
        // InvalidProgramException for a null slot, BadImageFormatException ("Bad class token") for a
        // slot holding the wrong kind of thing -- including a `default(RuntimeTypeHandle)`, whose
        // `m_type` is null -- and ArgumentOutOfRangeException for an index exactly at the list's
        // length. All three are catchable, so a guest can tell them apart.
        let badImage (why : string) =
            Error (baseClassTypes.BadImageFormatException, why)

        let invalidProgram (why : string) =
            Error (baseClassTypes.InvalidProgramException, why)

        match entryObject operation scopeIndex state thread with
        | ScopeEntryLookup.PastEnd ->
            Error (
                baseClassTypes.ArgumentOutOfRangeException,
                $"DynamicScope entry %d{scopeIndex} is exactly at the end of the scope's token list"
            )
        | ScopeEntryLookup.Absent -> invalidProgram $"DynamicScope entry %d{scopeIndex} is null, so it names no type"
        | ScopeEntryLookup.Found entry ->

        // The entry's type is established before it is dereferenced, because a rewritten slot can
        // hold *anything* — a `byte[]` above all, which is what a signature blob is, and which
        // `ManagedHeap.get` refuses because it reads only non-array objects. Measured on real .NET,
        // that case is a BadImageFormatException the guest can catch, not a runtime failure.
        // `tryGetObjectConcreteType` covers arrays, so its `None` is not that case: it means the
        // slot points off the heap entirely, which is interpreter corruption rather than anything a
        // guest can arrange.
        match ManagedHeap.tryGetObjectConcreteType entry state.ManagedHeap with
        | None -> failwith $"%s{operation}: DynamicScope entry %d{scopeIndex} is at %O{entry}, which is not on the heap"
        | Some concreteType when not (isCorelibType baseClassTypes.RuntimeTypeHandle state concreteType) ->
            badImage $"DynamicScope entry %d{scopeIndex} is not a System.RuntimeTypeHandle"
        | Some _ ->

        let boxed = ManagedHeap.get entry state.ManagedHeap

        let mTypeField =
            IlMachineState.requiredOwnInstanceFieldId state boxed.ConcreteType "m_type"

        let target =
            match
                AllocatedNonArrayObject.DereferenceFieldById mTypeField boxed
                |> CliType.unwrapPrimitiveLikeDeep
            with
            | CliType.ObjectRef (Some runtimeType) ->
                Ok (runtimeTypeHandleTargetOfRuntimeType operation state runtimeType)
            | CliType.ObjectRef None ->
                // `default(RuntimeTypeHandle)`. `Emit(OpCode, Type)` rejects a null `Type` and
                // demands a `RuntimeType`, so this is reachable only by writing the scope directly —
                // and measured to be BadImageFormatException rather than InvalidProgramException,
                // because the token resolves to a null type handle rather than to nothing at all.
                badImage $"DynamicScope entry %d{scopeIndex} is a RuntimeTypeHandle whose m_type is null"
            | other ->
                failwith
                    $"%s{operation}: expected DynamicScope entry %d{scopeIndex}'s RuntimeTypeHandle.m_type to be a reference to a RuntimeType, got %O{other}"

        match target with
        | Error e -> Error e
        | Ok (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Byref _ as handle)) ->
            invalidProgram $"DynamicScope entry %d{scopeIndex} names the byref type %O{handle}"
        | Ok (RuntimeTypeHandleTarget.Closed handle) when isCorelibType baseClassTypes.Void state handle ->
            invalidProgram $"DynamicScope entry %d{scopeIndex} names System.Void"
        | Ok (RuntimeTypeHandleTarget.Closed handle) -> Ok handle
        | Ok notClosed ->
            invalidProgram $"DynamicScope entry %d{scopeIndex} names %O{notClosed}, which is not a closed type"

    /// <summary>
    /// The dynamic method named by entry <paramref name="scopeIndex"/>, or the object to mint if it
    /// has not been minted yet.
    /// </summary>
    /// <remarks>
    /// <para>
    /// This is CoreCLR's <c>ResolveToken</c> arm <c>methodHandle = dm.GetMethodDescriptor().Value</c>
    /// (<c>DynamicILGenerator.cs:798-803</c>). <c>GetMethodDescriptor</c> mints an unminted callee
    /// rather than failing on one, so the unminted case is a <c>NeedsMinting</c> answer rather than a
    /// refusal; <see cref="mintDynamicMethod"/> is the other half.
    /// </para>
    /// <para>
    /// Reading late is not a stylistic echo of <see cref="closedType"/> here; it is what makes the
    /// feature work. A dynamic method may take a token for *itself*, and on CoreCLR that resolves
    /// because installation and compilation are separate steps: <c>ModuleHandle.GetDynamicMethod</c>
    /// returns and its caller assigns <c>_methodHandle</c> *before* anything walks the body's
    /// tokens. PawPrint reproduces that ordering by reading the field when the instruction runs, at
    /// which point the executing method has necessarily been minted. Resolving at mint would find
    /// null and have to invent a cycle-breaking rule.
    /// </para>
    /// <para>
    /// Every refusal below is a <c>failwith</c> rather than a guest exception, and that is a
    /// statement about reachability rather than laziness. Real .NET's answers are measured — a null
    /// slot is <c>InvalidProgramException</c>, a slot holding something <c>ResolveToken</c> falls
    /// through on (a <c>string</c>, a <c>RuntimeTypeHandle</c>, a signature blob) is
    /// <c>BadImageFormatException</c> with the fixed message "Bad method token.", and an index
    /// exactly at the list's length is <c>ArgumentOutOfRangeException</c> — but no guest can provoke
    /// any of them today: <c>Emit(OpCode, MethodInfo)</c> accepts only a <c>DynamicMethod</c> or a
    /// <c>RuntimeMethodInfo</c> (which stops at the unimplemented
    /// <c>RuntimeMethodHandle::GetMethodDef</c>), <c>GetTokenFor</c> never hands out index 0 or an
    /// index at <c>Count</c>, and rewriting <c>m_tokens</c> needs reflection PawPrint does not
    /// implement. Raising those exceptions would be three arms nothing could ever kill. Worse, the
    /// obvious rule — "not a <c>DynamicMethod</c>, so bad token" — is *false*:
    /// <c>ResolveToken</c> resolves a <c>RuntimeMethodHandle</c>, a <c>GenericMethodInfo</c> and a
    /// <c>VarArgMethod</c> in method position perfectly happily, so a guest exception here would
    /// diverge from real .NET the day the reflection primitive lands. Crashing with the found kind
    /// named is the honest answer until one of these becomes reachable.
    /// </para>
    /// </remarks>
    let dynamicMethod
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (scopeIndex : int)
        (state : IlMachineState)
        (thread : ThreadId)
        : DynamicMethodResolution
        =
        let entry =
            match entryObject operation scopeIndex state thread with
            | ScopeEntryLookup.Found entry -> entry
            | ScopeEntryLookup.Absent ->
                failwith
                    $"TODO: %s{operation} names DynamicScope entry %d{scopeIndex}, which is null or does not exist; real .NET raises InvalidProgramException, but no guest can reach this today so PawPrint does not fabricate it"
            | ScopeEntryLookup.PastEnd ->
                failwith
                    $"TODO: %s{operation} names DynamicScope entry %d{scopeIndex}, which is exactly at the end of the scope's token list; real .NET raises ArgumentOutOfRangeException, but no guest can reach this today so PawPrint does not fabricate it"

        // The entry's type before it is dereferenced, for the reason `closedType` gives: a slot can
        // hold an array (a signature blob is a `byte[]`), which `ManagedHeap.get` refuses.
        let entryType =
            match ManagedHeap.tryGetObjectConcreteType entry state.ManagedHeap with
            | Some concreteType -> concreteType
            | None ->
                failwith $"%s{operation}: DynamicScope entry %d{scopeIndex} is at %O{entry}, which is not on the heap"

        // `ILGenerator.EmitCall` wraps whatever it was given in a `VarArgMethod`, unconditionally
        // (`GetMemberRefToken`, `DynamicILGenerator.cs:396-443`), so an ordinary
        // `EmitCall(OpCodes.Call, dm, null)` produces one where `Emit(OpCodes.Call, dm)` does not.
        // `ResolveToken` unwraps it to `vaMeth.m_dynamicMethod.GetMethodDescriptor()` and ignores
        // `m_signature` entirely for that case — which is safe rather than sloppy of it, because a
        // `DynamicMethod` is always `CallingConventions.Standard` (its constructors reject anything
        // else, `DynamicMethod.cs:227`), so `GetMemberRefToken` would have thrown had the call site
        // tried to add optional parameter types, and the wrapper's signature is therefore always
        // the callee's own.
        let entry, entryType =
            if isCorelibType baseClassTypes.VarArgMethod state entryType then
                let wrapper = ManagedHeap.get entry state.ManagedHeap

                let inner =
                    match
                        AllocatedNonArrayObject.DereferenceField "m_dynamicMethod" wrapper
                        |> CliType.unwrapPrimitiveLikeDeep
                    with
                    | CliType.ObjectRef (Some inner) -> inner
                    | CliType.ObjectRef None ->
                        // A wrapper round a *reflected* method, which `ResolveToken` resolves
                        // through `m_method`. Unreachable today: obtaining a `RuntimeMethodInfo` at
                        // all stops at the unimplemented `RuntimeMethodHandle::GetMethodDef`.
                        failwith
                            $"TODO: %s{operation} names DynamicScope entry %d{scopeIndex}, a VarArgMethod whose m_dynamicMethod is null, so it wraps a reflected method; PawPrint resolves only dynamic methods in method position"
                    | other ->
                        failwith
                            $"%s{operation}: expected DynamicScope entry %d{scopeIndex}'s VarArgMethod.m_dynamicMethod to be a reference, got %O{other}"

                match ManagedHeap.tryGetObjectConcreteType inner state.ManagedHeap with
                | Some innerType -> inner, innerType
                | None ->
                    failwith
                        $"%s{operation}: DynamicScope entry %d{scopeIndex}'s VarArgMethod.m_dynamicMethod is at %O{inner}, which is not on the heap"
            else
                entry, entryType

        if not (isCorelibType baseClassTypes.DynamicMethod state entryType) then
            failwith
                $"TODO: %s{operation} names DynamicScope entry %d{scopeIndex}, which holds a %O{entryType} rather than a System.Reflection.Emit.DynamicMethod; PawPrint resolves only dynamic methods in method position, and neither the reflected kinds real .NET also accepts there nor real .NET's BadImageFormatException for the rest is implemented"

        let dm = ManagedHeap.get entry state.ManagedHeap

        // `DynamicMethod._methodHandle` is an `IRuntimeMethodInfo` -- under PawPrint the
        // `RuntimeMethodInfoStub` that `MethodHandleRegistry.mintDynamicMethod` allocated -- and is
        // null until the target has been minted.
        let stub =
            match
                AllocatedNonArrayObject.DereferenceField "_methodHandle" dm
                |> CliType.unwrapPrimitiveLikeDeep
            with
            | CliType.ObjectRef stub -> stub
            | other ->
                failwith
                    $"%s{operation}: expected DynamicScope entry %d{scopeIndex}'s DynamicMethod._methodHandle to be a reference, got %O{other}"

        match stub with
        | None ->
            // Not a refusal: real .NET mints the target at exactly this point, because
            // `ResolveToken` calls `GetMethodDescriptor`, which takes `lock (this)` and runs the
            // guest's `GetCallableMethod` — reaching the very QCall PawPrint implements. Reading an
            // operand cannot run guest code, so the object goes back to the instruction to mint and
            // then re-execute.
            //
            // `entry`, not the scope slot: where the slot held a `VarArgMethod` this is the
            // unwrapped `m_dynamicMethod`, which is exactly what `ResolveToken` mints in that case
            // (`DynamicILGenerator.cs:827`).
            DynamicMethodResolution.NeedsMinting entry
        | Some stub ->

        let registryId =
            match
                AllocatedNonArrayObject.DereferenceField "m_value" (ManagedHeap.get stub state.ManagedHeap)
                |> CliType.unwrapPrimitiveLikeDeep
            with
            | CliType.RuntimePointer (CliRuntimePointer.MethodRegistryHandle id) -> id
            | other ->
                failwith
                    $"%s{operation}: expected DynamicScope entry %d{scopeIndex}'s RuntimeMethodInfoStub.m_value to carry a method registry id, got %O{other}"

        match MethodHandleRegistry.resolveMethodFromId registryId state.MethodHandles with
        | Some (MethodHandle.FromDynamic handle) -> DynamicMethodResolution.Resolved handle
        | Some (MethodHandle.FromMetadata identity) ->
            failwith
                $"%s{operation}: DynamicScope entry %d{scopeIndex} is a DynamicMethod whose _methodHandle names the metadata method %O{identity.GetMethodDefinitionHandle ()}; only mintDynamicMethod may populate that field"
        | None ->
            failwith
                $"%s{operation}: DynamicScope entry %d{scopeIndex} is a DynamicMethod whose _methodHandle carries registry id %d{registryId}, which is not registered"

    /// <summary>
    /// Push a frame that mints <paramref name="callee"/>, an as-yet-unminted <c>DynamicMethod</c>
    /// that some instruction on <paramref name="thread"/> names. The caller's program counter is
    /// deliberately not advanced: when this frame returns, the naming instruction runs again and
    /// this time finds a non-null <c>_methodHandle</c>.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The minting is done by *calling the guest's own* <c>DynamicMethod.GetMethodDescriptor</c>,
    /// which is what CoreCLR's <c>ResolveToken</c> calls (<c>DynamicILGenerator.cs:800</c>), rather
    /// than by reading the builder's fields and minting natively. That matters for more than
    /// tidiness: <c>GetMethodDescriptor</c> takes <c>lock (this)</c> and double-checks under it, so
    /// two threads racing to call the same unminted callee agree on one identity without the
    /// interpreter arranging anything; it refuses an empty body with
    /// <c>InvalidOperationException</c>, which a native shortcut would have to fabricate; and it
    /// bakes the body <em>now</em>, so a guest that emitted into the callee after binding the
    /// caller to a delegate gets the later body, as measured on real .NET.
    /// </para>
    /// <para>
    /// No re-entry marker is needed, and none is wanted. Whether the mint has happened is a fact
    /// about the guest heap — <c>_methodHandle</c> is null or it is not — exactly as it is for
    /// class initialisation, so the re-executed instruction can simply ask again. A marker would
    /// be a second copy of that answer, free to disagree with it.
    /// </para>
    /// <para>
    /// The caller's evaluation stack already holds the naming instruction's operands, and they must
    /// still be there when it re-executes. <c>callMethod</c> pops exactly the one <c>this</c>
    /// pushed here (<c>GetMethodDescriptor</c> takes no arguments and is not virtual, so nothing
    /// peeks deeper), and <c>ReturnValueDisposition.Discard</c> stops the <c>RuntimeMethodHandle</c>
    /// it returns from landing on top of them. The value is genuinely redundant rather than merely
    /// inconvenient: it wraps the same <c>_methodHandle</c> the re-execution reads off the heap.
    /// </para>
    /// <para>
    /// <b>Divergence.</b> Real .NET mints as part of *compiling* the caller, not of running it, so
    /// it mints every callee the JIT's importer reaches before the body's first instruction — even
    /// one behind an untaken conditional branch — and a callee that cannot be minted therefore
    /// fails the whole invocation. All measured: an empty-bodied callee named from an untaken
    /// branch still throws <c>InvalidOperationException</c>, and a <c>stsfld</c> preceding the call
    /// site does not take effect. Minting here instead means PawPrint runs the prefix and skips the
    /// unreached mint.
    /// </para>
    /// <para>
    /// Not fixed, because the fix is worse. Preparing a whole body up front means deciding which
    /// call sites the importer would have reached, and it is not "all of them": a call site after
    /// an unconditional <c>ret</c> is measurably *not* resolved (the bytes and the scope entry are
    /// both present — <c>1F 2A 2A 28 02 00 00 06 2A</c> — and the callee stays unminted, so the
    /// program answers 42 rather than throwing). A whole-body walk would mint a callee real .NET
    /// never touches and throw where real .NET answers, trading a mistimed exception for a wrong
    /// one. Reproducing the importer's reachability, constant-branch folding included, is the only
    /// honest version of that fix.
    /// </para>
    /// <para>
    /// No test pins this, because no guest can currently observe it. Seeing the difference requires
    /// a dynamic method body that either performs an externally-visible side effect before the call
    /// site or catches the failure itself, and a body can do neither today: every field-shaped
    /// opcode is <c>ScopeOperandKind.NotYetSupported</c>, a metadata callee needs the unimplemented
    /// <c>RuntimeMethodHandle::GetMethodDef</c>, and <c>DynamicMethodBody.read</c> refuses a body
    /// with any exception region. Whichever of those lands first makes this observable, and should
    /// bring a test with it.
    /// </para>
    /// </remarks>
    let mintDynamicMethod
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (callee : ManagedHeapAddress)
        (thread : ThreadId)
        (state : IlMachineState)
        : IlMachineState
        =
        let dynamicMethodType = baseClassTypes.DynamicMethod
        let assy = state._LoadedAssemblies.[dynamicMethodType.Assembly]
        let typeDef = assy.TypeDefs.[dynamicMethodType.Identity.TypeDefinition.Get]

        let getMethodDescriptor =
            typeDef.Methods
            |> List.tryFind (fun method ->
                method.Name = "GetMethodDescriptor"
                && not method.IsStatic
                && MethodInfo.arity method = 0
            )
            |> Option.defaultWith (fun () ->
                failwith
                    $"%s{operation}: System.Reflection.Emit.DynamicMethod has no parameterless instance GetMethodDescriptor; CoreCLR's ResolveToken calls it to mint an unminted callee, so PawPrint cannot mint one without it"
            )
            // `DynamicMethod` is not generic (it is a sealed class with no type parameters), so
            // nothing in this method's type-generic positions can be a `GenericParamFromMetadata`.
            |> MethodInfo.mapTypeGenerics (fun _ ->
                failwith<TypeDefn> $"%s{operation}: System.Reflection.Emit.DynamicMethod was unexpectedly generic"
            )

        let state, concretized, _declaringType =
            ExecutionConcretization.concretizeMethodForExecution
                loggerFactory
                baseClassTypes
                thread
                getMethodDescriptor
                None
                None
                state

        let state =
            IlMachineState.pushToEvalStack (CliType.ObjectRef (Some callee)) thread state

        let threadState = state.ThreadState.[thread]

        IlMachineStateExecution.callMethod
            loggerFactory
            baseClassTypes
            None // not initialising a type
            ConstructionState.NotConstructing
            false // no interface resolution: GetMethodDescriptor is a non-virtual instance method
            false // not a class constructor
            false // do NOT advance the caller's PC: the naming instruction has to run again
            concretized.Generics
            concretized
            thread
            threadState
            None
            ReturnValueDisposition.Discard
            false // wrapExceptionInTargetInvocation
            state
