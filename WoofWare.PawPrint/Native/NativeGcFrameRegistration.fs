namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeGcFrameRegistration =
    /// `System.Runtime.GCFrameRegistration::RegisterForGCReporting` and its
    /// `UnregisterForGCReporting` partner: the two InternalCalls (`vm/ecalllist.h:266-268`,
    /// bound to `GCReporting::Register`/`Unregister` declared at `vm/ecall.h:96`) by which
    /// managed code hands the GC a block of stack slots the JIT's own GC info cannot describe.
    /// The four callers — `MethodBaseInvoker`, `MethodBaseInvoker.Constructor`, `MethodInvoker`,
    /// `ConstructorInvoker` — each `stackalloc` an `IntPtr` block that holds `object` references
    /// and `ByReference`s at runtime-computed offsets, so nothing static can describe it.
    ///
    /// `GCFrameRegistration` is a `[StructLayout(Sequential)]` struct laid out to be punned as
    /// the VM's `GCFrame` (`vm/frames.h:1865-1917`): `_reserved1`/`_reserved2` are `m_Next` and
    /// `m_pCurThread`, and the remaining three are `m_pObjRefs`, `m_numObjRefs`,
    /// `m_MaybeInterior`. The managed constructor fills the last three and zeroes the two
    /// reserved words; the bodies (`vm/eetwain.cpp:1185-1204`) then do nothing but
    /// `frame->Push(GetThread())` and `frame->Remove()`, which link that frame onto, and unlink
    /// it from, the calling thread's GC frame chain.
    ///
    /// **PawPrint never collects**, so both are no-ops here, and that is an exact behavioural
    /// match rather than a shortcut. (`Native/NativeGc.fs` rests on the same fact twice: once
    /// for `GC_ALLOC_PINNED_OBJECT_HEAP`, once for every field `GC.GetMemoryInfo` reports. Of
    /// those three, this is the one that would become a *silent* soundness bug if a collector
    /// ever landed, because the blocks registered here are precisely the roots nothing else can
    /// find — so a collector's first job is to make this handler stop being a no-op.)
    ///
    /// The case for that, in full. The chain these two maintain has three readers upstream:
    ///
    ///   * `GCFrame::GcScanRoots`, reached from `vm/gcenv.ee.cpp:209` during a collection, which
    ///     promotes the `_numObjRefs` slots at `_pObjRefs` (as interior pointers when
    ///     `_maybeInterior`). This is the reader the mechanism exists for, and PawPrint never
    ///     runs it: nothing collects, and no guest can make it collect — `GC.Collect` and
    ///     `GC.WaitForPendingFinalizers` are not implemented at all, so a guest reaching for the
    ///     distinguishing experiment stops loudly at `NativeDispatch.failUnimplemented` rather
    ///     than diverging quietly.
    ///   * `popGCFrames` in `vm/exceptionhandling.cpp:510-517`, which pops frames below the
    ///     unwind target during exception dispatch. That only *unlinks*; with nothing linked
    ///     there is nothing to unlink, and PawPrint runs the managed `finally` that calls
    ///     `Unregister` through its own unwinder regardless.
    ///   * `Thread::GetGCFrame`'s `_DEBUG_IMPL` assert (`vm/threads.h:1191-1205`) that the head
    ///     lies within the current stack bounds. Debug-only, and not guest-observable.
    ///
    /// So the complete set of guest-observable consequences of a registration is "the referenced
    /// objects stay alive across the call", and PawPrint supplies that unconditionally, for
    /// every object, registered or not. This is observational equivalence rather than a
    /// divergence — hence no entry in `docs/divergences.md` — and it is not weakened by the
    /// usual GC-adjacent surfaces: finalizers never run, a weak or dependent handle's target is
    /// cleared only by explicit guest action, and no object is ever moved, so identity and
    /// pinning are unaffected. Nothing managed can read the difference out of the struct
    /// either: `_reserved1`/`_reserved2` are private, are written only by the constructor (to
    /// zero) and by the native `Push`, and are read only by the native `Remove` — grep of
    /// `System.Private.CoreLib` finds no other mention of either field.
    ///
    /// That equivalence is scoped to calls whose argument points at a registration the managed
    /// constructor built, which is every call CoreLib can make. It does *not* extend to a forged
    /// pointer, which reflection can produce (`RuntimeType.CanValueSpecialCast` accepts a bare
    /// `IntPtr` for a pointer-typed parameter, and reflection ignores the `internal` on these
    /// methods). CoreCLR has no defined behaviour there — its `_ASSERTE(frame != NULL)` compiles
    /// out of a release build, leaving `Push` to write `m_Next`/`m_pCurThread` straight through
    /// the pointer — so a null is refused loudly here rather than silently succeeding, which is
    /// as close as PawPrint gets to "the runtime faulted". `ManagedPointerSource.Null` is the
    /// only spelling of null the refusal has to catch: `ofBitPattern` normalises a zero
    /// `NativeIntPlaceholder` into it, so the placeholder case never carries zero. A *non-null*
    /// forged pointer is still accepted and no-opped, which is the same answer PawPrint gives
    /// any other unreachable-but-representable argument: there is nothing here for it to
    /// corrupt.
    ///
    /// No guest can reach that refusal today, and this was measured rather than assumed: a guest
    /// doing exactly the above (`GetType("System.Runtime.GCFrameRegistration")`, then
    /// `Invoke(null, new object[] { IntPtr.Zero })`) stops one level earlier, in
    /// `RuntimeMethodHandle_InvokeMethod`, with "parameter 0 is a pointer or function pointer,
    /// whose argument buffer entry addresses a boxed IntPtr payload rather than an object slot"
    /// — the gap the parked `sourcesPure/ReflectionInvokePointerSignature.cs` is filed against.
    /// So the only live route into this handler is CoreLib's own, and CoreLib always passes
    /// `&someLocal`. The refusal below is therefore an arm no test can currently kill; it is
    /// kept because without it PawPrint would *silently succeed* where CoreCLR faults, and
    /// because un-parking that reflection gap makes it reachable without anyone revisiting this
    /// file.
    ///
    /// Deliberately, no registration is recorded anywhere. A per-thread table of registered
    /// blocks would be write-only — PawPrint has no collector to consult it — so no test could
    /// tell a correct table from a wrong one, and building one means reading the struct back
    /// through a pointer into a guest stack frame, i.e. new failure modes bought with no
    /// observable behaviour. That table is what to add if a collector ever lands; it is also
    /// where an "every `Register` is balanced by an `Unregister`" invariant check would live, if
    /// a use for one appears (today, unbalanced registration is unobservable by construction).
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
          "System.Runtime",
          "GCFrameRegistration",
          (("RegisterForGCReporting" | "UnregisterForGCReporting") as methodName),
          [ ConcretePointer (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                               "System.Runtime",
                                                               "GCFrameRegistration",
                                                               registrationGenerics)) ],
          MethodReturnType.Void when registrationGenerics.IsEmpty ->
            let operation = $"GCFrameRegistration.%s{methodName}"

            if instruction.Arguments.Length <> 1 then
                failwith $"%s{operation}: expected one argument, got %d{instruction.Arguments.Length}"

            match NativeCall.managedPointerOfPointerArgument operation "pRegistration" instruction.Arguments.[0] with
            | ManagedPointerSource.Null ->
                failwith
                    $"%s{operation}: pRegistration was null. Every CoreLib caller passes the address of a live local, so this is a forged pointer; CoreCLR's own behaviour here is an assert that compiles out of a release build, leaving it to fault writing through the pointer."
            | _ ->
                // The registration itself has nothing to do; see this module's doc comment.
                NativeHandlerResult.completed state |> Some
        | _ -> None
