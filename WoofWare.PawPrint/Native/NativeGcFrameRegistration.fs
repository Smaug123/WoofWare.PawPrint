namespace WoofWare.PawPrint

open System.Reflection

[<RequireQualifiedAccess>]
module NativeGcFrameRegistration =
    /// The `System.Reflection` frames, all in `System.Private.CoreLib`, that may register a GC
    /// frame here: every method in .NET 10's CoreLib containing a call to
    /// `GCFrameRegistration.RegisterForGCReporting`. Each builds its registration over a
    /// `stackalloc` of its own, hands it straight to the invoke and unregisters it in the
    /// matching `finally`, so the registration cannot escape to anything that reads it back —
    /// which is what makes the no-op below exact rather than approximate. `MethodBaseInvoker`
    /// contributes two entries under one name because `InvokeConstructorWithoutAlloc` is
    /// overloaded (`MethodBaseInvoker.Constructor.cs:15` and `:68`), and matching by name
    /// covers both.
    ///
    /// A CoreLib method that is *not* on this list gets the same loud refusal as a guest: if a
    /// future CoreLib grows another caller, the failure names it rather than silently assuming
    /// the new caller has the same escape property.
    ///
    /// Three of the four are reached by guests today, measured by removing each entry and watching
    /// the refusal below name it: `MethodBaseInvoker.InvokeWithManyArgs` and
    /// `ConstructorInvoker.InvokeWithManyArgs` each have a covering test
    /// (`sourcesPure/ReflectionInvokeMethodManyArguments.cs` and
    /// `sourcesPure/ReflectionInvokeConstructor.cs`), and `MethodInvoker.InvokeWithManyArgs` is
    /// reached by the same shape on the method side but has no test here.
    /// `MethodBaseInvoker.InvokeConstructorWithoutAlloc` is the exception: its five-argument
    /// overload is the only one that registers anything, and reaching it means invoking a
    /// constructor against an existing instance with more than four arguments, which stops a few
    /// instructions later in `MethodBaseInvoker.CopyBack` reading its uninitialised
    /// `stackalloc bool[argCount]` (measured). It is on the list on the authority of the upstream
    /// grep; leaving a real caller off would turn a working path into a loud failure the moment it
    /// became reachable.
    let private permittedCallers : (string * string) list =
        [
            "MethodBaseInvoker", "InvokeWithManyArgs"
            "MethodBaseInvoker", "InvokeConstructorWithoutAlloc"
            "MethodInvoker", "InvokeWithManyArgs"
            "ConstructorInvoker", "InvokeWithManyArgs"
        ]

    /// `System.Runtime.GCFrameRegistration::RegisterForGCReporting` and its
    /// `UnregisterForGCReporting` partner: the two InternalCalls (`vm/ecalllist.h:266-268`,
    /// bound to `GCReporting::Register`/`Unregister` declared at `vm/ecall.h:96`) by which
    /// managed code hands the GC a block of stack slots the JIT's own GC info cannot describe.
    /// The callers — the reflection invoker frames enumerated in `permittedCallers` above —
    /// each `stackalloc` an `IntPtr` block that holds `object` references and `ByReference`s at
    /// runtime-computed offsets, so nothing static can describe it.
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
    /// The chain these two maintain has three readers upstream:
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
    /// So for a caller that does not read the registration back, the complete set of
    /// guest-observable consequences is "the referenced objects stay alive across the call", and
    /// PawPrint supplies that unconditionally, for every object, registered or not. That is
    /// observational equivalence rather than a divergence — hence no entry in
    /// `docs/divergences.md` — and it is not weakened by the usual GC-adjacent surfaces:
    /// finalizers never run, a weak or dependent handle's target is cleared only by explicit
    /// guest action, and no object is ever moved, so identity and pinning are unaffected.
    ///
    /// "Does not read it back" is a property of the *caller*, not of this InternalCall, so it is
    /// enforced rather than assumed: the handler reads the calling frame and refuses anything
    /// not in `permittedCallers`. Two distinct callers would otherwise get a wrong answer, and
    /// the second is why the check is on the frame rather than merely on its assembly:
    ///
    ///   * A guest. PawPrint honours `[IgnoresAccessChecksTo]`
    ///     (`WoofWare.PawPrint.Domain/FriendAssemblies.fs`), so a guest assembly can `call` this
    ///     InternalCall directly on a `GCFrameRegistration` of its own and then read the second
    ///     native word, which CoreCLR's `Push` has by then set to the thread pointer and
    ///     PawPrint would leave at zero.
    ///   * A guest going through reflection. `RuntimeMethodHandle.InvokeMethod` issues the call
    ///     itself, so the immediate caller frame is CoreLib's even though the registration and
    ///     the code that inspects it afterwards are both the guest's. An assembly-level check
    ///     would wave that through. It is not reachable today — pointer-typed parameters are
    ///     refused by that QCall, see below.
    ///
    /// The frames on the list qualify because the registration cannot escape them, not because
    /// they are CoreLib: `_reserved1`/`_reserved2` are private, are written only by the
    /// constructor (to zero) and by the native `Push`, and are read only by the native `Remove`
    /// — a grep of `System.Private.CoreLib` finds no other mention of either field.
    ///
    /// The equivalence has a second boundary, this one on the argument rather than the caller:
    /// it holds for a pointer to a registration the managed constructor built, which is every
    /// call CoreLib can make. It does *not* extend to a forged pointer, which reflection can
    /// produce (`RuntimeType.CanValueSpecialCast` accepts a bare
    /// `IntPtr` for a pointer-typed parameter, and reflection ignores the `internal` on these
    /// methods). CoreCLR has no defined behaviour there — its `_ASSERTE(frame != NULL)` compiles
    /// out of a release build, leaving `Push` to write `m_Next`/`m_pCurThread` straight through
    /// the pointer — so a null is refused loudly here rather than silently succeeding, which is
    /// as close as PawPrint gets to "the runtime faulted". A *non-null*
    /// forged pointer is accepted and no-opped: CoreCLR has no defined behaviour for it either,
    /// and with no chain to link it onto there is nothing here for it to corrupt.
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

            // The no-op is only sound for a caller that does not read the registration back.
            // That is a property of the four CoreLib frames below, not of the InternalCall, so
            // the contract is enforced on the caller rather than assumed. See the doc comment.
            let caller =
                match instruction.ReturnState with
                | None ->
                    failwith
                        $"%s{operation}: reached with no caller frame, so it is the thread's entry point; it is only ever called from the CoreLib invoker frames."
                | Some returnState -> (IlMachineState.getFrame ctx.Thread returnState.JumpTo state).ExecutingMethod

            let callerAssembly : AssemblyName = caller.DeclaringAssembly

            let callerIsPermitted =
                callerAssembly.Name = "System.Private.CoreLib"
                && caller.RequiredDeclaringType.Namespace = "System.Reflection"
                && permittedCallers
                   |> List.contains (caller.RequiredDeclaringType.Name, caller.Name)

            // The alternative to refusing is modelling the frame chain in guest memory, which
            // needs a `Thread*` representation PawPrint does not have and would have to invent
            // a bit pattern for.
            if not callerIsPermitted then
                failwith
                    $"%s{operation}: called from %s{callerAssembly.Name} %s{MethodOwner.describe caller.Owner}::%s{caller.Name}, which is not one of the CoreLib invoker frames PawPrint implements this for. Those frames never read the registration back, which is what makes doing nothing an exact match; a caller that can see `_reserved1`/`_reserved2` would see them stay zero where CoreCLR writes the frame link and the thread pointer. Modelling those two words means modelling the GC frame chain itself. Note that the caller being CoreLib is not enough on its own: a guest that reaches this through `RuntimeMethodHandle.InvokeMethod` presents a CoreLib frame too, and can inspect the registration afterwards."

            // `ManagedPointerSource.Null` is the only spelling of null the refusal has to
            // catch: `ofBitPattern` normalises a zero `NativeIntPlaceholder` into it, so the
            // placeholder case never carries zero.
            //
            // No guest can reach this refusal today (measured): a guest doing exactly the
            // forged-pointer reflection call from the doc comment
            // (`GetType("System.Runtime.GCFrameRegistration")`, then
            // `Invoke(null, new object[] { IntPtr.Zero })`) stops one level earlier, in
            // `RuntimeMethodHandle_InvokeMethod`, with "parameter 0 is a pointer or function
            // pointer, whose argument buffer entry addresses a boxed IntPtr payload rather than
            // an object slot" — the gap the parked
            // `sourcesPure/ReflectionInvokePointerSignature.cs` is filed against. So the only
            // live route into this handler is CoreLib's own, and CoreLib always passes
            // `&someLocal`. No test can currently kill this refusal; without it PawPrint would
            // *silently succeed* where CoreCLR faults, and un-parking that reflection gap makes
            // it reachable.
            match NativeCall.managedPointerOfPointerArgument operation "pRegistration" instruction.Arguments.[0] with
            | ManagedPointerSource.Null ->
                failwith
                    $"%s{operation}: pRegistration was null. Every CoreLib caller passes the address of a live local, so this is a forged pointer; CoreCLR's own behaviour here is an assert that compiles out of a release build, leaving it to fault writing through the pointer."
            | _ ->
                // The registration itself has nothing to do; see `tryExecute`'s doc comment.
                // No registration is recorded anywhere: PawPrint has no collector to consult a
                // table of registered blocks, so it would be write-only, and building one means
                // reading the struct back through a pointer into a guest stack frame. Such a
                // table is what to add if a collector ever lands; it is also where an "every
                // `Register` is balanced by an `Unregister`" invariant check would live (today,
                // unbalanced registration is unobservable by construction).
                NativeHandlerResult.completed state |> Some
        | _ -> None
