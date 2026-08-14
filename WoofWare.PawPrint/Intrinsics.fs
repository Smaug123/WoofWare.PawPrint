namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module Intrinsics =
    type IntrinsicMethodKey = IntrinsicMethodKeys.IntrinsicMethodKey

    let methodKey
        (state : IlMachineState)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : IntrinsicMethodKey
        =
        IntrinsicMethodKeys.methodKey state methodToCall

    let formatMethodKey (key : IntrinsicMethodKey) : string = IntrinsicMethodKeys.formatMethodKey key

    let isSafeIntrinsic (key : IntrinsicMethodKey) : bool = IntrinsicMethodKeys.isSafeIntrinsic key

    /// The int32 value argument of an intrinsic whose signature match already established
    /// that the parameter's declared type is int32.
    ///
    /// Honours the CLI's implicit call-boundary coercion between int32 and the
    /// pointer-sized integer types: `impImplicitIorI4Cast` (importer.cpp:2459) runs on
    /// every call argument (importercalls.cpp:6453, "insert any widening or narrowing casts
    /// for backwards compatibility"), and on 64-bit `varTypeIsI(TYP_LONG)` holds — `VTF_I64`
    /// aliases `VTF_I` there (typelist.h:10-16) — so a 64-bit-wide integer on the stack
    /// narrows into an int32 parameter. That is legal IL, so PawPrint accepts it too.
    ///
    /// What it does *not* do, and what `conv.i4` would, is synthesise bits for a pointer:
    /// an int32 parameter cannot legally receive one, and narrowing it would assign the
    /// pointer a `PointerHashState` identity, perturbing every synthesised value later in
    /// the run. `Int32Source.value` likewise still refuses a byref that `conv.i4` already
    /// truncated, whose numeric value depends on an address PawPrint does not model.
    let internal int32ValueArgument (operation : string) (value : EvalStackValue) : int32 =
        match value with
        | EvalStackValue.Int32 src -> Int32Source.value operation src
        // There is no implicit float-to-integer coercion at a call boundary:
        // `impImplicitR4orR8Cast` converts only between R4 and R8.
        | EvalStackValue.Float f ->
            failwith
                $"%s{operation}: refusing to coerce float %f{f} into the int32 value argument; the CLI coerces integers at a call boundary, not floats"
        | _ ->
            match EvalStackValue.tryExactIntegerBits value with
            | ValueSome bits -> int32 bits
            | ValueNone ->
                failwith
                    $"%s{operation}: refusing to narrow %O{value} into the int32 value argument; its bits are an address PawPrint does not model, and synthesising them would register a pointer identity for a value that an int32 parameter cannot legally hold"

    /// The int64 value argument of an intrinsic whose signature match already established
    /// that the parameter's declared type is int64. See `int32ValueArgument` for the
    /// coercion rule; the same `impImplicitIorI4Cast` sign-extends an int32 stack value into
    /// an int64 parameter on 64-bit.
    ///
    /// No refusal is needed for a pointer here, because widening to int64 is bit-preserving
    /// on a 64-bit interpreter: `Int64Source.widenedNativeInt` keeps the provenance rather
    /// than fabricating bits, and `Interlocked.And` / `Or` go on to feed it through
    /// `Int64Source.bitAnd` / `bitOr`, which can answer for a pointer-derived operand.
    let internal int64ValueArgument (operation : string) (value : EvalStackValue) : Int64Source =
        match value with
        | EvalStackValue.Int64 src -> src
        | EvalStackValue.Int32 src -> Int64Source.Verbatim (int64<int32> (Int32Source.value operation src))
        | EvalStackValue.NativeInt src -> Int64Source.widenedNativeInt src true
        // A byref, and `ldnull`, are both pointer-sized and both coerced:
        // `impImplicitIorI4Cast` retypes a zero `TYP_REF` constant to `TYP_I_IMPL`
        // outright ("We also allow an implicit conversion of a ldnull into a
        // TYP_I_IMPL(0)"), and `varTypeIsI(TYP_BYREF)` holds. `widenedNativeInt`
        // normalises the exactly-known bit patterns and keeps a real byref's
        // provenance, so any refusal is left to the arithmetic that consumes it.
        | EvalStackValue.ManagedPointer ptr -> Int64Source.widenedNativeInt (NativeIntSource.ManagedPointer ptr) true
        | EvalStackValue.NullObjectRef -> Int64Source.Verbatim 0L
        | EvalStackValue.Float _
        | EvalStackValue.ObjectRef _
        | EvalStackValue.UserDefinedValueType _ ->
            failwith $"%s{operation}: expected an integer value argument, got %O{value}"

    /// The bit pattern of an integer value argument of an intrinsic whose result is defined
    /// by those bits directly — a bit count, say — rather than by the value's arithmetic.
    ///
    /// The bits come back widened to int64, which is the shape `tryExactIntegerBits` reports
    /// (a 32-bit stack value is sign-extended into it). A caller whose declared parameter is
    /// narrower than that must therefore narrow to its own width before reading the bits;
    /// that narrowing is the CLI's implicit call-boundary coercion, for which see
    /// `int32ValueArgument`.
    ///
    /// A pointer PawPrint does not model has no bits to report, and reporting some anyway
    /// would assign it a `PointerHashState` identity, perturbing every synthesised value
    /// later in the run — so those are refused rather than answered.
    let internal bitPatternValueArgument (operation : string) (value : EvalStackValue) : int64 =
        match EvalStackValue.tryExactIntegerBits value with
        | ValueSome bits -> bits
        | ValueNone ->
            failwith
                $"%s{operation}: refusing to report the bit pattern of %O{value}; PawPrint does not model its bits, and synthesising them would register a pointer identity for a value whose bits the guest is asking about"

    open IntrinsicHelpers

    let call
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<_>)
        (wasConstructing : ConstructionState)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : IntrinsicResult
        =
        let intrinsicKey = methodKey state methodToCall

        // Every `Interlocked` overload documents
        // `<exception cref="NullReferenceException">The address of location1 is a null
        // pointer.</exception>`, and every managed body opens by dereferencing the location, so
        // the fault is that load and the runtime raises the parameterless exception. Without
        // this guard the null byref reaches `readManagedByref`, whose null case is a `failwith`
        // that terminates the interpreter instead of producing catchable guest state.
        //
        // Callers must already have popped the intrinsic's arguments and must NOT have advanced
        // the program counter: exception dispatch keys the handler search on the faulting
        // instruction's offset.
        let interlockedNullLocation (state : IlMachineState) : IntrinsicResult =
            IntrinsicResult.RaiseException (state, baseClassTypes.NullReferenceException, None)

        // Predicates shared by the Interlocked.CompareExchange / Interlocked.Exchange intrinsic arms,
        // which both dispatch by the (location, value, [comparand]) shape of the overload.
        let isReferenceTypeHandle (handle : ConcreteTypeHandle) : bool =
            IlMachineState.isReferenceTypeHandle baseClassTypes "Interlocked reference-type intrinsic" state handle

        let isNativeIntPrimitive (primitive : PrimitiveType) : bool =
            match primitive with
            | PrimitiveType.IntPtr
            | PrimitiveType.UIntPtr -> true
            | _ -> false

        // CIL widens Boolean (1-byte zero-extending) and Char (2-byte zero-extending)
        // to Int32 on the eval stack and `EvalStackValue.toCliTypeCoerced` already
        // rewraps from Int32 back to `CliType.Bool` / `CliType.Char`, so for atomic
        // Exchange / CompareExchange they behave identically to the scalar integers
        // here. Naming the predicate after the eval-stack shape rather than the spec
        // name "integer" keeps its contract truthful for the call sites that justify
        // dispatching to `executeScalarIntegerExchange` / `executeScalarInteger`.
        let isScalarIntegralLikePrimitive (primitive : PrimitiveType) : bool =
            match primitive with
            | PrimitiveType.Boolean
            | PrimitiveType.Char
            | PrimitiveType.SByte
            | PrimitiveType.Byte
            | PrimitiveType.Int16
            | PrimitiveType.UInt16
            | PrimitiveType.Int32
            | PrimitiveType.UInt32
            | PrimitiveType.Int64
            | PrimitiveType.UInt64 -> true
            | _ -> false

        // In general, some implementations are in:
        // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/coreclr/tools/Common/TypeSystem/IL/Stubs/UnsafeIntrinsics.cs#L192
        match methodToCall.DeclaringAssembly.Name, methodToCall.RequiredDeclaringType.Name, methodToCall.Name with
        | "System.Private.CoreLib", _, "get_IsSupported" when
            scalarOnlyFalseIsSupportedIntrinsics.Contains intrinsicKey.DeclaringTypeFullName
            ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [], MethodReturnType.Returns (ConcreteBool state.ConcreteTypes) -> ()
            | _ -> failwith $"bad signature for %s{formatMethodKey intrinsicKey}"

            state
            |> IlMachineState.pushToEvalStack (CliType.ofBool false) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", ("ReadOnlySpan`1" | "Span`1"), ".ctor" when
            intrinsicKey.ParameterShapes = [ "*" ; "System.Int32" ]
            && (intrinsicKey.DeclaringTypeFullName = "System.ReadOnlySpan`1"
                || intrinsicKey.DeclaringTypeFullName = "System.Span`1")
            ->
            writePointerBackedSpanConstructor
                loggerFactory
                baseClassTypes
                currentThread
                wasConstructing
                methodToCall
                state
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", ("ReadOnlySpan`1" | "Span`1"), "ToString" ->
            spanToString loggerFactory baseClassTypes currentThread methodToCall state
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "MemoryExtensions", "Equals" ->
            memoryExtensionsEquals baseClassTypes currentThread methodToCall state
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "SpanHelpers", "SequenceEqual" when
            isSpanHelpersByteSequenceEqual state methodToCall
            ->
            spanHelpersSequenceEqual baseClassTypes currentThread methodToCall state
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", ("Vector128" | "Vector256" | "Vector512"), "get_IsHardwareAccelerated"
        | "System.Private.CoreLib", "Vector", "get_IsHardwareAccelerated" when
            // System.Runtime.Intrinsics.Vector{128,256,512}.IsHardwareAccelerated and
            // System.Numerics.Vector.IsHardwareAccelerated are JIT intrinsic capability queries
            // whose IL bodies are recursive self-calls the JIT replaces with a constant. PawPrint
            // models a deterministic virtual CPU profile; the default scalar-only profile reports
            // them unavailable without consulting the host. The fully-qualified-name guard on the
            // "Vector" arm rejects any unrelated CoreLib type that happens to share the short name.
            methodToCall.RequiredDeclaringType.Name <> "Vector"
            || intrinsicKey.DeclaringTypeFullName = "System.Numerics.Vector"
            ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [], MethodReturnType.Returns (ConcreteBool state.ConcreteTypes) -> ()
            | _ ->
                failwith
                    $"bad signature for System.Private.CoreLib.%s{MethodOwner.describe methodToCall.Owner}.get_IsHardwareAccelerated"

            let isAccelerated =
                vectorAccelerationAvailable methodToCall.RequiredDeclaringType.Name state.HardwareIntrinsics

            IlMachineState.pushToEvalStack (CliType.ofBool isAccelerated) currentThread state
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "Object", "MemberwiseClone" ->
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/coreclr/System.Private.CoreLib/src/System/Object.CoreCLR.cs#L26-L45
            // The managed body allocates an uninitialised clone via the
            // `RuntimeHelpers.AllocateUninitializedClone` QCall and then raw-byte-copies the object
            // payload. PawPrint holds fields as `CliType` cells rather than bytes, so that
            // formulation is not the primitive available here — reproducing it would flatten every
            // field to bytes and lose the provenance of non-`Verbatim` cells. This is the same
            // reasoning `Array.Clone` records, and `IlMachineState.cloneObject` is the
            // corresponding primitive: a same-shaped object holding the same cells, which is
            // exactly the shallow copy the method promises.
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [],
              MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                          "System",
                                                                          "Object",
                                                                          generics)) when generics.IsEmpty -> ()
            | _ -> failwith "bad signature for System.Private.CoreLib.Object.MemberwiseClone"

            let receiver, state = IlMachineState.popEvalStack currentThread state

            match receiver with
            | EvalStackValue.ObjectRef addr ->
                let clone, state = IlMachineState.cloneObject addr state

                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.ObjectRef clone) currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> IntrinsicResult.Completed
            | EvalStackValue.NullObjectRef
            | EvalStackValue.ManagedPointer ManagedPointerSource.Null ->
                // The body's first act is `ObjectHandleOnStack.Create(ref clone)` over `this`,
                // followed by `this.GetRawData()`, so a null receiver faults and the runtime
                // translates it into `NullReferenceException`.
                //
                // `MemberwiseClone` is `protected internal`, so C# only ever calls it on `this`,
                // which is non-null in an instance method; and `callvirt`'s own null check would
                // fire first in any case (see `Array.Clone`'s matching arm). Only hand-written IL
                // reaches this.
                IntrinsicResult.RaiseException (state, baseClassTypes.NullReferenceException, None)
            | other -> failwith $"Object.MemberwiseClone: expected an object reference receiver, got %O{other}"
        | "System.Private.CoreLib", "Object", "GetType" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [],
              MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                          "System",
                                                                          "Type",
                                                                          generics)) when generics.IsEmpty -> ()
            | _ -> failwith "bad signature Object.GetType"

            let arg, state = IlMachineState.popEvalStack currentThread state

            // A null receiver raises `NullReferenceException`: `Object.GetType`'s body starts
            // `MethodTable* pMT = RuntimeHelpers.GetMethodTable(this)`, which the JIT expands to a
            // bare indirection at offset 0, so the access faults and the runtime translates it.
            //
            // Not reachable from C#, which emits `callvirt` for `GetType()` — and
            // `executeCallvirt`'s own null check fires before `callMethod` is entered, so the
            // intrinsic never sees a null receiver. `constrained.callvirt` on a value type boxes
            // first, so it cannot produce one either. Only hand-written
            // `ldnull; call instance Object::GetType()` gets here, and there is no ilasm in this
            // repo to write that with; `NullReceiverGuards.cs` pins the guard instead.
            // `None` means the receiver was null.
            let receiver : (ConcreteTypeHandle * IlMachineState) option =
                // Normal Object.GetType dispatch arrives here with an ObjectRef. The managed-pointer
                // arms are deliberately defensive for future receiver shapes and direct intrinsic use;
                // constrained.callvirt on value types boxes before dispatching this intrinsic.
                match arg with
                | EvalStackValue.ObjectRef addr ->
                    Some (ManagedHeap.getObjectConcreteType addr state.ManagedHeap, state)
                | EvalStackValue.ManagedPointer ManagedPointerSource.Null
                | EvalStackValue.NullObjectRef -> None
                | EvalStackValue.ManagedPointer ptr ->
                    match IlMachineState.readManagedByref baseClassTypes state ptr with
                    | CliType.ObjectRef (Some addr) ->
                        Some (ManagedHeap.getObjectConcreteType addr state.ManagedHeap, state)
                    | CliType.ObjectRef None -> None
                    | CliType.ValueType valueType -> Some (valueType.Declared, state)
                    | other -> failwith $"Object.GetType: expected object ref or value type receiver, got %O{other}"
                | other -> failwith $"Object.GetType: expected object ref or managed pointer receiver, got %O{other}"

            match receiver with
            | None -> IntrinsicResult.RaiseException (state, baseClassTypes.NullReferenceException, None)
            | Some (concreteType, state) ->

            let runtimeTypeAddr, state =
                IlMachineState.getOrAllocateType
                    loggerFactory
                    baseClassTypes
                    (RuntimeTypeHandleTarget.Closed concreteType)
                    state

            state
            |> IlMachineState.pushToEvalStack (CliType.ObjectRef (Some runtimeTypeAddr)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "RuntimeHelpers", "GetMethodTable" ->
            match methodToCall.Signature.ParameterTypes with
            | [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Object ] -> ()
            | _ -> failwith "bad signature RuntimeHelpers.GetMethodTable"

            match methodToCall.Signature.ReturnType with
            | MethodReturnType.Returns (ConcreteTypeHandle.Pointer (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                                                      "System.Runtime.CompilerServices",
                                                                                                      "MethodTable",
                                                                                                      generics))) when
                generics.IsEmpty
                ->
                ()
            | _ -> failwith "bad return type RuntimeHelpers.GetMethodTable"

            let arg, state = IlMachineState.popEvalStack currentThread state

            match arg with
            | EvalStackValue.NullObjectRef ->
                // The JIT expands this to a bare load at offset 0 (`gtNewMethodTableLookup`, which
                // asserts `VPTR_OFFS == 0`), so a null argument faults into
                // `NullReferenceException`.
                //
                // Not reachable from C#: the method is `internal` to CoreLib and returns
                // `MethodTable*`, an internal type, so it cannot be named from a test source — not
                // even via `[UnsafeAccessor]`, whose signature match would need that return type.
                // Every CoreLib caller null-guards first, except the non-generic
                // `MemoryMarshal.GetArrayDataReference(Array)`, which PawPrint does not yet handle
                // at all (its arm assumes a generic overload).
                IntrinsicResult.RaiseException (state, baseClassTypes.NullReferenceException, None)
            | _ ->

            let addr =
                match arg with
                | EvalStackValue.ObjectRef addr -> addr
                | other -> failwith $"RuntimeHelpers.GetMethodTable: expected ObjectRef, got %O{other}"

            let concreteType = ManagedHeap.getObjectConcreteType addr state.ManagedHeap

            state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed concreteType)))
                currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "Unsafe", "AsPointer" ->
            // Method signature: 1 generic parameter, we take a Byref of that parameter, and return a TypeDefn.Pointer(Void)
            let arg, state = IlMachineState.popEvalStack currentThread state

            let toPush =
                match arg with
                | EvalStackValue.ManagedPointer ptr -> CliRuntimePointer.Managed ptr
                | x -> failwith $"TODO: Unsafe.AsPointer(%O{x})"

            IlMachineState.pushToEvalStack (CliType.RuntimePointer toPush) currentThread state
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "Unsafe", "SkipInit" ->
            // `SkipInit<T>(out T)` is a JIT intrinsic that deliberately leaves
            // the byref target untouched. PawPrint's storage is already
            // deterministic, so the only observable effect is consuming the
            // byref argument and returning void.
            let t =
                match Seq.toList methodToCall.Generics with
                | [ t ] -> t
                | _ -> failwith "bad generics Unsafe.SkipInit"

            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteByref tParam ], MethodReturnType.Void when tParam = t -> ()
            | _ -> failwith $"bad signature Unsafe.SkipInit: %A{methodToCall.Signature}"

            let arg, state = IlMachineState.popEvalStack currentThread state

            match arg with
            | EvalStackValue.ManagedPointer _ -> ()
            | other -> failwith $"Unsafe.SkipInit: expected managed byref argument, got %O{other}"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "Unsafe", "AsRef" ->
            // `AsRef<T>(ref readonly T)` and `AsRef<T>(void* source)` are JIT
            // intrinsics. The CoreLib bodies in this runtime throw
            // PlatformNotSupportedException; the intended intrinsic semantics
            // are the address-preserving `ldarg.0; ret`.
            //
            // The `void*` overload is invoked by BCL code like
            // `MemoryMarshal.GetNonNullPinnableReference` which fabricates
            // `Unsafe.AsRef<T>((void*)1)` for empty spans so the subsequent
            // `fixed` pins to a non-null pointer. Translate the native int back
            // through the managed-pointer view, normalising `0L` to `Null` and
            // existing managed-pointer provenance back to its underlying
            // source; raw verbatim bits become a `NativeIntPlaceholder` whose
            // contract is "must never be dereferenced".
            let t =
                match Seq.toList methodToCall.Generics with
                | [ t ] -> t
                | _ -> failwith "bad generics Unsafe.AsRef"

            let isByrefOverload =
                match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
                | [ ConcreteByref tParam ], MethodReturnType.Returns (ConcreteByref tRet) when tParam = t && tRet = t ->
                    true
                | [ ConcretePointer _ ], MethodReturnType.Returns (ConcreteByref tRet) when tRet = t -> false
                | _ -> failwith $"TODO: Unsafe.AsRef unsupported signature %A{methodToCall.Signature.ParameterTypes}"

            let arg, state = IlMachineState.popEvalStack currentThread state

            let toPush =
                if isByrefOverload then
                    match arg with
                    | EvalStackValue.ManagedPointer ptr -> EvalStackValue.ManagedPointer ptr
                    | x -> failwith $"TODO: Unsafe.AsRef(ref readonly T) on %O{x}"
                else
                    let placeholderOf (bits : int64) =
                        if bits = 0L then
                            ManagedPointerSource.Null
                        else
                            ManagedPointerSource.NativeIntPlaceholder bits

                    match arg with
                    | EvalStackValue.ManagedPointer ptr -> EvalStackValue.ManagedPointer ptr
                    | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr) -> EvalStackValue.ManagedPointer ptr
                    | EvalStackValue.NativeInt (NativeIntSource.Verbatim bits) ->
                        EvalStackValue.ManagedPointer (placeholderOf bits)
                    | EvalStackValue.Int32 (Int32Source.Verbatim bits) ->
                        EvalStackValue.ManagedPointer (placeholderOf (int64 bits))
                    | EvalStackValue.Int64 (Int64Source.Verbatim bits) ->
                        EvalStackValue.ManagedPointer (placeholderOf bits)
                    | EvalStackValue.NullObjectRef -> EvalStackValue.ManagedPointer ManagedPointerSource.Null
                    | x -> failwith $"TODO: Unsafe.AsRef(void*) on %O{x}"

            state
            |> IlMachineState.pushToEvalStack' toPush currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "Unsafe", "NullRef" ->
            // CoreCLR's UNSAFE__BYREF_NULLREF intrinsic replaces the CoreLib
            // body with a null managed byref (`ldc.i4.0; conv.u; ret`).
            let t =
                let generics = Seq.toList methodToCall.Generics

                match generics with
                | [ t ] -> t
                | _ -> failwith $"bad generics Unsafe.NullRef: expected exactly one generic argument, got %A{generics}"

            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [], MethodReturnType.Returns (ConcreteByref ret) when ret = t -> ()
            | _ ->
                failwith
                    $"bad signature Unsafe.NullRef: expected no parameters and byref return matching %O{t}, got %A{methodToCall.Signature}"

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ManagedPointerSource.Null) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "Unsafe", "IsNullRef" ->
            // The JIT intrinsic compares the byref argument against the null
            // managed byref.
            let t =
                let generics = Seq.toList methodToCall.Generics

                match generics with
                | [ t ] -> t
                | _ ->
                    failwith $"bad generics Unsafe.IsNullRef: expected exactly one generic argument, got %A{generics}"

            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteByref param ], MethodReturnType.Returns (ConcreteBool state.ConcreteTypes) when param = t -> ()
            | _ ->
                failwith
                    $"bad signature Unsafe.IsNullRef: expected one byref parameter matching %O{t} and bool return, got %A{methodToCall.Signature}"

            let arg, state = IlMachineState.popEvalStack currentThread state

            let isNullRef =
                match arg with
                | EvalStackValue.ManagedPointer ManagedPointerSource.Null -> true
                | EvalStackValue.ManagedPointer _ -> false
                | other -> failwith $"Unsafe.IsNullRef: expected managed byref argument, got %O{other}"

            state
            |> IlMachineState.pushToEvalStack (CliType.ofBool isNullRef) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "Interlocked", ("Add" | "ExchangeAdd") ->
            // `Add` returns the newly-stored sum; the private `ExchangeAdd`
            // primitive returns the original value. The read-modify-write
            // happens inside one intrinsic dispatch, so the scheduler cannot
            // interleave another guest thread between the read and write.
            let returnsOriginalValue = methodToCall.Name = "ExchangeAdd"

            let executeInt32 (operation : string) (state : IlMachineState) : IntrinsicResult =
                let valueArg, state = IlMachineState.popEvalStack currentThread state
                let byrefArg, state = IlMachineState.popEvalStack currentThread state

                let value = int32ValueArgument operation valueArg

                match popManagedByrefArgument operation byrefArg with
                | ManagedPointerSource.Null -> interlockedNullLocation state
                | byrefSrc ->
                    let currentValue = IlMachineState.readManagedByref baseClassTypes state byrefSrc

                    let current =
                        match EvalStackValue.ofCliType currentValue with
                        | EvalStackValue.Int32 (Int32Source.Verbatim i) -> i
                        | other -> failwith $"%s{operation}: expected int32 in target location, got %O{other}"

                    // From the docs:
                    // This method handles an overflow condition by wrapping:
                    // if the value at location1 is Int32.MaxValue and value is 1, the result is Int32.MinValue;
                    // if value is 2, the result is (Int32.MinValue + 1); and so on.
                    // No exception is thrown.
                    let updated = uint32<int32> current + uint32<int32> value |> int32<uint32>

                    let state =
                        IlMachineState.writeManagedByrefWithBase
                            baseClassTypes
                            state
                            byrefSrc
                            (EvalStackValue.toCliTypeCoerced
                                currentValue
                                (EvalStackValue.Int32 (Int32Source.Verbatim updated)))

                    let result = if returnsOriginalValue then current else updated

                    state
                    |> IlMachineState.pushToEvalStack'
                        (EvalStackValue.Int32 (Int32Source.Verbatim result))
                        currentThread
                    |> IlMachineState.advanceProgramCounter currentThread
                    |> IntrinsicResult.Completed

            let executeInt64 (operation : string) (state : IlMachineState) : IntrinsicResult =
                let valueArg, state = IlMachineState.popEvalStack currentThread state
                let byrefArg, state = IlMachineState.popEvalStack currentThread state

                let value = int64ValueArgument operation valueArg

                match popManagedByrefArgument operation byrefArg with
                | ManagedPointerSource.Null -> interlockedNullLocation state
                | byrefSrc ->
                    let currentValue = IlMachineState.readManagedByref baseClassTypes state byrefSrc

                    let current =
                        match EvalStackValue.ofCliType currentValue with
                        | EvalStackValue.Int64 i -> i
                        | other -> failwith $"%s{operation}: expected int64 in target location, got %O{other}"

                    // From the docs:
                    // This method handles an overflow condition by wrapping:
                    // if the value at location1 is Int64.MaxValue and value is 1, the result is Int64.MinValue;
                    // if value is 2, the result is (Int64.MinValue + 1); and so on.
                    // No exception is thrown.
                    let updated =
                        match current, value with
                        | Int64Source.Verbatim current, Int64Source.Verbatim value ->
                            uint64<int64> current + uint64<int64> value
                            |> int64<uint64>
                            |> Int64Source.Verbatim
                        | _, _ ->
                            // `Interlocked.And` / `Or` route through `Int64Source.bitAnd` /
                            // `bitOr`, which synthesise hash bits for a pointer-derived
                            // operand. Addition has no `Int64Source` counterpart yet, so a
                            // pointer-derived location or addend stops here rather than
                            // silently dropping provenance.
                            failwith
                                $"TODO: %s{operation} on int64 needs both operands verbatim; got location %O{current} and value %O{value}"

                    let state =
                        IlMachineState.writeManagedByrefWithBase
                            baseClassTypes
                            state
                            byrefSrc
                            (EvalStackValue.toCliTypeCoerced currentValue (EvalStackValue.Int64 updated))

                    let result = if returnsOriginalValue then current else updated

                    state
                    |> IlMachineState.pushToEvalStack' (EvalStackValue.Int64 result) currentThread
                    |> IlMachineState.advanceProgramCounter currentThread
                    |> IntrinsicResult.Completed

            let operation = $"Interlocked.%s{methodToCall.Name}"

            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteByref (ConcreteInt32 state.ConcreteTypes) ; ConcreteInt32 state.ConcreteTypes ],
              MethodReturnType.Returns (ConcreteInt32 state.ConcreteTypes)
            | [ ConcreteByref (ConcreteUInt32 state.ConcreteTypes) ; ConcreteUInt32 state.ConcreteTypes ],
              MethodReturnType.Returns (ConcreteUInt32 state.ConcreteTypes) -> executeInt32 operation state
            | [ ConcreteByref (ConcreteInt64 state.ConcreteTypes) ; ConcreteInt64 state.ConcreteTypes ],
              MethodReturnType.Returns (ConcreteInt64 state.ConcreteTypes)
            | [ ConcreteByref (ConcreteUInt64 state.ConcreteTypes) ; ConcreteUInt64 state.ConcreteTypes ],
              MethodReturnType.Returns (ConcreteUInt64 state.ConcreteTypes) -> executeInt64 operation state
            | _ -> IntrinsicResult.Unrecognised

        | "System.Private.CoreLib", "Interlocked", ("And" | "Or") ->
            // Both return the *original* value at the location, not the combined one.
            // The read-modify-write happens inside one intrinsic dispatch, so the
            // scheduler cannot interleave another guest thread between the read and
            // the write; that is what makes these atomic, and it is why we implement
            // them here rather than letting the shipped CAS-loop IL be interpreted.
            // Only the Int32 and Int64 overloads are [Intrinsic]; the UInt32/UInt64
            // ones are `Unsafe.As`-to-signed forwarders whose IL runs normally and
            // bottoms out in these two shapes, so they deliberately get no arm.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Threading/Interlocked.cs#L583-L710
            let isOr = methodToCall.Name = "Or"

            let executeInt32 (operation : string) (state : IlMachineState) : IntrinsicResult =
                let valueArg, state = IlMachineState.popEvalStack currentThread state
                let byrefArg, state = IlMachineState.popEvalStack currentThread state

                let value = int32ValueArgument operation valueArg

                match popManagedByrefArgument operation byrefArg with
                | ManagedPointerSource.Null -> interlockedNullLocation state
                | byrefSrc ->
                    let currentValue = IlMachineState.readManagedByref baseClassTypes state byrefSrc

                    let current =
                        match EvalStackValue.ofCliType currentValue with
                        | EvalStackValue.Int32 (Int32Source.Verbatim i) -> i
                        | other -> failwith $"%s{operation}: expected int32 in target location, got %O{other}"

                    let updated = if isOr then current ||| value else current &&& value

                    let state =
                        IlMachineState.writeManagedByrefWithBase
                            baseClassTypes
                            state
                            byrefSrc
                            (EvalStackValue.toCliTypeCoerced
                                currentValue
                                (EvalStackValue.Int32 (Int32Source.Verbatim updated)))

                    state
                    |> IlMachineState.pushToEvalStack'
                        (EvalStackValue.Int32 (Int32Source.Verbatim current))
                        currentThread
                    |> IlMachineState.advanceProgramCounter currentThread
                    |> IntrinsicResult.Completed

            let executeInt64 (operation : string) (state : IlMachineState) : IntrinsicResult =
                let valueArg, state = IlMachineState.popEvalStack currentThread state
                let byrefArg, state = IlMachineState.popEvalStack currentThread state

                let value = int64ValueArgument operation valueArg

                match popManagedByrefArgument operation byrefArg with
                | ManagedPointerSource.Null -> interlockedNullLocation state
                | byrefSrc ->
                    let currentValue = IlMachineState.readManagedByref baseClassTypes state byrefSrc

                    let current =
                        match EvalStackValue.ofCliType currentValue with
                        | EvalStackValue.Int64 i -> i
                        | other -> failwith $"%s{operation}: expected int64 in target location, got %O{other}"

                    // Defer to the same provenance-aware bit ops the `and`/`or` IL opcodes use
                    // (NullaryIlOp.fs), so that masking a pointer-derived int64 synthesises hash
                    // bits rather than failing.
                    let combine = if isOr then Int64Source.bitOr else Int64Source.bitAnd

                    let updated, counters = combine operation current value state.PointerHashState

                    let state =
                        { state with
                            PointerHashState = counters
                        }

                    let state =
                        IlMachineState.writeManagedByrefWithBase
                            baseClassTypes
                            state
                            byrefSrc
                            (EvalStackValue.toCliTypeCoerced currentValue (EvalStackValue.Int64 updated))

                    state
                    |> IlMachineState.pushToEvalStack' (EvalStackValue.Int64 current) currentThread
                    |> IlMachineState.advanceProgramCounter currentThread
                    |> IntrinsicResult.Completed

            let operation = $"Interlocked.%s{methodToCall.Name}"

            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteByref (ConcreteInt32 state.ConcreteTypes) ; ConcreteInt32 state.ConcreteTypes ],
              MethodReturnType.Returns (ConcreteInt32 state.ConcreteTypes) -> executeInt32 operation state
            | [ ConcreteByref (ConcreteInt64 state.ConcreteTypes) ; ConcreteInt64 state.ConcreteTypes ],
              MethodReturnType.Returns (ConcreteInt64 state.ConcreteTypes) -> executeInt64 operation state
            | _ -> IntrinsicResult.Unrecognised

        | "System.Private.CoreLib", "Interlocked", "MemoryBarrier" ->
            // [Intrinsic] public static void MemoryBarrier() => MemoryBarrier();
            // Same shape as Volatile.{Read,Write}Barrier (below): the managed body is
            // infinite self-recursion and the JIT replaces the call with the
            // appropriate processor fence. PawPrint single-steps a deterministic
            // virtual CPU, so there is no host memory reordering for a fence to
            // constrain; the no-op is correct for the same reason as the `volatile.`
            // IL prefix (NullaryIlOp.fs). Cannot live in safeIntrinsics because the
            // IL would loop forever.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Threading/Interlocked.cs#L713-L714
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [], MethodReturnType.Void -> ()
            | _ -> failwith $"Interlocked.MemoryBarrier: unexpected signature %A{methodToCall.Signature}"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed

        | "System.Private.CoreLib", "Interlocked", "CompareExchange" ->
            // The native-int-shaped overloads need their own path: the shipped IL wrappers do
            // `Unsafe.As<_, long>` and delegate to the Int64 overload, which would destroy our
            // NativeIntSource provenance.
            // Narrow scalar and reference-type overloads are JIT intrinsic boundaries too; handle
            // those primitives here instead of executing their Unsafe.As / InternalCall wrappers.
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/Threading/Interlocked.cs#L452
            let executeScalarInteger (operation : string) (state : IlMachineState) : IntrinsicResult =
                let comparand, state = IlMachineState.popEvalStack currentThread state
                let value, state = IlMachineState.popEvalStack currentThread state
                let byrefArg, state = IlMachineState.popEvalStack currentThread state

                match popManagedByrefArgument operation byrefArg with
                | ManagedPointerSource.Null -> interlockedNullLocation state
                | byrefSrc ->
                    let currentValue = IlMachineState.readManagedByref baseClassTypes state byrefSrc
                    let currentEval = EvalStackValue.ofCliType currentValue
                    let valueCli = EvalStackValue.toCliTypeCoerced currentValue value
                    let comparandCli = EvalStackValue.toCliTypeCoerced currentValue comparand

                    // The intrinsic bypasses normal method-frame construction, so coerce the eval-stack
                    // operands to the signedness/width of the overload before comparing and writing.
                    let state =
                        if
                            EvalStackValueComparisons.ceq
                                state.PointerHashState
                                currentEval
                                (EvalStackValue.ofCliType comparandCli)
                        then
                            IlMachineState.writeManagedByrefWithBase baseClassTypes state byrefSrc valueCli
                        else
                            state

                    state
                    |> IlMachineState.pushToEvalStack currentValue currentThread
                    |> IlMachineState.advanceProgramCounter currentThread
                    |> IntrinsicResult.Completed

            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteByref (ConcretePrimitive state.ConcreteTypes locationPrimitive)
                ConcretePrimitive state.ConcreteTypes valuePrimitive
                ConcretePrimitive state.ConcreteTypes comparandPrimitive ],
              MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes returnPrimitive) when
                isNativeIntPrimitive locationPrimitive
                && locationPrimitive = valuePrimitive
                && locationPrimitive = comparandPrimitive
                && locationPrimitive = returnPrimitive
                ->

                let comparand, state = IlMachineState.popEvalStack currentThread state
                let value, state = IlMachineState.popEvalStack currentThread state
                let byrefArg, state = IlMachineState.popEvalStack currentThread state

                match popManagedByrefArgument "Interlocked.CompareExchange(ref native-int,...)" byrefArg with
                | ManagedPointerSource.Null -> interlockedNullLocation state
                | byrefSrc ->
                    // Eval-stack IntPtr/UIntPtr arguments are flattened to the primitive by the push
                    // boundary (see EvalStackValue.ofCliType), so a UserDefinedValueType IntPtr or
                    // UIntPtr is unreachable here by invariant.
                    let toNativeIntSource (v : EvalStackValue) : NativeIntSource =
                        match v with
                        | EvalStackValue.NativeInt src -> src
                        | EvalStackValue.Int64 (Int64Source.Verbatim i) -> NativeIntSource.Verbatim i
                        | EvalStackValue.Int32 (Int32Source.Verbatim i) -> NativeIntSource.Verbatim (int64<int> i)
                        | EvalStackValue.ManagedPointer src -> NativeIntSource.ManagedPointer src
                        | EvalStackValue.NullObjectRef -> NativeIntSource.ManagedPointer ManagedPointerSource.Null
                        | other ->
                            failwith
                                $"Interlocked.CompareExchange(ref native-int,...): unexpected native-int-shaped eval stack value %O{other}"

                    let comparandSrc = toNativeIntSource comparand
                    let valueSrc = toNativeIntSource value

                    let currentValue = IlMachineState.readManagedByref baseClassTypes state byrefSrc

                    // `ref IntPtr` / `ref UIntPtr` derefs to a wrapper struct. Route the read/write through
                    // the eval-stack flatten/rewrap boundary: `ofCliType` peels the primitive-like
                    // wrapper to `NativeInt`, and `toCliTypeCoerced` reconstructs the wrapper shape
                    // on write. The primitive-like registry is the single source of truth for shape.
                    let currentSrc =
                        match EvalStackValue.ofCliType currentValue with
                        | EvalStackValue.NativeInt src -> src
                        | EvalStackValue.Int64 (Int64Source.Verbatim i) -> NativeIntSource.Verbatim i
                        | EvalStackValue.Int32 (Int32Source.Verbatim i) -> NativeIntSource.Verbatim (int64<int> i)
                        | other ->
                            failwith
                                $"Interlocked.CompareExchange(ref native-int,...): expected NativeInt at byref target, got %O{other}"

                    let state =
                        if NativeIntSourceComparison.equalsForCli state.PointerHashState currentSrc comparandSrc then
                            let newValue =
                                EvalStackValue.toCliTypeCoerced currentValue (EvalStackValue.NativeInt valueSrc)

                            IlMachineState.writeManagedByrefWithBase baseClassTypes state byrefSrc newValue
                        else
                            state

                    state
                    |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt currentSrc) currentThread
                    |> IlMachineState.advanceProgramCounter currentThread
                    |> IntrinsicResult.Completed
            | [ ConcreteByref (ConcretePrimitive state.ConcreteTypes locationPrimitive)
                ConcretePrimitive state.ConcreteTypes valuePrimitive
                ConcretePrimitive state.ConcreteTypes comparandPrimitive ],
              MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes returnPrimitive) when
                isScalarIntegralLikePrimitive locationPrimitive
                && locationPrimitive = valuePrimitive
                && locationPrimitive = comparandPrimitive
                && locationPrimitive = returnPrimitive
                ->
                executeScalarInteger "Interlocked.CompareExchange" state
            | [ ConcreteByref locationType ; valueType ; comparandType ], MethodReturnType.Returns returnType when
                locationType = valueType
                && locationType = comparandType
                && locationType = returnType
                && isReferenceTypeHandle locationType
                ->
                // Reference-typed CompareExchange overloads are JIT/runtime intrinsic boundaries
                // in CoreLib. Implement the object-reference primitive directly instead of trying
                // to execute the generic Unsafe.As<T, object> path or the non-generic
                // CompareExchangeObject InternalCall boundary.
                let comparand, state = IlMachineState.popEvalStack currentThread state
                let value, state = IlMachineState.popEvalStack currentThread state
                let byrefArg, state = IlMachineState.popEvalStack currentThread state

                match popManagedByrefArgument "Interlocked.CompareExchange<T>" byrefArg with
                | ManagedPointerSource.Null -> interlockedNullLocation state
                | byrefSrc ->
                    let currentValue = IlMachineState.readManagedByref baseClassTypes state byrefSrc

                    let objectTarget (argName : string) (value : CliType) : ManagedHeapAddress option =
                        match value with
                        | CliType.ObjectRef target -> target
                        | other ->
                            failwith
                                $"Interlocked.CompareExchange<T>: expected reference-type %s{argName}, got %O{other}"

                    let currentTarget = objectTarget "location" currentValue

                    let valueCli = EvalStackValue.toCliTypeCoerced currentValue value

                    let comparandCli = EvalStackValue.toCliTypeCoerced currentValue comparand

                    let comparandTarget = objectTarget "comparand" comparandCli

                    let state =
                        if currentTarget = comparandTarget then
                            IlMachineState.writeManagedByrefWithBase baseClassTypes state byrefSrc valueCli
                        else
                            state

                    state
                    |> IlMachineState.pushToEvalStack currentValue currentThread
                    |> IlMachineState.advanceProgramCounter currentThread
                    |> IntrinsicResult.Completed
            | [ ConcreteByref locationType ; valueType ; comparandType ], MethodReturnType.Returns returnType when
                locationType = valueType
                && locationType = comparandType
                && locationType = returnType
                ->
                // The correctly-shaped overloads the arms above did not claim: everything whose
                // location/value/comparand/return are one and the same type but which is neither
                // a scalar-integral or native-int primitive nor a reference type. That leaves
                // enums, `Single`/`Double`, and unsupported value types; only the enum case is
                // implemented here.
                //
                // `CompareExchange<TEnum>`'s shipped managed body bitcasts to the unsigned
                // integer of the same size and calls that overload (Interlocked.cs:507-539), but
                // `[Intrinsic]` means the JIT never runs that IL, and running it here would go
                // through `Unsafe.BitCast`/`Unsafe.As`. We don't need to: an enum's eval-stack
                // form *is* its underlying integer (`PrimitiveLikeKind.EnumLike`), and
                // `readManagedByref`/`toCliTypeCoerced` peel and rewrap the `value__` slot around
                // it, so the scalar-integer path already compares and writes at the underlying
                // width.
                //
                // `isEnumValueType` asks CoreCLR's question — is the immediate base
                // `System.Enum`? — rather than inspecting the storage's structural shape, because
                // the contract being implemented is about `T`, not about what happens to be in
                // the location. Its returned state carries any concretization the base-type walk
                // performed, so it is threaded on rather than discarded.
                let state, isEnum =
                    IlMachineState.isEnumValueType loggerFactory baseClassTypes state locationType

                if isEnum then
                    executeScalarInteger "Interlocked.CompareExchange<TEnum>" state
                else
                    // `Single`/`Double` are not yet intrinsified: their shipped IL bodies
                    // reinterpret-cast to the integer overloads, so falling through would either
                    // re-enter this intrinsic path or lose the bit-level shape of the
                    // floating-point value. A `T` that is none of the supported kinds should
                    // instead raise `NotSupportedException` (Interlocked.cs:502-505). Both want
                    // their own arm; until then, failing loudly beats mistranslating either.
                    IntrinsicResult.Unrecognised
            | _ ->
                // A signature shape this intrinsic does not recognise at all — the four types are
                // not all the same, or the parameter count is wrong.
                IntrinsicResult.Unrecognised
        | "System.Private.CoreLib", "Interlocked", "Exchange" ->
            // Same intrinsic-boundary motivation as CompareExchange: the shipped CoreLib
            // bodies for Exchange ride Unsafe.As / InternalCall paths that would either
            // destroy NativeIntSource provenance for IntPtr/UIntPtr or re-enter this
            // intrinsic at the wrong width. Implement the primitive directly.
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/Threading/Interlocked.cs#L80
            let executeScalarIntegerExchange (operation : string) (state : IlMachineState) : IntrinsicResult =
                let value, state = IlMachineState.popEvalStack currentThread state
                let byrefArg, state = IlMachineState.popEvalStack currentThread state

                match popManagedByrefArgument operation byrefArg with
                | ManagedPointerSource.Null -> interlockedNullLocation state
                | byrefSrc ->
                    let currentValue = IlMachineState.readManagedByref baseClassTypes state byrefSrc
                    let valueCli = EvalStackValue.toCliTypeCoerced currentValue value

                    // The intrinsic bypasses normal method-frame construction, so coerce the
                    // eval-stack value to the signedness/width of the overload before writing.
                    let state =
                        IlMachineState.writeManagedByrefWithBase baseClassTypes state byrefSrc valueCli

                    state
                    |> IlMachineState.pushToEvalStack currentValue currentThread
                    |> IlMachineState.advanceProgramCounter currentThread
                    |> IntrinsicResult.Completed

            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteByref (ConcretePrimitive state.ConcreteTypes locationPrimitive)
                ConcretePrimitive state.ConcreteTypes valuePrimitive ],
              MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes returnPrimitive) when
                isNativeIntPrimitive locationPrimitive
                && locationPrimitive = valuePrimitive
                && locationPrimitive = returnPrimitive
                ->

                let value, state = IlMachineState.popEvalStack currentThread state
                let byrefArg, state = IlMachineState.popEvalStack currentThread state

                match popManagedByrefArgument "Interlocked.Exchange(ref native-int,...)" byrefArg with
                | ManagedPointerSource.Null -> interlockedNullLocation state
                | byrefSrc ->
                    // Eval-stack IntPtr/UIntPtr arguments are flattened to the primitive by the push
                    // boundary (see EvalStackValue.ofCliType), so a UserDefinedValueType IntPtr or
                    // UIntPtr is unreachable here by invariant.
                    let toNativeIntSource (v : EvalStackValue) : NativeIntSource =
                        match v with
                        | EvalStackValue.NativeInt src -> src
                        | EvalStackValue.Int64 (Int64Source.Verbatim i) -> NativeIntSource.Verbatim i
                        | EvalStackValue.Int32 (Int32Source.Verbatim i) -> NativeIntSource.Verbatim (int64<int> i)
                        | EvalStackValue.ManagedPointer src -> NativeIntSource.ManagedPointer src
                        | EvalStackValue.NullObjectRef -> NativeIntSource.ManagedPointer ManagedPointerSource.Null
                        | other ->
                            failwith
                                $"Interlocked.Exchange(ref native-int,...): unexpected native-int-shaped eval stack value %O{other}"

                    let valueSrc = toNativeIntSource value

                    let currentValue = IlMachineState.readManagedByref baseClassTypes state byrefSrc

                    // `ref IntPtr` / `ref UIntPtr` derefs to a wrapper struct. Route the read/write through
                    // the eval-stack flatten/rewrap boundary: `ofCliType` peels the primitive-like
                    // wrapper to `NativeInt`, and `toCliTypeCoerced` reconstructs the wrapper shape
                    // on write. The primitive-like registry is the single source of truth for shape.
                    let currentSrc =
                        match EvalStackValue.ofCliType currentValue with
                        | EvalStackValue.NativeInt src -> src
                        | EvalStackValue.Int64 (Int64Source.Verbatim i) -> NativeIntSource.Verbatim i
                        | EvalStackValue.Int32 (Int32Source.Verbatim i) -> NativeIntSource.Verbatim (int64<int> i)
                        | other ->
                            failwith
                                $"Interlocked.Exchange(ref native-int,...): expected NativeInt at byref target, got %O{other}"

                    let newValue =
                        EvalStackValue.toCliTypeCoerced currentValue (EvalStackValue.NativeInt valueSrc)

                    let state =
                        IlMachineState.writeManagedByrefWithBase baseClassTypes state byrefSrc newValue

                    state
                    |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt currentSrc) currentThread
                    |> IlMachineState.advanceProgramCounter currentThread
                    |> IntrinsicResult.Completed
            | [ ConcreteByref (ConcretePrimitive state.ConcreteTypes locationPrimitive)
                ConcretePrimitive state.ConcreteTypes valuePrimitive ],
              MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes returnPrimitive) when
                isScalarIntegralLikePrimitive locationPrimitive
                && locationPrimitive = valuePrimitive
                && locationPrimitive = returnPrimitive
                ->
                executeScalarIntegerExchange "Interlocked.Exchange" state
            | [ ConcreteByref locationType ; valueType ], MethodReturnType.Returns returnType when
                locationType = valueType
                && locationType = returnType
                && isReferenceTypeHandle locationType
                ->
                // Reference-typed Exchange overloads are JIT/runtime intrinsic boundaries
                // in CoreLib. Implement the object-reference primitive directly instead of
                // trying to execute the generic Unsafe.As<T, object> path.
                let value, state = IlMachineState.popEvalStack currentThread state
                let byrefArg, state = IlMachineState.popEvalStack currentThread state

                match popManagedByrefArgument "Interlocked.Exchange<T>" byrefArg with
                | ManagedPointerSource.Null -> interlockedNullLocation state
                | byrefSrc ->
                    let currentValue = IlMachineState.readManagedByref baseClassTypes state byrefSrc

                    let valueCli = EvalStackValue.toCliTypeCoerced currentValue value

                    let state =
                        IlMachineState.writeManagedByrefWithBase baseClassTypes state byrefSrc valueCli

                    state
                    |> IlMachineState.pushToEvalStack currentValue currentThread
                    |> IlMachineState.advanceProgramCounter currentThread
                    |> IntrinsicResult.Completed
            | [ ConcreteByref locationType ; valueType ], MethodReturnType.Returns returnType when
                locationType = valueType && locationType = returnType
                ->
                // The enum instantiation of `Exchange<T>`, exactly mirroring the `CompareExchange`
                // arm above: same `[Intrinsic]`-so-the-IL-never-runs argument, same
                // `Unsafe.BitCast`-to-underlying-integer managed body we decline to execute
                // (Interlocked.cs:257-286), and same reason the scalar-integer path is already
                // right — an enum's eval-stack form is its underlying integer
                // (`PrimitiveLikeKind.EnumLike`). See that arm for the full rationale, including
                // why the nominal `isEnumValueType` question is the one being asked and why its
                // returned state is threaded on.
                let state, isEnum =
                    IlMachineState.isEnumValueType loggerFactory baseClassTypes state locationType

                if isEnum then
                    executeScalarIntegerExchange "Interlocked.Exchange<TEnum>" state
                else
                    // `Single`/`Double`, and the `T` that should raise `NotSupportedException`
                    // (Interlocked.cs:252-255). Both want their own arm, matching the
                    // CompareExchange precedent above; until then, failing loudly beats
                    // mistranslating either.
                    IntrinsicResult.Unrecognised
            | _ ->
                // A signature shape this intrinsic does not recognise at all — the three types are
                // not all the same, or the parameter count is wrong.
                IntrinsicResult.Unrecognised
        | "System.Private.CoreLib", "Thread", "FastPollGC" ->
            // [Intrinsic] internal static void Thread.FastPollGC() => Thread.FastPollGC();
            // The managed IL body is an infinite self-recursive call; the JIT replaces
            // every call site with an inline fast GC poll. PawPrint has no GC, so the
            // intrinsic is a pure no-op. This cannot live in safeIntrinsics because
            // executing the IL would loop forever.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Threading/Thread.cs#L390-L391
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [], MethodReturnType.Void -> ()
            | _ -> failwith $"Thread.FastPollGC: unexpected signature %A{methodToCall.Signature}"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "Volatile", ("ReadBarrier" | "WriteBarrier") ->
            // [Intrinsic] public static void Volatile.{Read,Write}Barrier() => Volatile.{Read,Write}Barrier();
            // Same shape as Thread.FastPollGC: the managed body is infinite self-recursion
            // and the JIT replaces the call with the appropriate processor fence. PawPrint
            // does not model memory-ordering effects across threads, and even if it did the
            // single-stepping interpreter has no instruction reordering to fence against,
            // so the no-op is correct. Cannot live in safeIntrinsics because the IL would
            // loop forever.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Threading/Volatile.cs#L236-L245
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [], MethodReturnType.Void -> ()
            | _ -> failwith $"Volatile.%s{methodToCall.Name}: unexpected signature %A{methodToCall.Signature}"

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "BitConverter", "SingleToInt32Bits" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteSingle state.ConcreteTypes ], MethodReturnType.Returns (ConcreteInt32 state.ConcreteTypes) -> ()
            | _ -> failwith "bad signature BitConverter.SingleToInt32Bits"

            let arg, state = IlMachineState.popEvalStack currentThread state

            let result =
                match arg with
                | EvalStackValue.Float f ->
                    BitConverter.SingleToInt32Bits (float32<float> f)
                    |> Int32Source.Verbatim
                    |> EvalStackValue.Int32
                | _ -> failwith "TODO"

            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "BitConverter", "Int32BitsToSingle" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteInt32 state.ConcreteTypes ], MethodReturnType.Returns (ConcreteSingle state.ConcreteTypes) -> ()
            | _ -> failwith "bad signature BitConverter.Int64BitsToSingle"

            let arg, state = IlMachineState.popEvalStack currentThread state

            let arg =
                match arg with
                | EvalStackValue.Int32 (Int32Source.Verbatim i) -> i
                | _ -> failwith "$TODO: {arr}"

            let result =
                BitConverter.Int32BitsToSingle arg |> CliNumericType.Float32 |> CliType.Numeric

            state
            |> IlMachineState.pushToEvalStack result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "BitConverter", "DoubleToUInt64Bits" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteDouble state.ConcreteTypes ], MethodReturnType.Returns (ConcreteUInt64 state.ConcreteTypes) ->
                ()
            | _ -> failwith "bad signature BitConverter.DoubleToUInt64Bits"

            let arg, state = IlMachineState.popEvalStack currentThread state

            let arg =
                match arg with
                | EvalStackValue.Float i -> i
                | _ -> failwith "$TODO: {arr}"

            let result =
                BitConverter.DoubleToUInt64Bits arg
                |> int64<uint64>
                |> Int64Source.Verbatim
                |> CliNumericType.Int64
                |> CliType.Numeric

            state
            |> IlMachineState.pushToEvalStack result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "BitConverter", "UInt64BitsToDouble" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteUInt64 state.ConcreteTypes ], MethodReturnType.Returns (ConcreteDouble state.ConcreteTypes) ->
                ()
            | _ -> failwith "bad signature BitConverter.DoubleToUInt64Bits"

            let arg, state = IlMachineState.popEvalStack currentThread state

            let arg =
                match arg with
                | EvalStackValue.Int64 (Int64Source.Verbatim i) -> uint64<int64> i
                | _ -> failwith "$TODO: {arr}"

            let result =
                BitConverter.UInt64BitsToDouble arg |> CliNumericType.Float64 |> CliType.Numeric

            state
            |> IlMachineState.pushToEvalStack result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "BitConverter", "Int64BitsToDouble" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteInt64 state.ConcreteTypes ], MethodReturnType.Returns (ConcreteDouble state.ConcreteTypes) -> ()
            | _ -> failwith "bad signature BitConverter.Int64BitsToDouble"

            let arg, state = IlMachineState.popEvalStack currentThread state

            let arg =
                match arg with
                | EvalStackValue.Int64 (Int64Source.Verbatim i) -> i
                | _ -> failwith "$TODO: {arr}"

            let result =
                BitConverter.Int64BitsToDouble arg |> CliNumericType.Float64 |> CliType.Numeric

            state
            |> IlMachineState.pushToEvalStack result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "BitConverter", "DoubleToInt64Bits" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteDouble state.ConcreteTypes ], MethodReturnType.Returns (ConcreteInt64 state.ConcreteTypes) -> ()
            | _ -> failwith "bad signature BitConverter.DoubleToInt64Bits"

            let arg, state = IlMachineState.popEvalStack currentThread state

            let result =
                match arg with
                | EvalStackValue.Float f ->
                    BitConverter.DoubleToInt64Bits f |> Int64Source.Verbatim |> EvalStackValue.Int64
                | _ -> failwith "TODO"

            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "BitConverter", "SingleToUInt32Bits" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteSingle state.ConcreteTypes ], MethodReturnType.Returns (ConcreteUInt32 state.ConcreteTypes) ->
                ()
            | _ -> failwith "bad signature BitConverter.SingleToUInt32Bits"

            let arg, state = IlMachineState.popEvalStack currentThread state

            let result =
                match arg with
                | EvalStackValue.Float f ->
                    BitConverter.SingleToUInt32Bits (float32<float> f)
                    |> int<uint32>
                    |> Int32Source.Verbatim
                    |> EvalStackValue.Int32
                | _ -> failwith "TODO"

            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "BitConverter", "UInt32BitsToSingle" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteUInt32 state.ConcreteTypes ], MethodReturnType.Returns (ConcreteSingle state.ConcreteTypes) ->
                ()
            | _ -> failwith "bad signature BitConverter.UInt32BitsToSingle"

            let arg, state = IlMachineState.popEvalStack currentThread state

            let result =
                match arg with
                | EvalStackValue.Int32 (Int32Source.Verbatim f) ->
                    BitConverter.UInt32BitsToSingle (uint32<int> f)
                    |> float<float32>
                    |> EvalStackValue.Float
                | _ -> failwith "TODO"

            state
            |> IlMachineState.pushToEvalStack' result currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "BitOperations", "TrailingZeroCount" when
            intrinsicKey.DeclaringTypeFullName = "System.Numerics.BitOperations"
            ->
            // BitOperations.TrailingZeroCount is a JIT intrinsic in the real CLR, lowered to
            // TZCNT on x86 or RBIT+CLZ on Arm. Only the uint32 overload needs modelling: with
            // every hardware profile reporting unavailable, its body falls through to a De
            // Bruijn lookup table backed by a PE byte range, which PawPrint does not model.
            // The other overloads' bodies are IL PawPrint can already run — they either
            // forward outright or, for uint64, split into halves that land back here — so
            // they are allowlisted in `safeIntrinsics` rather than duplicated as arms.
            //
            // Delegating to the host BCL is deterministic, for the same reason the sibling
            // LeadingZeroCount arm records: the method is a pure function of the argument's
            // bits, fully specified for every input, including the zero that TZCNT and BSF
            // disagree about (hence the BCL's own explicit zero check).
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Numerics/BitOperations.cs#L526-L577
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteUInt32 state.ConcreteTypes ], MethodReturnType.Returns (ConcreteInt32 state.ConcreteTypes) ->
                let arg, state = IlMachineState.popEvalStack currentThread state

                // The narrowing to the operand's own width is load-bearing: the bits arrive
                // widened to int64, and the zeros above bit 31 are not the operand's.
                let value =
                    bitPatternValueArgument "BitOperations.TrailingZeroCount(uint)" arg
                    |> uint32<int64>

                let result = System.Numerics.BitOperations.TrailingZeroCount value

                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim result)) currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> IntrinsicResult.Completed
            | _ -> failwith $"BitOperations.TrailingZeroCount: unexpected signature %s{formatMethodKey intrinsicKey}"
        | "System.Private.CoreLib", "BitOperations", "LeadingZeroCount" when
            intrinsicKey.DeclaringTypeFullName = "System.Numerics.BitOperations"
            ->
            // BitOperations.LeadingZeroCount is a JIT intrinsic in the real CLR, lowered to
            // LZCNT on x86 or CLZ on Arm. PawPrint models a deterministic virtual CPU that
            // reports every hardware profile unavailable, so executing the BCL IL body would
            // fall through all of its `IsSupported` guards to `31 ^ Log2SoftwareFallback(value)`
            // (the 64-bit overload first splits into halves and then reaches the same place),
            // which reads a De Bruijn lookup table backed by a PE byte range — the very path
            // the sibling Log2 arm exists to avoid. Model the boundary directly instead.
            //
            // Delegating to the host BCL is deterministic here, unlike Math.Pow: the method is
            // a pure function of the argument's bits, and its answer is fully specified for
            // every input — including zero, where the BCL adds an explicit check precisely
            // because BSR and the software fallback would otherwise disagree with LZCNT/CLZ.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Numerics/BitOperations.cs#L167-L267
            //
            // Only the two widths the BCL genuinely implements separately are modelled here.
            // The `(nuint)` overload's body is `ldarg.0; conv.u8; call LeadingZeroCount(uint64);
            // ret` — IL PawPrint can run — so it is allowlisted in `safeIntrinsics` and reaches
            // the uint64 arm below rather than duplicating a width decision on this side.
            //
            // Each arm narrows the bits back to its own operand width before calling the host
            // method of that same width. That narrowing is load-bearing: the bits arrive
            // widened to int64, and the zeros the widening introduced are not the operand's.
            let result, state =
                match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
                | [ ConcreteUInt32 state.ConcreteTypes ], MethodReturnType.Returns (ConcreteInt32 state.ConcreteTypes) ->
                    let arg, state = IlMachineState.popEvalStack currentThread state

                    let value =
                        bitPatternValueArgument "BitOperations.LeadingZeroCount(uint)" arg
                        |> uint32<int64>

                    System.Numerics.BitOperations.LeadingZeroCount value, state
                | [ ConcreteUInt64 state.ConcreteTypes ], MethodReturnType.Returns (ConcreteInt32 state.ConcreteTypes) ->
                    let arg, state = IlMachineState.popEvalStack currentThread state

                    let value =
                        bitPatternValueArgument "BitOperations.LeadingZeroCount(ulong)" arg
                        |> uint64<int64>

                    System.Numerics.BitOperations.LeadingZeroCount value, state
                | _ -> failwith $"BitOperations.LeadingZeroCount: unexpected signature %s{formatMethodKey intrinsicKey}"

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim result)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "BitOperations", "Log2" ->
            // BitOperations.Log2 is a JIT intrinsic in the real CLR. The BCL IL body falls
            // through to a software fallback that reads from a De Bruijn lookup table backed
            // by a PE byte range, which collides with paths PawPrint does not yet model.
            // Model the boundary directly instead: delegate to the host BCL, which honours
            // the documented `Log2(0) = 0` contract.
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteUInt32 state.ConcreteTypes ], MethodReturnType.Returns (ConcreteInt32 state.ConcreteTypes) ->
                let arg, state = IlMachineState.popEvalStack currentThread state

                let value =
                    match arg with
                    | EvalStackValue.Int32 (Int32Source.Verbatim i) -> uint32<int> i
                    | _ -> failwith $"BitOperations.Log2(uint): unexpected eval stack value %O{arg}"

                let result = System.Numerics.BitOperations.Log2 value

                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim result)) currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> IntrinsicResult.Completed
            | [ ConcreteUInt64 state.ConcreteTypes ], MethodReturnType.Returns (ConcreteInt32 state.ConcreteTypes) ->
                let arg, state = IlMachineState.popEvalStack currentThread state

                let value =
                    match arg with
                    | EvalStackValue.Int64 (Int64Source.Verbatim i) -> uint64<int64> i
                    | _ -> failwith $"BitOperations.Log2(ulong): unexpected eval stack value %O{arg}"

                let result = System.Numerics.BitOperations.Log2 value

                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim result)) currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> IntrinsicResult.Completed
            | [ ConcreteUIntPtr state.ConcreteTypes ], MethodReturnType.Returns (ConcreteInt32 state.ConcreteTypes) ->
                let arg, state = IlMachineState.popEvalStack currentThread state

                let value : unativeint =
                    match arg with
                    | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) -> unativeint<int64> i
                    | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null) -> 0un
                    | EvalStackValue.Int64 (Int64Source.Verbatim i) -> unativeint<int64> i
                    | EvalStackValue.Int32 (Int32Source.Verbatim i) -> unativeint<int> i
                    | _ -> failwith $"BitOperations.Log2(nuint): unexpected eval stack value %O{arg}"

                let result = System.Numerics.BitOperations.Log2 value

                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim result)) currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> IntrinsicResult.Completed
            | _ -> failwith $"BitOperations.Log2: unexpected signature %s{formatMethodKey intrinsicKey}"
        | "System.Private.CoreLib", "Math", "Pow" when intrinsicKey.DeclaringTypeFullName = "System.Math" ->
            // Math.Pow has no IL body at all, so it cannot be allowlisted in safeIntrinsics:
            // CoreCLR declares it `[Intrinsic]` + `MethodImplOptions.InternalCall` and the JIT
            // lowers it to a call into the platform C library's `pow`.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/coreclr/System.Private.CoreLib/src/System/Math.CoreCLR.cs#L84-L86
            //
            // The host's `System.Math.Pow` is deliberately *not* what we call here. `pow` is not
            // correctly rounded and libm implementations are not required to agree bit-for-bit,
            // so forwarding to the host would make a replay depend on the machine that recorded
            // it. `DeterministicMath.pow` computes the same function from integer arithmetic
            // alone; see its comments for the accuracy argument.
            //
            // Reached from ordinary guest code, and also from `PortableThreadPool`'s
            // hill-climbing controller, which is what bounds how many blocking thread-pool
            // waits a guest can perform (issue #755).
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteDouble state.ConcreteTypes ; ConcreteDouble state.ConcreteTypes ],
              MethodReturnType.Returns (ConcreteDouble state.ConcreteTypes) -> ()
            | _ -> failwith $"Math.Pow: unexpected signature %s{formatMethodKey intrinsicKey}"

            // The exponent was pushed last, so it pops first.
            let exponent, state = IlMachineState.popEvalStack currentThread state
            let baseValue, state = IlMachineState.popEvalStack currentThread state

            let asFloat (name : string) (value : EvalStackValue) : float =
                match value with
                | EvalStackValue.Float f -> f
                | _ -> failwith $"Math.Pow: unexpected eval stack value for %s{name}: %O{value}"

            let result =
                DeterministicMath.pow (asFloat "base" baseValue) (asFloat "exponent" exponent)

            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Float64 result)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "Math", "Cos" when intrinsicKey.DeclaringTypeFullName = "System.Math" ->
            // As with `Math.Pow` above: `[Intrinsic]` + `MethodImplOptions.InternalCall` with
            // no IL body, lowered by the JIT to the platform C library's `cos`.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/coreclr/System.Private.CoreLib/src/System/Math.CoreCLR.cs#L56-L57
            //
            // `cos` is likewise not correctly rounded and libms differ in the last bit, so the
            // host's is deliberately not what we call; `DeterministicMath.cos` computes it from
            // integer arithmetic alone.
            //
            // Reached from ordinary guest code, and also from `PortableThreadPool`'s
            // hill-climbing controller, whose `GetWaveComponent`
            // (PortableThreadPool.HillClimbing.cs:448) is what a guest doing enough blocking
            // thread-pool waits eventually runs.
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteDouble state.ConcreteTypes ], MethodReturnType.Returns (ConcreteDouble state.ConcreteTypes) ->
                ()
            | _ -> failwith $"Math.Cos: unexpected signature %s{formatMethodKey intrinsicKey}"

            let argument, state = IlMachineState.popEvalStack currentThread state

            let argument =
                match argument with
                | EvalStackValue.Float f -> f
                | _ -> failwith $"Math.Cos: unexpected eval stack value: %O{argument}"

            let result = DeterministicMath.cos argument

            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Float64 result)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "Math", "Sin" when intrinsicKey.DeclaringTypeFullName = "System.Math" ->
            // The twin of `Math.Cos` above, and the same shape: `[Intrinsic]` +
            // `MethodImplOptions.InternalCall` with no IL body, lowered by the JIT to the
            // platform C library's `sin`.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/coreclr/System.Private.CoreLib/src/System/Math.CoreCLR.cs#L92-L93
            //
            // Note that `Math.SinCos` is *not* this method: it has an IL body of its own and
            // bottoms out in a separate `SinCos(double, double*, double*)` InternalCall, which
            // is still unimplemented. The hill-climbing controller below does not use it.
            //
            // Reached from ordinary guest code, and from `GetWaveComponent`
            // (PortableThreadPool.HillClimbing.cs:457), nine lines after the `Math.Cos` call
            // that the same controller makes.
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteDouble state.ConcreteTypes ], MethodReturnType.Returns (ConcreteDouble state.ConcreteTypes) ->
                ()
            | _ -> failwith $"Math.Sin: unexpected signature %s{formatMethodKey intrinsicKey}"

            let argument, state = IlMachineState.popEvalStack currentThread state

            let argument =
                match argument with
                | EvalStackValue.Float f -> f
                | _ -> failwith $"Math.Sin: unexpected eval stack value: %O{argument}"

            let result = DeterministicMath.sin argument

            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Float64 result)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "Math", "Sqrt" when intrinsicKey.DeclaringTypeFullName = "System.Math" ->
            // Declared the same way as the three above -- `[Intrinsic]` +
            // `MethodImplOptions.InternalCall`, no IL body -- so it likewise cannot be
            // allowlisted in `safeIntrinsics`.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/coreclr/System.Private.CoreLib/src/System/Math.CoreCLR.cs#L112-L114
            //
            // The reason for computing it in-tree is different from theirs, though. The JIT
            // lowers this one to a hardware square-root instruction rather than to a libm call,
            // and IEEE 754 clause 5.4.1 *requires* squareRoot to be correctly rounded -- so
            // unlike `pow`, `sin` and `cos`, the host's answer does not vary between machines
            // and forwarding to it would not have cost determinism. `DeterministicMath.sqrt`
            // exists so that the guarantee is this runtime's own rather than a property of the
            // host we happen to be running on, and so that the tests have an exact oracle.
            // `TestDeterministicMath` asserts the two agree bit-for-bit on every finite
            // argument, which is an assertion the other three cannot make.
            //
            // Reached from ordinary guest code, and from `PortableThreadPool`'s hill-climbing
            // controller, which takes the magnitude of the wave components that its `Math.Sin`
            // and `Math.Cos` calls produce: its own private `Complex.Abs`
            // (PortableThreadPool.HillClimbing.Complex.cs:35) is a bare `Math.Sqrt`.
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteDouble state.ConcreteTypes ], MethodReturnType.Returns (ConcreteDouble state.ConcreteTypes) ->
                ()
            | _ -> failwith $"Math.Sqrt: unexpected signature %s{formatMethodKey intrinsicKey}"

            let argument, state = IlMachineState.popEvalStack currentThread state

            let argument =
                match argument with
                | EvalStackValue.Float f -> f
                | _ -> failwith $"Math.Sqrt: unexpected eval stack value: %O{argument}"

            let result = DeterministicMath.sqrt argument

            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Float64 result)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "Math", "Ceiling" when intrinsicKey.DeclaringTypeFullName = "System.Math" ->
            // Declared like the four above -- `[Intrinsic]` + `MethodImplOptions.InternalCall`,
            // no IL body -- so it likewise cannot be allowlisted in `safeIntrinsics`. The JIT
            // lowers it to `roundsd`/`frintp` where the hardware has them and to the platform
            // C library's `ceil` otherwise.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/coreclr/System.Private.CoreLib/src/System/Math.CoreCLR.cs#L51-L53
            //
            // This is the `Math.Sqrt` situation taken further: `roundToIntegralTowardPositive`
            // is not merely correctly rounded but exact, so there is one right answer for
            // every argument and every conforming implementation returns it. Computing it
            // in-tree therefore changes nothing about the result, and is about the guarantee
            // being this runtime's own; `DeterministicMath.ceiling` also gives the tests an
            // exact oracle, which the `pow`/`sin`/`cos` arms above lack.
            //
            // The `decimal` overload of `Math.Ceiling` never arrives here: it is not marked
            // `[Intrinsic]` and has an ordinary IL body, so it runs as managed code. The guard
            // on `DeclaringTypeFullName` keeps this arm off any other type that happens to
            // have a `Ceiling`, and the signature check below rejects anything else.
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteDouble state.ConcreteTypes ], MethodReturnType.Returns (ConcreteDouble state.ConcreteTypes) ->
                ()
            | _ -> failwith $"Math.Ceiling: unexpected signature %s{formatMethodKey intrinsicKey}"

            let argument, state = IlMachineState.popEvalStack currentThread state

            let argument =
                match argument with
                | EvalStackValue.Float f -> f
                | _ -> failwith $"Math.Ceiling: unexpected eval stack value: %O{argument}"

            let result = DeterministicMath.ceiling argument

            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Float64 result)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "Math", "Round" when
            intrinsicKey.DeclaringTypeFullName = "System.Math"
            && intrinsicKey.ParameterShapes = [ "System.Double" ]
            ->
            // The odd one out among the five `System.Math` arms above: this one is `[Intrinsic]`
            // but *not* `MethodImplOptions.InternalCall`, so it does have an IL body and could
            // in principle be allowlisted in `safeIntrinsics` and simply run.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Math.cs#L1306-L1348
            //
            // That body is not the definition, though. It is a managed emulation of the
            // instruction the JIT actually emits -- `roundsd` with mode 0 on x86, `frintn` on
            // Arm -- and it gets ties-to-even out of the ambient rounding mode by computing
            // `(a + 2^52) - 2^52`. Running it would make the answer a property of whatever
            // performed that addition rather than of this runtime, which is the class of
            // dependency `DeterministicMath` exists to remove; so, as with `Math.Ceiling` above,
            // the operation is named in-tree. `roundToIntegralTiesToEven` is exact, so this
            // changes nothing about the result and gives the tests an exact oracle.
            //
            // The three other `double` overloads -- `Round(double, int)`,
            // `Round(double, MidpointRounding)` and `Round(double, int, MidpointRounding)` --
            // are not `[Intrinsic]`, so they run as ordinary managed IL and reach this arm
            // through their own `MidpointRounding.ToEven` path. The `decimal` overloads never
            // arrive here either, for the same reason. The guard on `DeclaringTypeFullName`
            // keeps this arm off any other type with a `Round` (`System.MathF.Round(float)` is
            // also `[Intrinsic]`, and is a different operation on a different width), and the
            // guard on `ParameterShapes` keeps it off the multi-argument overloads should any
            // of them ever become intrinsic.
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteDouble state.ConcreteTypes ], MethodReturnType.Returns (ConcreteDouble state.ConcreteTypes) ->
                ()
            | _ -> failwith $"Math.Round: unexpected signature %s{formatMethodKey intrinsicKey}"

            let argument, state = IlMachineState.popEvalStack currentThread state

            let argument =
                match argument with
                | EvalStackValue.Float f -> f
                | _ -> failwith $"Math.Round: unexpected eval stack value: %O{argument}"

            let result = DeterministicMath.round argument

            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Float64 result)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "String", "Equals" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteString state.ConcreteTypes ; ConcreteString state.ConcreteTypes ],
              MethodReturnType.Returns (ConcreteBool state.ConcreteTypes) ->
                let arg1, state = IlMachineState.popEvalStack currentThread state

                let arg1 =
                    match arg1 with
                    | EvalStackValue.ObjectRef h -> Some h
                    | EvalStackValue.NullObjectRef -> None
                    | EvalStackValue.Int32 _
                    | EvalStackValue.Int64 _
                    | EvalStackValue.Float _ -> failwith $"this isn't a string! {arg1}"
                    | _ -> failwith $"TODO: %O{arg1}"

                let arg2, state = IlMachineState.popEvalStack currentThread state

                let arg2 =
                    match arg2 with
                    | EvalStackValue.ObjectRef h -> Some h
                    | EvalStackValue.NullObjectRef -> None
                    | EvalStackValue.Int32 _
                    | EvalStackValue.Int64 _
                    | EvalStackValue.Float _ -> failwith $"this isn't a string! {arg2}"
                    | _ -> failwith $"TODO: %O{arg2}"

                let areEqual =
                    match arg1, arg2 with
                    | None, None -> true
                    | Some _, None
                    | None, Some _ -> false
                    | Some arg1, Some arg2 -> ManagedHeap.stringsEqual arg1 arg2 state.ManagedHeap

                state
                |> IlMachineState.pushToEvalStack (CliType.ofBool areEqual) currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> IntrinsicResult.Completed
            | _ -> IntrinsicResult.Unrecognised
        | "System.Private.CoreLib", "Unsafe", "ReadUnaligned" ->
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/Unsafe.cs#L558
            // Semantically this returns the T that would be read by
            // reinterpreting the pointer as `ref T` and dereferencing. The JIT
            // lowers it to `Unsafe.As<byte, T>(ref source)` + deref. Our heap
            // stores typed cells rather than raw bytes, so we model the read
            // by delegating the bytewise gather/reconstruction to managed
            // byref byte helpers.
            //
            // Two overloads exist: `ReadUnaligned<T>(ref byte source)` and
            // `ReadUnaligned<T>(void* source)`. PawPrint handles the pointer
            // overload only when the pointer has managed provenance, for
            // example a PE byte-range pointer produced by `ldsflda`.
            match methodToCall.Signature.ParameterTypes with
            | [ ConcreteByref _ ] ->

                let t =
                    match Seq.toList methodToCall.Generics with
                    | [ t ] -> t
                    | _ -> failwith "bad generics Unsafe.ReadUnaligned"

                let tZero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes t

                let ptr, state = IlMachineState.popEvalStack currentThread state

                match ptr with
                // A null byref is `ManagedPointer ManagedPointerSource.Null` — that is what
                // `Unsafe.NullRef` and `Unsafe.AsRef(void*)` produce — so it must be matched
                // *before* the general `ManagedPointer` arm, which would otherwise carry it into
                // the byref machinery. `NullObjectRef` is the same condition reached from
                // hand-written IL.
                //
                // `ReadUnaligned`'s body is `ldarg.0; unaligned. 1; ldobj !!T; ret`: no explicit
                // null check, so the load at address 0 faults and the runtime translates it into
                // the ordinary parameterless `NullReferenceException`.
                | EvalStackValue.ManagedPointer ManagedPointerSource.Null
                | EvalStackValue.NullObjectRef ->
                    IntrinsicResult.RaiseException (state, baseClassTypes.NullReferenceException, None)
                | EvalStackValue.ManagedPointer src ->
                    let v = IlMachineState.readManagedByrefBytesAs baseClassTypes state src tZero

                    state
                    |> IlMachineState.pushToEvalStack v currentThread
                    |> IlMachineState.advanceProgramCounter currentThread
                    |> IntrinsicResult.Completed
                | _ -> failwith $"TODO: Unsafe.ReadUnaligned: expected ManagedPointer, got %O{ptr}"
            | [ ConcretePointer _ ] ->

                let t =
                    match Seq.toList methodToCall.Generics with
                    | [ t ] -> t
                    | _ -> failwith "bad generics Unsafe.ReadUnaligned"

                let tZero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes t

                let ptr, state = IlMachineState.popEvalStack currentThread state

                let src = managedPointerOfPointerArgument "Unsafe.ReadUnaligned(void*)" ptr

                let v = IlMachineState.readManagedByrefBytesAs baseClassTypes state src tZero

                let state =
                    state
                    |> IlMachineState.pushToEvalStack v currentThread
                    |> IlMachineState.advanceProgramCounter currentThread

                IntrinsicResult.Completed state
            | _ -> IntrinsicResult.Unrecognised
        | "System.Private.CoreLib", "Unsafe", "WriteUnaligned" ->
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/Unsafe.cs#L609
            // Symmetric to ReadUnaligned: writes a T through a byte-level
            // byref by delegating byte scattering to managed byref byte helpers.
            //
            // The `(void*, T)` overload is handled only for pointers with
            // managed provenance, symmetric with `ReadUnaligned`.
            match methodToCall.Signature.ParameterTypes with
            | [ ConcreteByref _ ; _ ] ->

                let t =
                    match Seq.toList methodToCall.Generics with
                    | [ t ] -> t
                    | _ -> failwith "bad generics Unsafe.WriteUnaligned"

                let tZero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes t
                let tSize = CliType.sizeOf tZero

                // Stack order: the ref byte goes on first (arg0), the value on
                // top (arg1). Pop value first.
                let value, state = IlMachineState.popEvalStack currentThread state
                let ptr, state = IlMachineState.popEvalStack currentThread state

                match ptr with
                // As for `ReadUnaligned` above: the C#-reachable null byref is the
                // `ManagedPointerSource.Null` spelling, so it must precede the general
                // `ManagedPointer` arm. `WriteUnaligned`'s body is
                // `ldarg.0; ldarg.1; unaligned. 1; stobj !!T; ret`, with no null check, so the
                // store at address 0 faults into a parameterless `NullReferenceException`.
                | EvalStackValue.ManagedPointer ManagedPointerSource.Null
                | EvalStackValue.NullObjectRef ->
                    IntrinsicResult.RaiseException (state, baseClassTypes.NullReferenceException, None)
                | EvalStackValue.ManagedPointer src ->
                    // Coerce the stack value to a CliType shaped like T: sub-int
                    // primitives arrive as Int32 and must narrow back to their
                    // CliType flavour before the byte helpers write it.
                    let valueAsCli = EvalStackValue.toCliTypeCoerced tZero value

                    let valueSize = CliType.sizeOf valueAsCli

                    if valueSize <> tSize then
                        failwith
                            $"Unsafe.WriteUnaligned: coerced value has size %d{valueSize}, expected %d{tSize} for %O{valueAsCli}"

                    IlMachineState.writeManagedByrefBytesOrTypedCell baseClassTypes state src valueAsCli
                    |> IlMachineState.advanceProgramCounter currentThread
                    |> IntrinsicResult.Completed
                | _ -> failwith $"TODO: Unsafe.WriteUnaligned: expected ManagedPointer, got %O{ptr}"
            | [ ConcretePointer _ ; _ ] ->

                let t =
                    match Seq.toList methodToCall.Generics with
                    | [ t ] -> t
                    | _ -> failwith "bad generics Unsafe.WriteUnaligned"

                let tZero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes t
                let tSize = CliType.sizeOf tZero

                // Stack order: the pointer goes on first (arg0), the value on
                // top (arg1). Pop value first.
                let value, state = IlMachineState.popEvalStack currentThread state
                let ptr, state = IlMachineState.popEvalStack currentThread state

                let src = managedPointerOfPointerArgument "Unsafe.WriteUnaligned(void*)" ptr

                let valueAsCli = EvalStackValue.toCliTypeCoerced tZero value

                let valueSize = CliType.sizeOf valueAsCli

                if valueSize <> tSize then
                    failwith
                        $"Unsafe.WriteUnaligned(void*): coerced value has size %d{valueSize}, expected %d{tSize} for %O{valueAsCli}"

                let state =
                    IlMachineState.writeManagedByrefBytesOrTypedCell baseClassTypes state src valueAsCli

                let state = state |> IlMachineState.advanceProgramCounter currentThread
                IntrinsicResult.Completed state
            | _ -> IntrinsicResult.Unrecognised
        | "System.Private.CoreLib", "Unsafe", ("CopyBlock" | "CopyBlockUnaligned") ->
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/Unsafe.cs#L313
            // The CoreLib bodies throw PlatformNotSupportedException; the real JIT replaces
            // these with `cpblk` (optionally prefixed by `unaligned.`). Both overloads accept
            // the byref and pointer forms uniformly via managedPointerOfPointerArgument.
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
                ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
                ConcreteUInt32 state.ConcreteTypes ],
              MethodReturnType.Void
            | [ ConcretePointer _ ; ConcretePointer _ ; ConcreteUInt32 state.ConcreteTypes ], MethodReturnType.Void ->
                let operation = $"Unsafe.%s{methodToCall.Name}"

                executeUnsafeCopyBlock baseClassTypes currentThread operation state
                |> IntrinsicResult.Completed
            | _ -> IntrinsicResult.Unrecognised
        | "System.Private.CoreLib", "SpanHelpers", "Memmove" ->
            // `[Intrinsic] internal static void Memmove(ref byte dest, ref byte src, nuint len)`
            // (SpanHelpers.ByteMemOps.cs:37). The managed body executes the platform-tuned
            // byte/Block16 unrolled walk and P/Invokes into native memmove on overlap; both
            // paths flow through PawPrint's byte-walk model, which cannot serialise non-`Verbatim`
            // `NativeIntSource` provenance via `CliNumericType.ToBytes`. Routing the intrinsic
            // through `CellAwareMemOps.copy` with `Memmove` policy preserves whole-cell ranges
            // (provenance, ObjectRef cells) when both endpoints anchor on cell-aware roots, and
            // falls back to the byte walk for genuinely byte-addressable storage.
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
                ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
                ConcreteUIntPtr state.ConcreteTypes ],
              MethodReturnType.Void ->
                let operation = "SpanHelpers.Memmove"

                executeSpanHelpersMemmove baseClassTypes currentThread operation state
                |> IntrinsicResult.Completed
            | _ -> IntrinsicResult.Unrecognised
        | "System.Private.CoreLib", "SpanHelpers", "ClearWithoutReferences" ->
            // `[Intrinsic] public static void ClearWithoutReferences(ref byte dest, nuint len)`
            // (SpanHelpers.ByteMemOps.cs:246). This is the boundary `Array.Clear` reaches for
            // every element type without GC pointers, and `NativeMemory.Clear` reaches for all
            // of them. The managed body is the platform-tuned unrolled walk of
            // `Unsafe.WriteUnaligned<Block16>`/`<long>` stores, and beyond
            // `ZeroMemoryNativeThreshold` it P/Invokes into native memset (label `MZER05`) —
            // so, exactly as for its `Memmove` sibling, the managed IL is not a route PawPrint
            // can take. Routing the intrinsic through `CellAwareMemOps.clear` writes each
            // destination cell's own zero, preserving cell shape for storage that is not
            // byte-addressable, and falls back to the byte walk for genuinely flat storage.
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
                ConcreteUIntPtr state.ConcreteTypes ],
              MethodReturnType.Void ->
                let operation = "SpanHelpers.ClearWithoutReferences"

                executeSpanHelpersClearWithoutReferences baseClassTypes currentThread operation state
                |> IntrinsicResult.Completed
            | _ -> IntrinsicResult.Unrecognised
        | "System.Private.CoreLib", "String", "op_Implicit" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ par ], MethodReturnType.Returns ret ->
                let par = state.ConcreteTypes |> AllConcreteTypes.lookup par |> Option.get
                let ret = state.ConcreteTypes |> AllConcreteTypes.lookup ret |> Option.get

                if
                    par.Namespace = "System"
                    && par.Name = "String"
                    && ret.Namespace = "System"
                    && ret.Name = "ReadOnlySpan`1"
                then
                    match ret.Generics |> Seq.toList with
                    | [ gen ] ->
                        let gen = state.ConcreteTypes |> AllConcreteTypes.lookup gen |> Option.get

                        if gen.Namespace = "System" && gen.Name = "Char" then
                            // This is just an optimisation
                            // https://github.com/dotnet/runtime/blob/ab105b51f8b50ec5567d7cfe9001ca54dd6f64c3/src/libraries/System.Private.CoreLib/src/System/String.cs#L363-L366
                            IntrinsicResult.Unrecognised
                        else
                            failwith "TODO: unexpected params to String.op_Implicit"
                    | _ -> failwith "TODO: unexpected params to String.op_Implicit"
                else
                    failwith "TODO: unexpected params to String.op_Implicit"
            | _ -> failwith "TODO: unexpected params to String.op_Implicit"
        | "System.Private.CoreLib", "RuntimeHelpers", "IsReferenceOrContainsReferences" ->
            // https://github.com/dotnet/runtime/blob/1d1bf92fcf43aa6981804dc53c5174445069c9e4/src/coreclr/System.Private.CoreLib/src/System/Runtime/CompilerServices/RuntimeHelpers.CoreCLR.cs#L207
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [], MethodReturnType.Returns (ConcreteBool state.ConcreteTypes) -> ()
            | _ -> failwith "bad signature for System.Private.CoreLib.RuntimeHelpers.IsReferenceOrContainsReference"

            let arg = Seq.exactlyOne methodToCall.Generics

            let state, result =
                concreteTypeContainsReferences loggerFactory baseClassTypes state arg

            let state =
                state
                |> IlMachineState.pushToEvalStack (CliType.ofBool result) currentThread
                |> IlMachineState.advanceProgramCounter currentThread

            IntrinsicResult.Completed state
        | "System.Private.CoreLib", "RuntimeHelpers", "InitializeArray" ->
            // https://github.com/dotnet/runtime/blob/9e5e6aa7bc36aeb2a154709a9d1192030c30a2ef/src/coreclr/System.Private.CoreLib/src/System/Runtime/CompilerServices/RuntimeHelpers.CoreCLR.cs#L18
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteSystemArray state.ConcreteTypes ; ConcreteRuntimeFieldHandle state.ConcreteTypes ],
              MethodReturnType.Void -> ()
            | _ -> failwith "bad signature for System.Private.CoreLib.RuntimeHelpers.InitializeArray"

            // Pop args: arg1 (RuntimeFieldHandle) is on top, then arg0 (array ref)
            let fldHandle, state = IlMachineState.popEvalStack currentThread state
            let arrayRef, state = IlMachineState.popEvalStack currentThread state

            // Argument validation, in the BCL's order:
            //
            //     if (array is null) ThrowHelper.ThrowArgumentNullException(ExceptionArgument.array);
            //     if (fldHandle.IsNullHandle()) throw new ArgumentException(SR.Argument_InvalidHandle);
            //
            // Note the first is an `ArgumentNullException`, not the `NullReferenceException` the
            // shape of the check suggests. The JIT only expands this intrinsic when it recognises
            // a `newarr` plus a constant `ldtoken`, which a null argument never matches, so the
            // managed body — and hence these checks — is what really runs.
            //
            // Both arguments are already popped and the PC has not advanced, as the raise needs.
            match arrayRef, fldHandle with
            | EvalStackValue.NullObjectRef, _ ->
                // No message: the CLR's is "Value cannot be null. (Parameter 'array')", but that
                // suffix comes from `_paramName` via `ArgumentException.Message`, and this channel
                // cannot set `_paramName`. Writing the suffix into `_message` alone would leave
                // `.Message` and `.ParamName` disagreeing about whether the name is known.
                IntrinsicResult.RaiseException (state, baseClassTypes.ArgumentNullException, None)
            | _, EvalStackValue.NullObjectRef ->
                // `RuntimeFieldHandle.IsNullHandle()` is `m_ptr == null`, which is exactly this
                // flattened `NullObjectRef` (see the primitive-like note below).
                IntrinsicResult.RaiseException (state, baseClassTypes.ArgumentException, Some "The handle is invalid.")
            | _ ->

            // Extract the array address
            let arrayAddr : ManagedHeapAddress =
                match arrayRef with
                | EvalStackValue.ObjectRef addr -> addr
                | other -> failwith $"InitializeArray: expected array object ref, got %O{other}"

            // RuntimeFieldHandle is primitive-like (FlattenToObjectRef): its single `m_ptr`
            // (an IRuntimeFieldInfo ref) arrives on the stack flattened to an ObjectRef,
            // including after box/unbox round-trips (Unbox_Any flattens primitive-like types).
            // The referenced object can be either a RuntimeFieldInfoStub (the form that
            // FieldHandleRegistry.getOrAllocate produces for ldtoken) or an RtFieldInfo
            // (the form reflection's RuntimeTypeHandle.GetFields populates from the IntPtr
            // ids returned by that QCall, https://github.com/dotnet/runtime/blob/9e5e6aa7bc36aeb2a154709a9d1192030c30a2ef/src/coreclr/System.Private.CoreLib/src/System/Reflection/RtFieldInfo.cs ).
            let runtimeFieldInfoAddr : ManagedHeapAddress =
                match fldHandle with
                | EvalStackValue.ObjectRef addr -> addr
                | other -> failwith $"InitializeArray: expected RuntimeFieldHandle ObjectRef, got %O{other}"

            // The address-keyed registry index is populated when PawPrint allocates a
            // RuntimeFieldInfoStub. Reflection-produced RtFieldInfo objects are not in that
            // index — they are constructed in managed code from the IntPtr field ids that
            // RuntimeTypeHandle.GetFields returned, so we recover the FieldHandle by reading
            // the heap object's `m_fieldHandle` slot and resolving it against the id-keyed
            // index. Both RuntimeFieldInfoStub and RtFieldInfo declare a field with that name.
            // `None` means `m_fieldHandle` was zero, i.e. the `RuntimeFieldHandle` points at a
            // field info that names no field.
            let fieldHandle : FieldHandle option =
                match FieldHandleRegistry.resolveFieldFromAddress runtimeFieldInfoAddr state.FieldHandles with
                | Some fh -> Some fh
                | None ->

                let heapObj = ManagedHeap.get runtimeFieldInfoAddr state.ManagedHeap

                let typeInfo =
                    match IlMachineState.tryGetConcreteTypeInfo state heapObj.ConcreteType with
                    | Some (_, typeInfo) -> typeInfo
                    | None ->
                        failwith
                            $"InitializeArray: object at %O{runtimeFieldInfoAddr} has concrete type %O{heapObj.ConcreteType} with no TypeDef row"

                let fieldHandleField =
                    typeInfo.Fields
                    |> List.tryFind (fun field -> field.Name = "m_fieldHandle" && not field.IsStatic)
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"InitializeArray: object at %O{runtimeFieldInfoAddr} (type %s{typeInfo.Namespace}.%s{typeInfo.Name}) is not in the field handle registry and has no instance field 'm_fieldHandle' to recover the field id from"
                    )

                let fieldHandleId =
                    let fieldId = FieldIdentity.fieldId heapObj.ConcreteType fieldHandleField

                    match
                        AllocatedNonArrayObject.DereferenceFieldById fieldId heapObj
                        |> CliType.unwrapPrimitiveLikeDeep
                    with
                    | CliType.RuntimePointer (CliRuntimePointer.FieldRegistryHandle id) -> Some id
                    | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr id)) -> Some id
                    // All four spellings of a zero `IntPtr`. `ManagedPointerSource.Null` is the
                    // canonical one — it is what `CliType.zeroOfPrimitive` plants for
                    // `IntPtr.Zero`, and so what a zero-initialised `m_fieldHandle` actually holds
                    // — so omitting it would send the very case this arm exists for into the
                    // host-failure arm below. `Verbatim 0L` is the spelling that arrives from
                    // integer arithmetic.
                    | CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L)
                    | CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null)
                    | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))
                    | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)) ->
                        None
                    | other ->
                        failwith
                            $"InitializeArray: m_fieldHandle on %s{typeInfo.Namespace}.%s{typeInfo.Name} did not contain a field-registry handle, got %O{other}"

                match fieldHandleId with
                | None -> None
                | Some fieldHandleId ->

                match FieldHandleRegistry.resolveFieldFromId fieldHandleId state.FieldHandles with
                | Some fh -> Some fh
                | None ->
                    failwith
                        $"InitializeArray: m_fieldHandle id %d{fieldHandleId} on object at %O{runtimeFieldInfoAddr} (type %s{typeInfo.Namespace}.%s{typeInfo.Name}) was not present in the field handle registry"

            match fieldHandle with
            | None ->
                // A zero `m_fieldHandle` is a *non-null* `RuntimeFieldHandle` (`m_ptr` points at a
                // real field info) whose field info names no field, so `IsNullHandle()` is false
                // and the `Argument_InvalidHandle` branch above is not the one taken. CoreCLR
                // instead reaches `if (!RuntimeFieldHandle.GetRVAFieldInfo(...)) throw new
                // ArgumentException(SR.Argument_BadFieldForInitializeArray)`, and the QCall returns
                // FALSE for a null `FieldDesc*` — a different message from the null-handle case.
                //
                // Defensive: PawPrint's `FieldHandleRegistry` only ever writes real ids, and
                // reflection populates `m_fieldHandle` from real ids too, so
                // `default(RuntimeFieldHandle)` lands on the `IsNullHandle` check above instead.
                // No C# reaches here.
                IntrinsicResult.RaiseException (
                    state,
                    baseClassTypes.ArgumentException,
                    Some "The field is invalid for initializing array or span."
                )
            | Some fieldHandle ->

            // `if (!RuntimeFieldHandle.GetRVAFieldInfo(fldInfo.Value, out address, out size))
            //      throw new ArgumentException(SR.Argument_BadFieldForInitializeArray);`
            // The QCall returns FALSE exactly when the field carries no RVA, and its `size`
            // out-parameter is the field's own `LoadSize()` — the bound the copy below must
            // respect so it cannot run off the end of the static into neighbouring data.
            let state, rvaData =
                FieldRvaData.tryGet loggerFactory baseClassTypes "InitializeArray" fieldHandle state

            match rvaData with
            | None ->
                IntrinsicResult.RaiseException (
                    state,
                    baseClassTypes.ArgumentException,
                    Some "The field is invalid for initializing array or span."
                )
            | Some rvaData ->

            let shape = ManagedHeap.getArrayShape arrayAddr state.ManagedHeap

            let elementHandle : ConcreteTypeHandle =
                match shape.ConcreteType with
                | ConcreteTypeHandle.OneDimArrayZero element -> element
                | ConcreteTypeHandle.Array (element, _) -> element
                | other ->
                    failwith
                        $"InitializeArray: object at %O{arrayAddr} is in the array heap but its concrete type %O{other} is not an array type"

            // `if (elementTH.IsTypeDesc || !elementTH.AsMethodTable()->IsPrimitive) // Enum is included
            //      throw new ArgumentException(SR.Argument_BadArrayForInitializeArray);`
            //
            // `IsPrimitive` is the MethodTable *category* test, so it is broader than
            // `MethodTableProjection.isTruePrimitive`: `SetInternalCorElementType` normalises an
            // enum to its underlying integer (methodtablebuilder.cpp:11157), which lands the enum
            // in the primitive category too. It also admits the three structs `CheckForSystemTypes`
            // normalises to `ELEMENT_TYPE_I` — `RuntimeArgumentHandle`,
            // `RuntimeMethodHandleInternal`, `RuntimeFieldHandleInternal`
            // (methodtablebuilder.cpp:10559). PawPrint models the latter two and answers `true` for
            // them, because that is the truthful answer to the question this classifier asks and a
            // zero-length array of them copies no bytes at all; whether PawPrint can *render* their
            // payload is a separate question, settled at the copy below. `RuntimeArgumentHandle` is
            // not modelled, so an array of it would take the `Argument_BadArrayForInitializeArray`
            // path CoreCLR would not have taken — a known gap, unreachable because no metadata can
            // name an RVA-initialised array of it.
            let state, elementIsPrimitive =
                match elementHandle with
                // A byref, pointer or function pointer element type is a CoreCLR `TypeDesc`, so it
                // fails the first half of the test; a nested array is a MethodTable but its
                // category is `Array`, so it fails the second. Both reject.
                | ConcreteTypeHandle.Byref _
                | ConcreteTypeHandle.Pointer _
                | ConcreteTypeHandle.FunctionPointer _
                | ConcreteTypeHandle.OneDimArrayZero _
                | ConcreteTypeHandle.Array _ -> state, false
                | ConcreteTypeHandle.Concrete _ ->
                    match IlMachineState.tryGetConcreteTypeInfo state elementHandle with
                    | None ->
                        failwith
                            $"InitializeArray: array element type %O{elementHandle} has no TypeDef row, so it cannot be classified as primitive"
                    | Some (elementCt, elementTypeInfo) ->

                    if MethodTableProjection.isTruePrimitive baseClassTypes elementTypeInfo then
                        state, true
                    else

                    match PrimitiveLikeStruct.kind baseClassTypes elementCt with
                    | Some PrimitiveLikeKind.FlattenToRuntimePointer ->
                        // `RuntimeMethodHandleInternal`/`RuntimeFieldHandleInternal`: normalised to
                        // `ELEMENT_TYPE_I`, so CoreCLR's category test says primitive, and so does
                        // this. Answering `false` to keep a later unsupported copy from being
                        // reached would make the classifier lie, and would turn a zero-length array
                        // — which CoreCLR copies nothing into and accepts — into a guest exception
                        // it would never have raised. PawPrint has no byte rendering for the runtime
                        // pointer these store, so a *non-empty* array instead fails in
                        // `CliType.OfBytesLike` at the copy below, where bytes are genuinely
                        // required.
                        state, true
                    | Some PrimitiveLikeKind.FlattenToNativeInt
                    | Some PrimitiveLikeKind.FlattenToObjectRef
                    | Some PrimitiveLikeKind.FlattenToManagedPointer
                    | Some PrimitiveLikeKind.EnumLike
                    | None ->
                        // `IntPtr`/`UIntPtr` are already covered by `isTruePrimitive`; everything
                        // else primitive-like is an ordinary value class to the type loader. What
                        // is left to decide is enum-ness, asked nominally (is the immediate base
                        // `System.Enum`?) as CoreCLR's `IsEnum()` does.
                        IlMachineState.isEnumValueType loggerFactory baseClassTypes state elementHandle

            if not elementIsPrimitive then
                IntrinsicResult.RaiseException (
                    state,
                    baseClassTypes.ArgumentException,
                    Some "Only array or span of primitive or enum types can be initialized from static data."
                )
            else

            // `nuint totalSize = pMT->ComponentSize * array.NativeLength;`
            // `ComponentSize` is the element type's storage size; take it from the element type's
            // zero rather than from a stored element so that an empty array is sized the same way
            // as a populated one. This is how `MethodTableProjection` projects `ComponentSize`.
            let elementZero, state =
                IlMachineState.cliTypeZeroOfHandle state baseClassTypes elementHandle

            let elementStride : int = CliType.sizeOf elementZero
            let totalSize : int64 = int64 elementStride * int64 shape.Length

            // `// make certain you don't go off the end of the rva static
            //  if (totalSize > size) throw new ArgumentException(SR.Argument_BadFieldForInitializeArray);`
            if totalSize > int64 rvaData.Size then
                IntrinsicResult.RaiseException (
                    state,
                    baseClassTypes.ArgumentException,
                    Some "The field is invalid for initializing array or span."
                )
            else

            // CoreCLR memmoves `totalSize` bytes from the static into the array's element data
            // (the little-endian branch; every target the CLR runs on is little-endian, which is
            // the same assumption `CliType.ToBytes` already makes). PawPrint stores elements as
            // structured `CliType`s rather than one byte blob, so the equivalent is to slice the
            // source per element and rebuild each element from its slice: `CliType.OfBytesLike`,
            // which `readPeByteRangeBytesAs` applies, is the inverse of the `CliType.ToBytes`
            // encoding used everywhere else, so enums — and anything else it later learns to
            // reconstruct — need no decoder of their own here.
            //
            // The template is the element type's zero — the same value the stride above was
            // measured from — rather than the cell's current contents. Every cell is overwritten
            // here regardless, so what one happens to hold on the way in is not information this
            // decode wants; reading it would be a guest-visible read performed to answer a
            // question about a type. Multi-dimensional arrays are stored flat in row-major order,
            // matching the CLR's own layout, so the same flat walk serves them.
            let state =
                (state, seq { 0 .. shape.Length - 1 })
                ||> Seq.fold (fun (state : IlMachineState) (i : int) ->
                    let decoded =
                        IlMachineState.readPeByteRangeBytesAs state rvaData (i * elementStride) elementZero

                    IlMachineState.setArrayValue arrayAddr decoded i state
                )

            let state = state |> IlMachineState.advanceProgramCounter currentThread
            IntrinsicResult.Completed state
        | "System.Private.CoreLib", "RuntimeHelpers", "IsBitwiseEquatable" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [], MethodReturnType.Returns (ConcreteBool state.ConcreteTypes) -> ()
            | _ -> failwith "bad signature for System.Private.CoreLib.RuntimeHelpers.IsBitwiseEquatable"

            let ty = Seq.exactlyOne methodToCall.Generics

            let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes ty

            let result =
                match CliType.unwrapPrimitiveLikeDeep zero with
                | CliType.Numeric numeric ->
                    match numeric with
                    | CliNumericType.Float32 _
                    | CliNumericType.Float64 _
                    | CliNumericType.NativeFloat _ -> false
                    | CliNumericType.Int32 _
                    | CliNumericType.Int64 _
                    | CliNumericType.Int8 _
                    | CliNumericType.Int16 _
                    | CliNumericType.UInt8 _
                    | CliNumericType.UInt16 _
                    | CliNumericType.NativeInt _ -> true
                | CliType.Bool _
                | CliType.Char _ -> true
                // Returning false is semantically safe: it only disables the BCL's bitwise
                // equality fast path. In PawPrint today that may still be observable for user
                // structs because the fallback SpanHelpers.SequenceEqual<T> path is not implemented.
                // TODO: Return true for eligible value types after implementing the same
                // override, field-recursion, and IEquatable<T> checks as the MethodTable QCall.
                | CliType.ValueType _
                | CliType.ObjectRef _
                | CliType.RuntimePointer _ -> false

            state
            |> IlMachineState.pushToEvalStack (CliType.ofBool result) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "GC", "KeepAlive" ->
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Object ], MethodReturnType.Void -> ()
            | _ -> failwith "bad signature for System.Private.CoreLib.GC.KeepAlive"

            let _, state = IlMachineState.popEvalStack currentThread state

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "Unsafe", "As" ->
            // https://github.com/dotnet/runtime/blob/721fdf6dcb032da1f883d30884e222e35e3d3c99/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/Unsafe.cs#L64
            let byrefAs () =
                let inputType, retType =
                    match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
                    | [ input ], MethodReturnType.Returns ret -> input, ret
                    | _ -> failwith "bad signature Unsafe.As"

                let from, to_ =
                    match Seq.toList methodToCall.Generics with
                    | [ from ; to_ ] -> from, to_
                    | _ -> failwith "bad generics"

                if ConcreteTypeHandle.Byref to_ <> retType then
                    failwith "bad return type"

                if ConcreteTypeHandle.Byref from <> inputType then
                    failwith "bad input type"

                let from =
                    match AllConcreteTypes.lookup from state.ConcreteTypes with
                    | None -> failwith "somehow have not concretised input type"
                    | Some t -> t

                let to_ =
                    match AllConcreteTypes.lookup to_ state.ConcreteTypes with
                    | None -> failwith "somehow have not concretised ret type"
                    | Some t -> t

                let inputAddr, state = IlMachineState.popEvalStack currentThread state

                let ptr =
                    match inputAddr with
                    | EvalStackValue.Int32 _
                    | EvalStackValue.Int64 _
                    | EvalStackValue.Float _ -> failwith "expected pointer type"
                    | EvalStackValue.NativeInt nativeIntSource -> failwith "todo"
                    | EvalStackValue.NullObjectRef ->
                        // A null byref spelled as an object reference, which is what hand-written
                        // IL produces; `reinterpretAs` below handles the
                        // `ManagedPointer ManagedPointerSource.Null` spelling that guest C#
                        // produces, but it takes a `ManagedPointerSource` and so cannot see this
                        // one. Same answer either way: `Unsafe.As` does not null-check (its CoreLib
                        // body is `ldarg.0; ret`, and the JIT's `NI_SRCS_UNSAFE_As` expansion is a
                        // bare `impPopStack().val`), and reinterpreting is address-preserving, so
                        // null in means null out.
                        //
                        // Normalising to the managed-pointer spelling matches how `Unsafe.NullRef`
                        // and `Unsafe.AsRef(void*)` above represent a null byref, so that
                        // `Unsafe.IsNullRef` recognises the result.
                        EvalStackValue.ManagedPointer ManagedPointerSource.Null
                    | EvalStackValue.ManagedPointer src when from = to_ ->
                        // Unsafe.As<T,T> is a no-op: same address and same type view.
                        // Skipping the projection keeps the representation canonical so
                        // that AreSame / ceq on the result compares equal to the input.
                        EvalStackValue.ManagedPointer src
                    | EvalStackValue.ManagedPointer src ->
                        // `reinterpretAs` rather than a bare `appendProjection`: the change of
                        // type view is address-preserving and never dereferences, so it is also
                        // defined on a null byref and on an `Unsafe.AsRef<T>((void*)bits)`
                        // placeholder, neither of which can carry a projection.
                        ManagedPointerSource.reinterpretAs to_ src |> EvalStackValue.ManagedPointer
                    | EvalStackValue.ObjectRef addr -> failwith "todo: Unsafe.As on ObjectRef"
                    | EvalStackValue.UserDefinedValueType evalStackValueUserType -> failwith "todo"

                let state =
                    state
                    |> IlMachineState.pushToEvalStack' ptr currentThread
                    |> IlMachineState.advanceProgramCounter currentThread

                IntrinsicResult.Completed state

            match methodToCall.Signature.ParameterTypes, Seq.toList methodToCall.Generics with
            | [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Object ], [ target ] ->
                if methodToCall.Signature.ReturnType <> MethodReturnType.Returns target then
                    failwith "bad return type Unsafe.As<T>(object)"

                let obj, state = IlMachineState.popEvalStack currentThread state

                match obj with
                | EvalStackValue.ObjectRef _
                | EvalStackValue.NullObjectRef ->
                    state
                    |> IlMachineState.pushToEvalStack' obj currentThread
                    |> IlMachineState.advanceProgramCounter currentThread
                    |> IntrinsicResult.Completed
                | other -> failwith $"Unsafe.As<T>(object): expected object reference, got %O{other}"
            | _ -> byrefAs ()
        | "System.Private.CoreLib", "Unsafe", "BitCast" ->
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/Unsafe.cs#L259
            // BCL body:
            //   if (sizeof(TFrom) != sizeof(TTo)
            //       || !typeof(TFrom).IsValueType
            //       || !typeof(TTo).IsValueType)
            //       ThrowHelper.ThrowNotSupportedException();
            //   return ReadUnaligned<TTo>(ref As<TFrom, byte>(ref source));
            //
            // PawPrint models this as a primitive byte reinterpretation between
            // two byte-addressable storage shapes. We are stricter than the BCL:
            // a value type carrying provenance the byte model cannot render
            // (managed pointers, runtime/method/field handles, GC handles, ...)
            // is rejected via `CliType.ByteAddressability`. The BCL would happily
            // produce undefined garbage in those cases; refusing is consistent
            // with PawPrint's deterministic byte model and with the user-facing
            // contract "between equal-sized unmanaged storage shapes".
            //
            // The one exception is `TFrom` and `TTo` naming the *same* concrete type,
            // where no reinterpretation happens at all and the value can be moved
            // across as it stands — see the equal-handle arm below.
            let fromHandle, toHandle =
                match Seq.toList methodToCall.Generics with
                | [ f ; t ] -> f, t
                | _ -> failwith "bad generics Unsafe.BitCast: expected exactly two type arguments"

            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ paramTy ], MethodReturnType.Returns retTy when paramTy = fromHandle && retTy = toHandle -> ()
            | _ -> failwith $"bad signature Unsafe.BitCast: %A{methodToCall.Signature}"

            let fromZero, state =
                IlMachineState.cliTypeZeroOfHandle state baseClassTypes fromHandle

            let toZero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes toHandle

            let fromSize = CliType.sizeOf fromZero
            let toSize = CliType.sizeOf toZero

            let popped, state = IlMachineState.popEvalStack currentThread state
            let inputCli = EvalStackValue.toCliTypeCoerced fromZero popped

            let inputAddressable =
                match CliType.ByteAddressability inputCli with
                | CliByteAddressability.ByteAddressable -> true
                | CliByteAddressability.Rejected _ -> false

            let targetAddressable =
                match CliType.ByteAddressability toZero with
                | CliByteAddressability.ByteAddressable -> true
                | CliByteAddressability.Rejected _ -> false

            // `!typeof(T).IsValueType` from the BCL guard. Reference types are pointer-sized on
            // both sides, so the size clause does not catch them; without this they would reach
            // PawPrint's byte model and be rejected as a host failure instead of the
            // `NotSupportedException` the BCL raises. The JIT declines to expand for them for
            // exactly this reason ("Fallback to the software implementation to throw for
            // reference types").
            //
            // Byrefs, pointers, function pointers and arrays are TypeDescs, for which CoreCLR's
            // `IsValueTypeImpl` resolves to `IsSubclassOf(typeof(ValueType))` — false for all of
            // them. `isValueTypeHandleAsCoreClr` is where that classification lives.
            let isValueTypeHandle (handle : ConcreteTypeHandle) : bool =
                IntrinsicHelpers.isValueTypeHandleAsCoreClr baseClassTypes state "Unsafe.BitCast" handle

            // The BCL's guard is a single `if` with three clauses, all of which mean
            // `NotSupportedException` via `ThrowHelper.ThrowNotSupportedException` — the
            // parameterless ctor, hence no message override.
            //
            // The two addressability clauses below are PawPrint's own stricter rule, described
            // above, and are NOT part of that guard: the BCL accepts those inputs
            // (`Unsafe.BitCast<IntPtr, long>` is legal .NET). Raising a guest exception for them
            // would make PawPrint throw where the real runtime succeeds, so they stay a host
            // failure with a precise diagnostic. Because the value-type clause is checked first
            // and the equal-handle arm intercepts the rest, what reaches them is only a genuine
            // reinterpretation — two *distinct* value types, one of whose provenance the byte
            // model cannot render — which is the real PawPrint-only restriction.
            if
                fromSize <> toSize
                || not (isValueTypeHandle fromHandle)
                || not (isValueTypeHandle toHandle)
            then
                IntrinsicResult.RaiseException (state, baseClassTypes.NotSupportedException, None)
            elif fromHandle = toHandle then
                // Identical concrete types: no reinterpretation, so nothing to render as bytes.
                // The JIT agrees, and does not even emit a copy — "Handle matching handles,
                // compatible struct layouts or integrals where we can simply return op1"
                // (importercalls.cpp, `NI_SRCS_UNSAFE_BitCast`). Serving this arm without
                // consulting `ByteAddressability` is what lets provenance-carrying value types
                // (a `ReadOnlySpan<char>`, a struct holding a `RuntimeTypeHandle`) through: their
                // contents travel intact rather than being laundered into a bit pattern.
                //
                // `AllConcreteTypes` deduplicates by (identity, generic arguments), so equal
                // handles really do mean the same type — including when only one side spells it
                // through a generic parameter, as CoreLib's `TChar`-generic formatting code does.
                // Unequal handles may still denote types of the same *shape*; those keep the byte
                // path, because moving between distinct types is exactly the reinterpretation
                // this restriction is about.
                //
                // The BCL's value-type guard above still applies: `BitCast<string, string>`
                // throws, so this arm deliberately sits after it rather than before.
                state
                |> IlMachineState.pushToEvalStack inputCli currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> IntrinsicResult.Completed
            elif not inputAddressable || not targetAddressable then
                let reason =
                    if not inputAddressable then
                        $"input is not byte-addressable: %s{(CliType.ByteAddressability inputCli).Description}"
                    else
                        $"target is not byte-addressable: %s{(CliType.ByteAddressability toZero).Description}"

                failwith
                    $"TODO: Unsafe.BitCast<%O{fromHandle}, %O{toHandle}> is rejected by PawPrint's byte model, though the BCL would allow it (%s{reason})"
            else
                let bytes = CliType.ToBytes inputCli
                let result = CliType.OfBytesLike toZero bytes

                state
                |> IlMachineState.pushToEvalStack result currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "Unsafe", "SizeOf" ->
            // https://github.com/dotnet/runtime/blob/721fdf6dcb032da1f883d30884e222e35e3d3c99/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/Unsafe.cs#L51
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [], MethodReturnType.Returns (ConcreteInt32 state.ConcreteTypes) -> ()
            | _ -> failwith "bad signature Unsafe.SizeOf"

            let ty =
                match Seq.toList methodToCall.Generics with
                | [ ty ] -> ty
                | _ -> failwith "bad generics"

            let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes ty

            let size = CliType.sizeOf zero

            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 size)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "Unsafe", "AreSame" ->
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/coreclr/tools/Common/TypeSystem/IL/Stubs/UnsafeIntrinsics.cs#L55
            // The source-level IL body throws PlatformNotSupportedException; the JIT replaces it with ceq on two byrefs.
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteByref _ ; ConcreteByref _ ], MethodReturnType.Returns (ConcreteBool state.ConcreteTypes) -> ()
            | _ -> failwith "bad signature Unsafe.AreSame"

            let right, state = IlMachineState.popEvalStack currentThread state
            let left, state = IlMachineState.popEvalStack currentThread state

            let extractPtr (v : EvalStackValue) : ManagedPointerSource =
                match v with
                | EvalStackValue.ManagedPointer p -> p
                | _ -> failwith $"TODO: Unsafe.AreSame: expected ManagedPointer, got %O{v}"

            // `ReinterpretAs` projections are address-preserving, so two byrefs
            // that reach the same byte location by different reinterpret chains
            // must compare equal. Strip trailing reinterprets before comparison.
            // A `ReinterpretAs` followed by a `Field` would need a bytewise
            // layout comparison (a field at the same offset under different
            // type views still aliases); refuse rather than risk a silent false
            // negative.
            let leftPtr = extractPtr left
            let rightPtr = extractPtr right

            let normalisation =
                ManagedPointerByteView.normalisationContextForPointers state [ leftPtr ; rightPtr ]

            let leftNormalised =
                ManagedPointerSource.normaliseForComparison normalisation leftPtr

            let rightNormalised =
                ManagedPointerSource.normaliseForComparison normalisation rightPtr

            // `AreSame` *is* byref CEQ — the JIT lowers it to exactly that — so it shares
            // `ceqNormalised` rather than keeping a second copy of the stripping rules and
            // the shapes they must refuse.
            let areSame =
                ManagedPointerSource.ceqNormalised "Unsafe.AreSame" leftNormalised rightNormalised

            state
            |> IlMachineState.pushToEvalStack (CliType.ofBool areSame) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "Unsafe", "IsAddressLessThan" ->
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/coreclr/tools/Common/TypeSystem/IL/Stubs/UnsafeIntrinsics.cs#L62-L67
            // The source-level IL body throws PlatformNotSupportedException; the runtime replaces
            // it with `ldarg.0; ldarg.1; clt.un; ret` (the commented-out body in CoreLib's
            // Unsafe.cs spells exactly that). So this delegates to the very function that services
            // the `Clt_un` opcode: whatever ordering `clt.un` gives two byrefs, this must agree,
            // including the null-byref arms and the refusal to order byrefs with no common root.
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteByref _ ; ConcreteByref _ ], MethodReturnType.Returns (ConcreteBool state.ConcreteTypes) -> ()
            | _ ->
                failwith
                    $"bad signature Unsafe.IsAddressLessThan: expected two byref parameters and bool return, got %A{methodToCall.Signature}"

            let right, state = IlMachineState.popEvalStack currentThread state
            let left, state = IlMachineState.popEvalStack currentThread state

            let isLessThan = EvalStackValueComparisons.cltUn left right

            state
            |> IlMachineState.pushToEvalStack (CliType.ofBool isLessThan) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "Unsafe", "Add" ->
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/coreclr/tools/Common/TypeSystem/IL/Stubs/UnsafeIntrinsics.cs#L99
            // The source-level IL body throws PlatformNotSupportedException; the JIT replaces it with sizeof + conv.i + mul + add.
            let t =
                match Seq.toList methodToCall.Generics with
                | [ t ] -> t
                | _ -> failwith "bad generics Unsafe.Add"

            // Three overloads: `(ref T, int32)`, `(ref T, IntPtr)`, `(ref T, UIntPtr)`.
            // The IntPtr/UIntPtr overloads exist for native-sized element indices
            // (e.g. `Unsafe.Add(ref T, (nint)n)`). All three are JIT-lowered to
            // `sizeof * offset + base`, so we treat them uniformly.
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteByref tFromParam ; ConcreteInt32 state.ConcreteTypes ],
              MethodReturnType.Returns (ConcreteByref tFromRet)
            | [ ConcreteByref tFromParam ; ConcreteIntPtr state.ConcreteTypes ],
              MethodReturnType.Returns (ConcreteByref tFromRet)
            | [ ConcreteByref tFromParam ; ConcreteUIntPtr state.ConcreteTypes ],
              MethodReturnType.Returns (ConcreteByref tFromRet) when tFromParam = t && tFromRet = t -> ()
            | _ ->
                failwith
                    $"TODO: Unsafe.Add: only the (ref T, int32), (ref T, IntPtr), and (ref T, UIntPtr) overloads are implemented; got params %A{methodToCall.Signature.ParameterTypes}"

            let offset, state = IlMachineState.popEvalStack currentThread state
            let src, state = IlMachineState.popEvalStack currentThread state

            // `conv.i` / `conv.u` produce `EvalStackValue.NativeInt (Verbatim ...)`;
            // the IntPtr/UIntPtr overloads feed us one of those. The int32 overload
            // produces `EvalStackValue.Int32` directly. Both narrow safely to int
            // so long as the verbatim value fits; on a 64-bit host the C# compiler
            // never emits an out-of-range native-int offset for array arithmetic.
            let offset =
                match offset with
                | EvalStackValue.Int32 (Int32Source.Verbatim i) -> i
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) ->
                    if i < int64<int> System.Int32.MinValue || i > int64<int> System.Int32.MaxValue then
                        failwith
                            $"TODO: Unsafe.Add: native-int offset %d{i} does not fit in Int32; byte-level arithmetic on array byrefs is not modelled"

                    int32<int64> i
                | _ -> failwith $"TODO: Unsafe.Add: expected Int32 or Verbatim NativeInt offset, got %O{offset}"

            let ptr, state =
                offsetManagedPointerByElements baseClassTypes state t (int64<int> offset) src

            state
            |> IlMachineState.pushToEvalStack' ptr currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "Unsafe", "Subtract" ->
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/Unsafe.cs#L812-L833
            // The CORECLR managed body throws PlatformNotSupportedException; the JIT replaces it
            // with `sizeof !!T; conv.i; mul; sub`, which is exactly the `Unsafe.Add<T>(ref T, int32)`
            // lowering above with `add` swapped for `sub`. So this walks the same element-offset
            // path with the offset negated, and every byref shape `Add` supports is supported here
            // by construction rather than by a parallel implementation.
            let t =
                match Seq.toList methodToCall.Generics with
                | [ t ] -> t
                | _ -> failwith "bad generics Unsafe.Subtract"

            // Only the `(ref T, int32)` overload. `(ref T, IntPtr)`, `(ref T, nuint)` and
            // `(void*, int32)` are separate JIT intrinsics; the `nuint` one in particular cannot
            // share this arm, because its element offset is *unsigned* and so does not negate the
            // way a signed one does.
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteByref tFromParam ; ConcreteInt32 state.ConcreteTypes ],
              MethodReturnType.Returns (ConcreteByref tFromRet) when tFromParam = t && tFromRet = t -> ()
            | _ ->
                failwith
                    $"TODO: Unsafe.Subtract: only the (ref T, int32) overload is implemented; got params %A{methodToCall.Signature.ParameterTypes} and return %A{methodToCall.Signature.ReturnType}"

            let offset, state = IlMachineState.popEvalStack currentThread state
            let src, state = IlMachineState.popEvalStack currentThread state

            let offset = int32ValueArgument "Unsafe.Subtract" offset

            // Negate at native-int width, which is where the IL's `mul`/`sub` happen. Doing it in
            // int32 would wrap `Int32.MinValue` back to itself and move the byref 2^32 elements the
            // wrong way; and narrowing the result would refuse walks the element walk can represent
            // perfectly well (`Subtract(ref a[-1], Int32.MinValue)` lands on `Int32.MaxValue`).
            // Whether the destination is representable depends on the source byref's shape, so that
            // judgement belongs to `offsetManagedPointerByElements`, which can see it.
            let ptr, state =
                offsetManagedPointerByElements baseClassTypes state t (-(int64<int32> offset)) src

            state
            |> IlMachineState.pushToEvalStack' ptr currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "Unsafe", "AddByteOffset" ->
            // CoreCLR's managed body throws PlatformNotSupportedException; the JIT replaces
            // the call with raw byref + native-int addition. Both overloads (IntPtr and
            // UIntPtr) share the same semantics: advance the byref by `byteOffset` bytes,
            // preserving the static `T` view.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/Unsafe.cs#L661
            // (the UIntPtr overload is at L210 of the same file.)
            let t =
                match Seq.toList methodToCall.Generics with
                | [ t ] -> t
                | _ -> failwith "bad generics Unsafe.AddByteOffset"

            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteByref tFromParam ; ConcreteIntPtr state.ConcreteTypes ],
              MethodReturnType.Returns (ConcreteByref tFromRet)
            | [ ConcreteByref tFromParam ; ConcreteUIntPtr state.ConcreteTypes ],
              MethodReturnType.Returns (ConcreteByref tFromRet) when tFromParam = t && tFromRet = t -> ()
            | _ ->
                failwith
                    $"TODO: Unsafe.AddByteOffset: only the (ref T, IntPtr) and (ref T, UIntPtr) overloads are implemented; got params %A{methodToCall.Signature.ParameterTypes}"

            let offset, state = IlMachineState.popEvalStack currentThread state
            let src, state = IlMachineState.popEvalStack currentThread state

            let offset : int =
                let ofInt64 (i : int64) : int =
                    if i < int64<int> System.Int32.MinValue || i > int64<int> System.Int32.MaxValue then
                        failwith $"TODO: Unsafe.AddByteOffset: native-int byte offset %d{i} does not fit in Int32"

                    int32<int64> i

                match offset with
                | EvalStackValue.NativeInt (NativeIntSource.Verbatim i) -> ofInt64 i
                | EvalStackValue.Int32 (Int32Source.Verbatim i) -> i
                // A `nuint`/`nint` byte offset that was seeded from `IntPtr.Zero` (or `(nint)0`)
                // arrives as `ManagedPointer Null`, because zero is the canonical null byref;
                // accumulating onto it with `add` then yields a `NativeIntPlaceholder`. Both are
                // bit-pattern byrefs — pure native-int values with no storage behind them — so
                // their bits *are* the offset. `Ordinal.EqualsIgnoreCase_Scalar` is written
                // exactly this way (`IntPtr byteOffset = IntPtr.Zero; … byteOffset += 8`).
                // A byref anchored to real storage is not a number, and still fails loudly.
                | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer offsetPtr) ->
                    match ManagedPointerSource.tryBitPatternBits offsetPtr with
                    | ValueSome bits -> ofInt64 bits
                    | ValueNone ->
                        failwith
                            $"TODO: Unsafe.AddByteOffset: byte offset is an anchored byref, not a native-int value: %O{offset}"
                | _ ->
                    failwith
                        $"TODO: Unsafe.AddByteOffset: expected Verbatim NativeInt or Int32 byte offset, got %O{offset}"

            let srcPtr =
                match src with
                | EvalStackValue.ManagedPointer p -> p
                | _ -> failwith $"TODO: Unsafe.AddByteOffset on non-ManagedPointer source byref: %O{src}"

            // `Unsafe.AsRef<T>((void*)bits)` byrefs are bit patterns, not
            // anchored byrefs. `Unsafe.AddByteOffset` on a placeholder is just
            // bit addition; appending a `ReinterpretAs` would be meaningless on
            // a target that doesn't represent memory. `Null` is the bit pattern
            // `0` (we normalise placeholder→Null on zero), so an offset from
            // `Null` must use the same bit-arithmetic route — otherwise the
            // chain `placeholder + (-bits)` (which normalises to `Null`) +
            // another `AddByteOffset` would fall into the byref path and try
            // to project off a null managed pointer.
            match ManagedPointerSource.tryBitPatternBits srcPtr with
            | ValueSome bits ->
                let ptr = bits + int64 offset |> ManagedPointerSource.ofBitPattern

                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ptr) currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> IntrinsicResult.Completed
            | ValueNone ->

            // `addByteOffsetUnderReinterpret` anchors the byte cursor under `ReinterpretAs T`
            // before appending the offset, so it works regardless of whether the source byref
            // already carries a trailing byte-view tail. The trailing `ReinterpretAs T` is
            // address-preserving; the `appendProjection` collapse rules handle the common
            // case where the source already has a `ReinterpretAs T` (idempotent) or a
            // `[ReinterpretAs T; ByteOffset n]` tail whose `n` cancels the new offset (e.g.
            // `RawData::Data` on an array followed by the canonical `+sizeof(nint)` skip).
            //
            // The byte-view path requires the reinterpret target's storage to be
            // byte-addressable on read. Object references (and value types containing
            // them) deliberately are not, so a naturally-typed byref to such cells
            // must stay in its natural form. We short-circuit when (a) the source
            // is itself naturally-typed (no trailing byte-view tail) and (b) the
            // byte offset is a whole-cell multiple, so the result is still
            // expressible without a reinterpret tail. The general byte-view path
            // handles all other shapes.
            let normalisation =
                match srcPtr with
                | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, _), _) ->
                    // See `IntrinsicHelpers.offsetManagedPointerByElements`: a zero here means
                    // "do not normalise", so an empty array used to keep a raw byte cursor
                    // where a populated one folded it into the cell index.
                    let elementSize = (ManagedHeap.getArrayShape arr state.ManagedHeap).ElementStride

                    ByteOffsetNormalisationContext.withArrayElementSize arr elementSize
                | _ -> ByteOffsetNormalisationContext.fixedStrideRootsOnly

            let typedShortcut : ManagedPointerSource option =
                match srcPtr with
                | ManagedPointerSource.Byref (root, projs) ->
                    let hasByteViewTail =
                        match List.tryLast projs with
                        | Some (ByrefProjection.ReinterpretAs _)
                        | Some (ByrefProjection.ByteOffset _) -> true
                        | _ -> false

                    if hasByteViewTail then
                        None
                    elif offset = 0 then
                        // Zero-byte advance on a naturally-typed byref is the identity;
                        // returning the source preserves the typed view that the bytewise
                        // path would otherwise destroy by appending a `ReinterpretAs T`.
                        Some srcPtr
                    else
                        match root, projs with
                        | ByrefRoot.ArrayElement (arr, i), [] ->
                            // `ElementStride` is strictly positive by construction (see
                            // `ArrayShape`), so no divisor check is needed and an empty array
                            // needs no special case: the whole-cell test is a question about
                            // the element type, which an empty array has just like any other.
                            let elementSize = (ManagedHeap.getArrayShape arr state.ManagedHeap).ElementStride

                            if offset % elementSize <> 0 then
                                None
                            else

                            // `ArrayElement` stores an int32 cell index, so a fold that does
                            // not fit in one cannot be represented — and wrapping would not
                            // merely lose precision, it would put the byref on the *wrong side*
                            // of its own root, so `Unsafe.ByteOffset` would report
                            // -8589934592 bytes instead of +8589934592. Refuse what we cannot
                            // represent, as `IntrinsicHelpers.offsetManagedPointerByElements`
                            // already does for the same arithmetic.
                            //
                            // Declining the shortcut instead would not give a right answer
                            // either: the byte-view fallback normalises the resulting cursor
                            // back into the cell index through `normaliseTrailingByteOffset`,
                            // which performs the same addition. That file is `Checked`, so it
                            // raises `OverflowException` rather than wrapping — a crash, but
                            // one naming neither the byref nor the walk that produced it.
                            let folded = int64<int> i + int64<int> (offset / elementSize)

                            if
                                folded < int64<int> System.Int32.MinValue
                                || folded > int64<int> System.Int32.MaxValue
                            then
                                failwith
                                    $"TODO: Unsafe.AddByteOffset: advancing the byref at cell %d{i} of array %O{arr} by %d{offset} bytes reaches cell %d{folded}, which does not fit in the int32 PawPrint stores for a cell index; a byref this far from its root is not modelled"

                            Some (ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, int32<int64> folded), []))
                        | _ -> None
                | _ -> None

            // Concretising T is only required for the byte-view fallback (which
            // anchors a `ReinterpretAs T` tail). The typed shortcut never touches
            // T, so structural concrete-type handles (array, pointer, function
            // pointer) — which `AllConcreteTypes.lookup` doesn't store — can still
            // resolve cleanly through the shortcut.
            let ptr =
                match typedShortcut with
                | Some p -> p
                | None ->
                    let tConcrete =
                        match AllConcreteTypes.lookup t state.ConcreteTypes with
                        | Some c -> c
                        | None -> failwith $"Unsafe.AddByteOffset: T not concretised: %O{t}"

                    ManagedPointerSource.addByteOffsetUnderReinterpret normalisation tConcrete offset srcPtr

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ptr) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "Unsafe", "ByteOffset" ->
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/coreclr/tools/Common/TypeSystem/IL/Stubs/UnsafeIntrinsics.cs#L69
            // The source-level IL body throws PlatformNotSupportedException; the JIT replaces it with sub on two byrefs.
            let t =
                match Seq.toList methodToCall.Generics with
                | [ t ] -> t
                | _ -> failwith "bad generics Unsafe.ByteOffset"

            match methodToCall.Signature.ParameterTypes with
            | [ ConcreteByref _ ; ConcreteByref _ ] -> ()
            | _ -> failwith "bad signature Unsafe.ByteOffset"

            let target, state = IlMachineState.popEvalStack currentThread state
            let origin, state = IlMachineState.popEvalStack currentThread state

            let tSize, state =
                let tZero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes t
                CliType.sizeOf tZero, state

            // `Unsafe.AsRef<T>((void*)bits)` byrefs are bit patterns, not
            // anchored byrefs. `Unsafe.ByteOffset` on a pair of them is just
            // the bit-difference, matching the IL `sub` semantics implemented
            // in BinaryArithmetic. Null is the placeholder for bits=0, so
            // pairings with Null are still well-defined as bit subtraction.
            let asPlaceholderBits (v : EvalStackValue) : int64 voption =
                match v with
                | EvalStackValue.ManagedPointer ptr -> ManagedPointerSource.tryBitPatternBits ptr
                | _ -> ValueNone

            match asPlaceholderBits origin, asPlaceholderBits target with
            | ValueSome originBits, ValueSome targetBits ->
                let byteOffset = targetBits - originBits

                state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.NativeInt (NativeIntSource.Verbatim byteOffset))
                    currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> IntrinsicResult.Completed
            | _ ->

            // ByteOffset measures the byte distance between two byref address
            // targets. The generic T on the method is only the static view
            // through which each byref was declared; reinterpreting a byref
            // doesn't move it. Trailing `ByteOffset` projections contribute
            // to the absolute byte address; `ReinterpretAs` projections are
            // address-preserving.
            let extractByteLocation (v : EvalStackValue) : ByteStorageIdentity * int64 =
                let src =
                    match v with
                    | EvalStackValue.ManagedPointer p -> p
                    | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer p) -> p
                    | _ -> failwith $"TODO: Unsafe.ByteOffset on non-ManagedPointer: %O{v}"

                let projectionByteOffset (projs : ByrefProjection list) : int64 =
                    let mutable byteOff = 0L

                    for p in projs do
                        match p with
                        | ByrefProjection.ReinterpretAs _ -> ()
                        | ByrefProjection.ByteOffset n -> byteOff <- byteOff + int64 n
                        | _ -> failwith $"TODO: Unsafe.ByteOffset on byref with non-ReinterpretAs projection: %O{p}"

                    byteOff

                match src with
                | ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (thread, frame, block, byteOffset), projs) ->
                    ByteStorageIdentity.StackMemory (thread, frame, block),
                    int64 byteOffset + projectionByteOffset projs
                | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (block, byteOffset), projs) ->
                    ByteStorageIdentity.NativeMemory block, int64 byteOffset + projectionByteOffset projs
                | ManagedPointerSource.Byref (ByrefRoot.LocalVariable (thread, frame, local), projs) ->
                    ByteStorageIdentity.StackLocal (thread, frame, local), projectionByteOffset projs
                | ManagedPointerSource.Byref (ByrefRoot.Argument (thread, frame, arg), projs) ->
                    ByteStorageIdentity.StackArgument (thread, frame, arg), projectionByteOffset projs
                | ManagedPointerSource.Byref (ByrefRoot.StaticField (declaringType, field, owner), projs) ->
                    ByteStorageIdentity.StaticField (declaringType, field, owner), projectionByteOffset projs
                | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, i), projs) ->
                    // The cell index is a position in *this array's* layout, so the array's
                    // stride is what converts it to bytes — not `sizeof(T)` from the calling
                    // method, which is only the same number when `T` is the element type.
                    // `Array.Empty<T>()` needs no special case: it has no stored element to
                    // measure, but it has a recorded stride like any other array.
                    let elementSize = (ManagedHeap.getArrayShape arr state.ManagedHeap).ElementStride

                    ByteStorageIdentity.Array arr, int64 i * int64 elementSize + projectionByteOffset projs
                | ManagedPointerSource.Byref (ByrefRoot.StringCharAt (str, charIndex), projs) ->
                    ByteStorageIdentity.String str, int64 charIndex * 2L + projectionByteOffset projs
                | ManagedPointerSource.Byref (ByrefRoot.PeByteRange peByteRange, projs) ->
                    ByteStorageIdentity.PeByteRange peByteRange, projectionByteOffset projs
                | _ -> failwith $"TODO: Unsafe.ByteOffset on unsupported byref: %O{v}"

            let storage1, originOffset = extractByteLocation origin
            let storage2, targetOffset = extractByteLocation target

            // Same-storage ByteOffset is an honest byte delta and composes
            // correctly with Unsafe.Add / further arithmetic. Cross-storage
            // ByteOffset has no principled byte distance in our model, so we
            // reuse the cross-storage helper to synthesise a
            // deterministic sentinel large enough to defeat the unsigned
            // overlap check `(nuint)offset < len` used by Memmove. The tag
            // makes any subsequent `add`/`sub` fail loudly via BinaryArithmetic.execute's
            // "refusing to operate on non-verbatim native int" branch, rather
            // than silently composing into a wrong answer.
            if storage1 = storage2 then
                let byteOffset = targetOffset - originOffset

                state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.NativeInt (NativeIntSource.Verbatim byteOffset))
                    currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> IntrinsicResult.Completed
            else
                let byteOffset =
                    NativeIntSource.syntheticCrossStorageByteOffset storage1 originOffset storage2 targetOffset

                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt byteOffset) currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> IntrinsicResult.Completed
        | "System.Private.CoreLib", ("ReadOnlySpan`1" | "Span`1"), "get_Item" ->
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/libraries/System.Private.CoreLib/src/System/ReadOnlySpan.cs#L141
            // The source-level body returns `ref Unsafe.Add(ref _reference, index)`;
            // the method is intrinsic so we model that primitive boundary directly.
            let spanTypeName : string = methodToCall.RequiredDeclaringType.Name

            let elementType : ConcreteTypeHandle =
                methodToCall.DeclaringTypeGenerics |> Seq.exactlyOne

            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcreteInt32 state.ConcreteTypes ], MethodReturnType.Returns (ConcreteByref ret) when ret = elementType ->
                ()
            | _ ->
                failwith
                    $"bad signature for System.Private.CoreLib.%s{spanTypeName}.get_Item: %A{methodToCall.Signature}"

            let index, state = IlMachineState.popEvalStack currentThread state
            let receiver, state = IlMachineState.popEvalStack currentThread state

            let index : int =
                match index with
                | EvalStackValue.Int32 (Int32Source.Verbatim i) -> i
                | other -> failwith $"%s{spanTypeName}.get_Item expected Int32 index, got %O{other}"

            let span : CliValueType =
                match receiver with
                | EvalStackValue.ManagedPointer src ->
                    match IlMachineState.readManagedByref baseClassTypes state src with
                    | CliType.ValueType vt -> vt
                    | other ->
                        failwith $"%s{spanTypeName}.get_Item receiver byref read produced non-value-type %O{other}"
                | EvalStackValue.UserDefinedValueType vt -> vt
                | other -> failwith $"%s{spanTypeName}.get_Item expected span receiver byref, got %O{other}"

            let length : int =
                let lengthField =
                    IlMachineState.requiredOwnInstanceFieldId state span.Declared "_length"

                match
                    CliValueType.DereferenceFieldById lengthField span
                    |> CliType.unwrapPrimitiveLike
                with
                | CliType.Numeric (CliNumericType.Int32 i) -> i
                | other -> failwith $"%s{spanTypeName}.get_Item expected _length to be int32, got %O{other}"

            if uint32<int32> index >= uint32<int32> length then
                // `ThrowHelper.ThrowIndexOutOfRangeException()`, i.e. the parameterless ctor, so
                // no message override. Both arguments are already popped and the PC has not been
                // advanced, which is what the raise needs.
                //
                // The unsigned comparison is the BCL's own: it folds the negative-index case into
                // the same branch.
                IntrinsicResult.RaiseException (state, baseClassTypes.IndexOutOfRangeException, None)
            else

            let reference : EvalStackValue =
                let referenceField =
                    IlMachineState.requiredOwnInstanceFieldId state span.Declared "_reference"

                match
                    CliValueType.DereferenceFieldById referenceField span
                    |> CliType.unwrapPrimitiveLikeDeep
                with
                | CliType.RuntimePointer (CliRuntimePointer.Managed src) -> EvalStackValue.ManagedPointer src
                | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer src)) ->
                    EvalStackValue.ManagedPointer src
                | other ->
                    failwith $"%s{spanTypeName}.get_Item expected _reference to be a managed byref, got %O{other}"

            let ptr, state =
                offsetManagedPointerByElements baseClassTypes state elementType (int64<int> index) reference

            state
            |> IlMachineState.pushToEvalStack' ptr currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "Span`1", "Clear" ->
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/libraries/System.Private.CoreLib/src/System/Span.cs#L280
            // Span<T>.Clear is a JIT intrinsic; the BCL IL falls through to
            // SpanHelpers.ClearWithReferences (for a T containing references) or
            // ClearWithoutReferences (otherwise). Only the latter is itself `[Intrinsic]`,
            // and it is implemented below; `ClearWithReferences` is plain managed IL whose
            // `Unsafe.Add(ref ip, n) = default` writes land as pointer-width zero stores
            // through a reinterpreted byref onto object-reference cells, which the
            // byref-write model does not yet support. So the reference half of this IL is
            // still not walkable, and we keep modelling the JIT semantics directly: write
            // default(T) to each of `_length` elements starting at `_reference`, using the
            // same byref-projection helpers as get_Item. That is also the more direct
            // model — it needs no byte-count derivation at all.
            let elementType : ConcreteTypeHandle =
                methodToCall.DeclaringTypeGenerics |> Seq.exactlyOne

            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [], MethodReturnType.Void -> ()
            | _ -> failwith $"bad signature for System.Span`1.Clear: %A{methodToCall.Signature}"

            let receiver, state = IlMachineState.popEvalStack currentThread state

            let span : CliValueType =
                match receiver with
                | EvalStackValue.ManagedPointer src ->
                    match IlMachineState.readManagedByref baseClassTypes state src with
                    | CliType.ValueType vt -> vt
                    | other -> failwith $"Span`1.Clear receiver byref read produced non-value-type %O{other}"
                | EvalStackValue.UserDefinedValueType vt -> vt
                | other -> failwith $"Span`1.Clear expected span receiver byref, got %O{other}"

            let length : int =
                let lengthField =
                    IlMachineState.requiredOwnInstanceFieldId state span.Declared "_length"

                match
                    CliValueType.DereferenceFieldById lengthField span
                    |> CliType.unwrapPrimitiveLike
                with
                | CliType.Numeric (CliNumericType.Int32 i) -> i
                | other -> failwith $"Span`1.Clear expected _length to be int32, got %O{other}"

            let reference : EvalStackValue =
                let referenceField =
                    IlMachineState.requiredOwnInstanceFieldId state span.Declared "_reference"

                match
                    CliValueType.DereferenceFieldById referenceField span
                    |> CliType.unwrapPrimitiveLikeDeep
                with
                | CliType.RuntimePointer (CliRuntimePointer.Managed src) -> EvalStackValue.ManagedPointer src
                | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer src)) ->
                    EvalStackValue.ManagedPointer src
                | other -> failwith $"Span`1.Clear expected _reference to be a managed byref, got %O{other}"

            let zero, state =
                IlMachineState.cliTypeZeroOfHandle state baseClassTypes elementType

            let state =
                (state, seq { 0 .. length - 1 })
                ||> Seq.fold (fun state i ->
                    let ptr, state =
                        offsetManagedPointerByElements baseClassTypes state elementType (int64<int> i) reference

                    let byrefSrc =
                        match ptr with
                        | EvalStackValue.ManagedPointer src -> src
                        | other ->
                            failwith $"Span`1.Clear: offsetManagedPointerByElements returned non-byref %O{other}"

                    IlMachineState.writeManagedByrefWithBase baseClassTypes state byrefSrc zero
                )

            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> IntrinsicResult.Completed
        | "System.Private.CoreLib", "RuntimeHelpers", "CreateSpan" ->
            // https://github.com/dotnet/runtime/blob/9e5e6aa7bc36aeb2a154709a9d1192030c30a2ef/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/RuntimeHelpers.cs#L153
            IntrinsicResult.Unrecognised
        | "System.Private.CoreLib", "MemoryMarshal", "GetArrayDataReference" ->
            // Two `[Intrinsic]` overloads, distinguished by arity of `Generics`:
            //
            //   ref T    GetArrayDataReference<T>(T[] array)   MemoryMarshal.CoreCLR.cs#L20
            //   ref byte GetArrayDataReference(Array array)    MemoryMarshal.CoreCLR.cs#L38
            //
            // Both denote element 0's storage; they differ only in the stride the returned
            // byref carries for subsequent pointer arithmetic. The generic form yields a
            // `ref T`, i.e. element stride, which is a plain `ArrayElement` byref. The
            // non-generic form yields a `ref byte`, so it gets an explicit byte-stride anchor.
            //
            // Deliberately *not* `anchorByteViewIfPlainArrayByref` (the `Conv_U`/`Conv_I`
            // helper): that one preserves the element's own CLI shape as the reinterpret
            // target, and silently returns its input unchanged for element handles it declines
            // to anchor — pointer, byref and function-pointer elements. A caller transporting
            // a `ref T` can live with that, but here the byref's declared pointee is `byte`,
            // so an unanchored result would carry element stride under a `ref byte` static
            // type and make legal arithmetic like `Unsafe.Add(ref pStart, 1)` fail on an
            // `int*[]`. `anchorByteStrideOverArrayData` is total over element handles.
            //
            // The non-generic body is `ref Unsafe.AddByteOffset(ref Unsafe.As<RawData>(array).Data,
            // pMT->BaseSize - 2 * sizeof(IntPtr))`: raw arithmetic over the object header layout,
            // which PawPrint does not model as bytes. `Array.Clear(Array)` is its main caller.
            let generic =
                match methodToCall.Generics |> Seq.toList with
                | [] -> None
                | [ generic ] -> Some generic
                | generics ->
                    failwith
                        $"bad generic arity for MemoryMarshal.GetArrayDataReference: %d{generics.Length} generic arguments"

            match generic with
            | Some generic ->
                match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
                | [ ConcreteGenericArray state.ConcreteTypes generic ], MethodReturnType.Returns (ConcreteByref t) when
                    t = generic
                    ->
                    ()
                | _ -> failwith $"bad signature MemoryMarshal.GetArrayDataReference<T>: %A{methodToCall.Signature}"
            | None ->
                match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
                | [ ConcreteSystemArray state.ConcreteTypes ],
                  MethodReturnType.Returns (ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)) ->
                    ()
                | _ -> failwith $"bad signature MemoryMarshal.GetArrayDataReference: %A{methodToCall.Signature}"

            let arr, state = IlMachineState.popEvalStack currentThread state

            match arr with
            | EvalStackValue.Int32 _
            | EvalStackValue.Int64 _
            | EvalStackValue.Float _ -> failwith "expected reference"
            | EvalStackValue.NativeInt nativeIntSource -> failwith "todo"
            | EvalStackValue.ObjectRef addr ->
                if not (ManagedHeap.isArray addr state.ManagedHeap) then
                    failwith "array not found"

                let toPush =
                    let element = ManagedPointerSource.Byref (ByrefRoot.ArrayElement (addr, 0), [])

                    match generic with
                    | Some _ -> element
                    | None -> ManagedPointerByteView.anchorByteStrideOverArrayData baseClassTypes state element
                    |> EvalStackValue.ManagedPointer

                state
                |> IlMachineState.pushToEvalStack' toPush currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> IntrinsicResult.Completed
            | EvalStackValue.NullObjectRef
            | EvalStackValue.ManagedPointer ManagedPointerSource.Null ->
                // The null case is a documented `NullReferenceException`
                // (`<exception cref="NullReferenceException">` on the method), and the JIT emits
                // an explicit `gtNewNullCheck` for it rather than relying on the load faulting,
                // so it is guaranteed rather than incidental. The parameterless ctor's message is
                // the one the runtime produces.
                IntrinsicResult.RaiseException (state, baseClassTypes.NullReferenceException, None)
            | EvalStackValue.UserDefinedValueType evalStackValueUserType -> failwith "todo"
            | EvalStackValue.ManagedPointer _ -> failwith "todo"
        | "System.Private.CoreLib", "Array", "Clone" ->
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Array.cs#L1071-L1077
            // The managed body is `return MemberwiseClone();`, and CoreCLR's MemberwiseClone
            // (Object.CoreCLR.cs) allocates an uninitialised clone via the
            // RuntimeHelpers.AllocateUninitializedClone QCall and then raw-byte-copies the
            // object payload. PawPrint stores array elements as `CliType` cells rather than
            // bytes, so the byte-copy formulation is not the primitive available to us here:
            // reproducing it would have to flatten every element to bytes and would lose the
            // provenance of non-`Verbatim` cells (the same reason `SpanHelpers.Memmove` is
            // intercepted rather than executed). The host-provided primitive on our side of
            // the boundary is "allocate a same-shaped array holding the same element cells",
            // which `IlMachineState.cloneArray` performs directly.
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [],
              MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                          "System",
                                                                          "Object",
                                                                          generics)) when generics.IsEmpty -> ()
            | _ -> failwith "bad signature Array.Clone"

            let receiver, state = IlMachineState.popEvalStack currentThread state

            match receiver with
            | EvalStackValue.ObjectRef addr ->
                let clone, state = IlMachineState.cloneArray addr state

                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.ObjectRef clone) currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> IntrinsicResult.Completed
            | EvalStackValue.NullObjectRef
            | EvalStackValue.ManagedPointer ManagedPointerSource.Null ->
                // `Array.Clone`'s body is `return MemberwiseClone()`, whose first act is to touch
                // `this` (`ref byte src = ref this.GetRawData()`), so a null receiver faults into
                // `NullReferenceException`.
                //
                // Unreachable from any C#-emitted call site: `Array.Clone` is an instance method,
                // so callvirt's own null check (UnaryMetadataCallOps.executeCallvirt) raises the
                // NullReferenceException before we get here — `NullReceiverGuards.cs` pins that,
                // for the direct and the `ICloneable` form. Only hand-written IL using a
                // non-virtual `call` could reach this arm.
                IntrinsicResult.RaiseException (state, baseClassTypes.NullReferenceException, None)
            | other -> failwith $"Array.Clone: expected an object reference receiver, got %O{other}"
        | "System.Private.CoreLib", "Array", (("GetLength" | "GetLowerBound" | "GetUpperBound") as boundKind) ->
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Array.cs#L763-L806
            // All three are `[Intrinsic]` and every upstream body bottoms out in
            // `this.GetMultiDimensionalArrayBounds()`, a raw walk over the inline bounds block
            // CoreCLR lays out between the object header and the element data. PawPrint does not
            // model that block at all (see the explicit failure in RawArrayDataProjection's
            // sibling, `RuntimeFieldProjection`), so — as with `Array.Clone` above — the byte-level
            // formulation is not the primitive available on our side of the boundary. The shape is
            // held structurally instead, on `AllocatedArray.Lengths`.
            //
            // Upstream's `rank` comes from the MethodTable's multi-dim rank field, which is 0 for a
            // szarray, hence its `rank == 0 && dimension == 0` special case. PawPrint stores a
            // szarray as `Lengths = [| totalLength |]`, so indexing `Lengths` uniformly reproduces
            // both the szarray and the multi-dim answers with no special case. Upstream's bound
            // check is the unsigned `(uint)dimension >= (uint)rank`; the signed pair below is
            // equivalent over the whole of Int32, including Int32.MinValue.
            //
            // `GetLongLength` is not itself `[Intrinsic]` — it just widens `GetLength`, so it starts
            // working via ordinary interpretation once this arm exists.
            match methodToCall.Signature.ParameterTypes, methodToCall.Signature.ReturnType with
            | [ ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
              MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) -> ()
            | _ -> failwith $"bad signature Array.%s{boundKind}"

            // Instance method with one argument: the receiver sits below the argument.
            let dimensionArg, state = IlMachineState.popEvalStack currentThread state
            let receiver, state = IlMachineState.popEvalStack currentThread state

            let dimension =
                match dimensionArg with
                | EvalStackValue.Int32 (Int32Source.Verbatim d) -> d
                | other -> failwith $"Array.%s{boundKind}: expected an Int32 dimension, got %O{other}"

            match receiver with
            | EvalStackValue.ObjectRef addr ->
                let arr =
                    match ManagedHeap.tryGetArrayShape addr state.ManagedHeap with
                    | Some arr -> arr
                    | None -> failwith $"Array.%s{boundKind}: no array allocated at %O{addr}"

                // The stored shape and the receiver's concrete type must agree on rank. Both are
                // written together by `allocateArray` / `allocateMultiDimArray`, so this cannot
                // currently fire; it is here to catch representation drift rather than to handle a
                // reachable case.
                let declaredRank =
                    match arr.ConcreteType with
                    | ConcreteTypeHandle.OneDimArrayZero _ -> 1
                    | ConcreteTypeHandle.Array (_, rank) -> rank
                    | other -> failwith $"Array.%s{boundKind}: array at %O{addr} has non-array concrete type %O{other}"

                if arr.Lengths.Length <> declaredRank then
                    failwith
                        $"Array.%s{boundKind}: array at %O{addr} has concrete-type rank %d{declaredRank} but %d{arr.Lengths.Length} stored dimension length(s)"

                if dimension < 0 || dimension >= arr.Lengths.Length then
                    // Both arguments are already popped, so the eval stack is clean for dispatch,
                    // and the PC has deliberately not been advanced.
                    //
                    // The message is the string the CLR would have passed
                    // (`SR.IndexOutOfRange_ArrayRankIndex`); the parameterless ctor's default
                    // ("Index was outside the bounds of the array.") is the wrong one here.
                    IntrinsicResult.RaiseException (
                        state,
                        baseClassTypes.IndexOutOfRangeException,
                        Some "Array does not have that many dimensions."
                    )
                else

                let result =
                    match boundKind with
                    | "GetLength" -> arr.Lengths.[dimension]
                    // PawPrint has no representation for a non-zero lower bound:
                    // `allocateMultiDimArray` documents that only the zero-lower-bound constructor
                    // form is modelled, and no guest-reachable path produces anything else. Revisit
                    // this arm if `Array.CreateInstance(Type, int[], int[])` is ever implemented.
                    | "GetLowerBound" -> 0
                    | "GetUpperBound" -> arr.Lengths.[dimension] - 1
                    | other -> failwith $"logic error: unreachable Array bound accessor %s{other}"

                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim result)) currentThread
                |> IlMachineState.advanceProgramCounter currentThread
                |> IntrinsicResult.Completed
            | EvalStackValue.NullObjectRef
            | EvalStackValue.ManagedPointer ManagedPointerSource.Null ->
                // Unreachable from C#-emitted code: these are instance methods, so callvirt's own
                // null check (UnaryMetadataCallOps.executeCallvirt) raises the NullReferenceException
                // first. See the matching note on the Array.Clone arm above.
                IntrinsicResult.RaiseException (state, baseClassTypes.NullReferenceException, None)
            | other -> failwith $"Array.%s{boundKind}: expected an object reference receiver, got %O{other}"
        | "System.Private.CoreLib", "Enum", "HasFlag" ->
            // https://github.com/dotnet/runtime/blob/dbd3e33df9ccf74b91045e095477726c2bf83916/src/libraries/System.Private.CoreLib/src/System/Enum.cs#L398
            // Enum.HasFlag(Enum flag) returns (thisValue & flagValue) == flagValue
            // The arguments are boxed enums (ObjectRef) since the method signature takes System.Enum.
            //
            // Peek first to check type compatibility. If types mismatch, raise ArgumentException
            // directly before consuming the boxed enum values for the raw bitwise comparison below.
            let evalStack = state.ThreadState.[currentThread].MethodState.EvaluationStack
            let flagPeek = EvalStack.PeekNthFromTop 0 evalStack
            let thisPeek = EvalStack.PeekNthFromTop 1 evalStack

            match thisPeek, flagPeek with
            | Some (EvalStackValue.ObjectRef thisAddr), Some (EvalStackValue.ObjectRef flagAddr) ->
                let thisObj = ManagedHeap.get thisAddr state.ManagedHeap
                let flagObj = ManagedHeap.get flagAddr state.ManagedHeap

                if thisObj.ConcreteType <> flagObj.ConcreteType then
                    // Type mismatch: raise ArgumentException (Enum.cs:403-404).
                    // We must pop the two args before raising, so the eval stack is clean.
                    let _, state = IlMachineState.popEvalStack currentThread state
                    let _, state = IlMachineState.popEvalStack currentThread state

                    // No message: the CLR's is `SR.Argument_EnumTypeDoesNotMatch` formatted with
                    // `flag.GetType()` and `GetType()`, and rendering those two the way
                    // `Type.ToString()` would (nested types are `Outer+Inner`, generics are
                    // backtick-arity) is a fidelity question of its own. A half-right string would
                    // be worse than the parameterless ctor's honest default, which is already an
                    // improvement on the null `_message` this arm used to leave behind.
                    IntrinsicResult.RaiseException (state, baseClassTypes.ArgumentException, None)
                else
                    let flag, state = IlMachineState.popEvalStack currentThread state
                    let thisVal, state = IlMachineState.popEvalStack currentThread state

                    let numericToInt64 (n : CliNumericType) : int64 =
                        match n with
                        | CliNumericType.Int32 i -> int64 i
                        | CliNumericType.Int64 (Int64Source.Verbatim i) -> i
                        | CliNumericType.Int8 i -> int64 i
                        | CliNumericType.UInt8 i -> int64 i
                        | CliNumericType.Int16 i -> int64 i
                        | CliNumericType.UInt16 i -> int64 i
                        | other -> failwith $"Enum.HasFlag: unexpected underlying numeric type %O{other}"

                    let extractInt (contents : CliValueType) : int64 =
                        match (CliValueType.PrimitiveLikeField contents).Contents with
                        | CliType.Numeric n -> numericToInt64 n
                        | other -> failwith $"Enum.HasFlag: unexpected underlying type %O{other}"

                    let thisInt = extractInt thisObj.Contents
                    let flagInt = extractInt flagObj.Contents
                    let result = (thisInt &&& flagInt) = flagInt

                    state
                    |> IlMachineState.pushToEvalStack'
                        (EvalStackValue.Int32 (Int32Source.Verbatim (if result then 1 else 0)))
                        currentThread
                    |> IlMachineState.advanceProgramCounter currentThread
                    |> IntrinsicResult.Completed
            | Some _, Some EvalStackValue.NullObjectRef ->
                // Null flag: `ArgumentNullException.ThrowIfNull(flag)` (Enum.cs:401), which runs
                // before the type-equivalence check above.
                let _, state = IlMachineState.popEvalStack currentThread state
                let _, state = IlMachineState.popEvalStack currentThread state

                // No message: the CLR's `.Message` here is "Value cannot be null. (Parameter
                // 'flag')", but that suffix is synthesised by `ArgumentException.Message` from
                // `_paramName`, which this channel cannot set. Writing the suffix into `_message`
                // would make `.Message` right while `.ParamName` stayed null — two fields
                // disagreeing about whether a parameter name is known. The parameterless ctor's
                // "Value cannot be null." is at least self-consistent.
                IntrinsicResult.RaiseException (state, baseClassTypes.ArgumentNullException, None)
            | _ -> failwith $"Enum.HasFlag: expected two ObjectRefs on eval stack"
        | _ -> IntrinsicResult.Unrecognised
