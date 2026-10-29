namespace WoofWare.PawPrint.Analysis

open WoofWare.PawPrint

/// How CoreCLR makes an exception that it raises by itself, rather than one the code throws, which
/// decides what raising it runs.
[<RequireQualifiedAccess>]
type internal ExceptionMaking =
    /// It raises an object it allocated in advance, and runs no managed code.
    | Preallocated
    /// IL makes the exception with `newobj` of its parameterless constructor and throws it: a
    /// CoreLib helper the JIT calls (`ThrowHelpers`, `CastHelpers`), or an IL stub the runtime
    /// generates. The constructor looks up its message, and what it raises escapes in place of the
    /// exception, on every runtime.
    | ThrownByIl
    /// The VM allocates the exception and runs its parameterless constructor from native code
    /// (`EEException::CreateThrowable`), which looks up its message. What that constructor raises
    /// does as `vmMakingRaise` says.
    | VmConstructor
    /// The VM allocates the exception, runs its parameterless constructor, and then looks up its
    /// message by a resource name (`EEResourceException`), all from native code. What either of
    /// those raises does as `vmMakingRaise` says.
    | ResourceMessage
    /// The VM allocates a `TypeLoadException` and runs its `(string, string, string, int)`
    /// constructor on the type's name, its assembly's name, an argument for the message, and the
    /// number of the runtime's resource saying why the load failed
    /// (`EETypeLoadException::CreateThrowable`). That constructor formats its message from the
    /// runtime's native resources. What it raises does as `vmMakingRaise` says.
    | TypeLoadConstructor
    /// It runs `TypeInitializationException`'s `(string, Exception)` constructor around what a type
    /// initializer raised, and should that constructor fail, raises what the initializer raised
    /// instead, throwing it again: that runs `Exception.InternalPreserveStackTrace` on it, whose
    /// own raises escape in its place. What the constructor raises never escapes.
    | InitializerFailure

/// What follows where a method the VM runs from native code to make an exception raises
/// (`ExceptionMaking.VmConstructor`, `ResourceMessage` and `TypeLoadConstructor`).
[<RequireQualifiedAccess>]
type internal VmMakingRaise =
    /// What the method raised escapes in place of the exception being made.
    | EscapesInPlace
    /// The VM throws what the method raised again (`COMPlusThrow`), which first runs
    /// `Exception.InternalPreserveStackTrace` on it, and throws what that raises again the same
    /// way: what either raises escapes in place of the exception being made. A failure that recurs
    /// on every attempt recurs until the stack overflows, which ends the process.
    | ThrownAgainPreservingTrace

[<RequireQualifiedAccess>]
module internal ExceptionMaking =

    /// What follows on `runtime` where a method the VM runs from native code to make an exception
    /// raises. Where the CPU reported the fault the exception is for, the process ends on every
    /// runtime instead, which each answer over-approximates.
    let vmMakingRaise (runtime : EmulatedRuntime) : VmMakingRaise =
        match runtime with
        | EmulatedRuntime.Net10 -> VmMakingRaise.EscapesInPlace
        // The VM calls each method through an `[UnmanagedCallersOnly]` wrapper
        // (`UnmanagedCallersOnlyCaller::InvokeThrowing`), which hands back what it raised for
        // `RealCOMPlusThrow` to throw again, after `ExceptionPreserveStackTrace`, itself such a call.
        | EmulatedRuntime.Net11 -> VmMakingRaise.ThrownAgainPreservingTrace

    /// How CoreCLR makes the exception of a fault `op` raises by itself.
    let ofOpcodeFault (op : IlOp) (fault : OpcodeFault) : ExceptionMaking =
        match fault with
        // The JIT throws these by calling CoreLib's helpers (`ThrowHelpers`, and `CastHelpers`'
        // `StelemRef`), each of which does `throw new`. A null reference, a zero divisor or a
        // quotient's overflow the CPU may report instead, and the VM then makes the exception, where
        // a raising constructor ends the process; the helper's making covers that.
        | OpcodeFault.NullReference
        | OpcodeFault.IndexOutOfRange
        | OpcodeFault.ArrayTypeMismatch
        | OpcodeFault.DivideByZero -> ExceptionMaking.ThrownByIl
        | OpcodeFault.Overflow ->
            match op with
            // The allocation helper raises it for a negative length (`COMPlusThrow`).
            | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Newarr, _) -> ExceptionMaking.VmConstructor
            | _ -> ExceptionMaking.ThrownByIl
        // `CastHelpers` raises it through the QCall `ThrowInvalidCastExceptionInternal`, whose VM
        // makes the exception.
        | OpcodeFault.InvalidCast -> ExceptionMaking.VmConstructor
        | OpcodeFault.OutOfMemory ->
            match op with
            // `AllocateSzArray` makes one for a length too large to allocate
            // (`ThrowOutOfMemoryDimensionsExceeded`), as well as raising the preallocated one when
            // the heap is exhausted; running the constructor runs all that either does.
            | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Newarr, _) -> ExceptionMaking.VmConstructor
            // `CLRException::GetBestOutOfMemoryException`.
            | _ -> ExceptionMaking.Preallocated
        // `GetPreallocatedStackOverflowException`.
        | OpcodeFault.StackOverflow -> ExceptionMaking.Preallocated
        // `CreateTypeInitializationExceptionObject`.
        | OpcodeFault.TypeInitialization -> ExceptionMaking.InitializerFailure

    /// The exceptions CoreCLR raises by itself, by full name, from a method it supplies on
    /// `arrayType`, and how it makes each.
    let ofArrayMethod (arrayType : TypeDefn) (accessor : ArrayAccessor) : (string * ExceptionMaking) list =
        let lowerBounds =
            match arrayType, accessor with
            | TypeDefn.Array (_, rank), ArrayAccessor.Constructor arity -> arity = 2 * rank
            | _ -> false

        match accessor with
        // Those of the instructions they stand in for, `ldelem`, `stelem.ref` and `ldelema`. The
        // runtime's IL stub (`ArrayOpLinker`, array.cpp) makes the latter two with `newobj` and
        // throws them; a null reference is the JIT's, as for `ofOpcodeFault`.
        | ArrayAccessor.Get ->
            [
                "System.NullReferenceException", ExceptionMaking.ThrownByIl
                "System.IndexOutOfRangeException", ExceptionMaking.ThrownByIl
            ]
        | ArrayAccessor.Set
        | ArrayAccessor.Address ->
            [
                "System.NullReferenceException", ExceptionMaking.ThrownByIl
                "System.IndexOutOfRangeException", ExceptionMaking.ThrownByIl
                "System.ArrayTypeMismatchException", ExceptionMaking.ThrownByIl
            ]
        // The JIT calls `Array.CreateInstanceMDArray`, whose QCall's `AllocateArrayEx` raises
        // `OverflowException` for a negative length; `OutOfMemoryException` for lengths too large to
        // allocate (`ThrowOutOfMemoryDimensionsExceeded`), or the preallocated one when the heap is
        // exhausted; and, given lower bounds, `ArgumentOutOfRangeException` for a dimension whose
        // upper end overflows, by `COMPlusThrow` with a resource name.
        | ArrayAccessor.Constructor _ ->
            [
                "System.OverflowException", ExceptionMaking.VmConstructor
                "System.OutOfMemoryException", ExceptionMaking.VmConstructor
                if lowerBounds then
                    "System.ArgumentOutOfRangeException", ExceptionMaking.ResourceMessage
            ]

    /// The exception CoreCLR raises refusing a signature it converts for the JIT, and how it makes
    /// it: `COMPlusThrow` with one of the runtime's own resources (`EEMessageException`), which
    /// runs the exception's parameterless constructor and then sets the message it reads from the
    /// runtime's native resources, running no more managed code.
    let ofSignatureRefusal (refusal : SignatureRefusal) : string * ExceptionMaking =
        match refusal with
        | SignatureRefusal.VariableArguments -> "System.InvalidProgramException", ExceptionMaking.VmConstructor

    /// The exception CoreCLR raises refusing the unmanaged calling convention of a call whose
    /// signature's modifiers name two base conventions (`UnmanagedConventionRead.MultipleBases`),
    /// and how it makes it: `COMPlusThrowHR` with `COR_E_INVALIDPROGRAM` and one of the runtime's
    /// own resources (`EEMessageException`), as for `ofSignatureRefusal`.
    let multipleCallingConventions : string * ExceptionMaking =
        "System.InvalidProgramException", ExceptionMaking.VmConstructor

    /// The exception CoreCLR's JIT raises refusing a call of unmanaged code by Thiscall's
    /// convention that passes no argument
    /// (`UnmanagedCallingConvention.refusesThiscallWithoutArguments`), and how it makes it: the JIT
    /// fails the compilation (`CORJIT_BADCODE`), and the VM raises `InvalidProgramException` by
    /// `COMPlusThrow` with no resource (`EEException`), which runs the exception's parameterless
    /// constructor and then sets a message it reads from the runtime's native resources.
    let thiscallWithoutArguments : string * ExceptionMaking =
        "System.InvalidProgramException", ExceptionMaking.VmConstructor

    /// The exception CoreCLR raises refusing a P/Invoke as it describes it for the walk of its
    /// signature (`JitTypeLoads.unmanagedWalkRefuses`): it is synchronized, or its calling
    /// conventions conflict. And how it makes it: `COMPlusThrow` with one of the runtime's own
    /// resources (`EEMessageException`), which runs the exception's parameterless constructor and
    /// then sets the message it reads from the runtime's native resources.
    let pInvokeDescriptionRefused : string * ExceptionMaking =
        "System.TypeLoadException", ExceptionMaking.VmConstructor

    /// The exception CoreCLR raises refusing to make a delegate over an `[UnmanagedCallersOnly]`
    /// method, and how it makes it: `COMDelegate::GetDelegateCtor` throws `NotSupportedException`
    /// by `COMPlusThrow` with the resource name `NotSupported_UnmanagedCallersOnlyTarget`.
    let unmanagedCallersOnlyDelegateTarget : string * ExceptionMaking =
        "System.NotSupportedException", ExceptionMaking.ResourceMessage

    /// The exception CoreCLR raises refusing to compile an invalid `[UnmanagedCallersOnly]` method, and
    /// how it makes it: `COMDelegate::ThrowIfInvalidUnmanagedCallersOnlyUsage` throws
    /// `InvalidProgramException` as an `EEResourceException` naming the resource that says why.
    let unmanagedCallersOnlyInvalid : string * ExceptionMaking =
        "System.InvalidProgramException", ExceptionMaking.ResourceMessage

    /// How CoreCLR makes the exception of a fault one of its own operations raises: the CPU reports
    /// each, and the VM makes the exception (`EEException::CreateThrowable`).
    let ofPrimitiveFault (fault : PrimitiveFault) : ExceptionMaking =
        match fault with
        | PrimitiveFault.NullReference
        | PrimitiveFault.DataMisaligned -> ExceptionMaking.VmConstructor

    /// How CoreCLR makes the exception of a fault a hardware instruction raises, or `None` for an
    /// out-of-range immediate, for which the JIT calls a helper in CoreLib whose IL makes it.
    let ofInstructionFault (fault : InstructionFault) : ExceptionMaking option =
        match fault with
        // The CPU reports each, and the VM makes the exception (`EEException::CreateThrowable`).
        | InstructionFault.NullAddress
        | InstructionFault.ZeroDivisor
        | InstructionFault.QuotientOverflow -> Some ExceptionMaking.VmConstructor
        | InstructionFault.ImmediateOutOfRange -> None
