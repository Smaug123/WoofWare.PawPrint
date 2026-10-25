namespace WoofWare.PawPrint.Analysis

open WoofWare.PawPrint

/// How CoreCLR makes an exception that it raises by itself, rather than one the code throws, which
/// decides what raising it runs.
[<RequireQualifiedAccess>]
type internal ExceptionMaking =
    /// It raises an object it allocated in advance, and runs no managed code.
    | Preallocated
    /// It allocates the exception and runs its parameterless constructor, which looks up its
    /// message, so what that constructor raises can escape in place of the exception. (Where the
    /// CPU reported the fault, a constructor that raises ends the process instead.)
    | ParameterlessConstructor
    /// It allocates the exception, runs its parameterless constructor, and then looks up its
    /// message by a resource name through `Environment.GetResourceStringLocal`
    /// (`EEResourceException`), so what either of those raises can escape in place of the
    /// exception.
    | ResourceMessage
    /// It allocates a `TypeLoadException` and runs its `(string, string, string, int)` constructor
    /// on the type's name, its assembly's name, an argument for the message, and the number of the
    /// runtime's resource saying why the load failed (`EETypeLoadException::CreateThrowable`). That
    /// constructor formats its message from the runtime's native resources, so what it raises can
    /// escape in place of the exception.
    | TypeLoadConstructor
    /// It runs `TypeInitializationException`'s `(string, Exception)` constructor around what a type
    /// initializer raised, and should that constructor fail, raises what the initializer raised
    /// instead, throwing it again: that runs `Exception.InternalPreserveStackTrace` on it, whose
    /// own raises escape in its place. What the constructor raises never escapes.
    | InitializerFailure

[<RequireQualifiedAccess>]
module internal ExceptionMaking =

    /// How CoreCLR makes the exception of a fault `op` raises by itself.
    let ofOpcodeFault (op : IlOp) (fault : OpcodeFault) : ExceptionMaking =
        match fault with
        // The JIT throws these by calling CoreLib's helpers (`ThrowHelpers`, `CastHelpers`), each
        // of which does `throw new`, or the CPU reports them and the VM makes the exception
        // (`EEException::CreateThrowable`), which also runs the parameterless constructor.
        | OpcodeFault.NullReference
        | OpcodeFault.IndexOutOfRange
        | OpcodeFault.ArrayTypeMismatch
        | OpcodeFault.InvalidCast
        | OpcodeFault.Overflow
        | OpcodeFault.DivideByZero -> ExceptionMaking.ParameterlessConstructor
        | OpcodeFault.OutOfMemory ->
            match op with
            // `AllocateSzArray` makes one for a length too large to allocate
            // (`ThrowOutOfMemoryDimensionsExceeded`), as well as raising the preallocated one when
            // the heap is exhausted; running the constructor runs all that either does.
            | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Newarr, _) -> ExceptionMaking.ParameterlessConstructor
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
        // Those of the instructions they stand in for, `ldelem`, `stelem.ref` and `ldelema`, made as
        // `ofOpcodeFault` says those instructions' are.
        | ArrayAccessor.Get ->
            [
                "System.NullReferenceException", ExceptionMaking.ParameterlessConstructor
                "System.IndexOutOfRangeException", ExceptionMaking.ParameterlessConstructor
            ]
        | ArrayAccessor.Set
        | ArrayAccessor.Address ->
            [
                "System.NullReferenceException", ExceptionMaking.ParameterlessConstructor
                "System.IndexOutOfRangeException", ExceptionMaking.ParameterlessConstructor
                "System.ArrayTypeMismatchException", ExceptionMaking.ParameterlessConstructor
            ]
        // The JIT calls `Array.CreateInstanceMDArray`, whose QCall's `AllocateArrayEx` raises
        // `OverflowException` for a negative length; `OutOfMemoryException` for lengths too large to
        // allocate (`ThrowOutOfMemoryDimensionsExceeded`), or the preallocated one when the heap is
        // exhausted; and, given lower bounds, `ArgumentOutOfRangeException` for a dimension whose
        // upper end overflows, by `COMPlusThrow` with a resource name.
        | ArrayAccessor.Constructor _ ->
            [
                "System.OverflowException", ExceptionMaking.ParameterlessConstructor
                "System.OutOfMemoryException", ExceptionMaking.ParameterlessConstructor
                if lowerBounds then
                    "System.ArgumentOutOfRangeException", ExceptionMaking.ResourceMessage
            ]

    /// The exception CoreCLR raises refusing a signature it converts for the JIT, and how it makes
    /// it: `COMPlusThrow` with one of the runtime's own resources (`EEMessageException`), which
    /// runs the exception's parameterless constructor and then sets the message it reads from the
    /// runtime's native resources, running no more managed code.
    let ofSignatureRefusal (refusal : SignatureRefusal) : string * ExceptionMaking =
        match refusal with
        | SignatureRefusal.VariableArguments ->
            "System.InvalidProgramException", ExceptionMaking.ParameterlessConstructor

    /// The exception CoreCLR raises refusing the unmanaged calling convention of a call whose
    /// signature's modifiers name two base conventions (`UnmanagedConventionRead.MultipleBases`),
    /// and how it makes it: `COMPlusThrowHR` with `COR_E_INVALIDPROGRAM` and one of the runtime's
    /// own resources (`EEMessageException`), as for `ofSignatureRefusal`.
    let multipleCallingConventions : string * ExceptionMaking =
        "System.InvalidProgramException", ExceptionMaking.ParameterlessConstructor

    /// The exception CoreCLR raises refusing the calling conventions of a P/Invoke as it describes
    /// it for the walk of its signature (`PInvokeMarshalling.RefusesConventions`), and how it makes
    /// it: `COMPlusThrow` with one of the runtime's own resources (`EEMessageException`), which runs
    /// the exception's parameterless constructor and then sets the message it reads from the
    /// runtime's native resources.
    let pInvokeConventionsRefused : string * ExceptionMaking =
        "System.TypeLoadException", ExceptionMaking.ParameterlessConstructor

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
        | PrimitiveFault.DataMisaligned -> ExceptionMaking.ParameterlessConstructor

    /// How CoreCLR makes the exception of a fault a hardware instruction raises, or `None` for an
    /// out-of-range immediate, for which the JIT calls a helper in CoreLib whose IL makes it.
    let ofInstructionFault (fault : InstructionFault) : ExceptionMaking option =
        match fault with
        // The CPU reports each, and the VM makes the exception (`EEException::CreateThrowable`).
        | InstructionFault.NullAddress
        | InstructionFault.ZeroDivisor
        | InstructionFault.QuotientOverflow -> Some ExceptionMaking.ParameterlessConstructor
        | InstructionFault.ImmediateOutOfRange -> None
