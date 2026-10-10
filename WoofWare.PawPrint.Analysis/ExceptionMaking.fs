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
    /// It runs `TypeInitializationException`'s `(string, Exception)` constructor around what a type
    /// initializer raised, and should that constructor fail, raises what the initializer raised
    /// instead, throwing it again: that runs `Exception.InternalPreserveStackTrace` on it, whose
    /// own raises escape in its place. What the constructor raises never escapes.
    | InitializerFailure

[<RequireQualifiedAccess>]
module internal ExceptionMaking =

    /// How CoreCLR makes the exception of a fault an instruction raises by itself.
    let ofOpcodeFault (fault : OpcodeFault) : ExceptionMaking =
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
        // `CLRException::GetBestOutOfMemoryException` and `GetPreallocatedStackOverflowException`.
        | OpcodeFault.OutOfMemory
        | OpcodeFault.StackOverflow -> ExceptionMaking.Preallocated
        // `CreateTypeInitializationExceptionObject`.
        | OpcodeFault.TypeInitialization -> ExceptionMaking.InitializerFailure

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
