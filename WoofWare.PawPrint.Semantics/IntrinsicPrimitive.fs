namespace WoofWare.PawPrint

open System.Reflection.Metadata

/// The operand of an atomic read-modify-write, by the CoreLib overload that takes it.
[<RequireQualifiedAccess>]
type AtomicOperand =
    | UInt8
    | UInt16
    | Int32
    | Int64

/// The operand of an atomic addition: CoreLib has one for these two widths only.
[<RequireQualifiedAccess>]
type AtomicAddOperand =
    | Int32
    | Int64

/// An operation CoreCLR's runtime performs in code of its own when a CoreLib intrinsic is called:
/// something no IL body expresses, on any CPU or on the CPU a run describes.
///
/// The cases are operations, not methods. Which CoreLib method is which operation is read from the
/// image (`IntrinsicPrimitive.recognise`), so a CoreLib of another runtime version is classified
/// from its own methods.
[<RequireQualifiedAccess>]
type IntrinsicPrimitive =
    /// `Interlocked.MemoryBarrier()`: a full fence.
    | FullBarrier
    /// `Volatile.ReadBarrier()`: no read after it is performed before a read before it.
    | ReadBarrier
    /// `Volatile.WriteBarrier()`: no write before it is performed after a write after it.
    | WriteBarrier
    /// `Thread.FastPollGC()`: a point at which the garbage collector may suspend the thread.
    | GcPoll
    /// `Interlocked.CompareExchange(ref x, value, comparand)`, atomically.
    | AtomicCompareExchange of AtomicOperand
    /// `Interlocked.Exchange(ref x, value)`, atomically.
    | AtomicExchange of AtomicOperand
    /// `Interlocked.ExchangeAdd(ref x, value)`, atomically, returning the old value.
    | AtomicAdd of AtomicAddOperand
    /// `RuntimeHelpers.GetMethodTable(object)`: the object's type pointer.
    | MethodTableOf
    /// `MemoryMarshal.GetArrayDataReference<T>(T[])`: a byref to element 0, without a bounds
    /// check, so it is defined on an empty array.
    | ArrayDataReference
    /// `StaticsHelpers.VolatileReadAsByref(ref nint)`: the pointer stored at the address, read
    /// with acquire semantics, as a byref.
    | VolatileReadByref
    /// `RuntimeHelpers.IsReferenceOrContainsReferences<T>()`: a fact about `T`.
    | IsReferenceOrContainsReferences
    /// `RuntimeHelpers.IsBitwiseEquatable<T>()`: a fact about `T`, which CoreCLR's VM decides
    /// (`getILIntrinsicImplementationForRuntimeHelpers`, jitinterface.cpp) rather than its JIT.
    | IsBitwiseEquatable
    /// `Math.ReciprocalEstimate` and `MathF.ReciprocalEstimate`: an approximation of `1 / x`
    /// whose precision is the CPU's.
    | ReciprocalEstimate of FloatWidth
    /// `Math.ReciprocalSqrtEstimate` and `MathF.ReciprocalSqrtEstimate`: an approximation of
    /// `1 / sqrt x` whose precision is the CPU's.
    | ReciprocalSqrtEstimate of FloatWidth
    /// `double.MultiplyAddEstimate` and `float.MultiplyAddEstimate`: `a * b + c`, fused or not
    /// as the CPU does it.
    | MultiplyAddEstimate of FloatWidth
    /// `double.ConvertToIntegerNative<T>` and `float.ConvertToIntegerNative<T>` for a primitive
    /// `T`: the conversion the architecture's own instruction performs, whose result for a value
    /// `T` cannot represent is the architecture's. On x64 the JIT uses the baseline truncating
    /// conversion for a signed `T` whatever the instruction sets (`impPrimitiveNamedIntrinsic`),
    /// so NaN converts to `int.MinValue`; on Arm64 it emits an ordinary cast, which saturates.
    | ConvertToIntegerNative of FloatWidth

/// An exception an intrinsic primitive can raise.
[<RequireQualifiedAccess>]
type PrimitiveFault =
    /// `System.NullReferenceException`.
    | NullReference
    /// `System.DataMisalignedException`, which CoreCLR raises from the hardware fault of an
    /// atomic or acquire access to an address the CPU requires aligned. Arm64 requires it; with
    /// FEAT_LSE2 only for an access that crosses a 16-byte boundary, and x64 never.
    | DataMisaligned

/// A condition under which an intrinsic primitive can raise a fault. It is necessary, not
/// sufficient: a misaligned location faults only on a CPU that requires the alignment.
[<RequireQualifiedAccess>]
type FaultCondition =
    /// The static method's argument at this zero-based index is null.
    | ArgumentNull of index : int
    /// The location the static method's argument at this zero-based index addresses is not
    /// aligned to the size of the access the primitive makes there.
    | ArgumentMisaligned of index : int

/// What is known of the reference an intrinsic primitive returns.
[<RequireQualifiedAccess>]
type ResultNullness =
    /// It returns no reference or byref: `void`, a number, a Boolean.
    | NotAReference
    /// It returns a reference or byref that is never null.
    | NonNull
    /// It returns a reference or byref that may be null.
    | MaybeNull

/// What an intrinsic primitive can do to its caller, as an analyser needs it. Unrecoverable
/// failures (`StackOverflowException`, and `OutOfMemoryException` from an operation that
/// allocates nothing) are out of scope, as for `OpcodeFaults`.
type IntrinsicContract =
    {
        /// Performing the operation can raise these, each only under its condition, and nothing
        /// else. The empty list is a positive claim that the operation cannot fault.
        Raises : (PrimitiveFault * FaultCondition) list
        /// Whether the operation can complete normally. False would mean every call raises.
        CanReturn : bool
        /// What is known of its result's nullness.
        Result : ResultNullness
    }

[<RequireQualifiedAccess>]
module IntrinsicPrimitive =

    let private corelib : string = "System.Private.CoreLib"

    /// A parameter type, as far as recognising a primitive needs one.
    [<RequireQualifiedAccess>]
    type private Shape =
        | Of of PrimitiveType
        | RefOf of PrimitiveType
        | T
        | ArrayOfT
        | Other

    let rec private shapeOf (ty : TypeDefn) : Shape =
        match ty with
        | TypeDefn.Modified m -> shapeOf m.Unmodified
        | TypeDefn.PrimitiveType p -> Shape.Of p
        | TypeDefn.Byref inner ->
            match shapeOf inner with
            | Shape.Of p -> Shape.RefOf p
            | _ -> Shape.Other
        | TypeDefn.GenericMethodParameter 0 -> Shape.T
        | TypeDefn.OneDimensionalArrayLowerBoundZero (TypeDefn.GenericMethodParameter 0) -> Shape.ArrayOfT
        | _ -> Shape.Other

    let private atomicOperand (p : PrimitiveType) : AtomicOperand option =
        match p with
        | PrimitiveType.Byte -> Some AtomicOperand.UInt8
        | PrimitiveType.UInt16 -> Some AtomicOperand.UInt16
        | PrimitiveType.Int32 -> Some AtomicOperand.Int32
        | PrimitiveType.Int64 -> Some AtomicOperand.Int64
        | _ -> None

    /// The primitive `method` is, when it is a static method of `assembly`, a CoreLib, that CoreCLR
    /// implements as one. Recognition is by class, name and parameter shapes, so it holds for any
    /// CoreLib that keeps the method's signature; a method this does not recognise is `None`.
    let recognise (assembly : DumpedAssembly) (method : MethodDefinitionHandle) : IntrinsicPrimitive option =
        let definition = assembly.Methods.[method]

        let declaringType =
            assembly.TypeDefs.[definition.RequiredDeclaringType.Definition.Get]

        if
            assembly.ThisAssemblyDefinition.Name.Name <> corelib
            || declaringType.IsNested
            || not definition.IsStatic
        then
            None
        else

        let parameters = definition.Signature.ParameterTypes |> List.map shapeOf
        let generic = definition.Signature.GenericParameterCount

        match declaringType.Namespace, declaringType.Name, definition.Name, parameters, generic with
        | "System.Threading", "Interlocked", "MemoryBarrier", [], 0 -> Some IntrinsicPrimitive.FullBarrier
        | "System.Threading", "Volatile", "ReadBarrier", [], 0 -> Some IntrinsicPrimitive.ReadBarrier
        | "System.Threading", "Volatile", "WriteBarrier", [], 0 -> Some IntrinsicPrimitive.WriteBarrier
        | "System.Threading", "Thread", "FastPollGC", [], 0 -> Some IntrinsicPrimitive.GcPoll
        | "System.Threading", "Interlocked", "CompareExchange", [ Shape.RefOf a ; Shape.Of b ; Shape.Of c ], 0 when
            a = b && b = c
            ->
            atomicOperand a |> Option.map IntrinsicPrimitive.AtomicCompareExchange
        | "System.Threading", "Interlocked", "Exchange", [ Shape.RefOf a ; Shape.Of b ], 0 when a = b ->
            atomicOperand a |> Option.map IntrinsicPrimitive.AtomicExchange
        | "System.Threading", "Interlocked", "ExchangeAdd", [ Shape.RefOf a ; Shape.Of b ], 0 when a = b ->
            match atomicOperand a with
            | Some AtomicOperand.Int32 -> Some (IntrinsicPrimitive.AtomicAdd AtomicAddOperand.Int32)
            | Some AtomicOperand.Int64 -> Some (IntrinsicPrimitive.AtomicAdd AtomicAddOperand.Int64)
            | _ -> None
        | "System.Runtime.CompilerServices", "RuntimeHelpers", "GetMethodTable", [ Shape.Of PrimitiveType.Object ], 0 ->
            Some IntrinsicPrimitive.MethodTableOf
        | "System.Runtime.CompilerServices", "RuntimeHelpers", "IsReferenceOrContainsReferences", [], 1 ->
            Some IntrinsicPrimitive.IsReferenceOrContainsReferences
        | "System.Runtime.CompilerServices", "RuntimeHelpers", "IsBitwiseEquatable", [], 1 ->
            Some IntrinsicPrimitive.IsBitwiseEquatable
        | "System.Runtime.InteropServices", "MemoryMarshal", "GetArrayDataReference", [ Shape.ArrayOfT ], 1 ->
            Some IntrinsicPrimitive.ArrayDataReference
        | "System.Runtime.CompilerServices",
          "StaticsHelpers",
          "VolatileReadAsByref",
          [ Shape.RefOf PrimitiveType.IntPtr ],
          0 -> Some IntrinsicPrimitive.VolatileReadByref
        | "System", "Math", "ReciprocalEstimate", [ Shape.Of PrimitiveType.Double ], 0 ->
            Some (IntrinsicPrimitive.ReciprocalEstimate FloatWidth.Double)
        | "System", "MathF", "ReciprocalEstimate", [ Shape.Of PrimitiveType.Single ], 0 ->
            Some (IntrinsicPrimitive.ReciprocalEstimate FloatWidth.Single)
        | "System", "Math", "ReciprocalSqrtEstimate", [ Shape.Of PrimitiveType.Double ], 0 ->
            Some (IntrinsicPrimitive.ReciprocalSqrtEstimate FloatWidth.Double)
        | "System", "MathF", "ReciprocalSqrtEstimate", [ Shape.Of PrimitiveType.Single ], 0 ->
            Some (IntrinsicPrimitive.ReciprocalSqrtEstimate FloatWidth.Single)
        | "System",
          "Double",
          "MultiplyAddEstimate",
          [ Shape.Of PrimitiveType.Double ; Shape.Of PrimitiveType.Double ; Shape.Of PrimitiveType.Double ],
          0 -> Some (IntrinsicPrimitive.MultiplyAddEstimate FloatWidth.Double)
        | "System",
          "Single",
          "MultiplyAddEstimate",
          [ Shape.Of PrimitiveType.Single ; Shape.Of PrimitiveType.Single ; Shape.Of PrimitiveType.Single ],
          0 -> Some (IntrinsicPrimitive.MultiplyAddEstimate FloatWidth.Single)
        | "System", "Double", "ConvertToIntegerNative", [ Shape.Of PrimitiveType.Double ], 1 ->
            Some (IntrinsicPrimitive.ConvertToIntegerNative FloatWidth.Double)
        | "System", "Single", "ConvertToIntegerNative", [ Shape.Of PrimitiveType.Single ], 1 ->
            Some (IntrinsicPrimitive.ConvertToIntegerNative FloatWidth.Single)
        | _ -> None

    let private cannotFault (result : ResultNullness) : IntrinsicContract =
        {
            Raises = []
            CanReturn = true
            Result = result
        }

    /// Dereferences its first argument, so a null one raises `NullReferenceException`: CoreCLR
    /// turns a null dereference in managed code into that exception, or checks explicitly first.
    let private dereferencesFirstArgument (result : ResultNullness) : IntrinsicContract =
        {
            Raises = [ PrimitiveFault.NullReference, FaultCondition.ArgumentNull 0 ]
            CanReturn = true
            Result = result
        }

    /// Accesses its first argument's location atomically or with acquire semantics, so besides
    /// the null dereference, a location misaligned for the access can fault.
    let private atomicallyAccessesFirstArgument (result : ResultNullness) : IntrinsicContract =
        let contract = dereferencesFirstArgument result

        { contract with
            Raises =
                contract.Raises
                @ [ PrimitiveFault.DataMisaligned, FaultCondition.ArgumentMisaligned 0 ]
        }

    /// What `primitive` can do to its caller, from the code CoreCLR's JIT emits for it
    /// (`impIntrinsic` and its helpers, importercalls.cpp) or its VM substitutes.
    let contract (primitive : IntrinsicPrimitive) : IntrinsicContract =
        match primitive with
        | IntrinsicPrimitive.FullBarrier
        | IntrinsicPrimitive.ReadBarrier
        | IntrinsicPrimitive.WriteBarrier
        | IntrinsicPrimitive.GcPoll -> cannotFault ResultNullness.NotAReference
        // `gtNewAtomicNode` addresses the location directly: a null one faults at the access, and
        // so can a misaligned one, except that a byte cannot be misaligned.
        | IntrinsicPrimitive.AtomicCompareExchange AtomicOperand.UInt8
        | IntrinsicPrimitive.AtomicExchange AtomicOperand.UInt8 ->
            dereferencesFirstArgument ResultNullness.NotAReference
        | IntrinsicPrimitive.AtomicCompareExchange _
        | IntrinsicPrimitive.AtomicExchange _
        | IntrinsicPrimitive.AtomicAdd _ -> atomicallyAccessesFirstArgument ResultNullness.NotAReference
        // `gtNewMethodTableLookup` loads through the object.
        | IntrinsicPrimitive.MethodTableOf -> dereferencesFirstArgument ResultNullness.NonNull
        // `gtNewNullCheck` on the array unless the JIT knows it non-null; the address it then
        // forms is marked `GTF_INX_ADDR_NONNULL`.
        | IntrinsicPrimitive.ArrayDataReference -> dereferencesFirstArgument ResultNullness.NonNull
        // An acquire load of a native int. The pointer read may be null: its callers mask and
        // test it.
        | IntrinsicPrimitive.VolatileReadByref -> atomicallyAccessesFirstArgument ResultNullness.MaybeNull
        | IntrinsicPrimitive.IsReferenceOrContainsReferences
        | IntrinsicPrimitive.IsBitwiseEquatable -> cannotFault ResultNullness.NotAReference
        // Floating-point arithmetic and conversion: an unrepresentable result is a value, not a
        // fault.
        | IntrinsicPrimitive.ReciprocalEstimate _
        | IntrinsicPrimitive.ReciprocalSqrtEstimate _
        | IntrinsicPrimitive.MultiplyAddEstimate _
        | IntrinsicPrimitive.ConvertToIntegerNative _ -> cannotFault ResultNullness.NotAReference
