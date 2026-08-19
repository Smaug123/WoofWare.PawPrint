namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging

/// <summary>
/// How one custom-attribute argument is both read out of the blob and turned into a runtime value:
/// the ECMA-335 II.23.3 grammar its bytes follow, plus — for an <c>SZARRAY</c> — the concretised
/// element type, which the blob does not record.
/// </summary>
/// <remarks>
/// This mirrors <see cref="T:WoofWare.PawPrint.CustomAttribArgShape"/>, which is what the decoder
/// consumes and which <c>CustomAttribArgPlan.shape</c> projects back out. The extra information is
/// needed because a fixed argument's element type comes from the constructor signature rather than
/// from the blob, and because the shape's <c>Enum</c> case carries only the underlying width —
/// enough to read the bytes, but not enough to say that a <c>MyEnum[]</c> is a <c>MyEnum[]</c>
/// rather than an <c>int16[]</c>. Driving the read and the lowering from one value is what makes a
/// value/plan mismatch impossible for anything the decoder produced.
/// </remarks>
[<RequireQualifiedAccess>]
type CustomAttribArgPlan =
    | Primitive of PrimitiveType
    | Enum of underlying : EnumUnderlyingType
    /// <c>elementType</c> is the element type the constructor *declared*, concretised: CoreCLR's
    /// fixed-arg loop likewise takes it from the signature (<c>customattribute.cpp:961</c>) rather
    /// than from the blob, so the allocated array's element type does not depend on what the bytes
    /// happen to look like.
    | SzArray of elementType : ConcreteTypeHandle * elements : CustomAttribArgPlan

[<RequireQualifiedAccess>]
module CustomAttribArgPlan =
    /// The decoder's view of a plan: what the bytes look like, with the runtime types dropped.
    let rec shape (plan : CustomAttribArgPlan) : CustomAttribArgShape =
        match plan with
        | CustomAttribArgPlan.Primitive pt -> CustomAttribArgShape.Primitive pt
        | CustomAttribArgPlan.Enum underlying -> CustomAttribArgShape.Enum underlying
        | CustomAttribArgPlan.SzArray (_, elements) -> CustomAttribArgShape.SzArray (shape elements)

/// <summary>
/// Lower a decoded <c>CustomAttribFixedArg</c> to the <c>CliType</c> value the
/// attribute ctor expects on its parameter slot.
/// </summary>
/// <remarks>
/// Mirrors the per-arg cases that CoreCLR's <c>CustomAttribute_CreateCustomAttributeInstance</c>
/// (<c>customattribute.cpp:900</c>) produces via <c>GetDataFromBlob</c> + <c>Box</c> just
/// before invoking the ctor. The current set is limited to the cases the blob
/// reader emits: primitives, <c>SerString</c>, <c>ENUM</c>, and <c>SZARRAY</c> of those.
/// <c>TYPE</c> and <c>TAGGED_OBJECT</c> will be added when the QCall handler needs them.
/// </remarks>
[<RequireQualifiedAccess>]
module CustomAttribValueLowering =

    /// <summary>
    /// Convert a fixed arg whose lowering needs nothing but the decoded value itself. Returns
    /// <c>Error</c> for the variants that need more — a heap allocation (<c>String (Some _)</c>),
    /// or the element type an <c>Array</c> does not carry; callers should use <c>toCliType</c> in
    /// that case.
    /// </summary>
    /// <remarks>
    /// CLI eval-stack rules treat <c>uint32</c>/<c>uint64</c> identically to their
    /// signed counterparts (the spec stores them with two's-complement wraparound;
    /// see the <c>UInt32</c>/<c>UInt64</c> branches of <c>CliType.zeroOfPrimitive</c>),
    /// so the unsigned variants here route through <c>Int32</c>/<c>Int64</c>.
    /// </remarks>
    let rec tryToPureCliType (arg : CustomAttribFixedArg) : Result<CliType, string> =
        match arg with
        | CustomAttribFixedArg.Bool b -> CliType.Bool (if b then 1uy else 0uy) |> Ok
        | CustomAttribFixedArg.Char c ->
            let v = uint16 c
            CliType.Char (byte (v >>> 8), byte (v &&& 0xFFus)) |> Ok
        | CustomAttribFixedArg.I1 v -> CliType.Numeric (CliNumericType.Int8 v) |> Ok
        | CustomAttribFixedArg.U1 v -> CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim v)) |> Ok
        | CustomAttribFixedArg.I2 v -> CliType.Numeric (CliNumericType.Int16 v) |> Ok
        | CustomAttribFixedArg.U2 v -> CliType.Numeric (CliNumericType.UInt16 v) |> Ok
        | CustomAttribFixedArg.I4 v -> CliType.Numeric (CliNumericType.Int32 v) |> Ok
        | CustomAttribFixedArg.U4 v -> CliType.Numeric (CliNumericType.Int32 (int32 v)) |> Ok
        | CustomAttribFixedArg.I8 v -> CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim v)) |> Ok
        | CustomAttribFixedArg.U8 v -> CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim (int64 v))) |> Ok
        | CustomAttribFixedArg.R4 v -> CliType.Numeric (CliNumericType.Float32 v) |> Ok
        | CustomAttribFixedArg.R8 v -> CliType.Numeric (CliNumericType.Float64 v) |> Ok
        | CustomAttribFixedArg.String None -> CliType.ObjectRef None |> Ok
        | CustomAttribFixedArg.String (Some _) ->
            Error "CustomAttribFixedArg.String (Some _) requires allocation; use CustomAttribValueLowering.toCliType"
        // An enum argument lowers to the bare underlying integer, with no enum wrapper built here.
        // `IlMachineStateExecution.callMethod` derives each parameter's
        // zero value from the ctor's *declared* parameter type — for an enum parameter, the
        // `value__` struct — and `EvalStackValue.toCliTypeCoerced` rewraps the popped integer into
        // that slot. Enums flatten to their underlying integer on the eval stack
        // (`PrimitiveLikeKind.EnumLike`), so pushing the wrapper here would be wrong.
        | CustomAttribFixedArg.Enum underlying -> tryToPureCliType underlying
        // Even the null-array sentinel is refused here rather than answered with `ObjectRef None`:
        // ECMA-335 II.23.3 gives an SZARRAY argument no element type, so without a
        // `CustomAttribArgPlan` there is nothing to confirm this value came from an array
        // parameter at all.
        | CustomAttribFixedArg.Array _ ->
            Error
                "CustomAttribFixedArg.Array requires a CustomAttribArgPlan naming the element type; use CustomAttribValueLowering.toCliType"

    /// <summary>
    /// Lower a fixed arg to a <c>CliType</c>, allocating on the heap when the arg is a non-null
    /// <c>SerString</c> or a non-null <c>SZARRAY</c>. For every other variant the state is returned
    /// unchanged. <paramref name="plan"/> must be the plan the arg was decoded with.
    /// </summary>
    /// <remarks>
    /// The empty <c>SerString</c> routes through the canonical interned empty
    /// string, mirroring CoreCLR's <c>GetDataFromBlob</c> for
    /// <c>SERIALIZATION_TYPE_STRING</c>: <c>StringObject::NewString(0)</c>
    /// (and the <c>(LPCUTF8, cBytes)</c> overload's <c>cBytes == 0</c> branch)
    /// both return <c>GetEmptyString()</c>. Non-empty <c>SerString</c> values
    /// allocate a fresh heap object on every call.
    /// </remarks>
    let rec toCliType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (plan : CustomAttribArgPlan)
        (arg : CustomAttribFixedArg)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        match plan, arg with
        // ECMA-335 II.23.3's `NumElem = 0xFFFFFFFF` is the null-array sentinel, which is a
        // different result from the zero-length array below; CoreCLR likewise leaves the argument
        // slot null rather than allocating (`customattribute.cpp:845`).
        | CustomAttribArgPlan.SzArray _, CustomAttribFixedArg.Array None -> CliType.ObjectRef None, state
        | CustomAttribArgPlan.SzArray (elementType, elementPlan), CustomAttribFixedArg.Array (Some elements) ->
            let elementZero, state =
                IlMachineState.cliTypeZeroOfHandle state baseClassTypes elementType

            let addr, state =
                IlMachineState.allocateArray
                    (ConcreteTypeHandle.OneDimArrayZero elementType)
                    (fun () -> elementZero)
                    (List.length elements)
                    state

            let state =
                (state, List.indexed elements)
                ||> List.fold (fun state (index, element) ->
                    let cell, state = toCliType loggerFactory baseClassTypes elementPlan element state

                    // The coercion `stelem` performs on every guest array store. It is what rewraps
                    // an enum element — which lowers to its bare underlying integer — into the
                    // `EnumLike` value-type cell the array's stride was measured from. For the
                    // other element types in scope it is the identity, bar `Float32`, which widens
                    // to double on the evaluation stack and narrows back; that quietens a
                    // signalling-NaN bit pattern, which no C#-emitted blob carries and which the
                    // scalar argument path already does when it pushes.
                    let cell =
                        EvalStackValue.toCliTypeCoerced elementZero (EvalStackValue.ofCliType cell)

                    IlMachineState.setArrayValue addr cell index state
                )

            CliType.ObjectRef (Some addr), state
        | CustomAttribArgPlan.SzArray _, other ->
            failwithf
                "CustomAttribValueLowering.toCliType: bug — SZARRAY plan %A paired with non-array value %A; a value must be lowered with the plan it was decoded with"
                plan
                other
        | (CustomAttribArgPlan.Primitive _ | CustomAttribArgPlan.Enum _), CustomAttribFixedArg.Array _ ->
            failwithf
                "CustomAttribValueLowering.toCliType: bug — scalar plan %A paired with array value %A; a value must be lowered with the plan it was decoded with"
                plan
                arg
        | _, CustomAttribFixedArg.String (Some "") ->
            let addr, state =
                IlMachineState.internCanonicalEmptyString loggerFactory baseClassTypes state

            CliType.ObjectRef (Some addr), state
        | _, CustomAttribFixedArg.String (Some s) ->
            let addr, state =
                IlMachineState.allocateManagedString loggerFactory baseClassTypes s state

            CliType.ObjectRef (Some addr), state
        | _, _ ->
            match tryToPureCliType arg with
            | Ok t -> t, state
            | Error msg ->
                failwithf "CustomAttribValueLowering.toCliType: bug — non-allocating arg %A produced Error %s" arg msg
