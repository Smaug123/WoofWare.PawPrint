namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging

/// <summary>
/// Lower a decoded <c>CustomAttribFixedArg</c> to the <c>CliType</c> value the
/// attribute ctor expects on its parameter slot.
/// </summary>
/// <remarks>
/// Mirrors the per-arg cases that CoreCLR's <c>CustomAttribute_CreateCustomAttributeInstance</c>
/// (<c>customattribute.cpp:900</c>) produces via <c>GetDataFromBlob</c> + <c>Box</c> just
/// before invoking the ctor. The current set is limited to the cases the Phase A
/// blob reader emits: primitives and <c>SerString</c>. <c>TYPE</c>, <c>ENUM</c>,
/// <c>SZARRAY</c>, and <c>TAGGED_OBJECT</c> will be added when the QCall handler
/// needs them.
/// </remarks>
[<RequireQualifiedAccess>]
module CustomAttribValueLowering =

    /// <summary>
    /// Convert a fixed arg that does not need heap allocation. Returns
    /// <c>Error</c> for variants whose lowering must allocate
    /// (currently only <c>String (Some _)</c>); callers should use
    /// <c>toCliType</c> in that case.
    /// </summary>
    /// <remarks>
    /// CLI eval-stack rules treat <c>uint32</c>/<c>uint64</c> identically to their
    /// signed counterparts (the spec stores them with two's-complement wraparound;
    /// see the <c>UInt32</c>/<c>UInt64</c> branches of <c>CliType.zeroOfPrimitive</c>),
    /// so the unsigned variants here route through <c>Int32</c>/<c>Int64</c>.
    /// </remarks>
    let tryToPureCliType (arg : CustomAttribFixedArg) : Result<CliType, string> =
        match arg with
        | CustomAttribFixedArg.Bool b -> CliType.Bool (if b then 1uy else 0uy) |> Ok
        | CustomAttribFixedArg.Char c ->
            let v = uint16 c
            CliType.Char (byte (v >>> 8), byte (v &&& 0xFFus)) |> Ok
        | CustomAttribFixedArg.I1 v -> CliType.Numeric (CliNumericType.Int8 v) |> Ok
        | CustomAttribFixedArg.U1 v -> CliType.Numeric (CliNumericType.UInt8 v) |> Ok
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

    /// <summary>
    /// Lower a fixed arg to a <c>CliType</c>, allocating a managed string on the
    /// heap when the arg is a non-null <c>SerString</c>. For every other variant
    /// the state is returned unchanged.
    /// </summary>
    /// <remarks>
    /// The empty <c>SerString</c> routes through the canonical interned empty
    /// string, mirroring CoreCLR's <c>GetDataFromBlob</c> for
    /// <c>SERIALIZATION_TYPE_STRING</c>: <c>StringObject::NewString(0)</c>
    /// (and the <c>(LPCUTF8, cBytes)</c> overload's <c>cBytes == 0</c> branch)
    /// both return <c>GetEmptyString()</c>. Non-empty <c>SerString</c> values
    /// allocate a fresh heap object on every call.
    /// </remarks>
    let toCliType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (arg : CustomAttribFixedArg)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        match arg with
        | CustomAttribFixedArg.String (Some "") ->
            let addr, state =
                IlMachineState.internCanonicalEmptyString loggerFactory baseClassTypes state

            CliType.ObjectRef (Some addr), state
        | CustomAttribFixedArg.String (Some s) ->
            let addr, state =
                IlMachineState.allocateManagedString loggerFactory baseClassTypes s state

            CliType.ObjectRef (Some addr), state
        | _ ->
            match tryToPureCliType arg with
            | Ok t -> t, state
            | Error msg ->
                failwithf "CustomAttribValueLowering.toCliType: bug — non-allocating arg %A produced Error %s" arg msg
