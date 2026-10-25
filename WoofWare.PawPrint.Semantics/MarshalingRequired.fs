namespace WoofWare.PawPrint

/// Which generic definition a value type instantiates, as `IsValidForGenericMarshalling`
/// (vm/mlinfo.cpp) reads it.
[<RequireQualifiedAccess>]
type ValueTypeInstantiation =
    /// It instantiates none (`MethodTable::HasInstantiation` is false).
    | NotGeneric
    /// It instantiates a generic definition, as a type nested in a generic type does too: where
    /// that definition is a top-level type of CoreLib, its namespace and name, such as
    /// ``("System", "Span`1")``.
    | Generic of coreLibDefinition : (string * string) option

/// What `PInvoke::MarshalingRequired` (vm/dllimport.cpp) reads of a value type it has loaded in
/// full: the answers of the `MethodTable` accessors of these names, and the type's size.
type ValueTypeFacts =
    {
        IsEnum : bool
        IsBlittable : bool
        ContainsGcPointers : bool
        IsAutoLayoutOrHasAutoLayoutField : bool
        IsInt128OrHasInt128Fields : bool
        /// Only .NET 11's CoreCLR reads this, and only .NET 11's CoreLib declares the types that
        /// make it true, so it is false wherever .NET 10's verdict would not read it.
        IsDecimalFloatingPointOrHasDecimalFloatingPointFields : bool
        Instantiation : ValueTypeInstantiation
        /// At least the type's instance size in bytes (`TypeHandle::GetSize`).
        SizeUpperBound : int64
    }

/// What `PInvoke::MarshalingRequired` makes of one type of a signature, as far as its spelling
/// decides, before it reads the position's `[MarshalAs]`.
[<RequireQualifiedAccess>]
type MarshalingStep =
    /// It passes, loading nothing.
    | NotRequired
    /// It needs marshalling, which CoreCLR decides without loading anything.
    | Required
    /// CoreCLR loads this type in full (`GetTypeHandleThrowing`), and then finds it needs
    /// marshalling: it instantiates a class.
    | RequiredOnceLoaded of loaded : TypeDefn
    /// CoreCLR loads this value type in full, and then decides by its facts
    /// (`MarshalingRequired.valueTypeRequires`).
    | DecidedByFacts of loaded : TypeDefn

/// What the caller of the walk says of a value type the walk loads in full
/// (`MarshalingStep.DecidedByFacts`).
[<RequireQualifiedAccess>]
type ValueTypeVerdictInput =
    /// It loads, and these are its facts.
    | Loads of ValueTypeFacts
    /// Loading it fails, and the walk raises the failure there.
    | FailsToLoad
    /// The caller cannot say, such as where the type spells a type variable of code shared
    /// between instantiations. The walk goes on past it, as it would if the type needed no
    /// marshalling.
    | Undecided

/// How the walk of a signature ends (`MarshalingWalk`).
[<RequireQualifiedAccess>]
type MarshalingEnding =
    /// It passes every position without finding one that needs marshalling. Then marshalling is
    /// still required where the parameters take more than 65535 bytes of stack (`FitsInU2`):
    /// `stackMayOverflow` says whether an upper bound on their size reaches that, as it does
    /// wherever a value type's verdict was undecided.
    | NoneRequired of stackMayOverflow : bool
    /// It finds that the type at this position, 0 the return type and then each parameter from
    /// 1, needs marshalling, having made the loads `MarshalingWalk.Loads` lists.
    | Required of position : int
    /// Loading the value type at this position fails, the last of `MarshalingWalk.Loads`.
    | LoadFails of position : int

/// What `PInvoke::MarshalingRequired` does over a signature, from its return type and then each
/// parameter, ending at the first position that needs marshalling.
type MarshalingWalk =
    {
        /// Each type it loads in full, with its position, in the order it loads them.
        Loads : (int * TypeDefn) list
        /// The positions before its end where it loaded a value type whose verdict its caller could
        /// not give (`ValueTypeVerdictInput.Undecided`): it may have ended at any of them instead.
        Undecided : int list
        Ending : MarshalingEnding
    }

/// The walk of `MarshalingRequired`, waiting for its caller to load a value type.
type PendingMarshalingWalk =
    private
        {
            RuntimeMarshalling : bool
            /// The position of the value type, and the type.
            Awaiting : int * TypeDefn
            /// The positions after it, each with its step and whether its `[MarshalAs]` needs
            /// marshalling.
            Remaining : (int * MarshalingStep * bool) list
            /// The loads so far, latest first, before `Awaiting`'s.
            LoadedSoFar : (int * TypeDefn) list
            UndecidedSoFar : int list
            /// The parameters' stack size so far, at most; `None` once a size is unknown.
            StackBound : int64 option
        }

/// Where the walk of a signature is (`MarshalingRequired.start`).
[<RequireQualifiedAccess>]
type MarshalingWalkState =
    | Ended of MarshalingWalk
    /// It loads this value type, at this position, in full, and waits for its caller to say what
    /// that gives (`MarshalingRequired.resume`).
    | LoadsValueType of position : int * loaded : TypeDefn * pending : PendingMarshalingWalk

/// CoreCLR's `PInvoke::MarshalingRequired` (vm/dllimport.cpp): whether a call between managed and
/// unmanaged code needs marshalling, as it walks the signature, and what it loads doing so. The
/// method-level conditions it checks first (`PreserveSig`, `SetLastError`, `LCIDConversion`) are
/// its callers'.
[<RequireQualifiedAccess>]
module MarshalingRequired =

    /// The modifiers `MarshalingRequired` looks for on a value type a pointer points to, which make
    /// the pointer need marshalling where runtime marshalling is enabled, by the full names CoreCLR
    /// compares (`IsTypeRefOrDef`).
    let private copyConstructorModifiers : Set<string> =
        Set.ofList
            [
                "Microsoft.VisualC.NeedsCopyConstructorModifier"
                "System.Runtime.CompilerServices.IsCopyConstructed"
            ]

    /// The full name of a type `assembly` spells by a reference or a definition: its namespace and
    /// name joined with a dot, or its name alone where the namespace is empty, which is how CoreCLR
    /// compares a name with one the metadata splits (`IsTypeRefOrDef`).
    let private spelledName (assembly : DumpedAssembly) (spelling : TypeDefn) : string option =
        let joined (ns : string) (name : string) : string =
            if ns = "" then name else ns + "." + name

        match spelling with
        | TypeDefn.FromReference (typeRef, _) -> Some (joined typeRef.Namespace typeRef.Name)
        | TypeDefn.FromDefinition (identity, _) when identity.AssemblyFullName = assembly.DefinitionFullName ->
            let definition = assembly.TypeDefs.[identity.TypeDefinition.Get]
            Some (joined definition.Namespace definition.Name)
        | _ -> None

    /// The names of the required modifiers on `spelling`, outermost first, and what they modify.
    let rec private requiredModifiers (assembly : DumpedAssembly) (spelling : TypeDefn) : string list * TypeDefn =
        match spelling with
        | TypeDefn.Modified modified ->
            let names, unmodified = requiredModifiers assembly modified.Unmodified

            let own =
                if modified.IsRequired then
                    Option.toList (spelledName assembly modified.Modifier)
                else
                    []

            own @ names, unmodified
        | spelling -> [], spelling

    /// Whether a type reference or definition of a signature names a value type, as the signature's
    /// element type says (`VALUETYPE` or `CLASS`).
    let private namesValueType (kind : System.Reflection.Metadata.SignatureTypeKind) : bool =
        match kind with
        | System.Reflection.Metadata.SignatureTypeKind.ValueType -> true
        | System.Reflection.Metadata.SignatureTypeKind.Class -> false
        | kind -> failwith $"BUG: a signature spells a type of kind %O{kind}, neither a value type nor a class"

    /// What the walk makes of `spelling`, a type of a signature of `assembly` (its return type,
    /// `TypeDefn.Void` for none, or a parameter's), where runtime marshalling is enabled for the
    /// module whose signature it walks or not (`Module::IsRuntimeMarshallingEnabled`: the module
    /// carries no `[DisableRuntimeMarshalling]`). It reads past custom modifiers, as `PeekElemType`
    /// does, except on what a pointer points to, where it looks for a copy-constructor modifier.
    let ofSpelling (assembly : DumpedAssembly) (runtimeMarshalling : bool) (spelling : TypeDefn) : MarshalingStep =
        match TypeDefn.stripCustomModifiers spelling with
        | TypeDefn.Void -> MarshalingStep.NotRequired
        | TypeDefn.PrimitiveType primitive ->
            match primitive with
            | PrimitiveType.Boolean
            | PrimitiveType.Char ->
                if runtimeMarshalling then
                    MarshalingStep.Required
                else
                    MarshalingStep.NotRequired
            // `ELEMENT_TYPE_TYPEDBYREF` is not a primitive of `CorTypeInfo`'s table.
            | PrimitiveType.String
            | PrimitiveType.Object
            | PrimitiveType.TypedReference -> MarshalingStep.Required
            | PrimitiveType.SByte
            | PrimitiveType.Byte
            | PrimitiveType.Int16
            | PrimitiveType.UInt16
            | PrimitiveType.Int32
            | PrimitiveType.UInt32
            | PrimitiveType.Int64
            | PrimitiveType.UInt64
            | PrimitiveType.Single
            | PrimitiveType.Double
            | PrimitiveType.IntPtr
            | PrimitiveType.UIntPtr -> MarshalingStep.NotRequired
        | TypeDefn.Pointer pointee ->
            let modifiers, pointee = requiredModifiers assembly pointee

            let pointsToValueType =
                match pointee with
                | TypeDefn.FromReference (_, System.Reflection.Metadata.SignatureTypeKind.ValueType)
                | TypeDefn.FromDefinition (_, System.Reflection.Metadata.SignatureTypeKind.ValueType) -> true
                | _ -> false

            if
                runtimeMarshalling
                && pointsToValueType
                && modifiers |> List.exists copyConstructorModifiers.Contains
            then
                MarshalingStep.Required
            else
                MarshalingStep.NotRequired
        | TypeDefn.FunctionPointer _ -> MarshalingStep.NotRequired
        | TypeDefn.FromReference (_, kind)
        | TypeDefn.FromDefinition (_, kind) as named ->
            if namesValueType kind then
                MarshalingStep.DecidedByFacts named
            else
                MarshalingStep.Required
        | TypeDefn.GenericInstantiation (root, _) as instantiation ->
            match TypeDefn.stripCustomModifiers root with
            | TypeDefn.FromReference (_, kind)
            | TypeDefn.FromDefinition (_, kind) when namesValueType kind -> MarshalingStep.DecidedByFacts instantiation
            | _ -> MarshalingStep.RequiredOnceLoaded instantiation
        | TypeDefn.Byref _
        | TypeDefn.Array _
        | TypeDefn.OneDimensionalArrayLowerBoundZero _
        | TypeDefn.Pinned _
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _ -> MarshalingStep.Required
        | TypeDefn.Modified _ -> failwith "BUG: stripCustomModifiers left a modifier"

    /// Whether a position's `[MarshalAs]`, as its Param row's FieldMarshal says, makes it need
    /// marshalling where runtime marshalling is enabled or not: where it is, any blob CoreCLR fails
    /// to parse or that names a native type other than `NATIVE_TYPE_DEFAULT` does
    /// (`ParseNativeTypeInfo`); where it is disabled, CoreCLR does not read it.
    let marshalAsRequires (runtimeMarshalling : bool) (marshal : ParameterMarshal) : bool =
        match marshal with
        | ParameterMarshal.Absent -> false
        | ParameterMarshal.Present None -> runtimeMarshalling
        | ParameterMarshal.Present (Some info) ->
            runtimeMarshalling && info.NativeType <> NativeTypeParamInfo.NativeTypeDefault

    /// The positions of `signature`, spelled in `assembly`, as `start` takes them: its return type
    /// and then each parameter, each with its step (`ofSpelling`) and the `[MarshalAs]` that
    /// `marshalling`, the method's Param rows by sequence number (`MethodInfo.parameterMarshalling`),
    /// gives it, where runtime marshalling is enabled for `assembly` or not. Where a sequence number
    /// names no Param row, or several, CoreCLR reads the position as having no `[MarshalAs]`, or
    /// refuses the signature (`CollateParamTokens`), which is not modelled: the first row counts.
    let positions
        (assembly : DumpedAssembly)
        (runtimeMarshalling : bool)
        (signature : TypeMethodSignature<TypeDefn>)
        (marshalling : (int * ParameterMarshal) list)
        : (MarshalingStep * ParameterMarshal) list
        =
        let returned =
            match signature.ReturnType with
            | MethodReturnType.Void -> TypeDefn.Void
            | MethodReturnType.Returns returned -> returned

        returned :: signature.ParameterTypes
        |> List.mapi (fun sequence spelling ->
            let marshal =
                marshalling
                |> List.tryFind (fun (row, _) -> row = sequence)
                |> Option.map snd
                |> Option.defaultValue ParameterMarshal.Absent

            ofSpelling assembly runtimeMarshalling spelling, marshal
        )

    /// The generic definitions of CoreLib that `IsValidForGenericMarshalling` refuses an
    /// instantiation of, by namespace and name (`CoreLibBinder::GetClass`).
    let private genericMarshallingRefusals : Set<string * string> =
        Set.ofList
            [
                "System", "Nullable`1"
                "System", "Span`1"
                "System", "ReadOnlySpan`1"
                "System.Runtime.Intrinsics", "Vector64`1"
                "System.Runtime.Intrinsics", "Vector128`1"
                "System.Runtime.Intrinsics", "Vector256`1"
                "System.Runtime.Intrinsics", "Vector512`1"
                "System.Numerics", "Vector`1"
            ]

    /// Whether a value type whose facts are `facts` needs marshalling as a type of a signature,
    /// where runtime marshalling is enabled or not (dllimport.cpp, from `IsValidForGenericMarshalling`
    /// on): it is an instantiation that is not blittable with runtime marshalling enabled, or of one
    /// of CoreLib's definitions that marshalling refuses (`Nullable<T>`, the spans, the vectors);
    /// it holds an `Int128` or a decimal floating-point type; with runtime marshalling enabled, it
    /// is neither blittable nor an enum; with it disabled, it is not an enum and holds a GC pointer
    /// or has auto layout somewhere.
    let valueTypeRequires (runtimeMarshalling : bool) (facts : ValueTypeFacts) : bool =
        let validForGenericMarshalling =
            match facts.Instantiation with
            | ValueTypeInstantiation.NotGeneric -> true
            | ValueTypeInstantiation.Generic definition ->
                (not runtimeMarshalling || facts.IsBlittable)
                && not (definition |> Option.exists genericMarshallingRefusals.Contains)

        not validForGenericMarshalling
        || facts.IsInt128OrHasInt128Fields
        || facts.IsDecimalFloatingPointOrHasDecimalFloatingPointFields
        || (if runtimeMarshalling then
                not facts.IsBlittable && not facts.IsEnum
            else
                not facts.IsEnum
                && (facts.ContainsGcPointers || facts.IsAutoLayoutOrHasAutoLayoutField))

    /// The most bytes of stack the parameters may take before marshalling is required (`FitsInU2`).
    [<Literal>]
    let MaxStackSize = 65535L

    /// The stack size a parameter takes, at most, for each type `ofSpelling` passes without a load:
    /// a primitive, a pointer or a function pointer, each at most a stack slot (`StackElemSize`).
    [<Literal>]
    let private SlotSize = 8L

    /// `StackElemSize` of a value type at most `size` bytes long, at most: rounded up to a slot.
    /// It saturates well above `MaxStackSize`, so that no bound overflows.
    let private valueTypeStackSize (size : int64) : int64 =
        let size = min (max size 0L) (1L <<< 40)
        (size + SlotSize - 1L) / SlotSize * SlotSize

    let private add (position : int) (size : int64) (bound : int64 option) : int64 option =
        if position = 0 then
            bound
        else
            bound |> Option.map (fun bound -> min (bound + size) (1L <<< 40))

    let rec private advance
        (runtimeMarshalling : bool)
        (remaining : (int * MarshalingStep * bool) list)
        (loaded : (int * TypeDefn) list)
        (undecided : int list)
        (stack : int64 option)
        : MarshalingWalkState
        =
        let ended (loaded : (int * TypeDefn) list) (ending : MarshalingEnding) =
            MarshalingWalkState.Ended
                {
                    Loads = List.rev loaded
                    Undecided = List.rev undecided
                    Ending = ending
                }

        match remaining with
        | [] ->
            let mayOverflow =
                match stack with
                | Some bound -> bound > MaxStackSize
                | None -> true

            ended loaded (MarshalingEnding.NoneRequired mayOverflow)
        | (position, step, marshalAs) :: rest ->

        match step with
        | MarshalingStep.Required -> ended loaded (MarshalingEnding.Required position)
        | MarshalingStep.NotRequired ->
            if marshalAs then
                ended loaded (MarshalingEnding.Required position)
            else
                advance runtimeMarshalling rest loaded undecided (add position SlotSize stack)
        | MarshalingStep.RequiredOnceLoaded spelling ->
            ended ((position, spelling) :: loaded) (MarshalingEnding.Required position)
        | MarshalingStep.DecidedByFacts spelling ->
            // Whatever the value type's verdict, a `[MarshalAs]` read after it needs marshalling.
            if marshalAs then
                ended ((position, spelling) :: loaded) (MarshalingEnding.Required position)
            else
                MarshalingWalkState.LoadsValueType (
                    position,
                    spelling,
                    {
                        RuntimeMarshalling = runtimeMarshalling
                        Awaiting = position, spelling
                        Remaining = rest
                        LoadedSoFar = loaded
                        UndecidedSoFar = undecided
                        StackBound = stack
                    }
                )

    /// Begin the walk over `positions`, the return type (`TypeDefn.Void` for none) and then each
    /// parameter of a signature, each with its step (`ofSpelling`) and its `[MarshalAs]`, where
    /// runtime marshalling is enabled for the module whose signature it walks or not.
    let start (runtimeMarshalling : bool) (positions : (MarshalingStep * ParameterMarshal) list) : MarshalingWalkState =
        let positions =
            positions
            |> List.mapi (fun position (step, marshal) -> position, step, marshalAsRequires runtimeMarshalling marshal)

        advance runtimeMarshalling positions [] [] (Some 0L)

    /// Go on with `pending`, given what loading the value type it waits on gives.
    let resume (pending : PendingMarshalingWalk) (input : ValueTypeVerdictInput) : MarshalingWalkState =
        let position, spelling = pending.Awaiting
        let loaded = (position, spelling) :: pending.LoadedSoFar

        match input with
        | ValueTypeVerdictInput.FailsToLoad ->
            MarshalingWalkState.Ended
                {
                    Loads = List.rev loaded
                    Undecided = List.rev pending.UndecidedSoFar
                    Ending = MarshalingEnding.LoadFails position
                }
        | ValueTypeVerdictInput.Undecided ->
            advance pending.RuntimeMarshalling pending.Remaining loaded (position :: pending.UndecidedSoFar) None
        | ValueTypeVerdictInput.Loads facts ->
            if valueTypeRequires pending.RuntimeMarshalling facts then
                MarshalingWalkState.Ended
                    {
                        Loads = List.rev loaded
                        Undecided = List.rev pending.UndecidedSoFar
                        Ending = MarshalingEnding.Required position
                    }
            else
                advance
                    pending.RuntimeMarshalling
                    pending.Remaining
                    loaded
                    pending.UndecidedSoFar
                    (add position (valueTypeStackSize facts.SizeUpperBound) pending.StackBound)
