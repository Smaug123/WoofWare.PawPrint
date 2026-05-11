namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module internal RuntimeFieldProjection =
    /// Field address projections for runtime-managed layouts. These are fields
    /// whose metadata names an ordinary field, but whose address in the real CLR
    /// is a view over structured runtime storage rather than a standalone object
    /// field cell. Keep new trailing-data cases here so IL op execution stays
    /// generic.
    let private isCorelibType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        (namespaceName : string)
        (typeName : string)
        : bool
        =
        field.DeclaringType.Assembly.FullName = baseClassTypes.Corelib.Name.FullName
        && field.DeclaringType.Namespace = namespaceName
        && field.DeclaringType.Name = typeName
        && field.DeclaringType.Generics.IsEmpty

    let private isStringFirstChar
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        : bool
        =
        field.Name = "_firstChar"
        && isCorelibType baseClassTypes field "System" "String"

    let private tryProjectStringTrailingDataFieldAddress
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        (addr : ManagedHeapAddress)
        : ManagedPointerSource option
        =
        if isStringFirstChar baseClassTypes field then
            ManagedPointerSource.Byref (ByrefRoot.StringCharAt (addr, 0), []) |> Some
        else
            None

    /// `String._firstChar` is the metadata-level handle for char 0 of the inline
    /// character data; PawPrint stores that data in `StringArrayData`. Load
    /// projections synthesise the field's value from the side-table so there is
    /// only one storage location to keep coherent.
    let private tryProjectStringFirstCharLoad
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        (addr : ManagedHeapAddress)
        (heap : ManagedHeap)
        : CliType option
        =
        if isStringFirstChar baseClassTypes field then
            ManagedHeap.getStringChar addr 0 heap |> CliType.ofChar |> Some
        else
            None

    /// Symmetric to `tryProjectStringFirstCharLoad`: route `stfld _firstChar`
    /// writes through `setStringChar 0` so the byte view and the canonical
    /// `StringContents` value stay coherent with the metadata-level field.
    let private tryProjectStringFirstCharStore
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        (addr : ManagedHeapAddress)
        (value : CliType)
        (heap : ManagedHeap)
        : ManagedHeap option
        =
        if isStringFirstChar baseClassTypes field then
            let c =
                match value with
                | CliType.Char (high, low) -> char (int high * 256 + int low)
                | other -> failwith $"stfld String._firstChar: expected char value, got %O{other}"

            ManagedHeap.setStringChar addr 0 c heap |> Some
        else
            None

    let private byteConcreteType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : ConcreteType<ConcreteTypeHandle>
        =
        let byteHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Byte

        match AllConcreteTypes.lookup byteHandle state.ConcreteTypes with
        | Some byteType -> byteType
        | None -> failwith "RawData projection could not find System.Byte in AllConcreteTypes"

    let private isRawDataField
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        : bool
        =
        isCorelibType baseClassTypes field "System.Runtime.CompilerServices" "RawData"

    /// Render a `ConcreteTypeHandle` as `Namespace.Name [AssemblyShortName] (concrete H)`
    /// for diagnostic messages. Falls back gracefully when the lookup chain breaks,
    /// since this is called from failure paths that should not throw a second time.
    let private describeConcreteType (state : IlMachineState) (handle : ConcreteTypeHandle) : string =
        match AllConcreteTypes.lookup handle state.ConcreteTypes with
        | None -> $"<unregistered concrete type %O{handle}>"
        | Some concrete ->
            match state.LoadedAssembly concrete.Assembly with
            | None -> $"<unloaded assembly %O{concrete.Assembly} for concrete type %O{handle}>"
            | Some assembly ->
                match assembly.TypeDefs.TryGetValue concrete.Definition.Get with
                | true, typeDef ->
                    $"%s{typeDef.Namespace}.%s{typeDef.Name} [%s{assembly.Name.Name}] (concrete %O{handle})"
                | false, _ ->
                    $"<missing TypeDef %O{concrete.Definition.Get} in %s{assembly.Name.Name}> (concrete %O{handle})"

    /// `RawData::Data` projects to a byref over the instance data of any heap object.
    /// For boxed value types this is the boxed payload; for reference types this is the
    /// instance fields (the method-table header is implicit in PawPrint's storage model).
    /// For arrays, CoreCLR places `RawData::Data` at the start of the length-and-padding
    /// header (`sizeof(nint)` bytes before the first element); see the layout diagram in
    /// `RuntimeHelpers.CoreCLR.cs:622-638`. Both non-array cases share the same byte-view
    /// shape over `HeapValue`; the array case starts at `ArrayElement(arr, 0)` carrying a
    /// trailing negative `ByteOffset` so the canonical `+sizeof(nint)` skip used by
    /// `CastCache.TableData` collapses cleanly to `&array[0]` via the existing
    /// `ManagedPointerSource` offset arithmetic.
    ///
    /// Per-byte safety is enforced when the byref is read or written, so the projection
    /// itself only needs to confirm a heap object exists.
    let private requireHeapObject (addr : ManagedHeapAddress) (state : IlMachineState) : unit =
        let exists =
            state.ManagedHeap.NonArrayObjects.ContainsKey addr
            || state.ManagedHeap.Arrays.ContainsKey addr

        if not exists then
            failwith $"RawData::Data projection expected heap object at %O{addr}, but no such object exists"

    /// Size of `nint` on the guest. PawPrint targets 64-bit guests exclusively, so this is
    /// always 8, but routing through `CliType.sizeOf` keeps the contract explicit and matches
    /// the helper in `Native/NativeRuntimeType.fs`.
    let private nativeIntSize : int =
        CliType.sizeOf (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)))

    let private tryProjectRawDataFieldAddress
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        (addr : ManagedHeapAddress)
        (state : IlMachineState)
        : ManagedPointerSource option
        =
        if not (isRawDataField baseClassTypes field) then
            None
        else
            match field.Name with
            | "Data" ->
                requireHeapObject addr state

                let byteView = ByrefProjection.ReinterpretAs (byteConcreteType baseClassTypes state)

                if state.ManagedHeap.Arrays.ContainsKey addr then
                    // CoreCLR's `Unsafe.As<RawData>(arr).Data` is a byref to the array's
                    // length-and-padding header, `sizeof(nint)` bytes before the first
                    // element. We model that "before-element-0" position by anchoring at
                    // ArrayElement(arr, 0) with a trailing negative byte offset under the
                    // byte view. The canonical follow-up arithmetic
                    // `Unsafe.AddByteOffset(rawData, sizeof(nint))` collapses this offset
                    // to zero via the existing ByteOffset-pair rule in ManagedPointerSource
                    // (see `appendProjection` collapse on `n = -m`), leaving &array[0] as
                    // a clean byte byref. Reads at the raw byref position (i.e. before any
                    // forward skip) would land at ArrayElement(arr, -k) and fail at the
                    // array-access boundary with a tightened error message; this is the
                    // intended degradation for any caller that tries to read the
                    // length-header bytes through `RawData::Data` rather than via
                    // `RawArrayData::Length`.
                    ManagedPointerSource.Byref (
                        ByrefRoot.ArrayElement (addr, 0),
                        [ byteView ; ByrefProjection.ByteOffset (-nativeIntSize) ]
                    )
                    |> Some
                else
                    // Non-array heap object: byref to the start of instance data.
                    // Payload byte-view safety, including object-reference and layout
                    // checks, is enforced when the byref is read or written.
                    ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, [ byteView ]) |> Some
            | _ ->
                failwith
                    $"TODO: RawData field address projection for System.Runtime.CompilerServices.RawData::{field.Name}"

    let tryProjectFieldAddress
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        (addr : ManagedHeapAddress)
        (state : IlMachineState)
        : ManagedPointerSource option
        =
        match RawArrayDataProjection.tryProjectFieldAddress baseClassTypes field addr state with
        | Some projection -> Some projection
        | None ->
            match tryProjectRawDataFieldAddress baseClassTypes field addr state with
            | Some projection -> Some projection
            | None -> tryProjectStringTrailingDataFieldAddress baseClassTypes field addr

    /// Synthesise an `ldfld` value for fields whose canonical storage lives outside the
    /// heap object's field map (currently `RawArrayData::Length` and `String._firstChar`).
    /// Returns `None` for fields that should fall through to the standard field-map lookup.
    let tryProjectFieldLoad
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        (addr : ManagedHeapAddress)
        (state : IlMachineState)
        : CliType option
        =
        match RawArrayDataProjection.tryProjectField baseClassTypes field addr state with
        | Some value -> Some value
        | None -> tryProjectStringFirstCharLoad baseClassTypes field addr state.ManagedHeap

    /// Route an `stfld` value for projected fields back to their canonical storage. Returns
    /// the updated heap, or `None` for fields that should fall through to the standard
    /// field-map write.
    let tryProjectFieldStore
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        (addr : ManagedHeapAddress)
        (value : CliType)
        (heap : ManagedHeap)
        : ManagedHeap option
        =
        tryProjectStringFirstCharStore baseClassTypes field addr value heap
