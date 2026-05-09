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

    let private tryProjectStringTrailingDataFieldAddress
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        (addr : ManagedHeapAddress)
        : ManagedPointerSource option
        =
        if
            field.Name = "_firstChar"
            && isCorelibType baseClassTypes field "System" "String"
        then
            ManagedPointerSource.Byref (ByrefRoot.StringCharAt (addr, 0), []) |> Some
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

    /// `RawData::Data` projects to a byref over the instance data of any non-array heap object.
    /// For boxed value types this is the boxed payload; for reference types this is the
    /// instance fields (the method-table header is implicit in PawPrint's storage model). Both
    /// cases share the same byte-view shape; rejecting reference types here would block
    /// reflection/EventSource code paths whose `obj.GetRawData()` walks reach reference fields
    /// via byte-arithmetic plus typed reinterpret. Per-byte safety is enforced when the byref
    /// is read or written, so the projection only needs to fail for arrays.
    let private requireNonArrayHeapObject (addr : ManagedHeapAddress) (state : IlMachineState) : unit =
        match state.ManagedHeap.NonArrayObjects.TryGetValue addr with
        | true, _ -> ()
        | false, _ ->
            let arrayDescription =
                match state.ManagedHeap.Arrays.TryGetValue addr with
                | true, arr -> describeConcreteType state arr.ConcreteType
                | false, _ -> "<no heap object at this address>"

            failwith
                $"RawData::Data projection expected non-array heap object at %O{addr}, got array %s{arrayDescription}"

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
                requireNonArrayHeapObject addr state

                // The projection establishes the runtime storage identity only.
                // Payload byte-view safety, including object-reference and layout
                // checks, is enforced when the byref is read or written.
                ManagedPointerSource.Byref (
                    ByrefRoot.HeapValue addr,
                    [ ByrefProjection.ReinterpretAs (byteConcreteType baseClassTypes state) ]
                )
                |> Some
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
