namespace WoofWare.PawPrint

open System.Collections.Immutable
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module NativeMetadataImport =
    let private metadataTokenTypeCustomAttribute : int32 = 0x0c000000
    let private metadataTokenTypeFieldDef : int32 = 0x04000000
    let private metadataTokenTypeExportedType : int32 = 0x27000000

    /// <c>mdtTypeDef</c>. As an <c>Enum</c> token type this does *not* mean "enumerate TypeDefs":
    /// CoreCLR special-cases it to mean the nested classes of the parent (see
    /// <c>nestedTypeDefinitionsForTypeDefinition</c>).
    let private metadataTokenTypeTypeDef : int32 = 0x02000000

    /// <c>mdtProperty</c>. Passed by <c>MetadataImport.EnumProperties</c>, whose only caller is
    /// <c>RuntimeType.PopulateProperties</c>.
    let private metadataTokenTypeProperty : int32 = 0x17000000

    /// <c>mdtMethodDef</c>. As an <c>Enum</c> token type this does *not* mean "enumerate MethodDefs":
    /// CoreCLR pairs it with a Property or Event parent to mean that member's MethodSemantics
    /// associates (see <c>methodSemanticsForAssociation</c>).
    let private metadataTokenTypeMethodDef : int32 = 0x06000000

    /// <c>mdtParamDef</c>. Passed by <c>MetadataImport.EnumParams</c>, whose only caller is
    /// <c>RuntimeParameterInfo.GetParameters</c>.
    let private metadataTokenTypeParamDef : int32 = 0x08000000

    let private metadataEnumSmallResultLimit : int = 16

    /// <c>mdTypeDefNil</c>: TypeDef table code (0x02) | row 0. Returned by
    /// <c>MetadataImport.GetParentToken</c> for top-level types (no NestedClass row).
    let private metadataTypeDefNil : int32 = 0x02000000

    let private metadataReaderOf (assembly : DumpedAssembly) : System.Reflection.Metadata.MetadataReader =
        System.Reflection.Metadata.PEReaderExtensions.GetMetadataReader assembly.PeReader

    let private metadataTokenOfFieldDefinitionHandle
        (fieldHandle : System.Reflection.Metadata.FieldDefinitionHandle)
        : int32
        =
        let fieldHandle : System.Reflection.Metadata.EntityHandle =
            System.Reflection.Metadata.FieldDefinitionHandle.op_Implicit fieldHandle

        System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken fieldHandle

    let private metadataTokenOfTypeDefinitionHandle
        (typeHandle : System.Reflection.Metadata.TypeDefinitionHandle)
        : int32
        =
        let typeHandle : System.Reflection.Metadata.EntityHandle =
            System.Reflection.Metadata.TypeDefinitionHandle.op_Implicit typeHandle

        System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken typeHandle

    let private metadataTokenOfPropertyDefinitionHandle
        (propertyHandle : System.Reflection.Metadata.PropertyDefinitionHandle)
        : int32
        =
        let propertyHandle : System.Reflection.Metadata.EntityHandle =
            System.Reflection.Metadata.PropertyDefinitionHandle.op_Implicit propertyHandle

        System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken propertyHandle

    let private metadataTokenOfParameterHandle (parameterHandle : System.Reflection.Metadata.ParameterHandle) : int32 =
        let parameterHandle : System.Reflection.Metadata.EntityHandle =
            System.Reflection.Metadata.ParameterHandle.op_Implicit parameterHandle

        System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken parameterHandle

    let private metadataImportHandleOfArg (operation : string) (arg : CliType) : string =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.MetadataImportHandle assemblyFullName)) ->
            assemblyFullName
        | other -> failwith $"%s{operation}: expected MetadataImportHandle argument, got %O{other}"

    let private metadataImportAssembly
        (operation : string)
        (state : IlMachineState)
        (assemblyFullName : string)
        : DumpedAssembly
        =
        state.LoadedAssembly' assemblyFullName
        |> Option.defaultWith (fun () ->
            failwith $"%s{operation}: metadata import assembly is not loaded: %s{assemblyFullName}"
        )

    let private writeInt32AtPointer
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        (value : int32)
        : IlMachineState
        =
        IlMachineState.writeManagedByrefWithBase baseClassTypes state ptr (CliType.Numeric (CliNumericType.Int32 value))

    let private int32BufferElementPointer
        (operation : string)
        (buffer : ManagedPointerSource)
        (index : int)
        : ManagedPointerSource option
        =
        match buffer with
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, baseIndex), []) ->
            ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, baseIndex + index), [])
            |> Some
        | ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (thread, frame, block, byteOffset), []) ->
            ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (thread, frame, block, byteOffset + (index * 4)), [])
            |> Some
        | ManagedPointerSource.Null -> failwith $"%s{operation}: expected non-null Int32 result buffer"
        | ManagedPointerSource.NativeIntPlaceholder bits ->
            failwith
                $"%s{operation}: cannot use fake non-null byref @ 0x%x{bits} as Int32 result buffer; the placeholder must never be dereferenced"
        | ManagedPointerSource.Byref _ -> None

    let private tryWriteSmallInt32Buffer
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (buffer : ManagedPointerSource)
        (values : int32 list)
        : IlMachineState option
        =
        if values.Length > metadataEnumSmallResultLimit then
            None
        else
            let mutable state = state
            let mutable index = 0
            let mutable canWrite = true

            while canWrite && index < values.Length do
                match int32BufferElementPointer operation buffer index with
                | Some ptr ->
                    state <- writeInt32AtPointer baseClassTypes state ptr values.[index]
                    index <- index + 1
                | None -> canWrite <- false

            if canWrite then Some state else None

    let private allocateInt32Array
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (values : int32 list)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let int32Handle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Int32

        let arrayAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero int32Handle)
                (fun () -> CliType.Numeric (CliNumericType.Int32 0))
                values.Length
                state

        let state =
            ((state, 0), values)
            ||> List.fold (fun (state, index) value ->
                IlMachineState.setArrayValue arrayAddr (CliType.Numeric (CliNumericType.Int32 value)) index state,
                index + 1
            )
            |> fst

        arrayAddr, state

    let private moduleHandleOfRuntimeModuleRef
        (operation : string)
        (state : IlMachineState)
        (runtimeModuleRef : EvalStackValue)
        : string
        =
        let runtimeModuleAddr =
            match runtimeModuleRef with
            | EvalStackValue.ObjectRef addr -> addr
            | other -> failwith $"%s{operation}: expected ObjectRef for RuntimeModule argument, got %O{other}"

        let heapObj = ManagedHeap.get runtimeModuleAddr state.ManagedHeap

        let pDataField =
            IlMachineState.requiredOwnInstanceFieldId state heapObj.ConcreteType "m_pData"

        match
            AllocatedNonArrayObject.DereferenceFieldById pDataField heapObj
            |> CliType.unwrapPrimitiveLike
        with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ModuleHandle assemblyFullName)) -> assemblyFullName
        | other -> failwith $"%s{operation}: expected ModuleHandle in RuntimeModule.m_pData, got %O{other}"

    let private typeDefinitionNamespace
        (operation : string)
        (state : IlMachineState)
        (assemblyFullName : string)
        (mdToken : int32)
        : string
        =
        let assembly = metadataImportAssembly operation state assemblyFullName

        match MetadataToken.ofInt mdToken with
        | MetadataToken.TypeDefinition typeDefHandle ->
            let mutable typeInfo =
                Unchecked.defaultof<TypeInfo<GenericParamFromMetadata, TypeDefn>>

            if assembly.TypeDefs.TryGetValue (typeDefHandle, &typeInfo) then
                typeInfo.Namespace
            else
                failwith $"%s{operation}: TypeDef token 0x%08x{mdToken} was not present in %s{assemblyFullName}"
        | token ->
            failwith
                $"%s{operation}: expected TypeDef token for MetadataImport.GetNamespace, got %O{token} from 0x%08x{mdToken}"

    let private fieldDefinitionsForTypeDefinition
        (operation : string)
        (assembly : DumpedAssembly)
        (parent : int32)
        : int32 list
        =
        match MetadataToken.ofInt parent with
        | MetadataToken.TypeDefinition typeDefHandle ->
            let mutable typeInfo =
                Unchecked.defaultof<TypeInfo<GenericParamFromMetadata, TypeDefn>>

            if assembly.TypeDefs.TryGetValue (typeDefHandle, &typeInfo) then
                typeInfo.Fields
                |> List.map (fun field -> metadataTokenOfFieldDefinitionHandle field.Handle)
            else
                failwith $"%s{operation}: TypeDef token 0x%08x{parent} was not present in %s{assembly.Name.FullName}"
        | token ->
            failwith
                $"%s{operation}: expected TypeDef parent token for FieldDef enumeration, got %O{token} from 0x%08x{parent}"

    /// The properties declared by the TypeDef named by <paramref name="parent"/>, as raw metadata
    /// tokens, in the order the real runtime returns them.
    ///
    /// Unfiltered and non-transitive: CoreCLR's fallback branch is a plain
    /// <c>EnumInit</c>/<c>EnumNext</c> over that run, so private, static and indexer properties are
    /// all returned, and inherited ones are not — <c>RuntimeType.PopulateProperties</c> applies
    /// binding flags itself and walks the base chain, calling this once per type.
    let private propertyDefinitionsForTypeDefinition
        (operation : string)
        (assembly : DumpedAssembly)
        (parent : int32)
        : int32 list
        =
        match MetadataToken.ofInt parent with
        | MetadataToken.TypeDefinition typeDefHandle ->
            // The parent must be validated here, because an out-of-range handle reaches the
            // reader as `BadImageFormatException: Read out of bounds`, which reads as a corrupt
            // image when the real problem is a token PawPrint should never have minted.
            if not (assembly.TypeDefs.ContainsKey typeDefHandle) then
                failwith $"%s{operation}: TypeDef token 0x%08x{parent} was not present in %s{assembly.Name.FullName}"

            let metadataReader = metadataReaderOf assembly

            // Reads the PropertyMap run (ECMA-335 II.22.35) straight from the metadata;
            // PawPrint keeps no parsed property index.
            (metadataReader.GetTypeDefinition typeDefHandle).GetProperties ()
            |> Seq.map metadataTokenOfPropertyDefinitionHandle
            |> List.ofSeq
        | token ->
            failwith
                $"%s{operation}: expected TypeDef parent token for property enumeration, got %O{token} from 0x%08x{parent}"

    /// One Property row, bounds-checked: an out-of-range handle fails loudly here.
    let private propertyDefinition
        (operation : string)
        (assembly : DumpedAssembly)
        (propertyHandle : System.Reflection.Metadata.PropertyDefinitionHandle)
        : System.Reflection.Metadata.PropertyDefinition
        =
        let metadataReader = metadataReaderOf assembly

        // `MetadataReader` has no total lookup, so the row number is compared against the
        // table's length directly; an out-of-range handle would otherwise reach the reader as
        // `BadImageFormatException: Read out of bounds`.
        let rowNumber =
            System.Reflection.Metadata.Ecma335.MetadataTokens.GetRowNumber (
                System.Reflection.Metadata.PropertyDefinitionHandle.op_Implicit propertyHandle
            )

        let propertyRowCount =
            System.Reflection.Metadata.Ecma335.MetadataReaderExtensions.GetTableRowCount (
                metadataReader,
                System.Reflection.Metadata.Ecma335.TableIndex.Property
            )

        if rowNumber < 1 || rowNumber > propertyRowCount then
            failwith
                $"%s{operation}: PropertyDef token 0x%08x{metadataTokenOfPropertyDefinitionHandle propertyHandle} was not present in %s{assembly.Name.FullName}"

        metadataReader.GetPropertyDefinition propertyHandle

    /// The <c>#Strings</c> entry naming a property definition.
    let private propertyDefinitionName
        (operation : string)
        (assembly : DumpedAssembly)
        (propertyHandle : System.Reflection.Metadata.PropertyDefinitionHandle)
        : string
        =
        (metadataReaderOf assembly).GetString ((propertyDefinition operation assembly propertyHandle).Name)

    /// The Param rows (ECMA-335 II.22.33) owned by the MethodDef named by <paramref name="parent"/>,
    /// as raw metadata tokens, in the order the real runtime returns them.
    ///
    /// Table order, which is not parameter order and not one row per parameter: a Param row's
    /// <c>Sequence</c> column says which parameter it describes (0 being the return value), and a
    /// parameter with no name and no attributes gets no row at all. <c>RuntimeParameterInfo</c>
    /// reconciles both, reading <c>Sequence</c> through <c>GetParamDefProps</c> and filling the gaps
    /// from the method signature.
    let private parameterDefinitionsForMethodDefinition
        (operation : string)
        (assembly : DumpedAssembly)
        (parent : int32)
        : int32 list
        =
        match MetadataToken.ofInt parent with
        | MetadataToken.MethodDef methodDefHandle ->
            // The parent must be validated here, because an out-of-range handle reaches the reader
            // as `BadImageFormatException: Read out of bounds`, which reads as a corrupt image when
            // the real problem is a token PawPrint should never have minted.
            if not (assembly.Methods.ContainsKey methodDefHandle) then
                failwith $"%s{operation}: MethodDef token 0x%08x{parent} was not present in %s{assembly.Name.FullName}"

            let metadataReader = metadataReaderOf assembly

            // Reads the Param run straight from the metadata; PawPrint's `MethodInfo` decodes the
            // signature rather than keeping the Param rows, and the rows are what carries names and
            // `ParameterAttributes`.
            (metadataReader.GetMethodDefinition methodDefHandle).GetParameters ()
            |> Seq.map metadataTokenOfParameterHandle
            |> List.ofSeq
        | token ->
            failwith
                $"%s{operation}: expected MethodDef parent token for parameter enumeration, got %O{token} from 0x%08x{parent}"

    /// One Param row, bounds-checked: an out-of-range handle fails loudly here.
    let private parameterDefinition
        (operation : string)
        (assembly : DumpedAssembly)
        (parameterHandle : System.Reflection.Metadata.ParameterHandle)
        : System.Reflection.Metadata.Parameter
        =
        let metadataReader = metadataReaderOf assembly

        // `MetadataReader` has no total lookup, so the row number is compared against the table's
        // length directly; an out-of-range handle would otherwise reach the reader as
        // `BadImageFormatException: Read out of bounds`. CoreCLR's own `GetParamDefProps` FCall
        // makes exactly this check (`pScope->IsValidToken`, managedmdimport.cpp:305) and reports
        // COR_E_BADIMAGEFORMAT.
        let rowNumber =
            System.Reflection.Metadata.Ecma335.MetadataTokens.GetRowNumber (
                System.Reflection.Metadata.ParameterHandle.op_Implicit parameterHandle
            )

        let parameterRowCount =
            System.Reflection.Metadata.Ecma335.MetadataReaderExtensions.GetTableRowCount (
                metadataReader,
                System.Reflection.Metadata.Ecma335.TableIndex.Param
            )

        if rowNumber < 1 || rowNumber > parameterRowCount then
            failwith
                $"%s{operation}: ParamDef token 0x%08x{metadataTokenOfParameterHandle parameterHandle} was not present in %s{assembly.Name.FullName}"

        metadataReader.GetParameter parameterHandle

    /// The types immediately nested inside the TypeDef named by <paramref name="parent"/>, as raw
    /// metadata tokens, in the order the real runtime returns them.
    ///
    /// CoreCLR answers an <c>mdtTypeDef</c> enumeration by calling <c>GetNestedClasses</c> on the
    /// parent (managedmdimport.cpp:547), not by enumerating TypeDefs in general;
    /// <c>MetadataImport.EnumNestedTypes</c> is its only managed caller. Nesting is *not*
    /// transitive here: a type nested inside a nested type belongs to that inner type's list, and
    /// CoreCLR's single pass over the NestedClass table matches only rows whose EnclosingClass is
    /// exactly this parent.
    let private nestedTypeDefinitionsForTypeDefinition
        (operation : string)
        (assembly : DumpedAssembly)
        (parent : int32)
        : int32 list
        =
        match MetadataToken.ofInt parent with
        | MetadataToken.TypeDefinition typeDefHandle ->
            if typeDefHandle.IsNil then
                // CoreCLR asserts a non-nil parent here, and `RuntimeType.PopulateNestedClasses`
                // returns early on `MdToken.IsNullToken` rather than calling in. Reaching this
                // means a caller skipped that guard, which a silent empty list would hide.
                failwith
                    $"%s{operation}: nil TypeDef parent token 0x%08x{parent} for nested-type enumeration; the caller should have screened this out"

            if not (assembly.TypeDefs.ContainsKey typeDefHandle) then
                failwith $"%s{operation}: TypeDef token 0x%08x{parent} was not present in %s{assembly.Name.FullName}"

            match
                assembly.NestedTypeDefsByEnclosing.TryGetValue (ComparableTypeDefinitionHandle.Make typeDefHandle)
            with
            | true, nested -> nested |> Seq.map metadataTokenOfTypeDefinitionHandle |> List.ofSeq
            // Absent means "no nested types"; the index does not store empty entries.
            | false, _ -> []
        | token ->
            failwith
                $"%s{operation}: expected TypeDef parent token for nested-type enumeration, got %O{token} from 0x%08x{parent}"

    /// The MethodSemantics rows (ECMA-335 II.22.28) associated with one property or event, flattened
    /// into the <c>[method; semantics; method; semantics; …]</c> layout the QCall reports — the one
    /// token type whose INT32 buffer is not a list of tokens. CoreCLR fills the buffer with
    /// <c>ASSOCIATE_RECORD</c>s, which are pairs of INT32s (inc/metadata.h:252-257), and
    /// <c>Associates.AssignAssociates</c> reads them back at <c>[i * 2]</c> and <c>[i * 2 + 1]</c>.
    /// The QCall's contract is the rows themselves, with their real <c>Semantics</c> values —
    /// <c>AssignAssociates</c> switches on the raw value and assigns <c>addOn</c> for an AddOn row
    /// even on a *property*.
    ///
    /// Deliberately a full scan collecting every match, where CoreCLR binary-searches for one row and
    /// expands to the contiguous group around it (metamodel.h:689-755), which is sound only because
    /// ECMA-335 II.24.2.6 requires this table to be sorted by Association — a claim CoreCLR does not
    /// verify. On a conforming image the two agree exactly; on a non-conforming one PawPrint finds
    /// rows CoreCLR's binary search could miss.
    let private methodSemanticsForAssociation
        (operation : string)
        (assembly : DumpedAssembly)
        (parent : int32)
        : int32 list
        =
        let metadataReader = metadataReaderOf assembly

        let rowCount (table : System.Reflection.Metadata.Ecma335.TableIndex) : int =
            System.Reflection.Metadata.Ecma335.MetadataReaderExtensions.GetTableRowCount (metadataReader, table)

        // The parent must exist. CoreCLR would instead report an empty run, because
        // `getAssociatesForToken` cannot tell "no such row" from "a real row with no accessors"; but
        // every token that legitimately arrives here came from `RuntimeType.PopulateProperties` or
        // `PopulateEvents`, so an out-of-range one is a PawPrint bug or a corrupt image, and an
        // empty answer would hand the guest a silently accessor-less property instead.
        let association : System.Reflection.Metadata.EntityHandle =
            match MetadataToken.ofInt parent with
            | MetadataToken.PropertyDefinition propertyHandle ->
                if
                    System.Reflection.Metadata.Ecma335.MetadataTokens.GetRowNumber (
                        System.Reflection.Metadata.PropertyDefinitionHandle.op_Implicit propertyHandle
                    )
                    |> fun row -> row < 1 || row > rowCount System.Reflection.Metadata.Ecma335.TableIndex.Property
                then
                    failwith
                        $"%s{operation}: PropertyDef token 0x%08x{parent} was not present in %s{assembly.Name.FullName}"

                System.Reflection.Metadata.PropertyDefinitionHandle.op_Implicit propertyHandle
            | MetadataToken.EventDefinition eventHandle ->
                if
                    System.Reflection.Metadata.Ecma335.MetadataTokens.GetRowNumber (
                        System.Reflection.Metadata.EventDefinitionHandle.op_Implicit eventHandle
                    )
                    |> fun row -> row < 1 || row > rowCount System.Reflection.Metadata.Ecma335.TableIndex.Event
                then
                    failwith
                        $"%s{operation}: EventDef token 0x%08x{parent} was not present in %s{assembly.Name.FullName}"

                System.Reflection.Metadata.EventDefinitionHandle.op_Implicit eventHandle
            | token ->
                // CoreCLR falls through to the generic `EnumInit`/`EnumNext` path here, which for a
                // TypeDef parent would mean "the methods of this type". No managed caller asks for
                // that: `Associates.AssignAssociates` is the only one that passes mdtMethodDef at all.
                failwith
                    $"%s{operation}: expected Property or Event parent token for associate enumeration, got %O{token} from 0x%08x{parent}"

        // Read straight from the metadata bytes, because `MetadataReader` does not expose the
        // MethodSemantics table at all. It exposes only `PropertyDefinition.GetAccessors` and
        // `EventDefinition.GetAccessors`, which are a *view*: they classify each row into a
        // named slot and drop the row's real `Semantics` value for everything that is not a
        // getter or setter (respectively adder, remover or raiser) — lossy in a way that is
        // visible to a guest.
        //
        // ECMA-335 II.24.2.6 column widths. A simple index is 2 bytes while its target table has
        // fewer than 2^16 rows; a coded index with `n` tag bits is 2 bytes while every table it spans
        // has fewer than 2^(16-n). HasSemantics spans Event and Property with one tag bit.
        let methodIndexSize =
            if rowCount System.Reflection.Metadata.Ecma335.TableIndex.MethodDef < 0x10000 then
                2
            else
                4

        let associationIndexSize =
            let widest =
                max
                    (rowCount System.Reflection.Metadata.Ecma335.TableIndex.Event)
                    (rowCount System.Reflection.Metadata.Ecma335.TableIndex.Property)

            if widest < 0x8000 then 2 else 4

        let rowSize =
            System.Reflection.Metadata.Ecma335.MetadataReaderExtensions.GetTableRowSize (
                metadataReader,
                System.Reflection.Metadata.Ecma335.TableIndex.MethodSemantics
            )

        // Not a proof — a row size of 8 is ambiguous between (2, 4, 2) and (2, 2, 4) — but it does
        // catch a wholesale wrong rule. The Semantics column is a fixed 2-byte constant.
        if rowSize <> 2 + methodIndexSize + associationIndexSize then
            failwith
                $"%s{operation}: MethodSemantics row size %d{rowSize} in %s{assembly.Name.FullName} disagrees with the ECMA-335 II.24.2.6 widths derived from the table row counts (2 + %d{methodIndexSize} + %d{associationIndexSize})"

        // The coded index for the target, compared as an integer against each row's raw
        // Association column. `CodedIndex.HasSemantics` rather than a hand-rolled tag bit,
        // because getting the tag order backwards is the one mistake this encoding admits.
        let target = System.Reflection.Metadata.Ecma335.CodedIndex.HasSemantics association

        let rows = rowCount System.Reflection.Metadata.Ecma335.TableIndex.MethodSemantics

        let tableOffset =
            System.Reflection.Metadata.Ecma335.MetadataReaderExtensions.GetTableMetadataOffset (
                metadataReader,
                System.Reflection.Metadata.Ecma335.TableIndex.MethodSemantics
            )

        // `BlobReader` is a struct: an immutably-bound one is copied before each `Read*` call,
        // so every read would silently return the same bytes. Hence `let mutable`.
        let mutable reader =
            (assembly.PeReader.GetMetadata ()).GetReader (tableOffset, rows * rowSize)

        let associates = ResizeArray ()

        for _ = 1 to rows do
            let semantics = int (reader.ReadUInt16 ())

            let method =
                if methodIndexSize = 2 then
                    int (reader.ReadUInt16 ())
                else
                    reader.ReadInt32 ()

            let rowAssociation =
                if associationIndexSize = 2 then
                    int (reader.ReadUInt16 ())
                else
                    reader.ReadInt32 ()

            if rowAssociation = target then
                associates.Add (metadataTokenTypeMethodDef ||| method)
                associates.Add semantics

        List.ofSeq associates

    let private fieldDefinition
        (operation : string)
        (assembly : DumpedAssembly)
        (mdToken : int32)
        : FieldInfo<GenericParamFromMetadata, TypeDefn>
        =
        match MetadataToken.ofInt mdToken with
        | MetadataToken.FieldDefinition fieldDefHandle ->
            let mutable fieldInfo =
                Unchecked.defaultof<FieldInfo<GenericParamFromMetadata, TypeDefn>>

            if assembly.Fields.TryGetValue (fieldDefHandle, &fieldInfo) then
                fieldInfo
            else
                failwith $"%s{operation}: FieldDef token 0x%08x{mdToken} was not present in %s{assembly.Name.FullName}"
        | token -> failwith $"%s{operation}: expected FieldDef token, got %O{token} from 0x%08x{mdToken}"

    /// The Constant table row (ECMA-335 II.22.9) attached to a field definition: its declared type
    /// code and a reader over its value blob. <c>None</c> when the field has no Constant row.
    ///
    /// Raw, because the two callers disagree about what the answer means.
    /// <c>NativeEnum</c> requires the type code to match the enum's declared underlying type and
    /// fails otherwise, and treats a missing row as a corrupt image;
    /// <c>MetadataImport.GetDefaultValue</c> must do neither, because <c>MdConstant</c> is the thing
    /// that decides what the bytes mean, and a missing row is its ordinary "no default value"
    /// answer.
    let constantRowOfField
        (metadataReader : System.Reflection.Metadata.MetadataReader)
        (fieldHandle : System.Reflection.Metadata.FieldDefinitionHandle)
        : (System.Reflection.Metadata.ConstantTypeCode * System.Reflection.Metadata.BlobReader) option
        =
        let constantHandle =
            (metadataReader.GetFieldDefinition fieldHandle).GetDefaultValue ()

        if constantHandle.IsNil then
            None
        else

        let constant = metadataReader.GetConstant constantHandle
        Some (constant.TypeCode, metadataReader.GetBlobReader constant.Value)

    /// ECMA-335 II.23.1.16 <c>ELEMENT_TYPE_*</c> code for a Constant row's type, paired with the
    /// number of bytes its value blob must contain — CoreCLR's <c>_FillMDDefaultValue</c> checks
    /// exactly this width per code and reports <c>CLDB_E_FILE_CORRUPT</c> when the blob is shorter.
    /// A *longer* blob is tolerated there and only its first <c>width</c> bytes are read, so the
    /// width is a minimum for validation and an exact count for decoding.
    ///
    /// <c>ELEMENT_TYPE_STRING</c> has no fixed width; its blob is however many UTF-16 code units
    /// the string has, so it carries width 0 and is handled separately by the caller.
    let private elementTypeOfConstantTypeCode
        (operation : string)
        (code : System.Reflection.Metadata.ConstantTypeCode)
        : int32 * int
        =
        // `System.Reflection.Metadata`'s `ConstantTypeCode` values happen to be the same
        // numbers as the element types, but this maps them explicitly rather than casting: they
        // are two separate contracts, and a cast would silently follow either one if it moved.
        match code with
        | System.Reflection.Metadata.ConstantTypeCode.Boolean -> 0x02, 1
        | System.Reflection.Metadata.ConstantTypeCode.Char -> 0x03, 2
        | System.Reflection.Metadata.ConstantTypeCode.SByte -> 0x04, 1
        | System.Reflection.Metadata.ConstantTypeCode.Byte -> 0x05, 1
        | System.Reflection.Metadata.ConstantTypeCode.Int16 -> 0x06, 2
        | System.Reflection.Metadata.ConstantTypeCode.UInt16 -> 0x07, 2
        | System.Reflection.Metadata.ConstantTypeCode.Int32 -> 0x08, 4
        | System.Reflection.Metadata.ConstantTypeCode.UInt32 -> 0x09, 4
        | System.Reflection.Metadata.ConstantTypeCode.Int64 -> 0x0A, 8
        | System.Reflection.Metadata.ConstantTypeCode.UInt64 -> 0x0B, 8
        | System.Reflection.Metadata.ConstantTypeCode.Single -> 0x0C, 4
        | System.Reflection.Metadata.ConstantTypeCode.Double -> 0x0D, 8
        | System.Reflection.Metadata.ConstantTypeCode.String -> 0x0E, 0
        | System.Reflection.Metadata.ConstantTypeCode.NullReference -> 0x12, 4
        | code -> failwith $"%s{operation}: Constant row has unrepresentable type code %O{code}"

    /// <c>ELEMENT_TYPE_VOID</c>, which is how CoreCLR reports "this token has no Constant row".
    let private elementTypeVoid : int32 = 0x01

    /// <c>ELEMENT_TYPE_STRING</c>, the one code whose blob is handed back as a pointer rather than
    /// packed into the 64-bit buffer.
    let private elementTypeString : int32 = 0x0E

    /// <c>ELEMENT_TYPE_CLASS</c>, which ECMA-335 II.22.9 permits only as a null reference: exactly
    /// four bytes, all zero.
    let private elementTypeClass : int32 = 0x12

    /// Pack a Constant blob into the low bytes of a 64-bit buffer, little-endian, as
    /// <c>MetaDataImport::GetDefaultValue</c>'s <c>*pDefaultValue = value.m_ullValue</c> does.
    ///
    /// The high bytes are zero *by PawPrint's choice*, not by CoreCLR's: there,
    /// <c>MDDefaultValue</c> is an uninitialised stack union and <c>_FillMDDefaultValue</c> writes
    /// only the member-width low bytes, so the rest is whatever was on the stack. That is
    /// unobservable upstream — <c>MdConstant</c> reinterprets only the low member-width bytes for
    /// every type code, so sign- versus zero-extension cannot be told apart by a guest — but a
    /// replay must not depend on the host's stack, so we pick zeros.
    /// Reads exactly <paramref name="width"/> bytes, which is what CoreCLR does: a blob longer than
    /// its type requires has its tail ignored rather than folded in.
    let private packConstantBuffer (width : int) (bytes : byte array) : int64 =
        let mutable buffer = 0UL

        for i in 0 .. width - 1 do
            buffer <- buffer ||| (uint64 bytes.[i] <<< (8 * i))

        int64 buffer

    /// Walk the assembly's methods to find which one owns <paramref name="paramHandle"/>.
    /// CLI metadata exposes the param->method relation only via per-method ranges, so
    /// answering "who owns this Param row?" requires iterating method definitions.
    let private methodOwningParameter
        (assembly : DumpedAssembly)
        (paramHandle : System.Reflection.Metadata.ParameterHandle)
        : System.Reflection.Metadata.MethodDefinitionHandle option
        =
        let mr = metadataReaderOf assembly

        assembly.Methods.Keys
        |> Seq.tryFind (fun methodHandle ->
            let methodDef = mr.GetMethodDefinition methodHandle
            let parameters = methodDef.GetParameters ()
            let mutable enumerator = parameters.GetEnumerator ()
            let mutable found = false

            while not found && enumerator.MoveNext () do
                if enumerator.Current = paramHandle then
                    found <- true

            found
        )

    /// Walk the assembly's TypeDefs to find which one owns <paramref name="eventHandle"/>.
    let private typeOwningEvent
        (assembly : DumpedAssembly)
        (eventHandle : System.Reflection.Metadata.EventDefinitionHandle)
        : System.Reflection.Metadata.TypeDefinitionHandle option
        =
        let mr = metadataReaderOf assembly

        assembly.TypeDefs.Keys
        |> Seq.tryFind (fun typeHandle ->
            let typeDef = mr.GetTypeDefinition typeHandle
            let events = typeDef.GetEvents ()
            let mutable enumerator = events.GetEnumerator ()
            let mutable found = false

            while not found && enumerator.MoveNext () do
                if enumerator.Current = eventHandle then
                    found <- true

            found
        )

    /// Walk the assembly's TypeDefs to find which one owns <paramref name="propertyHandle"/>.
    let private typeOwningProperty
        (assembly : DumpedAssembly)
        (propertyHandle : System.Reflection.Metadata.PropertyDefinitionHandle)
        : System.Reflection.Metadata.TypeDefinitionHandle option
        =
        let mr = metadataReaderOf assembly

        assembly.TypeDefs.Keys
        |> Seq.tryFind (fun typeHandle ->
            let typeDef = mr.GetTypeDefinition typeHandle
            let properties = typeDef.GetProperties ()
            let mutable enumerator = properties.GetEnumerator ()
            let mutable found = false

            while not found && enumerator.MoveNext () do
                if enumerator.Current = propertyHandle then
                    found <- true

            found
        )

    /// Resolve the <c>System.Reflection.ConstArray</c> TypeInfo from the loaded corelib.
    /// <c>ConstArray</c> is the shape returned by several <c>MetadataImport</c> InternalCalls
    /// (<c>GetCustomAttributeProps</c>, <c>GetMemberRefProps</c>, <c>GetSigOfMethodDef</c>, …).
    let private constArrayTypeInfo
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        match baseClassTypes.Corelib.TryGetTopLevelTypeDef "System.Reflection" "ConstArray" with
        | Some ty -> ty
        | None ->
            failwith
                $"%s{operation}: System.Reflection.ConstArray was not found in corelib %s{baseClassTypes.Corelib.Name.FullName}"

    /// Concretize <c>System.Reflection.ConstArray</c> and return its handle (cached implicitly via
    /// <c>AllConcreteTypes</c>).
    let private concretizeConstArray
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        : IlMachineState * ConcreteTypeHandle * TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        let typeInfo = constArrayTypeInfo operation baseClassTypes

        let state, handle =
            TypeDefn.FromDefinition (
                ResolvedTypeIdentity.ofTypeDefinition baseClassTypes.Corelib.Name typeInfo.TypeDefHandle,
                System.Reflection.Metadata.SignatureTypeKind.ValueType
            )
            |> IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty

        state, handle, typeInfo

    /// Build a <c>System.Reflection.ConstArray</c> value with the given <c>m_length</c> and
    /// <c>m_constArray</c>. The caller decides what the pointer addresses; this only assembles
    /// the struct and checks that corelib's field signatures are still what we assume.
    let private constArrayOfPointer
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (length : int)
        (pointerValue : CliType)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let state, constArrayHandle, typeInfo =
            concretizeConstArray loggerFactory baseClassTypes operation state

        let lengthFieldInfo = FieldIdentity.requiredOwnInstanceField typeInfo "m_length"

        let pointerFieldInfo =
            FieldIdentity.requiredOwnInstanceField typeInfo "m_constArray"

        match lengthFieldInfo.Signature with
        | TypeDefn.PrimitiveType PrimitiveType.Int32 -> ()
        | s -> failwith $"%s{operation}: ConstArray.m_length had unexpected signature %O{s}"

        match pointerFieldInfo.Signature with
        | TypeDefn.PrimitiveType PrimitiveType.IntPtr -> ()
        | s -> failwith $"%s{operation}: ConstArray.m_constArray had unexpected signature %O{s}"

        let int32Handle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Int32

        let intPtrHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.IntPtr

        let lengthField =
            FieldIdentity.cliField
                constArrayHandle
                lengthFieldInfo
                (CliType.Numeric (CliNumericType.Int32 length))
                int32Handle

        let pointerField =
            FieldIdentity.cliField constArrayHandle pointerFieldInfo pointerValue intPtrHandle

        let valueType =
            [ lengthField ; pointerField ]
            |> CliValueType.OfFields
                baseClassTypes
                state.ConcreteTypes
                constArrayHandle
                (DeclaredTypeFacts.ofTypeInfo baseClassTypes state._LoadedAssemblies typeInfo)
            |> CliType.ValueType

        valueType, state

    /// Build a <c>System.Reflection.ConstArray</c> value with <c>m_length = blob.Length</c> and
    /// <c>m_constArray</c> pointing at the first byte of <paramref name="blob"/>. Allocates the
    /// backing managed <c>byte[]</c> on the heap.
    let private buildConstArray
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (blob : ImmutableArray<byte>)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let storage : byte array = Array.init blob.Length (fun i -> blob.[i])

        let pointerValue, state =
            if storage.Length = 0 then
                CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null), state
            else
                let bytePtr, state = NativeCall.allocateBlobByteArray baseClassTypes storage state
                CliType.RuntimePointer (CliRuntimePointer.Managed bytePtr), state

        constArrayOfPointer loggerFactory baseClassTypes operation storage.Length pointerValue state

    /// Build a <c>System.Reflection.ConstArray</c> whose <c>m_constArray</c> addresses the metadata
    /// bytes themselves rather than a copy of them.
    ///
    /// CoreCLR's <c>ConstArray</c>-returning imports hand back a <c>PCCOR_SIGNATURE</c> straight
    /// into the mapped metadata, so a PE byte range is the faithful model: it keeps the blob's
    /// provenance (which FieldDef in which assembly), which is what lets a later consumer such as
    /// <c>NativeSignature.resolveSignatureBlobHandle</c> recover the definition instead of
    /// re-parsing anonymous bytes, and it makes the read-only-ness of a <c>ConstArray</c> a
    /// machine-checked fact — <c>IlMachineManagedByref</c> refuses writes through a
    /// <c>PeByteRange</c> root.
    ///
    /// The sibling handlers that copy (<c>GetSigOfMethodDef</c>, <c>GetMemberRefProps</c>,
    /// <c>GetCustomAttributeProps</c>) have no consumer that needs that provenance, and MemberRef
    /// and CustomAttribute blobs have no <c>PeByteRangePointerSource</c> variant at all.
    let private buildConstArrayOverPeByteRange
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (peByteRange : PeByteRangePointer)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let state, pointer =
            IlMachineState.peByteRangePointer loggerFactory baseClassTypes peByteRange state

        constArrayOfPointer
            loggerFactory
            baseClassTypes
            operation
            peByteRange.Size
            (CliType.RuntimePointer (CliRuntimePointer.Managed pointer))
            state

    /// Finish a <c>MetadataImport</c> call whose contract is "write an <c>LPCSTR</c> through an out
    /// parameter and return an HRESULT": allocate the null-terminated UTF-8 bytes, point the out
    /// parameter at them, and push <c>S_OK</c>.
    ///
    /// CoreCLR returns a pointer straight into the mapped <c>#Strings</c> heap, so it hands back the
    /// same address for the same token every time and never allocates. PawPrint copies instead,
    /// which is safe only because these strings are consumed purely as bytes: the caller wraps the
    /// pointer in <c>MdUtf8String</c>, which compares contents (<c>SequenceEqual</c>) or decodes
    /// them, and nothing anywhere compares the pointer itself.
    let private completeWithUtf8String
        (ctx : NativeCallContext)
        (out : ManagedPointerSource)
        (value : string)
        (state : IlMachineState)
        : NativeHandlerResult option
        =
        // The copy must be null-terminated because `MdUtf8String`'s constructor measures it
        // with `string.strlen` — `SpanHelpers.IndexOfNullByte`, which deliberately over-reads
        // in its vector paths. Those are all behind `VectorNNN.IsHardwareAccelerated`, and
        // PawPrint only ever reports `HardwareIntrinsicsProfile.ScalarOnly`, so what runs is
        // the scalar loop: one byte at a time, stopping at the first zero, never reading past
        // the terminator. Giving the guest an accelerated profile would invalidate that, and
        // this buffer would need to be padded to a vector width.
        let ptr, state =
            NativeCall.allocateNullTerminatedUtf8 ctx.BaseClassTypes value state

        let state =
            IlMachineState.writeManagedByrefWithBase
                ctx.BaseClassTypes
                state
                out
                (CliType.RuntimePointer (CliRuntimePointer.Managed ptr))

        // S_OK; the managed wrapper runs every HRESULT through `ThrowBadImageExceptionForHR`.
        let state =
            IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) ctx.Thread state

        NativeHandlerResult.completed state |> Some

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        // The executing-method name is deliberately omitted from the match. CoreLib declares
        // `MetadataImport.Enum` with `[LibraryImport(RuntimeHelpers.QCall, EntryPoint =
        // "MetadataImport_Enum")]`, so Roslyn emits a marshalling stub whose synthesised
        // name (`<Enum>g____PInvoke|N_M`) carries source-generator counters that drift
        // whenever neighbouring members are reordered. The entry-point name plus the
        // parameter/return signature shape are stable and disambiguate the QCall on their
        // own. Same approach as `NativeCustomAttribute.tryExecuteQCall`.
        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "MetadataImport_Enum",
          "System.Private.CoreLib",
          "System.Reflection",
          "MetadataImport",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              objectHandleGenerics) ],
          MethodReturnType.Void when objectHandleGenerics.IsEmpty ->
            let operation = "MetadataImport.Enum"
            let assemblyFullName = metadataImportHandleOfArg operation instruction.Arguments.[0]

            let assembly = metadataImportAssembly operation state assemblyFullName

            let tokenType =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                | CliType.Numeric (CliNumericType.Int32 tokenType) -> tokenType
                | other -> failwith $"%s{operation}: expected Int32 token type argument, got %O{other}"

            let parent =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[2] with
                | CliType.Numeric (CliNumericType.Int32 parent) -> parent
                | other -> failwith $"%s{operation}: expected Int32 parent token argument, got %O{other}"

            let values =
                if tokenType = metadataTokenTypeExportedType && parent = 0 then
                    []
                elif tokenType = metadataTokenTypeCustomAttribute then
                    match assembly.CustomAttributesByParentToken.TryGetValue parent with
                    | true, tokens -> tokens |> Seq.toList
                    | false, _ -> []
                elif tokenType = metadataTokenTypeFieldDef then
                    fieldDefinitionsForTypeDefinition operation assembly parent
                elif tokenType = metadataTokenTypeTypeDef then
                    nestedTypeDefinitionsForTypeDefinition operation assembly parent
                elif tokenType = metadataTokenTypeProperty then
                    propertyDefinitionsForTypeDefinition operation assembly parent
                elif tokenType = metadataTokenTypeParamDef then
                    parameterDefinitionsForMethodDefinition operation assembly parent
                elif tokenType = metadataTokenTypeMethodDef then
                    // The one branch whose result is not a token list: these are ASSOCIATE_RECORD
                    // pairs, `[method; semantics; …]`, and `*length` counts INT32s rather than
                    // records. Everything downstream — the short buffer, the large-result escape
                    // hatch, `*length` — is indifferent to that, because it only ever moves INT32s.
                    methodSemanticsForAssociation operation assembly parent
                else
                    failwith
                        $"TODO: %s{operation} does not yet support token type 0x%08x{tokenType} with parent 0x%08x{parent}"

            let lengthOut =
                NativeCall.managedPointerOfPointerArgument operation "length" instruction.Arguments.[3]

            let state =
                if values.IsEmpty then
                    state
                else
                    let shortResult =
                        NativeCall.managedPointerOfPointerArgument operation "shortResult" instruction.Arguments.[4]

                    match tryWriteSmallInt32Buffer operation ctx.BaseClassTypes state shortResult values with
                    | Some state -> state
                    | None ->
                        // Some fixed inline-array byrefs are not yet addressable as Int32 spans in PawPrint.
                        // The CoreLib wrapper checks _largeResult before reading _smallResult, so using the
                        // large-result escape hatch preserves the managed contract for those shapes.
                        let longResult =
                            NativeCall.objectHandleOnStackTarget operation state "longResult" instruction.Arguments.[5]

                        let resultArrayAddr, state = allocateInt32Array ctx.BaseClassTypes values state

                        IlMachineState.writeManagedByrefWithBase
                            ctx.BaseClassTypes
                            state
                            longResult
                            (CliType.ObjectRef (Some resultArrayAddr))

            let state = writeInt32AtPointer ctx.BaseClassTypes state lengthOut values.Length

            NativeHandlerResult.completed state |> Some
        | _ -> None

    let tryExecute (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "System.Private.CoreLib",
          "System.Reflection",
          "MetadataImport",
          "GetMetadataImport",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Reflection",
                                              "RuntimeModule",
                                              runtimeModuleGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr) when
            runtimeModuleGenerics.IsEmpty
            ->
            let operation = "MetadataImport.GetMetadataImport"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeModuleRef, state = IlMachineState.popEvalStack ctx.Thread state

            let assemblyFullName =
                moduleHandleOfRuntimeModuleRef operation state runtimeModuleRef

            // CoreCLR returns an IMDInternalImport pointer distinct from RuntimeModule.m_pData.
            // PawPrint preserves that handle-domain split while using the same module identity payload.
            let state =
                IlMachineState.pushToEvalStack'
                    (EvalStackValue.NativeInt (NativeIntSource.MetadataImportHandle assemblyFullName))
                    ctx.Thread
                    state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System.Reflection",
          "MetadataImport",
          "GetNamespace",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteByref (ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "MetadataImport.GetNamespace"
            let assemblyFullName = metadataImportHandleOfArg operation instruction.Arguments.[0]

            let mdToken =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                | CliType.Numeric (CliNumericType.Int32 mdToken) -> mdToken
                | other -> failwith $"%s{operation}: expected Int32 mdToken argument, got %O{other}"

            let namespaceOut =
                NativeCall.managedPointerOfPointerArgument operation "namespace out pointer" instruction.Arguments.[2]

            let namespaceName = typeDefinitionNamespace operation state assemblyFullName mdToken

            completeWithUtf8String ctx namespaceOut namespaceName state
        | "System.Private.CoreLib",
          "System.Reflection",
          "MetadataImport",
          "GetName",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteByref (ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // CoreCLR's FCall (`managedmdimport.cpp:204`) answers seven token kinds, forwarding each
            // to a different `IMDInternalImport` accessor. This answers three.
            //
            // MethodDef, `mdtModule` and TypeDef have no managed caller at all: `RuntimeType.Name`
            // goes through `Cache.GetName()`/`ConstructName`, and methods and non-literal fields
            // have their own `RuntimeMethodHandle.GetName` / `RuntimeFieldHandle.GetName` QCalls.
            //
            // Event *does* have a caller — the name filter in `RuntimeType.PopulateEvents` — but its
            // tokens are minted only by `MetadataImport.Enum`, which PawPrint refuses for
            // `mdtEvent`. Adding that enumeration means adding the matching arm here in the same
            // change.
            //
            // Any other kind reaching here is therefore a PawPrint gap rather than a bad image.
            // CoreCLR would instead return `E_FAIL` and the guest would see a
            // `BadImageFormatException`, which would disguise the gap as a corrupt assembly.
            let operation = "MetadataImport.GetName"
            let assemblyFullName = metadataImportHandleOfArg operation instruction.Arguments.[0]
            let assembly = metadataImportAssembly operation state assemblyFullName

            let mdToken =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                | CliType.Numeric (CliNumericType.Int32 mdToken) -> mdToken
                | other -> failwith $"%s{operation}: expected Int32 mdToken argument, got %O{other}"

            let nameOut =
                NativeCall.managedPointerOfPointerArgument operation "name out pointer" instruction.Arguments.[2]

            // All three are `mr.GetString def.Name`, i.e. the `#Strings` entry itself, so these are
            // the same strings CoreCLR's `GetNameOfFieldDef`/`GetNameOfProperty`/`GetParamDefProps`
            // return rather than reconstructions. That matters for an indexer, whose metadata name
            // (`Item`, or whatever `[IndexerName]` says) is not its C# spelling.
            let name =
                match MetadataToken.ofInt mdToken with
                | MetadataToken.PropertyDefinition propertyHandle ->
                    propertyDefinitionName operation assembly propertyHandle
                | MetadataToken.FieldDefinition _ -> (fieldDefinition operation assembly mdToken).Name
                | MetadataToken.Parameter parameterHandle ->
                    // A Param row may carry a nil Name (ECMA-335 II.22.33 makes the column
                    // optional), and both runtimes report that as the empty string rather than as
                    // absent: CoreCLR hands back a pointer to offset 0 of `#Strings`, and
                    // `MetadataReader.GetString` of a nil handle is `""`.
                    (metadataReaderOf assembly).GetString (parameterDefinition operation assembly parameterHandle).Name
                | token ->
                    failwith
                        $"%s{operation}: expected FieldDef, PropertyDef or ParamDef token, got %O{token} from 0x%08x{mdToken}"

            completeWithUtf8String ctx nameOut name state
        | "System.Private.CoreLib",
          "System.Reflection",
          "MetadataImport",
          "GetFieldDefProps",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "MetadataImport.GetFieldDefProps"
            let assemblyFullName = metadataImportHandleOfArg operation instruction.Arguments.[0]
            let assembly = metadataImportAssembly operation state assemblyFullName

            let mdToken =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                | CliType.Numeric (CliNumericType.Int32 mdToken) -> mdToken
                | other -> failwith $"%s{operation}: expected Int32 mdToken argument, got %O{other}"

            let attributesOut =
                NativeCall.managedPointerOfPointerArgument
                    operation
                    "fieldAttributes out pointer"
                    instruction.Arguments.[2]

            let field = fieldDefinition operation assembly mdToken

            let state =
                writeInt32AtPointer ctx.BaseClassTypes state attributesOut (int32 field.Attributes)

            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System.Reflection",
          "MetadataImport",
          "GetDefaultValue",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int64)
            ConcreteByref (ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Char))
            ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32)
            ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // CoreCLR's FCall (managedmdimport.cpp:80) unpacks one `MDDefaultValue`: a string
            // constant reports its blob as a `char*` with a length in *characters*, everything else
            // packs into the 64-bit buffer with a length in *bytes*, and a token with no Constant
            // row reports ELEMENT_TYPE_VOID, which `MdConstant` turns into DBNull.Value.
            //
            // Only FieldDef parents are covered. The Constant table's Parent is a HasConstant coded
            // index spanning ParamDef and PropertyDef too.
            //
            // A ParamDef token reaches here whenever a guest asks a `ParameterInfo` for
            // `HasDefaultValue`, `DefaultValue` or `RawDefaultValue`, all of which funnel into
            // `RuntimeParameterInfo.TryGetDefaultValueInternal`. That is unimplemented: the token
            // is rejected below with a host-level crash rather than silently reported as having no
            // Constant row, which would make an optional parameter look mandatory.
            //
            // A PropertyDef Constant row is read by `RuntimePropertyInfo.GetRawConstantValue`, which
            // needs a fully constructed `RuntimePropertyInfo` — so what blocks it is
            // `RuntimeMethodHandle.GetSlot`, which `RuntimeType.PopulateProperties` needs for its
            // vtable-slot duplicate check, and not anything this file is missing.
            let operation = "MetadataImport.GetDefaultValue"
            let assemblyFullName = metadataImportHandleOfArg operation instruction.Arguments.[0]
            let assembly = metadataImportAssembly operation state assemblyFullName

            let mdToken =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                | CliType.Numeric (CliNumericType.Int32 mdToken) -> mdToken
                | other -> failwith $"%s{operation}: expected Int32 mdToken argument, got %O{other}"

            let valueOut =
                NativeCall.managedPointerOfPointerArgument operation "value out pointer" instruction.Arguments.[2]

            let stringValueOut =
                NativeCall.managedPointerOfPointerArgument
                    operation
                    "stringMetadataEncoding out pointer"
                    instruction.Arguments.[3]

            let lengthOut =
                NativeCall.managedPointerOfPointerArgument operation "length out pointer" instruction.Arguments.[4]

            let corElementTypeOut =
                NativeCall.managedPointerOfPointerArgument
                    operation
                    "corElementType out pointer"
                    instruction.Arguments.[5]

            // Rejects a non-FieldDef token, and a FieldDef absent from this assembly.
            let field = fieldDefinition operation assembly mdToken
            let mr = metadataReaderOf assembly

            let writeInt64 (state : IlMachineState) (value : int64) : IlMachineState =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    valueOut
                    (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim value)))

            let writeStringPointer (state : IlMachineState) (pointer : ManagedPointerSource) : IlMachineState =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    stringValueOut
                    (CliType.RuntimePointer (CliRuntimePointer.Managed pointer))

            let state, value, stringPointer, length, corElementType =
                match constantRowOfField mr field.Handle with
                | None ->
                    // CoreCLR returns here having set only `m_bType`, so its buffer and length are
                    // whatever was on the stack. `MdConstant` looks at nothing but the element type
                    // in this case, but a replay must not depend on the host's stack, so pick zeros.
                    // (Its `*pStringValue = NULL` *is* written, unconditionally, by the FCall.)
                    state, 0L, ManagedPointerSource.Null, 0, elementTypeVoid
                | Some (typeCode, blobReader) ->

                let elementType, requiredWidth = elementTypeOfConstantTypeCode operation typeCode
                let mutable reader = blobReader
                let bytes = reader.ReadBytes reader.Length

                // `_FillMDDefaultValue` bounds-checks every fixed-width code and reports
                // CLDB_E_FILE_CORRUPT on a short blob, so zero-padding one would fabricate a value
                // the real runtime refuses to produce. STRING is exempt: its width is whatever the
                // string is.
                if bytes.Length < requiredWidth then
                    failwith
                        $"%s{operation}: Constant blob for %O{field.Handle} has %d{bytes.Length} bytes but element type 0x%02x{elementType} requires %d{requiredWidth}; CoreCLR reports CLDB_E_FILE_CORRUPT for this"

                if elementType = elementTypeString then
                    // An odd-length blob is *not* rejected. `_FillMDDefaultValue` applies no length
                    // check to STRING, and the FCall's `m_cbSize / sizeof(WCHAR)` is integer
                    // division, so CoreCLR silently drops a trailing half code unit. Refusing here
                    // would make PawPrint decline metadata the real runtime reads without complaint.
                    if bytes.Length = 0 then
                        // `_FillMDDefaultValue` nulls the pointer for an empty string blob
                        // (mdinternalro.cpp:3214), and the managed wrapper's `stringVal ?? string.Empty`
                        // is what recovers `""`. Pointing at a zero-length range instead would make
                        // the wrapper build a string from a pointer that addresses nothing.
                        state, 0L, ManagedPointerSource.Null, 0, elementType
                    else

                    let peByteRange =
                        IlMachineState.peByteRangeForConstantBlob assembly field.Handle
                        |> Option.defaultWith (fun () ->
                            failwith $"%s{operation}: Constant row for %O{field.Handle} vanished between reads"
                        )

                    let state, pointer =
                        IlMachineState.peByteRangeCharPointer ctx.LoggerFactory ctx.BaseClassTypes peByteRange state

                    // Length is in characters here and in bytes everywhere else — the FCall divides
                    // by sizeof(WCHAR) only on this branch.
                    state, 0L, pointer, bytes.Length / 2, elementType
                else if

                    elementType = elementTypeClass
                then
                    // ECMA-335 II.22.9: a CLASS constant is a null reference, and CoreCLR asserts
                    // the four bytes it reads are zero before reporting CLDB_E_FILE_CORRUPT for a
                    // non-null one. The width check above has already rejected a short blob.
                    if packConstantBuffer 4 bytes <> 0L then
                        failwith
                            $"%s{operation}: ELEMENT_TYPE_CLASS Constant blob for %O{field.Handle} must be a null reference, got %A{bytes}"

                    state, 0L, ManagedPointerSource.Null, bytes.Length, elementType
                else

                state, packConstantBuffer requiredWidth bytes, ManagedPointerSource.Null, bytes.Length, elementType

            let state = writeInt64 state value
            let state = writeStringPointer state stringPointer
            let state = writeInt32AtPointer ctx.BaseClassTypes state lengthOut length

            let state =
                writeInt32AtPointer ctx.BaseClassTypes state corElementTypeOut corElementType

            // The managed wrapper turns a negative HRESULT into BadImageFormatException, but as in
            // every sibling handler the failures above are host-level crashes: the only guest caller
            // passes a token the runtime itself minted.
            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System.Reflection",
          "MetadataImport",
          "GetCustomAttributeProps",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32)
            ConcreteByref (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                             "System.Reflection",
                                                             "ConstArray",
                                                             constArrayGenerics)) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            constArrayGenerics.IsEmpty
            ->
            let operation = "MetadataImport.GetCustomAttributeProps"
            let assemblyFullName = metadataImportHandleOfArg operation instruction.Arguments.[0]
            let assembly = metadataImportAssembly operation state assemblyFullName

            let mdToken =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                | CliType.Numeric (CliNumericType.Int32 mdToken) -> mdToken
                | other -> failwith $"%s{operation}: expected Int32 customAttributeToken argument, got %O{other}"

            let ctorTokenOut =
                NativeCall.managedPointerOfPointerArgument
                    operation
                    "constructorToken out pointer"
                    instruction.Arguments.[2]

            let signatureOut =
                NativeCall.managedPointerOfPointerArgument operation "signature out pointer" instruction.Arguments.[3]

            let attrHandle =
                match MetadataToken.ofInt mdToken with
                | MetadataToken.CustomAttribute h -> h
                | token -> failwith $"%s{operation}: expected CustomAttribute token, got %O{token} from 0x%08x{mdToken}"

            let mutable attr = Unchecked.defaultof<WoofWare.PawPrint.CustomAttribute>

            let attr =
                if assembly.Attributes.TryGetValue (attrHandle, &attr) then
                    attr
                else
                    failwith
                        $"%s{operation}: CustomAttribute token 0x%08x{mdToken} was not present in %s{assemblyFullName}"

            let state =
                writeInt32AtPointer ctx.BaseClassTypes state ctorTokenOut (MetadataToken.toInt attr.Constructor)

            let constArrayValue, state =
                buildConstArray ctx.LoggerFactory ctx.BaseClassTypes operation attr.Value state

            let state =
                IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state signatureOut constArrayValue

            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System.Reflection",
          "MetadataImport",
          "GetSigOfMethodDef",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteByref (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                             "System.Reflection",
                                                             "ConstArray",
                                                             constArrayGenerics)) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            constArrayGenerics.IsEmpty
            ->
            let operation = "MetadataImport.GetSigOfMethodDef"
            let assemblyFullName = metadataImportHandleOfArg operation instruction.Arguments.[0]
            let assembly = metadataImportAssembly operation state assemblyFullName

            let mdToken =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                | CliType.Numeric (CliNumericType.Int32 mdToken) -> mdToken
                | other -> failwith $"%s{operation}: expected Int32 methodToken argument, got %O{other}"

            let signatureOut =
                NativeCall.managedPointerOfPointerArgument operation "signature out pointer" instruction.Arguments.[2]

            let methodDefHandle =
                match MetadataToken.ofInt mdToken with
                | MetadataToken.MethodDef h -> h
                | token -> failwith $"%s{operation}: expected MethodDef token, got %O{token} from 0x%08x{mdToken}"

            let mutable methodInfo =
                Unchecked.defaultof<MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>>

            if not (assembly.Methods.TryGetValue (methodDefHandle, &methodInfo)) then
                failwith $"%s{operation}: MethodDef token 0x%08x{mdToken} was not present in %s{assemblyFullName}"

            // The MetadataImport.GetSigOfMethodDef contract is "raw signature blob bytes
            // for the supplied MethodDef token". PawPrint's MethodInfo decodes the signature
            // eagerly; the unparsed blob is recovered on demand from the metadata reader.
            let mr = metadataReaderOf assembly
            let methodDef = mr.GetMethodDefinition methodDefHandle
            let blob = ImmutableArray.Create<byte> (mr.GetBlobBytes methodDef.Signature)

            let constArrayValue, state =
                buildConstArray ctx.LoggerFactory ctx.BaseClassTypes operation blob state

            let state =
                IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state signatureOut constArrayValue

            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System.Reflection",
          "MetadataImport",
          "GetSigOfFieldDef",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteByref (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                             "System.Reflection",
                                                             "ConstArray",
                                                             constArrayGenerics)) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            constArrayGenerics.IsEmpty
            ->
            // CoreCLR's FCall forwards to `IMDInternalImport::GetSigOfFieldDef`
            // (managedmdimport.cpp:372), so despite the `fieldMarshal` parameter name this returns
            // the FieldDef's *signature* blob; `GetFieldMarshal` is the separate call for
            // marshalling info. The sole managed caller is `MdFieldInfo.FieldType`, which exists
            // only for literal fields (a literal has no FieldDesc, so `PopulateLiteralFields`
            // reflects over it from tokens alone).
            let operation = "MetadataImport.GetSigOfFieldDef"
            let assemblyFullName = metadataImportHandleOfArg operation instruction.Arguments.[0]
            let assembly = metadataImportAssembly operation state assemblyFullName

            let mdToken =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                | CliType.Numeric (CliNumericType.Int32 mdToken) -> mdToken
                | other -> failwith $"%s{operation}: expected Int32 fieldToken argument, got %O{other}"

            let signatureOut =
                NativeCall.managedPointerOfPointerArgument operation "signature out pointer" instruction.Arguments.[2]

            // Rejects a non-FieldDef token, and a FieldDef absent from this assembly.
            let fieldInfo = fieldDefinition operation assembly mdToken

            let peByteRange =
                IlMachineState.peByteRangeForFieldSignatureBlob assembly fieldInfo.Handle

            // ECMA-335 II.23.2.4: a FIELD signature is the 0x06 calling-convention byte followed by
            // a non-empty Type, so it is never shorter than two bytes. Anything shorter means we
            // resolved the wrong blob, and passing it on would leave the managed parser to fail
            // somewhere far from the cause.
            if peByteRange.Size < 2 then
                failwith
                    $"%s{operation}: FieldDef token 0x%08x{mdToken} in %s{assemblyFullName} has a %d{peByteRange.Size}-byte signature blob, but an ECMA-335 II.23.2.4 FIELD signature is at least two bytes"

            let constArrayValue, state =
                buildConstArrayOverPeByteRange ctx.LoggerFactory ctx.BaseClassTypes operation peByteRange state

            let state =
                IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state signatureOut constArrayValue

            // The managed wrapper turns a negative HRESULT into BadImageFormatException
            // (MdImport.cs, ThrowBadImageExceptionForHR). Every failure above is instead a host-level
            // crash, as in the sibling handlers: the only guest caller passes tokens the runtime
            // itself minted, so a rejected token is a PawPrint bug rather than a malformed image.
            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System.Reflection",
          "MetadataImport",
          "GetPropertyProps",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteByref (ConcretePointer (ConcreteVoid state.ConcreteTypes))
            ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32)
            ConcreteByref (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                             "System.Reflection",
                                                             "ConstArray",
                                                             constArrayGenerics)) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            constArrayGenerics.IsEmpty
            ->
            // CoreCLR's FCall (managedmdimport.cpp:330) forwards straight to
            // `IMDInternalImport::GetPropertyProps`, which reads one Property row and reports three
            // things: the `#Strings` name, the raw `Property.Flags` column, and a
            // `PCCOR_SIGNATURE`/length pair over the row's Type blob (mdinternalro.cpp:2329). No
            // filtering, no base-chain walk, no associates — `RuntimePropertyInfo`'s constructor
            // does that separately through `Associates.AssignAssociates`.
            let operation = "MetadataImport.GetPropertyProps"
            let assemblyFullName = metadataImportHandleOfArg operation instruction.Arguments.[0]
            let assembly = metadataImportAssembly operation state assemblyFullName

            let mdToken =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                | CliType.Numeric (CliNumericType.Int32 mdToken) -> mdToken
                | other -> failwith $"%s{operation}: expected Int32 mdToken argument, got %O{other}"

            let nameOut =
                NativeCall.managedPointerOfPointerArgument operation "name out pointer" instruction.Arguments.[2]

            let attributesOut =
                NativeCall.managedPointerOfPointerArgument
                    operation
                    "propertyAttributes out pointer"
                    instruction.Arguments.[3]

            let signatureOut =
                NativeCall.managedPointerOfPointerArgument operation "signature out pointer" instruction.Arguments.[4]

            let propertyHandle =
                match MetadataToken.ofInt mdToken with
                | MetadataToken.PropertyDefinition propertyHandle -> propertyHandle
                | token -> failwith $"%s{operation}: expected PropertyDef token, got %O{token} from 0x%08x{mdToken}"

            // Rejects a PropertyDef absent from this assembly, so everything below reads a row that
            // exists.
            let property = propertyDefinition operation assembly propertyHandle

            let peByteRange =
                IlMachineState.peByteRangeForPropertySignatureBlob assembly propertyHandle

            // ECMA-335 II.23.2.5: a PropertySig is the calling-convention byte, then a compressed
            // ParamCount, then a non-empty Type — so it is never shorter than three bytes. Anything
            // shorter means we resolved the wrong blob, and passing it on would leave the managed
            // parser to fail somewhere far from the cause. Same reasoning as the two-byte floor in
            // `GetSigOfFieldDef`.
            if peByteRange.Size < 3 then
                failwith
                    $"%s{operation}: PropertyDef token 0x%08x{mdToken} in %s{assemblyFullName} has a %d{peByteRange.Size}-byte signature blob, but an ECMA-335 II.23.2.5 PropertySig is at least three bytes"

            // A PE byte range rather than a copy, unlike the `GetMemberRefProps` sibling. The blob's
            // only consumer is `RuntimePropertyInfo.Signature`, which passes it to the handle-less
            // `Signature` constructor; PawPrint resolves that through
            // `NativeSignature.corSigPeByteRange`, which accepts only null or a `PeByteRange` — and
            // parsing the blob will need the provenance anyway, because a custom modifier in a
            // property signature carries a coded token that only means something against the
            // owning assembly.
            let constArrayValue, state =
                buildConstArrayOverPeByteRange ctx.LoggerFactory ctx.BaseClassTypes operation peByteRange state

            let state =
                IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state signatureOut constArrayValue

            // The raw Property.Flags column, as `getPropFlagsOfProperty` returns it; the managed
            // wrapper casts it to `PropertyAttributes`.
            let state =
                writeInt32AtPointer ctx.BaseClassTypes state attributesOut (int32 property.Attributes)

            // The name is the `#Strings` entry itself, as `getNameOfProperty` hands back, so an
            // indexer reports its metadata name (`Item`, or whatever `[IndexerName]` says) rather
            // than its C# spelling.
            //
            // Every failure above is instead a host-level crash rather than the negative HRESULT
            // CoreCLR would return, as in the sibling handlers: the only guest caller passes tokens
            // the runtime itself minted, so a rejected token is a PawPrint bug rather than a
            // malformed image.
            completeWithUtf8String ctx nameOut ((metadataReaderOf assembly).GetString property.Name) state
        | "System.Private.CoreLib",
          "System.Reflection",
          "MetadataImport",
          "GetMemberRefProps",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteByref (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                             "System.Reflection",
                                                             "ConstArray",
                                                             constArrayGenerics)) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            constArrayGenerics.IsEmpty
            ->
            let operation = "MetadataImport.GetMemberRefProps"
            let assemblyFullName = metadataImportHandleOfArg operation instruction.Arguments.[0]
            let assembly = metadataImportAssembly operation state assemblyFullName

            let mdToken =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                | CliType.Numeric (CliNumericType.Int32 mdToken) -> mdToken
                | other -> failwith $"%s{operation}: expected Int32 memberTokenRef argument, got %O{other}"

            let signatureOut =
                NativeCall.managedPointerOfPointerArgument operation "signature out pointer" instruction.Arguments.[2]

            let memberRefHandle =
                match MetadataToken.ofInt mdToken with
                | MetadataToken.MemberReference h -> h
                | token -> failwith $"%s{operation}: expected MemberRef token, got %O{token} from 0x%08x{mdToken}"

            if not (assembly.Members.ContainsKey memberRefHandle) then
                failwith $"%s{operation}: MemberRef token 0x%08x{mdToken} was not present in %s{assemblyFullName}"

            // CoreCLR's FCall forwards to `IMDInternalImport::GetNameAndSigOfMemberRef` and discards
            // the name, so the contract is "raw signature blob bytes for the supplied MemberRef
            // token". PawPrint decodes MemberRef signatures eagerly into `MemberSignature`; the
            // unparsed blob is recovered on demand from the metadata reader, as for GetSigOfMethodDef.
            let mr = metadataReaderOf assembly
            let memberRef = mr.GetMemberReference memberRefHandle
            let blob = ImmutableArray.Create<byte> (mr.GetBlobBytes memberRef.Signature)

            let constArrayValue, state =
                buildConstArray ctx.LoggerFactory ctx.BaseClassTypes operation blob state

            let state =
                IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state signatureOut constArrayValue

            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System.Reflection",
          "MetadataImport",
          "GetParentToken",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "MetadataImport.GetParentToken"
            let assemblyFullName = metadataImportHandleOfArg operation instruction.Arguments.[0]
            let assembly = metadataImportAssembly operation state assemblyFullName

            let mdToken =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                | CliType.Numeric (CliNumericType.Int32 mdToken) -> mdToken
                | other -> failwith $"%s{operation}: expected Int32 mdToken argument, got %O{other}"

            let parentOut =
                NativeCall.managedPointerOfPointerArgument
                    operation
                    "parent token out pointer"
                    instruction.Arguments.[2]

            let parentToken =
                match MetadataToken.ofInt mdToken with
                | MetadataToken.TypeDefinition typeDefHandle ->
                    let mutable typeInfo =
                        Unchecked.defaultof<TypeInfo<GenericParamFromMetadata, TypeDefn>>

                    if assembly.TypeDefs.TryGetValue (typeDefHandle, &typeInfo) then
                        if typeInfo.DeclaringType.IsNil then
                            metadataTypeDefNil
                        else
                            let parentHandle : System.Reflection.Metadata.EntityHandle =
                                System.Reflection.Metadata.TypeDefinitionHandle.op_Implicit typeInfo.DeclaringType

                            System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken parentHandle
                    else
                        failwith $"%s{operation}: TypeDef token 0x%08x{mdToken} was not present in %s{assemblyFullName}"
                | MetadataToken.MethodDef methodDefHandle ->
                    let mutable methodInfo =
                        Unchecked.defaultof<MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>>

                    if assembly.Methods.TryGetValue (methodDefHandle, &methodInfo) then
                        let parentHandle : System.Reflection.Metadata.EntityHandle =
                            System.Reflection.Metadata.TypeDefinitionHandle.op_Implicit
                                methodInfo.RequiredDeclaringType.Definition.Get

                        System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken parentHandle
                    else
                        failwith
                            $"%s{operation}: MethodDef token 0x%08x{mdToken} was not present in %s{assemblyFullName}"
                | MetadataToken.FieldDefinition fieldDefHandle ->
                    let mutable fieldInfo =
                        Unchecked.defaultof<FieldInfo<GenericParamFromMetadata, TypeDefn>>

                    if assembly.Fields.TryGetValue (fieldDefHandle, &fieldInfo) then
                        let parentHandle : System.Reflection.Metadata.EntityHandle =
                            System.Reflection.Metadata.TypeDefinitionHandle.op_Implicit
                                fieldInfo.DeclaringType.Definition.Get

                        System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken parentHandle
                    else
                        failwith
                            $"%s{operation}: FieldDef token 0x%08x{mdToken} was not present in %s{assemblyFullName}"
                | MetadataToken.CustomAttribute attrHandle ->
                    let mutable attr = Unchecked.defaultof<WoofWare.PawPrint.CustomAttribute>

                    if assembly.Attributes.TryGetValue (attrHandle, &attr) then
                        MetadataToken.toInt attr.Parent
                    else
                        failwith
                            $"%s{operation}: CustomAttribute token 0x%08x{mdToken} was not present in %s{assemblyFullName}"
                | MetadataToken.MemberReference memberRefHandle ->
                    let mutable memberRef =
                        Unchecked.defaultof<WoofWare.PawPrint.MemberReference<MetadataToken>>

                    if assembly.Members.TryGetValue (memberRefHandle, &memberRef) then
                        MetadataToken.toInt memberRef.Parent
                    else
                        failwith
                            $"%s{operation}: MemberRef token 0x%08x{mdToken} was not present in %s{assemblyFullName}"
                | MetadataToken.MethodSpecification methodSpecHandle ->
                    let mutable methodSpec = Unchecked.defaultof<WoofWare.PawPrint.MethodSpec>

                    if assembly.MethodSpecs.TryGetValue (methodSpecHandle, &methodSpec) then
                        MetadataToken.toInt methodSpec.Method
                    else
                        failwith
                            $"%s{operation}: MethodSpec token 0x%08x{mdToken} was not present in %s{assemblyFullName}"
                | MetadataToken.GenericParameter genericParamHandle ->
                    let mr = metadataReaderOf assembly

                    let parent : System.Reflection.Metadata.EntityHandle =
                        (mr.GetGenericParameter genericParamHandle).Parent

                    if parent.IsNil then
                        failwith
                            $"%s{operation}: GenericParameter token 0x%08x{mdToken} has nil parent in %s{assemblyFullName}"
                    else
                        System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken parent
                | MetadataToken.Parameter paramHandle ->
                    match methodOwningParameter assembly paramHandle with
                    | Some methodHandle ->
                        let parentHandle : System.Reflection.Metadata.EntityHandle =
                            System.Reflection.Metadata.MethodDefinitionHandle.op_Implicit methodHandle

                        System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken parentHandle
                    | None ->
                        failwith
                            $"%s{operation}: Parameter token 0x%08x{mdToken} had no owning method in %s{assemblyFullName}"
                | MetadataToken.EventDefinition eventHandle ->
                    match typeOwningEvent assembly eventHandle with
                    | Some typeHandle ->
                        let parentHandle : System.Reflection.Metadata.EntityHandle =
                            System.Reflection.Metadata.TypeDefinitionHandle.op_Implicit typeHandle

                        System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken parentHandle
                    | None ->
                        failwith
                            $"%s{operation}: Event token 0x%08x{mdToken} had no owning type in %s{assemblyFullName}"
                | MetadataToken.PropertyDefinition propertyHandle ->
                    match typeOwningProperty assembly propertyHandle with
                    | Some typeHandle ->
                        let parentHandle : System.Reflection.Metadata.EntityHandle =
                            System.Reflection.Metadata.TypeDefinitionHandle.op_Implicit typeHandle

                        System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken parentHandle
                    | None ->
                        failwith
                            $"%s{operation}: Property token 0x%08x{mdToken} had no owning type in %s{assemblyFullName}"
                | token ->
                    failwith $"TODO: %s{operation} does not yet support token kind %O{token} for token 0x%08x{mdToken}"

            let state = writeInt32AtPointer ctx.BaseClassTypes state parentOut parentToken

            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System.Reflection",
          "MetadataImport",
          "GetParamDefProps",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32)
            ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            // CoreCLR's FCall (managedmdimport.cpp:298) reports the Param row's `Sequence` and
            // `Flags` columns raw. `Sequence` is 1-based over the method's parameters, with 0
            // meaning the return value; `RuntimeParameterInfo.GetParameters` is what subtracts one
            // and turns -1 into the return parameter.
            let operation = "MetadataImport.GetParamDefProps"
            let assemblyFullName = metadataImportHandleOfArg operation instruction.Arguments.[0]
            let assembly = metadataImportAssembly operation state assemblyFullName

            let mdToken =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                | CliType.Numeric (CliNumericType.Int32 mdToken) -> mdToken
                | other -> failwith $"%s{operation}: expected Int32 parameterToken argument, got %O{other}"

            let sequenceOut =
                NativeCall.managedPointerOfPointerArgument operation "sequence out pointer" instruction.Arguments.[2]

            let attributesOut =
                NativeCall.managedPointerOfPointerArgument operation "attributes out pointer" instruction.Arguments.[3]

            let parameterHandle =
                match MetadataToken.ofInt mdToken with
                | MetadataToken.Parameter parameterHandle -> parameterHandle
                | token -> failwith $"%s{operation}: expected ParamDef token, got %O{token} from 0x%08x{mdToken}"

            let parameter = parameterDefinition operation assembly parameterHandle

            let state =
                writeInt32AtPointer ctx.BaseClassTypes state sequenceOut parameter.SequenceNumber

            let state =
                writeInt32AtPointer ctx.BaseClassTypes state attributesOut (int32 parameter.Attributes)

            // The managed wrapper turns a negative HRESULT into BadImageFormatException, but as in
            // every sibling handler the failures above are host-level crashes: the only guest caller
            // passes tokens `MetadataImport.Enum` minted from this same assembly.
            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System.Reflection",
          "MetadataImport",
          "GetGenericParamProps",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "MetadataImport.GetGenericParamProps"
            let assemblyFullName = metadataImportHandleOfArg operation instruction.Arguments.[0]
            let assembly = metadataImportAssembly operation state assemblyFullName

            let mdToken =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                | CliType.Numeric (CliNumericType.Int32 mdToken) -> mdToken
                | other -> failwith $"%s{operation}: expected Int32 genericParameter argument, got %O{other}"

            let attributesOut =
                NativeCall.managedPointerOfPointerArgument operation "flags out pointer" instruction.Arguments.[2]

            let genericParamHandle =
                match MetadataToken.ofInt mdToken with
                | MetadataToken.GenericParameter h -> h
                | token ->
                    failwith $"%s{operation}: expected GenericParameter token, got %O{token} from 0x%08x{mdToken}"

            let mr = metadataReaderOf assembly
            let genericParam = mr.GetGenericParameter genericParamHandle
            let flags = int genericParam.Attributes

            let state = writeInt32AtPointer ctx.BaseClassTypes state attributesOut flags

            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim 0)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | _ -> None
