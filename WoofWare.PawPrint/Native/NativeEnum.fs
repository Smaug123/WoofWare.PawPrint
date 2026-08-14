namespace WoofWare.PawPrint

open System
open System.Reflection.Metadata

[<RequireQualifiedAccess>]
module NativeEnum =
    let private typeDisplayName (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>) : string =
        if String.IsNullOrEmpty typeInfo.Namespace then
            typeInfo.Name
        else
            $"%s{typeInfo.Namespace}.%s{typeInfo.Name}"

    let private requireConcreteEnumType
        (operation : string)
        (ctx : NativeCallContext)
        (arg : EvalStackValue)
        : IlMachineState * DumpedAssembly * TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        let state = ctx.State

        let concreteTypeHandle =
            match NativeCall.qCallTypeHandleToRuntimeTypeHandleTarget operation state arg with
            | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
                RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
            | RuntimeTypeHandleTarget.OpenConstructed _ as openConstructed ->
                failwith
                    $"TODO: open constructed types are not handled at Native/NativeEnum.fs:%s{__LINE__}; got %O{openConstructed}"
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
                failwith $"%s{operation}: expected a closed enum RuntimeTypeHandle, got open generic %O{identity}"
            | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
                failwith
                    $"%s{operation}: expected a closed enum RuntimeTypeHandle, got generic parameter #%i{position} of %O{declaringType.TypeDefinition.Get}"
            | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
                failwith
                    $"%s{operation}: expected a closed enum RuntimeTypeHandle, got method generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}"
            | RuntimeTypeHandleTarget.Closed typeHandle ->
                match typeHandle with
                | ConcreteTypeHandle.Concrete _ -> typeHandle
                | ConcreteTypeHandle.Byref _
                | ConcreteTypeHandle.Pointer _
                | ConcreteTypeHandle.FunctionPointer _
                | ConcreteTypeHandle.OneDimArrayZero _
                | ConcreteTypeHandle.Array _ ->
                    failwith $"%s{operation}: expected exact concrete enum type handle, got non-concrete %O{typeHandle}"

        let concreteType =
            AllConcreteTypes.lookup concreteTypeHandle state.ConcreteTypes
            |> Option.defaultWith (fun () ->
                failwith $"%s{operation}: concrete type handle %O{concreteTypeHandle} is not registered"
            )

        let assembly =
            state.LoadedAssembly concreteType.Assembly
            |> Option.defaultWith (fun () ->
                failwith $"%s{operation}: defining assembly is not loaded: %s{concreteType.Assembly.FullName}"
            )

        let typeInfo = assembly.TypeDefs.[concreteType.Definition.Get]

        let state =
            match typeInfo.BaseType with
            | None -> failwith $"%s{operation}: %s{typeDisplayName typeInfo} has no base type; expected System.Enum"
            | Some baseType ->
                let state, _baseAssembly, resolvedBase =
                    IlMachineState.resolveBaseTypeInfo ctx.LoggerFactory ctx.BaseClassTypes state assembly baseType

                match resolvedBase with
                | TypeDefn.FromDefinition (identity, _) when identity = ctx.BaseClassTypes.Enum.Identity -> state
                | other ->
                    failwith
                        $"%s{operation}: %s{typeDisplayName typeInfo} is not an enum; resolved base type was %O{other}"

        state, assembly, typeInfo

    let private enumUnderlyingPrimitive
        (operation : string)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : PrimitiveType
        =
        let instanceFields =
            typeInfo.Fields |> List.filter (fun field -> not field.IsStatic)

        match instanceFields with
        | [ field ] when field.Name = "value__" ->
            match field.Signature with
            | TypeDefn.PrimitiveType primitive -> primitive
            | other -> failwith $"%s{operation}: enum value__ field had non-primitive signature %O{other}"
        | _ ->
            failwith $"%s{operation}: enum %s{typeDisplayName typeInfo} did not have exactly one instance value__ field"

    let private storageElementHandle
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (underlying : PrimitiveType)
        : ConcreteTypeHandle
        =
        // CoreLib's EnumInfo<TStorage> asks the runtime for the unsigned storage
        // view of signed enum constants: sbyte -> byte, short -> ushort, etc.
        let typeInfo =
            match underlying with
            | PrimitiveType.SByte
            | PrimitiveType.Byte -> baseClassTypes.Byte
            | PrimitiveType.Int16
            | PrimitiveType.UInt16 -> baseClassTypes.UInt16
            | PrimitiveType.Int32
            | PrimitiveType.UInt32 -> baseClassTypes.UInt32
            | PrimitiveType.Int64
            | PrimitiveType.UInt64 -> baseClassTypes.UInt64
            | PrimitiveType.Boolean
            | PrimitiveType.Char
            | PrimitiveType.Single
            | PrimitiveType.Double
            | PrimitiveType.String
            | PrimitiveType.TypedReference
            | PrimitiveType.IntPtr
            | PrimitiveType.UIntPtr
            | PrimitiveType.Object ->
                // TODO: ECMA-335 permits bool, char, native int, and native uint
                // enum underlyings; model those storage views if PawPrint hits them.
                failwith $"%s{operation}: unsupported enum underlying primitive type %O{underlying}"

        AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes typeInfo

    let private failConstantType
        (operation : string)
        (field : FieldInfo<GenericParamFromMetadata, TypeDefn>)
        (underlying : PrimitiveType)
        (actual : ConstantTypeCode)
        : 'a
        =
        failwith
            $"%s{operation}: enum field %s{field.Name} for underlying type %O{underlying} had unsupported metadata constant type %O{actual}"

    let private constantToStorageValue
        (operation : string)
        (metadataReader : MetadataReader)
        (underlying : PrimitiveType)
        (field : FieldInfo<GenericParamFromMetadata, TypeDefn>)
        : CliType
        =
        // The raw Constant row; this projection is the *enum* view of it, which insists the row's
        // type code agrees with the declared underlying type. `MetadataImport.GetDefaultValue` takes
        // the same row and must not insist on anything, so the two share the lookup and not the
        // interpretation.
        let typeCode, blobReader =
            NativeMetadataImport.constantRowOfField metadataReader field.Handle
            |> Option.defaultWith (fun () ->
                failwith $"%s{operation}: static literal enum field %s{field.Name} had no metadata constant"
            )

        let mutable reader = blobReader

        match underlying, typeCode with
        | PrimitiveType.SByte, ConstantTypeCode.SByte ->
            CliType.Numeric (CliNumericType.UInt8 (byte (reader.ReadSByte ())))
        | PrimitiveType.Byte, ConstantTypeCode.Byte -> CliType.Numeric (CliNumericType.UInt8 (reader.ReadByte ()))
        | PrimitiveType.Int16, ConstantTypeCode.Int16 ->
            CliType.Numeric (CliNumericType.UInt16 (uint16 (reader.ReadInt16 ())))
        | PrimitiveType.UInt16, ConstantTypeCode.UInt16 ->
            CliType.Numeric (CliNumericType.UInt16 (reader.ReadUInt16 ()))
        | PrimitiveType.Int32, ConstantTypeCode.Int32 -> CliType.Numeric (CliNumericType.Int32 (reader.ReadInt32 ()))
        | PrimitiveType.UInt32, ConstantTypeCode.UInt32 ->
            CliType.Numeric (CliNumericType.Int32 (int32 (reader.ReadUInt32 ())))
        | PrimitiveType.Int64, ConstantTypeCode.Int64 ->
            CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim (reader.ReadInt64 ())))
        | PrimitiveType.UInt64, ConstantTypeCode.UInt64 ->
            CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim (int64 (reader.ReadUInt64 ()))))
        | _, actual -> failConstantType operation field underlying actual

    let private allocateArrayWithValues
        (state : IlMachineState)
        (arrayType : ConcreteTypeHandle)
        (elementZero : unit -> CliType)
        (values : CliType list)
        : ManagedHeapAddress * IlMachineState
        =
        let arrayAddr, state =
            IlMachineState.allocateArray arrayType elementZero values.Length state

        let state =
            ((state, 0), values)
            ||> List.fold (fun (state, index) value ->
                IlMachineState.setArrayValue arrayAddr value index state, index + 1
            )
            |> fst

        arrayAddr, state

    let private allocateValuesArray
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (storageElement : ConcreteTypeHandle)
        (values : CliType list)
        : ManagedHeapAddress * IlMachineState
        =
        let zero, state =
            IlMachineState.cliTypeZeroOfHandle state baseClassTypes storageElement

        allocateArrayWithValues state (ConcreteTypeHandle.OneDimArrayZero storageElement) (fun () -> zero) values

    let private allocateNamesArray
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (names : string list)
        : ManagedHeapAddress * IlMachineState
        =
        let stringHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.String

        let arrayAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero stringHandle)
                (fun () -> CliType.ObjectRef None)
                names.Length
                state

        let state =
            ((state, 0), names)
            ||> List.fold (fun (state, index) name ->
                let stringAddr, state =
                    IlMachineState.allocateManagedString loggerFactory baseClassTypes name state

                IlMachineState.setArrayValue arrayAddr (CliType.ObjectRef (Some stringAddr)) index state, index + 1
            )
            |> fst

        arrayAddr, state

    let private boolArgument (operation : string) (arg : CliType) : bool =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.Int32 value) -> value <> 0
        | CliType.Bool value -> value <> 0uy
        | other -> failwith $"%s{operation}: expected Interop.BOOL argument as Int32, got %O{other}"

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "Enum_GetValuesAndNames",
          "System.Private.CoreLib",
          "System",
          "Enum",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallTypeHandle",
                                              qCallGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              valuesHandleGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              namesHandleGenerics)
            _getNamesType ],
          MethodReturnType.Void when
            qCallGenerics.IsEmpty
            && valuesHandleGenerics.IsEmpty
            && namesHandleGenerics.IsEmpty
            ->
            let operation = "Enum.GetValuesAndNames"

            if instruction.Arguments.Length <> 4 then
                failwith $"%s{operation}: expected four native arguments, got %d{instruction.Arguments.Length}"

            let enumHandleArg = instruction.Arguments.[0] |> EvalStackValue.ofCliType

            let state, assembly, typeInfo = requireConcreteEnumType operation ctx enumHandleArg

            let valuesOut =
                NativeCall.objectHandleOnStackTarget operation state "pReturnValues" instruction.Arguments.[1]

            let namesOut =
                NativeCall.objectHandleOnStackTarget operation state "pReturnNames" instruction.Arguments.[2]

            let getNames = boolArgument operation instruction.Arguments.[3]

            let underlying = enumUnderlyingPrimitive operation typeInfo

            let storageElement =
                storageElementHandle operation ctx.BaseClassTypes state underlying

            let metadataReader = assembly.PeReader.GetMetadataReader ()

            // CoreCLR returns declaration-order metadata here. Managed
            // EnumInfo<TStorage> owns any sorting needed by higher-level APIs.
            let enumFields =
                typeInfo.Fields
                |> List.filter (fun field ->
                    field.IsStatic
                    && field.Attributes.HasFlag System.Reflection.FieldAttributes.Literal
                )

            let values =
                enumFields
                |> List.map (constantToStorageValue operation metadataReader underlying)

            let valuesArrayAddr, state =
                allocateValuesArray ctx.BaseClassTypes state storageElement values

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    valuesOut
                    (CliType.ObjectRef (Some valuesArrayAddr))

            let state =
                if getNames then
                    let names = enumFields |> List.map _.Name

                    let namesArrayAddr, state =
                        allocateNamesArray ctx.LoggerFactory ctx.BaseClassTypes state names

                    IlMachineState.writeManagedByrefWithBase
                        ctx.BaseClassTypes
                        state
                        namesOut
                        (CliType.ObjectRef (Some namesArrayAddr))
                else
                    state

            NativeHandlerResult.completed state |> Some
        | _ -> None
