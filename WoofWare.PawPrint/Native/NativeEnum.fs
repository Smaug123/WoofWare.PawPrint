namespace WoofWare.PawPrint

open System
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.PortableExecutable

[<RequireQualifiedAccess>]
module NativeEnum =
    type private EnumFieldValue =
        {
            Name : string
            ValueBits : uint64
        }

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
            failwith
                $"%s{operation}: expected enum type %s{typeInfo.Namespace}.%s{typeInfo.Name} to have one value__ instance field"

    let private enumStoragePrimitive (operation : string) (underlying : PrimitiveType) : PrimitiveType =
        match underlying with
        | PrimitiveType.SByte
        | PrimitiveType.Byte -> PrimitiveType.Byte
        | PrimitiveType.Int16
        | PrimitiveType.UInt16 -> PrimitiveType.UInt16
        | PrimitiveType.Int32
        | PrimitiveType.UInt32 -> PrimitiveType.UInt32
        | PrimitiveType.Int64
        | PrimitiveType.UInt64 -> PrimitiveType.UInt64
        | other -> failwith $"TODO: %s{operation} for enum underlying type %O{other}"

    let private primitiveTypeInfo
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (primitive : PrimitiveType)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        match primitive with
        | PrimitiveType.Byte -> baseClassTypes.Byte
        | PrimitiveType.UInt16 -> baseClassTypes.UInt16
        | PrimitiveType.UInt32 -> baseClassTypes.UInt32
        | PrimitiveType.UInt64 -> baseClassTypes.UInt64
        | other -> failwith $"%s{operation}: unsupported enum storage primitive %O{other}"

    let private storageValueOfBits (operation : string) (storage : PrimitiveType) (bits : uint64) : CliType =
        match storage with
        | PrimitiveType.Byte -> CliType.Numeric (CliNumericType.UInt8 (byte bits))
        | PrimitiveType.UInt16 -> CliType.Numeric (CliNumericType.UInt16 (uint16 bits))
        | PrimitiveType.UInt32 ->
            let value = BitConverter.ToInt32 (BitConverter.GetBytes (uint32 bits), 0)

            CliType.Numeric (CliNumericType.Int32 value)
        | PrimitiveType.UInt64 ->
            let value = BitConverter.ToInt64 (BitConverter.GetBytes bits, 0)

            CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim value))
        | other -> failwith $"%s{operation}: unsupported enum storage primitive %O{other}"

    let private readEnumConstantBits
        (operation : string)
        (assembly : DumpedAssembly)
        (field : FieldInfo<GenericParamFromMetadata, TypeDefn>)
        (underlying : PrimitiveType)
        : uint64
        =
        let metadataReader = assembly.PeReader.GetMetadataReader ()
        let fieldDef = metadataReader.GetFieldDefinition field.Handle
        let constantHandle = fieldDef.GetDefaultValue ()

        if constantHandle.IsNil then
            failwith $"%s{operation}: enum literal field %s{field.Name} did not have a metadata constant"

        let constant = metadataReader.GetConstant constantHandle
        let mutable reader = metadataReader.GetBlobReader constant.Value

        match underlying with
        | PrimitiveType.SByte
        | PrimitiveType.Byte -> reader.ReadByte () |> uint64
        | PrimitiveType.Int16
        | PrimitiveType.UInt16 -> reader.ReadUInt16 () |> uint64
        | PrimitiveType.Int32
        | PrimitiveType.UInt32 -> reader.ReadUInt32 () |> uint64
        | PrimitiveType.Int64
        | PrimitiveType.UInt64 -> reader.ReadUInt64 ()
        | other -> failwith $"TODO: %s{operation} for enum underlying type %O{other}"

    let private enumLiteralFields
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : FieldInfo<GenericParamFromMetadata, TypeDefn> list
        =
        typeInfo.Fields
        |> List.filter (fun field -> field.IsStatic && field.Attributes.HasFlag FieldAttributes.Literal)

    let private allocateValuesArray
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (storage : PrimitiveType)
        (values : EnumFieldValue list)
        : ManagedHeapAddress * IlMachineState
        =
        let elementType = primitiveTypeInfo operation baseClassTypes storage

        let elementHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes elementType

        let zero = CliType.zeroOfPrimitive state.ConcreteTypes baseClassTypes storage

        let arrayAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero elementHandle)
                (fun () -> zero)
                values.Length
                state

        let state =
            ((state, 0), values)
            ||> List.fold (fun (state, index) value ->
                let element = storageValueOfBits operation storage value.ValueBits
                IlMachineState.setArrayValue arrayAddr element index state, index + 1
            )
            |> fst

        arrayAddr, state

    let private allocateNamesArray
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (values : EnumFieldValue list)
        : ManagedHeapAddress * IlMachineState
        =
        let elementHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.String

        let arrayAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero elementHandle)
                (fun () -> CliType.ObjectRef None)
                values.Length
                state

        let state =
            ((state, 0), values)
            ||> List.fold (fun (state, index) value ->
                let nameAddr, state =
                    IlMachineState.allocateManagedString loggerFactory baseClassTypes value.Name state

                IlMachineState.setArrayValue arrayAddr (CliType.ObjectRef (Some nameAddr)) index state, index + 1
            )
            |> fst

        arrayAddr, state

    let private boolArgument (operation : string) (arg : CliType) : bool =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Bool b -> b <> 0uy
        | CliType.Numeric (CliNumericType.Int32 i) -> i <> 0
        | other -> failwith $"%s{operation}: expected BOOL argument as int32, got %O{other}"

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : ExecutionResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "Enum_GetValuesAndNames",
          "System.Private.CoreLib",
          "System",
          "Enum",
          "GetEnumValuesAndNames",
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
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib", _, "BOOL", boolGenerics) ],
          MethodReturnType.Void when
            qCallGenerics.IsEmpty
            && valuesHandleGenerics.IsEmpty
            && namesHandleGenerics.IsEmpty
            && boolGenerics.IsEmpty
            ->
            let operation = "Enum.GetEnumValuesAndNames"

            if instruction.Arguments.Length <> 4 then
                failwith $"%s{operation}: expected four native arguments, got %d{instruction.Arguments.Length}"

            let enumTypeHandle =
                NativeCall.qCallTypeHandleToConcreteTypeHandle
                    operation
                    state
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let valuesOut =
                NativeCall.objectHandleOnStackTarget operation state "values" instruction.Arguments.[1]

            let namesOut =
                NativeCall.objectHandleOnStackTarget operation state "names" instruction.Arguments.[2]

            let getNames = boolArgument operation instruction.Arguments.[3]

            let concreteType =
                AllConcreteTypes.lookup enumTypeHandle state.ConcreteTypes
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: enum concrete type handle was not registered: %O{enumTypeHandle}"
                )

            let enumAssembly =
                state.LoadedAssembly concreteType.Assembly
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: enum assembly is not loaded: %s{concreteType.Assembly.FullName}"
                )

            let enumTypeInfo = enumAssembly.TypeDefs.[concreteType.Definition.Get]
            let underlying = enumUnderlyingPrimitive operation enumTypeInfo
            let storage = enumStoragePrimitive operation underlying

            let values =
                enumLiteralFields enumTypeInfo
                |> List.map (fun field ->
                    {
                        Name = field.Name
                        ValueBits = readEnumConstantBits operation enumAssembly field underlying
                    }
                )

            let valuesAddr, state =
                allocateValuesArray operation ctx.BaseClassTypes state storage values

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    valuesOut
                    (CliType.ObjectRef (Some valuesAddr))

            let state =
                if getNames then
                    let namesAddr, state =
                        allocateNamesArray ctx.LoggerFactory ctx.BaseClassTypes state values

                    IlMachineState.writeManagedByrefWithBase
                        ctx.BaseClassTypes
                        state
                        namesOut
                        (CliType.ObjectRef (Some namesAddr))
                else
                    state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | _ -> None
