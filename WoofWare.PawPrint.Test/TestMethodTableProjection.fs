namespace WoofWare.PawPrint.Test

open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
module TestMethodTableProjection =

    // The factory is intentionally undisposed: the returned DumpedAssembly.Logger closes over
    // its sinks, and disposing while the assembly is still live would silently drop events.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : ImmutableDictionary<string, DumpedAssembly> =
        ImmutableDictionary.CreateRange [ KeyValuePair (corelib.Name.FullName, corelib) ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loaded bct AllConcreteTypes.Empty

    let private stateWithLogger (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory) : IlMachineState =
        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    let private state () : IlMachineState =
        // Factory intentionally undisposed: state.Logger outlives this scope.
        let _, loggerFactory = LoggerFactory.makeTest ()

        stateWithLogger loggerFactory

    let private topLevelType (``namespace`` : string) (name : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        match corelib.TryGetTopLevelTypeDef ``namespace`` name with
        | None -> failwith $"%s{``namespace``}.%s{name} not found in corelib"
        | Some typeInfo -> typeInfo

    let private methodTableField (name : string) : FieldInfo<GenericParamFromMetadata, TypeDefn> =
        match corelib.TryGetTopLevelTypeDef "System.Runtime.CompilerServices" "MethodTable" with
        | None -> failwith "System.Runtime.CompilerServices.MethodTable not found in corelib"
        | Some methodTable ->
            methodTable.Fields
            |> List.tryFind (fun field -> field.Name = name)
            |> Option.defaultWith (fun () -> failwith $"MethodTable::{name} not found")

    let private methodTableAuxiliaryDataField (name : string) : FieldInfo<GenericParamFromMetadata, TypeDefn> =
        match corelib.TryGetTopLevelTypeDef "System.Runtime.CompilerServices" "MethodTableAuxiliaryData" with
        | None -> failwith "System.Runtime.CompilerServices.MethodTableAuxiliaryData not found in corelib"
        | Some methodTableAuxiliaryData ->
            methodTableAuxiliaryData.Fields
            |> List.tryFind (fun field -> field.Name = name)
            |> Option.defaultWith (fun () -> failwith $"MethodTableAuxiliaryData::{name} not found")

    let private rawArrayDataField (name : string) : FieldInfo<GenericParamFromMetadata, TypeDefn> =
        match corelib.TryGetTopLevelTypeDef "System.Runtime.CompilerServices" "RawArrayData" with
        | None -> failwith "System.Runtime.CompilerServices.RawArrayData not found in corelib"
        | Some rawArrayData ->
            rawArrayData.Fields
            |> List.tryFind (fun field -> field.Name = name)
            |> Option.defaultWith (fun () -> failwith $"RawArrayData::{name} not found")

    let private handleFor (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>) : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes ti

    let private concreteTypeFor (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>) : ConcreteType<ConcreteTypeHandle> =
        handleFor ti
        |> fun handle -> AllConcreteTypes.lookup handle concreteTypes
        |> Option.defaultWith (fun () -> failwith $"Could not find concrete type for %O{ti}")

    let private allocateIntArray (length : int) (state : IlMachineState) : ManagedHeapAddress * IlMachineState =
        let intArrayHandle = ConcreteTypeHandle.OneDimArrayZero (handleFor bct.Int32)

        IlMachineState.allocateArray intArrayHandle (fun () -> CliType.Numeric (CliNumericType.Int32 0)) length state

    let private hasComponentSizeFlag : int32 = int32 0x80000000u
    let private containsGcPointersFlag : int32 = 0x01000000
    let private categoryMask : int32 = 0x000C0000
    let private categoryInterface : int32 = 0x000C0000
    let private categoryArray : int32 = 0x00080000
    let private componentSizeMask : int32 = 0x0000FFFF

    let private projectFromState
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (state : IlMachineState)
        (fieldName : string)
        (target : ConcreteTypeHandle)
        : CliType * IlMachineState
        =
        match MethodTableProjection.tryProjectField loggerFactory bct (methodTableField fieldName) target state with
        | None -> failwith $"Expected MethodTable::{fieldName} to project"
        | Some result -> result

    let private projectWithState (fieldName : string) (target : ConcreteTypeHandle) : CliType * IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()

        projectFromState loggerFactory (stateWithLogger loggerFactory) fieldName target

    let private project (fieldName : string) (target : ConcreteTypeHandle) : CliType =
        // Current cases use already-concretized corelib shapes; non-primitive value-type elements should assert state too.
        projectWithState fieldName target |> fst

    let private projectFlags (target : ConcreteTypeHandle) : int32 =
        match project "Flags" target with
        | CliType.Numeric (CliNumericType.Int32 flags) -> flags
        | other -> failwith $"Expected MethodTable::Flags as Int32, got %O{other}"

    let private projectFlagsFromState
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (state : IlMachineState)
        (target : ConcreteTypeHandle)
        : int32
        =
        match projectFromState loggerFactory state "Flags" target with
        | CliType.Numeric (CliNumericType.Int32 flags), _ -> flags
        | other, _ -> failwith $"Expected MethodTable::Flags as Int32, got %O{other}"

    let private concretizeCorelibType
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : IlMachineState * ConcreteTypeHandle
        =
        typeInfo
        |> DumpedAssembly.typeInfoToTypeDefn' bct state._LoadedAssemblies
        |> IlMachineState.concretizeType loggerFactory bct state corelib.Name ImmutableArray.Empty ImmutableArray.Empty

    let private projectAuxiliaryData (fieldName : string) (target : ConcreteTypeHandle) : CliType =
        match
            MethodTableProjection.tryProjectAuxiliaryDataField
                bct
                (methodTableAuxiliaryDataField fieldName)
                target
                (state ())
        with
        | None -> failwith $"Expected MethodTableAuxiliaryData::{fieldName} to project"
        | Some (result, _) -> result

    let private methodWithSingleInstruction
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (op : IlOp)
        (state : IlMachineState)
        : IlMachineState * MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let objectToString =
            bct.Object.Methods
            |> List.find (fun method -> method.Name = "ToString" && method.Parameters.IsEmpty)

        let state, signature =
            TypeMethodSignature.map
                state
                (fun state ty ->
                    IlMachineState.concretizeType
                        loggerFactory
                        bct
                        state
                        corelib.Name
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        ty
                )
                objectToString.Signature

        let instructions : MethodInstructions<ConcreteTypeHandle> =
            { MethodInstructions.onlyRet () with
                Instructions = [ op, 0 ]
                Locations = Map.empty |> Map.add 0 op
            }

        let method =
            objectToString
            |> MethodInfo.mapTypeGenerics (fun _ -> failwith "System.Object::ToString is not type-generic")
            |> MethodInfo.mapMethodGenerics (fun _ _ -> failwith "System.Object::ToString is not method-generic")
            |> MethodInfo.setMethodVars (Some instructions) signature

        state, method

    let private stateWithSingleInstruction (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory) (op : IlOp) =
        let state, method = state () |> methodWithSingleInstruction loggerFactory op

        let methodState =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    bct
                    state._LoadedAssemblies
                    corelib
                    method
                    ImmutableArray.Empty
                    (ImmutableArray.Create (CliType.ObjectRef None))
                    None
            with
            | Ok methodState -> methodState
            | Error missing ->
                failwith $"Unexpected missing assembly references creating MethodTableProjection frame: %O{missing}"

        let thread = ThreadId.ThreadId 0

        { state with
            ThreadState = Map.empty |> Map.add thread (ThreadState.New methodState)
        },
        thread

    [<Test>]
    let ``BaseSize distinguishes szarrays from multidimensional arrays`` () : unit =
        let intHandle = handleFor bct.Int32

        project "BaseSize" (ConcreteTypeHandle.OneDimArrayZero intHandle)
        |> shouldEqual (CliType.Numeric (CliNumericType.Int32 (3 * NATIVE_INT_SIZE)))

        project "BaseSize" (ConcreteTypeHandle.Array (intHandle, 2))
        |> shouldEqual (CliType.Numeric (CliNumericType.Int32 (5 * NATIVE_INT_SIZE)))

    [<Test>]
    let ``ComponentSize is computed from the structured element type`` () : unit =
        project "ComponentSize" (ConcreteTypeHandle.OneDimArrayZero (handleFor bct.Int32))
        |> shouldEqual (CliType.Numeric (CliNumericType.UInt16 4us))

        project "ComponentSize" (ConcreteTypeHandle.OneDimArrayZero (handleFor bct.Object))
        |> shouldEqual (CliType.Numeric (CliNumericType.UInt16 (uint16 NATIVE_INT_SIZE)))

        project "ComponentSize" (handleFor bct.String)
        |> shouldEqual (CliType.Numeric (CliNumericType.UInt16 2us))

    [<Test>]
    let ``Flags identify array component size and GC pointer containment`` () : unit =
        let intArrayFlags =
            projectFlags (ConcreteTypeHandle.OneDimArrayZero (handleFor bct.Int32))

        let objectArrayFlags =
            projectFlags (ConcreteTypeHandle.OneDimArrayZero (handleFor bct.Object))

        let stringFlags = projectFlags (handleFor bct.String)

        intArrayFlags &&& hasComponentSizeFlag |> shouldEqual hasComponentSizeFlag
        intArrayFlags &&& containsGcPointersFlag |> shouldEqual 0
        intArrayFlags &&& categoryMask |> shouldEqual categoryArray
        intArrayFlags &&& componentSizeMask |> shouldEqual 4

        objectArrayFlags &&& hasComponentSizeFlag |> shouldEqual hasComponentSizeFlag

        objectArrayFlags &&& containsGcPointersFlag
        |> shouldEqual containsGcPointersFlag

        objectArrayFlags &&& categoryMask |> shouldEqual categoryArray
        objectArrayFlags &&& componentSizeMask |> shouldEqual NATIVE_INT_SIZE

        stringFlags &&& hasComponentSizeFlag |> shouldEqual hasComponentSizeFlag
        stringFlags &&& containsGcPointersFlag |> shouldEqual 0
        stringFlags &&& componentSizeMask |> shouldEqual 2

    [<Test>]
    let ``Flags compute non-array reference type GC pointer containment from instance fields`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let state = stateWithLogger loggerFactory

        let state, exceptionHandle = concretizeCorelibType loggerFactory state bct.Exception

        let state, disposableHandle =
            concretizeCorelibType loggerFactory state (topLevelType "System" "IDisposable")

        let objectFlags = projectFlags (handleFor bct.Object)
        let exceptionFlags = projectFlagsFromState loggerFactory state exceptionHandle
        let disposableFlags = projectFlagsFromState loggerFactory state disposableHandle

        objectFlags &&& hasComponentSizeFlag |> shouldEqual 0
        objectFlags &&& containsGcPointersFlag |> shouldEqual 0
        objectFlags &&& categoryMask |> shouldEqual 0

        exceptionFlags &&& hasComponentSizeFlag |> shouldEqual 0

        exceptionFlags &&& containsGcPointersFlag |> shouldEqual containsGcPointersFlag

        exceptionFlags &&& categoryMask |> shouldEqual 0

        disposableFlags &&& hasComponentSizeFlag |> shouldEqual 0
        disposableFlags &&& containsGcPointersFlag |> shouldEqual 0
        disposableFlags &&& categoryMask |> shouldEqual categoryInterface

    [<Test>]
    let ``Ldfld projects MethodTable flags from MethodTable pointer provenance`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let field = methodTableField "Flags"
        let token = MetadataToken.FieldDefinition field.Handle
        let token = SourcedMetadataToken.make corelib.Name token
        let op = IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldfld, token)
        let state, thread = stateWithSingleInstruction loggerFactory op

        let intArrayHandle = ConcreteTypeHandle.OneDimArrayZero (handleFor bct.Int32)

        let state =
            state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr intArrayHandle))
                thread

        let state, whatWeDid =
            UnaryMetadataIlOp.execute loggerFactory bct UnaryMetadataTokenIlOp.Ldfld token state thread

        whatWeDid |> shouldEqual WhatWeDid.Executed

        IlMachineState.peekEvalStack thread state
        |> shouldEqual (Some (EvalStackValue.Int32 (hasComponentSizeFlag ||| categoryArray ||| 4)))

        state.ThreadState.[thread].MethodState.IlOpIndex
        |> shouldEqual (IlOp.NumberOfBytes op)

    [<Test>]
    let ``ElementType preserves MethodTable pointer provenance`` () : unit =
        let intHandle = handleFor bct.Int32

        project "ElementType" (ConcreteTypeHandle.OneDimArrayZero intHandle)
        |> shouldEqual (CliType.RuntimePointer (CliRuntimePointer.MethodTablePtr intHandle))

    [<Test>]
    let ``AuxiliaryData preserves MethodTable auxiliary-data pointer provenance`` () : unit =
        let intHandle = handleFor bct.Int32

        project "AuxiliaryData" intHandle
        |> shouldEqual (CliType.RuntimePointer (CliRuntimePointer.MethodTableAuxiliaryDataPtr intHandle))

    [<Test>]
    let ``AuxiliaryData flags start with fast-compare cache bits unset`` () : unit =
        projectAuxiliaryData "Flags" (handleFor bct.Int32)
        |> shouldEqual (CliType.Numeric (CliNumericType.Int32 0))

    [<Test>]
    let ``Ldfld projects MethodTableAuxiliaryData flags from auxiliary-data pointer provenance`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let field = methodTableAuxiliaryDataField "Flags"
        let token = MetadataToken.FieldDefinition field.Handle
        let token = SourcedMetadataToken.make corelib.Name token
        let op = IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldfld, token)
        let state, thread = stateWithSingleInstruction loggerFactory op
        let intHandle = handleFor bct.Int32

        let state =
            state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.NativeInt (NativeIntSource.MethodTableAuxiliaryDataPtr intHandle))
                thread

        let state, whatWeDid =
            UnaryMetadataIlOp.execute loggerFactory bct UnaryMetadataTokenIlOp.Ldfld token state thread

        whatWeDid |> shouldEqual WhatWeDid.Executed

        IlMachineState.peekEvalStack thread state
        |> shouldEqual (Some (EvalStackValue.Int32 0))

        state.ThreadState.[thread].MethodState.IlOpIndex
        |> shouldEqual (IlOp.NumberOfBytes op)

    [<Test>]
    let ``Ldfld projects RawArrayData length from structured array storage`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let field = rawArrayDataField "Length"
        let token = MetadataToken.FieldDefinition field.Handle
        let token = SourcedMetadataToken.make corelib.Name token
        let op = IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldfld, token)
        let state, thread = stateWithSingleInstruction loggerFactory op
        let arrayAddr, state = allocateIntArray 3 state

        let state =
            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ObjectRef arrayAddr) thread

        let state, whatWeDid =
            UnaryMetadataIlOp.execute loggerFactory bct UnaryMetadataTokenIlOp.Ldfld token state thread

        whatWeDid |> shouldEqual WhatWeDid.Executed

        IlMachineState.peekEvalStack thread state
        |> shouldEqual (Some (EvalStackValue.Int32 3))

        state.ThreadState.[thread].MethodState.IlOpIndex
        |> shouldEqual (IlOp.NumberOfBytes op)

    [<Test>]
    let ``Ldflda projects RawArrayData data as a byte view of array storage`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let field = rawArrayDataField "Data"
        let token = MetadataToken.FieldDefinition field.Handle
        let token = SourcedMetadataToken.make corelib.Name token
        let op = IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldflda, token)
        let state, thread = stateWithSingleInstruction loggerFactory op
        let arrayAddr, state = allocateIntArray 3 state

        let state =
            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ObjectRef arrayAddr) thread

        let state, whatWeDid =
            UnaryMetadataIlOp.execute loggerFactory bct UnaryMetadataTokenIlOp.Ldflda token state thread

        whatWeDid |> shouldEqual WhatWeDid.Executed

        match IlMachineState.peekEvalStack thread state with
        | Some (EvalStackValue.ManagedPointer (ManagedPointerSource.Byref (ByrefRoot.ArrayElement (actualArrayAddr,
                                                                                                   actualIndex),
                                                                           [ ByrefProjection.ReinterpretAs actualView ]))) ->
            actualArrayAddr |> shouldEqual arrayAddr
            actualIndex |> shouldEqual 0
            actualView |> shouldEqual (concreteTypeFor bct.Byte)
        | other -> failwith $"Expected RawArrayData::Data byte-view byref, got %O{other}"

        state.ThreadState.[thread].MethodState.IlOpIndex
        |> shouldEqual (IlOp.NumberOfBytes op)
