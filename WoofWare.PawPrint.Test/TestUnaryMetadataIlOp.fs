namespace WoofWare.PawPrint.Test

open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
module TestUnaryMetadataIlOp =

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private baseClassTypes : BaseClassTypes<DumpedAssembly> =
        Corelib.getBaseTypes corelib

    let private loadedAssemblies : ImmutableDictionary<string, DumpedAssembly> =
        ImmutableDictionary.CreateRange [ KeyValuePair (corelib.Name.FullName, corelib) ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loadedAssemblies baseClassTypes AllConcreteTypes.Empty

    let private initialState (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory) : IlMachineState =
        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    let private methodWithSingleInstruction
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (op : IlOp)
        (state : IlMachineState)
        : IlMachineState * MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let objectToString =
            baseClassTypes.Object.Methods
            |> List.find (fun method -> method.Name = "ToString" && method.Parameters.IsEmpty)

        let state, signature =
            TypeMethodSignature.map
                state
                (fun state ty ->
                    IlMachineTypeResolution.concretizeType
                        loggerFactory
                        baseClassTypes
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

    let private stateWithSingleInstruction
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (op : IlOp)
        : IlMachineState * ThreadId
        =
        let state, method =
            initialState loggerFactory |> methodWithSingleInstruction loggerFactory op

        let methodState =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    baseClassTypes
                    state._LoadedAssemblies
                    corelib
                    method
                    ImmutableArray.Empty
                    (ImmutableArray.Create (CliType.ObjectRef None))
                    None
            with
            | Ok methodState -> methodState
            | Error missing ->
                failwith $"Unexpected missing assembly references creating unary-metadata test frame: %O{missing}"

        let thread = ThreadId.ThreadId 0

        { state with
            ThreadState = Map.empty |> Map.add thread (ThreadState.New methodState)
        },
        thread

    let private sourceToken (token : MetadataToken) : SourcedMetadataToken =
        SourcedMetadataToken.make corelib.Name token

    let private int32StaticField (name : string) : FieldInfo<GenericParamFromMetadata, TypeDefn> =
        match corelib.TryGetTopLevelTypeDef "System" "Int32" with
        | None -> failwith "System.Int32 not found in corelib"
        | Some int32Type ->
            int32Type.Fields
            |> List.tryFind (fun field ->
                field.Name = name
                && field.Attributes.HasFlag System.Reflection.FieldAttributes.Static
            )
            |> Option.defaultWith (fun () -> failwith $"System.Int32::{name} static field not found")

    let private volatileObjectType () : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        let volatileType =
            match corelib.TryGetTopLevelTypeDef "System.Threading" "Volatile" with
            | None -> failwith "System.Threading.Volatile not found in corelib"
            | Some volatileType -> volatileType

        match corelib.TryGetNestedTypeDef volatileType.TypeDefHandle "VolatileObject" with
        | None -> failwith "System.Threading.Volatile/VolatileObject not found in corelib"
        | Some volatileObject -> volatileObject

    let private concretizeCorelibType
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (state : IlMachineState)
        (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : IlMachineState * ConcreteTypeHandle * ConcreteType<ConcreteTypeHandle>
        =
        let typeDefn =
            DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies ty

        let state, handle =
            IlMachineTypeResolution.concretizeType
                loggerFactory
                baseClassTypes
                state
                corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                typeDefn

        let concreteType =
            AllConcreteTypes.lookup handle state.ConcreteTypes
            |> Option.defaultWith (fun () -> failwith $"Missing concrete type for %O{ty}")

        state, handle, concreteType

    [<Test>]
    let ``Ldfld rejects static fields rather than returning without advancing`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let field = int32StaticField "MaxValue"
        let token = sourceToken (MetadataToken.FieldDefinition field.Handle)
        let op = IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldfld, token)
        let state, thread = stateWithSingleInstruction loggerFactory op

        let state =
            state
            |> IlMachineThreadState.pushToEvalStack' EvalStackValue.NullObjectRef thread

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                UnaryMetadataIlOp.execute loggerFactory baseClassTypes UnaryMetadataTokenIlOp.Ldfld token state thread
                |> ignore
            )

        ex.Message |> shouldContainText "ldfld cannot load static field"
        ex.Message |> shouldContainText "use ldsfld"

    [<Test>]
    let ``Ldfld through reinterpreted byref projects object field without materialising overlay`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let volatileObject = volatileObjectType ()

        let valueField =
            volatileObject.Fields
            |> List.tryFind (fun field -> field.Name = "Value")
            |> Option.defaultWith (fun () -> failwith "VolatileObject::Value not found")

        let token = sourceToken (MetadataToken.FieldDefinition valueField.Handle)
        let op = IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldfld, token)
        let state, thread = stateWithSingleInstruction loggerFactory op

        let state, volatileObjectHandle, volatileObjectConcrete =
            concretizeCorelibType loggerFactory state volatileObject

        let fieldId =
            FieldId.metadata volatileObjectHandle valueField.Handle valueField.Name

        let frame = state.ThreadState.[thread].ActiveMethodState
        let sourceAddress = ManagedHeapAddress 987

        let source =
            ManagedPointerSource.Byref (
                ByrefRoot.Argument (thread, frame, 0us),
                [ ByrefProjection.ReinterpretAs volatileObjectConcrete ]
            )

        let state =
            state
            |> IlMachineThreadState.setArgument thread frame 0us (CliType.ObjectRef (Some sourceAddress))
            |> IlMachineThreadState.pushToEvalStack' (EvalStackValue.ManagedPointer source) thread

        let state, whatWeDid =
            UnaryMetadataIlOp.execute loggerFactory baseClassTypes UnaryMetadataTokenIlOp.Ldfld token state thread

        whatWeDid |> shouldEqual WhatWeDid.Executed

        match IlMachineThreadState.peekEvalStack thread state with
        | Some (EvalStackValue.ObjectRef actual) -> actual |> shouldEqual sourceAddress
        | other -> failwith $"Expected ObjectRef %O{sourceAddress}, got %O{other}"

        state.ThreadState.[thread].MethodState.IlOpIndex
        |> shouldEqual (IlOp.NumberOfBytes op)

    [<Test>]
    let ``Stfld rejects static fields rather than returning without advancing`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let field = int32StaticField "MaxValue"
        let token = sourceToken (MetadataToken.FieldDefinition field.Handle)
        let op = IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Stfld, token)
        let state, thread = stateWithSingleInstruction loggerFactory op

        let state =
            state
            |> IlMachineThreadState.pushToEvalStack' EvalStackValue.NullObjectRef thread
            |> IlMachineThreadState.pushToEvalStack' (EvalStackValue.Int32 42) thread

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                UnaryMetadataIlOp.execute loggerFactory baseClassTypes UnaryMetadataTokenIlOp.Stfld token state thread
                |> ignore
            )

        ex.Message |> shouldContainText "stfld cannot store static field"
        ex.Message |> shouldContainText "use stsfld"
