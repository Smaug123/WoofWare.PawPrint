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
        use stream = File.OpenRead corelibPath
        Assembly.read loggerFactory (Some corelibPath) stream

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
                    IlMachineState.concretizeType
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

    [<Test>]
    let ``Ldfld rejects static fields rather than returning without advancing`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let field = int32StaticField "MaxValue"
        let token = sourceToken (MetadataToken.FieldDefinition field.Handle)
        let op = IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldfld, token)
        let state, thread = stateWithSingleInstruction loggerFactory op

        let state =
            state |> IlMachineState.pushToEvalStack' EvalStackValue.NullObjectRef thread

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                UnaryMetadataIlOp.execute loggerFactory baseClassTypes UnaryMetadataTokenIlOp.Ldfld token state thread
                |> ignore
            )

        ex.Message |> shouldContainText "ldfld cannot load static field"
        ex.Message |> shouldContainText "use ldsfld"

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
            |> IlMachineState.pushToEvalStack' EvalStackValue.NullObjectRef thread
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 42) thread

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                UnaryMetadataIlOp.execute loggerFactory baseClassTypes UnaryMetadataTokenIlOp.Stfld token state thread
                |> ignore
            )

        ex.Message |> shouldContainText "stfld cannot store static field"
        ex.Message |> shouldContainText "use stsfld"
