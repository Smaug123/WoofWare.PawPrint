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

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = File.OpenRead corelibPath
        Assembly.read loggerFactory (Some corelibPath) stream

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : ImmutableDictionary<string, DumpedAssembly> =
        ImmutableDictionary.CreateRange [ KeyValuePair (corelib.Name.FullName, corelib) ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loaded bct AllConcreteTypes.Empty

    let private state () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()

        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    let private methodTableField (name : string) : FieldInfo<GenericParamFromMetadata, TypeDefn> =
        match corelib.TryGetTopLevelTypeDef "System.Runtime.CompilerServices" "MethodTable" with
        | None -> failwith "System.Runtime.CompilerServices.MethodTable not found in corelib"
        | Some methodTable ->
            methodTable.Fields
            |> List.tryFind (fun field -> field.Name = name)
            |> Option.defaultWith (fun () -> failwith $"MethodTable::{name} not found")

    let private handleFor (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>) : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes ti

    let private hasComponentSizeFlag : int32 = int32 0x80000000u
    let private containsGcPointersFlag : int32 = 0x01000000
    let private categoryMask : int32 = 0x000C0000
    let private categoryArray : int32 = 0x00080000
    let private componentSizeMask : int32 = 0x0000FFFF

    let private projectWithState (fieldName : string) (target : ConcreteTypeHandle) : CliType * IlMachineState =
        match MethodTableProjection.tryProjectField bct (methodTableField fieldName) target (state ()) with
        | None -> failwith $"Expected MethodTable::{fieldName} to project"
        | Some result -> result

    let private project (fieldName : string) (target : ConcreteTypeHandle) : CliType =
        // Current cases use already-concretized corelib shapes; non-primitive value-type elements should assert state too.
        projectWithState fieldName target |> fst

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
            ThreadState = Map.empty |> Map.add thread (ThreadState.New corelib.Name methodState)
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
            match project "Flags" (ConcreteTypeHandle.OneDimArrayZero (handleFor bct.Int32)) with
            | CliType.Numeric (CliNumericType.Int32 flags) -> flags
            | other -> failwith $"Expected MethodTable::Flags as Int32, got %O{other}"

        let objectArrayFlags =
            match project "Flags" (ConcreteTypeHandle.OneDimArrayZero (handleFor bct.Object)) with
            | CliType.Numeric (CliNumericType.Int32 flags) -> flags
            | other -> failwith $"Expected MethodTable::Flags as Int32, got %O{other}"

        let stringFlags =
            match project "Flags" (handleFor bct.String) with
            | CliType.Numeric (CliNumericType.Int32 flags) -> flags
            | other -> failwith $"Expected MethodTable::Flags as Int32, got %O{other}"

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
    let ``Ldfld projects MethodTable flags from MethodTable pointer provenance`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let field = methodTableField "Flags"
        let token = MetadataToken.FieldDefinition field.Handle
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
