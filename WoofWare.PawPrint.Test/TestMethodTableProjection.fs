namespace WoofWare.PawPrint.Test

open System.Collections.Generic
open System.Collections.Immutable
open System.IO
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

    let private project (fieldName : string) (target : ConcreteTypeHandle) : CliType =
        match MethodTableProjection.tryProjectField bct (methodTableField fieldName) target (state ()) with
        | None -> failwith $"Expected MethodTable::{fieldName} to project"
        | Some (value, _) -> value

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

        intArrayFlags &&& int32 0x80000000u |> shouldEqual (int32 0x80000000u)
        intArrayFlags &&& 0x01000000 |> shouldEqual 0

        objectArrayFlags &&& int32 0x80000000u |> shouldEqual (int32 0x80000000u)
        objectArrayFlags &&& 0x01000000 |> shouldEqual 0x01000000

    [<Test>]
    let ``ElementType preserves MethodTable pointer provenance`` () : unit =
        let intHandle = handleFor bct.Int32

        project "ElementType" (ConcreteTypeHandle.OneDimArrayZero intHandle)
        |> shouldEqual (CliType.RuntimePointer (CliRuntimePointer.MethodTablePtr intHandle))
