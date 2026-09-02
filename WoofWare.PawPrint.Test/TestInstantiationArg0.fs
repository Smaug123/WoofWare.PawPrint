namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// `MethodTable::InstantiationArg0` is CoreCLR's `mt->GetInstantiation()[0].AsMethodTable()`, and
/// `MethodTable::NullableValueAddrOffset` is where `Nullable&lt;T&gt;` keeps its payload. Together
/// they are what `CastHelpers.Box_Nullable` needs, and that is their only guest-reachable caller —
/// which asks about a one-argument instantiation and so can never tell arg 0 from arg 1, nor a
/// generic instantiation from a type that has no instantiation at all. Those are the mistakes this
/// fixture rules out, by asking the projections directly about types the guest constructed.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestInstantiationArg0 =

    let private assy = typeof<RunResult>.Assembly

    /// Two `Pair` instantiations rather than one, and with *different* first arguments, so that a
    /// projection reading the wrong index — or ignoring the index and answering something
    /// constant — cannot satisfy both rows. The nullables span the payload alignments: one byte,
    /// four, eight, and a struct that brings its own.
    let private guestSource : string =
        """
using System.Collections.Generic;

public enum Colour : byte { Black = 0, Red = 7 }

public struct Pair<A, B>
{
    public A First;
    public B Second;
}

public struct TwoInts
{
    public int X;
    public int Y;
}

public static class Program
{
    public static int Main(string[] args)
    {
        Pair<long, byte> longByte = default;
        Pair<string, int> stringInt = default;
        List<int> ints = new List<int>();

        byte? nullableByte = 1;
        int? nullableInt = 3;
        long? nullableLong = 4;
        double? nullableDouble = 5.5;
        Colour? nullableColour = Colour.Red;
        TwoInts? nullableTwoInts = new TwoInts { X = 6, Y = 7 };

        if (longByte.Second != 0) { return 1; }
        if (stringInt.First != null) { return 2; }
        if (ints.Count != 0) { return 3; }
        if (nullableByte != 1) { return 4; }
        if (nullableInt != 3) { return 5; }
        if (nullableLong != 4) { return 6; }
        if (nullableDouble != 5.5) { return 7; }
        if (nullableColour != Colour.Red) { return 8; }
        if (nullableTwoInts.Value.Y != 7) { return 9; }

        return 0;
    }
}
"""

    /// Runs the guest to completion and hands back its final state together with the
    /// `BaseClassTypes` the run resolved. `Program.run` returns only the outcome, and the
    /// projections below need both.
    let private runGuest () : IlMachineState * BaseClassTypes<DumpedAssembly> =
        let image = Roslyn.compile [ guestSource ]

        let messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", "InstantiationArg0.cs" ]

        use _loggerFactoryResource = loggerFactory
        let logger = loggerFactory.CreateLogger "TestInstantiationArg0"

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        try
            let prepared =
                match
                    Program.prepare
                        loggerFactory
                        (Some "InstantiationArg0.cs")
                        peImage
                        (HostConfig.Default dotnetRuntimes)
                with
                | Program.ProgramStartResult.Ready prepared -> prepared
                | Program.ProgramStartResult.CompletedBeforeMain outcome ->
                    failwith $"guest completed before Main: %O{outcome}"

            let baseClassTypes = prepared.BaseClassTypes

            let rec go (steps : int) (prepared : Program.PreparedProgram) : IlMachineState =
                if steps > 20_000_000 then
                    failwith "guest did not terminate"

                match Program.stepPrepared loggerFactory logger prepared with
                | Program.ProgramStepOutcome.Completed (RunOutcome.NormalExit (state, _)) ->
                    if state.LatchedExitCode <> 0 then
                        failwith $"guest did not return 0: %d{state.LatchedExitCode}"

                    state
                | Program.ProgramStepOutcome.Completed other -> failwith $"guest did not exit normally: %O{other}"
                | Program.ProgramStepOutcome.Deadlocked (_, stuck) -> failwith $"guest deadlocked: %s{stuck}"
                | Program.ProgramStepOutcome.InstructionStepped (prepared, _, _, _)
                | Program.ProgramStepOutcome.WorkerTerminated (prepared, _) -> go (steps + 1) prepared

            go 0 prepared, baseClassTypes
        with _ ->
            for message in messages () do
                System.Console.Error.WriteLine $"{message}"

            reraise ()

    /// The one type across every loaded assembly with this namespace and name. Deliberately a
    /// scan rather than a `BaseClassTypes` lookup, so that guest-declared types and corelib types
    /// are found the same way.
    let private typeInfo
        (state : IlMachineState)
        (ns : string)
        (name : string)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        state._LoadedAssemblies.DefinitionNames
        |> Seq.collect (fun assemblyName -> state._LoadedAssemblies.ByDefinitionName(assemblyName).TypeDefs.Values)
        |> Seq.filter (fun ty -> ty.Name = name && ty.Namespace = ns)
        |> Seq.toList
        |> function
            | [ ty ] -> ty
            | other -> failwith $"expected exactly one type %s{ns}.%s{name}, got %d{other.Length}"

    /// The handle of a non-generic type the guest caused to be concretized.
    let private nonGeneric (state : IlMachineState) (ns : string) (name : string) : ConcreteTypeHandle =
        let ty = typeInfo state ns name

        AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes ty.Identity
        |> Option.defaultWith (fun () -> failwith $"%s{ns}.%s{name} was never concretized by the guest")

    /// The handle of the instantiation `ns.name&lt;args&gt;`, looked up by identity rather than by
    /// walking any type's generic arguments: the point of the assertions below is that the
    /// projection lands on the same handle this independent lookup finds.
    let private instantiation
        (state : IlMachineState)
        (ns : string)
        (name : string)
        (args : ConcreteTypeHandle list)
        : ConcreteTypeHandle
        =
        let ty = typeInfo state ns name

        AllConcreteTypes.findExistingConcreteType state.ConcreteTypes ty.Identity (ImmutableArray.CreateRange args)
        |> Option.defaultWith (fun () -> failwith $"%s{ns}.%s{name} was never concretized at those arguments")

    let private arg0 (state : IlMachineState) (target : RuntimeTypeHandleTarget) : ConcreteTypeHandle =
        MethodTableProjection.instantiationArg0 state target

    let private refusalMessage (state : IlMachineState) (target : RuntimeTypeHandleTarget) : string =
        let exn = Assert.Throws (fun () -> arg0 state target |> ignore<ConcreteTypeHandle>)
        exn.Message

    [<Test>]
    let ``answers the first type argument, not the second`` () : unit =
        let state, _ = runGuest ()

        let int64Handle = nonGeneric state "System" "Int64"
        let byteHandle = nonGeneric state "System" "Byte"
        let stringHandle = nonGeneric state "System" "String"
        let int32Handle = nonGeneric state "System" "Int32"

        // Distinct, or the two rows below could not tell the indices apart.
        int64Handle |> shouldNotEqual stringHandle

        let pairLongByte = instantiation state "" "Pair`2" [ int64Handle ; byteHandle ]
        let pairStringInt = instantiation state "" "Pair`2" [ stringHandle ; int32Handle ]

        arg0 state (RuntimeTypeHandleTarget.Closed pairLongByte)
        |> shouldEqual int64Handle

        arg0 state (RuntimeTypeHandleTarget.Closed pairStringInt)
        |> shouldEqual stringHandle

    [<Test>]
    let ``answers for a single-argument instantiation`` () : unit =
        let state, _ = runGuest ()

        let int32Handle = nonGeneric state "System" "Int32"
        let nullableInt = instantiation state "System" "Nullable`1" [ int32Handle ]

        let listInt =
            instantiation state "System.Collections.Generic" "List`1" [ int32Handle ]

        arg0 state (RuntimeTypeHandleTarget.Closed nullableInt)
        |> shouldEqual int32Handle

        arg0 state (RuntimeTypeHandleTarget.Closed listInt) |> shouldEqual int32Handle

    [<Test>]
    let ``refuses a type with no instantiation`` () : unit =
        let state, _ = runGuest ()

        let int32Handle = nonGeneric state "System" "Int32"

        // CoreCLR would read off the end of an empty `Instantiation` here.
        refusalMessage state (RuntimeTypeHandleTarget.Closed int32Handle)
        |> shouldContainText "not a generic instantiation"

        // An array is parameterised without being a generic instantiation, so it reaches the same
        // dead end by a different route — and its element must not be mistaken for an argument.
        refusalMessage state (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.OneDimArrayZero int32Handle))
        |> shouldContainText "only a generic instantiation has an instantiation to index"

        refusalMessage state (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Byref int32Handle))
        |> shouldContainText "only a generic instantiation has an instantiation to index"

    [<Test>]
    let ``refuses targets that have no MethodTable`` () : unit =
        let state, _ = runGuest ()

        let pairIdentity = (typeInfo state "" "Pair`2").Identity

        refusalMessage state (RuntimeTypeHandleTarget.OpenGenericTypeDefinition pairIdentity)
        |> shouldContainText "InstantiationArg0 for open type"

        refusalMessage state (RuntimeTypeHandleTarget.GenericParameter (pairIdentity, 0))
        |> shouldContainText "generic parameters have no MethodTable"

        refusalMessage state (RuntimeTypeHandleTarget.DynamicMethodsClass pairIdentity.AssemblyFullName)
        |> shouldContainText "not generic"

    /// CoreLib's `MethodTable::NullableValueAddrOffset` field, as the projection's callers name it.
    let private nullableValueAddrOffsetField (state : IlMachineState) : FieldInfo<GenericParamFromMetadata, TypeDefn> =
        (typeInfo state "System.Runtime.CompilerServices" "MethodTable").Fields
        |> List.filter (fun f -> f.Name = "NullableValueAddrOffset")
        |> List.exactlyOne

    /// The underlying types the guest made `Nullable`1` instantiations of, as (namespace, name).
    let private nullablePayloads : obj[] list =
        [
            [| "System" ; "Byte" |]
            [| "System" ; "Int32" |]
            [| "System" ; "Int64" |]
            [| "System" ; "Double" |]
            [| "" ; "Colour" |]
            [| "" ; "TwoInts" |]
        ]

    /// CoreCLR asserts exactly this in `MethodTable::GetNullableNumInstanceFieldBytes`
    /// (RuntimeHelpers.CoreCLR.cs:956) and again where it fills the fields in
    /// (methodtablebuilder.cpp:10500): the payload offset plus the payload's size is the whole
    /// nullable. It ties `NullableValueAddrOffset` to two projections computed by entirely
    /// different code — `InstantiationArg0` for which type the payload is, and
    /// `GetNumInstanceFieldBytes` for how big things are — so an offset that drifted from
    /// PawPrint's own layout of `Nullable&lt;T&gt;` fails here rather than silently reading the
    /// wrong bytes.
    [<TestCaseSource(nameof nullablePayloads)>]
    let ``the payload offset and its size fill the nullable`` (ns : string) (name : string) : unit =
        let state, baseClassTypes = runGuest ()

        let payloadHandle = nonGeneric state ns name
        let nullableHandle = instantiation state "System" "Nullable`1" [ payloadHandle ]

        arg0 state (RuntimeTypeHandleTarget.Closed nullableHandle)
        |> shouldEqual payloadHandle

        let offsetField = nullableValueAddrOffsetField state

        let offset, state =
            match
                MethodTableProjection.tryProjectField
                    (LoggerFactory.makeTest () |> snd)
                    baseClassTypes
                    offsetField
                    nullableHandle
                    state
            with
            | Some (CliType.Numeric (CliNumericType.Int32 offset), state) -> offset, state
            | other -> failwith $"NullableValueAddrOffset projection produced %O{other}"

        // The has-value flag occupies byte zero, so the payload can never start there.
        offset |> shouldBeGreaterThan 0

        let payloadSize, state =
            MethodTableProjection.numInstanceFieldBytes baseClassTypes state payloadHandle

        let nullableSize, _state =
            MethodTableProjection.numInstanceFieldBytes baseClassTypes state nullableHandle

        (uint32 offset + payloadSize) |> shouldEqual nullableSize

    [<Test>]
    let ``the payload offset is refused for a type that is not a nullable`` () : unit =
        let state, baseClassTypes = runGuest ()

        let offsetField = nullableValueAddrOffsetField state

        // CoreCLR overlaps these bytes with the interface map, so on any other MethodTable they
        // are an unrelated pointer. No CoreLib caller asks — both readers assert `IsNullable`
        // first — so this is the guard that catches a new caller appearing, not a live gap.
        let pairHandle =
            instantiation state "" "Pair`2" [ nonGeneric state "System" "Int64" ; nonGeneric state "System" "Byte" ]

        let exn =
            Assert.Throws (fun () ->
                MethodTableProjection.tryProjectField
                    (LoggerFactory.makeTest () |> snd)
                    baseClassTypes
                    offsetField
                    pairHandle
                    state
                |> ignore<(CliType * IlMachineState) option>
            )

        exn.Message |> shouldContainText "not a System.Nullable`1 instantiation"
