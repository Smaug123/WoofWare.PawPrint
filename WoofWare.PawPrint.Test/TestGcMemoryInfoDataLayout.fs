namespace WoofWare.PawPrint.Test

open System.IO
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open Microsoft.FSharp.Reflection
open NUnit.Framework
open WoofWare.PawPrint

/// `GC.GetMemoryInfo`'s native filler writes `System.GCMemoryInfoData` only in a layout
/// `GcMemoryInfoDataLayout.classify` recognises, and refuses every other.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestGcMemoryInfoDataLayout =

    [<Test>]
    let ``known lists every layout`` () =
        let cases =
            FSharpType.GetUnionCases typeof<GcMemoryInfoDataLayout>
            |> Array.map (fun case -> case.Name)
            |> Set.ofArray

        GcMemoryInfoDataLayout.known
        |> List.map (fun layout -> (FSharpValue.GetUnionFields (layout, typeof<GcMemoryInfoDataLayout>) |> fst).Name)
        |> Set.ofList
        |> shouldEqual cases

        GcMemoryInfoDataLayout.known
        |> List.distinct
        |> shouldEqual GcMemoryInfoDataLayout.known

    [<Test>]
    let ``each known layout classifies as itself`` () =
        for layout in GcMemoryInfoDataLayout.known do
            GcMemoryInfoDataLayout.classify (GcMemoryInfoDataLayout.fieldNames layout)
            |> shouldEqual (Some layout)

    /// One edit of a list of field names.
    type private Edit =
        | Delete of int
        | Duplicate of int
        | Swap of int
        | Insert of int * string
        | Rename of int * string

    let private apply (names : string list) (edit : Edit) : string list =
        let at (i : int) : int = i % names.Length

        match edit with
        | Edit.Delete i -> List.removeAt (at i) names
        | Edit.Duplicate i -> List.insertAt (at i) names.[at i] names
        | Edit.Swap i ->
            let i = at i
            let j = (i + 1) % names.Length
            names |> List.updateAt i names.[j] |> List.updateAt j names.[i]
        | Edit.Insert (i, name) -> List.insertAt (at i) name names
        | Edit.Rename (i, name) -> List.updateAt (at i) name names

    [<Test>]
    let ``every layout but a known one is refused`` () =
        // Names to insert or rename to: the other layout's fields, and near misses of them.
        let alphabet =
            GcMemoryInfoDataLayout.known
            |> List.collect GcMemoryInfoDataLayout.fieldNames
            |> List.distinct
            |> List.append [ "_generationInfo5" ; "_pauseDuration2" ; "_generationInfos" ; "" ]

        let gen =
            gen {
                let! layout = Gen.elements GcMemoryInfoDataLayout.known
                let! i = Gen.choose (0, 1000)
                let! name = Gen.elements alphabet

                let! edit =
                    Gen.elements
                        [
                            Edit.Delete i
                            Edit.Duplicate i
                            Edit.Swap i
                            Edit.Insert (i, name)
                            Edit.Rename (i, name)
                        ]

                let! more = Gen.listOf (Gen.elements [ Edit.Delete (i / 2) ; Edit.Swap (i / 3) ])
                return layout, edit :: List.truncate 3 more
            }

        let property (layout : GcMemoryInfoDataLayout, edits : Edit list) =
            let names = List.fold apply (GcMemoryInfoDataLayout.fieldNames layout) edits

            let expected =
                GcMemoryInfoDataLayout.known
                |> List.tryFind (fun known -> GcMemoryInfoDataLayout.fieldNames known = names)

            GcMemoryInfoDataLayout.classify names |> shouldEqual expected

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 2000, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``a refusal names the known layouts`` () =
        GcMemoryInfoDataLayout.classify [ "_index" ] |> shouldEqual None

        for layout in GcMemoryInfoDataLayout.known do
            for name in GcMemoryInfoDataLayout.fieldNames layout do
                GcMemoryInfoDataLayout.describeKnown |> shouldContainText name

    let runtimes : TestCaseData list = FrameworkUnderTest.everyRuntime

    [<TestCaseSource(nameof runtimes)>]
    let ``each runtime's CoreLib declares that runtime's layout`` (runtime : EmulatedRuntime) =
        let corelibPath = FrameworkUnderTest.requireCoreLibPathOf runtime
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        use stream = File.OpenRead corelibPath
        let corelib = Assembly.read loggerFactory (Some corelibPath) stream

        let data =
            corelib.TryGetTopLevelTypeDef "System" "GCMemoryInfoData"
            |> Option.defaultWith (fun () -> failwith "CoreLib declares no System.GCMemoryInfoData")

        let expected =
            match runtime with
            | EmulatedRuntime.Net10 -> GcMemoryInfoDataLayout.SeparateFields
            | EmulatedRuntime.Net11 -> GcMemoryInfoDataLayout.InlineArrays

        data.Fields
        |> List.filter (fun field -> not field.IsStatic)
        |> List.map (fun field -> field.Name)
        |> GcMemoryInfoDataLayout.classify
        |> shouldEqual (Some expected)
