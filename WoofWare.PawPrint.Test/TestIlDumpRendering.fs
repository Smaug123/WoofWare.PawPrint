namespace WoofWare.PawPrint.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Tests for the default (non-`--attrs-only`) IlDump rendering: the whole-type
/// dump that IlDump's Mode.Default prints for each type matching the filters.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestIlDumpRendering =

    // The factory is intentionally undisposed: the returned DumpedAssembly.Logger closes
    // over its sinks, and disposing while the assembly is still live would silently drop
    // events.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private findTypeByName (qualified : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        corelib.TypeDefs.Values
        |> Seq.find (fun td -> IlFormatting.qualifyTypeName corelib.TypeDefs td = qualified)

    /// The filter shape IlDump builds when the user supplies both a type and a
    /// member argument on the command line.
    let private filterOn (typeName : string) (memberName : string option) : IlDumpFilter =
        {
            Type = Some typeName
            Member = memberName
        }

    // ----- filter construction from command-line arguments -------------------

    [<Test>]
    let ``an empty filter argument means no narrowing`` () : unit =
        // "ildump foo.dll '' SomeMember" is the only way to spell a member search
        // across every type, so the empty type argument must not count as having
        // narrowed by type.
        IlDumpFilter.make (Some "") (Some "Foo")
        |> shouldEqual
            {
                Type = None
                Member = Some "Foo"
            }

        IlDumpFilter.make (Some "System") (Some "")
        |> shouldEqual
            {
                Type = Some "System"
                Member = None
            }

    // ----- the regression from issue #700 ------------------------------------

    [<Test>]
    let ``a field-name member filter finds the field`` () : unit =
        // Before the fix, Mode.Default iterated only Methods, so filtering a type
        // by one of its field names emitted nothing at all — making a data-only
        // type look as though it did not exist.
        let ty = findTypeByName "System.GCMemoryInfoData"

        let lines =
            IlDumpRendering.formatTypeLines corelib (filterOn "GCMemoryInfoData" (Some "_heapSizeBytes")) ty

        lines
        |> List.exists (fun l -> l.StartsWith ("// field ", System.StringComparison.Ordinal))
        |> shouldEqual true

        lines
        |> List.exists (fun l -> l.Contains "System.GCMemoryInfoData::" && l.Contains "_heapSizeBytes")
        |> shouldEqual true

    [<Test>]
    let ``every declared field is visible in an unfiltered type dump`` () : unit =
        let ty = findTypeByName "System.GCMemoryInfoData"

        // Guard the test itself: this fixture is worthless if the type has no fields.
        ty.Fields |> List.isEmpty |> shouldEqual false

        let lines =
            IlDumpRendering.formatTypeLines corelib (filterOn "GCMemoryInfoData" None) ty

        let rendered = String.concat "\n" lines

        for field in ty.Fields do
            if not (rendered.Contains field.Name) then
                failwithf "field %s was not rendered in the type dump" field.Name

    // ----- type header policy ------------------------------------------------

    [<Test>]
    let ``a type filter that matches no member still reports the type's existence`` () : unit =
        let ty = findTypeByName "System.GCMemoryInfoData"

        let lines =
            IlDumpRendering.formatTypeLines corelib (filterOn "GCMemoryInfoData" (Some "NoSuchMemberName")) ty

        // Exactly the type header: enough to distinguish "type exists, no such
        // member" from "no such type".
        lines |> shouldEqual [ "// type System.GCMemoryInfoData" ]

    [<Test>]
    let ``with no type filter, a type whose members all fail the filter emits nothing`` () : unit =
        // A bare member search across a whole assembly must not emit a header for
        // every one of the assembly's thousands of types.
        let ty = findTypeByName "System.GCMemoryInfoData"

        let filter =
            {
                Type = None
                Member = Some "NoSuchMemberName"
            }

        IlDumpRendering.formatTypeLines corelib filter ty |> shouldEqual []

    [<Test>]
    let ``with no type filter, a type with a matching member is still headed`` () : unit =
        let ty = findTypeByName "System.GCMemoryInfoData"

        let filter =
            {
                Type = None
                Member = Some "_heapSizeBytes"
            }

        let lines = IlDumpRendering.formatTypeLines corelib filter ty
        lines |> List.isEmpty |> shouldEqual false
        lines.Head |> shouldEqual "// type System.GCMemoryInfoData"

    // ----- methods keep their IL bodies --------------------------------------

    [<Test>]
    let ``methods are still dumped with their IL`` () : unit =
        let ty = findTypeByName "System.GCMemoryInfoData"

        let lines =
            IlDumpRendering.formatTypeLines corelib (filterOn "GCMemoryInfoData" (Some "get_GenerationInfoAsSpan")) ty

        lines
        |> List.exists (fun l -> l.Contains "get_GenerationInfoAsSpan")
        |> shouldEqual true

        lines |> List.exists (fun l -> l.Contains "IL_0000:") |> shouldEqual true

    // ----- properties and events ---------------------------------------------

    [<Test>]
    let ``properties are listed`` () : unit =
        // System.Exception has properties (Message, StackTrace, ...) but the
        // property rows themselves are only reachable via the MetadataReader.
        let ty = findTypeByName "System.Exception"

        let lines =
            IlDumpRendering.formatTypeLines corelib (filterOn "System.Exception" (Some "Message")) ty

        lines
        |> List.exists (fun l -> l = "// property System.Exception::Message")
        |> shouldEqual true

    [<Test>]
    let ``events are listed`` () : unit =
        let ty =
            corelib.TypeDefs.Values
            |> Seq.filter (fun td -> not (Seq.isEmpty td.Events))
            |> Seq.sortBy (fun td -> IlFormatting.qualifyTypeName corelib.TypeDefs td)
            |> Seq.tryHead

        match ty with
        | None -> Assert.Inconclusive "corelib declares no events; nothing to exercise"
        | Some ty ->

        let qualified = IlFormatting.qualifyTypeName corelib.TypeDefs ty
        let evt = Seq.head ty.Events

        let lines =
            IlDumpRendering.formatTypeLines corelib (filterOn qualified (Some evt.Name)) ty

        lines
        |> List.exists (fun l -> l = sprintf "// event %s::%s" qualified evt.Name)
        |> shouldEqual true

    // ----- the invariant, over a deterministic sample of corelib -------------

    [<Test>]
    let ``no declared member of any sampled type is invisible`` () : unit =
        // The property the fix is really asserting: for any type, an unfiltered
        // dump mentions every field, method and event the type declares. Sampled
        // with a fixed stride so the test is deterministic and doesn't format the
        // IL of all of corelib.
        let sample =
            corelib.TypeDefs.Values
            |> Seq.sortBy (fun td -> IlFormatting.qualifyTypeName corelib.TypeDefs td)
            |> Seq.indexed
            |> Seq.filter (fun (i, _) -> i % 250 = 0)
            |> Seq.map snd
            |> Seq.toList

        sample |> List.isEmpty |> shouldEqual false

        for ty in sample do
            let qualified = IlFormatting.qualifyTypeName corelib.TypeDefs ty

            let rendered =
                IlDumpRendering.formatTypeLines corelib (filterOn qualified None) ty
                |> String.concat "\n"

            for field in ty.Fields do
                if not (rendered.Contains field.Name) then
                    failwithf "field %s::%s was not rendered" qualified field.Name

            for method in ty.Methods do
                if not (rendered.Contains method.Name) then
                    failwithf "method %s::%s was not rendered" qualified method.Name

            for evt in ty.Events do
                if not (rendered.Contains evt.Name) then
                    failwithf "event %s::%s was not rendered" qualified evt.Name
