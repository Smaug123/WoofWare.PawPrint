namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestAttributeFormatting =

    // The factory is intentionally undisposed: the returned DumpedAssembly.Logger closes
    // over its sinks, and disposing while the assembly is still live would silently drop
    // events.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    // ----- formatFixedArg (pure unit tests) ---------------------------------

    [<Test>]
    let ``formatFixedArg Bool true / false`` () : unit =
        AttributeFormatting.formatFixedArg (CustomAttribFixedArg.Bool true)
        |> shouldEqual "true"

        AttributeFormatting.formatFixedArg (CustomAttribFixedArg.Bool false)
        |> shouldEqual "false"

    [<Test>]
    let ``formatFixedArg Char escapes specials`` () : unit =
        AttributeFormatting.formatFixedArg (CustomAttribFixedArg.Char 'A')
        |> shouldEqual "'A'"

        AttributeFormatting.formatFixedArg (CustomAttribFixedArg.Char '\n')
        |> shouldEqual "'\\n'"

    [<Test>]
    let ``formatFixedArg Char escapes single quote unambiguously`` () : unit =
        // Without escape, this would render as the ambiguous '''.
        AttributeFormatting.formatFixedArg (CustomAttribFixedArg.Char '\'')
        |> shouldEqual "'\\''"

        // Backslash must still be escaped (and not double-escaped).
        AttributeFormatting.formatFixedArg (CustomAttribFixedArg.Char '\\')
        |> shouldEqual "'\\\\'"

    [<Test>]
    let ``formatFixedArg integers carry suffixes`` () : unit =
        AttributeFormatting.formatFixedArg (CustomAttribFixedArg.I1 -1y)
        |> shouldEqual "-1y"

        AttributeFormatting.formatFixedArg (CustomAttribFixedArg.U1 255uy)
        |> shouldEqual "255uy"

        AttributeFormatting.formatFixedArg (CustomAttribFixedArg.I2 -2s)
        |> shouldEqual "-2s"

        AttributeFormatting.formatFixedArg (CustomAttribFixedArg.U2 7us)
        |> shouldEqual "7us"

        AttributeFormatting.formatFixedArg (CustomAttribFixedArg.I4 42)
        |> shouldEqual "42"

        AttributeFormatting.formatFixedArg (CustomAttribFixedArg.U4 4u)
        |> shouldEqual "4u"

        AttributeFormatting.formatFixedArg (CustomAttribFixedArg.I8 -9L)
        |> shouldEqual "-9L"

        AttributeFormatting.formatFixedArg (CustomAttribFixedArg.U8 9UL)
        |> shouldEqual "9uL"

    [<Test>]
    let ``formatFixedArg String None is null and String Some is quoted-escaped`` () : unit =
        AttributeFormatting.formatFixedArg (CustomAttribFixedArg.String None)
        |> shouldEqual "null"

        AttributeFormatting.formatFixedArg (CustomAttribFixedArg.String (Some ""))
        |> shouldEqual "\"\""

        AttributeFormatting.formatFixedArg (CustomAttribFixedArg.String (Some "a\"b"))
        |> shouldEqual "\"a\\\"b\""

    [<Test>]
    let ``formatFixedArg Array None is null; empty is empty braces; non-empty is comma-separated`` () : unit =
        AttributeFormatting.formatFixedArg (CustomAttribFixedArg.Array None)
        |> shouldEqual "null"

        AttributeFormatting.formatFixedArg (CustomAttribFixedArg.Array (Some []))
        |> shouldEqual "{  }"

        AttributeFormatting.formatFixedArg (
            CustomAttribFixedArg.Array (Some [ CustomAttribFixedArg.I4 1 ; CustomAttribFixedArg.I4 2 ])
        )
        |> shouldEqual "{ 1, 2 }"

    [<Test>]
    let ``formatFixedArg Array nests`` () : unit =
        let arg =
            CustomAttribFixedArg.Array (
                Some
                    [
                        CustomAttribFixedArg.Array (Some [ CustomAttribFixedArg.U1 1uy ; CustomAttribFixedArg.U1 2uy ])
                        CustomAttribFixedArg.Array None
                    ]
            )

        AttributeFormatting.formatFixedArg arg |> shouldEqual "{ { 1uy, 2uy }, null }"

    // ----- attributeTypeName: strips "Attribute" suffix ---------------------

    let private findTypeByName (qualified : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        corelib.TypeDefs.Values
        |> Seq.find (fun td ->
            let n = IlFormatting.qualifyTypeName corelib.TypeDefs td
            n = qualified
        )

    [<Test>]
    let ``attributeTypeName strips Attribute suffix from corelib SerializableAttribute`` () : unit =
        // System.SerializableAttribute is applied to System.Exception. Find its application.
        let exn = findTypeByName "System.Exception"

        let attrs =
            AttributeFormatting.attributesFor corelib (MetadataToken.TypeDefinition exn.TypeDefHandle)

        let names = attrs |> List.map (AttributeFormatting.attributeTypeName corelib)

        // Every rendered name should retain its namespace, and none should retain the literal "Attribute" suffix.
        for name in names do
            if name.EndsWith ("Attribute", System.StringComparison.Ordinal) then
                failwithf "attributeTypeName did not strip Attribute suffix from %s" name

        // TypeForwardedFromAttribute is one of the attributes applied; check that it's rendered with the
        // namespace intact (i.e., dot-separated, no leading dot).
        names
        |> List.exists (fun n -> n = "System.Runtime.CompilerServices.TypeForwardedFrom")
        |> shouldEqual true

    [<Test>]
    let ``attributeTypeName keeps bare "Attribute" intact (not stripped to empty)`` () : unit =
        // We can't easily construct a CustomAttribute pointing at the literal "Attribute" type
        // without scaffolding, but the suffix-stripping helper's contract is testable directly
        // by exercising attributeTypeName on attributes from corelib and checking no empty name
        // appears (which is what the suffix-stripper would produce if it stripped from a name
        // equal to "Attribute").
        for kvp in corelib.TypeDefs do
            let attrs =
                AttributeFormatting.attributesFor corelib (MetadataToken.TypeDefinition kvp.Value.TypeDefHandle)

            for attr in attrs do
                let name = AttributeFormatting.attributeTypeName corelib attr

                if name = "" then
                    failwithf "attributeTypeName produced empty string for an attribute on %O" kvp.Value

    // ----- formatAttributeApplication: end-to-end against corelib -----------

    [<Test>]
    let ``System.Exception has [Serializable]-style attribute applications`` () : unit =
        let exn = findTypeByName "System.Exception"

        let attrs =
            AttributeFormatting.attributesFor corelib (MetadataToken.TypeDefinition exn.TypeDefHandle)

        let rendered =
            attrs |> List.map (AttributeFormatting.formatAttributeApplication corelib)

        // Every rendering should be a [..] application.
        for r in rendered do
            r.StartsWith ('[') |> shouldEqual true
            r.EndsWith (']') |> shouldEqual true

        // TypeForwardedFrom is present with a string arg.
        rendered
        |> List.exists (fun r -> r.Contains "[System.Runtime.CompilerServices.TypeForwardedFrom(\"mscorlib")
        |> shouldEqual true

    [<Test>]
    let ``formatAttributeApplication emits hex blob fallback when decoding fails`` () : unit =
        // The reader doesn't yet handle ENUM/TYPE/TAGGED_OBJECT ctor args, so any attribute whose
        // ctor takes one (e.g. anything with an enum) lands in the fallback path. Corelib has many
        // such attributes; search for the first one rather than pinning to a specific ctor that
        // could be reorganised between dotnet releases.
        let allRenderings =
            seq {
                for kvp in corelib.TypeDefs do
                    let ti = kvp.Value

                    for m in ti.Methods do
                        for attr in AttributeFormatting.attributesFor corelib (MetadataToken.MethodDef m.Handle) do
                            yield AttributeFormatting.formatAttributeApplication corelib attr
            }

        allRenderings
        |> Seq.exists (fun r -> r.Contains "(/* blob:")
        |> shouldEqual true

    [<Test>]
    let ``formatAttributeApplication surfaces named-args count`` () : unit =
        // System.Exception::OnDeserialized and ::.ctor(...) have attribute applications with named args.
        // Find at least one rendering that contains the "+N named" annotation.
        let exn = findTypeByName "System.Exception"

        let allRenderings =
            seq {
                for m in exn.Methods do
                    for attr in AttributeFormatting.attributesFor corelib (MetadataToken.MethodDef m.Handle) do
                        yield AttributeFormatting.formatAttributeApplication corelib attr
            }
            |> Seq.toList

        allRenderings
        |> List.exists (fun r -> r.Contains "named */]")
        |> shouldEqual true

    // ----- type header includes generics -------------------------------------

    [<Test>]
    let ``typeHeader for generic List<T> includes the <T> clause`` () : unit =
        let listOfT = findTypeByName "System.Collections.Generic.List`1"
        let header = AttributeFormatting.typeHeader corelib listOfT

        header.StartsWith "// type System.Collections.Generic.List`1<"
        |> shouldEqual true

        header.EndsWith ">" |> shouldEqual true

    [<Test>]
    let ``typeHeader for non-generic Exception omits any angle-bracket clause`` () : unit =
        let exn = findTypeByName "System.Exception"
        let header = AttributeFormatting.typeHeader corelib exn
        header |> shouldEqual "// type System.Exception"

    // ----- renderOwnerLines: skips empty owners ------------------------------

    [<Test>]
    let ``renderOwnerLines returns empty for a token with no attributes`` () : unit =
        // CustomAttributeHandle 0 is a row that never exists in the index (rows are 1-based).
        // Build a token whose parent never has attributes: use a TypeDefinition handle with row
        // 0xFFFFFF, which won't appear in the index.
        let bogus =
            MetadataToken.TypeDefinition (
                System.Reflection.Metadata.Ecma335.MetadataTokens.TypeDefinitionHandle 0xFFFFFF
            )

        AttributeFormatting.renderOwnerLines corelib "// type Bogus" bogus
        |> shouldEqual []

    [<Test>]
    let ``renderOwnerLines emits header + one indented line per attribute`` () : unit =
        let exn = findTypeByName "System.Exception"

        let lines =
            AttributeFormatting.renderOwnerLines
                corelib
                "// type System.Exception"
                (MetadataToken.TypeDefinition exn.TypeDefHandle)

        // First line is the header; subsequent lines are indented attribute applications.
        lines.Head |> shouldEqual "// type System.Exception"

        for line in List.tail lines do
            line.StartsWith "//   [" |> shouldEqual true
            line.EndsWith "]" |> shouldEqual true

    [<Test>]
    let ``attributesFor returns no attributes for owners outside the index`` () : unit =
        // Use a fresh CustomAttributeHandle which is unlikely to be in the dictionary.
        let bogus =
            MetadataToken.MethodDef (System.Reflection.Metadata.Ecma335.MetadataTokens.MethodDefinitionHandle 0xFFFFFF)

        AttributeFormatting.attributesFor corelib bogus |> shouldEqual []

    // ----- generic attribute applications (TypeSpec ctor parent) -----------------

    [<Test>]
    let ``attributeTypeName renders generic attribute with its type argument and strips Attribute suffix`` () : unit =
        // A generic attribute's ctor lives on a MemberRef whose Parent is a TypeSpecification;
        // without TypeSpec handling, the rendered name comes from TypeDefn.ToString and loses
        // the attribute's actual name.
        let source =
            """
using System;

[AttributeUsage(AttributeTargets.All)]
public class MyGenericAttribute<T> : Attribute { }

[MyGeneric<string>]
public class Target { }
"""

        let image =
            Roslyn.compileAssembly
                "GenericAttributeFormattingTest"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ source ]

        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = new System.IO.MemoryStream (image)
        let assembly = global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None stream

        let target = assembly.TypeDefs.Values |> Seq.find (fun td -> td.Name = "Target")

        let attrs =
            AttributeFormatting.attributesFor assembly (MetadataToken.TypeDefinition target.TypeDefHandle)

        let names = attrs |> List.map (AttributeFormatting.attributeTypeName assembly)

        // We should see exactly one attribute application on Target, and its rendered name
        // must contain the simple "MyGeneric<...>" (suffix stripped) — not "<type defined in ...>".
        let found =
            names
            |> List.exists (fun n -> n.EndsWith ("MyGeneric<string>", System.StringComparison.Ordinal))

        if not found then
            failwithf "actual rendered names: %s" (names |> String.concat " ; ")

        for n in names do
            n.Contains "<type defined in" |> shouldEqual false
