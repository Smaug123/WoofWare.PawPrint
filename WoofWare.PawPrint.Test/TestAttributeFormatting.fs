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
    let ``formatFixedArg R4 and R8 round-trip their stored value`` () : unit =
        // Pick a value that "%g" would truncate at its default precision; the rendered string
        // must reparse to the exact original bits.
        let v8 = 1.234567890123456
        let rendered8 = AttributeFormatting.formatFixedArg (CustomAttribFixedArg.R8 v8)

        let parsed8 =
            System.Double.Parse (rendered8, System.Globalization.CultureInfo.InvariantCulture)

        parsed8 |> shouldEqual v8

        let v4 = 1.2345678f
        let rendered4 = AttributeFormatting.formatFixedArg (CustomAttribFixedArg.R4 v4)
        rendered4.EndsWith "f" |> shouldEqual true

        let parsed4 =
            System.Single.Parse (
                rendered4.Substring (0, rendered4.Length - 1),
                System.Globalization.CultureInfo.InvariantCulture
            )

        parsed4 |> shouldEqual v4

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

    // ----- field header: static-ness and explicit layout ---------------------

    [<Test>]
    let ``fieldHeader marks a static field static, mirroring methodHeader`` () : unit =
        let str = findTypeByName "System.String"

        let empty = str.Fields |> List.find (fun f -> f.Name = "Empty")

        // The signature rendering is TypeDefn's business; assert only the prefix
        // this header controls.
        AttributeFormatting.fieldHeader "System.String" empty
        |> fun h -> h.StartsWith "// field System.String::static Empty : " |> shouldEqual true

    [<Test>]
    let ``fieldHeader omits the static marker for an instance field`` () : unit =
        let str = findTypeByName "System.String"

        let length = str.Fields |> List.find (fun f -> f.Name = "_stringLength")

        AttributeFormatting.fieldHeader "System.String" length
        |> fun h -> h.StartsWith "// field System.String::_stringLength : " |> shouldEqual true

    [<Test>]
    let ``fieldHeader reports an explicit field offset`` () : unit =
        // Search corelib for any explicitly-laid-out field rather than hard-coding a
        // type whose layout may change between servicing releases.
        let found =
            corelib.TypeDefs.Values
            |> Seq.collect (fun td ->
                td.Fields
                |> Seq.choose (fun f ->
                    match f.Offset with
                    | None -> None
                    | Some offset -> Some (IlFormatting.qualifyTypeName corelib.TypeDefs td, f, offset)
                )
            )
            |> Seq.sortBy (fun (qualified, f, _) -> qualified, f.Name)
            |> Seq.tryHead

        match found with
        | None -> Assert.Inconclusive "corelib declares no explicitly-laid-out fields; nothing to exercise"
        | Some (qualified, field, offset) ->

        let header = AttributeFormatting.fieldHeader qualified field
        header.EndsWith (sprintf " @ 0x%X" offset) |> shouldEqual true

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

    [<Test>]
    let ``formatAttributeApplication decodes a closed-generic ctor whose param is the type parameter`` () : unit =
        // ms.ParameterTypes on the MemberRef references the open generic via
        // GenericTypeParameter 0. Without substitution, the blob decoder hits the
        // unsupported generic-param case and we'd fall back to a raw hex blob — even
        // though the TypeSpec parent already pins T = int.
        let source =
            """
using System;

[AttributeUsage(AttributeTargets.All)]
public class MyGenericAttribute<T> : Attribute
{
    public MyGenericAttribute(T value) { }
}

[MyGeneric<int>(42)]
public class Target { }
"""

        let image =
            Roslyn.compileAssembly
                "GenericAttributeFormattingClosedCtorTest"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ source ]

        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = new System.IO.MemoryStream (image)
        let assembly = global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None stream

        let target = assembly.TypeDefs.Values |> Seq.find (fun td -> td.Name = "Target")

        let rendered =
            AttributeFormatting.attributesFor assembly (MetadataToken.TypeDefinition target.TypeDefHandle)
            |> List.map (AttributeFormatting.formatAttributeApplication assembly)

        // The decoded form must show the literal 42 — not a "/* blob:" fallback.
        let found =
            rendered
            |> List.exists (fun r -> r.Contains "(42)" && not (r.Contains "/* blob:"))

        if not found then
            failwithf "actual rendered applications: %s" (rendered |> String.concat " ; ")

    [<Test>]
    let ``attributeTypeName renders user-defined type argument by qualified name`` () : unit =
        // The args of a generic-attribute instantiation are themselves TypeDefns;
        // user-defined ones must be routed through the same resolver as the head,
        // otherwise they collapse to "<type defined in ...>".
        let source =
            """
using System;

public class ArgType { }

[AttributeUsage(AttributeTargets.All)]
public class MyGenericAttribute<T> : Attribute { }

[MyGeneric<ArgType>]
public class Target { }
"""

        let image =
            Roslyn.compileAssembly
                "GenericAttributeFormattingUserArgTest"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ source ]

        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = new System.IO.MemoryStream (image)
        let assembly = global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None stream

        let target = assembly.TypeDefs.Values |> Seq.find (fun td -> td.Name = "Target")

        let names =
            AttributeFormatting.attributesFor assembly (MetadataToken.TypeDefinition target.TypeDefHandle)
            |> List.map (AttributeFormatting.attributeTypeName assembly)

        let found =
            names
            |> List.exists (fun n -> n.EndsWith ("MyGeneric<ArgType>", System.StringComparison.Ordinal))

        if not found then
            failwithf "actual rendered names: %s" (names |> String.concat " ; ")

        for n in names do
            n.Contains "<type defined in" |> shouldEqual false

    // ----- assembly- and module-scoped attributes ---------------------------

    [<Test>]
    let ``renderOwnerLines emits assembly-scoped attributes for AssemblyDefinition token`` () : unit =
        // [assembly: ...] attributes are stored in the metadata reader's
        // CustomAttributes table with parent token AssemblyDefinition (row 1).
        // The attrs-only walker must look them up via that singleton token, not
        // skip over them because they aren't owned by any TypeDef.
        let source =
            """
using System.Runtime.CompilerServices;

[assembly: InternalsVisibleTo("SomeFriendAssembly")]

public class Placeholder { }
"""

        let image =
            Roslyn.compileAssembly
                "AssemblyScopedAttributeFormattingTest"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ source ]

        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = new System.IO.MemoryStream (image)
        let assembly = global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None stream

        let header = AttributeFormatting.assemblyHeader assembly

        let lines =
            AttributeFormatting.renderOwnerLines
                assembly
                header
                (MetadataToken.AssemblyDefinition System.Reflection.Metadata.EntityHandle.AssemblyDefinition)

        // The header should be the first line, and at least one rendered application line
        // should mention the friend assembly name from the InternalsVisibleTo attribute.
        match lines with
        | [] -> failwith "expected at least the assembly header plus one InternalsVisibleTo line"
        | first :: rest ->
            first |> shouldEqual header

            let mentionsFriend = rest |> List.exists (fun l -> l.Contains "SomeFriendAssembly")

            if not mentionsFriend then
                failwithf "actual rendered lines: %s" (lines |> String.concat " | ")
