namespace WoofWare.PawPrint.Test

open System.IO
open System.Reflection
open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `SharedNativeField.all` is exactly what the pinned `vm/corelib.h` of the runtime under test
/// lays more than one managed field out as, and names fields its CoreLib declares.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSharedNativeField =

    /// One `DEFINE_FIELD_U` row: the managed field it binds, and the native class and field it binds
    /// it to. `Conditional` is whether the row sits inside a preprocessor conditional, so that a
    /// build of the runtime might lack it.
    type private LayoutRow =
        {
            Managed : CoreLibInstanceField
            NativeClass : string
            NativeField : string
            Conditional : bool
        }

    /// The namespace a `DEFINE_CLASS_U` names by the token `corelib.h` pastes into `g_<token>NS`
    /// (`vm/namespace.h`).
    let private namespaceOfToken (token : string) : string =
        match token with
        | "System" -> "System"
        | "Reflection" -> "System.Reflection"
        | other -> $"<namespace token %s{other}>"

    let private classRow =
        Regex @"^DEFINE_CLASS_U\(\s*(\w+)\s*,\s*(\w+)\s*,\s*(\w+)\s*\)"

    let private fieldRow =
        Regex @"^DEFINE_FIELD_U\(\s*(\w+)\s*,\s*(\w+)\s*,\s*(\w+)\s*\)"

    /// Every `DEFINE_FIELD_U` row of `corelib.h`, each under the `DEFINE_CLASS_U` that precedes it.
    let private layoutRows (corelibH : string) : LayoutRow list =
        // The header opens with include guards of the form `#ifndef DEFINE_CLASS_U` around each
        // macro's default definition, which close before any row; every other conditional is one a
        // row could be inside.
        let mutable depth = 0
        let mutable currentClass : (string * string) option = None
        let rows = ResizeArray<LayoutRow> ()

        for line in File.ReadAllLines corelibH do
            let trimmed = line.Trim ()

            if trimmed.StartsWith "#if" then
                depth <- depth + 1
            elif trimmed.StartsWith "#endif" then
                depth <- depth - 1

            let classMatch = classRow.Match trimmed

            if classMatch.Success then
                currentClass <- Some (namespaceOfToken classMatch.Groups.[1].Value, classMatch.Groups.[2].Value)

            let fieldMatch = fieldRow.Match trimmed

            if fieldMatch.Success then
                match currentClass with
                | None -> failwith $"corelib.h has a DEFINE_FIELD_U row before any DEFINE_CLASS_U: %s{trimmed}"
                | Some (ns, typeName) ->
                    rows.Add
                        {
                            Managed =
                                {
                                    Namespace = ns
                                    TypeName = typeName
                                    FieldName = fieldMatch.Groups.[1].Value
                                }
                            NativeClass = fieldMatch.Groups.[2].Value
                            NativeField = fieldMatch.Groups.[3].Value
                            Conditional = depth > 0
                        }

        depth |> shouldEqual 0
        List.ofSeq rows

    let private corelibH () : string =
        Path.Combine (FrameworkUnderTest.requireRuntimeSource (), "src", "coreclr", "vm", "corelib.h")

    /// A table entry as a value with no order in it, for comparing tables.
    let private canonical (shared : SharedNativeField) : string * string * Set<CoreLibInstanceField> =
        shared.NativeClass, shared.NativeField, Set.ofList shared.ManagedFields

    [<Test>]
    let ``the table is every native field corelib.h lays more than one managed field out as`` () : unit =
        let rows = layoutRows (corelibH ())

        // The parse found the rows at all: every supported runtime's header has well over a hundred.
        rows.Length |> shouldBeGreaterThan 100

        let shared =
            rows
            |> List.groupBy (fun row -> row.NativeClass, row.NativeField)
            |> List.filter (fun (_, group) -> group.Length > 1)

        for (nativeClass, nativeField), group in shared do
            for row in group do
                if row.Conditional then
                    failwith
                        $"corelib.h lays %O{row.Managed} out as %s{nativeClass}::%s{nativeField} inside a preprocessor conditional, which SharedNativeField has no way to say"

        let fromHeader =
            shared
            |> List.map (fun ((nativeClass, nativeField), group) ->
                nativeClass, nativeField, group |> List.map (fun row -> row.Managed) |> Set.ofList
            )
            |> Set.ofList

        SharedNativeField.all
        |> List.map canonical
        |> Set.ofList
        |> shouldEqual fromHeader

    [<Test>]
    let ``each managed field is laid out as one native field, and each class has one of them`` () : unit =
        let managed =
            SharedNativeField.all |> List.collect (fun shared -> shared.ManagedFields)

        managed |> List.distinct |> shouldEqual managed

        for shared in SharedNativeField.all do
            let classes =
                shared.ManagedFields |> List.map (fun field -> field.Namespace, field.TypeName)

            classes |> List.distinct |> shouldEqual classes

    [<Test>]
    let ``every field the table names is an instance field of a sealed top-level CoreLib class`` () : unit =
        // Sealed, so that a receiver of one of the classes is exactly of that class: a subclass
        // would lay the field out at the same offset too, and the table would not name it.
        let corelib = TestIntrinsicBody.coreLib AuditedCoreLib.UnderTest

        SharedNativeField.all |> shouldNotEqual []

        for shared in SharedNativeField.all do
            for field in shared.ManagedFields do
                match corelib.TryGetTopLevelTypeDef field.Namespace field.TypeName with
                | None -> failwith $"CoreLib declares no top-level class %s{field.Namespace}.%s{field.TypeName}"
                | Some typeInfo ->
                    typeInfo.TypeAttributes.HasFlag TypeAttributes.Sealed |> shouldEqual true
                    typeInfo.Generics.IsEmpty |> shouldEqual true

                    typeInfo.Fields
                    |> List.filter (fun f -> f.Name = field.FieldName && not f.IsStatic)
                    |> List.length
                    |> shouldEqual 1

    [<Test>]
    let ``a field's counterpart on each other class of its native field is that class's field`` () : unit =
        SharedNativeField.all |> shouldNotEqual []

        for shared in SharedNativeField.all do
            for requested in shared.ManagedFields do
                for other in shared.ManagedFields do
                    let expected = if other = requested then None else Some other

                    SharedNativeField.counterpartOn requested other.Namespace other.TypeName
                    |> shouldEqual expected

    [<Test>]
    let ``a field has no counterpart on a class corelib.h lays out with no field of its native field`` () : unit =
        SharedNativeField.all |> shouldNotEqual []

        for shared in SharedNativeField.all do
            for requested in shared.ManagedFields do
                SharedNativeField.counterpartOn requested "System" "RuntimeType"
                |> shouldEqual None

        let unshared =
            {
                Namespace = "System"
                TypeName = "RuntimeMethodInfoStub"
                FieldName = "m_keepalive"
            }

        SharedNativeField.counterpartOn unshared "System.Reflection" "RuntimeMethodInfo"
        |> shouldEqual None
