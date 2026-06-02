namespace WoofWare.PawPrint.Test

open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open FsUnitTyped
open NUnit.Framework

/// Pin the set of `[Intrinsic]` static fields in `System.Private.CoreLib`. The JIT recognises
/// each via the closed `CORINFO_FIELD_INTRINSIC_*` enum at `src/coreclr/inc/corinfo.h` and
/// synthesises the load rather than reading the static slot. An IL interpreter like PawPrint
/// actually reads the slot, so the value present there has to be correct on its own — either
/// because a normal `.cctor` writes it, because the zero-initialised slot happens to be the
/// right value, or because PawPrint has special-cased the field. This test fails whenever a
/// new entry appears, forcing a deliberate audit.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestBclIntrinsicStaticFields =

    /// `[Intrinsic]` static fields we have audited:
    ///
    /// - `System.String::Empty` — declared without an initialiser. The real CLR's execution
    ///   engine populates it at startup; PawPrint has no equivalent hook, so the
    ///   `ldsfld`/`ldsflda` paths in `UnaryMetadataFieldOps` detect this specific field and
    ///   lazily intern the empty string. See `isSystemStringEmptyField` for the predicate.
    /// - `System.IntPtr::Zero` and `System.UIntPtr::Zero` — declared without an initialiser.
    ///   The JIT recognises these via `CORINFO_FIELD_INTRINSIC_ZERO` and synthesises a zero
    ///   load. PawPrint reads the static slot literally, and `CliType.zeroOfPrimitive`
    ///   populates the slot with `NativeIntSource.ManagedPointer Null` (an isomorphic but
    ///   distinct representation of nint zero). The comparison operators bridge the two
    ///   representations of zero: `EvalStackValueComparisons.cgtUn`/`cltUn` carry explicit
    ///   `Verbatim 0L` vs `ManagedPointer Null` arms (0 is the unsigned minimum), and
    ///   `equalsForCli` relates them via `isZero`, so the simple C# `zero != default(IntPtr)`
    ///   behaves correctly. The `IntPtrZero.cs` / `UIntPtrZero.cs` pure tests pin this
    ///   contract and pass.
    /// - `System.BitConverter::IsLittleEndian` — declared `= true` in the `!BIGENDIAN` build
    ///   (the only flavour PawPrint executes against). The normal `.cctor` populates it; the
    ///   `BitConverterIsLittleEndian.cs` pure test passes end-to-end. The `BIGENDIAN` build
    ///   would have the same shape of problem as `String::Empty`, but the runtime we depend
    ///   on never ships in that configuration.
    ///
    /// If this set changes — a new intrinsic field appears in a future runtime version, or an
    /// existing declaration changes shape — the JIT's `CORINFO_FIELD_INTRINSIC_*` enum will
    /// also have grown. Audit each new field, decide how PawPrint should handle it, then
    /// update this set.
    let private expectedIntrinsicStaticFields : Set<string> =
        Set.ofList
            [
                "System.BitConverter.IsLittleEndian"
                "System.IntPtr.Zero"
                "System.String.Empty"
                "System.UIntPtr.Zero"
            ]

    /// Returns true if the custom-attribute constructor lives on
    /// `System.Runtime.CompilerServices.IntrinsicAttribute`. We resolve the constructor's
    /// declaring type via either a `MemberReference` (when the attribute is defined in a
    /// different assembly) or a `MethodDefinition` (when defined in this assembly).
    let private isIntrinsicAttribute (md : MetadataReader) (attr : CustomAttribute) : bool =
        let ns, name =
            match attr.Constructor.Kind with
            | HandleKind.MemberReference ->
                let memberRef =
                    md.GetMemberReference (MemberReferenceHandle.op_Explicit attr.Constructor)

                match memberRef.Parent.Kind with
                | HandleKind.TypeReference ->
                    let typeRef = md.GetTypeReference (TypeReferenceHandle.op_Explicit memberRef.Parent)

                    md.GetString typeRef.Namespace, md.GetString typeRef.Name
                | HandleKind.TypeDefinition ->
                    let typeDef =
                        md.GetTypeDefinition (TypeDefinitionHandle.op_Explicit memberRef.Parent)

                    md.GetString typeDef.Namespace, md.GetString typeDef.Name
                | _ -> "", ""
            | HandleKind.MethodDefinition ->
                let methodDef =
                    md.GetMethodDefinition (MethodDefinitionHandle.op_Explicit attr.Constructor)

                let typeDef = md.GetTypeDefinition (methodDef.GetDeclaringType ())
                md.GetString typeDef.Namespace, md.GetString typeDef.Name
            | _ -> "", ""

        ns = "System.Runtime.CompilerServices" && name = "IntrinsicAttribute"

    [<Test>]
    let ``[Intrinsic] static fields in System.Private.CoreLib match the audited set`` () : unit =
        let corelibPath = typeof<obj>.Assembly.Location

        use fs = File.OpenRead corelibPath
        use peReader = new PEReader (fs)
        let md = peReader.GetMetadataReader ()

        let fields =
            md.TypeDefinitions
            |> Seq.collect (fun typeHandle ->
                let typeDef = md.GetTypeDefinition typeHandle

                typeDef.GetFields ()
                |> Seq.choose (fun fieldHandle ->
                    let field = md.GetFieldDefinition fieldHandle

                    if not (field.Attributes.HasFlag FieldAttributes.Static) then
                        None
                    else
                        let isIntrinsic =
                            field.GetCustomAttributes ()
                            |> Seq.exists (fun attrHandle ->
                                isIntrinsicAttribute md (md.GetCustomAttribute attrHandle)
                            )

                        if isIntrinsic then
                            let ns = md.GetString typeDef.Namespace
                            let typeName = md.GetString typeDef.Name

                            let qualifiedType = if ns = "" then typeName else $"%s{ns}.%s{typeName}"

                            Some $"%s{qualifiedType}.%s{md.GetString field.Name}"
                        else
                            None
                )
            )
            |> Set.ofSeq

        fields |> shouldEqual expectedIntrinsicStaticFields
