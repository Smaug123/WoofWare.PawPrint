namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// <summary>
/// Tests for <see cref="PropertySignatureDecoding" />, the decoder for an ECMA-335 II.23.2.5
/// PropertySig blob.
/// </summary>
/// <remarks>
/// The oracle is the host CLR reading the same image: <c>PropertyInfo.PropertyType</c> and
/// <c>GetIndexParameters()</c> are what CoreCLR derived from these very rows, so they are an
/// answer arrived at without consulting this decoder.
/// </remarks>
[<TestFixture>]
module TestPropertySignatureDecoding =

    let private corelibReader : PEReader =
        new PEReader (File.OpenRead typeof<obj>.Assembly.Location)

    let private metadataReader : MetadataReader = corelibReader.GetMetadataReader ()

    let private assemblyName : AssemblyName =
        metadataReader.GetAssemblyDefinition().GetAssemblyName ()

    let private decode (signature : BlobHandle) : MethodSignature<TypeDefn> =
        PropertySignatureDecoding.decode assemblyName metadataReader signature

    let private propertyHandleOf (property : PropertyInfo) : PropertyDefinitionHandle =
        MetadataTokens.PropertyDefinitionHandle (property.MetadataToken &&& 0x00FFFFFF)

    let private signatureOf (property : PropertyInfo) : BlobHandle =
        // A `MetadataToken` indexes its *own* assembly's tables. Reading one against corelib's
        // reader silently decodes an unrelated row rather than failing, so refuse a foreign type
        // here instead of producing a confident wrong answer.
        if property.DeclaringType.Assembly <> typeof<obj>.Assembly then
            failwith
                $"%s{property.DeclaringType.FullName}.%s{property.Name} is declared in %s{property.DeclaringType.Assembly.GetName().Name}, not corelib; its metadata token does not index corelib's Property table"

        (metadataReader.GetPropertyDefinition (propertyHandleOf property)).Signature

    /// Every property the host declares on a type, indexers included.
    let private declaredProperties (ty : Type) : PropertyInfo array =
        ty.GetProperties (
            BindingFlags.DeclaredOnly
            ||| BindingFlags.Public
            ||| BindingFlags.NonPublic
            ||| BindingFlags.Instance
            ||| BindingFlags.Static
        )

    [<Test>]
    let ``decodes a non-indexer property`` () : unit =
        let decoded = decode (signatureOf (typeof<string>.GetProperty "Length"))

        decoded.ReturnType |> shouldEqual (TypeDefn.PrimitiveType PrimitiveType.Int32)
        decoded.ParameterTypes.Length |> shouldEqual 0
        decoded.Header.Kind |> shouldEqual SignatureKind.Property
        decoded.Header.IsInstance |> shouldEqual true

    [<Test>]
    let ``decodes an indexer's index parameters`` () : unit =
        // `String.Chars` is the indexer behind `s[i]`.
        let decoded = decode (signatureOf (typeof<string>.GetProperty "Chars"))

        decoded.ReturnType |> shouldEqual (TypeDefn.PrimitiveType PrimitiveType.Char)

        decoded.ParameterTypes
        |> List.ofSeq
        |> shouldEqual [ TypeDefn.PrimitiveType PrimitiveType.Int32 ]

    [<Test>]
    let ``a static property carries no HASTHIS`` () : unit =
        // The bit `Signature_Init` translates into `CallingConventions.HasThis`.
        decode(signatureOf (typeof<DateTime>.GetProperty "Now")).Header.IsInstance
        |> shouldEqual false

        decode(signatureOf (typeof<DateTime>.GetProperty "Ticks")).Header.IsInstance
        |> shouldEqual true

    [<Test>]
    let ``index parameter count agrees with the host runtime`` () : unit =
        // Outside oracle over many rows at once. `GetIndexParameters` is derived by CoreCLR from
        // the accessors rather than from the PropertySig, so agreement here is a real cross-check
        // of the ParamCount this decoder reads, not a restatement of it.
        let types =
            [
                typeof<string>
                typeof<System.Collections.Generic.List<int>>
                typeof<Exception>
                typeof<DateTime>
                typeof<System.Text.StringBuilder>
            ]

        let mutable checked' = 0
        let mutable indexers = 0

        for ty in types do
            for property in declaredProperties ty do
                let decoded = decode (signatureOf property)
                let expected = property.GetIndexParameters().Length

                decoded.ParameterTypes.Length |> shouldEqual expected
                decoded.RequiredParameterCount |> shouldEqual expected

                checked' <- checked' + 1

                if expected > 0 then
                    indexers <- indexers + 1

        // Non-vacuity: the sweep must actually have run, and must have included at least one
        // indexer — otherwise every expectation above is `0 = 0`.
        checked' |> shouldBeGreaterThan 40
        indexers |> shouldBeGreaterThan 0

    [<Test>]
    let ``property type agrees with the host runtime for primitive-typed properties`` () : unit =
        let expectedPrimitive (ty : Type) : PrimitiveType option =
            if ty = typeof<bool> then Some PrimitiveType.Boolean
            elif ty = typeof<char> then Some PrimitiveType.Char
            elif ty = typeof<sbyte> then Some PrimitiveType.SByte
            elif ty = typeof<byte> then Some PrimitiveType.Byte
            elif ty = typeof<int16> then Some PrimitiveType.Int16
            elif ty = typeof<uint16> then Some PrimitiveType.UInt16
            elif ty = typeof<int32> then Some PrimitiveType.Int32
            elif ty = typeof<uint32> then Some PrimitiveType.UInt32
            elif ty = typeof<int64> then Some PrimitiveType.Int64
            elif ty = typeof<uint64> then Some PrimitiveType.UInt64
            elif ty = typeof<single> then Some PrimitiveType.Single
            elif ty = typeof<double> then Some PrimitiveType.Double
            elif ty = typeof<string> then Some PrimitiveType.String
            else None

        // Non-generic declaring types only. On a generic type the host reports the *substituted*
        // property type (`List<int>.Item` is `Int32`) while the PropertySig spells the
        // unsubstituted `!0` — the decoder returns the signature as written, and substituting is
        // the caller's job, under the declaring type's instantiation.
        let types =
            [
                typeof<string>
                typeof<Exception>
                typeof<DateTime>
                typeof<System.Text.StringBuilder>
                typeof<TimeSpan>
            ]

        let mutable compared = 0

        for ty in types do
            for property in declaredProperties ty do
                match expectedPrimitive property.PropertyType with
                | None -> ()
                | Some primitive ->
                    (decode (signatureOf property)).ReturnType
                    |> shouldEqual (TypeDefn.PrimitiveType primitive)

                    compared <- compared + 1

        compared |> shouldBeGreaterThan 15

    [<Test>]
    let ``refuses a method signature blob`` () : unit =
        // `SignatureDecoder.DecodeMethodSignature` accepts a MethodDefSig quite happily — it is
        // the same entry point — so without this decoder's own kind check a MethodDef blob would
        // decode into a confident wrong answer rather than an error. This is the input that makes
        // the check falsifiable, and it is only reachable because `decode` takes a blob rather
        // than a PropertyDefinitionHandle.
        let methodSignature =
            (metadataReader.GetMethodDefinition (
                MetadataTokens.MethodDefinitionHandle (
                    typeof<string>.GetMethod("Substring", [| typeof<int> |]).MetadataToken
                    &&& 0x00FFFFFF
                )
            ))
                .Signature

        let ex = Assert.Throws (fun () -> decode methodSignature |> ignore)
        ex.Message |> shouldContainText "expected a property signature"
        ex.Message |> shouldContainText "Method"

    [<Test>]
    let ``refuses a field signature blob`` () : unit =
        let fieldSignature =
            (metadataReader.GetFieldDefinition (
                MetadataTokens.FieldDefinitionHandle (
                    typeof<string>.GetField("Empty", BindingFlags.Public ||| BindingFlags.Static).MetadataToken
                    &&& 0x00FFFFFF
                )
            ))
                .Signature

        let ex = Assert.Throws (fun () -> decode fieldSignature |> ignore)
        ex.Message |> shouldContainText "expected a property signature"
        ex.Message |> shouldContainText "Field"
