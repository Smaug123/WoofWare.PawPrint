namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `Marshal.SizeOf` over structs whose one field carries a field-marshal blob (ECMA-335 II.23.4)
/// no C# source can spell, against the real runtime.
///
/// CoreCLR reads a field's blob with the same `ParseNativeTypeInfo` (mlinfo.cpp) as the
/// `GetMarshalAs` FCall, so a blob it refuses marks the field `NativeFieldCategory::ILLEGAL` and
/// `Marshal.SizeOf` of the struct throws `ArgumentException`. Roslyn only ever writes canonical,
/// complete blobs, so the shapes here — a compressed integer in more bytes than it needs, a
/// truncated or corrupt blob, the `NATIVE_TYPE_DEFAULT` byte spelled explicitly, a `FieldMarshal`
/// row whose field lacks the `HasFieldMarshal` flag — come from fabricated metadata.
///
/// An empty blob is absent from the guests: CoreCLR's answer for it depends on whether the whole
/// struct is blittable (see `FieldMarshalDescriptor.Empty`), which PawPrint refuses to guess.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedFieldMarshal =

    [<RequireQualifiedAccess>]
    type private FieldShape =
        | Int32
        | Int32Array
        | CharArray
        | String

    /// One struct, `Name`, with one field of `Shape`. `Blob` is the field's `FieldMarshal` row, or
    /// `None` for no row; `Flag` is whether the field carries `FieldAttributes.HasFieldMarshal`.
    /// `Expected` is `Marshal.SizeOf` of the struct on the real runtime, or `None` where it throws
    /// `ArgumentException`.
    type private Case =
        {
            Name : string
            Shape : FieldShape
            Blob : byte[] option
            Flag : bool
            Expected : int option
        }

    let private case (name : string) (shape : FieldShape) (blob : byte[]) (expected : int option) : Case =
        {
            Name = name
            Shape = shape
            Blob = Some blob
            Flag = true
            Expected = expected
        }

    let private fabricate (cases : Case list) : byte[] =
        let metadata = MetadataBuilder ()

        metadata.AddModule (
            0,
            metadata.GetOrAddString "FieldMarshal.dll",
            metadata.GetOrAddGuid (Guid "3f0d6a52-8e1b-4c7a-9d24-6b5e1f8c0a93"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "FieldMarshal",
            Version (1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            Unchecked.defaultof<AssemblyFlags>,
            AssemblyHashAlgorithm.None
        )
        |> ignore<AssemblyDefinitionHandle>

        // Reference the host's own CoreLib, so the image loads on the real runtime as well.
        let corelibName = typeof<obj>.Assembly.GetName ()

        let corelibRef =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString corelibName.Name,
                corelibName.Version,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddBlob (corelibName.GetPublicKeyToken ()),
                Unchecked.defaultof<AssemblyFlags>,
                Unchecked.defaultof<BlobHandle>
            )

        let valueTypeRef =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit corelibRef : EntityHandle),
                metadata.GetOrAddString "System",
                metadata.GetOrAddString "ValueType"
            )

        let fieldSignature (shape : FieldShape) : BlobHandle =
            let blob = BlobBuilder ()
            let encoder = BlobEncoder(blob).Field().Type ()

            match shape with
            | FieldShape.Int32 -> encoder.Int32 ()
            | FieldShape.Int32Array -> encoder.SZArray().Int32 ()
            | FieldShape.CharArray -> encoder.SZArray().Char ()
            | FieldShape.String -> encoder.String ()

            metadata.GetOrAddBlob blob

        // The real runtime declines an image with no `<Module>` row.
        metadata.AddTypeDefinition (
            Unchecked.defaultof<TypeAttributes>,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            MetadataTokens.MethodDefinitionHandle 1
        )
        |> ignore<TypeDefinitionHandle>

        cases
        |> List.iteri (fun index case ->
            let fieldRow = index + 1

            metadata.AddTypeDefinition (
                TypeAttributes.Public
                ||| TypeAttributes.Sealed
                ||| TypeAttributes.SequentialLayout
                ||| TypeAttributes.BeforeFieldInit,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddString case.Name,
                (TypeReferenceHandle.op_Implicit valueTypeRef : EntityHandle),
                MetadataTokens.FieldDefinitionHandle fieldRow,
                MetadataTokens.MethodDefinitionHandle 1
            )
            |> ignore<TypeDefinitionHandle>

            let attributes =
                if case.Flag then
                    FieldAttributes.Public ||| FieldAttributes.HasFieldMarshal
                else
                    FieldAttributes.Public

            let field =
                metadata.AddFieldDefinition (attributes, metadata.GetOrAddString "Value", fieldSignature case.Shape)

            match case.Blob with
            | None -> ()
            | Some blob ->
                metadata.AddMarshallingDescriptor (
                    (FieldDefinitionHandle.op_Implicit field : EntityHandle),
                    metadata.GetOrAddBlob blob
                )
        )

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = (Characteristics.ExecutableImage ||| Characteristics.Dll)),
                MetadataRootBuilder metadata,
                BlobBuilder (),
                null,
                null,
                null,
                null,
                0,
                Unchecked.defaultof<MethodDefinitionHandle>,
                CorFlags.ILOnly
            )

        let peImage = BlobBuilder ()
        peBuilder.Serialize peImage |> ignore<BlobContentId>
        peImage.ToArray ()

    /// The driver returns `i + 1` if case `i`'s size is wrong or it did not throw when it should
    /// have, `101 + i` if it threw `ArgumentException` when it should not have, and 0 if every case
    /// behaved as expected.
    let private driverSource (cases : Case list) : string =
        let checks =
            cases
            |> List.mapi (fun index case ->
                let expected = case.Expected |> Option.defaultValue -1
                $"        result = Check(%d{index}, typeof(%s{case.Name}), %d{expected}); if (result != 0) return result;"
            )
            |> String.concat "\n"

        $$"""
using System;
using System.Runtime.InteropServices;

public static class Driver
{
    private static int Check(int index, Type type, int expected)
    {
        try
        {
            int size = Marshal.SizeOf(type);
            return size == expected ? 0 : index + 1;
        }
        catch (ArgumentException)
        {
            return expected < 0 ? 0 : 101 + index;
        }
    }

    public static int Main(string[] args)
    {
        int result;
{{checks}}
        return 0;
    }
}
"""

    let private run (cases : Case list) : unit =
        FabricatedGuest.run "FieldMarshal" (fabricate cases) "FieldMarshalDriver" (driverSource cases) 0

    [<Test>]
    let ``a FixedArray element type is a compressed integer`` () : unit =
        run
            [
                // NATIVE_TYPE_U2 (0x06) in two bytes. The struct is ANSI, so a `char` element
                // defaults to one byte, and only an element type read as U2 makes it two.
                case "WideElementType" FieldShape.CharArray [| 0x1Euy ; 0x04uy ; 0x80uy ; 0x06uy |] (Some 8)
                case "NarrowElementType" FieldShape.CharArray [| 0x1Euy ; 0x04uy ; 0x06uy |] (Some 8)
                // SizeConst 5 in two bytes and in four.
                case "WideSize" FieldShape.Int32Array [| 0x1Euy ; 0x80uy ; 0x05uy ; 0x07uy |] (Some 20)
                case
                    "WidestSize"
                    FieldShape.Int32Array
                    [| 0x1Euy ; 0xC0uy ; 0x00uy ; 0x00uy ; 0x05uy ; 0x07uy |]
                    (Some 20)
                // Bytes after the element type are never read.
                case "TrailingBytes" FieldShape.Int32Array [| 0x1Euy ; 0x04uy ; 0x07uy ; 0xFFuy ; 0xFFuy |] (Some 16)
            ]

    [<Test>]
    let ``a FixedSysString size is a compressed integer`` () : unit =
        run
            [
                case "WideStringSize" FieldShape.String [| 0x17uy ; 0x80uy ; 0x80uy |] (Some 128)
            ]

    [<Test>]
    let ``an explicit NATIVE_TYPE_DEFAULT is no descriptor at all`` () : unit =
        run [ case "ExplicitDefault" FieldShape.Int32 [| 0x50uy |] (Some 4) ]

    [<Test>]
    let ``a blob ParseNativeTypeInfo refuses makes the field unmarshalable`` () : unit =
        run
            [
                case "FixedArrayWithoutSize" FieldShape.Int32Array [| 0x1Euy |] None
                case "FixedArrayCorruptSize" FieldShape.Int32Array [| 0x1Euy ; 0xE0uy ; 0x07uy |] None
                case "FixedArrayTruncatedSize" FieldShape.Int32Array [| 0x1Euy ; 0x80uy |] None
                case "FixedSysStringWithoutSize" FieldShape.String [| 0x17uy |] None
                case "FixedSysStringCorruptSize" FieldShape.String [| 0x17uy ; 0xFFuy |] None
                case "CustomMarshalerWithoutStrings" FieldShape.String [| 0x2Cuy |] None
            ]

    [<Test>]
    let ``a FieldMarshal row is read whether or not the field carries HasFieldMarshal`` () : unit =
        run
            [
                { case "RowWithoutFlag" FieldShape.Int32 [| 0x17uy |] None with
                    Flag = false
                }
                { case "RowWithoutFlagParsed" FieldShape.Int32Array [| 0x1Euy ; 0x03uy ; 0x07uy |] (Some 12) with
                    Flag = false
                }
                // And a flag without a row is no descriptor.
                { case "FlagWithoutRow" FieldShape.Int32 [||] (Some 4) with
                    Blob = None
                }
            ]

    /// The field's descriptor as `FieldInfo.make` reads it out of the fabricated image.
    let private descriptorsOf (cases : Case list) : FieldMarshalDescriptor option list =
        use peReader = new PEReader (ImmutableArray.CreateRange (fabricate cases))
        let mr = peReader.GetMetadataReader ()
        let assembly = AssemblyName "FieldMarshal"

        mr.FieldDefinitions
        |> Seq.map (fun handle ->
            (FieldInfo.make mr assembly handle (mr.GetFieldDefinition handle)).MarshallingDescriptor
        )
        |> List.ofSeq

    /// `FieldMarshal` rows and the `HasFieldMarshal` flag in every combination the image can
    /// express. System.Reflection.Metadata reports a row naming the empty blob exactly as it reports
    /// no row at all, and CoreCLR ignores the flag, so only the row itself separates those two.
    [<Test>]
    let ``FieldInfo reads the FieldMarshal row whatever the flag, including a row naming the empty blob`` () : unit =
        let cases =
            [
                case "EmptyWithFlag" FieldShape.Int32 [||] None
                { case "EmptyWithoutFlag" FieldShape.Int32 [||] None with
                    Flag = false
                }
                { case "RowWithoutFlag" FieldShape.Int32 [| 0x07uy |] None with
                    Flag = false
                }
                { case "FlagWithoutRow" FieldShape.Int32 [||] None with
                    Blob = None
                }
                { case "NoRowNoFlag" FieldShape.Int32 [||] None with
                    Blob = None
                    Flag = false
                }
            ]

        descriptorsOf cases
        |> shouldEqual
            [
                Some FieldMarshalDescriptor.Empty
                Some FieldMarshalDescriptor.Empty
                Some (FieldMarshalDescriptor.Other UnmanagedType.I4)
                None
                None
            ]
