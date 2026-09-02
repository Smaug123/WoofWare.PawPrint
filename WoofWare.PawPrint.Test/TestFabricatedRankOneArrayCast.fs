namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open NUnit.Framework

/// Casting between an SZ array (`int[]`, ELEMENT_TYPE_SZARRAY) and a rank-1 multi-dimensional
/// array (`int[*]`, ELEMENT_TYPE_ARRAY of rank 1), against the real runtime.
///
/// The two are distinct types, but CoreCLR's `MethodTable::ArrayIsInstanceOf` (`methodtable.cpp`)
/// rejects a multi-dimensional source only when the *target* is an SZ array; against any other
/// array target it compares ranks, and an SZ array's rank is 1. So `int[]` is an `int[*]` while
/// `int[*]` is not an `int[]`. C# cannot spell `int[*]`, so the tokens come from fabricated IL.
[<TestFixture>]
module TestFabricatedRankOneArrayCast =

    /// `Arrays::IsIntRankOne(object) : object` is `ldarg.0; isinst int32[*]; ret`, and
    /// `ChkIntRankOne` is the same with `castclass`. `IsUIntRankOne`, `IsLongRankOne`,
    /// `IsObjectRankOne` and `IsIntRankTwo` are `isinst` with `uint32[*]`, `int64[*]`,
    /// `object[*]` and `int32[,]` tokens. `IntRankOneType() : Type` is
    /// `ldtoken int32[*]; call Type.GetTypeFromHandle; ret`.
    ///
    /// Built with `MetadataBuilder` rather than `PersistedAssemblyBuilder`, because the latter
    /// writes the signature of `typeof<int>.MakeArrayType 1` as `ELEMENT_TYPE_SZARRAY` (measured:
    /// the TypeSpec blob comes out as `1D 08`, and `ldtoken` of it answers `typeof(int[])` on the
    /// real runtime), so it cannot spell `int32[*]` at all. Each array shape here is the canonical
    /// encoding — no sizes, one explicit zero lower bound per dimension — which is the one
    /// encoding PawPrint's signature decoder accepts.
    let private fabricate () : byte[] =
        let metadata = MetadataBuilder ()

        metadata.AddModule (
            0,
            metadata.GetOrAddString "Arrays.dll",
            metadata.GetOrAddGuid (Guid "7c1e5a2d-4b9f-4e8a-b3c6-2d1f0e9a8b7c"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "Arrays",
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

        let corelibType (name : string) : EntityHandle =
            let handle =
                metadata.AddTypeReference (
                    (AssemblyReferenceHandle.op_Implicit corelibRef : EntityHandle),
                    metadata.GetOrAddString "System",
                    metadata.GetOrAddString name
                )

            (TypeReferenceHandle.op_Implicit handle : EntityHandle)

        let objectRef = corelibType "Object"
        let typeRef = corelibType "Type"
        let runtimeTypeHandleRef = corelibType "RuntimeTypeHandle"

        let arrayToken (element : SignatureTypeEncoder -> unit) (rank : int) : EntityHandle =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .TypeSpecificationSignature()
                .Array (
                    element,
                    fun (shape : ArrayShapeEncoder) ->
                        shape.Shape (rank, ImmutableArray.Empty, ImmutableArray.CreateRange (Array.zeroCreate rank))
                )

            let handle = metadata.AddTypeSpecification (metadata.GetOrAddBlob blob)
            (TypeSpecificationHandle.op_Implicit handle : EntityHandle)

        let intRankOne = arrayToken (fun element -> element.Int32 ()) 1
        let uintRankOne = arrayToken (fun element -> element.UInt32 ()) 1
        let longRankOne = arrayToken (fun element -> element.Int64 ()) 1
        let objectRankOne = arrayToken (fun element -> element.Object ()) 1
        let intRankTwo = arrayToken (fun element -> element.Int32 ()) 2

        let getTypeFromHandle =
            let signature = BlobBuilder ()

            BlobEncoder(signature)
                .MethodSignature()
                .Parameters (
                    1,
                    (fun (ret : ReturnTypeEncoder) -> ret.Type().Type (typeRef, false)),
                    fun (parameters : ParametersEncoder) ->
                        parameters.AddParameter().Type().Type (runtimeTypeHandleRef, true)
                )

            let handle =
                metadata.AddMemberReference (
                    typeRef,
                    metadata.GetOrAddString "GetTypeFromHandle",
                    metadata.GetOrAddBlob signature
                )

            (MemberReferenceHandle.op_Implicit handle : EntityHandle)

        let objectToObject =
            let signature = BlobBuilder ()

            BlobEncoder(signature)
                .MethodSignature()
                .Parameters (
                    1,
                    (fun (ret : ReturnTypeEncoder) -> ret.Type().Object ()),
                    fun (parameters : ParametersEncoder) -> parameters.AddParameter().Type().Object ()
                )

            metadata.GetOrAddBlob signature

        let unitToType =
            let signature = BlobBuilder ()

            BlobEncoder(signature)
                .MethodSignature()
                .Parameters (0, (fun (ret : ReturnTypeEncoder) -> ret.Type().Type (typeRef, false)), ignore)

            metadata.GetOrAddBlob signature

        let ilStream = BlobBuilder ()
        let bodies = MethodBodyStreamEncoder ilStream

        let attributes = MethodAttributes.Public ||| MethodAttributes.Static

        let define (name : string) (signature : BlobHandle) (body : InstructionEncoder -> unit) : unit =
            let il = InstructionEncoder (BlobBuilder ())
            body il
            let bodyOffset = bodies.AddMethodBody il

            metadata.AddMethodDefinition (
                attributes,
                MethodImplAttributes.IL,
                metadata.GetOrAddString name,
                signature,
                bodyOffset,
                MetadataTokens.ParameterHandle 1
            )
            |> ignore<MethodDefinitionHandle>

        let typeTest (name : string) (opcode : ILOpCode) (token : EntityHandle) : unit =
            define
                name
                objectToObject
                (fun il ->
                    il.LoadArgument 0
                    il.OpCode opcode
                    il.Token token
                    il.OpCode ILOpCode.Ret
                )

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

        metadata.AddTypeDefinition (
            TypeAttributes.Public
            ||| TypeAttributes.Abstract
            ||| TypeAttributes.Sealed
            ||| TypeAttributes.Class,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "Arrays",
            objectRef,
            MetadataTokens.FieldDefinitionHandle 1,
            MetadataTokens.MethodDefinitionHandle 1
        )
        |> ignore<TypeDefinitionHandle>

        typeTest "IsIntRankOne" ILOpCode.Isinst intRankOne
        typeTest "ChkIntRankOne" ILOpCode.Castclass intRankOne
        typeTest "IsUIntRankOne" ILOpCode.Isinst uintRankOne
        typeTest "IsLongRankOne" ILOpCode.Isinst longRankOne
        typeTest "IsObjectRankOne" ILOpCode.Isinst objectRankOne
        typeTest "IsIntRankTwo" ILOpCode.Isinst intRankTwo

        define
            "IntRankOneType"
            unitToType
            (fun il ->
                il.OpCode ILOpCode.Ldtoken
                il.Token intRankOne
                il.Call getTypeFromHandle
                il.OpCode ILOpCode.Ret
            )

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = (Characteristics.ExecutableImage ||| Characteristics.Dll)),
                MetadataRootBuilder metadata,
                ilStream,
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

    /// Each check returns its own index on failure and 0 when every check passes, so a
    /// disagreement names the check.
    let private driverSource : string =
        """
using System;

public static class Driver
{
    private static bool Throws(Func<object> cast)
    {
        try
        {
            cast();
            return false;
        }
        catch (InvalidCastException)
        {
            return true;
        }
    }

    public static int Main(string[] args)
    {
        int[] ints = new int[3];
        uint[] uints = new uint[3];
        long[] longs = new long[3];
        string[] strings = new string[1];
        int[,] grid = new int[2, 2];

        // An SZ array is an instance of the rank-1 multi-dimensional array of the same element.
        if (!ReferenceEquals(Arrays.IsIntRankOne(ints), ints)) return 1;
        if (!ReferenceEquals(Arrays.ChkIntRankOne(ints), ints)) return 2;

        // Null passes through both instructions.
        if (Arrays.IsIntRankOne(null) != null) return 3;
        if (Arrays.ChkIntRankOne(null) != null) return 4;

        // The element rule is the ordinary one for arrays: same-width integers are
        // interchangeable, a different width is not, and reference elements are covariant.
        if (!ReferenceEquals(Arrays.IsUIntRankOne(ints), ints)) return 5;
        if (!ReferenceEquals(Arrays.IsIntRankOne(uints), uints)) return 6;
        if (Arrays.IsLongRankOne(ints) != null) return 7;
        if (Arrays.IsIntRankOne(longs) != null) return 8;
        if (!ReferenceEquals(Arrays.IsObjectRankOne(strings), strings)) return 9;
        if (Arrays.IsObjectRankOne(ints) != null) return 10;

        // Rank still has to agree: a rank-2 array is not rank 1, and an SZ array is not rank 2.
        if (Arrays.IsIntRankOne(grid) != null) return 11;
        if (Arrays.IsIntRankTwo(ints) != null) return 12;
        if (!Throws(() => Arrays.ChkIntRankOne(grid))) return 13;
        if (!Throws(() => Arrays.ChkIntRankOne(strings))) return 14;

        // The same rule through reflection, and its asymmetry: int[*] accepts an int[], but
        // int[] does not accept an int[*], and the two types are not equal.
        Type intRankOne = Arrays.IntRankOneType();
        if (intRankOne == typeof(int[])) return 15;
        if (!intRankOne.IsInstanceOfType(ints)) return 16;
        if (!intRankOne.IsAssignableFrom(typeof(int[]))) return 17;
        if (typeof(int[]).IsAssignableFrom(intRankOne)) return 18;
        if (intRankOne.IsInstanceOfType(grid)) return 19;
        if (!typeof(Array).IsAssignableFrom(intRankOne)) return 20;

        return 0;
    }
}
"""

    [<Test>]
    let ``an SZ array is an instance of the rank-1 multi-dimensional array type`` () : unit =
        FabricatedGuest.run "Arrays" (fabricate ()) "RankOneArrayCastDriver" driverSource 0
