namespace WoofWare.PawPrint.Test

open System
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped
open NUnit.Framework

/// Two MethodImpl rows on one value type overriding the same class virtual, `Object.GetHashCode`, in
/// shapes no C# compiler emits, reached by a `constrained.` call whose exact-type probe looks for the
/// override among the value type's own MethodImpls.
///
/// CoreCLR accepts the rows when they name the same body, and refuses the type at load time when the
/// bodies differ (`IDS_CLASSLOAD_MI_MULTIPLEOVERRIDES`, `AddMethodImplDispatchMapping`): there is no
/// call-time ambiguity to raise.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedClassSlotMethodImpl =

    [<RequireQualifiedAccess>]
    type private Shape =
        /// Two rows, both naming `HashImpl`.
        | SameBodyTwice
        /// Two rows, naming `HashImpl` and `HashOther`.
        | DifferentBodies

    /// `struct S` overriding `Object.GetHashCode` with a private `HashImpl` returning 4 through the
    /// MethodImpl rows `shape` describes. `HashOther` returns 6.
    let private fabricate (shape : Shape) : byte[] =
        let metadata = MetadataBuilder ()
        let ilStream = BlobBuilder ()
        let bodies = MethodBodyStreamEncoder ilStream

        metadata.AddModule (
            0,
            metadata.GetOrAddString "DupClassImpl.dll",
            metadata.GetOrAddGuid (Guid "6a0e3c51-9d42-4b7e-8f13-2c5d7e9b0a16"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "DupClassImpl",
            Version (1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            Unchecked.defaultof<AssemblyFlags>,
            AssemblyHashAlgorithm.None
        )
        |> ignore<AssemblyDefinitionHandle>

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

        let corelibType (name : string) : TypeReferenceHandle =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit corelibRef : EntityHandle),
                metadata.GetOrAddString "System",
                metadata.GetOrAddString name
            )

        let objectRef = corelibType "Object"
        let valueTypeRef = corelibType "ValueType"

        let returningInt32 : BlobHandle =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(isInstanceMethod = true)
                .Parameters (0, (fun returnType -> returnType.Type().Int32 ()), ignore<ParametersEncoder>)

            metadata.GetOrAddBlob blob

        let getHashCode =
            metadata.AddMemberReference (
                (TypeReferenceHandle.op_Implicit objectRef : EntityHandle),
                metadata.GetOrAddString "GetHashCode",
                returningInt32
            )

        let returning (value : int) : int =
            let code = InstructionEncoder (BlobBuilder ())
            code.LoadConstantI4 value
            code.OpCode ILOpCode.Ret
            bodies.AddMethodBody code

        let addOverride (name : string) (value : int) : MethodDefinitionHandle =
            metadata.AddMethodDefinition (
                MethodAttributes.Private
                ||| MethodAttributes.Final
                ||| MethodAttributes.Virtual
                ||| MethodAttributes.NewSlot
                ||| MethodAttributes.HideBySig,
                MethodImplAttributes.IL,
                metadata.GetOrAddString name,
                returningInt32,
                returning value,
                Unchecked.defaultof<ParameterHandle>
            )

        // MethodDef rows, in the order the TypeDef rows below claim them.
        let hashImpl = addOverride "HashImpl" 4

        let hashOther =
            match shape with
            | Shape.DifferentBodies -> Some (addOverride "HashOther" 6)
            | Shape.SameBodyTwice -> None

        metadata.AddTypeDefinition (
            Unchecked.defaultof<TypeAttributes>,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            hashImpl
        )
        |> ignore<TypeDefinitionHandle>

        let s =
            metadata.AddTypeDefinition (
                TypeAttributes.Public
                ||| TypeAttributes.Sealed
                ||| TypeAttributes.SequentialLayout
                ||| TypeAttributes.BeforeFieldInit,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddString "S",
                (TypeReferenceHandle.op_Implicit valueTypeRef : EntityHandle),
                MetadataTokens.FieldDefinitionHandle 1,
                hashImpl
            )

        let overrides : MethodDefinitionHandle list =
            match shape with
            | Shape.SameBodyTwice -> [ hashImpl ; hashImpl ]
            | Shape.DifferentBodies -> [ hashImpl ; hashOther.Value ]

        for body in overrides do
            metadata.AddMethodImplementation (
                s,
                (MethodDefinitionHandle.op_Implicit body : EntityHandle),
                (MemberReferenceHandle.op_Implicit getHashCode : EntityHandle)
            )
            |> ignore<MethodImplementationHandle>

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

    /// `Hash`'s `t.GetHashCode()` is a `constrained. !!T callvirt Object::GetHashCode`.
    let private driver : string =
        """
public static class Driver
{
    static int Hash<T>(T t) => t.GetHashCode();

    public static int Main(string[] args) => Hash(new S());
}
"""

    [<Test>]
    let ``two MethodImpl rows naming the same body override the slot with it`` () : unit =
        FabricatedGuest.run "DupClassImpl" (fabricate Shape.SameBodyTwice) "DupClassImplDriver" driver 4

    [<Test>]
    let ``two MethodImpl rows naming different bodies are refused, as the real runtime refuses the type`` () : unit =
        let onHost, onPawPrint =
            FabricatedGuest.runOnBoth "DupClassImpl" (fabricate Shape.DifferentBodies) "DupClassImplDriver" driver

        match onHost with
        | RealRuntimeResult.UnhandledException report -> report |> shouldContainText "System.TypeLoadException"
        | other -> failwith $"expected the real runtime to refuse the conflicting type, but got %O{other}"

        match onPawPrint with
        | FabricatedOutcome.Exited code -> failwith $"PawPrint ran the conflicting type and exited %i{code}"
        | FabricatedOutcome.Failed e -> e.ToString () |> shouldContainText "IDS_CLASSLOAD_MI_MULTIPLEOVERRIDES"
