namespace WoofWare.PawPrint.Test

open System
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped
open NUnit.Framework

/// MethodImpl rows for interface methods in shapes no C# compiler emits, each of which CoreCLR
/// either accepts or refuses at type load. A type's interface dispatch map is built from all its rows
/// at once, so refusing a row CoreCLR accepts would break every interface call on the type, not only
/// calls to the method the row names.
///
/// Two rows naming the same interface method are accepted when they name the same body, and refused
/// only when the bodies differ (`AddMethodImplDispatchMapping`). A body named by a MemberRef rather
/// than a MethodDef is normalised to the type's own method (`EnumerateMethodImpls`).
[<TestFixture>]
module TestFabricatedInterfaceMethodImpl =

    [<RequireQualifiedAccess>]
    type private Shape =
        /// Two rows, both naming `MImpl` by MethodDef.
        | SameBodyTwice
        /// Two rows, naming `MImpl` and `MOther`.
        | DifferentBodies
        /// One row, naming `MImpl` through a MemberRef on `C`.
        | MemberRefBody

    /// `IA { long M(); }`, `IB { long N(); }` and `C : IA, IB`, where `C` implements `IA.M` with a
    /// private `MImpl` returning 4 through the MethodImpl rows `shape` describes, and `IB.N`
    /// implicitly with a public `N` returning 5. `MOther` returns 6.
    ///
    /// Built from raw metadata because `TypeBuilder.DefineMethodOverride` refuses to override one
    /// declaration twice.
    let private fabricate (shape : Shape) : byte[] =
        let metadata = MetadataBuilder ()
        let ilStream = BlobBuilder ()
        let bodies = MethodBodyStreamEncoder ilStream

        metadata.AddModule (
            0,
            metadata.GetOrAddString "DupImpl.dll",
            metadata.GetOrAddGuid (Guid "0f6f1e44-2b1c-4d8e-9a57-3c0d2e1b6a74"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "DupImpl",
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

        let objectRef =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit corelibRef : EntityHandle),
                metadata.GetOrAddString "System",
                metadata.GetOrAddString "Object"
            )

        let signature (returnsInt64 : bool) : BlobHandle =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(isInstanceMethod = true)
                .Parameters (
                    0,
                    (fun returnType ->
                        if returnsInt64 then
                            returnType.Type().Int64 ()
                        else
                            returnType.Void ()
                    ),
                    ignore<ParametersEncoder>
                )

            metadata.GetOrAddBlob blob

        let objectCtor =
            metadata.AddMemberReference (
                (TypeReferenceHandle.op_Implicit objectRef : EntityHandle),
                metadata.GetOrAddString ".ctor",
                signature false
            )

        let body (emit : InstructionEncoder -> unit) : int =
            let code = InstructionEncoder (BlobBuilder ())
            emit code
            bodies.AddMethodBody code

        let returning (value : int64) : int =
            body (fun code ->
                code.OpCode ILOpCode.Ldc_i8
                code.CodeBuilder.WriteInt64 value
                code.OpCode ILOpCode.Ret
            )

        let abstractSlot =
            MethodAttributes.Public
            ||| MethodAttributes.Abstract
            ||| MethodAttributes.Virtual
            ||| MethodAttributes.NewSlot
            ||| MethodAttributes.HideBySig

        let explicitImpl =
            MethodAttributes.Private
            ||| MethodAttributes.Final
            ||| MethodAttributes.Virtual
            ||| MethodAttributes.NewSlot
            ||| MethodAttributes.HideBySig

        let addMethod (attributes : MethodAttributes) (name : string) (returnsInt64 : bool) (bodyOffset : int) =
            metadata.AddMethodDefinition (
                attributes,
                MethodImplAttributes.IL,
                metadata.GetOrAddString name,
                signature returnsInt64,
                bodyOffset,
                Unchecked.defaultof<ParameterHandle>
            )

        // MethodDef rows, in the order the TypeDef rows below claim them.
        let iaM = addMethod abstractSlot "M" true -1
        let ibN = addMethod abstractSlot "N" true -1

        let ctor =
            addMethod
                (MethodAttributes.Public
                 ||| MethodAttributes.HideBySig
                 ||| MethodAttributes.SpecialName
                 ||| MethodAttributes.RTSpecialName)
                ".ctor"
                false
                (body (fun code ->
                    code.OpCode ILOpCode.Ldarg_0
                    code.Call (MemberReferenceHandle.op_Implicit objectCtor : EntityHandle)
                    code.OpCode ILOpCode.Ret
                ))

        let mImpl = addMethod explicitImpl "MImpl" true (returning 4L)

        let mOther =
            match shape with
            | Shape.DifferentBodies -> Some (addMethod explicitImpl "MOther" true (returning 6L))
            | Shape.SameBodyTwice
            | Shape.MemberRefBody -> None

        addMethod
            (MethodAttributes.Public
             ||| MethodAttributes.Final
             ||| MethodAttributes.Virtual
             ||| MethodAttributes.NewSlot
             ||| MethodAttributes.HideBySig)
            "N"
            true
            (returning 5L)
        |> ignore<MethodDefinitionHandle>

        metadata.AddTypeDefinition (
            Unchecked.defaultof<TypeAttributes>,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            iaM
        )
        |> ignore<TypeDefinitionHandle>

        let interfaceAttributes =
            TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract

        let ia =
            metadata.AddTypeDefinition (
                interfaceAttributes,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddString "IA",
                Unchecked.defaultof<EntityHandle>,
                MetadataTokens.FieldDefinitionHandle 1,
                iaM
            )

        let ib =
            metadata.AddTypeDefinition (
                interfaceAttributes,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddString "IB",
                Unchecked.defaultof<EntityHandle>,
                MetadataTokens.FieldDefinitionHandle 1,
                ibN
            )

        let c =
            metadata.AddTypeDefinition (
                TypeAttributes.Public ||| TypeAttributes.Class,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddString "C",
                (TypeReferenceHandle.op_Implicit objectRef : EntityHandle),
                MetadataTokens.FieldDefinitionHandle 1,
                ctor
            )

        metadata.AddInterfaceImplementation (c, (TypeDefinitionHandle.op_Implicit ia : EntityHandle))
        |> ignore<InterfaceImplementationHandle>

        metadata.AddInterfaceImplementation (c, (TypeDefinitionHandle.op_Implicit ib : EntityHandle))
        |> ignore<InterfaceImplementationHandle>

        let bodies : EntityHandle list =
            match shape with
            | Shape.SameBodyTwice ->
                [
                    MethodDefinitionHandle.op_Implicit mImpl
                    MethodDefinitionHandle.op_Implicit mImpl
                ]
            | Shape.DifferentBodies ->
                [
                    MethodDefinitionHandle.op_Implicit mImpl
                    MethodDefinitionHandle.op_Implicit mOther.Value
                ]
            | Shape.MemberRefBody ->
                [
                    metadata.AddMemberReference (
                        (TypeDefinitionHandle.op_Implicit c : EntityHandle),
                        metadata.GetOrAddString "MImpl",
                        signature true
                    )
                    |> MemberReferenceHandle.op_Implicit
                ]

        for body in bodies do
            metadata.AddMethodImplementation (c, body, (MethodDefinitionHandle.op_Implicit iaM : EntityHandle))
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

    let private driver : string =
        """
public static class Driver
{
    public static int Main(string[] args)
    {
        C c = new C();
        return (int) (((IA) c).M() * 10 + ((IB) c).N());
    }
}
"""

    [<Test>]
    let ``two MethodImpl rows naming the same body are one mapping`` () : unit =
        FabricatedGuest.run "DupImpl" (fabricate Shape.SameBodyTwice) "DupImplDriver" driver 45

    [<Test>]
    let ``a MethodImpl body named by a MemberRef is the type's own method`` () : unit =
        FabricatedGuest.run "DupImpl" (fabricate Shape.MemberRefBody) "DupImplDriver" driver 45

    [<Test>]
    let ``two MethodImpl rows naming different bodies are refused, as the real runtime refuses the type`` () : unit =
        let onHost, onPawPrint =
            FabricatedGuest.runOnBoth "DupImpl" (fabricate Shape.DifferentBodies) "DupImplDriver" driver

        match onHost with
        | RealRuntimeResult.NormalExit code ->
            failwith $"the real runtime loaded the conflicting type and exited %i{code}"
        | _ -> ()

        match onPawPrint with
        | FabricatedOutcome.Exited code -> failwith $"PawPrint ran the conflicting type and exited %i{code}"
        | FabricatedOutcome.Failed e -> e.ToString () |> shouldContainText "IDS_CLASSLOAD_MI_MULTIPLEOVERRIDES"
