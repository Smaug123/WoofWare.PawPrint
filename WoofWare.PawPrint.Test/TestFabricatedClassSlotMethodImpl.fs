namespace WoofWare.PawPrint.Test

open System
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped
open NUnit.Framework

/// MethodImpl rows on one value type overriding the class virtual `Object.GetHashCode`, in shapes no C#
/// compiler emits, reached by a `constrained.` call whose exact-type probe looks for the override among
/// the value type's own MethodImpls, and by a call on the boxed value, which reads its vtable.
///
/// CoreCLR accepts two rows when they name the same body, and refuses the type at load time when the
/// bodies differ (`IDS_CLASSLOAD_MI_MULTIPLEOVERRIDES`, `AddMethodImplDispatchMapping`): there is no
/// call-time ambiguity to raise. It accepts a body named by a MemberRef that resolves to a method of
/// the type itself, and refuses the type when the body is another type's method, is not virtual, or is
/// static.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedClassSlotMethodImpl =

    [<RequireQualifiedAccess>]
    type private Shape =
        /// Two rows, both naming `HashImpl`.
        | SameBodyTwice
        /// Two rows, naming `HashImpl` and `HashOther`.
        | DifferentBodies
        /// One row, naming `HashMember` by a MemberRef whose parent is `S`. `S` declares `HashImpl`, of
        /// the same signature but another name, and the overload `HashMember(int)` before it.
        | MemberRefBody
        /// One row, naming `Helper.HashImpl` by its MethodDef.
        | ForeignBody
        /// One row, naming `Helper.HashImpl` by a MemberRef whose parent is `Helper`. `S` declares a
        /// `HashImpl` of the same signature, which the row does not name.
        | ForeignMemberRefBody
        /// One row, naming `HashPlain`, an instance method of `S` that is not virtual.
        | NonVirtualBody
        /// One row, naming `HashStatic`, a static method of `S`.
        | StaticBody

    /// `struct S` overriding `Object.GetHashCode` with a private `HashImpl` returning 4 through the
    /// MethodImpl rows `shape` describes. `HashOther` returns 6, `HashMember()` 5 and `HashMember(int)`
    /// 7; `HashPlain`, `HashStatic` and `class Helper`'s `HashImpl` return 4.
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

        let staticReturningInt32 : BlobHandle =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(isInstanceMethod = false)
                .Parameters (0, (fun returnType -> returnType.Type().Int32 ()), ignore<ParametersEncoder>)

            metadata.GetOrAddBlob blob

        let int32OfInt32 : BlobHandle =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(isInstanceMethod = true)
                .Parameters (
                    1,
                    (fun returnType -> returnType.Type().Int32 ()),
                    fun parameters -> parameters.AddParameter().Type().Int32 ()
                )

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

        let addMethod
            (attributes : MethodAttributes)
            (signature : BlobHandle)
            (name : string)
            (value : int)
            : MethodDefinitionHandle
            =
            metadata.AddMethodDefinition (
                attributes,
                MethodImplAttributes.IL,
                metadata.GetOrAddString name,
                signature,
                returning value,
                Unchecked.defaultof<ParameterHandle>
            )

        let addOverride (name : string) (value : int) : MethodDefinitionHandle =
            addMethod
                (MethodAttributes.Private
                 ||| MethodAttributes.Final
                 ||| MethodAttributes.Virtual
                 ||| MethodAttributes.NewSlot
                 ||| MethodAttributes.HideBySig)
                returningInt32
                name
                value

        // MethodDef rows, in the order the TypeDef rows below claim them.
        let hashImpl = addOverride "HashImpl" 4

        let hashOther =
            match shape with
            | Shape.DifferentBodies -> Some (addOverride "HashOther" 6)
            | _ -> None

        match shape with
        | Shape.MemberRefBody ->
            addMethod
                (MethodAttributes.Private
                 ||| MethodAttributes.Final
                 ||| MethodAttributes.Virtual
                 ||| MethodAttributes.NewSlot
                 ||| MethodAttributes.HideBySig)
                int32OfInt32
                "HashMember"
                7
            |> ignore<MethodDefinitionHandle>

            addOverride "HashMember" 5 |> ignore<MethodDefinitionHandle>
        | _ -> ()

        let hashPlain =
            match shape with
            | Shape.NonVirtualBody ->
                Some (addMethod (MethodAttributes.Private ||| MethodAttributes.HideBySig) returningInt32 "HashPlain" 4)
            | _ -> None

        let hashStatic =
            match shape with
            | Shape.StaticBody ->
                Some (
                    addMethod
                        (MethodAttributes.Private
                         ||| MethodAttributes.Static
                         ||| MethodAttributes.HideBySig)
                        staticReturningInt32
                        "HashStatic"
                        4
                )
            | _ -> None

        // `Helper`'s methods come after all of `S`'s, as its TypeDef row comes after `S`'s.
        let hashForeign =
            match shape with
            | Shape.ForeignBody
            | Shape.ForeignMemberRefBody ->
                Some (
                    addMethod
                        (MethodAttributes.Public
                         ||| MethodAttributes.Virtual
                         ||| MethodAttributes.NewSlot
                         ||| MethodAttributes.HideBySig)
                        returningInt32
                        "HashImpl"
                        4
                )
            | _ -> None

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

        let helper =
            hashForeign
            |> Option.map (fun hashForeign ->
                metadata.AddTypeDefinition (
                    TypeAttributes.Public ||| TypeAttributes.BeforeFieldInit,
                    Unchecked.defaultof<StringHandle>,
                    metadata.GetOrAddString "Helper",
                    (TypeReferenceHandle.op_Implicit objectRef : EntityHandle),
                    MetadataTokens.FieldDefinitionHandle 1,
                    hashForeign
                )
            )

        let methodDef (handle : MethodDefinitionHandle) : EntityHandle =
            MethodDefinitionHandle.op_Implicit handle

        let memberRef (parent : TypeDefinitionHandle) (name : string) : EntityHandle =
            metadata.AddMemberReference (
                (TypeDefinitionHandle.op_Implicit parent : EntityHandle),
                metadata.GetOrAddString name,
                returningInt32
            )
            |> MemberReferenceHandle.op_Implicit

        let overrides : EntityHandle list =
            match shape with
            | Shape.SameBodyTwice -> [ methodDef hashImpl ; methodDef hashImpl ]
            | Shape.DifferentBodies -> [ methodDef hashImpl ; methodDef hashOther.Value ]
            | Shape.MemberRefBody -> [ memberRef s "HashMember" ]
            | Shape.ForeignBody -> [ methodDef hashForeign.Value ]
            | Shape.ForeignMemberRefBody -> [ memberRef helper.Value "HashImpl" ]
            | Shape.NonVirtualBody -> [ methodDef hashPlain.Value ]
            | Shape.StaticBody -> [ methodDef hashStatic.Value ]

        for body in overrides do
            metadata.AddMethodImplementation (s, body, (MemberReferenceHandle.op_Implicit getHashCode : EntityHandle))
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

    /// `o.GetHashCode()` on the boxed value is a `callvirt Object::GetHashCode`, which reads the slot
    /// from `S`'s vtable.
    let private boxedDriver : string =
        """
public static class Driver
{
    public static int Main(string[] args)
    {
        object o = new S();
        return o.GetHashCode();
    }
}
"""

    [<Test>]
    let ``two MethodImpl rows naming the same body override the slot with it`` () : unit =
        FabricatedGuest.run "DupClassImpl" (fabricate Shape.SameBodyTwice) "DupClassImplDriver" driver 4

    [<Test>]
    let ``a MethodImpl body named by a MemberRef is the type's own method`` () : unit =
        FabricatedGuest.run "DupClassImpl" (fabricate Shape.MemberRefBody) "DupClassImplDriver" driver 5

    [<Test>]
    let ``a MethodImpl body named by a MemberRef fills the vtable slot`` () : unit =
        FabricatedGuest.run "DupClassImpl" (fabricate Shape.MemberRefBody) "DupClassImplBoxedDriver" boxedDriver 5

    /// Each body CoreCLR refuses, with the load error PawPrint names for it.
    let private refusedBodies : (Shape * string) list =
        [
            Shape.ForeignBody, "IDS_CLASSLOAD_MI_ILLEGAL_BODY"
            Shape.ForeignMemberRefBody, "IDS_CLASSLOAD_MI_ILLEGAL_BODY"
            Shape.NonVirtualBody, "IDS_CLASSLOAD_MI_MUSTBEVIRTUAL"
            Shape.StaticBody, "IDS_CLASSLOAD_MI_BODY_DECL_MISMATCH"
        ]

    [<TestCase "constrained">]
    [<TestCase "boxed">]
    let ``a body that is not a virtual method of the type itself is refused, as the real runtime refuses the type``
        (call : string)
        : unit
        =
        let driverName, driverSource =
            match call with
            | "constrained" -> "DupClassImplDriver", driver
            | "boxed" -> "DupClassImplBoxedDriver", boxedDriver
            | other -> failwith $"no driver %s{other}"

        let problems =
            [
                for shape, loadError in refusedBodies do
                    let onHost, onPawPrint =
                        FabricatedGuest.runOnBoth "DupClassImpl" (fabricate shape) driverName driverSource

                    match onHost with
                    | RealRuntimeResult.UnhandledException report when report.Contains "System.TypeLoadException" -> ()
                    | other -> yield $"%A{shape}: expected the real runtime to refuse the type, but got %O{other}"

                    match onPawPrint with
                    | FabricatedOutcome.Exited code ->
                        yield $"%A{shape}: PawPrint ran the refused type and exited %i{code}"
                    | FabricatedOutcome.Failed e when e.ToString().Contains loadError -> ()
                    | FabricatedOutcome.Failed e ->
                        yield $"%A{shape}: expected PawPrint to name %s{loadError}, but it failed with %s{e.Message}"
            ]

        problems |> shouldEqual []

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
