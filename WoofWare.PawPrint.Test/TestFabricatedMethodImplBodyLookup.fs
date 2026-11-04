namespace WoofWare.PawPrint.Test

open System
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open NUnit.Framework

/// A value type with one MethodImpl row whose body is a MemberRef, in shapes no C# compiler emits,
/// where the type declares two methods of the MemberRef's name and the first is not the body.
///
/// CoreCLR resolves such a body with `FindMethodDef` (mdinternalro.cpp:794-880): the first of the
/// type's own methods with that name whose signature blob is byte-for-byte the MemberRef's, skipping
/// PrivateScope methods. Signatures that name the same type through different TypeRef rows are not
/// the same blob.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedMethodImplBodyLookup =

    [<RequireQualifiedAccess>]
    type private Shape =
        /// `S` implements `IDescribe.Describe(System.Exception)` by a MethodImpl. Its two `Impl`
        /// methods take `System.Exception` through two different TypeRef rows, and the MemberRef
        /// spells the second's row.
        | TypeRefRows
        /// `S` overrides `Object.ToString` by a MethodImpl. Its two `Impl` methods have the same
        /// signature, and the first is PrivateScope.
        | PrivateScope

    /// `struct S` whose first `Impl` answers 7 and whose second answers 5: as an `int`, or as a string
    /// of that length.
    let private fabricate (shape : Shape) : byte[] =
        let metadata = MetadataBuilder ()
        let ilStream = BlobBuilder ()
        let bodies = MethodBodyStreamEncoder ilStream

        metadata.AddModule (
            0,
            metadata.GetOrAddString "BodyLookup.dll",
            metadata.GetOrAddGuid (Guid "0c7d2e94-51a3-4f68-9b2e-7d14c3a8e5f0"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "BodyLookup",
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

        // A fresh TypeRef row each call, even for a type an earlier call named.
        let corelibType (name : string) : EntityHandle =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit corelibRef : EntityHandle),
                metadata.GetOrAddString "System",
                metadata.GetOrAddString name
            )
            |> TypeReferenceHandle.op_Implicit

        let objectRef = corelibType "Object"
        let valueTypeRef = corelibType "ValueType"

        let signature
            (returns : ReturnTypeEncoder -> unit)
            (parameters : ParametersEncoder -> unit)
            (count : int)
            : BlobHandle
            =
            let blob = BlobBuilder ()

            BlobEncoder(blob).MethodSignature(isInstanceMethod = true).Parameters (count, returns, parameters)

            metadata.GetOrAddBlob blob

        let returningString =
            signature (fun returns -> returns.Type().String ()) ignore<ParametersEncoder> 0

        let takingException (row : EntityHandle) : BlobHandle =
            signature
                (fun returns -> returns.Type().Int32 ())
                (fun parameters -> parameters.AddParameter().Type().Type (row, false))
                1

        let body (emit : InstructionEncoder -> unit) : int =
            let code = InstructionEncoder (BlobBuilder ())
            emit code
            code.OpCode ILOpCode.Ret
            bodies.AddMethodBody code

        let answering (value : int) : int =
            match shape with
            | Shape.TypeRefRows -> body (fun code -> code.LoadConstantI4 value)
            | Shape.PrivateScope ->
                body (fun code -> code.LoadString (metadata.GetOrAddUserString (String ('!', value))))

        let implSignature, memberRefSignature =
            match shape with
            | Shape.TypeRefRows ->
                let first = takingException (corelibType "Exception")
                first, takingException (corelibType "Exception")
            | Shape.PrivateScope -> returningString, returningString

        let firstAccess =
            match shape with
            | Shape.TypeRefRows -> MethodAttributes.Private
            | Shape.PrivateScope -> MethodAttributes.PrivateScope

        let explicitImpl =
            MethodAttributes.Final
            ||| MethodAttributes.Virtual
            ||| MethodAttributes.NewSlot
            ||| MethodAttributes.HideBySig

        let addMethod
            (attributes : MethodAttributes)
            (name : string)
            (signature : BlobHandle)
            (bodyOffset : int)
            : MethodDefinitionHandle
            =
            metadata.AddMethodDefinition (
                attributes,
                MethodImplAttributes.IL,
                metadata.GetOrAddString name,
                signature,
                bodyOffset,
                Unchecked.defaultof<ParameterHandle>
            )

        // MethodDef rows, in the order the TypeDef rows below claim them.
        let describe =
            match shape with
            | Shape.TypeRefRows ->
                addMethod
                    (MethodAttributes.Public
                     ||| MethodAttributes.Abstract
                     ||| MethodAttributes.Virtual
                     ||| MethodAttributes.NewSlot
                     ||| MethodAttributes.HideBySig)
                    "Describe"
                    (takingException (corelibType "Exception"))
                    -1
                |> Some
            | Shape.PrivateScope -> None

        let firstImpl =
            addMethod (firstAccess ||| explicitImpl) "Impl" implSignature (answering 7)

        addMethod (MethodAttributes.Private ||| explicitImpl) "Impl" memberRefSignature (answering 5)
        |> ignore<MethodDefinitionHandle>

        let firstMethod = describe |> Option.defaultValue firstImpl

        metadata.AddTypeDefinition (
            Unchecked.defaultof<TypeAttributes>,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            firstMethod
        )
        |> ignore<TypeDefinitionHandle>

        let iDescribe =
            describe
            |> Option.map (fun describe ->
                metadata.AddTypeDefinition (
                    TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract,
                    Unchecked.defaultof<StringHandle>,
                    metadata.GetOrAddString "IDescribe",
                    Unchecked.defaultof<EntityHandle>,
                    MetadataTokens.FieldDefinitionHandle 1,
                    describe
                )
            )

        let s =
            metadata.AddTypeDefinition (
                TypeAttributes.Public
                ||| TypeAttributes.Sealed
                ||| TypeAttributes.SequentialLayout
                ||| TypeAttributes.BeforeFieldInit,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddString "S",
                valueTypeRef,
                MetadataTokens.FieldDefinitionHandle 1,
                firstImpl
            )

        let declaration : EntityHandle =
            match describe, iDescribe with
            | Some describe, Some iDescribe ->
                metadata.AddInterfaceImplementation (s, TypeDefinitionHandle.op_Implicit iDescribe)
                |> ignore<InterfaceImplementationHandle>

                MethodDefinitionHandle.op_Implicit describe
            | _ ->
                metadata.AddMemberReference (objectRef, metadata.GetOrAddString "ToString", returningString)
                |> MemberReferenceHandle.op_Implicit

        let implRef =
            metadata.AddMemberReference (
                (TypeDefinitionHandle.op_Implicit s : EntityHandle),
                metadata.GetOrAddString "Impl",
                memberRefSignature
            )

        metadata.AddMethodImplementation (s, (MemberReferenceHandle.op_Implicit implRef : EntityHandle), declaration)
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

    /// The driver for `shape` making its call `call`: through a `constrained.` prefix, which looks for
    /// the override among `S`'s own MethodImpls, or on the boxed value, which reads `S`'s tables.
    let private driver (shape : Shape) (call : string) : string =
        match shape, call with
        | Shape.TypeRefRows, "constrained" ->
            """
public static class Driver
{
    static int Ask<T>(T t) where T : IDescribe => t.Describe(null);

    public static int Main(string[] args) => Ask(new S());
}
"""
        | Shape.TypeRefRows, "boxed" ->
            """
public static class Driver
{
    public static int Main(string[] args)
    {
        IDescribe d = new S();
        return d.Describe(null);
    }
}
"""
        | Shape.PrivateScope, "constrained" ->
            """
public static class Driver
{
    static int Ask<T>(T t) => t.ToString().Length;

    public static int Main(string[] args) => Ask(new S());
}
"""
        | Shape.PrivateScope, "boxed" ->
            """
public static class Driver
{
    public static int Main(string[] args)
    {
        object o = new S();
        return o.ToString().Length;
    }
}
"""
        | _, other -> failwith $"no driver %s{other}"

    [<TestCase "constrained">]
    [<TestCase "boxed">]
    let ``a MemberRef body names the type's method whose signature blob is its own`` (call : string) : unit =
        FabricatedGuest.run
            "BodyLookup"
            (fabricate Shape.TypeRefRows)
            "BodyLookupDriver"
            (driver Shape.TypeRefRows call)
            5

    [<TestCase "constrained">]
    [<TestCase "boxed">]
    let ``a MemberRef body never names a PrivateScope method`` (call : string) : unit =
        FabricatedGuest.run
            "BodyLookup"
            (fabricate Shape.PrivateScope)
            "BodyLookupDriver"
            (driver Shape.PrivateScope call)
            5
