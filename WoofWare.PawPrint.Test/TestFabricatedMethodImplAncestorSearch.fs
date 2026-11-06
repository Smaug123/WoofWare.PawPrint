namespace WoofWare.PawPrint.Test

open System
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped
open NUnit.Framework

/// A class `C` with one MethodImpl whose declaration is a MemberRef naming its parent, which does not
/// itself declare the method: a shape no C# compiler emits, since Roslyn names the type that declares
/// the method.
///
/// CoreCLR resolves such a declaration with `MethodTableBuilder::FindDeclMethodOnClassInHierarchy`
/// (methodtablebuilder.cpp:6003-6112). It compares every method the named type introduces, then only
/// the virtual methods of each ancestor above it, nearest first, under a substitution that re-reads
/// each ancestor's signatures in the named type's own vocabulary. The first match gives the slot.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedMethodImplAncestorSearch =

    [<RequireQualifiedAccess>]
    type private Shape =
        /// `A { virtual int N(); virtual int M() }`, `B : A {}`, `C : B` overriding `B::M()`. `N` has
        /// `M`'s signature under another name.
        | Inherited
        /// `A<T> { virtual int M(T) }`, `B : A<string> {}`, `C : B` overriding `B::M(string)`.
        | Substituted
        /// As `Substituted`, but `C` overrides `B::M(object)`, which no ancestor declares.
        | NoSubstitutedMatch
        /// `A { virtual int M() }`, `B : A { int M() }` not virtual, `C : B` overriding `B::M()`.
        | NonVirtualAtNamedType
        /// `A { virtual int M() }`, `B : A { new virtual int M() }`, `P : B {}`, `C : P` overriding
        /// `P::M()`.
        | NearestAncestor

    /// The image `shape` describes. `A`'s `M` answers 7 and its `N` 8, `B`'s `M` 6, and `C`'s body
    /// `Impl` 5.
    let private fabricate (shape : Shape) : byte[] =
        let metadata = MetadataBuilder ()
        let ilStream = BlobBuilder ()
        let bodies = MethodBodyStreamEncoder ilStream

        metadata.AddModule (
            0,
            metadata.GetOrAddString "AncestorSearch.dll",
            metadata.GetOrAddGuid (Guid "3e81b5c2-6d47-4a09-8f1e-52c9a7d03b64"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "AncestorSearch",
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

        let objectRef : EntityHandle =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit corelibRef : EntityHandle),
                metadata.GetOrAddString "System",
                metadata.GetOrAddString "Object"
            )
            |> TypeReferenceHandle.op_Implicit

        let signature (parameters : (SignatureTypeEncoder -> unit) list) : BlobHandle =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(isInstanceMethod = true)
                .Parameters (
                    parameters.Length,
                    (fun returns -> returns.Type().Int32 ()),
                    fun encoder ->
                        for parameter in parameters do
                            parameter (encoder.AddParameter().Type ())
                )

            metadata.GetOrAddBlob blob

        let voidSignature : BlobHandle =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(isInstanceMethod = true)
                .Parameters (0, (fun returns -> returns.Void ()), ignore<ParametersEncoder>)

            metadata.GetOrAddBlob blob

        // `M`'s parameters as `A` declares them, and as `C`'s declaration and body spell them.
        let declaredParameters, overriddenParameters: (SignatureTypeEncoder -> unit) list *
                                                      (SignatureTypeEncoder -> unit) list =
            match shape with
            | Shape.Inherited
            | Shape.NonVirtualAtNamedType
            | Shape.NearestAncestor -> [], []
            | Shape.Substituted ->
                [
                    fun t -> t.GenericTypeParameter 0
                ],
                [
                    fun t -> t.String ()
                ]
            | Shape.NoSubstitutedMatch ->
                [
                    fun t -> t.GenericTypeParameter 0
                ],
                [
                    fun t -> t.Object ()
                ]

        let isGeneric = not declaredParameters.IsEmpty

        let answering (value : int) : int =
            let code = InstructionEncoder (BlobBuilder ())
            code.LoadConstantI4 value
            code.OpCode ILOpCode.Ret
            bodies.AddMethodBody code

        let constructing (baseConstructor : EntityHandle) : int =
            let code = InstructionEncoder (BlobBuilder ())
            code.LoadArgument 0
            code.Call baseConstructor
            code.OpCode ILOpCode.Ret
            bodies.AddMethodBody code

        let objectConstructor : EntityHandle =
            metadata.AddMemberReference (objectRef, metadata.GetOrAddString ".ctor", voidSignature)
            |> MemberReferenceHandle.op_Implicit

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

        let constructorAttributes =
            MethodAttributes.Public
            ||| MethodAttributes.HideBySig
            ||| MethodAttributes.SpecialName
            ||| MethodAttributes.RTSpecialName

        let newVirtual =
            MethodAttributes.Public
            ||| MethodAttributes.Virtual
            ||| MethodAttributes.NewSlot
            ||| MethodAttributes.HideBySig

        // MethodDef rows, in the order the TypeDef rows below claim them: A's, B's, P's, then C's.
        let aConstructor =
            addMethod constructorAttributes ".ctor" voidSignature (constructing objectConstructor)

        match shape with
        | Shape.Inherited ->
            addMethod newVirtual "N" (signature []) (answering 8)
            |> ignore<MethodDefinitionHandle>
        | Shape.Substituted
        | Shape.NoSubstitutedMatch
        | Shape.NonVirtualAtNamedType
        | Shape.NearestAncestor -> ()

        addMethod newVirtual "M" (signature declaredParameters) (answering 7)
        |> ignore<MethodDefinitionHandle>

        // `B`'s base constructor: `A`'s, or `A<string>`'s when `A` is generic.
        let aOfString : EntityHandle option =
            if isGeneric then
                let blob = BlobBuilder ()

                let arguments =
                    BlobEncoder(blob)
                        .TypeSpecificationSignature()
                        .GenericInstantiation (
                            TypeDefinitionHandle.op_Implicit (MetadataTokens.TypeDefinitionHandle 2),
                            1,
                            false
                        )

                arguments.AddArgument().String ()

                Some (
                    TypeSpecificationHandle.op_Implicit (metadata.AddTypeSpecification (metadata.GetOrAddBlob blob))
                    : EntityHandle
                )
            else
                None

        let aConstructorFromB : EntityHandle =
            match aOfString with
            | Some instantiation ->
                metadata.AddMemberReference (instantiation, metadata.GetOrAddString ".ctor", voidSignature)
                |> MemberReferenceHandle.op_Implicit
            | None -> MethodDefinitionHandle.op_Implicit aConstructor

        let bConstructor =
            addMethod constructorAttributes ".ctor" voidSignature (constructing aConstructorFromB)

        match shape with
        | Shape.NonVirtualAtNamedType ->
            addMethod (MethodAttributes.Public ||| MethodAttributes.HideBySig) "M" (signature []) (answering 6)
            |> ignore<MethodDefinitionHandle>
        | Shape.NearestAncestor ->
            addMethod newVirtual "M" (signature []) (answering 6)
            |> ignore<MethodDefinitionHandle>
        | Shape.Inherited
        | Shape.Substituted
        | Shape.NoSubstitutedMatch -> ()

        let hasP =
            match shape with
            | Shape.NearestAncestor -> true
            | _ -> false

        let pConstructor =
            if hasP then
                Some (
                    addMethod
                        constructorAttributes
                        ".ctor"
                        voidSignature
                        (constructing (MethodDefinitionHandle.op_Implicit bConstructor))
                )
            else
                None

        let parentConstructor : EntityHandle =
            MethodDefinitionHandle.op_Implicit (pConstructor |> Option.defaultValue bConstructor)

        let cConstructor =
            addMethod constructorAttributes ".ctor" voidSignature (constructing parentConstructor)

        let impl =
            addMethod
                (MethodAttributes.Private
                 ||| MethodAttributes.Final
                 ||| MethodAttributes.Virtual
                 ||| MethodAttributes.NewSlot
                 ||| MethodAttributes.HideBySig)
                "Impl"
                (signature overriddenParameters)
                (answering 5)

        metadata.AddTypeDefinition (
            Unchecked.defaultof<TypeAttributes>,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            aConstructor
        )
        |> ignore<TypeDefinitionHandle>

        let classAttributes = TypeAttributes.Public ||| TypeAttributes.BeforeFieldInit

        // TypeDef row 2, which `aOfString` names.
        let a =
            metadata.AddTypeDefinition (
                classAttributes,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddString (if isGeneric then "A`1" else "A"),
                objectRef,
                MetadataTokens.FieldDefinitionHandle 1,
                aConstructor
            )

        if a <> MetadataTokens.TypeDefinitionHandle 2 then
            failwith
                $"A is TypeDef row %i{MetadataTokens.GetRowNumber (TypeDefinitionHandle.op_Implicit a : EntityHandle)}, but aOfString names row 2"

        if isGeneric then
            metadata.AddGenericParameter (
                TypeDefinitionHandle.op_Implicit a,
                GenericParameterAttributes.None,
                metadata.GetOrAddString "T",
                0
            )
            |> ignore<GenericParameterHandle>

        let b =
            metadata.AddTypeDefinition (
                classAttributes,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddString "B",
                aOfString |> Option.defaultValue (TypeDefinitionHandle.op_Implicit a),
                MetadataTokens.FieldDefinitionHandle 1,
                bConstructor
            )

        let named =
            match pConstructor with
            | Some pConstructor ->
                metadata.AddTypeDefinition (
                    classAttributes,
                    Unchecked.defaultof<StringHandle>,
                    metadata.GetOrAddString "P",
                    TypeDefinitionHandle.op_Implicit b,
                    MetadataTokens.FieldDefinitionHandle 1,
                    pConstructor
                )
            | None -> b

        let c =
            metadata.AddTypeDefinition (
                classAttributes,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddString "C",
                TypeDefinitionHandle.op_Implicit named,
                MetadataTokens.FieldDefinitionHandle 1,
                cConstructor
            )

        let declaration =
            metadata.AddMemberReference (
                TypeDefinitionHandle.op_Implicit named,
                metadata.GetOrAddString "M",
                signature overriddenParameters
            )

        metadata.AddMethodImplementation (
            c,
            MethodDefinitionHandle.op_Implicit impl,
            MemberReferenceHandle.op_Implicit declaration
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

    /// A call through the vtable slot of each `M` that `C` could have overridden.
    let private driver (shape : Shape) : string =
        match shape with
        | Shape.Inherited ->
            """
public static class Driver
{
    public static int Main(string[] args)
    {
        A a = new C();
        return a.N() * 10 + a.M();
    }
}
"""
        | Shape.NonVirtualAtNamedType ->
            """
public static class Driver
{
    public static int Main(string[] args)
    {
        A a = new C();
        return a.M();
    }
}
"""
        | Shape.Substituted
        | Shape.NoSubstitutedMatch ->
            """
public static class Driver
{
    public static int Main(string[] args)
    {
        A<string> a = new C();
        return a.M(null);
    }
}
"""
        | Shape.NearestAncestor ->
            """
public static class Driver
{
    public static int Main(string[] args)
    {
        C c = new C();
        A a = c;
        B b = c;
        return a.M() * 10 + b.M();
    }
}
"""

    let private run (shape : Shape) (expected : int) : unit =
        FabricatedGuest.run "AncestorSearch" (fabricate shape) "AncestorSearchDriver" (driver shape) expected

    [<Test>]
    let ``a declaration naming a type that inherits the method overrides the ancestor's slot`` () : unit =
        // `C`'s body takes `M`'s slot, and `N`'s keeps `A`'s 8.
        run Shape.Inherited 85

    [<Test>]
    let ``an ancestor's method is compared in the named type's vocabulary`` () : unit = run Shape.Substituted 5

    [<Test>]
    let ``the nearest ancestor declaring a match supplies the slot`` () : unit =
        // `C`'s body takes `B`'s slot, so `a.M()` still reaches `A`'s 7 and `b.M()` reaches 5.
        run Shape.NearestAncestor 75

    /// Each declaration CoreCLR refuses, with what the real runtime throws and what PawPrint names.
    let private refused : (Shape * string * string) list =
        [
            Shape.NoSubstitutedMatch, "System.MissingMethodException", "binds to no method"
            Shape.NonVirtualAtNamedType, "System.TypeLoadException", "IDS_CLASSLOAD_MI_MUSTBEVIRTUAL"
        ]

    [<Test>]
    let ``a declaration the search does not resolve is refused, as the real runtime refuses the type`` () : unit =
        let problems =
            [
                for shape, onHostError, onPawPrintError in refused do
                    let onHost, onPawPrint =
                        FabricatedGuest.runOnBoth
                            "AncestorSearch"
                            (fabricate shape)
                            "AncestorSearchDriver"
                            (driver shape)

                    match onHost with
                    | RealRuntimeResult.UnhandledException report when report.Contains onHostError -> ()
                    | other -> yield $"%A{shape}: expected the real runtime to throw %s{onHostError}, but got %O{other}"

                    match onPawPrint with
                    | FabricatedOutcome.Exited code ->
                        yield $"%A{shape}: PawPrint ran the refused type and exited %i{code}"
                    | FabricatedOutcome.Failed e when e.ToString().Contains onPawPrintError -> ()
                    | FabricatedOutcome.Failed e ->
                        yield
                            $"%A{shape}: expected PawPrint to name %s{onPawPrintError}, but it failed with %s{e.Message}"
            ]

        problems |> shouldEqual []
