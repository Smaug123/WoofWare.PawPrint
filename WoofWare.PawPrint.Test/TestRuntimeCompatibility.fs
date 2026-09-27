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

/// `RuntimeCompatibility.wrapsNonExceptionThrows` against the real runtime: an assembly wraps a
/// thrown non-exception exactly when its own `catch (Exception)` stops one.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestRuntimeCompatibility =

    /// A `RuntimeCompatibility` blob: the prolog, a named-argument count, and the arguments.
    let compatibilityBlob (count : int16) (arguments : byte list list) : byte[] =
        [ 0x01uy ; 0x00uy ; byte count ; byte (count >>> 8) ] @ List.concat arguments
        |> Array.ofList

    /// A named argument: its field or property tag, its serialization type, its name and its value.
    let namedArgument (kind : byte) (serialization : byte) (name : string) (value : byte list) : byte list =
        let name = Text.Encoding.UTF8.GetBytes name
        [ kind ; serialization ; byte name.Length ] @ List.ofArray name @ value

    /// The `WrapNonExceptionThrows` property, set to `value`'s bytes.
    let wrapProperty (value : byte list) : byte list =
        namedArgument 0x54uy 0x02uy "WrapNonExceptionThrows" value

    /// A TypeSpec around a TypeRef to CoreLib's `RuntimeCompatibilityAttribute`.
    [<RequireQualifiedAccess>]
    type private TypeSpecShape =
        | Class
        | Pointer
        /// Instantiated at `int32`, although the attribute is not generic.
        | GenericInstance
        /// `modopt(object)` before the class.
        | Modified
        | Vector

    /// What the `.ctor` MemberRef of a probe's attribute names as its parent.
    [<RequireQualifiedAccess>]
    type private Constructor =
        /// A TypeRef into CoreLib with this namespace and name.
        | OnTypeRef of ns : string * name : string
        | OnTypeSpec of TypeSpecShape
        /// A ModuleRef, which is no type at all.
        | OnModuleRef

    let private standard : Constructor =
        Constructor.OnTypeRef ("System.Runtime.CompilerServices", "RuntimeCompatibilityAttribute")

    /// An assembly carrying one attribute per entry of `attributes`, in order, each with the
    /// constructor and blob given, whose `W.Probe.Catches` throws a plain `object` inside
    /// `catch (Exception)`.
    let private emitProbe (attributes : (Constructor * byte[]) list) : byte[] =
        let metadata = MetadataBuilder ()
        let ilStream = BlobBuilder ()
        let bodies = MethodBodyStreamEncoder ilStream

        metadata.AddModule (
            0,
            metadata.GetOrAddString "Probe.dll",
            metadata.GetOrAddGuid (Guid "0d4f6b8a-2c1e-4a3b-8d5f-7e9a1b3c5d7f"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        let assemblyDefinition =
            metadata.AddAssembly (
                metadata.GetOrAddString "Probe",
                Version (1, 0, 0, 0),
                Unchecked.defaultof<StringHandle>,
                Unchecked.defaultof<BlobHandle>,
                Unchecked.defaultof<AssemblyFlags>,
                AssemblyHashAlgorithm.None
            )

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

        let typeRef (ns : string) (name : string) : EntityHandle =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit corelibRef : EntityHandle),
                metadata.GetOrAddString ns,
                metadata.GetOrAddString name
            )
            |> TypeReferenceHandle.op_Implicit

        let objectRef = typeRef "System" "Object"

        let voidSignature (isInstance : bool) : BlobHandle =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(isInstanceMethod = isInstance)
                .Parameters (0, (fun returnType -> returnType.Void ()), ignore<ParametersEncoder>)

            metadata.GetOrAddBlob blob

        let constructorOf (ty : EntityHandle) : EntityHandle =
            metadata.AddMemberReference (ty, metadata.GetOrAddString ".ctor", voidSignature true)
            |> MemberReferenceHandle.op_Implicit

        let objectConstructor = constructorOf objectRef

        let typeSpec (shape : TypeSpecShape) : EntityHandle =
            let attribute =
                typeRef "System.Runtime.CompilerServices" "RuntimeCompatibilityAttribute"

            let blob = BlobBuilder ()
            let encoder = BlobEncoder(blob).TypeSpecificationSignature ()

            match shape with
            | TypeSpecShape.Class -> encoder.Type (attribute, false)
            | TypeSpecShape.Pointer -> encoder.Pointer().Type (attribute, false)
            | TypeSpecShape.GenericInstance -> encoder.GenericInstantiation(attribute, 1, false).AddArgument().Int32 ()
            | TypeSpecShape.Modified ->
                encoder.CustomModifiers().AddModifier (objectRef, true)
                |> ignore<CustomModifiersEncoder>

                encoder.Type (attribute, false)
            | TypeSpecShape.Vector -> encoder.SZArray().Type (attribute, false)

            TypeSpecificationHandle.op_Implicit (metadata.AddTypeSpecification (metadata.GetOrAddBlob blob))

        for constructor, blob in attributes do
            let parent =
                match constructor with
                | Constructor.OnTypeRef (ns, name) -> typeRef ns name
                | Constructor.OnTypeSpec shape -> typeSpec shape
                | Constructor.OnModuleRef ->
                    metadata.AddModuleReference (metadata.GetOrAddString "Elsewhere.dll")
                    |> ModuleReferenceHandle.op_Implicit

            metadata.AddCustomAttribute (
                (AssemblyDefinitionHandle.op_Implicit assemblyDefinition : EntityHandle),
                constructorOf parent,
                metadata.GetOrAddBlob blob
            )
            |> ignore<CustomAttributeHandle>

        let body =
            let flow = ControlFlowBuilder ()
            let code = InstructionEncoder (BlobBuilder (), flow)
            let tryStart = code.DefineLabel ()
            let handlerStart = code.DefineLabel ()
            let handlerEnd = code.DefineLabel ()
            code.MarkLabel tryStart
            code.OpCode ILOpCode.Newobj
            code.Token objectConstructor
            code.OpCode ILOpCode.Throw
            code.MarkLabel handlerStart
            code.OpCode ILOpCode.Pop
            code.Branch (ILOpCode.Leave_s, handlerEnd)
            code.MarkLabel handlerEnd
            code.OpCode ILOpCode.Ret
            flow.AddCatchRegion (tryStart, handlerStart, handlerStart, handlerEnd, typeRef "System" "Exception")
            bodies.AddMethodBody code

        let catches =
            metadata.AddMethodDefinition (
                MethodAttributes.Public ||| MethodAttributes.Static,
                MethodImplAttributes.IL,
                metadata.GetOrAddString "Catches",
                voidSignature false,
                body,
                MetadataTokens.ParameterHandle 1
            )

        metadata.AddTypeDefinition (
            TypeAttributes.Class,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            catches
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public
            ||| TypeAttributes.Class
            ||| TypeAttributes.Abstract
            ||| TypeAttributes.Sealed,
            metadata.GetOrAddString "W",
            metadata.GetOrAddString "Probe",
            objectRef,
            MetadataTokens.FieldDefinitionHandle 1,
            catches
        )
        |> ignore<TypeDefinitionHandle>

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = Characteristics.Dll),
                MetadataRootBuilder metadata,
                ilStream
            )

        let image = BlobBuilder ()
        peBuilder.Serialize image |> ignore<BlobContentId>
        image.ToArray ()

    /// Does the real runtime, running `W.Probe.Catches` in `image`, stop the thrown object?
    let private caughtOnRealRuntime (image : byte[]) : bool =
        let context =
            System.Runtime.Loader.AssemblyLoadContext ("Probe", isCollectible = true)

        try
            let probe = context.LoadFromStream(new MemoryStream (image)).GetType "W.Probe"

            try
                probe.GetMethod("Catches").Invoke ((null : obj), Array.empty<obj>)
                |> ignore<obj>

                true
            with :? TargetInvocationException ->
                false
        finally
            context.Unload ()

    [<Test>]
    let ``whether an assembly wraps what it throws is read as CoreCLR reads it`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let cases : (string * byte[] list) list =
            [
                "no attribute", []
                "true", [ compatibilityBlob 1s [ wrapProperty [ 1uy ] ] ]
                "false", [ compatibilityBlob 1s [ wrapProperty [ 0uy ] ] ]
                "any nonzero byte", [ compatibilityBlob 1s [ wrapProperty [ 2uy ] ] ]
                "set as a field",
                [
                    compatibilityBlob 1s [ namedArgument 0x53uy 0x02uy "WrapNonExceptionThrows" [ 1uy ] ]
                ]
                "set twice", [ compatibilityBlob 2s [ wrapProperty [ 1uy ] ; wrapProperty [ 1uy ] ] ]
                "then an unknown argument",
                [
                    compatibilityBlob 2s [ wrapProperty [ 1uy ] ; namedArgument 0x54uy 0x02uy "Other" [ 1uy ] ]
                ]
                "no arguments", [ compatibilityBlob 0s [] ]
                "no count", [ [| 0x01uy ; 0x00uy |] ]
                "a negative count", [ compatibilityBlob -1s [ wrapProperty [ 1uy ] ] ]
                "as an int32",
                [
                    compatibilityBlob
                        1s
                        [
                            namedArgument 0x54uy 0x08uy "WrapNonExceptionThrows" [ 1uy ; 0uy ; 0uy ; 0uy ]
                        ]
                ]
                "under another case",
                [
                    compatibilityBlob 1s [ namedArgument 0x54uy 0x02uy "wrapNonExceptionThrows" [ 1uy ] ]
                ]
                "without its value", [ compatibilityBlob 1s [ wrapProperty [] ] ]
                "with trailing bytes", [ Array.append (compatibilityBlob 1s [ wrapProperty [ 1uy ] ]) [| 0xAAuy |] ]
                "under a bad prolog",
                [
                    Array.append [| 0x00uy ; 0x00uy |] (compatibilityBlob 1s [ wrapProperty [ 1uy ] ]).[2..]
                ]
                "false, then true",
                [
                    compatibilityBlob 1s [ wrapProperty [ 0uy ] ]
                    compatibilityBlob 1s [ wrapProperty [ 1uy ] ]
                ]
                "true, then false",
                [
                    compatibilityBlob 1s [ wrapProperty [ 1uy ] ]
                    compatibilityBlob 1s [ wrapProperty [ 0uy ] ]
                ]
            ]

        let wraps = compatibilityBlob 1s [ wrapProperty [ 1uy ] ]

        // CoreCLR matches the attribute by its type's name alone, joining namespace and name, and
        // finds the name through a TypeSpec parent.
        let constructorCases : (string * (Constructor * byte[]) list) list =
            [
                "named with an empty namespace",
                [
                    Constructor.OnTypeRef ("", "System.Runtime.CompilerServices.RuntimeCompatibilityAttribute"), wraps
                ]
                "named split after System.Runtime",
                [
                    Constructor.OnTypeRef ("System.Runtime", "CompilerServices.RuntimeCompatibilityAttribute"), wraps
                ]
                "named with a namespace ending in a dot",
                [
                    Constructor.OnTypeRef ("System.Runtime.CompilerServices.", "RuntimeCompatibilityAttribute"), wraps
                ]
                "on the class as a TypeSpec", [ Constructor.OnTypeSpec TypeSpecShape.Class, wraps ]
                "on a pointer to the class", [ Constructor.OnTypeSpec TypeSpecShape.Pointer, wraps ]
                "on an instantiation of the class", [ Constructor.OnTypeSpec TypeSpecShape.GenericInstance, wraps ]
                "on the class under a custom modifier", [ Constructor.OnTypeSpec TypeSpecShape.Modified, wraps ]
                "on an array of the class", [ Constructor.OnTypeSpec TypeSpecShape.Vector, wraps ]
                "on an array of the class, then the attribute",
                [ Constructor.OnTypeSpec TypeSpecShape.Vector, wraps ; standard, wraps ]
            ]

        let answers =
            (cases
             |> List.map (fun (description, blobs) -> description, blobs |> List.map (fun blob -> standard, blob)))
            @ constructorCases
            |> List.map (fun (description, attributes) ->
                let image = emitProbe attributes
                // A `catch (Exception)` stops the object only if the assembly wraps it.
                let runtime = caughtOnRealRuntime image

                let ours =
                    Assembly.read loggerFactory (Some "Probe.dll") (new MemoryStream (image))
                    |> RuntimeCompatibility.wrapsNonExceptionThrows

                description, runtime, ours
            )

        for description, runtime, ours in answers do
            if runtime <> ours then
                failwith $"%s{description}: the runtime wraps %b{runtime}, we say %b{ours}"

        // Both answers occur, so neither side is constant.
        answers
        |> List.map (fun (_, runtime, _) -> runtime)
        |> List.distinct
        |> List.length
        |> shouldEqual 2

    [<Test>]
    let ``an assembly attribute whose constructor is on a module reference is refused, as CoreCLR refuses the assembly``
        ()
        : unit
        =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let image =
            emitProbe [ Constructor.OnModuleRef, compatibilityBlob 1s [ wrapProperty [ 1uy ] ] ]

        // Looking up any of the assembly's attributes by name fails on this one, and CoreCLR looks
        // some up while loading the assembly.
        Assert.Throws<BadImageFormatException> (fun () -> caughtOnRealRuntime image |> ignore<bool>)
        |> ignore<BadImageFormatException>

        let assembly =
            Assembly.read loggerFactory (Some "Probe.dll") (new MemoryStream (image))

        let e =
            Assert.Throws<Exception> (fun () -> RuntimeCompatibility.wrapsNonExceptionThrows assembly |> ignore<bool>)

        e.Message |> shouldContainText "module reference"
