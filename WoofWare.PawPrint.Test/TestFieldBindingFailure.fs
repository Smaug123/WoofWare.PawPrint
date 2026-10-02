namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open System.Runtime.Loader
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `MemberReferenceInstantiation`'s answer for a field reference that binds nothing, against the
/// real runtime's.
///
/// Each generated image references a field `nope : int32`, which no type declares, through a
/// randomly spelled parent. CoreCLR loads the parent fully before it looks for the field, so it
/// throws `MissingFieldException` if the parent loads and some load failure otherwise, of which a
/// missing type is one class among many. PawPrint answers `FieldReferenceBinding.Fails` only where it
/// can vouch for every type that load reaches, and refuses elsewhere; every answer it does give must
/// be the exception, message and type name the runtime throws. The oracle is
/// `ModuleHandle.ResolveFieldHandle` on the same bytes, which lets CoreCLR's own exception through.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFieldBindingFailure =

    /// A type a spelling names without instantiating it.
    [<RequireQualifiedAccess>]
    type private Leaf =
        /// A type CoreLib declares, through a TypeRef scoped to CoreLib, or to `System.Runtime`,
        /// which forwards it. `spelledAsValueType` is how the signature spells it, which need not be
        /// what it is.
        | CoreLib of ns : string * name : string * spelledAsValueType : bool * viaFacade : bool
        /// A generic definition CoreLib declares, named without an instantiation.
        | CoreLibDefinition of ns : string * name : string * isValueType : bool
        /// An `ELEMENT_TYPE` primitive.
        | Primitive of PrimitiveTypeCode
        /// `System.GoneN`, which CoreLib does not declare.
        | Missing of index : int * spelledAsValueType : bool
        /// `System.Object/Gone`, a nested type CoreLib's `Object` does not declare.
        | MissingNestedInCoreLib
        /// `GuestBroken/Gone`, a nested type the image's own `GuestBroken` does not declare.
        | MissingNestedInGuest
        /// The image's own `GuestClass`.
        | GuestClass
        /// The image's own `GuestStruct`.
        | GuestStruct
        /// The image's own `GuestBroken`, which derives from the sealed `System.String`, so CoreCLR
        /// cannot load it, though every type it names exists.
        | GuestBroken

    /// A generic definition a spelling instantiates.
    [<RequireQualifiedAccess>]
    type private Definition =
        /// One CoreLib declares, through a TypeRef scoped to CoreLib.
        | CoreLib of ns : string * name : string * arity : int * isValueType : bool
        /// The image's own `GuestGeneric`1`, a class.
        | Guest
        /// `System.GoneG`1`, which CoreLib does not declare.
        | Missing

    [<RequireQualifiedAccess>]
    type private Spelling =
        | Leaf of Leaf
        | Instantiation of Definition * arguments : Spelling list
        | SzArray of Spelling
        | Array of Spelling * rank : int
        | Pointer of Spelling
        | ByRef of Spelling

    /// How the MemberRef names its parent.
    [<RequireQualifiedAccess>]
    type private Parent =
        /// A TypeSpec of the spelling.
        | Spec of Spelling
        /// A TypeRef (or, for the image's own class, a TypeDef) naming the leaf directly.
        | Token of Leaf

    /// Non-generic types CoreLib declares, and whether each is a value type.
    let private coreLibTypes : (string * string * bool) list =
        [
            "System", "Object", false
            "System", "String", false
            "System", "Exception", false
            "System", "Version", false
            "System", "Math", false
            "System", "Array", false
            "System", "Enum", false
            "System", "ValueType", false
            "System", "Delegate", false
            "System", "Type", false
            "System", "IDisposable", false
            "System", "ICloneable", false
            "System.Runtime.Intrinsics", "Vector128", false
            "System", "Int32", true
            "System", "Guid", true
            "System", "DateTime", true
            "System", "Decimal", true
            "System", "DayOfWeek", true
            "System", "Void", true
            "System", "ArgIterator", true
            "System", "RuntimeArgumentHandle", true
            "System", "TypedReference", true
        ]

    /// Generic definitions CoreLib declares: arity, and whether each is a value type. Some carry
    /// constraints, some are byref-like, some allow byref-like arguments.
    let private coreLibDefinitions : (string * string * int * bool) list =
        [
            "System.Collections.Generic", "List`1", 1, false
            "System.Collections.Generic", "Dictionary`2", 2, false
            "System", "Tuple`2", 2, false
            "System", "WeakReference`1", 1, false
            "System.Runtime.CompilerServices", "ConditionalWeakTable`2", 2, false
            "System", "Lazy`1", 1, false
            "System.Collections.Generic", "Comparer`1", 1, false
            "System", "Func`2", 2, false
            "System", "Action`1", 1, false
            "System", "IEquatable`1", 1, false
            "System", "IComparable`1", 1, false
            "System.Collections.Generic", "IEnumerable`1", 1, false
            "System.Numerics", "INumber`1", 1, false
            "System.Collections.Generic", "KeyValuePair`2", 2, true
            "System", "ValueTuple`2", 2, true
            "System", "ValueTuple`7", 7, true
            "System", "Nullable`1", 1, true
            "System", "ArraySegment`1", 1, true
            "System", "Memory`1", 1, true
            "System.Runtime.Intrinsics", "Vector128`1", 1, true
            "System", "Span`1", 1, true
            "System", "ReadOnlySpan`1", 1, true
            "System.Runtime.CompilerServices", "InlineArray16`1", 1, true
        ]

    let private primitives : PrimitiveTypeCode list =
        [
            PrimitiveTypeCode.Int32
            PrimitiveTypeCode.Boolean
            PrimitiveTypeCode.Double
            PrimitiveTypeCode.IntPtr
            PrimitiveTypeCode.String
            PrimitiveTypeCode.Object
            PrimitiveTypeCode.TypedReference
        ]

    let private pick (random : Random) (items : 'a list) : 'a = items.[random.Next items.Length]

    let private generateLeaf (random : Random) : Leaf =
        let r = random.NextDouble ()

        if r < 0.03 then
            let ns, name, _, isValueType = pick random coreLibDefinitions
            Leaf.CoreLibDefinition (ns, name, isValueType)
        elif r < 0.5 then
            let ns, name, isValueType = pick random coreLibTypes
            // Now and then the signature spells the wrong kind.
            let spelledAsValueType =
                if random.NextDouble () < 0.05 then
                    not isValueType
                else
                    isValueType

            Leaf.CoreLib (ns, name, spelledAsValueType, random.NextDouble () < 0.25)
        elif r < 0.65 then
            Leaf.Primitive (pick random primitives)
        elif r < 0.8 then
            Leaf.Missing (random.Next 2, random.NextDouble () < 0.5)
        elif r < 0.85 then
            Leaf.MissingNestedInCoreLib
        elif r < 0.88 then
            Leaf.MissingNestedInGuest
        elif r < 0.92 then
            Leaf.GuestClass
        elif r < 0.96 then
            Leaf.GuestStruct
        else
            Leaf.GuestBroken

    let rec private generateSpelling (random : Random) (depth : int) : Spelling =
        let r = random.NextDouble ()

        if depth >= 4 || r < 0.45 then
            Spelling.Leaf (generateLeaf random)
        elif r < 0.75 then
            let definition, arity =
                let d = random.NextDouble ()

                if d < 0.85 then
                    let ns, name, arity, isValueType = pick random coreLibDefinitions
                    Definition.CoreLib (ns, name, arity, isValueType), arity
                elif d < 0.93 then
                    Definition.Guest, 1
                else
                    Definition.Missing, 1

            // Now and then the instantiation has the wrong number of arguments.
            let count =
                if random.NextDouble () < 0.05 then
                    max 1 (arity + (if random.NextDouble () < 0.5 then -1 else 1))
                else
                    arity

            Spelling.Instantiation (definition, List.init count (fun _ -> generateSpelling random (depth + 1)))
        elif r < 0.85 then
            Spelling.SzArray (generateSpelling random (depth + 1))
        elif r < 0.9 then
            let rank =
                if random.NextDouble () < 0.1 then
                    33
                else
                    random.Next (1, 4)

            Spelling.Array (generateSpelling random (depth + 1), rank)
        elif r < 0.95 then
            Spelling.Pointer (generateSpelling random (depth + 1))
        else
            Spelling.ByRef (generateSpelling random (depth + 1))

    /// `InlineArray16` nested `depth` deep over `int32`: its layout grows sixteenfold a level, past
    /// CoreCLR's limit on a value type's size.
    let rec private nestedInlineArray (depth : int) : Spelling =
        let element =
            if depth <= 1 then
                Spelling.Leaf (Leaf.Primitive PrimitiveTypeCode.Int32)
            else
                nestedInlineArray (depth - 1)

        Spelling.Instantiation (
            Definition.CoreLib ("System.Runtime.CompilerServices", "InlineArray16`1", 1, true),
            [ element ]
        )

    let private generateParent (random : Random) : Parent =
        if random.NextDouble () < 0.03 then
            Parent.Spec (nestedInlineArray (random.Next (1, 11)))
        else

        match generateSpelling random 0 with
        | Spelling.Leaf (Leaf.Primitive _) as spelling -> Parent.Spec spelling
        | Spelling.Leaf leaf when random.NextDouble () < 0.3 -> Parent.Token leaf
        | spelling -> Parent.Spec spelling

    /// An image declaring `GuestClass`, `GuestStruct` and `GuestGeneric`1`, which load, and
    /// `GuestBroken`, which does not, and a reference to `nope : int32` on `parent`.
    let private emit (parent : Parent) : byte[] * MemberReferenceHandle =
        let metadata = MetadataBuilder ()

        metadata.AddModule (
            0,
            metadata.GetOrAddString "FieldFailure.dll",
            metadata.GetOrAddGuid (Guid "8e3f1c52-6a0d-4b97-a4e8-2d71c9b06f35"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "FieldFailure",
            Version (1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            Unchecked.defaultof<AssemblyFlags>,
            AssemblyHashAlgorithm.None
        )
        |> ignore<AssemblyDefinitionHandle>

        let assemblyReference (name : AssemblyName) : EntityHandle =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString name.Name,
                name.Version,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddBlob (name.GetPublicKeyToken ()),
                Unchecked.defaultof<AssemblyFlags>,
                Unchecked.defaultof<BlobHandle>
            )
            |> AssemblyReferenceHandle.op_Implicit

        let corelibRef = assemblyReference (typeof<obj>.Assembly.GetName ())

        let facadeRef =
            let name = AssemblyName "System.Runtime"
            name.Version <- typeof<obj>.Assembly.GetName().Version
            name.SetPublicKeyToken [| 0xb0uy ; 0x3fuy ; 0x5fuy ; 0x7fuy ; 0x11uy ; 0xd5uy ; 0x0auy ; 0x3auy |]
            assemblyReference name

        let typeRefs =
            Collections.Generic.Dictionary<EntityHandle * string * string, EntityHandle> ()

        let typeRef (scope : EntityHandle) (ns : string) (name : string) : EntityHandle =
            match typeRefs.TryGetValue ((scope, ns, name)) with
            | true, handle -> handle
            | false, _ ->
                let handle : EntityHandle =
                    metadata.AddTypeReference (scope, metadata.GetOrAddString ns, metadata.GetOrAddString name)
                    |> TypeReferenceHandle.op_Implicit

                typeRefs.[(scope, ns, name)] <- handle
                handle

        let objectRef = typeRef corelibRef "System" "Object"
        let valueTypeRef = typeRef corelibRef "System" "ValueType"

        // `<Module>` is row 1; the image's own types follow it.
        let guestClass = MetadataTokens.TypeDefinitionHandle 2
        let guestStruct = MetadataTokens.TypeDefinitionHandle 3
        let guestGeneric = MetadataTokens.TypeDefinitionHandle 4
        let guestBroken = MetadataTokens.TypeDefinitionHandle 5

        let moduleScope : EntityHandle =
            ModuleDefinitionHandle.op_Implicit EntityHandle.ModuleDefinition

        // A nested type is named through a TypeRef to its enclosing type.
        let leafHandle (leaf : Leaf) : EntityHandle * bool =
            match leaf with
            | Leaf.CoreLib (ns, name, spelledAsValueType, viaFacade) ->
                typeRef (if viaFacade then facadeRef else corelibRef) ns name, spelledAsValueType
            | Leaf.CoreLibDefinition (ns, name, isValueType) -> typeRef corelibRef ns name, isValueType
            | Leaf.Missing (index, spelledAsValueType) ->
                typeRef corelibRef "System" $"Gone%d{index}", spelledAsValueType
            | Leaf.MissingNestedInCoreLib -> typeRef objectRef "" "Gone", false
            | Leaf.MissingNestedInGuest -> typeRef (typeRef moduleScope "" "GuestBroken") "" "Gone", false
            | Leaf.GuestClass -> TypeDefinitionHandle.op_Implicit guestClass, false
            | Leaf.GuestStruct -> TypeDefinitionHandle.op_Implicit guestStruct, true
            | Leaf.GuestBroken -> TypeDefinitionHandle.op_Implicit guestBroken, false
            | Leaf.Primitive code -> failwith $"BUG: a primitive (%O{code}) has no handle"

        let rec encode (encoder : SignatureTypeEncoder) (spelling : Spelling) : unit =
            match spelling with
            | Spelling.Leaf (Leaf.Primitive PrimitiveTypeCode.TypedReference) ->
                // ELEMENT_TYPE_TYPEDBYREF, which `SignatureTypeEncoder` has no method for.
                encoder.Builder.WriteByte 0x16uy
            | Spelling.Leaf (Leaf.Primitive code) -> encoder.PrimitiveType code
            | Spelling.Leaf leaf ->
                let handle, isValueType = leafHandle leaf
                encoder.Type (handle, isValueType)
            | Spelling.Instantiation (definition, arguments) ->
                let handle, isValueType =
                    match definition with
                    | Definition.CoreLib (ns, name, _, isValueType) -> typeRef corelibRef ns name, isValueType
                    | Definition.Guest -> TypeDefinitionHandle.op_Implicit guestGeneric, false
                    | Definition.Missing -> typeRef corelibRef "System" "GoneG`1", false

                let encoders = encoder.GenericInstantiation (handle, arguments.Length, isValueType)

                for argument in arguments do
                    encode (encoders.AddArgument ()) argument
            | Spelling.SzArray element -> encode (encoder.SZArray ()) element
            | Spelling.Array (element, rank) ->
                encoder.Array (
                    (fun e -> encode e element),
                    // The canonical shape: no sizes, and an explicit zero lower bound per dimension.
                    (fun shape ->
                        shape.Shape (rank, ImmutableArray.Empty, ImmutableArray.CreateRange (Array.zeroCreate rank))
                    )
                )
            | Spelling.Pointer element -> encode (encoder.Pointer ()) element
            | Spelling.ByRef element ->
                // ELEMENT_TYPE_BYREF, which `SignatureTypeEncoder` has no method for.
                encoder.Builder.WriteByte 0x10uy
                encode encoder element

        let parentToken =
            match parent with
            | Parent.Token leaf -> fst (leafHandle leaf)
            | Parent.Spec spelling ->
                let blob = BlobBuilder ()
                encode (BlobEncoder(blob).TypeSpecificationSignature ()) spelling

                metadata.AddTypeSpecification (metadata.GetOrAddBlob blob)
                |> TypeSpecificationHandle.op_Implicit

        let int32Sig =
            let blob = BlobBuilder ()
            BlobEncoder(blob).Field().Type().Int32 ()
            metadata.GetOrAddBlob blob

        let reference =
            metadata.AddMemberReference (parentToken, metadata.GetOrAddString "nope", int32Sig)

        let firstField = MetadataTokens.FieldDefinitionHandle 1
        let firstMethod = MetadataTokens.MethodDefinitionHandle 1

        metadata.AddTypeDefinition (
            TypeAttributes.Class,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            firstField,
            firstMethod
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public ||| TypeAttributes.Class,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "GuestClass",
            objectRef,
            firstField,
            firstMethod
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public
            ||| TypeAttributes.Sealed
            ||| TypeAttributes.SequentialLayout
            ||| TypeAttributes.BeforeFieldInit,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "GuestStruct",
            valueTypeRef,
            firstField,
            firstMethod
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddFieldDefinition (FieldAttributes.Public, metadata.GetOrAddString "x", int32Sig)
        |> ignore<FieldDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public ||| TypeAttributes.Class,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "GuestGeneric`1",
            objectRef,
            MetadataTokens.FieldDefinitionHandle 2,
            firstMethod
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public ||| TypeAttributes.Class,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "GuestBroken",
            typeRef corelibRef "System" "String",
            MetadataTokens.FieldDefinitionHandle 2,
            firstMethod
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddGenericParameter (
            TypeDefinitionHandle.op_Implicit guestGeneric,
            GenericParameterAttributes.None,
            metadata.GetOrAddString "T",
            0
        )
        |> ignore<GenericParameterHandle>

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = Characteristics.Dll),
                MetadataRootBuilder metadata,
                BlobBuilder ()
            )

        let image = BlobBuilder ()
        peBuilder.Serialize image |> ignore<BlobContentId>
        image.ToArray (), reference

    /// What binding the reference throws.
    [<RequireQualifiedAccess>]
    type private Outcome =
        | MissingField of message : string
        | TypeLoad of message : string * typeName : string
        /// Anything else, which PawPrint must never claim.
        | Other of string

    let private askRuntime (m : Module) (handle : MemberReferenceHandle) : Outcome =
        try
            m.ModuleHandle.ResolveFieldHandle (
                MetadataTokens.GetToken (MemberReferenceHandle.op_Implicit handle : EntityHandle)
            )
            |> ignore<RuntimeFieldHandle>

            Outcome.Other "binds"
        with
        | :? MissingFieldException as e -> Outcome.MissingField e.Message
        | :? TypeLoadException as e -> Outcome.TypeLoad (e.Message, e.TypeName)
        | e -> Outcome.Other $"%s{e.GetType().Name}: %s{e.Message}"

    /// What PawPrint answers: `Some` outcome it claims the runtime throws, or `None` where it refuses.
    let private askPawPrint
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (runtimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (analysed : DumpedAssembly)
        (handle : MemberReferenceHandle)
        : Outcome option
        =
        let answer =
            try
                let _, _, binding, _ =
                    MemberReferenceInstantiation.resolveMemberWithGenerics
                        loggerFactory
                        runtimeDirs
                        baseClassTypes
                        analysed
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        handle
                        state

                Ok binding
            with e ->
                Error e

        match answer with
        | Ok (Choice2Of2 (FieldReferenceBinding.Fails (FieldReferenceFailure.MissingField (parent, name)))) ->
            Some (Outcome.MissingField $"Field not found: '%s{parent}.%s{name}'.")
        | Ok (Choice2Of2 (FieldReferenceBinding.Fails (FieldReferenceFailure.ParentTypeMissing (typeName, searchedIn)))) ->
            Some (Outcome.TypeLoad ($"Could not load type '%s{typeName}' from assembly '%s{searchedIn}'.", typeName))
        | Ok other -> Some (Outcome.Other $"%A{other}")
        // A refusal is never a wrong answer; the counts of agreed answers keep refusing from passing.
        | Error _ -> None

    [<Test>]
    let ``every answer PawPrint gives for a field reference that binds nothing is the real runtime's`` () : unit =
        let frameworkDir = FrameworkUnderTest.sharedFrameworkDirectory ()
        let runtimeDirs = FrameworkUnderTest.runtimeDirs ()
        let _, loggerFactory = LoggerFactory.makeTest ()

        let corelib =
            Assembly.readFile loggerFactory (Path.Combine (frameworkDir, "System.Private.CoreLib.dll"))

        let baseClassTypes = BaseClassTypes.ofCorelib corelib
        let random = Random 20261002
        let failures = ResizeArray<string> ()
        let mutable missingFields = 0
        let mutable typeLoads = 0
        let mutable refusedMissingFields = 0
        let mutable refusedTypeLoads = 0
        let mutable refusedOthers = 0

        for _ in 1..1500 do
            let parent = generateParent random
            let image, reference = emit parent
            let context = AssemblyLoadContext ("FieldFailure", isCollectible = true)

            try
                let reflected = context.LoadFromStream (new MemoryStream (image))

                let analysed =
                    Assembly.read loggerFactory (Some "FieldFailure.dll") (new MemoryStream (image))

                let state =
                    TypeSystemState.Empty.WithLoadedAssembly(corelib).WithLoadedAssembly analysed

                let oracle = askRuntime reflected.ManifestModule reference

                let ours =
                    askPawPrint loggerFactory runtimeDirs baseClassTypes state analysed reference

                match ours, oracle with
                | None, Outcome.MissingField _ -> refusedMissingFields <- refusedMissingFields + 1
                | None, Outcome.TypeLoad _ -> refusedTypeLoads <- refusedTypeLoads + 1
                | None, Outcome.Other _ -> refusedOthers <- refusedOthers + 1
                | Some ours, oracle when ours = oracle ->
                    match ours with
                    | Outcome.MissingField _ -> missingFields <- missingFields + 1
                    | Outcome.TypeLoad _ -> typeLoads <- typeLoads + 1
                    | Outcome.Other _ -> ()
                | Some ours, oracle -> failures.Add $"runtime %A{oracle}, PawPrint %A{ours}, for %A{parent}"
            finally
                context.Unload ()

        TestContext.Progress.WriteLine
            $"agreed: %d{missingFields} MissingField, %d{typeLoads} TypeLoad; refused where the runtime threw: %d{refusedMissingFields} MissingField, %d{refusedTypeLoads} TypeLoad, %d{refusedOthers} other"

        if failures.Count > 0 then
            failures |> Seq.truncate 8 |> String.concat Environment.NewLine |> failwith

        // Both answers must have been given often enough to mean something.
        missingFields |> shouldBeGreaterThan 200
        typeLoads |> shouldBeGreaterThan 100

    /// `ParentLoadVouching` lets any non-generic value type CoreLib declares be a generic argument or
    /// an array element, on the strength of CoreLib declaring none too large for an array
    /// (`MAX_SIZE_FOR_VALUECLASS_IN_ARRAY`). This holds that of the CoreLib the tests run on.
    [<Test>]
    let ``every non-generic value type CoreLib declares can be an array element`` () : unit =
        let candidates =
            typeof<obj>.Assembly.GetTypes ()
            |> Array.filter (fun t ->
                t.IsValueType
                && not t.ContainsGenericParameters
                && not t.IsByRefLike
                && t <> typeof<Void>
            )

        let failures =
            candidates
            |> Array.choose (fun t ->
                try
                    t.MakeArrayType () |> ignore<Type>
                    None
                with e ->
                    Some $"%s{t.FullName}: %s{e.Message}"
            )

        failures |> shouldEqual [||]
        candidates.Length |> shouldBeGreaterThan 200
