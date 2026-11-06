namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Emit
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The member references CoreCLR's emitter adds to a dynamic module for
/// `ModuleBuilder_GetMemberRefOfMethodInfo`: each `MemberRef` row is found among the rows already
/// there before one is added, and a signature that names no type by token is copied unchanged.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEmitScopeMemberRefs =

    /// Names a request may give besides those already in the scope.
    let private memberNames : string list = [ "M" ; "N" ; ".ctor" ; "日本" ; "m" ]

    /// Signatures a request may give besides those already in the scope, some of them alike but for
    /// their length or one byte.
    let private signatures : ImmutableArray<byte> list =
        [
            ImmutableArray.Create<byte> [| 0x20uy ; 0x00uy ; 0x01uy |]
            ImmutableArray.Create<byte> [| 0x20uy ; 0x00uy ; 0x08uy |]
            ImmutableArray.Create<byte> [| 0x20uy ; 0x00uy ; 0x08uy ; 0x08uy |]
            ImmutableArray.Create<byte> [| 0x00uy ; 0x00uy ; 0x08uy |]
        ]

    /// Every parent a `MemberRef` of `scope` could name.
    let private parents (scope : EmitScope) : MemberRefParent list =
        [
            for i in 1 .. (EmitScope.typeDefs scope).Count do
                yield MemberRefParent.TypeDef (TypeDefRid i)
            for i in 1 .. (EmitScope.typeRefs scope).Count do
                yield MemberRefParent.TypeRef (TypeRefRid i)
            for i in 1 .. (EmitScope.methodDefs scope).Count do
                yield MemberRefParent.MethodDef (MethodDefRid i)
        ]

    /// A row as the model holds it.
    let private held (scope : EmitScope) (row : MemberRefRow) : MemberRefParent * string * byte list =
        row.Parent, EmitScope.stringAt row.Name scope, List.ofSeq (EmitScope.blobAt row.Signature scope)

    /// A request: either one taken from a row the scope already has, or one made of choices.
    type private Request =
        | Existing of row : int
        | Made of parent : int * name : int * signature : int

    let private genRequest : Gen<Request> =
        Gen.frequency
            [
                1, Gen.map Request.Existing EmitScopeGen.choice
                3,
                Gen.map3
                    (fun p n s -> Request.Made (p, n, s))
                    EmitScopeGen.choice
                    (Gen.choose (0, memberNames.Length - 1))
                    (Gen.choose (0, signatures.Length - 1))
            ]

    [<Test>]
    let ``a member reference is the first row with its parent, name and signature, or else a new row`` () : unit =
        let mutable found = 0
        let mutable foundOfTwo = 0
        let mutable added = 0

        let property (generated : EmitScope) (duplicate : int option) (requests : Request list) : unit =
            // The emitter never adds a row it would find, but a scope may hold two alike all the same:
            // the first of them is the one found.
            let start =
                match duplicate with
                | Some row when (EmitScope.memberRefs generated).Count > 0 ->
                    let rows = EmitScope.memberRefs generated
                    EmitScope.addMemberRef rows.[row % rows.Count] generated |> snd
                | _ -> generated

            let candidates = parents start
            let mutable model = EmitScope.memberRefs start |> Seq.map (held start) |> List.ofSeq
            let mutable scope = start

            for request in requests do
                let parent, name, signature =
                    match request with
                    | Request.Existing row when not model.IsEmpty ->
                        let parent, name, signature = List.item (row % model.Length) model
                        parent, name, ImmutableArray.CreateRange signature
                    | Request.Existing row ->
                        List.item (row % candidates.Length) candidates, memberNames.Head, signatures.Head
                    | Request.Made (p, n, s) ->
                        List.item (p % candidates.Length) candidates, memberNames.[n], signatures.[s]

                let key = parent, name, List.ofSeq signature
                let rid, next = EmitScopeReferences.defineMemberRef parent name signature scope

                match List.tryFindIndex ((=) key) model with
                | Some index ->
                    found <- found + 1

                    if List.filter ((=) key) model |> List.length > 1 then
                        foundOfTwo <- foundOfTwo + 1

                    rid |> shouldEqual (MemberRefRid (index + 1))
                    Object.ReferenceEquals (next, scope) |> shouldEqual true

                    EmitScopeReferences.tryFindMemberRef parent name signature scope
                    |> shouldEqual (Some rid)
                | None ->
                    added <- added + 1

                    EmitScopeReferences.tryFindMemberRef parent name signature scope
                    |> shouldEqual None

                    rid |> shouldEqual (MemberRefRid (model.Length + 1))
                    model <- model @ [ key ]

                    EmitScope.memberRefs next
                    |> Seq.map (held next)
                    |> List.ofSeq
                    |> shouldEqual model

                    // Only the new row and its heap entries are added.
                    EmitScope.typeRefs next |> shouldEqual (EmitScope.typeRefs scope)
                    EmitScope.typeDefs next |> shouldEqual (EmitScope.typeDefs scope)
                    EmitScope.methodDefs next |> shouldEqual (EmitScope.methodDefs scope)

                scope <- next

        let arb =
            Gen.zip3 EmitScopeGen.genScope (Gen.optionOf EmitScopeGen.choice) (Gen.listOf genRequest)
            |> Arb.fromGen

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 300,
            Prop.forAll arb (fun (start, duplicate, requests) -> property start duplicate requests)
        )

        found |> shouldBeGreaterThan 100
        foundOfTwo |> shouldBeGreaterThan 20
        added |> shouldBeGreaterThan 100

    /// A compressed unsigned integer (ECMA-335 II.23.2) and how many bytes it is encoded in: 1 for its
    /// shortest form, or an overlong 2 or 4, which CoreCLR's reader accepts as well.
    type private Compressed =
        {
            Value : int
            Width : int
        }

    let private overlong (value : int) (width : int) : byte list =
        match width with
        | 2 when value < 0x4000 -> [ byte (0x80 ||| (value >>> 8)) ; byte value ]
        | 4 ->
            [
                byte (0xC0 ||| (value >>> 24))
                byte (value >>> 16)
                byte (value >>> 8)
                byte value
            ]
        | _ -> EmitScopeGen.compressedUnsigned value

    let private encoded (integer : Compressed) : byte list = overlong integer.Value integer.Width

    /// A type in a signature (ECMA-335 II.23.2.12), each token given as its `TypeDefOrRefOrSpecEncoded`
    /// value.
    type private Shape =
        | Primitive of code : byte
        | Named of valueType : bool * token : int
        | SzArray of Shape
        | Pointer of Shape
        | VoidPointer
        | ByRef of Shape
        | TypeParameter of index : Compressed
        | MethodParameter of index : Compressed
        | Instance of valueType : bool * token : int * countWidth : int * arguments : Shape list
        | Array of ArrayShape
        | FunctionPointer of MethodShape
        | Modified of required : bool * token : int * Shape

    /// An `ARRAY`'s element type and shape (ECMA-335 II.23.2.13). Each lower bound is a compressed
    /// signed integer, whose length is read as an unsigned one's.
    and private ArrayShape =
        {
            Element : Shape
            Rank : Compressed
            SizeCountWidth : int
            Sizes : Compressed list
            BoundCountWidth : int
            LowerBounds : Compressed list
        }

    /// A method signature (ECMA-335 II.23.2.1). A return type of `None` is `void`.
    and private MethodShape =
        {
            Header : byte
            GenericArity : Compressed
            ParameterCountWidth : int
            Return : Shape option
            Parameters : Shape list
        }

    let rec private namesTypeByToken (shape : Shape) : bool =
        match shape with
        | Shape.Primitive _
        | Shape.VoidPointer
        | Shape.TypeParameter _
        | Shape.MethodParameter _ -> false
        | Shape.Named _
        | Shape.Instance _
        | Shape.Modified _ -> true
        | Shape.SzArray inner
        | Shape.Pointer inner
        | Shape.ByRef inner -> namesTypeByToken inner
        | Shape.Array array -> namesTypeByToken array.Element
        | Shape.FunctionPointer signature -> methodNamesTypeByToken signature

    and private methodNamesTypeByToken (signature : MethodShape) : bool =
        Option.exists namesTypeByToken signature.Return
        || List.exists namesTypeByToken signature.Parameters

    /// A shape's bytes. With `shortestIndices`, a type or method parameter's index and an instantiation's
    /// argument count are in their shortest form, as CoreCLR's emitter writes them; every other integer
    /// is encoded as the shape says either way.
    let rec private encode (shortestIndices : bool) (shape : Shape) : byte list =
        let index (integer : Compressed) : byte list =
            if shortestIndices then
                EmitScopeGen.compressedUnsigned integer.Value
            else
                encoded integer

        match shape with
        | Shape.Primitive code -> [ code ]
        | Shape.Named (valueType, token) ->
            (if valueType then 0x11uy else 0x12uy) :: EmitScopeGen.compressedUnsigned token
        | Shape.SzArray inner -> 0x1Duy :: encode shortestIndices inner
        | Shape.Pointer inner -> 0x0Fuy :: encode shortestIndices inner
        | Shape.VoidPointer -> [ 0x0Fuy ; 0x01uy ]
        | Shape.ByRef inner -> 0x10uy :: encode shortestIndices inner
        | Shape.TypeParameter integer -> 0x13uy :: index integer
        | Shape.MethodParameter integer -> 0x1Euy :: index integer
        | Shape.Instance (valueType, token, countWidth, arguments) ->
            [
                yield 0x15uy
                yield (if valueType then 0x11uy else 0x12uy)
                yield! EmitScopeGen.compressedUnsigned token
                yield!
                    index
                        {
                            Value = arguments.Length
                            Width = countWidth
                        }
                for argument in arguments do
                    yield! encode shortestIndices argument
            ]
        | Shape.Array array ->
            [
                yield 0x14uy
                yield! encode shortestIndices array.Element
                yield! encoded array.Rank
                yield! overlong array.Sizes.Length array.SizeCountWidth
                for size in array.Sizes do
                    yield! encoded size
                yield! overlong array.LowerBounds.Length array.BoundCountWidth
                for bound in array.LowerBounds do
                    yield! encoded bound
            ]
        | Shape.FunctionPointer signature -> 0x1Buy :: encodeMethod shortestIndices signature
        | Shape.Modified (required, token, inner) ->
            [
                yield (if required then 0x1Fuy else 0x20uy)
                yield! EmitScopeGen.compressedUnsigned token
                yield! encode shortestIndices inner
            ]

    and private encodeMethod (shortestIndices : bool) (signature : MethodShape) : byte list =
        [
            yield signature.Header
            if signature.Header &&& 0x10uy <> 0uy then
                yield! encoded signature.GenericArity
            yield! overlong signature.Parameters.Length signature.ParameterCountWidth
            match signature.Return with
            | None -> yield 0x01uy
            | Some shape -> yield! encode shortestIndices shape
            for parameter in signature.Parameters do
                yield! encode shortestIndices parameter
        ]

    let private genBool : Gen<bool> = Gen.elements [ false ; true ]

    /// Mostly the shortest form, so that most signatures are not all overlong.
    let private genWidth : Gen<int> = Gen.elements [ 1 ; 1 ; 2 ; 4 ]

    let private genCompressed (low : int) (high : int) : Gen<Compressed> =
        Gen.map2
            (fun value width ->
                {
                    Value = value
                    Width = width
                }
            )
            (Gen.choose (low, high))
            genWidth

    /// The primitive element types, `object`, `string` and `TypedReference` among them.
    let private primitives : byte list =
        [ 0x02uy .. 0x0Euy ] @ [ 0x16uy ; 0x18uy ; 0x19uy ; 0x1Cuy ]

    /// A `TypeDefOrRefOrSpecEncoded` token: a TypeDef or TypeRef, or a TypeSpec where the grammar allows
    /// one, which is only in a custom modifier.
    let private genToken (allowSpec : bool) : Gen<int> =
        Gen.map2
            (fun rid tag -> (rid <<< 2) ||| tag)
            (Gen.choose (1, 0x3FFF))
            (Gen.choose (0, (if allowSpec then 2 else 1)))

    /// A type; one that names no type by token unless `allowTokens`.
    let rec private genShape (allowTokens : bool) (size : int) : Gen<Shape> =
        let leaf =
            Gen.frequency
                [
                    yield 3, Gen.map Shape.Primitive (Gen.elements primitives)
                    yield 1, Gen.constant Shape.VoidPointer
                    yield 3, Gen.map Shape.TypeParameter (genCompressed 0 0x5000)
                    yield 3, Gen.map Shape.MethodParameter (genCompressed 0 0x5000)
                    if allowTokens then
                        yield 3, Gen.map2 (fun v t -> Shape.Named (v, t)) genBool (genToken false)
                ]

        if size <= 0 then
            leaf
        else
            let inner = genShape allowTokens (size / 2)

            Gen.frequency
                [
                    yield 3, leaf
                    yield 1, Gen.map Shape.SzArray inner
                    yield 1, Gen.map Shape.Pointer inner
                    yield 1, Gen.map Shape.ByRef inner

                    yield
                        1,
                        gen {
                            let! element = inner
                            let! rank = genCompressed 1 4
                            let! sizeCountWidth = genWidth
                            let! sizes = Gen.listOf (genCompressed 0 0x5000) |> Gen.map (List.truncate rank.Value)
                            let! boundCountWidth = genWidth
                            let! bounds = Gen.listOf (genCompressed 0 0x1000) |> Gen.map (List.truncate rank.Value)

                            return
                                Shape.Array
                                    {
                                        Element = element
                                        Rank = rank
                                        SizeCountWidth = sizeCountWidth
                                        Sizes = sizes
                                        BoundCountWidth = boundCountWidth
                                        LowerBounds = bounds
                                    }
                        }

                    yield 1, Gen.map Shape.FunctionPointer (genMethod allowTokens (size / 2) false)

                    if allowTokens then
                        yield
                            1,
                            gen {
                                let! valueType = genBool
                                let! token = genToken false
                                let! countWidth = genWidth
                                let! count = Gen.choose (1, 3)
                                let! arguments = Gen.listOfLength count inner
                                return Shape.Instance (valueType, token, countWidth, arguments)
                            }

                        yield 1, Gen.map3 (fun r t s -> Shape.Modified (r, t, s)) genBool (genToken true) inner
                ]

    /// A method signature: a definition's when `allowGeneric`, and so possibly generic; a function
    /// pointer's otherwise. It names no type by token unless `allowTokens`.
    and private genMethod (allowTokens : bool) (size : int) (allowGeneric : bool) : Gen<MethodShape> =
        gen {
            let! hasThis = genBool
            let! convention = Gen.elements [ 0x00uy ; 0x05uy ]
            let! generic = if allowGeneric then genBool else Gen.constant false
            let! arity = genCompressed 1 3
            let! parameterCountWidth = genWidth
            let! returns = Gen.optionOf (genShape allowTokens size)
            let! parameters = Gen.listOf (genShape allowTokens size) |> Gen.map (List.truncate 4)

            return
                {
                    Header =
                        convention
                        ||| (if hasThis then 0x20uy else 0uy)
                        ||| (if generic then 0x10uy else 0uy)
                    GenericArity = arity
                    ParameterCountWidth = parameterCountWidth
                    Return = returns
                    Parameters = parameters
                }
        }

    [<Test>]
    let ``a signature is imported with each type parameter's index shortest and every other byte but trailing ones copied, unless it names a type by token``
        ()
        : unit
        =
        let mutable named = 0
        let mutable reencoded = 0
        let mutable unchanged = 0

        let property (signature : MethodShape) (trailing : byte list) : unit =
            let input = encodeMethod false signature

            let expected =
                if methodNamesTypeByToken signature then
                    named <- named + 1
                    MethodSignatureImport.NamesTypeByToken
                else
                    let written = encodeMethod true signature

                    if written = input then
                        unchanged <- unchanged + 1
                    else
                        reencoded <- reencoded + 1

                    MethodSignatureImport.Copied (ImmutableArray.CreateRange written)

            EmitScopeReferences.importMethodSignature (ImmutableArray.CreateRange (input @ trailing))
            |> shouldEqual expected

        let arb =
            Gen.zip
                (Gen.sized (fun size ->
                    genBool
                    |> Gen.bind (fun allowTokens -> genMethod allowTokens (min size 12) true)
                ))
                (Gen.listOf (Gen.elements [ 0x00uy ; 0x08uy ; 0x80uy ])
                 |> Gen.map (List.truncate 2))
            |> Arb.fromGen

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 2000,
            Prop.forAll arb (fun (signature, trailing) -> property signature trailing)
        )

        named |> shouldBeGreaterThan 100
        reencoded |> shouldBeGreaterThan 100
        unchanged |> shouldBeGreaterThan 100

    /// Every method and constructor CoreLib declares on a type that is neither generic nor nested in a
    /// generic type, so that the parent a dynamic module's reference names is a `TypeRef`.
    let private coreLibMethods : MethodBase list Lazy =
        lazy
            [
                let flags =
                    BindingFlags.DeclaredOnly
                    ||| BindingFlags.Public
                    ||| BindingFlags.NonPublic
                    ||| BindingFlags.Instance
                    ||| BindingFlags.Static

                for t in typeof<obj>.Assembly.GetTypes () |> Array.sortBy (fun t -> t.MetadataToken) do
                    if not t.ContainsGenericParameters && not t.IsGenericType then
                        for m in t.GetMethods flags |> Array.sortBy (fun m -> m.MetadataToken) do
                            yield m :> MethodBase

                        for c in t.GetConstructors flags |> Array.sortBy (fun c -> c.MetadataToken) do
                            yield c :> MethodBase
            ]

    [<Test>]
    let ``real .NET imports a CoreLib signature that names no type by token unchanged, and gives each reference the token defineMemberRef gives``
        ()
        : unit
        =
        let methods =
            coreLibMethods.Value
            |> List.filter (fun m ->
                match
                    EmitScopeReferences.importMethodSignature (
                        ImmutableArray.Create<byte> (m.Module.ResolveSignature m.MetadataToken)
                    )
                with
                | MethodSignatureImport.Copied _ -> true
                | MethodSignatureImport.NamesTypeByToken -> false
            )

        // Each method once in order, then each again in reverse, so that half the requests are found.
        let requests = methods @ List.rev methods
        List.length methods |> shouldBeGreaterThan 3000

        let assembly =
            AssemblyBuilder.DefineDynamicAssembly (AssemblyName "MemberRefOracle", AssemblyBuilderAccess.Run)

        let dynamicModule = assembly.DefineDynamicModule "MemberRefOracle"
        let mutable scope = EmitScope.create "MemberRefOracle" Guid.Empty

        // The model's TypeRefs stand in for the real module's: only their RIDs are compared.
        let rec withTypeRefs (count : int) (scope : EmitScope) : EmitScope =
            if (EmitScope.typeRefs scope).Count >= count then
                scope
            else
                let name, scope =
                    EmitScope.internString $"T%d{(EmitScope.typeRefs scope).Count + 1}" scope

                EmitScope.addTypeRef
                    {
                        ResolutionScope = ResolutionScope.Module
                        Name = name
                        Namespace = StringOffset 0
                    }
                    scope
                |> snd
                |> withTypeRefs count

        for m in requests do
            let definitionSignature = m.Module.ResolveSignature m.MetadataToken

            let token =
                match m with
                | :? ConstructorInfo as c -> dynamicModule.GetMethodMetadataToken c
                | :? MethodInfo as mi -> dynamicModule.GetMethodMetadataToken mi
                | _ -> failwith $"unexpected method kind %O{m}"

            token >>> 24 |> shouldEqual 0x0A
            dynamicModule.ResolveSignature token |> shouldEqual definitionSignature

            EmitScopeReferences.importMethodSignature (ImmutableArray.Create<byte> definitionSignature)
            |> shouldEqual (MethodSignatureImport.Copied (ImmutableArray.Create<byte> definitionSignature))

            let parent = dynamicModule.GetTypeMetadataToken m.DeclaringType
            parent >>> 24 |> shouldEqual 0x01
            scope <- withTypeRefs (parent &&& 0xFFFFFF) scope

            let rid, next =
                EmitScopeReferences.defineMemberRef
                    (MemberRefParent.TypeRef (TypeRefRid (parent &&& 0xFFFFFF)))
                    m.Name
                    (ImmutableArray.Create<byte> definitionSignature)
                    scope

            rid.Rid |> shouldEqual (token &&& 0xFFFFFF)
            scope <- next

    [<Test>]
    let ``an empty name or signature, a signature that ends early, and one that is not a method's are refused``
        ()
        : unit
        =
        let scope = EmitScope.create "Dyn" Guid.Empty
        let parent = MemberRefParent.TypeDef (TypeDefRid 1)
        let signature = ImmutableArray.Create<byte> [| 0x20uy ; 0x00uy ; 0x01uy |]

        Assert.Throws<ArgumentException> (fun () ->
            EmitScopeReferences.defineMemberRef parent "" signature scope |> ignore
        )
        |> ignore

        Assert.Throws<ArgumentException> (fun () ->
            EmitScopeReferences.defineMemberRef parent "M" ImmutableArray.Empty scope
            |> ignore
        )
        |> ignore

        Assert.Throws<ArgumentException> (fun () ->
            EmitScopeReferences.tryFindMemberRef parent "" signature scope |> ignore
        )
        |> ignore

        let refused (bytes : byte list) : unit =
            Assert.Throws<ArgumentException> (fun () ->
                EmitScopeReferences.importMethodSignature (ImmutableArray.CreateRange bytes)
                |> ignore
            )
            |> ignore

        refused []
        // One parameter, but no type for it.
        refused [ 0x20uy ; 0x01uy ; 0x01uy ]
        // A compressed integer cut short.
        refused [ 0x20uy ; 0x00uy ; 0x13uy ; 0x80uy ]
        // A field's, a local, a property's and an instantiation's signature, each of which would
        // otherwise be read whole as a method's.
        refused [ 0x06uy ; 0x00uy ; 0x08uy ]
        refused [ 0x07uy ; 0x00uy ; 0x08uy ]
        refused [ 0x08uy ; 0x00uy ; 0x08uy ]
        refused [ 0x0Auy ; 0x00uy ; 0x08uy ]
        // `VALUEARRAY`, and an element type past `ELEMENT_TYPE_MAX`.
        refused [ 0x00uy ; 0x00uy ; 0x17uy ]
        refused [ 0x00uy ; 0x00uy ; 0x22uy ]

    /// Method signatures that CoreCLR reads but did not write, each with the bytes .NET 10's emitter
    /// writes into a dynamic module for it, measured through `Module.ResolveSignature`.
    let private noncanonicalSignatures : (string * byte list * byte list) list =
        [
            "MethodParameterShortest", [ 0x30 ; 0x01 ; 0x00 ; 0x1E ; 0x00 ], [ 0x30 ; 0x01 ; 0x00 ; 0x1E ; 0x00 ]
            "MethodParameterTwoBytes", [ 0x30 ; 0x01 ; 0x00 ; 0x1E ; 0x80 ; 0x00 ], [ 0x30 ; 0x01 ; 0x00 ; 0x1E ; 0x00 ]
            "MethodParameterFourBytes",
            [ 0x30 ; 0x01 ; 0x00 ; 0x1E ; 0xC0 ; 0x00 ; 0x00 ; 0x00 ],
            [ 0x30 ; 0x01 ; 0x00 ; 0x1E ; 0x00 ]
            "TypeParameterTwoBytes", [ 0x20 ; 0x00 ; 0x13 ; 0x80 ; 0x00 ], [ 0x20 ; 0x00 ; 0x13 ; 0x00 ]
            "ParameterCountTwoBytes", [ 0x20 ; 0x80 ; 0x01 ; 0x01 ; 0x08 ], [ 0x20 ; 0x80 ; 0x01 ; 0x01 ; 0x08 ]
            "ArityTwoBytes", [ 0x30 ; 0x80 ; 0x01 ; 0x00 ; 0x01 ], [ 0x30 ; 0x80 ; 0x01 ; 0x00 ; 0x01 ]
            "ArraySizeTwoBytes",
            [ 0x20 ; 0x00 ; 0x14 ; 0x08 ; 0x01 ; 0x01 ; 0x80 ; 0x05 ; 0x00 ],
            [ 0x20 ; 0x00 ; 0x14 ; 0x08 ; 0x01 ; 0x01 ; 0x80 ; 0x05 ; 0x00 ]
            "VectorOfTypeParameterFourBytes",
            [ 0x20 ; 0x00 ; 0x1D ; 0x13 ; 0xC0 ; 0x00 ; 0x00 ; 0x00 ],
            [ 0x20 ; 0x00 ; 0x1D ; 0x13 ; 0x00 ]
            "FunctionPointerCountTwoBytes",
            [ 0x20 ; 0x01 ; 0x01 ; 0x1B ; 0x00 ; 0x80 ; 0x00 ; 0x08 ],
            [ 0x20 ; 0x01 ; 0x01 ; 0x1B ; 0x00 ; 0x80 ; 0x00 ; 0x08 ]
            "TrailingByte", [ 0x20 ; 0x00 ; 0x08 ; 0x08 ], [ 0x20 ; 0x00 ; 0x08 ]
            "PointerToTypeParameterTwoBytes",
            [ 0x20 ; 0x00 ; 0x0F ; 0x13 ; 0x80 ; 0x00 ],
            [ 0x20 ; 0x00 ; 0x0F ; 0x13 ; 0x00 ]
        ]
        |> List.map (fun (name, definition, written) -> name, List.map byte definition, List.map byte written)

    /// An image with an abstract class `C<T>` declaring one abstract method per signature, each named
    /// as the signature is.
    let private noncanonicalImage () : byte[] =
        let metadata = MetadataBuilder ()

        metadata.AddModule (
            0,
            metadata.GetOrAddString "Noncanonical.dll",
            metadata.GetOrAddGuid Guid.Empty,
            GuidHandle (),
            GuidHandle ()
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "Noncanonical",
            Version (1, 0, 0, 0),
            StringHandle (),
            BlobHandle (),
            enum<AssemblyFlags> 0,
            AssemblyHashAlgorithm.None
        )
        |> ignore<AssemblyDefinitionHandle>

        let coreLib = typeof<obj>.Assembly.GetName ()

        let coreLibRef =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString coreLib.Name,
                coreLib.Version,
                StringHandle (),
                metadata.GetOrAddBlob (coreLib.GetPublicKeyToken ()),
                enum<AssemblyFlags> 0,
                BlobHandle ()
            )

        let objectRef =
            metadata.AddTypeReference (
                AssemblyReferenceHandle.op_Implicit coreLibRef,
                metadata.GetOrAddString "System",
                metadata.GetOrAddString "Object"
            )

        metadata.AddTypeDefinition (
            enum<TypeAttributes> 0,
            StringHandle (),
            metadata.GetOrAddString "<Module>",
            EntityHandle (),
            MetadataTokens.FieldDefinitionHandle 1,
            MetadataTokens.MethodDefinitionHandle 1
        )
        |> ignore<TypeDefinitionHandle>

        let c =
            metadata.AddTypeDefinition (
                TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Class,
                StringHandle (),
                metadata.GetOrAddString "C`1",
                TypeReferenceHandle.op_Implicit objectRef,
                MetadataTokens.FieldDefinitionHandle 1,
                MetadataTokens.MethodDefinitionHandle 1
            )

        let methodGenerics =
            [
                for name, definition, _ in noncanonicalSignatures do
                    let m =
                        metadata.AddMethodDefinition (
                            MethodAttributes.Public
                            ||| MethodAttributes.Virtual
                            ||| MethodAttributes.Abstract
                            ||| MethodAttributes.HideBySig
                            ||| MethodAttributes.NewSlot,
                            MethodImplAttributes.IL,
                            metadata.GetOrAddString name,
                            metadata.GetOrAddBlob (List.toArray definition),
                            -1,
                            MetadataTokens.ParameterHandle 1
                        )

                    if definition.Head &&& 0x10uy <> 0uy then
                        yield (MethodDefinitionHandle.op_Implicit m : EntityHandle)
            ]

        // The GenericParam table is sorted by its owner's TypeOrMethodDef coded index.
        let coded (owner : EntityHandle) : int =
            (MetadataTokens.GetRowNumber owner <<< 1)
            ||| (if owner.Kind = HandleKind.MethodDefinition then 1 else 0)

        for owner in
            ((TypeDefinitionHandle.op_Implicit c : EntityHandle) :: methodGenerics)
            |> List.sortBy coded do
            metadata.AddGenericParameter (owner, GenericParameterAttributes.None, metadata.GetOrAddString "T", 0)
            |> ignore<GenericParameterHandle>

        let image = BlobBuilder ()

        ManagedPEBuilder(
            PEHeaderBuilder (imageCharacteristics = Characteristics.Dll),
            MetadataRootBuilder metadata,
            BlobBuilder ()
        )
            .Serialize
            image
        |> ignore<BlobContentId>

        image.ToArray ()

    [<Test>]
    let ``real .NET imports a signature it did not write as importMethodSignature says`` () : unit =
        let image = noncanonicalImage ()
        let loaded = Runtime.Loader.AssemblyLoadContext ("Noncanonical", true)

        try
            let c = loaded.LoadFromStream(new IO.MemoryStream (image)).GetType ("C`1", true)

            let dynamicModule =
                AssemblyBuilder
                    .DefineDynamicAssembly(AssemblyName "NoncanonicalOracle", AssemblyBuilderAccess.RunAndCollect)
                    .DefineDynamicModule
                    "NoncanonicalOracle"

            for name, definition, written in noncanonicalSignatures do
                let m = c.GetMethod (name, BindingFlags.Public ||| BindingFlags.Instance)

                m.Module.ResolveSignature m.MetadataToken
                |> List.ofArray
                |> shouldEqual definition

                let token = dynamicModule.GetMethodMetadataToken m
                dynamicModule.ResolveSignature token |> List.ofArray |> shouldEqual written

                EmitScopeReferences.importMethodSignature (ImmutableArray.CreateRange definition)
                |> shouldEqual (MethodSignatureImport.Copied (ImmutableArray.CreateRange written))
        finally
            loaded.Unload ()
