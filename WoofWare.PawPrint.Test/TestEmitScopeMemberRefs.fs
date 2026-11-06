namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Emit
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

    /// A type in a signature (ECMA-335 II.23.2.12), each token given as its `TypeDefOrRefOrSpecEncoded`
    /// value.
    type private Shape =
        | Primitive of code : byte
        | Named of valueType : bool * token : int
        | SzArray of Shape
        | Pointer of Shape
        | VoidPointer
        | ByRef of Shape
        | TypeParameter of index : int
        | MethodParameter of index : int
        | Instance of valueType : bool * token : int * arguments : Shape list
        | Array of element : Shape * rank : int * sizes : int list * lowerBounds : int list
        | FunctionPointer of MethodShape
        | Modified of required : bool * token : int * Shape

    /// A method signature (ECMA-335 II.23.2.1). A return type of `None` is `void`.
    and private MethodShape =
        {
            Header : byte
            GenericArity : int
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
        | Shape.Array (element, _, _, _) -> namesTypeByToken element
        | Shape.FunctionPointer signature -> methodNamesTypeByToken signature

    and private methodNamesTypeByToken (signature : MethodShape) : bool =
        Option.exists namesTypeByToken signature.Return
        || List.exists namesTypeByToken signature.Parameters

    let rec private encode (shape : Shape) : byte list =
        match shape with
        | Shape.Primitive code -> [ code ]
        | Shape.Named (valueType, token) ->
            (if valueType then 0x11uy else 0x12uy) :: EmitScopeGen.compressedUnsigned token
        | Shape.SzArray inner -> 0x1Duy :: encode inner
        | Shape.Pointer inner -> 0x0Fuy :: encode inner
        | Shape.VoidPointer -> [ 0x0Fuy ; 0x01uy ]
        | Shape.ByRef inner -> 0x10uy :: encode inner
        | Shape.TypeParameter index -> 0x13uy :: EmitScopeGen.compressedUnsigned index
        | Shape.MethodParameter index -> 0x1Euy :: EmitScopeGen.compressedUnsigned index
        | Shape.Instance (valueType, token, arguments) ->
            [
                yield 0x15uy
                yield (if valueType then 0x11uy else 0x12uy)
                yield! EmitScopeGen.compressedUnsigned token
                yield! EmitScopeGen.compressedUnsigned arguments.Length
                for argument in arguments do
                    yield! encode argument
            ]
        | Shape.Array (element, rank, sizes, lowerBounds) ->
            [
                yield 0x14uy
                yield! encode element
                yield! EmitScopeGen.compressedUnsigned rank
                yield! EmitScopeGen.compressedUnsigned sizes.Length
                for size in sizes do
                    yield! EmitScopeGen.compressedUnsigned size
                yield! EmitScopeGen.compressedUnsigned lowerBounds.Length
                // A compressed signed integer below 64 is one byte holding it shifted left once.
                for bound in lowerBounds do
                    yield byte (bound <<< 1)
            ]
        | Shape.FunctionPointer signature -> 0x1Buy :: encodeMethod signature
        | Shape.Modified (required, token, inner) ->
            [
                yield (if required then 0x1Fuy else 0x20uy)
                yield! EmitScopeGen.compressedUnsigned token
                yield! encode inner
            ]

    and private encodeMethod (signature : MethodShape) : byte list =
        [
            yield signature.Header
            if signature.Header &&& 0x10uy <> 0uy then
                yield! EmitScopeGen.compressedUnsigned signature.GenericArity
            yield! EmitScopeGen.compressedUnsigned signature.Parameters.Length
            match signature.Return with
            | None -> yield 0x01uy
            | Some shape -> yield! encode shape
            for parameter in signature.Parameters do
                yield! encode parameter
        ]

    let private genBool : Gen<bool> = Gen.elements [ false ; true ]

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

    let rec private genShape (size : int) : Gen<Shape> =
        let leaf =
            Gen.oneof
                [
                    Gen.map Shape.Primitive (Gen.elements primitives)
                    Gen.constant Shape.VoidPointer
                    Gen.map Shape.TypeParameter (Gen.choose (0, 200))
                    Gen.map Shape.MethodParameter (Gen.choose (0, 200))
                    Gen.map2 (fun v t -> Shape.Named (v, t)) genBool (genToken false)
                ]

        if size <= 0 then
            leaf
        else
            let inner = genShape (size / 2)

            Gen.frequency
                [
                    3, leaf
                    1, Gen.map Shape.SzArray inner
                    1, Gen.map Shape.Pointer inner
                    1, Gen.map Shape.ByRef inner
                    1,
                    Gen.map3
                        (fun (v, t) count arguments -> Shape.Instance (v, t, List.truncate (max 1 count) arguments))
                        (Gen.zip genBool (genToken false))
                        (Gen.choose (1, 3))
                        (Gen.listOfLength 3 inner)
                    1,
                    gen {
                        let! element = inner
                        let! rank = Gen.choose (1, 4)
                        let! sizes = Gen.listOf (Gen.choose (0, 0x5000)) |> Gen.map (List.truncate rank)
                        let! lowerBounds = Gen.listOf (Gen.choose (0, 63)) |> Gen.map (List.truncate rank)
                        return Shape.Array (element, rank, sizes, lowerBounds)
                    }
                    1, Gen.map Shape.FunctionPointer (genMethod (size / 2) false)
                    1, Gen.map3 (fun r t s -> Shape.Modified (r, t, s)) genBool (genToken true) inner
                ]

    /// A method signature: a definition's when `allowGeneric`, and so possibly generic; a function
    /// pointer's otherwise.
    and private genMethod (size : int) (allowGeneric : bool) : Gen<MethodShape> =
        gen {
            let! hasThis = genBool
            let! convention = Gen.elements [ 0x00uy ; 0x05uy ]
            let! generic = if allowGeneric then genBool else Gen.constant false
            let! arity = Gen.choose (1, 3)
            let! returns = Gen.optionOf (genShape size)
            let! parameters = Gen.listOf (genShape size) |> Gen.map (List.truncate 4)

            return
                {
                    Header =
                        convention
                        ||| (if hasThis then 0x20uy else 0uy)
                        ||| (if generic then 0x10uy else 0uy)
                    GenericArity = arity
                    Return = returns
                    Parameters = parameters
                }
        }

    [<Test>]
    let ``a signature names a type by token exactly when a class, value type, instantiation or modifier is in it``
        ()
        : unit
        =
        let mutable named = 0
        let mutable unnamed = 0

        let property (signature : MethodShape) : unit =
            let expected = methodNamesTypeByToken signature

            if expected then
                named <- named + 1
            else
                unnamed <- unnamed + 1

            EmitScopeReferences.methodSignatureNamesTypeByToken (ImmutableArray.CreateRange (encodeMethod signature))
            |> shouldEqual expected

        let arb = Gen.sized (fun size -> genMethod (min size 12) true) |> Arb.fromGen
        Check.One (Config.QuickThrowOnFailure.WithMaxTest 1000, Prop.forAll arb property)
        named |> shouldBeGreaterThan 20
        unnamed |> shouldBeGreaterThan 20

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
    let ``real .NET copies a signature that names no type by token, and gives each reference the token defineMemberRef gives``
        ()
        : unit
        =
        let methods =
            coreLibMethods.Value
            |> List.filter (fun m ->
                not (
                    EmitScopeReferences.methodSignatureNamesTypeByToken (
                        ImmutableArray.Create<byte> (m.Module.ResolveSignature m.MetadataToken)
                    )
                )
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
    let ``an empty name or signature, and a signature with bytes after its last type, are refused`` () : unit =
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

        Assert.Throws<ArgumentException> (fun () ->
            EmitScopeReferences.methodSignatureNamesTypeByToken ImmutableArray.Empty
            |> ignore
        )
        |> ignore

        // A trailing byte the emitter would not copy.
        Assert.Throws<ArgumentException> (fun () ->
            EmitScopeReferences.methodSignatureNamesTypeByToken (
                ImmutableArray.Create<byte> [| 0x20uy ; 0x00uy ; 0x01uy ; 0x08uy |]
            )
            |> ignore
        )
        |> ignore

        EmitScopeReferences.methodSignatureNamesTypeByToken signature
        |> shouldEqual false
