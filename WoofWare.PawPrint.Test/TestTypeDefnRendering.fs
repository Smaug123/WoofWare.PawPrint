namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Tests for <see cref="IlFormatting.renderTypeDefn"/>: the dump layer's own
/// rendering of a <see cref="TypeDefn"/>.
///
/// <c>TypeDefn.ToString</c> is a Domain-layer debug rendering, and three of its
/// cases carry no information a disassembly reader can use:
/// <c>FromDefinition</c> collapses to <c>&lt;type defined in Foo&gt;</c>, and the
/// two generic-parameter cases to <c>&lt;type param 0&gt;</c> /
/// <c>&lt;method param 0&gt;</c>. IlDump must resolve those itself.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestTypeDefnRendering =

    // The factory is intentionally undisposed: the returned DumpedAssembly.Logger closes
    // over its sinks, and disposing while the assembly is still live would silently drop
    // events.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    /// The corelib is self-contained: it has TypeDefs in abundance but essentially no TypeRefs,
    /// so on its own it cannot exercise `TypeDefn.FromReference` or the ResolutionScope walk.
    /// This test assembly does reference other assemblies, so the generated properties run
    /// against both.
    let private selfAssembly : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory (Reflection.Assembly.GetExecutingAssembly().Location)

    let private findTypeByName (qualified : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        corelib.TypeDefs.Values
        |> Seq.find (fun td -> IlFormatting.qualifyTypeName corelib.TypeDefs td = qualified)

    let private findMethod
        (typeName : string)
        (methodName : string)
        : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        =
        (findTypeByName typeName).Methods |> List.find (fun m -> m.Name = methodName)

    /// The renderings which mean "this case was left to TypeDefn.ToString".
    /// None of them may appear in IlDump output, at any nesting depth.
    let private opaqueMarkers = [ "<type defined in" ; "<type param" ; "<method param" ]

    let private containsOpaqueMarker (s : string) : bool =
        opaqueMarkers |> List.exists (fun m -> s.Contains (m, StringComparison.Ordinal))

    // ----- generators --------------------------------------------------------

    /// Leaves built from `assembly`'s own metadata, so `FromDefinition` and `FromReference`
    /// carry handles the renderer can actually resolve against it.
    let private genLeaf (assembly : DumpedAssembly) : Gen<TypeDefn> =
        let typeDefHandles = assembly.TypeDefs.Keys |> Seq.truncate 25 |> List.ofSeq
        let typeRefs = assembly.TypeRefs.Values |> Seq.truncate 25 |> List.ofSeq

        [
            yield
                Gen.elements
                    [
                        PrimitiveType.Boolean
                        PrimitiveType.Char
                        PrimitiveType.Int32
                        PrimitiveType.String
                        PrimitiveType.IntPtr
                        PrimitiveType.Object
                    ]
                |> Gen.map TypeDefn.PrimitiveType
            yield Gen.constant TypeDefn.Void
            yield Gen.choose (0, 4) |> Gen.map TypeDefn.GenericTypeParameter
            yield Gen.choose (0, 4) |> Gen.map TypeDefn.GenericMethodParameter
            yield
                Gen.elements typeDefHandles
                |> Gen.map (fun h ->
                    TypeDefn.FromDefinition (
                        ResolvedTypeIdentity.ofTypeDefinition assembly.Name h,
                        SignatureTypeKind.Class
                    )
                )
            // An identity in an assembly we have not loaded: still must not render opaquely.
            yield
                Gen.elements typeDefHandles
                |> Gen.map (fun h ->
                    TypeDefn.FromDefinition (
                        ResolvedTypeIdentity.ofTypeDefinition (Reflection.AssemblyName "Some.Other.Assembly") h,
                        SignatureTypeKind.ValueType
                    )
                )
            // An assembly with no TypeRefs at all (the corelib is one) simply cannot contribute
            // this case; `every assembly under test is covered` keeps that from going unnoticed
            // across the whole set.
            if not (List.isEmpty typeRefs) then
                yield
                    Gen.elements typeRefs
                    |> Gen.map (fun r -> TypeDefn.FromReference (r, SignatureTypeKind.Class))
        ]
        |> Gen.oneof

    let private signatureHeader : ComparableSignatureHeader =
        ComparableSignatureHeader.Make (
            SignatureHeader (SignatureKind.Method, SignatureCallingConvention.Default, SignatureAttributes.None)
        )

    let private genTypeDefn (assembly : DumpedAssembly) : Gen<TypeDefn> =
        let genLeaf = genLeaf assembly

        let rec go (size : int) : Gen<TypeDefn> =
            if size <= 0 then
                genLeaf
            else
                let child = go (size / 2)

                Gen.oneof
                    [
                        genLeaf
                        Gen.zip child (Gen.choose (1, 3))
                        |> Gen.map (fun (elt, rank) -> TypeDefn.Array (elt, rank))
                        child |> Gen.map TypeDefn.Pinned
                        child |> Gen.map TypeDefn.Pointer
                        child |> Gen.map TypeDefn.Byref
                        child |> Gen.map TypeDefn.OneDimensionalArrayLowerBoundZero
                        Gen.zip3 child child (ArbMap.defaults |> ArbMap.generate<bool>)
                        |> Gen.map (fun (unmodified, modifier, isRequired) ->
                            TypeDefn.Modified
                                {
                                    Unmodified = unmodified
                                    Modifier = modifier
                                    IsRequired = isRequired
                                }
                        )
                        Gen.zip child (Gen.listOfLength 2 child)
                        |> Gen.map (fun (generic, args) ->
                            TypeDefn.GenericInstantiation (generic, ImmutableArray.CreateRange args)
                        )
                        Gen.zip (Gen.listOfLength 2 child) child
                        |> Gen.map (fun (pars, ret) ->
                            TypeDefn.FunctionPointer
                                {
                                    Header = signatureHeader
                                    ParameterTypes = pars
                                    GenericParameterCount = 0
                                    RequiredParameterCount = List.length pars
                                    ReturnType = MethodReturnType.Returns ret
                                }
                        )
                    ]

        Gen.sized go

    let private genScope : Gen<GenericScope> =
        let names = Gen.elements [ "T" ; "TKey" ; "TValue" ; "TOutput" ; "TSelf" ]

        let binding = Gen.zip (Gen.choose (0, 4)) names |> Gen.listOf |> Gen.map Map.ofList

        Gen.zip binding binding
        |> Gen.map (fun (types, methods) ->
            {
                TypeParameters = types
                MethodParameters = methods
            }
        )

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    /// Every property runs against each of these: the corelib, which is all TypeDefs and no
    /// TypeRefs, and this test assembly, which references types it does not define.
    let private assembliesUnderTest : DumpedAssembly list = [ corelib ; selfAssembly ]

    /// Run `property` over TypeDefns generated from, and rendered against, each assembly.
    let private checkAgainstEachAssembly (property : DumpedAssembly -> TypeDefn * GenericScope -> bool) : unit =
        for assembly in assembliesUnderTest do
            Check.One (
                propertyConfig,
                Prop.forAll (Arb.fromGen (Gen.zip (genTypeDefn assembly) genScope)) (property assembly)
            )

    /// The renderer entry points, which differ only in how they spell a TypeRef. Properties
    /// which are about the *walk* rather than about that spelling must hold of both: the two
    /// drifting apart underneath a wrapper is a real failure mode, not a hypothetical one.
    let private renderers : (DumpedAssembly -> GenericScope -> TypeDefn -> string) list =
        [ IlFormatting.renderTypeDefn ; IlFormatting.renderTypeDefnAsName ]

    /// The immediate sub-TypeDefns of a compound. A `Void` return type is not a
    /// child: it is a property of the signature, not a type in it.
    let private children (td : TypeDefn) : TypeDefn list =
        match td with
        | TypeDefn.Array (elt, _) -> [ elt ]
        | TypeDefn.Pinned t
        | TypeDefn.Pointer t
        | TypeDefn.Byref t
        | TypeDefn.OneDimensionalArrayLowerBoundZero t -> [ t ]
        | TypeDefn.Modified m -> [ m.Unmodified ; m.Modifier ]
        | TypeDefn.GenericInstantiation (generic, args) -> generic :: List.ofSeq args
        | TypeDefn.FunctionPointer signature ->
            [
                yield! signature.ParameterTypes
                match signature.ReturnType with
                | MethodReturnType.Void -> ()
                | MethodReturnType.Returns t -> yield t
            ]
        | TypeDefn.PrimitiveType _
        | TypeDefn.FromReference _
        | TypeDefn.FromDefinition _
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _
        | TypeDefn.Void -> []

    // ----- properties --------------------------------------------------------

    [<Test>]
    let ``every assembly under test is covered by the generator`` () : unit =
        // The generator can only produce a TypeDefn case if the assembly has metadata for it,
        // so a case can silently vanish from every property at once. Between them the
        // assemblies under test must cover both ways of naming a type.
        assembliesUnderTest
        |> List.exists (fun a -> not (Seq.isEmpty a.TypeDefs))
        |> shouldEqual true

        assembliesUnderTest
        |> List.exists (fun a -> not (Seq.isEmpty a.TypeRefs))
        |> shouldEqual true

    [<Test>]
    let ``no rendering is opaque, at any depth or under any scope`` () : unit =
        // This is the bug, stated as an invariant: whatever the shape and whatever
        // is in scope, IlDump never emits TypeDefn.ToString's placeholder text.
        checkAgainstEachAssembly (fun assembly (td, scope) ->
            renderers
            |> List.forall (fun render -> render assembly scope td |> containsOpaqueMarker |> not)
        )

    [<Test>]
    let ``every child is rendered by the same renderer`` () : unit =
        // Catches the failure mode directly: a case that forgets to recurse and
        // falls back to `%O`, or to the other entry point, would render its children
        // differently, so their own renderings would no longer appear in the parent's.
        let rec check
            (render : DumpedAssembly -> GenericScope -> TypeDefn -> string)
            (assembly : DumpedAssembly)
            (scope : GenericScope)
            (td : TypeDefn)
            : bool
            =
            let rendered = render assembly scope td

            children td
            |> List.forall (fun child ->
                rendered.Contains (render assembly scope child, StringComparison.Ordinal)
                && check render assembly scope child
            )

        checkAgainstEachAssembly (fun assembly (td, scope) ->
            renderers |> List.forall (fun render -> check render assembly scope td)
        )

    /// How many <c>ref[</c> tags a rendering carries. <c>byref[</c> ends in the same three
    /// characters and is a different thing, so it is discounted rather than matched.
    let private taggedReferenceCount (rendered : string) : int =
        let occurrences (needle : string) : int =
            let rec go (from : int) (acc : int) : int =
                match rendered.IndexOf (needle, from, StringComparison.Ordinal) with
                | -1 -> acc
                | i -> go (i + 1) (acc + 1)

            go 0 0

        occurrences "ref[" - occurrences "byref["

    [<Test>]
    let ``a display name never tags a reference, at any depth`` () : unit =
        // `renderTypeDefnAsName` names types rather than describing them, and that has to hold
        // underneath wrappers too: `[MyGeneric<ArgType[]>]` puts an array between the caller
        // and the referenced leaf, and `arr[ref[Lib.ArgType]]` is not a name.
        checkAgainstEachAssembly (fun assembly (td, scope) ->
            taggedReferenceCount (IlFormatting.renderTypeDefnAsName assembly scope td) = 0
        )

    [<Test>]
    let ``an unbound index renders positionally`` () : unit =
        // ILDasm's !n / !!n. A name is only ever printed for an index the caller
        // has told us is genuinely bound at this point.
        let property (index : int) : bool =
            let index = abs index

            IlFormatting.renderTypeDefn corelib GenericScope.unknown (TypeDefn.GenericTypeParameter index) = $"!%d{index}"
            && IlFormatting.renderTypeDefn corelib GenericScope.unknown (TypeDefn.GenericMethodParameter index) = $"!!%d{index}"

        Check.One (propertyConfig, Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary<int>) property)

    [<Test>]
    let ``binding an index which does not occur changes nothing`` () : unit =
        // The scope is consulted, never merged into the output wholesale.
        checkAgainstEachAssembly (fun assembly (td, scope) ->
            let unoccupied =
                {
                    TypeParameters = Map.add 99 "TNotUsed" scope.TypeParameters
                    MethodParameters = Map.add 99 "TAlsoNotUsed" scope.MethodParameters
                }

            IlFormatting.renderTypeDefn assembly scope td = IlFormatting.renderTypeDefn assembly unoccupied td
        )

    // ----- leaves ------------------------------------------------------------

    [<Test>]
    let ``a FromDefinition in this assembly renders as its qualified name`` () : unit =
        let listType = findTypeByName "System.Collections.Generic.List`1"

        let td =
            TypeDefn.FromDefinition (
                ResolvedTypeIdentity.ofTypeDefinition corelib.Name listType.TypeDefHandle,
                SignatureTypeKind.Class
            )

        IlFormatting.renderTypeDefn corelib GenericScope.unknown td
        |> shouldEqual "System.Collections.Generic.List`1"

    [<Test>]
    let ``a FromDefinition in an assembly we do not have names the assembly and the row`` () : unit =
        // We cannot invent a name we do not have, but the assembly and the token
        // are enough to re-run ildump against the defining assembly.
        let handle = MetadataTokens.TypeDefinitionHandle 0x123

        let td =
            TypeDefn.FromDefinition (
                ResolvedTypeIdentity.ofTypeDefinition (Reflection.AssemblyName "Some.Other.Assembly") handle,
                SignatureTypeKind.Class
            )

        IlFormatting.renderTypeDefn corelib GenericScope.unknown td
        |> shouldEqual "[Some.Other.Assembly]TypeDef(0x02000123)"

    [<Test>]
    let ``a bound generic parameter renders by name`` () : unit =
        let scope =
            {
                TypeParameters = Map.ofList [ 0, "T" ; 1, "TValue" ]
                MethodParameters = Map.ofList [ 0, "TOutput" ]
            }

        IlFormatting.renderTypeDefn corelib scope (TypeDefn.GenericTypeParameter 0)
        |> shouldEqual "!T"

        IlFormatting.renderTypeDefn corelib scope (TypeDefn.GenericTypeParameter 1)
        |> shouldEqual "!TValue"

        IlFormatting.renderTypeDefn corelib scope (TypeDefn.GenericMethodParameter 0)
        |> shouldEqual "!!TOutput"

        // Out of range for this scope: positional, not a wrong name.
        IlFormatting.renderTypeDefn corelib scope (TypeDefn.GenericTypeParameter 3)
        |> shouldEqual "!3"

        IlFormatting.renderTypeDefn corelib scope (TypeDefn.GenericMethodParameter 1)
        |> shouldEqual "!!1"

    [<Test>]
    let ``a generic instantiation renders in angle brackets`` () : unit =
        let listType = findTypeByName "System.Collections.Generic.List`1"

        let td =
            TypeDefn.GenericInstantiation (
                TypeDefn.FromDefinition (
                    ResolvedTypeIdentity.ofTypeDefinition corelib.Name listType.TypeDefHandle,
                    SignatureTypeKind.Class
                ),
                ImmutableArray.Create (TypeDefn.PrimitiveType PrimitiveType.Int32)
            )

        IlFormatting.renderTypeDefn corelib GenericScope.unknown td
        |> shouldEqual "System.Collections.Generic.List`1<int32>"

    // ----- the rendered dump -------------------------------------------------

    [<Test>]
    let ``a method body names the enclosing type's generic parameter`` () : unit =
        // List`1::Add(!T) touches List`1<!T>::_version, _items and _size. Before
        // the fix every one of those read "<type defined in System.Private.CoreLib>".
        let lines =
            IlFormatting.formatMethodLines
                corelib
                "System.Collections.Generic.List`1"
                (findMethod "System.Collections.Generic.List`1" "Add")

        lines |> List.filter containsOpaqueMarker |> shouldEqual []

        lines
        |> List.exists (fun l ->
            l.Contains ("System.Collections.Generic.List`1<!T>::_version", StringComparison.Ordinal)
        )
        |> shouldEqual true

    [<Test>]
    let ``a method's generic parameters are named, not dumped as records`` () : unit =
        let lines =
            IlFormatting.formatMethodLines
                corelib
                "System.Collections.Generic.List`1"
                (findMethod "System.Collections.Generic.List`1" "ConvertAll")

        let header = List.head lines

        // The header used to contain the whole GenericParamFromMetadata record,
        // newlines and all.
        header.Contains ("SequenceNumber", StringComparison.Ordinal)
        |> shouldEqual false

        header.Contains ("ConvertAll<TOutput>", StringComparison.Ordinal)
        |> shouldEqual true

        // ConvertAll returns List<TOutput>, i.e. an instantiation at the *method's*
        // own generic parameter.
        header.Contains ("!!TOutput", StringComparison.Ordinal) |> shouldEqual true

        lines |> List.filter containsOpaqueMarker |> shouldEqual []

    [<Test>]
    let ``a member reference's signature is read in its declaring type's scope`` () : unit =
        // Inside ConvertAll<TOutput>, `ldfld List`1<!!TOutput>::_items` names a field
        // whose declared type is `!0[]` — and that !0 is List`1's own T, *not*
        // anything bound by ConvertAll. Rendering it in the enclosing method's scope
        // would print a confidently wrong name.
        let lines =
            IlFormatting.formatMethodLines
                corelib
                "System.Collections.Generic.List`1"
                (findMethod "System.Collections.Generic.List`1" "ConvertAll")

        lines
        |> List.exists (fun l ->
            l.Contains ("System.Collections.Generic.List`1<!!TOutput>::_items : arr[!T]", StringComparison.Ordinal)
        )
        |> shouldEqual true

    [<Test>]
    let ``a field header names the declaring type's generic parameter`` () : unit =
        let listType = findTypeByName "System.Collections.Generic.List`1"
        let items = listType.Fields |> List.find (fun f -> f.Name = "_items")

        AttributeFormatting.fieldHeader corelib "System.Collections.Generic.List`1" items
        |> shouldEqual "// field System.Collections.Generic.List`1::_items : arr[!T]"
