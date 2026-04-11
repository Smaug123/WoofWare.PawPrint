namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.Reflection.Metadata
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open TypeIdentityTestHelpers

[<TestFixture>]
module TestTypeIdentityProperties =

    type private CompiledReferenceScenario =
        {
            TargetAssembly : DumpedAssembly
            ConsumerAssembly : DumpedAssembly
            LoadedAssemblies : ImmutableDictionary<string, DumpedAssembly>
            TargetRef : TypeRef
            TargetPath : TypePath
        }

    type private ResolvedScenario =
        {
            ResolvedAssembly : DumpedAssembly
            Identity : ResolvedTypeIdentity
            TypeInfo : TypeInfo<TypeDefn, TypeDefn>
            TargetAssembly : DumpedAssembly
            TargetPath : TypePath
        }

    type GenericConcretizationScenario =
        {
            AssemblyName : string
            Namespace : string
            PlainName : string
            FirstArgumentName : string
            SecondArgumentName : string
            LeftContainerName : string
            RightContainerName : string
            BoxName : string
            OuterName : string
            InnerName : string
        }

    type private CompiledGenericScenario =
        {
            Assembly : DumpedAssembly
            Plain : TypeInfo<GenericParamFromMetadata, TypeDefn>
            FirstArgument : TypeInfo<GenericParamFromMetadata, TypeDefn>
            SecondArgument : TypeInfo<GenericParamFromMetadata, TypeDefn>
            LeftBox : TypeInfo<GenericParamFromMetadata, TypeDefn>
            RightBox : TypeInfo<GenericParamFromMetadata, TypeDefn>
            Inner : TypeInfo<GenericParamFromMetadata, TypeDefn>
        }

    let private compileSingleAssembly (shape : AssemblyShape) : DumpedAssembly =
        let bytes = compileLibrary shape.Name [] [ AssemblyShape.render shape ]
        dumpedAssembly (Some (shape.Name + ".dll")) bytes

    let private assertResolvedIdentityConsistent
        (resolvedAssembly : DumpedAssembly)
        (identity : ResolvedTypeIdentity)
        (typeInfo : TypeInfo<TypeDefn, TypeDefn>)
        : unit
        =
        identity.Assembly.FullName |> shouldEqual resolvedAssembly.Name.FullName

        let resolved =
            global.WoofWare.PawPrint.AssemblyApi.resolveTypeIdentityDefinition resolvedAssembly identity

        resolved.TypeDefHandle |> shouldEqual typeInfo.TypeDefHandle

    let private assertResolvedScenarioMatchesTarget (scenario : ResolvedScenario) : unit =
        let expectedType =
            AssemblyShape.findTypeDefByPath scenario.TargetAssembly scenario.TargetPath

        scenario.ResolvedAssembly.Name.FullName
        |> shouldEqual scenario.TargetAssembly.Name.FullName

        scenario.Identity
        |> shouldEqual (ResolvedTypeIdentity.ofTypeDefinition scenario.TargetAssembly.Name expectedType.TypeDefHandle)

        global.WoofWare.PawPrint.AssemblyApi.fullName scenario.ResolvedAssembly scenario.Identity
        |> shouldEqual (AssemblyShape.expectedFullName scenario.TargetPath)

        assertResolvedIdentityConsistent scenario.ResolvedAssembly scenario.Identity scenario.TypeInfo

    let private compileTopLevelReferenceScenario (scenario : TopLevelReferenceScenario) : CompiledReferenceScenario =
        let definingBytes =
            compileLibrary
                scenario.DefiningAssemblyName
                []
                [ AssemblyGraphShape.renderTopLevelDefiningAssembly scenario ]

        let consumerBytes =
            compileLibrary
                scenario.ConsumerAssemblyName
                [ metadataReferenceFromImage definingBytes ]
                [ AssemblyGraphShape.renderTopLevelConsumerAssembly scenario ]

        let defining =
            dumpedAssembly (Some (scenario.DefiningAssemblyName + ".dll")) definingBytes

        let consumer =
            dumpedAssembly (Some (scenario.ConsumerAssemblyName + ".dll")) consumerBytes

        let assemblies = loadedAssemblies [ defining ; consumer ]

        let targetRef =
            findTypeRef
                (fun typeRef ->
                    typeRef.Name = scenario.TargetName
                    && typeRef.Namespace = scenario.Namespace
                    && match typeRef.ResolutionScope with
                       | TypeRefResolutionScope.Assembly _ -> true
                       | _ -> false
                )
                consumer

        {
            TargetAssembly = defining
            ConsumerAssembly = consumer
            LoadedAssemblies = assemblies
            TargetRef = targetRef
            TargetPath = AssemblyGraphShape.topLevelTargetPath scenario
        }

    let private compileNestedReferenceScenario (scenario : NestedReferenceScenario) : CompiledReferenceScenario =
        let definingBytes =
            compileLibrary scenario.DefiningAssemblyName [] [ AssemblyGraphShape.renderNestedDefiningAssembly scenario ]

        let consumerBytes =
            compileLibrary
                scenario.ConsumerAssemblyName
                [ metadataReferenceFromImage definingBytes ]
                [ AssemblyGraphShape.renderNestedConsumerAssembly scenario ]

        let defining =
            dumpedAssembly (Some (scenario.DefiningAssemblyName + ".dll")) definingBytes

        let consumer =
            dumpedAssembly (Some (scenario.ConsumerAssemblyName + ".dll")) consumerBytes

        let assemblies = loadedAssemblies [ defining ; consumer ]

        let targetRef =
            findTypeRef
                (fun typeRef ->
                    typeRef.Name = scenario.ChildName
                    && typeRef.Namespace = ""
                    && match typeRef.ResolutionScope with
                       | TypeRefResolutionScope.TypeRef _ -> true
                       | _ -> false
                )
                consumer

        {
            TargetAssembly = defining
            ConsumerAssembly = consumer
            LoadedAssemblies = assemblies
            TargetRef = targetRef
            TargetPath = AssemblyGraphShape.nestedTargetPath scenario
        }

    let private resolveForwardedTopLevelScenario (scenario : ForwardedTopLevelScenario) : ResolvedScenario =
        let targetBytes =
            compileLibrary
                scenario.TargetAssemblyName
                []
                [ AssemblyGraphShape.renderForwardedTopLevelTargetAssembly scenario ]

        // Compile a "stub" with the forwarder's name that defines the type directly,
        // so the consumer's TypeRefs point to the forwarder's assembly name.
        let stubBytes =
            compileLibrary
                scenario.ForwarderAssemblyName
                []
                [ AssemblyGraphShape.renderForwardedTopLevelTargetAssembly scenario ]

        let consumerBytes =
            compileLibrary
                scenario.ConsumerAssemblyName
                [ metadataReferenceFromImage stubBytes ]
                [ AssemblyGraphShape.renderForwardedTopLevelConsumerAssembly scenario ]

        let forwarderBytes =
            compileLibrary
                scenario.ForwarderAssemblyName
                [ metadataReferenceFromImage targetBytes ]
                [ AssemblyGraphShape.renderForwardedTopLevelForwarderAssembly scenario ]

        let target =
            dumpedAssembly (Some (scenario.TargetAssemblyName + ".dll")) targetBytes

        let forwarder =
            dumpedAssembly (Some (scenario.ForwarderAssemblyName + ".dll")) forwarderBytes

        let consumer =
            dumpedAssembly (Some (scenario.ConsumerAssemblyName + ".dll")) consumerBytes

        let assemblies = loadedAssemblies [ target ; forwarder ; consumer ]

        let targetRef =
            findTypeRef
                (fun typeRef ->
                    typeRef.Name = scenario.TargetName
                    && typeRef.Namespace = scenario.Namespace
                    && match typeRef.ResolutionScope with
                       | TypeRefResolutionScope.Assembly _ -> true
                       | _ -> false
                )
                consumer

        let resolvedAssembly, identity, typeInfo =
            global.WoofWare.PawPrint.AssemblyApi.resolveTypeRef assemblies consumer ImmutableArray.Empty targetRef
            |> getResolvedIdentity

        {
            ResolvedAssembly = resolvedAssembly
            Identity = identity
            TypeInfo = typeInfo
            TargetAssembly = target
            TargetPath = AssemblyGraphShape.forwardedTopLevelTargetPath scenario
        }

    let private resolveForwardedNestedScenario (scenario : ForwardedNestedScenario) : ResolvedScenario =
        let targetBytes =
            compileLibrary
                scenario.TargetAssemblyName
                []
                [ AssemblyGraphShape.renderForwardedNestedTargetAssembly scenario ]

        // Compile a "stub" with the forwarder's name that defines the types directly,
        // so the consumer's TypeRefs point to the forwarder's assembly name.
        let stubBytes =
            compileLibrary
                scenario.ForwarderAssemblyName
                []
                [ AssemblyGraphShape.renderForwardedNestedTargetAssembly scenario ]

        let consumerBytes =
            compileLibrary
                scenario.ConsumerAssemblyName
                [ metadataReferenceFromImage stubBytes ]
                [ AssemblyGraphShape.renderForwardedNestedConsumerAssembly scenario ]

        let forwarderBytes =
            compileLibrary
                scenario.ForwarderAssemblyName
                [ metadataReferenceFromImage targetBytes ]
                [ AssemblyGraphShape.renderForwardedNestedForwarderAssembly scenario ]

        let target =
            dumpedAssembly (Some (scenario.TargetAssemblyName + ".dll")) targetBytes

        let forwarder =
            dumpedAssembly (Some (scenario.ForwarderAssemblyName + ".dll")) forwarderBytes

        let consumer =
            dumpedAssembly (Some (scenario.ConsumerAssemblyName + ".dll")) consumerBytes

        let assemblies = loadedAssemblies [ target ; forwarder ; consumer ]

        let targetRef =
            findTypeRef
                (fun typeRef ->
                    typeRef.Name = scenario.ChildName
                    && typeRef.Namespace = ""
                    && match typeRef.ResolutionScope with
                       | TypeRefResolutionScope.TypeRef _ -> true
                       | _ -> false
                )
                consumer

        let resolvedAssembly, identity, typeInfo =
            global.WoofWare.PawPrint.AssemblyApi.resolveTypeRef assemblies consumer ImmutableArray.Empty targetRef
            |> getResolvedIdentity

        {
            ResolvedAssembly = resolvedAssembly
            Identity = identity
            TypeInfo = typeInfo
            TargetAssembly = target
            TargetPath = AssemblyGraphShape.forwardedNestedTargetPath scenario
        }

    let private resolveResolutionScenario (scenario : ResolutionScenario) : ResolvedScenario =
        match scenario with
        | ResolutionScenario.TopLevelReference topLevel ->
            let compiled = compileTopLevelReferenceScenario topLevel

            let resolvedAssembly, identity, typeInfo =
                global.WoofWare.PawPrint.AssemblyApi.resolveTypeRef
                    compiled.LoadedAssemblies
                    compiled.ConsumerAssembly
                    ImmutableArray.Empty
                    compiled.TargetRef
                |> getResolvedIdentity

            {
                ResolvedAssembly = resolvedAssembly
                Identity = identity
                TypeInfo = typeInfo
                TargetAssembly = compiled.TargetAssembly
                TargetPath = compiled.TargetPath
            }
        | ResolutionScenario.NestedReference nested ->
            let compiled = compileNestedReferenceScenario nested

            let resolvedAssembly, identity, typeInfo =
                global.WoofWare.PawPrint.AssemblyApi.resolveTypeRef
                    compiled.LoadedAssemblies
                    compiled.ConsumerAssembly
                    ImmutableArray.Empty
                    compiled.TargetRef
                |> getResolvedIdentity

            {
                ResolvedAssembly = resolvedAssembly
                Identity = identity
                TypeInfo = typeInfo
                TargetAssembly = compiled.TargetAssembly
                TargetPath = compiled.TargetPath
            }
        | ResolutionScenario.ForwardedTopLevel forwarded -> resolveForwardedTopLevelScenario forwarded
        | ResolutionScenario.ForwardedNested forwarded -> resolveForwardedNestedScenario forwarded

    let private metadataTypeName (name : string) (genericArity : int) : string =
        AssemblyShape.metadataName
            {
                Name = name
                GenericArity = genericArity
            }

    let private asClassTypeDefn (assy : DumpedAssembly) (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>) : TypeDefn =
        TypeDefn.FromDefinition (
            ComparableTypeDefinitionHandle.Make ty.TypeDefHandle,
            assy.Name.FullName,
            SignatureTypeKind.Class
        )

    let private instantiateSingleArgumentType
        (assy : DumpedAssembly)
        (genericDef : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (arg : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : TypeDefn
        =
        TypeDefn.GenericInstantiation (
            asClassTypeDefn assy genericDef,
            ImmutableArray.Create (asClassTypeDefn assy arg)
        )

    let private genericIdentifierPool : string list =
        [
            "Plain"
            "ArgumentA"
            "ArgumentB"
            "Left"
            "Right"
            "Box"
            "Outer"
            "Inner"
            "Holder"
            "Node"
            "Alpha"
            "Beta"
            "Gamma"
            "Delta"
            "Epsilon"
            "Zeta"
            "Eta"
            "Theta"
            "Iota"
            "Kappa"
        ]

    let rec private genDistinctFromPool<'a when 'a : equality> (count : int) (pool : 'a list) : Gen<'a list> =
        if count <= 0 then
            Gen.constant []
        else
            gen {
                let! raw = Gen.listOfLength (count * 3 + 2) (Gen.elements pool)
                let picked = raw |> List.distinct |> List.truncate count

                if List.length picked = count then
                    return picked
                else
                    return! genDistinctFromPool count pool
            }

    let private genGenericConcretizationScenario : Gen<GenericConcretizationScenario> =
        gen {
            let! salt = Gen.choose (1, Int32.MaxValue)
            let! namespaceName = Gen.elements [ "N" ; "M" ; "Q" ]
            let! names = genDistinctFromPool 8 genericIdentifierPool

            return
                {
                    AssemblyName = $"TypeIdentity.Property.Generic.{salt}"
                    Namespace = namespaceName
                    PlainName = names.[0]
                    FirstArgumentName = names.[1]
                    SecondArgumentName = names.[2]
                    LeftContainerName = names.[3]
                    RightContainerName = names.[4]
                    BoxName = names.[5]
                    OuterName = names.[6]
                    InnerName = names.[7]
                }
        }

    let private renderGenericConcretizationAssembly (scenario : GenericConcretizationScenario) : string =
        $"""
namespace {scenario.Namespace}
{{
    public class {scenario.PlainName} {{ }}
    public class {scenario.FirstArgumentName} {{ }}
    public class {scenario.SecondArgumentName} {{ }}

    public class {scenario.LeftContainerName}
    {{
        public class {scenario.BoxName}<T> {{ }}
    }}

    public class {scenario.RightContainerName}
    {{
        public class {scenario.BoxName}<T> {{ }}
    }}

    public class {scenario.OuterName}<T>
    {{
        public class {scenario.InnerName}
        {{
            public T Value;
        }}
    }}
}}
"""

    let private compileGenericScenario (scenario : GenericConcretizationScenario) : CompiledGenericScenario =
        let bytes =
            compileLibrary scenario.AssemblyName [] [ renderGenericConcretizationAssembly scenario ]

        let assy = dumpedAssembly (Some (scenario.AssemblyName + ".dll")) bytes
        let plain = getTopLevelTypeDef assy scenario.Namespace scenario.PlainName

        let firstArgument =
            getTopLevelTypeDef assy scenario.Namespace scenario.FirstArgumentName

        let secondArgument =
            getTopLevelTypeDef assy scenario.Namespace scenario.SecondArgumentName

        let leftContainer =
            getTopLevelTypeDef assy scenario.Namespace scenario.LeftContainerName

        let rightContainer =
            getTopLevelTypeDef assy scenario.Namespace scenario.RightContainerName

        let leftBox =
            getNestedTypeDef assy leftContainer (metadataTypeName scenario.BoxName 1)

        let rightBox =
            getNestedTypeDef assy rightContainer (metadataTypeName scenario.BoxName 1)

        let outer =
            getTopLevelTypeDef assy scenario.Namespace (metadataTypeName scenario.OuterName 1)

        let inner = getNestedTypeDef assy outer scenario.InnerName

        {
            Assembly = assy
            Plain = plain
            FirstArgument = firstArgument
            SecondArgument = secondArgument
            LeftBox = leftBox
            RightBox = rightBox
            Inner = inner
        }

    type TypeIdentityPropertyArbitraries =
        static member GenericConcretizationScenario () : Arbitrary<GenericConcretizationScenario> =
            Arb.fromGen genGenericConcretizationScenario

    let private assemblyShapeConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 25

    let private assemblyGraphConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 35

    let private genericConcretizationConfig : Config =
        Config.QuickThrowOnFailure.WithMaxTest 30

    [<Test>]
    let ``Property A1 type-definition lookup partition is complete and disjoint`` () : unit =
        Check.One (
            assemblyShapeConfig,
            Prop.forAll
                (AssemblyShapeArbitraries.AssemblyShape ())
                (fun (shape : AssemblyShape) ->
                    let assy = compileSingleAssembly shape

                    for KeyValue (handle, ty) in assy.TypeDefs do
                        let foundAsTopLevel =
                            let found =
                                assy.TryGetTopLevelTypeDef ty.Namespace ty.Name |> Option.map _.TypeDefHandle

                            found = Some handle

                        let foundAsNested =
                            let found =
                                assy.TryGetNestedTypeDef ty.DeclaringType ty.Name |> Option.map _.TypeDefHandle

                            found = Some handle

                        (foundAsTopLevel <> foundAsNested) |> shouldEqual true
                )
        )

    [<Test>]
    let ``Property A2 resolved identities roundtrip to the original type definition handle`` () : unit =
        Check.One (
            assemblyShapeConfig,
            Prop.forAll
                (AssemblyShapeArbitraries.AssemblyShape ())
                (fun (shape : AssemblyShape) ->
                    let assy = compileSingleAssembly shape

                    for KeyValue (handle, _) in assy.TypeDefs do
                        let identity = ResolvedTypeIdentity.ofTypeDefinition assy.Name handle

                        let resolved =
                            global.WoofWare.PawPrint.AssemblyApi.resolveTypeIdentityDefinition assy identity

                        resolved.TypeDefHandle |> shouldEqual handle
                )
        )

    [<Test>]
    let ``Property A3 full names match the independently recovered model path`` () : unit =
        Check.One (
            assemblyShapeConfig,
            Prop.forAll
                (AssemblyShapeArbitraries.AssemblyShape ())
                (fun (shape : AssemblyShape) ->
                    let assy = compileSingleAssembly shape

                    for path in AssemblyShape.allTypePaths shape do
                        let typeDef = AssemblyShape.findTypeDefByPath assy path
                        let identity = ResolvedTypeIdentity.ofTypeDefinition assy.Name typeDef.TypeDefHandle

                        global.WoofWare.PawPrint.AssemblyApi.fullName assy identity
                        |> shouldEqual (AssemblyShape.expectedFullName path)
                )
        )

    [<Test>]
    let ``Property A4 full names are injective within an assembly`` () : unit =
        Check.One (
            assemblyShapeConfig,
            Prop.forAll
                (AssemblyShapeArbitraries.AssemblyShape ())
                (fun (shape : AssemblyShape) ->
                    let assy = compileSingleAssembly shape

                    let fullNames =
                        assy.TypeDefs.Keys
                        |> Seq.map (fun handle ->
                            global.WoofWare.PawPrint.AssemblyApi.fullName
                                assy
                                (ResolvedTypeIdentity.ofTypeDefinition assy.Name handle)
                        )
                        |> Seq.toList

                    fullNames.Length |> shouldEqual (fullNames |> Set.ofList |> Set.count)
                )
        )

    [<Test>]
    let ``Property B1 cross-assembly top-level TypeRef resolution is idempotent`` () : unit =
        Check.One (
            assemblyGraphConfig,
            Prop.forAll
                (AssemblyGraphArbitraries.TopLevelReferenceScenario ())
                (fun (scenario : TopLevelReferenceScenario) ->
                    let compiled = compileTopLevelReferenceScenario scenario

                    let firstAssembly, firstIdentity, firstTypeInfo =
                        global.WoofWare.PawPrint.AssemblyApi.resolveTypeRef
                            compiled.LoadedAssemblies
                            compiled.ConsumerAssembly
                            ImmutableArray.Empty
                            compiled.TargetRef
                        |> getResolvedIdentity

                    let secondAssembly, secondIdentity, secondTypeInfo =
                        global.WoofWare.PawPrint.AssemblyApi.resolveTypeRef
                            compiled.LoadedAssemblies
                            compiled.ConsumerAssembly
                            ImmutableArray.Empty
                            compiled.TargetRef
                        |> getResolvedIdentity

                    firstAssembly.Name.FullName |> shouldEqual secondAssembly.Name.FullName
                    firstIdentity |> shouldEqual secondIdentity
                    firstTypeInfo.TypeDefHandle |> shouldEqual secondTypeInfo.TypeDefHandle

                    assertResolvedScenarioMatchesTarget
                        {
                            ResolvedAssembly = firstAssembly
                            Identity = firstIdentity
                            TypeInfo = firstTypeInfo
                            TargetAssembly = compiled.TargetAssembly
                            TargetPath = compiled.TargetPath
                        }
                )
        )

    [<Test>]
    let ``Property B2 nested TypeRef parent-chain resolution selects the correct nested definition`` () : unit =
        Check.One (
            assemblyGraphConfig,
            Prop.forAll
                (AssemblyGraphArbitraries.NestedReferenceScenario ())
                (fun (scenario : NestedReferenceScenario) ->
                    let compiled = compileNestedReferenceScenario scenario

                    let resolvedAssembly, identity, typeInfo =
                        global.WoofWare.PawPrint.AssemblyApi.resolveTypeRef
                            compiled.LoadedAssemblies
                            compiled.ConsumerAssembly
                            ImmutableArray.Empty
                            compiled.TargetRef
                        |> getResolvedIdentity

                    assertResolvedScenarioMatchesTarget
                        {
                            ResolvedAssembly = resolvedAssembly
                            Identity = identity
                            TypeInfo = typeInfo
                            TargetAssembly = compiled.TargetAssembly
                            TargetPath = compiled.TargetPath
                        }
                )
        )

    [<Test>]
    let ``Property B3 forwarded top-level exports agree with direct lookup in the target assembly`` () : unit =
        Check.One (
            assemblyGraphConfig,
            Prop.forAll
                (AssemblyGraphArbitraries.ForwardedTopLevelScenario ())
                (fun (scenario : ForwardedTopLevelScenario) ->
                    resolveForwardedTopLevelScenario scenario |> assertResolvedScenarioMatchesTarget
                )
        )

    [<Test>]
    let ``Property B4 nested forwarded exports preserve the exported parent chain`` () : unit =
        Check.One (
            assemblyGraphConfig,
            Prop.forAll
                (AssemblyGraphArbitraries.ForwardedNestedScenario ())
                (fun (scenario : ForwardedNestedScenario) ->
                    resolveForwardedNestedScenario scenario |> assertResolvedScenarioMatchesTarget
                )
        )

    [<Test>]
    let ``Property B5 resolved identities always point at a valid definition in the returned assembly`` () : unit =
        Check.One (
            assemblyGraphConfig,
            Prop.forAll
                (AssemblyGraphArbitraries.ResolutionScenario ())
                (fun (scenario : ResolutionScenario) ->
                    let resolved = resolveResolutionScenario scenario
                    assertResolvedIdentityConsistent resolved.ResolvedAssembly resolved.Identity resolved.TypeInfo
                )
        )

    [<Test>]
    let ``Property C1 non-generic type-definition concretisation is idempotent`` () : unit =
        Check.One (
            genericConcretizationConfig,
            Prop.forAll
                (TypeIdentityPropertyArbitraries.GenericConcretizationScenario ())
                (fun (scenario : GenericConcretizationScenario) ->
                    let compiled = compileGenericScenario scenario
                    let ctx = emptyConcretizationContext [ compiled.Assembly ]

                    let firstHandle, ctx =
                        TypeConcretization.concretizeTypeDefinition
                            ctx
                            compiled.Assembly.Name
                            (ComparableTypeDefinitionHandle.Make compiled.Plain.TypeDefHandle)

                    let secondHandle, _ =
                        TypeConcretization.concretizeTypeDefinition
                            ctx
                            compiled.Assembly.Name
                            (ComparableTypeDefinitionHandle.Make compiled.Plain.TypeDefHandle)

                    firstHandle |> shouldEqual secondHandle
                )
        )

    [<Test>]
    let ``Property C2 same nominal identity plus same generic arguments is idempotent`` () : unit =
        Check.One (
            genericConcretizationConfig,
            Prop.forAll
                (TypeIdentityPropertyArbitraries.GenericConcretizationScenario ())
                (fun (scenario : GenericConcretizationScenario) ->
                    let compiled = compileGenericScenario scenario
                    let ctx = emptyConcretizationContext [ compiled.Assembly ]

                    let instantiated =
                        instantiateSingleArgumentType compiled.Assembly compiled.LeftBox compiled.FirstArgument

                    let firstHandle, ctx =
                        TypeConcretization.concretizeType
                            ctx
                            (NoAssemblyLoad ())
                            compiled.Assembly.Name
                            ImmutableArray.Empty
                            ImmutableArray.Empty
                            instantiated

                    let secondHandle, _ =
                        TypeConcretization.concretizeType
                            ctx
                            (NoAssemblyLoad ())
                            compiled.Assembly.Name
                            ImmutableArray.Empty
                            ImmutableArray.Empty
                            instantiated

                    firstHandle |> shouldEqual secondHandle
                )
        )

    [<Test>]
    let ``Property C3 same nominal identity plus different generic arguments yields different handles`` () : unit =
        Check.One (
            genericConcretizationConfig,
            Prop.forAll
                (TypeIdentityPropertyArbitraries.GenericConcretizationScenario ())
                (fun (scenario : GenericConcretizationScenario) ->
                    let compiled = compileGenericScenario scenario
                    let ctx = emptyConcretizationContext [ compiled.Assembly ]

                    let firstInstantiated =
                        instantiateSingleArgumentType compiled.Assembly compiled.LeftBox compiled.FirstArgument

                    let secondInstantiated =
                        instantiateSingleArgumentType compiled.Assembly compiled.LeftBox compiled.SecondArgument

                    let firstHandle, ctx =
                        TypeConcretization.concretizeType
                            ctx
                            (NoAssemblyLoad ())
                            compiled.Assembly.Name
                            ImmutableArray.Empty
                            ImmutableArray.Empty
                            firstInstantiated

                    let secondHandle, _ =
                        TypeConcretization.concretizeType
                            ctx
                            (NoAssemblyLoad ())
                            compiled.Assembly.Name
                            ImmutableArray.Empty
                            ImmutableArray.Empty
                            secondInstantiated

                    firstHandle |> shouldNotEqual secondHandle
                )
        )

    [<Test>]
    let ``Property C4 distinct nominal identities plus the same generic arguments yield different handles`` () : unit =
        Check.One (
            genericConcretizationConfig,
            Prop.forAll
                (TypeIdentityPropertyArbitraries.GenericConcretizationScenario ())
                (fun (scenario : GenericConcretizationScenario) ->
                    let compiled = compileGenericScenario scenario
                    let ctx = emptyConcretizationContext [ compiled.Assembly ]

                    let leftInstantiated =
                        instantiateSingleArgumentType compiled.Assembly compiled.LeftBox compiled.FirstArgument

                    let rightInstantiated =
                        instantiateSingleArgumentType compiled.Assembly compiled.RightBox compiled.FirstArgument

                    let leftHandle, ctx =
                        TypeConcretization.concretizeType
                            ctx
                            (NoAssemblyLoad ())
                            compiled.Assembly.Name
                            ImmutableArray.Empty
                            ImmutableArray.Empty
                            leftInstantiated

                    let rightHandle, _ =
                        TypeConcretization.concretizeType
                            ctx
                            (NoAssemblyLoad ())
                            compiled.Assembly.Name
                            ImmutableArray.Empty
                            ImmutableArray.Empty
                            rightInstantiated

                    leftHandle |> shouldNotEqual rightHandle
                )
        )

    [<Test>]
    let ``Property C5 enclosing generic arguments propagate into nested concretisation`` () : unit =
        Check.One (
            genericConcretizationConfig,
            Prop.forAll
                (TypeIdentityPropertyArbitraries.GenericConcretizationScenario ())
                (fun (scenario : GenericConcretizationScenario) ->
                    let compiled = compileGenericScenario scenario
                    compiled.Inner.Generics.Length |> shouldEqual 1

                    compiled.Inner.Fields
                    |> List.exactlyOne
                    |> _.Signature
                    |> shouldEqual (TypeDefn.GenericTypeParameter 0)

                    let firstInstantiated =
                        instantiateSingleArgumentType compiled.Assembly compiled.Inner compiled.FirstArgument

                    let secondInstantiated =
                        instantiateSingleArgumentType compiled.Assembly compiled.Inner compiled.SecondArgument

                    let ctx = emptyConcretizationContext [ compiled.Assembly ]

                    let firstArgumentHandle, ctx =
                        TypeConcretization.concretizeTypeDefinition
                            ctx
                            compiled.Assembly.Name
                            (ComparableTypeDefinitionHandle.Make compiled.FirstArgument.TypeDefHandle)

                    let secondArgumentHandle, ctx =
                        TypeConcretization.concretizeTypeDefinition
                            ctx
                            compiled.Assembly.Name
                            (ComparableTypeDefinitionHandle.Make compiled.SecondArgument.TypeDefHandle)

                    let firstHandle, ctx =
                        TypeConcretization.concretizeType
                            ctx
                            (NoAssemblyLoad ())
                            compiled.Assembly.Name
                            ImmutableArray.Empty
                            ImmutableArray.Empty
                            firstInstantiated

                    let repeatedFirstHandle, ctx =
                        TypeConcretization.concretizeType
                            ctx
                            (NoAssemblyLoad ())
                            compiled.Assembly.Name
                            ImmutableArray.Empty
                            ImmutableArray.Empty
                            firstInstantiated

                    let secondHandle, ctx =
                        TypeConcretization.concretizeType
                            ctx
                            (NoAssemblyLoad ())
                            compiled.Assembly.Name
                            ImmutableArray.Empty
                            ImmutableArray.Empty
                            secondInstantiated

                    firstHandle |> shouldEqual repeatedFirstHandle
                    firstHandle |> shouldNotEqual secondHandle

                    let firstConcrete =
                        AllConcreteTypes.lookup firstHandle ctx.ConcreteTypes
                        |> Option.defaultWith (fun () -> failwith "Expected first concretized nested type to exist")

                    let secondConcrete =
                        AllConcreteTypes.lookup secondHandle ctx.ConcreteTypes
                        |> Option.defaultWith (fun () -> failwith "Expected second concretized nested type to exist")

                    firstConcrete.Generics.Length |> shouldEqual 1
                    secondConcrete.Generics.Length |> shouldEqual 1
                    firstConcrete.Generics.[0] |> shouldEqual firstArgumentHandle
                    secondConcrete.Generics.[0] |> shouldEqual secondArgumentHandle
                )
        )
