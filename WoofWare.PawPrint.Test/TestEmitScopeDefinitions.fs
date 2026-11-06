namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Text
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint


/// The types CoreCLR's emitter defines in a dynamic module for `TypeBuilder_DefineType`, each checked
/// for a duplicate first; and the runtime's refusal to load such a type before it is created.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEmitScopeDefinitions =

    /// Names that split alike or apart, that differ only in case, that collide with `<Module>`, and
    /// that differ only in a lone surrogate, which the emitter stores as the same UTF-8.
    let private typeNames : string list =
        [
            "A"
            "N.A"
            "n.a"
            "N.B"
            "M.N.A"
            "N..ctor"
            ".ctor"
            "N."
            "<Module>"
            "X\uD800"
            "X\uDBFF"
        ]

    /// `tdReservedMask`, which `_SetTypeDefProps` clears from the flags it is given.
    let private reserved : int = 0x00040800

    /// `IsTdNested`: a visibility of one of the nested kinds.
    let private isNested (flags : TypeAttributes) : bool =
        int (flags &&& TypeAttributes.VisibilityMask) >= int TypeAttributes.NestedPublic

    /// The string a `#Strings` entry holds for `value`: its UTF-8, with each lone surrogate replaced.
    let private stored (value : string) : string =
        Encoding.UTF8.GetString (Encoding.UTF8.GetBytes value)

    /// A scope with two `TypeRef` rows, for a type to derive from or implement.
    let private withTwoTypeRefs () : EmitScope =
        let request : AssemblyRefRequest =
            {
                Name = "Lib"
                Version =
                    {
                        Major = 1us
                        Minor = 0us
                        Build = 0us
                        Revision = 0us
                    }
                Culture = ""
                PublicKeyToken = ImmutableArray.Empty
                Flags = enum<AssemblyFlags> 0
            }

        let assemblyRef, scope =
            EmitScopeReferences.defineAssemblyRef request (EmitScope.create "Dyn" Guid.Empty)

        let resolution = ResolutionScope.AssemblyRef assemblyRef

        let _, scope =
            EmitScopeReferences.defineTypeRefByName resolution "System.Object" scope

        let _, scope = EmitScopeReferences.defineTypeRefByName resolution "IThing" scope
        scope

    /// A row as the model holds it: namespace, name, flags, parent and interfaces.
    type private ModelRow = string * string * TypeAttributes * TypeDefOrRef option * TypeDefOrRef list

    [<Test>]
    let ``a type is defined unless a top-level type has its namespace and name, in definition order`` () : unit =
        let genFlags : Gen<int> =
            Gen.zip (Gen.choose (0, 0xFFFF)) (Gen.choose (0, 0xFFFF))
            |> Gen.map (fun (high, low) -> (high <<< 16) ||| low)

        let genOp : Gen<int * int * int * int list> =
            gen {
                let! nameIndex = Gen.choose (0, typeNames.Length - 1)
                let! flags = genFlags
                let! extendsChoice = Gen.choose (0, 1000)

                let! interfaceChoices = Gen.listOf (Gen.choose (0, 1000)) |> Gen.map (List.ofSeq >> List.truncate 4)

                return nameIndex, flags, extendsChoice, interfaceChoices
            }

        let genOps : Gen<(int * int * int * int list) list> =
            Gen.listOf genOp |> Gen.map List.ofSeq

        let property (ops : (int * int * int * int list) list) : unit =
            let start = withTwoTypeRefs ()

            // `<Module>`, which every scope starts with.
            let mutable model : ModelRow list =
                [ "", "<Module>", TypeAttributes.NotPublic, None, [] ]

            let mutable scope = start

            for nameIndex, flags, extendsChoice, interfaceChoices in ops do
                let targets =
                    [
                        yield TypeDefOrRef.TypeRef (TypeRefRid 1)
                        yield TypeDefOrRef.TypeRef (TypeRefRid 2)
                        for rid in 1 .. model.Length -> TypeDefOrRef.TypeDef (TypeDefRid rid)
                    ]

                let extends =
                    if extendsChoice % 3 = 0 then
                        None
                    else
                        Some targets.[extendsChoice % targets.Length]

                let interfaces =
                    interfaceChoices |> List.map (fun choice -> targets.[choice % targets.Length])

                let fullName = typeNames.[nameIndex]
                let ns, name = EmitScopeReferences.splitTypeName fullName
                let flags = enum<TypeAttributes> flags

                let topLevel (ns : string) (name : string) : int option =
                    model
                    |> List.tryFindIndex (fun (rowNs, rowName, rowFlags, _, _) ->
                        not (isNested rowFlags)
                        && stored rowNs = stored ns
                        && stored rowName = stored name
                    )

                match
                    topLevel ns name, EmitScopeDefinitions.defineTopLevelTypeDef fullName flags extends interfaces scope
                with
                | Some index, TypeDefDefinition.Duplicate rid -> rid |> shouldEqual (TypeDefRid (index + 1))
                | None, TypeDefDefinition.Defined (rid, next) ->
                    model <-
                        model
                        @ [ ns, name, flags &&& enum<TypeAttributes> (~~~reserved), extends, interfaces ]

                    rid |> shouldEqual (TypeDefRid model.Length)

                    // Each interface gets a row, in order, after those already there.
                    List.ofSeq (EmitScope.interfaceImpls next)
                    |> shouldEqual (
                        List.ofSeq (EmitScope.interfaceImpls scope)
                        @ (interfaces
                           |> List.map (fun target ->
                               {
                                   Class = rid
                                   Interface = target
                               }
                           ))
                    )

                    scope <- next
                | expected, actual ->
                    failwith $"%s{fullName}: the model expected a duplicate of %A{expected}, got %A{actual}"

                EmitScopeDefinitions.tryFindTopLevelTypeDef ns name scope
                |> shouldEqual (topLevel ns name |> Option.map (fun index -> TypeDefRid (index + 1)))

            let final = scope

            let rows : ModelRow list =
                let implemented =
                    EmitScope.interfaceImpls final
                    |> Seq.groupBy (fun row -> row.Class)
                    |> Map.ofSeq

                EmitScope.typeDefs final
                |> Seq.mapi (fun index row ->
                    let interfaces =
                        match Map.tryFind (TypeDefRid (index + 1)) implemented with
                        | None -> []
                        | Some rows -> rows |> Seq.map (fun row -> row.Interface) |> List.ofSeq

                    EmitScope.stringAt row.Namespace final,
                    EmitScope.stringAt row.Name final,
                    row.Flags,
                    row.Extends,
                    interfaces
                )
                |> List.ofSeq

            rows
            |> shouldEqual (
                model
                |> List.map (fun (ns, name, flags, extends, interfaces) ->
                    stored ns, stored name, flags, extends, interfaces
                )
            )

            // Nothing but the TypeDef and InterfaceImpl tables and the string heap changes.
            EmitScope.typeRefs final |> shouldEqual (EmitScope.typeRefs start)
            EmitScope.assemblyRefs final |> shouldEqual (EmitScope.assemblyRefs start)

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 300, Prop.forAll (Arb.fromGen genOps) property)

    [<Test>]
    let ``a found type adds no string`` () : unit =
        let scope = withTwoTypeRefs ()

        match EmitScopeDefinitions.defineTopLevelTypeDef "<Module>" TypeAttributes.Public None [] scope with
        | TypeDefDefinition.Duplicate rid -> rid |> shouldEqual (TypeDefRid 1)
        | TypeDefDefinition.Defined _ -> failwith "<Module> was defined again"

        // Asked for again after a type is defined, the duplicate's scope is the one given, whose heap
        // is unchanged.
        match EmitScopeDefinitions.defineTopLevelTypeDef "N.A" TypeAttributes.Public None [] scope with
        | TypeDefDefinition.Duplicate _ -> failwith "N.A is not in the scope yet"
        | TypeDefDefinition.Defined (rid, next) ->
            match EmitScopeDefinitions.defineTopLevelTypeDef "N.A" TypeAttributes.Sealed None [] next with
            | TypeDefDefinition.Duplicate again -> again |> shouldEqual rid
            | TypeDefDefinition.Defined _ -> failwith "N.A was defined twice"

            (EmitScope.strings next).Count
            |> shouldEqual ((EmitScope.strings scope).Count + 2)

    [<Test>]
    let ``a type interns its name before its namespace`` () : unit =
        match
            EmitScopeDefinitions.defineTopLevelTypeDef
                "Space.Name"
                TypeAttributes.Public
                None
                []
                (EmitScope.create "Dyn" Guid.Empty)
        with
        | TypeDefDefinition.Duplicate _ -> failwith "Space.Name is not in a new scope"
        | TypeDefDefinition.Defined (_, scope) ->
            let strings = EmitScope.strings scope |> List.ofSeq
            strings.[strings.Length - 2 ..] |> shouldEqual [ "Name" ; "Space" ]

    let private dynamicParts : DynamicAssemblyName =
        {
            SimpleName = "Dyn"
            Version = Version (0, 0, 0, 0)
            Culture = ""
            PublicKey = ImmutableArray<byte>.Empty
            Flags = enum<AssemblyFlags> 0
            HashAlgorithm = AssemblyHashAlgorithm.Sha1
        }

    let private corelib : DumpedAssembly Lazy =
        lazy
            (let _, loggerFactory = LoggerFactory.makeTest ()
             Assembly.readFile loggerFactory HostImage.coreLibPath)

    /// A dynamic assembly whose scope defines `Thing`, the type it returns, which is not created.
    let private withThing () : LoadedAssemblies * string * TypeDefRid =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let scope = DynamicAssemblyImage.scope dynamicParts Guid.Empty
        let name, scope = EmitScope.internString "Thing" scope

        let thing, scope =
            EmitScope.addTypeDef
                {
                    Flags = TypeAttributes.Public
                    Name = name
                    Namespace = StringOffset 0
                    Extends = None
                }
                scope

        match (LoadedAssemblies.ofAssemblies [ corelib.Value ]).WithDynamicAssembly loggerFactory scope with
        | Error existing -> failwith $"unexpected collision with %s{existing.DefinitionFullName}"
        | Ok (loaded, assy) -> loaded, assy.DefinitionFullName, thing

    let private identityOf (fullName : string) (typeDef : TypeDefRid) : ResolvedTypeIdentity =
        ResolvedTypeIdentity.ofDefinitionInAssembly fullName (MetadataTokens.TypeDefinitionHandle typeDef.Rid)

    let private refusal (f : unit -> 'a) : string =
        match
            (try
                f () |> ignore
                None
             with e ->
                 Some e.Message)
        with
        | None -> failwith "expected the runtime to refuse loading a type that is not created"
        | Some message -> message

    [<Test>]
    let ``a type of a dynamic assembly is uncreated until the runtime creates it`` () : unit =
        let loaded, fullName, thing = withThing ()

        loaded.IsUncreatedDynamicType (identityOf fullName thing) |> shouldEqual true
        // `<Module>` is never created by `CreateType`.
        loaded.IsUncreatedDynamicType (identityOf fullName (TypeDefRid 1))
        |> shouldEqual true

        let created = loaded.WithCreatedType fullName thing
        created.IsUncreatedDynamicType (identityOf fullName thing) |> shouldEqual false

        created.IsUncreatedDynamicType (identityOf fullName (TypeDefRid 1))
        |> shouldEqual true

        // A type of an assembly that is not dynamic, loaded or not, is never uncreated.
        loaded.IsUncreatedDynamicType (identityOf corelib.Value.DefinitionFullName (TypeDefRid 2))
        |> shouldEqual false

        loaded.IsUncreatedDynamicType (identityOf "Elsewhere, Version=1.0.0.0" (TypeDefRid 2))
        |> shouldEqual false

    [<Test>]
    let ``the type system refuses to intern a type that is not created, as CoreCLR's loader refuses to load it``
        ()
        : unit
        =
        let loaded, fullName, thing = withThing ()
        let bct = BaseClassTypes.ofCorelib corelib.Value

        let ct =
            ConcreteType.makeFromIdentity (identityOf fullName thing) "" "Thing" ImmutableArray.Empty

        for message in
            [
                refusal (fun () -> AllConcreteTypes.addNonGeneric loaded ct AllConcreteTypes.Empty)
                refusal (fun () -> AllConcreteTypes.add bct loaded ct AllConcreteTypes.Empty)
            ] do
            message |> shouldContainText "TODO"
            message |> shouldContainText "TypeResolve"
            message |> shouldContainText fullName
            message |> shouldContainText "Thing, TypeDef 0x02000002"

        let created = loaded.WithCreatedType fullName thing

        let handle, _ = AllConcreteTypes.addNonGeneric created ct AllConcreteTypes.Empty
        handle |> shouldEqual (ConcreteTypeHandle.Concrete 0)

        match AllConcreteTypes.add bct created ct AllConcreteTypes.Empty with
        | Error refusal -> failwith (TypeLoadRefusal.describe refusal)
        | Ok (handle, _) -> handle |> shouldEqual (ConcreteTypeHandle.Concrete 0)
