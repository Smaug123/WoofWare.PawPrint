namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// `TypeLoad.loadMiss` checked against the host's CoreCLR, which loads each type of a
/// client through a `typeof` token in a collectible `AssemblyLoadContext`: the client is compiled
/// against a provider declaring every type it names, and loaded against one lacking some of them
/// and without the assembly `Optional` at all.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestTypeLoad =

    /// What loading a type does: it loads, or fails with the exception of this name.
    [<RequireQualifiedAccess>]
    type Outcome =
        | Loads
        | Fails of exceptionName : string

    /// The provider. Types whose names begin `Gone` it declares only where `complete`.
    let private providerSource (complete : bool) : string =
        let gone =
            """
public class Gone { }
public interface IGone { }
public struct GoneStruct { }
public enum GoneEnum { A }
"""

        """
namespace Provider;
public class Kept { }
public interface IKept { }
public struct KeptStruct { }
public class KeptGeneric<T> { }
public interface IKeptGeneric<T> { }
public struct KeptGenericStruct<T> { }
"""
        + (if complete then gone else "")

    /// An assembly the client is compiled against, which is not there when it is loaded.
    let private optionalSource : string =
        """
namespace Optional;
public class Thing { }
public interface IThing { }
public struct ThingStruct { }
"""

    let private compileProvider (complete : bool) : byte[] =
        Roslyn.compileOptimizedAssembly "Provider" OutputKind.DynamicallyLinkedLibrary [ providerSource complete ]

    let private completeProvider : Lazy<byte[]> = lazy (compileProvider true)

    let private incompleteProvider : Lazy<byte[]> = lazy (compileProvider false)

    let private optionalImage : Lazy<byte[]> =
        lazy (Roslyn.compileOptimizedAssembly "Optional" OutputKind.DynamicallyLinkedLibrary [ optionalSource ])

    /// A client declaring `declarations` in the namespace `Client`, with a class `Tokens` whose
    /// method `Get{i}` returns `typeof` of the `i`th of `names`, each a type as C# spells it.
    let private compileClient (declarations : string) (names : string list) : byte[] =
        let tokens =
            names
            |> List.mapi (fun i name -> $"    public static System.Type Get%d{i}() => typeof(%s{name});")
            |> String.concat "\n"

        let source =
            "namespace Client;\n"
            + declarations
            + "\npublic static unsafe class Tokens\n{\n"
            + tokens
            + "\n}\n"

        Roslyn.compileOptimizedAssemblyWithReferences
            "Client"
            OutputKind.DynamicallyLinkedLibrary
            [
                MetadataReference.CreateFromImage (ImmutableArray.CreateRange completeProvider.Value)
                MetadataReference.CreateFromImage (ImmutableArray.CreateRange optionalImage.Value)
            ]
            [ source ]

    /// What the host's CoreCLR does at a `typeof` of each of `names`.
    let private hostOutcomes (client : byte[]) (names : string list) : Outcome list =
        let context = Runtime.Loader.AssemblyLoadContext ("TypeLoad", isCollectible = true)

        context.add_Resolving (
            Func<_, _, _> (fun (context : Runtime.Loader.AssemblyLoadContext) (name : AssemblyName) ->
                if name.Name = "Provider" then
                    context.LoadFromStream (new MemoryStream (incompleteProvider.Value))
                else
                    null
            )
        )

        try
            let tokens =
                context.LoadFromStream(new MemoryStream (client)).GetType "Client.Tokens"

            names
            |> List.mapi (fun i _ ->
                try
                    tokens.GetMethod($"Get%d{i}").Invoke ((null : obj), [||]) |> ignore<obj>
                    Outcome.Loads
                with :? TargetInvocationException as e ->
                    Outcome.Fails (e.InnerException.GetType().Name)
            )
        finally
            context.Unload ()

    /// The client's type definition that C# spells `name`.
    let private definitionNamed (client : DumpedAssembly) (name : string) : TypeDefinitionHandle =
        let rec spelled (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>) : string =
            if ty.DeclaringType.IsNil then
                $"%s{ty.Namespace}.%s{ty.Name}"
            else
                $"%s{spelled client.TypeDefs.[ty.DeclaringType]}.%s{ty.Name}"

        client.TypeDefs.Values
        |> Seq.filter (fun ty -> spelled ty = $"Client.%s{name}")
        |> Seq.exactlyOne
        |> fun ty -> ty.TypeDefHandle

    /// `body` over the client read from `client`, the directories the loader looks for assemblies in,
    /// where the provider is but not loaded, and the assemblies loaded so far: CoreLib and the client.
    let private overClient<'a>
        (corelib : DumpedAssembly)
        (client : byte[])
        (body : DumpedAssembly -> string list -> LoadedAssemblies -> 'a)
        : 'a
        =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let client =
            Assembly.read loggerFactory (Some "Client.dll") (new MemoryStream (client))

        let directory = Directory.CreateTempSubdirectory "type-load"

        try
            File.WriteAllBytes (Path.Combine (directory.FullName, "Provider.dll"), incompleteProvider.Value)
            let dirs = Seq.append [ directory.FullName ] HostImage.runtimeDirs |> List.ofSeq
            body client dirs (LoadedAssemblies.ofAssemblies [ corelib ; client ])
        finally
            directory.Delete true

    /// What `TypeLoad` finds loading, in turn, each of the types `spellingsOf` the client spells:
    /// the first miss of each load. Each load is from the definitions the loads before it brought,
    /// if `fromEarlier`, or else from nothing.
    let private missesOf
        (corelib : DumpedAssembly)
        (client : byte[])
        (spellingsOf : DumpedAssembly -> TypeDefn list)
        (fromEarlier : bool)
        : TypeLoad.LoadMiss option list
        =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let baseClassTypes = BaseClassTypes.ofCorelib corelib

        overClient
            corelib
            client
            (fun client dirs assemblies ->
                ((assemblies, TypeLoad.nothingLoaded, []), spellingsOf client)
                ||> List.fold (fun (assemblies, loaded, misses) spelling ->
                    let from = if fromEarlier then loaded else TypeLoad.nothingLoaded

                    let assemblies, loaded, miss =
                        TypeLoad.loadMissFrom loggerFactory dirs baseClassTypes client spelling from assemblies

                    assemblies, loaded, miss :: misses
                )
                |> fun (_, _, misses) -> List.rev misses
            )

    /// What `TypeLoad.loadMissFrom` says CoreCLR does at a `typeof` of each of `names` in turn,
    /// each from the definitions the ones before it loaded: the load fails with the exception of
    /// the first miss it reports.
    let private pawPrintOutcomes (corelib : DumpedAssembly) (client : byte[]) (names : string list) : Outcome list =
        let spellingsOf (client : DumpedAssembly) =
            names
            |> List.map (fun name ->
                TypeDefn.FromDefinition (
                    client.TypeDefs.[definitionNamed client name].Identity,
                    SignatureTypeKind.Unknown
                )
            )

        missesOf corelib client spellingsOf true
        |> List.map (fun miss ->
            match miss with
            | None -> Outcome.Loads
            | Some (TypeLoad.LoadMiss.TypeAbsent _) -> Outcome.Fails "TypeLoadException"
            | Some (TypeLoad.LoadMiss.AssemblyUnavailable _) -> Outcome.Fails "FileNotFoundException"
            | Some (TypeLoad.LoadMiss.Unmodelled unmodelled) -> failwith unmodelled.Diagnostic
        )

    let private hostCoreLib : Lazy<DumpedAssembly> =
        lazy
            (let _, loggerFactory = LoggerFactory.makeTest ()
             Assembly.readFile loggerFactory HostImage.coreLibPath)

    /// Where the host and `TypeLoad` disagree about the types `names` of a client declaring
    /// `declarations`, and what the host did with each.
    let private compare (declarations : string) (names : string list) : string list * Outcome list =
        let client = compileClient declarations names
        let host = hostOutcomes client names
        let pawPrint = pawPrintOutcomes hostCoreLib.Value client names

        let disagreements =
            List.zip3 names host pawPrint
            |> List.choose (fun (name, host, pawPrint) ->
                if host = pawPrint then
                    None
                else
                    Some $"%s{name}: the host %A{host}, PawPrint %A{pawPrint}"
            )

        disagreements, host

    let private curatedDeclarations : string =
        """
public class DerivesKept : Provider.Kept { }
public class DerivesGone : Provider.Gone { }
public class DerivesDerivesGone : DerivesGone { }
public class DerivesKeptOfGone : Provider.KeptGeneric<Provider.Gone> { }
public class DerivesKeptOfGoneArray : Provider.KeptGeneric<Provider.Gone[]> { }
public unsafe class DerivesKeptOfGoneFunctionPointers : Provider.KeptGeneric<delegate*<Provider.Gone>[]> { }
public class DerivesMissing : Optional.Thing { }
public class ImplementsGone : Provider.IGone { }
public class ImplementsKeptOfGone : Provider.IKeptGeneric<Provider.Gone> { }
public class ImplementsMissing : Optional.IThing { }
public interface ExtendsGone : Provider.IGone { }
public class ImplementsExtendsGone : ExtendsGone { }
public class HoldsGoneStruct { public Provider.GoneStruct F; }
public struct StructHoldsGoneStruct { public Provider.GoneStruct F; }
public class HoldsStaticGoneStruct { public static Provider.GoneStruct F; }
public class HoldsMissingStruct { public Optional.ThingStruct F; }
public class HoldsGoneClass { public Provider.Gone F; }
public class HoldsStaticGoneClass { public static Provider.Gone F; }
public class HoldsMissingClass { public Optional.Thing F; }
public class HoldsGoneEnum { public Provider.GoneEnum F; }
public class HoldsGoneEnumConstant { public const Provider.GoneEnum F = 0; }
public class HoldsKeptStructOfGone { public Provider.KeptGenericStruct<Provider.Gone> F; }
public class HoldsKeptClassOfGone { public Provider.KeptGeneric<Provider.Gone> F; }
public unsafe class HoldsGonePointer { public Provider.GoneStruct* F; }
public class HoldsGoneStructArray { public Provider.GoneStruct[] F; }
public unsafe class HoldsGoneFunctionPointer { public delegate*<Provider.GoneStruct> F; }
public struct HoldsStructHoldingGone { public StructHoldsGoneStruct F; }
public class MethodTakesGone { public void M(Provider.GoneStruct g) { } }
public class VirtualTakesGone { public virtual void M(Provider.GoneStruct g) { } }
public class OuterOfGone : Provider.Gone { public class Nested { } }
public struct SelfStatic { public static SelfStatic F; }
public class StaticMissingBeforeDeferredArgument : Provider.KeptGeneric<DerivesGone> { public static Optional.ThingStruct F; }
public class StaticGoneAfterDeferredArgument : Provider.KeptGeneric<DerivesMissing> { public static Provider.GoneStruct F; }
public class StaticMissingBeforeInstanceGone { public static Optional.ThingStruct S; public Provider.GoneStruct I; }
public class InstanceGoneBeforeStaticMissing { public Provider.GoneStruct I; public static Optional.ThingStruct S; }
"""

    let private curatedNames : string list =
        [
            "DerivesKept"
            "DerivesGone"
            "DerivesDerivesGone"
            "DerivesKeptOfGone"
            "DerivesKeptOfGoneArray"
            "DerivesKeptOfGoneFunctionPointers"
            "DerivesMissing"
            "ImplementsGone"
            "ImplementsKeptOfGone"
            "ImplementsMissing"
            "ExtendsGone"
            "ImplementsExtendsGone"
            "HoldsGoneStruct"
            "StructHoldsGoneStruct"
            "HoldsStaticGoneStruct"
            "HoldsMissingStruct"
            "HoldsGoneClass"
            "HoldsStaticGoneClass"
            "HoldsMissingClass"
            "HoldsGoneEnum"
            "HoldsGoneEnumConstant"
            "HoldsKeptStructOfGone"
            "HoldsKeptClassOfGone"
            "HoldsGonePointer"
            "HoldsGoneStructArray"
            "HoldsGoneFunctionPointer"
            "HoldsStructHoldingGone"
            "MethodTakesGone"
            "VirtualTakesGone"
            "OuterOfGone.Nested"
            "SelfStatic"
            "StaticMissingBeforeDeferredArgument"
            "StaticGoneAfterDeferredArgument"
            "StaticMissingBeforeInstanceGone"
            "InstanceGoneBeforeStaticMissing"
        ]

    [<Test>]
    let ``each of a curated set of types fails to load exactly where the host's does, with its exception`` () : unit =
        let disagreements, host = compare curatedDeclarations curatedNames

        if not disagreements.IsEmpty then
            failwith (String.concat Environment.NewLine disagreements)

        // Enough of each outcome for the comparison to say something.
        let count (outcome : Outcome) =
            host |> List.filter ((=) outcome) |> List.length

        count Outcome.Loads |> shouldBeGreaterThan 10
        count (Outcome.Fails "TypeLoadException") |> shouldBeGreaterThan 10
        count (Outcome.Fails "FileNotFoundException") |> shouldBeGreaterThan 2

    /// A type as a generated client spells it.
    type private Spelled =
        | Kept
        | Gone
        | KeptStruct
        | GoneStruct
        | GoneEnum
        | IKept
        | IGone
        | Missing
        | MissingStruct
        | IMissing
        /// The client type declared at this index, which comes before the one spelling it.
        | Earlier of index : int
        | Array of Spelled
        | KeptGeneric of Spelled
        | KeptGenericStruct of Spelled
        | IKeptGeneric of Spelled
        | Pointer of Spelled
        | FunctionPointer of Spelled

    [<RequireQualifiedAccess>]
    type private Kind =
        | Class
        | Struct
        | Interface

    /// A generated client type: its base class, the interfaces it implements, and its fields, each
    /// static or not.
    type private Declaration =
        {
            Kind : Kind
            Base : Spelled option
            Interfaces : Spelled list
            Fields : (bool * Spelled) list
        }

    let rec private render (spelled : Spelled) : string =
        match spelled with
        | Kept -> "Provider.Kept"
        | Gone -> "Provider.Gone"
        | KeptStruct -> "Provider.KeptStruct"
        | GoneStruct -> "Provider.GoneStruct"
        | GoneEnum -> "Provider.GoneEnum"
        | IKept -> "Provider.IKept"
        | IGone -> "Provider.IGone"
        | Missing -> "Optional.Thing"
        | MissingStruct -> "Optional.ThingStruct"
        | IMissing -> "Optional.IThing"
        | Earlier index -> $"T%d{index}"
        | Array element -> $"%s{render element}[]"
        | KeptGeneric argument -> $"Provider.KeptGeneric<%s{render argument}>"
        | KeptGenericStruct argument -> $"Provider.KeptGenericStruct<%s{render argument}>"
        | IKeptGeneric argument -> $"Provider.IKeptGeneric<%s{render argument}>"
        | Pointer target -> $"%s{render target}*"
        | FunctionPointer returned -> $"delegate*<%s{render returned}>"

    let private renderDeclaration (index : int) (declaration : Declaration) : string =
        let keyword =
            match declaration.Kind with
            | Kind.Class -> "class"
            | Kind.Struct -> "struct"
            | Kind.Interface -> "interface"

        let supertypes =
            Option.toList declaration.Base @ declaration.Interfaces
            |> List.map render
            |> function
                | [] -> ""
                | supertypes -> " : " + String.concat ", " supertypes

        let fields =
            declaration.Fields
            |> List.mapi (fun i (isStatic, spelled) ->
                let modifier = if isStatic then "static " else ""
                $" public %s{modifier}%s{render spelled} F%d{i};"
            )
            |> String.concat ""

        $"public unsafe %s{keyword} T%d{index}%s{supertypes} {{%s{fields} }}"

    /// One of `kept`, `gone` or `missing`, mostly `kept`, so that most generated types still load.
    let private weighted (kept : Spelled list) (gone : Spelled list) (missing : Spelled list) : Gen<Spelled> =
        Gen.frequency [ 6, Gen.elements kept ; 1, Gen.elements gone ; 1, Gen.elements missing ]

    /// A type to spell as a generic argument, an array's element or a field's type, which may name
    /// any of `earlier`, the kinds of the client types declared so far.
    let rec private genArgument (earlier : Kind list) (depth : int) : Gen<Spelled> =
        let leaves =
            weighted
                ([ Kept ; KeptStruct ; IKept ] @ List.init earlier.Length Earlier)
                [ Gone ; GoneStruct ; GoneEnum ; IGone ]
                [ Missing ; MissingStruct ; IMissing ]

        if depth = 0 then
            leaves
        else
            let inner = genArgument earlier (depth - 1)

            Gen.frequency
                [
                    3, leaves
                    1, Gen.map Array inner
                    1, Gen.map KeptGeneric inner
                    1, Gen.map KeptGenericStruct inner
                    1, Gen.map IKeptGeneric inner
                ]

    let private genField (earlier : Kind list) : Gen<Spelled> =
        Gen.frequency
            [
                6, genArgument earlier 2
                // A pointer's target must be unmanaged, which provider structs and enums are.
                1,
                Gen.elements [ KeptStruct ; GoneStruct ; GoneEnum ; MissingStruct ]
                |> Gen.map Pointer
                1, genArgument earlier 1 |> Gen.map FunctionPointer
            ]

    let private genDeclaration (earlier : Kind list) : Gen<Declaration> =
        let ofKind (kind : Kind) =
            earlier
            |> List.indexed
            |> List.filter (fun (_, k) -> k = kind)
            |> List.map (fst >> Earlier)

        let genInterface =
            Gen.oneof
                [
                    weighted (IKept :: ofKind Kind.Interface) [ IGone ] [ IMissing ]
                    genArgument earlier 1 |> Gen.map IKeptGeneric
                ]

        let genInterfaces =
            Gen.listOfLength 2 genInterface
            |> Gen.zip (Gen.choose (0, 2))
            |> Gen.map (fun (count, interfaces) ->
                // C# refuses an interface listed twice.
                List.truncate count interfaces |> List.distinctBy render
            )

        let genFields (staticOnly : bool) =
            Gen.zip
                (Gen.choose (0, 3))
                (Gen.listOfLength 3 (Gen.zip (Gen.elements [ true ; false ]) (genField earlier)))
            |> Gen.map (fun (count, fields) ->
                List.truncate count fields
                |> List.map (fun (isStatic, spelled) -> (staticOnly || isStatic), spelled)
            )

        gen {
            let! kind = Gen.elements [ Kind.Class ; Kind.Struct ; Kind.Interface ]

            let! baseClass =
                match kind with
                | Kind.Class ->
                    Gen.oneof
                        [
                            Gen.constant None
                            weighted (Kept :: ofKind Kind.Class) [ Gone ] [ Missing ] |> Gen.map Some
                            genArgument earlier 1 |> Gen.map (KeptGeneric >> Some)
                        ]
                | Kind.Struct
                | Kind.Interface -> Gen.constant None

            let! interfaces = genInterfaces
            let! fields = genFields (kind = Kind.Interface)

            return
                {
                    Kind = kind
                    Base = baseClass
                    Interfaces = interfaces
                    Fields = fields
                }
        }

    /// One to five client types, each naming only those before it.
    let private genClient : Gen<Declaration list> =
        let rec go (count : int) (soFar : Declaration list) : Gen<Declaration list> =
            if count = 0 then
                Gen.constant (List.rev soFar)
            else
                gen {
                    let! next = genDeclaration (soFar |> List.rev |> List.map (fun d -> d.Kind))
                    return! go (count - 1) (next :: soFar)
                }

        Gen.choose (1, 5) |> Gen.bind (fun count -> go count [])

    [<Test>]
    let ``a generated type fails to load exactly where the host's does, with its exception`` () : unit =
        let property (cover : Outcome -> unit) (declarations : Declaration list) : unit =
            let names = declarations |> List.mapi (fun i _ -> $"T%d{i}")

            let source =
                declarations |> List.mapi renderDeclaration |> String.concat Environment.NewLine

            let disagreements, host = compare source names
            List.iter cover host

            if not disagreements.IsEmpty then
                failwith (String.concat Environment.NewLine (source :: disagreements))

        let coverage =
            CoverageSample.check (Config.QuickThrowOnFailure.WithMaxTest 100) (Arb.fromGen genClient) property

        // The fixed sample reaches every outcome often enough for the comparison to say something.
        coverage.Count Outcome.Loads |> shouldBeGreaterThan 100
        coverage.Count (Outcome.Fails "TypeLoadException") |> shouldBeGreaterThan 40
        coverage.Count (Outcome.Fails "FileNotFoundException") |> shouldBeGreaterThan 30

    /// A type to load: one of the client's own, or one spelled over them and the provider's.
    let private genLoad (kinds : Kind list) : Gen<Spelled> =
        Gen.frequency
            [
                3, Gen.elements (List.init kinds.Length Earlier)
                3, genArgument kinds 2
                1,
                Gen.elements [ KeptStruct ; GoneStruct ; GoneEnum ; MissingStruct ]
                |> Gen.map Pointer
                1, genArgument kinds 1 |> Gen.map FunctionPointer
            ]

    /// The type the `typeof` in the client's method `Tokens.Get{index}` spells.
    let private spelledBy (client : DumpedAssembly) (index : int) : TypeDefn =
        let method =
            client.Methods.Values
            |> Seq.filter (fun m -> m.Name = $"Get%d{index}")
            |> Seq.exactlyOne

        let token =
            match method.Body with
            | MethodBody.Il body ->
                body.Instructions
                |> List.pick (fun (op, _) ->
                    match op with
                    | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldtoken, MetadataOperand.FromMetadata token) ->
                        Some token.Token
                    | _ -> None
                )
            | body -> failwith $"Tokens.Get%d{index} has no IL body: %A{body}"

        match token with
        | MetadataToken.TypeDefinition handle ->
            TypeDefn.FromDefinition (client.TypeDefs.[handle].Identity, SignatureTypeKind.Unknown)
        | MetadataToken.TypeReference handle ->
            TypeDefn.FromReference (client.TypeRefs.[handle], SignatureTypeKind.Unknown)
        | MetadataToken.TypeSpecification handle -> client.TypeSpecs.[handle].Signature
        | other -> failwith $"Tokens.Get%d{index} loads the token of %O{other}, which is not a type"

    /// Loading types in turn, each from the definitions the loads before it brought, finds what
    /// loading each from nothing finds, whatever the order: a definition a load brings without
    /// meeting a miss leads no later load to one.
    [<Test>]
    let ``a load from the definitions earlier loads brought finds what a load from nothing finds`` () : unit =
        let genCase =
            gen {
                let! declarations = genClient
                let kinds = declarations |> List.map (fun d -> d.Kind)
                let! count = Gen.choose (1, 8)
                let! loads = Gen.listOfLength count (genLoad kinds)
                return declarations, loads
            }

        let property (cover : Outcome -> unit) (declarations : Declaration list, loads : Spelled list) : unit =
            let source =
                declarations |> List.mapi renderDeclaration |> String.concat Environment.NewLine

            let names = List.map render loads
            let client = compileClient source names

            let spellingsOf (client : DumpedAssembly) =
                List.init names.Length (spelledBy client)
            // Each run reads the client afresh, and an assembly reference compares its name by
            // reference, so a miss is compared by what it names.
            let named (miss : TypeLoad.LoadMiss option) : Choice<TypeResolutionMiss, string> option =
                match miss with
                | None -> None
                | Some (TypeLoad.LoadMiss.TypeAbsent miss) -> Some (Choice1Of2 miss)
                | Some (TypeLoad.LoadMiss.AssemblyUnavailable reference) -> Some (Choice2Of2 reference.Name.FullName)
                | Some (TypeLoad.LoadMiss.Unmodelled unmodelled) -> failwith unmodelled.Diagnostic

            let fresh = missesOf hostCoreLib.Value client spellingsOf false |> List.map named

            let fromEarlier =
                missesOf hostCoreLib.Value client spellingsOf true |> List.map named

            for miss in fresh do
                match miss with
                | None -> cover Outcome.Loads
                | Some (Choice1Of2 _) -> cover (Outcome.Fails "TypeLoadException")
                | Some (Choice2Of2 _) -> cover (Outcome.Fails "FileNotFoundException")

            if fresh <> fromEarlier then
                failwith (
                    String.concat
                        Environment.NewLine
                        [
                            source
                            yield!
                                List.zip3 names fresh fromEarlier
                                |> List.map (fun (name, fresh, fromEarlier) ->
                                    $"%s{name}: from nothing %A{fresh}, from earlier loads %A{fromEarlier}"
                                )
                        ]
                )

        let coverage =
            CoverageSample.check (Config.QuickThrowOnFailure.WithMaxTest 100) (Arb.fromGen genCase) property

        // A load after another that met a miss is what tells a sound record from one that keeps
        // what a stopped walk marked; the fixed sample has plenty of each outcome.
        coverage.Count Outcome.Loads |> shouldBeGreaterThan 150
        coverage.Count (Outcome.Fails "TypeLoadException") |> shouldBeGreaterThan 60
        coverage.Count (Outcome.Fails "FileNotFoundException") |> shouldBeGreaterThan 40

    /// A value type to load approximately: one of the client's structs, or one spelled over the
    /// client's types and the provider's.
    let private genApproximate (kinds : Kind list) : Gen<Spelled> =
        let structs =
            kinds
            |> List.indexed
            |> List.filter (fun (_, kind) -> kind = Kind.Struct)
            |> List.map (fst >> Earlier)

        Gen.frequency
            [
                if not structs.IsEmpty then
                    3, Gen.elements structs
                3, genArgument kinds 1 |> Gen.map KeptGenericStruct
                1, Gen.elements [ KeptStruct ; GoneStruct ; GoneEnum ; MissingStruct ]
            ]

    /// A client declaring `declarations` in the namespace `Client`, and for the `i`th of `names`,
    /// each a value type as C# spells it, a delegate `D{i}` taking one, a static method `M{i}` taking
    /// one, and a method `Shuffles.Make{i}` making a `D{i}` over `M{i}`. The delegate is open over
    /// its target, so CoreCLR makes a thunk to shuffle the arguments while it compiles `Make{i}`,
    /// loading the type approximately to lay them out (`COMDelegate::GetDelegateCtor`).
    let private compileShuffles (declarations : string) (names : string list) : byte[] =
        let delegates =
            names
            |> List.mapi (fun i name -> $"public delegate void D%d{i}(%s{name} value);")
            |> String.concat "\n"

        let methods =
            names
            |> List.mapi (fun i name ->
                $"    public static void M%d{i}(%s{name} value) {{ }}\n    public static void Make%d{i}() => Keep(new D%d{i}(M%d{i}));"
            )
            |> String.concat "\n"

        let source =
            "namespace Client;\n"
            + declarations
            + "\n"
            + delegates
            + "\npublic static unsafe class Shuffles\n{\n"
            + "    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]\n"
            + "    public static void Keep(object o) { }\n"
            + methods
            + "\n}\n"

        Roslyn.compileOptimizedAssemblyWithReferences
            "Client"
            OutputKind.DynamicallyLinkedLibrary
            [
                MetadataReference.CreateFromImage (ImmutableArray.CreateRange completeProvider.Value)
                MetadataReference.CreateFromImage (ImmutableArray.CreateRange optionalImage.Value)
            ]
            [ source ]

    /// What the host's CoreCLR does running each of `count` methods `Shuffles.Make{i}` in turn.
    let private hostShuffleOutcomes (client : byte[]) (count : int) : Outcome list =
        let context =
            Runtime.Loader.AssemblyLoadContext ("ApproximateTypeLoad", isCollectible = true)

        context.add_Resolving (
            Func<_, _, _> (fun (context : Runtime.Loader.AssemblyLoadContext) (name : AssemblyName) ->
                if name.Name = "Provider" then
                    context.LoadFromStream (new MemoryStream (incompleteProvider.Value))
                else
                    null
            )
        )

        try
            let shuffles =
                context.LoadFromStream(new MemoryStream (client)).GetType "Client.Shuffles"

            List.init
                count
                (fun i ->
                    try
                        shuffles.GetMethod($"Make%d{i}").Invoke ((null : obj), [||]) |> ignore<obj>
                        Outcome.Loads
                    with :? TargetInvocationException as e ->
                        Outcome.Fails (e.InnerException.GetType().Name)
                )
        finally
            context.Unload ()

    /// `TypeLoad.approximateLoadMissFrom` checked against the host's CoreCLR, which loads a value type
    /// approximately when it makes a delegate open over a method taking one: the first miss of the
    /// approximate load is the exception making the delegate fails with. A full load of the same
    /// type meets a miss wherever the approximate load does, since it starts with that load.
    [<Test>]
    let ``a generated value type fails to load approximately exactly where the host's does, with its exception``
        ()
        : unit
        =
        let genCase =
            gen {
                let! declarations = genClient
                let kinds = declarations |> List.map (fun d -> d.Kind)
                let! count = Gen.choose (1, 6)
                let! loads = Gen.listOfLength count (genApproximate kinds)
                return declarations, loads
            }

        // Each load is labelled by what the host does, and by whether the full load meets a miss.
        let property (cover : Outcome * bool -> unit) (declarations : Declaration list, loads : Spelled list) : unit =
            let source =
                declarations |> List.mapi renderDeclaration |> String.concat Environment.NewLine

            let names = List.map render loads
            let client = compileShuffles source names
            let host = hostShuffleOutcomes client names.Length

            let _, loggerFactory = LoggerFactory.makeTest ()
            let baseClassTypes = BaseClassTypes.ofCorelib hostCoreLib.Value

            let misses =
                overClient
                    hostCoreLib.Value
                    client
                    (fun client dirs assemblies ->
                        let parameterOf (index : int) : TypeDefn =
                            client.Methods.Values
                            |> Seq.filter (fun m -> m.Name = $"M%d{index}")
                            |> Seq.exactlyOne
                            |> fun m -> List.exactlyOne m.Signature.ParameterTypes

                        ((assemblies, []), List.init names.Length parameterOf)
                        ||> List.fold (fun (assemblies, misses) spelling ->
                            let assemblies, approximate =
                                TypeLoad.approximateLoadMissFrom
                                    loggerFactory
                                    dirs
                                    client
                                    spelling
                                    TypeLoad.nothingLoaded
                                    assemblies

                            let assemblies, full =
                                TypeLoad.loadMiss loggerFactory dirs baseClassTypes client spelling assemblies

                            assemblies, (approximate, full) :: misses
                        )
                        |> fun (_, misses) -> List.rev misses
                    )

            let outcomeOf (miss : TypeLoad.LoadMiss option) : Outcome =
                match miss with
                | None -> Outcome.Loads
                | Some (TypeLoad.LoadMiss.TypeAbsent _) -> Outcome.Fails "TypeLoadException"
                | Some (TypeLoad.LoadMiss.AssemblyUnavailable _) -> Outcome.Fails "FileNotFoundException"
                | Some (TypeLoad.LoadMiss.Unmodelled unmodelled) -> failwith unmodelled.Diagnostic

            for host, (_, full) in List.zip host misses do
                cover (host, full.IsSome)

            let disagreements =
                List.zip3 names host misses
                |> List.collect (fun (name, host, (approximate, full)) ->
                    [
                        if host <> outcomeOf approximate then
                            yield $"%s{name}: the host %A{host}, PawPrint %A{outcomeOf approximate}"
                        if approximate.IsSome && full.IsNone then
                            yield $"%s{name}: the approximate load meets %A{approximate}, the full load nothing"
                    ]
                )

            if not disagreements.IsEmpty then
                failwith (String.concat Environment.NewLine (source :: disagreements))

        let coverage =
            CoverageSample.check (Config.QuickThrowOnFailure.WithMaxTest 100) (Arb.fromGen genCase) property

        // The fixed sample reaches every outcome often enough for the comparison to say something,
        // and has plenty of types that load approximately but not in full, which tell an
        // approximate load from a full one.
        coverage.Count (Outcome.Loads, false) |> shouldBeGreaterThan 100
        coverage.Count (Outcome.Loads, true) |> shouldBeGreaterThan 30

        coverage.Count (Outcome.Fails "TypeLoadException", true)
        |> shouldBeGreaterThan 20

        coverage.Count (Outcome.Fails "FileNotFoundException", true)
        |> shouldBeGreaterThan 15
