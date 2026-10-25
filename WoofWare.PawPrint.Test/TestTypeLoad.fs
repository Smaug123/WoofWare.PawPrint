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

    /// The provider. Types whose names begin `Gone` it declares only where `complete`, and only
    /// there does `LosesIGone` implement `IGone`.
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
        + (if complete then
               "public class LosesIGone : IGone { }\n"
           else
               "public class LosesIGone { }\n")
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

    /// The client's type definition that C# spells `name`, which may name a generic definition
    /// without arguments, as `G<,>`.
    let private definitionNamed (client : DumpedAssembly) (name : string) : TypeDefinitionHandle =
        let rec spelled (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>) : string =
            if ty.DeclaringType.IsNil then
                $"%s{ty.Namespace}.%s{ty.Name}"
            else
                $"%s{spelled client.TypeDefs.[ty.DeclaringType]}.%s{ty.Name}"

        // `G<,>` is the definition whose metadata name is ``G`2``.
        let metadataName =
            match name.IndexOf '<' with
            | -1 -> name
            | open' -> $"%s{name.Substring (0, open')}`%d{name.Length - open' - 1}"

        client.TypeDefs.Values
        |> Seq.filter (fun ty -> spelled ty = $"Client.%s{metadataName}")
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

    /// Generic definitions whose type parameters' constraints name types the provider lacks, or an
    /// assembly that is not there, loaded through `typeof` of the definition itself. An interface
    /// instantiation that is not a parameter's first constraint is one CoreCLR never walks.
    let private curatedConstrainedDeclarations : string =
        """
public class ConstrainedToGone<T> where T : Provider.Gone { }
public class ConstrainedToIGone<T> where T : Provider.IGone { }
public class ConstrainedToInterfaceOfGone<T> where T : Provider.IKeptGeneric<Provider.Gone> { }
public class ConstrainedToInterfaceOfGoneStruct<T> where T : Provider.IKeptGeneric<Provider.GoneStruct> { }
public class ConstrainedToInterfaceOfGoneArray<T> where T : Provider.IKeptGeneric<Provider.Gone[]> { }
public class ConstrainedToClassOfGone<T> where T : Provider.KeptGeneric<Provider.Gone> { }
public class ConstrainedToMissing<T> where T : Optional.Thing { }
public class ConstrainedToInterfaceOfMissing<T> where T : Provider.IKeptGeneric<Optional.Thing> { }
public class ConstrainedToStructAndIGone<T> where T : struct, Provider.IGone { }
public class ConstrainedToKept<T> where T : Provider.Kept, Provider.IKept, new() { }
public class ConstrainedToItself<T> where T : Provider.IKeptGeneric<T> { }
public class ConstrainedToEachOther<T, U> where T : U where U : Provider.IKept { }
public struct StructConstrainedToGone<T> where T : Provider.Gone { }
public interface InterfaceConstrainedToGone<T> where T : Provider.Gone { }
public class FirstParameterFirst<T, U> where T : Provider.Gone where U : Optional.Thing { }
public class SecondParameterSecond<T, U> where T : Optional.Thing where U : Provider.Gone { }
public class ConstraintsInOrder<T> where T : Provider.IGone, Optional.IThing { }
public class ConstraintsInOtherOrder<T> where T : Optional.IThing, Provider.IGone { }
public class ClassBeforeInterfaceOfMissing<T> where T : Provider.KeptGeneric<Provider.Gone>, Provider.IKeptGeneric<Optional.Thing> { }
public class InterfaceOfMissingAfterClass<T> where T : Provider.KeptGeneric<Optional.Thing>, Provider.IKeptGeneric<Provider.Gone> { }
public class InterfaceSignatureAfterVariable<T, U> where T : Provider.IKeptGeneric<Optional.Thing>, U where U : Provider.Gone { }
public class ParentBeforeConstraint<T> : Provider.Gone where T : Optional.Thing { }
public class StaticFieldBeforeConstraint<T> where T : Optional.Thing { public static Provider.GoneStruct F; }
public class ConstrainedBase<T> where T : Optional.IThing { }
public class DerivesConstrainedBase<X> : ConstrainedBase<X> where X : Optional.IThing { public static Provider.GoneStruct F; }
public class StaticGoneHolder { public static Provider.GoneStruct F; }
public interface IStaticGoneHolder { public static Provider.GoneStruct F; }
public class ConstrainedToStaticGoneHolder<T> where T : StaticGoneHolder { }
public class ConstrainedToIStaticGoneHolder<T> where T : IStaticGoneHolder { }
public class InterfaceOfGoneSecond<T> where T : Provider.IKept, Provider.IKeptGeneric<Provider.Gone> { }
public class InterfaceOfGoneAfterClass<T> where T : Provider.Kept, Provider.IKeptGeneric<Provider.Gone> { }
public class InterfaceOfGoneWithStruct<T> where T : struct, Provider.IKeptGeneric<Provider.Gone> { }
public class InterfaceOfGoneAfterVariable<T, U> where T : U, Provider.IKeptGeneric<Provider.Gone> { }
public class ConstrainedToIGoneBase<T> where T : Provider.IGone { }
public class DerivesConstrainedToIGoneBase<X> : ConstrainedToIGoneBase<X> where X : Optional.IThing, Provider.IGone { }
public interface InterfaceExtendingGone<U> : Provider.IGone { }
public class SecondInterfaceOverGoneBase<T> where T : Provider.IKept, InterfaceExtendingGone<Provider.Kept> { }
public unsafe class ConstrainedToInterfaceOfModifiedGone<T> where T : Provider.IKeptGeneric<delegate* unmanaged[Cdecl]<Provider.GoneStruct>[]> { }
public unsafe class ConstrainedToInterfaceOfModifiedKept<T> where T : Provider.IKeptGeneric<delegate* unmanaged[Cdecl]<Provider.KeptStruct>[]> { }
public unsafe class ConstrainedToInterfaceOfInParameterGone<T> where T : Provider.IKeptGeneric<delegate*<in Provider.GoneStruct, void>[]> { }
public unsafe class ConstrainedToInterfaceOfInParameterKept<T> where T : Provider.IKeptGeneric<delegate*<in Provider.KeptStruct, void>[]> { }
"""

    let private curatedConstrainedNames : string list =
        [
            "ConstrainedToGone<>"
            "ConstrainedToIGone<>"
            "ConstrainedToInterfaceOfGone<>"
            "ConstrainedToInterfaceOfGoneStruct<>"
            "ConstrainedToInterfaceOfGoneArray<>"
            "ConstrainedToClassOfGone<>"
            "ConstrainedToMissing<>"
            "ConstrainedToInterfaceOfMissing<>"
            "ConstrainedToStructAndIGone<>"
            "ConstrainedToKept<>"
            "ConstrainedToItself<>"
            "ConstrainedToEachOther<,>"
            "StructConstrainedToGone<>"
            "InterfaceConstrainedToGone<>"
            "FirstParameterFirst<,>"
            "SecondParameterSecond<,>"
            "ConstraintsInOrder<>"
            "ConstraintsInOtherOrder<>"
            "ClassBeforeInterfaceOfMissing<>"
            "InterfaceOfMissingAfterClass<>"
            "InterfaceSignatureAfterVariable<,>"
            "ParentBeforeConstraint<>"
            "StaticFieldBeforeConstraint<>"
            "DerivesConstrainedBase<>"
            "ConstrainedToStaticGoneHolder<>"
            "ConstrainedToIStaticGoneHolder<>"
            "InterfaceOfGoneSecond<>"
            "InterfaceOfGoneAfterClass<>"
            "InterfaceOfGoneWithStruct<>"
            "InterfaceOfGoneAfterVariable<,>"
            "DerivesConstrainedToIGoneBase<>"
            "SecondInterfaceOverGoneBase<>"
            "ConstrainedToInterfaceOfModifiedGone<>"
            "ConstrainedToInterfaceOfModifiedKept<>"
            "ConstrainedToInterfaceOfInParameterGone<>"
            "ConstrainedToInterfaceOfInParameterKept<>"
        ]

    [<Test>]
    let ``each of a curated set of constrained generic definitions fails to load exactly where the host's does``
        ()
        : unit
        =
        let disagreements, host =
            compare curatedConstrainedDeclarations curatedConstrainedNames

        if not disagreements.IsEmpty then
            failwith (String.concat Environment.NewLine disagreements)

        let count (outcome : Outcome) =
            host |> List.filter ((=) outcome) |> List.length

        count Outcome.Loads |> shouldBeGreaterThan 4
        count (Outcome.Fails "TypeLoadException") |> shouldBeGreaterThan 10
        count (Outcome.Fails "FileNotFoundException") |> shouldBeGreaterThan 4

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
        /// The type parameter at this index of the generic type spelling it, in a constraint.
        | Parameter of index : int

    [<RequireQualifiedAccess>]
    type private Kind =
        | Class
        | Struct
        | Interface

    /// Which of `struct` and `class` a generated type parameter's constraints begin with.
    [<RequireQualifiedAccess>]
    type private SpecialConstraint =
        | Struct
        | Class

    /// A generated type parameter's constraints, each kind in the place C# requires it.
    type private ParameterConstraints =
        {
            Special : SpecialConstraint option
            ClassConstraint : Spelled option
            /// Interfaces and earlier type parameters, in the order spelled.
            Others : Spelled list
            New : bool
        }

    /// A generated client type: its base class, the interfaces it implements, its fields, each
    /// static or not, and the constraints of each of its type parameters, if it is generic.
    type private Declaration =
        {
            Kind : Kind
            Base : Spelled option
            Interfaces : Spelled list
            Fields : (bool * Spelled) list
            Generics : ParameterConstraints list
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
        | Parameter index -> $"P%d{index}"

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

        let parameters, constraints =
            match declaration.Generics with
            | [] -> "", ""
            | generics ->
                let parameters = generics |> List.mapi (fun i _ -> $"P%d{i}") |> String.concat ", "

                let constraints =
                    generics
                    |> List.mapi (fun i constraints ->
                        let special =
                            match constraints.Special with
                            | None -> []
                            | Some SpecialConstraint.Struct -> [ "struct" ]
                            | Some SpecialConstraint.Class -> [ "class" ]

                        match
                            special
                            @ (Option.toList constraints.ClassConstraint |> List.map render)
                            @ (constraints.Others |> List.map render)
                            @ (if constraints.New then [ "new()" ] else [])
                        with
                        | [] -> ""
                        | parts ->
                            let parts = String.concat ", " parts
                            $" where P%d{i} : %s{parts}"
                    )
                    |> String.concat ""

                $"<%s{parameters}>", constraints

        $"public unsafe %s{keyword} T%d{index}%s{parameters}%s{supertypes}%s{constraints} {{%s{fields} }}"

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
                    Generics = []
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

    /// The constraints of the type parameter at `index` of `count`, after those of the parameters
    /// before it, `previous`, naming any of `earlier`, the kinds of the client's non-generic types.
    /// It keeps to what C# accepts: `struct` with no class constraint and no `new()`, and an earlier
    /// type parameter as a constraint only on a parameter with neither `struct`, `class` nor a class
    /// constraint, and only where that parameter has no `struct`.
    let private genParameterConstraints
        (earlier : Kind list)
        (count : int)
        (index : int)
        (previous : ParameterConstraints list)
        : Gen<ParameterConstraints>
        =
        let ofKind (kind : Kind) =
            earlier
            |> List.indexed
            |> List.filter (fun (_, k) -> k = kind)
            |> List.map (fst >> Earlier)

        let argument =
            Gen.frequency [ 3, genArgument earlier 1 ; 1, Gen.elements (List.init count Parameter) ]

        let genClass =
            Gen.oneof
                [
                    weighted (Kept :: ofKind Kind.Class) [ Gone ] [ Missing ]
                    Gen.map KeptGeneric argument
                ]

        let genInterface =
            Gen.oneof
                [
                    weighted (IKept :: ofKind Kind.Interface) [ IGone ] [ IMissing ]
                    Gen.map IKeptGeneric argument
                ]

        let genInterfaces =
            Gen.zip (Gen.choose (0, 2)) (Gen.listOfLength 2 genInterface)
            |> Gen.map (fun (count, interfaces) -> List.truncate count interfaces |> List.distinctBy render)

        let variables =
            previous
            |> List.indexed
            |> List.filter (fun (_, constraints) -> constraints.Special <> Some SpecialConstraint.Struct)
            |> List.map (fst >> Parameter)

        let unconstrained =
            {
                Special = None
                ClassConstraint = None
                Others = []
                New = false
            }

        let nominal =
            gen {
                let! special =
                    Gen.frequency
                        [
                            3, Gen.constant None
                            1, Gen.constant (Some SpecialConstraint.Struct)
                            1, Gen.constant (Some SpecialConstraint.Class)
                        ]

                let! classConstraint =
                    match special with
                    | None -> Gen.oneof [ Gen.constant None ; Gen.map Some genClass ]
                    | Some _ -> Gen.constant None

                let! interfaces = genInterfaces

                let! isNew =
                    match special with
                    | Some SpecialConstraint.Struct -> Gen.constant false
                    | _ -> Gen.frequency [ 3, Gen.constant false ; 1, Gen.constant true ]

                return
                    {
                        Special = special
                        ClassConstraint = classConstraint
                        Others = interfaces
                        New = isNew
                    }
            }

        let onVariable =
            gen {
                let! variable = Gen.elements variables
                let! interfaces = genInterfaces

                return
                    { unconstrained with
                        Others = variable :: interfaces
                    }
            }

        Gen.frequency
            [
                1, Gen.constant unconstrained
                4, nominal
                if not variables.IsEmpty then
                    2, onVariable
            ]

    /// A generic client type with one or two type parameters, naming any of `earlier`, the kinds of
    /// the client's non-generic types, and nothing generic.
    let private genGenericDeclaration (earlier : Kind list) : Gen<Declaration> =
        gen {
            let! declaration = genDeclaration earlier
            let! count = Gen.choose (1, 2)

            let! generics =
                (Gen.constant [], List.init count id)
                ||> List.fold (fun soFar index ->
                    gen {
                        let! previous = soFar
                        let! constraints = genParameterConstraints earlier count index previous
                        return previous @ [ constraints ]
                    }
                )

            return
                { declaration with
                    Generics = generics
                }
        }

    /// A client of one to five non-generic types followed by one to three generic ones.
    let private genConstrainedClient : Gen<Declaration list> =
        gen {
            let! plain = genClient
            let! count = Gen.choose (1, 3)
            let! generic = Gen.listOfLength count (genGenericDeclaration (plain |> List.map (fun d -> d.Kind)))
            return plain @ generic
        }

    /// Whether a declaration's type parameters' constraints name a type the provider lacks or one
    /// in the assembly that is not there.
    let private constraintsNameAbsent (declaration : Declaration) : bool =
        declaration.Generics
        |> List.exists (fun constraints ->
            Option.toList constraints.ClassConstraint @ constraints.Others
            |> List.exists (fun spelled ->
                let rendered = render spelled
                rendered.Contains "Gone" || rendered.Contains "Optional."
            )
        )

    [<Test>]
    let ``a generated generic definition fails to load exactly where the host's does, with its exception`` () : unit =
        let property (cover : Outcome * bool -> unit) (declarations : Declaration list) : unit =
            let names =
                declarations
                |> List.mapi (fun i declaration ->
                    match declaration.Generics with
                    | [] -> $"T%d{i}"
                    | generics -> $"""T%d{i}<%s{String.replicate (generics.Length - 1) ","}>"""
                )

            let source =
                declarations |> List.mapi renderDeclaration |> String.concat Environment.NewLine

            let disagreements, host = compare source names

            for declaration, outcome in List.zip declarations host do
                if not declaration.Generics.IsEmpty then
                    cover (outcome, constraintsNameAbsent declaration)

            if not disagreements.IsEmpty then
                failwith (String.concat Environment.NewLine (source :: disagreements))

        let coverage =
            CoverageSample.check
                (Config.QuickThrowOnFailure.WithMaxTest 100)
                (Arb.fromGen genConstrainedClient)
                property

        // Among the generic definitions, enough of each outcome where the constraints name what is
        // not there, and where they do not, for the comparison to say something.
        coverage.Count (Outcome.Loads, false) |> shouldBeGreaterThan 20

        coverage.Count (Outcome.Fails "TypeLoadException", true)
        |> shouldBeGreaterThan 20

        coverage.Count (Outcome.Fails "FileNotFoundException", true)
        |> shouldBeGreaterThan 15

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

    /// Constructed types a `typeof` spells, whose generic arguments CoreCLR loads each in full,
    /// constraints and all, before the next (`SigPointer::GetTypeHandleThrowing`). The argument's
    /// constraint names `IGone`, which `LosesIGone`, as it is loaded, does not implement, so the
    /// argument's own dependencies do not reach it.
    [<Test>]
    let ``a generic argument a type token spells is loaded in full before the next`` () : unit =
        let declarations =
            """
public class Pair<A, B> { }
public class ConstrainedToIGone<T> where T : Provider.IGone { }
"""

        let names =
            [
                "Pair<ConstrainedToIGone<Provider.LosesIGone>, Optional.Thing>"
                "Pair<Optional.Thing, ConstrainedToIGone<Provider.LosesIGone>>"
                "ConstrainedToIGone<Provider.LosesIGone>[]"
                "Pair<Provider.Kept, ConstrainedToIGone<Provider.LosesIGone>>"
            ]

        let client = compileClient declarations names
        let host = hostOutcomes client names

        let pawPrint =
            missesOf hostCoreLib.Value client (fun client -> List.init names.Length (spelledBy client)) false
            |> List.map (fun miss ->
                match miss with
                | None -> Outcome.Loads
                | Some (TypeLoad.LoadMiss.TypeAbsent _) -> Outcome.Fails "TypeLoadException"
                | Some (TypeLoad.LoadMiss.AssemblyUnavailable _) -> Outcome.Fails "FileNotFoundException"
                | Some (TypeLoad.LoadMiss.Unmodelled unmodelled) -> failwith unmodelled.Diagnostic
            )

        let disagreements =
            List.zip3 names host pawPrint
            |> List.choose (fun (name, host, pawPrint) ->
                if host = pawPrint then
                    None
                else
                    Some $"%s{name}: the host %A{host}, PawPrint %A{pawPrint}"
            )

        if not disagreements.IsEmpty then
            failwith (String.concat Environment.NewLine disagreements)

        host
        |> shouldEqual
            [
                Outcome.Fails "TypeLoadException"
                Outcome.Fails "FileNotFoundException"
                Outcome.Fails "TypeLoadException"
                Outcome.Fails "TypeLoadException"
            ]

    /// CoreCLR misreads a multidimensional array in the signature of a type parameter's first
    /// constraint (`DoAccessibilityCheckForConstraintSignature` reads past the element type before
    /// walking it), which `TypeLoad` refuses to follow.
    [<Test>]
    let ``a multidimensional array in a constraint's signature is unmodelled`` () : unit =
        let declarations =
            "public class ConstrainedToInterfaceOfGoneMatrix<T> where T : Provider.IKeptGeneric<Provider.Gone[,]> { }"

        let names = [ "ConstrainedToInterfaceOfGoneMatrix<>" ]
        let client = compileClient declarations names

        hostOutcomes client names
        |> shouldEqual [ Outcome.Fails "BadImageFormatException" ]

        let spellingsOf (client : DumpedAssembly) =
            [
                TypeDefn.FromDefinition (
                    client.TypeDefs.[definitionNamed client names.Head].Identity,
                    SignatureTypeKind.Unknown
                )
            ]

        match missesOf hostCoreLib.Value client spellingsOf false with
        | [ Some (TypeLoad.LoadMiss.Unmodelled _) ] -> ()
        | other -> failwith $"TypeLoad says %A{other}"

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
