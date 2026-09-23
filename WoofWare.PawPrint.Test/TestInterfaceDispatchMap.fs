namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Text
open FsUnitTyped
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open NUnit.Framework
open WoofWare.PawPrint

/// `InterfaceDispatch.tryFindImplementationSlot` is the single answer to "which method implements
/// this interface method on this receiver, if a class does". The host CLR answers the same question
/// through `Type.GetInterfaceMap`, which runs the real `MethodTable::FindDispatchImpl` over the real
/// dispatch maps that `MethodTableBuilder` built -- so it is an exact, outside oracle, and nothing
/// PawPrint computes feeds into it.
///
/// Two corpora go through it. CoreLib's own types are a broad one that costs nothing to write, and
/// PawPrint reads the very image the host has loaded. A generated corpus of small class hierarchies
/// is the targeted one: CoreLib has few `new` shadows or re-declared interfaces, and those are
/// where slot ownership is decided.
[<TestFixture>]
module TestInterfaceDispatchMap =

    let private loggerFactory = snd (LoggerFactory.makeTest ())

    // Undisposed on purpose: the DumpedAssembly's logger closes over its sinks.
    let private corelib : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory typeof<obj>.Assembly.Location

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private stateOf (assemblies : DumpedAssembly list) : IlMachineState =
        let loaded = LoadedAssemblies.ofAssemblies (corelib :: assemblies)

        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = Corelib.concretizeAll loaded bct AllConcreteTypes.Empty
            _LoadedAssemblies = loaded
        }

    /// The host type a PawPrint handle denotes. `hostAssemblies` maps each PawPrint assembly's
    /// definition name to the host's loaded copy of the same image.
    let rec private toHost
        (hostAssemblies : Map<string, System.Reflection.Assembly>)
        (state : IlMachineState)
        (handle : ConcreteTypeHandle)
        : Type
        =
        match handle with
        | ConcreteTypeHandle.Concrete _ ->
            let ty =
                AllConcreteTypes.lookup handle state.ConcreteTypes
                |> Option.defaultWith (fun () -> failwith $"%O{handle} is not registered")

            let definition =
                hostAssemblies.[ty.Identity.AssemblyFullName].ManifestModule
                    .ResolveType (
                        MetadataTokens.GetToken (
                            TypeDefinitionHandle.op_Implicit ty.Identity.TypeDefinition.Get : EntityHandle
                        )
                    )

            if ty.Generics.IsEmpty then
                definition
            else
                definition.MakeGenericType (ty.Generics |> Seq.map (toHost hostAssemblies state) |> Array.ofSeq)
        | ConcreteTypeHandle.OneDimArrayZero element -> (toHost hostAssemblies state element).MakeArrayType ()
        | ConcreteTypeHandle.Array (element, rank) -> (toHost hostAssemblies state element).MakeArrayType rank
        | ConcreteTypeHandle.Pointer element -> (toHost hostAssemblies state element).MakePointerType ()
        | ConcreteTypeHandle.Byref element -> (toHost hostAssemblies state element).MakeByRefType ()
        | ConcreteTypeHandle.FunctionPointer _ -> failwith $"no host type is built for the function pointer %O{handle}"

    let private concretizeDefinition
        (state : IlMachineState)
        (assembly : DumpedAssembly)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (arguments : ConcreteTypeHandle list)
        : IlMachineState * ConcreteTypeHandle
        =
        DumpedAssembly.typeInfoToTypeDefn' bct state._LoadedAssemblies typeInfo
        |> IlMachineState.concretizeType
            loggerFactory
            bct
            state
            assembly.DefinitionFullName
            (ImmutableArray.CreateRange arguments)
            ImmutableArray.Empty

    type private Outcome =
        {
            /// (interface, interface method) pairs compared.
            Compared : int
            /// Of those, how many the host answers with a class method rather than a default body.
            ClassImplemented : int
            Failures : string list
        }

    /// Compare PawPrint's interface map and dispatch against the host's for one receiver.
    let private compareReceiver
        (hostAssemblies : Map<string, System.Reflection.Assembly>)
        (state : IlMachineState)
        (receiver : ConcreteTypeHandle)
        (host : Type)
        : IlMachineState * Outcome
        =
        let state, interfaceMap =
            InterfaceDispatch.interfaceMapOf loggerFactory bct "test" state receiver

        let pawPrintInterfaces =
            interfaceMap
            |> List.map (fun entry -> toHost hostAssemblies state entry.Interface)

        let hostInterfaces = host.GetInterfaces () |> List.ofArray

        let mapFailures =
            if pawPrintInterfaces <> hostInterfaces then
                let render (types : Type list) =
                    types |> List.map (fun t -> t.ToString ()) |> String.concat ", "

                [
                    $"%O{host}: interface map\n    PawPrint [%s{render pawPrintInterfaces}]\n    host     [%s{render hostInterfaces}]"
                ]
            else
                []

        let state, _, table =
            match VirtualSlotLayout.dispatchTableOfClosed loggerFactory bct "test" state receiver with
            | state, Some table -> state, (), table
            | _, None -> failwith $"%O{host} has no method table"

        let hostAssemblyOf (fullName : string) = hostAssemblies.[fullName]

        ((state,
          {
              Compared = 0
              ClassImplemented = 0
              Failures = mapFailures
          }),
         interfaceMap)
        ||> List.fold (fun (state, outcome) entry ->
            let hostInterface = toHost hostAssemblies state entry.Interface

            // Only interfaces the host agrees the type implements can be asked about; a
            // disagreement there is already reported above.
            if not (List.contains hostInterface hostInterfaces) then
                state, outcome
            else

            let hostMap = host.GetInterfaceMap hostInterface

            ((state, outcome), Seq.zip hostMap.InterfaceMethods hostMap.TargetMethods)
            ||> Seq.fold (fun (state, outcome) (interfaceMethod, target) ->
                // Static virtuals are resolved through MethodImpl rows alone, not this map.
                if interfaceMethod.IsStatic then
                    state, outcome
                else

                let interfaceMethodKey : SlotIdentity =
                    entry.Type.AssemblyFullName,
                    (Some (MetadataTokens.MethodDefinitionHandle (interfaceMethod.MetadataToken &&& 0xFFFFFF)), None)

                let state, slot =
                    InterfaceDispatch.tryFindImplementationSlot
                        loggerFactory
                        bct
                        "test"
                        state
                        receiver
                        true
                        entry.Interface
                        interfaceMethodKey

                let hostIsClassImplementation =
                    not (isNull target) && not target.DeclaringType.IsInterface

                let describeHost () =
                    if isNull target then
                        "no implementation"
                    else
                        $"%O{target.DeclaringType}::%s{target.Name} (0x%08x{target.MetadataToken})"

                let failure =
                    match slot with
                    | None when hostIsClassImplementation ->
                        Some $"no class implementation, host says %s{describeHost ()}"
                    | None -> None
                    | Some slot ->
                        let occupant = table.Occupants.[slot]

                        let occupantToken =
                            occupant.Method.TryMetadata
                            |> Option.map (fun metadata ->
                                MetadataTokens.GetToken (
                                    MethodDefinitionHandle.op_Implicit metadata.Handle : EntityHandle
                                )
                            )

                        let agrees =
                            hostIsClassImplementation
                            && Object.ReferenceEquals (
                                hostAssemblyOf occupant.DeclaredBy.AssemblyFullName,
                                target.Module.Assembly
                            )
                            && occupantToken = Some target.MetadataToken

                        if agrees then
                            None
                        else
                            Some
                                $"slot %i{slot} holds %s{occupant.DeclaredBy.Description}::%s{occupant.Method.Name}, host says %s{describeHost ()}"

                let outcome =
                    {
                        Compared = outcome.Compared + 1
                        ClassImplemented = outcome.ClassImplemented + (if hostIsClassImplementation then 1 else 0)
                        Failures =
                            match failure with
                            | None -> outcome.Failures
                            | Some failure ->
                                $"%O{host} via %O{hostInterface}::%s{interfaceMethod.Name}: %s{failure}"
                                :: outcome.Failures
                    }

                state, outcome
            )
        )

    let private report (label : string) (failures : string list) : unit =
        if not failures.IsEmpty then
            let shown = failures |> List.rev |> List.truncate 40

            failwith (
                $"%s{label}: %i{failures.Length} disagreements with the host CLR (first %i{shown.Length}):\n"
                + String.Join ("\n", shown)
            )

    /// Every non-generic class and struct CoreLib declares, and a spread of generic instantiations.
    [<Test>]
    let ``CoreLib interface dispatch agrees with the host CLR`` () : unit =
        let hostAssemblies = Map.ofList [ corelib.DefinitionFullName, typeof<obj>.Assembly ]

        let nonGeneric =
            typeof<obj>.Assembly.GetTypes ()
            |> Array.filter (fun t ->
                not t.IsInterface
                && not t.IsGenericTypeDefinition
                && not t.ContainsGenericParameters
                && t.GetInterfaces().Length > 0
            )
            |> List.ofArray

        let generic : Type list =
            [
                typeof<System.Collections.Generic.List<int>>
                typeof<System.Collections.Generic.List<string>>
                typeof<System.Collections.Generic.Dictionary<string, obj>>
                typeof<System.Collections.Generic.HashSet<int>>
                typeof<System.Collections.Generic.Queue<Exception>>
                typeof<System.Collections.ObjectModel.ReadOnlyCollection<string>>
                typeof<System.Collections.ObjectModel.Collection<int>>
                typeof<System.Collections.Generic.EqualityComparer<string>>
                typeof<System.Collections.Generic.Comparer<int>>
                typeof<Nullable<int>>
                typeof<ArraySegment<byte>>
                typeof<Memory<char>>
                typeof<ValueTuple<int, string>>
                typeof<Tuple<int, string>>
                typeof<Lazy<int>>
                typeof<System.Collections.Generic.KeyValuePair<int, string>>
            ]

        let mutable outcome =
            {
                Compared = 0
                ClassImplemented = 0
                Failures = []
            }

        let mutable state = stateOf []

        for host in nonGeneric @ generic do
            let hostDefinition =
                if host.IsGenericType then
                    host.GetGenericTypeDefinition ()
                else
                    host

            let typeInfo =
                corelib.TypeDefs.[MetadataTokens.TypeDefinitionHandle (hostDefinition.MetadataToken &&& 0xFFFFFF)]

            let afterArguments, arguments =
                ((state, []), host.GenericTypeArguments)
                ||> Array.fold (fun (state, acc) argument ->
                    let argumentInfo =
                        corelib.TypeDefs.[MetadataTokens.TypeDefinitionHandle (argument.MetadataToken &&& 0xFFFFFF)]

                    let state, handle = concretizeDefinition state corelib argumentInfo []
                    state, acc @ [ handle ]
                )

            let afterConcretizing, receiver =
                concretizeDefinition afterArguments corelib typeInfo arguments

            let afterComparing, one =
                compareReceiver hostAssemblies afterConcretizing receiver host

            state <- afterComparing

            outcome <-
                {
                    Compared = outcome.Compared + one.Compared
                    ClassImplemented = outcome.ClassImplemented + one.ClassImplemented
                    Failures = one.Failures @ outcome.Failures
                }

        TestContext.Progress.WriteLine (
            $"CoreLib corpus: %i{outcome.Compared} comparisons, %i{outcome.ClassImplemented} class-implemented"
        )

        report "CoreLib" outcome.Failures
        // Floors well below what the corpus yields, so that a corpus gone quietly empty is caught.
        outcome.Compared |> shouldBeGreaterThan 2000
        outcome.ClassImplemented |> shouldBeGreaterThan 1000

    /// The interfaces every generated hierarchy draws on. `IDefault`'s methods have bodies, so that a
    /// slot no class implements is legal and dispatch falls through to it.
    let private interfaceSource : string =
        """
using System;
public interface IA { long M(); long N(); }
public interface IB : IA { long P(); }
public interface IDefault { long M() => 100; long Q() => 101; }
public interface IIn<in T> { long V(T x); }
public interface IOut<out T> { T O(); }
public class Gen1<T> : IIn<int> { public long V(T x) => 1; public long V(int x) => 2; }
public class Gen2<T> : IIn<T> { public long V(int x) => 3; public long V(T x) => 4; }
public class Gen3<T> : Gen2<T>, IIn<int> { }
public class Gen4<T> : Gen2<int>, IIn<T> { public new long V(T x) => 5; }
"""

    /// Each interface a generated class may list, and the interfaces listing it brings with it.
    let private interfaces : (string * string list) list =
        [
            "IA", [ "IA" ]
            "IB", [ "IB" ; "IA" ]
            "IDefault", [ "IDefault" ]
            "IIn<object>", [ "IIn<object>" ]
            "IIn<Exception>", [ "IIn<Exception>" ]
            "IIn<ArgumentException>", [ "IIn<ArgumentException>" ]
            "IOut<object>", [ "IOut<object>" ]
            "IOut<string>", [ "IOut<string>" ]
        ]

    /// Every member a class may declare, with the interfaces it can implement explicitly and whether
    /// those interfaces leave it without a default body.
    let private members : (string * string list * bool) list =
        [
            "long M()", [ "IA" ; "IDefault" ], true
            "long N()", [ "IA" ], true
            "long P()", [ "IB" ], true
            "long Q()", [ "IDefault" ], false
            "long V(object x)", [ "IIn<object>" ], true
            "long V(Exception x)", [ "IIn<Exception>" ], true
            "long V(ArgumentException x)", [ "IIn<ArgumentException>" ], true
            "object O()", [ "IOut<object>" ], true
            "string O()", [ "IOut<string>" ], true
        ]

    [<RequireQualifiedAccess>]
    type private Modifier =
        | Plain
        | Virtual
        | New
        | NewVirtual
        | Override
        | SealedOverride
        | ProtectedVirtual
        | Abstract
        | Explicit of owner : string

    /// What the nearest declaration of a signature on the chain so far makes of it.
    type private Declaration =
        {
            Public : bool
            Overridable : bool
            Abstract : bool
        }

    /// One small class hierarchy, as C# source, drawn from the shapes that decide slot ownership:
    /// `new` shadows, `virtual`/`override` chains, re-listed interfaces, explicit implementations and
    /// abstract members. The generator follows enough of C#'s rules that most hierarchies compile;
    /// `compileSurvivors` drops the rest. Every class is named with the hierarchy's prefix, so that a
    /// compiler error can be traced back to the hierarchy that caused it.
    let private hierarchy (index : int) (random : Random) : string =
        let depth = random.Next (1, 5)
        let source = StringBuilder ()
        let mutable constant = 0
        // Keyed on name and parameter list, which is what hiding and overriding go by.
        let mutable inherited : Map<string, string * Declaration> = Map.empty

        let parameterKey (signature : string) =
            signature.Substring (signature.IndexOf ' ' + 1)

        source.AppendLine "using System;" |> ignore<StringBuilder>

        let render (signature : string) (modifier : Modifier) : string =
            let returnType = signature.Substring (0, signature.IndexOf ' ')
            let rest = signature.Substring (signature.IndexOf ' ' + 1)

            let body =
                constant <- constant + 1

                match returnType with
                | "long" -> $" => %i{index * 1000 + constant};"
                | _ -> " => null;"

            match modifier with
            | Modifier.Plain -> $"public %s{signature}%s{body}"
            | Modifier.Virtual -> $"public virtual %s{signature}%s{body}"
            | Modifier.New -> $"public new %s{signature}%s{body}"
            | Modifier.NewVirtual -> $"public new virtual %s{signature}%s{body}"
            | Modifier.Override -> $"public override %s{signature}%s{body}"
            | Modifier.SealedOverride -> $"public sealed override %s{signature}%s{body}"
            | Modifier.ProtectedVirtual -> $"protected virtual %s{signature}%s{body}"
            | Modifier.Abstract -> $"public abstract %s{signature};"
            | Modifier.Explicit owner -> $"%s{returnType} %s{owner}.%s{rest}%s{body}"

        let effect (modifier : Modifier) : Declaration option =
            match modifier with
            | Modifier.Plain
            | Modifier.New ->
                Some
                    {
                        Public = true
                        Overridable = false
                        Abstract = false
                    }
            | Modifier.Virtual
            | Modifier.NewVirtual
            | Modifier.Override ->
                Some
                    {
                        Public = true
                        Overridable = true
                        Abstract = false
                    }
            | Modifier.SealedOverride ->
                Some
                    {
                        Public = true
                        Overridable = false
                        Abstract = false
                    }
            | Modifier.ProtectedVirtual ->
                Some
                    {
                        Public = false
                        Overridable = true
                        Abstract = false
                    }
            | Modifier.Abstract ->
                Some
                    {
                        Public = true
                        Overridable = true
                        Abstract = true
                    }
            | Modifier.Explicit _ -> None

        for level in 0 .. depth - 1 do
            let name = $"H%i{index}_C%i{level}"
            let isAbstract = random.Next 4 = 0

            let listed = interfaces |> List.filter (fun _ -> random.Next 4 = 0)

            let closure = listed |> List.collect snd |> List.distinct
            let listedNames = listed |> List.map fst

            // Named members at this level, one per name and parameter list: `O()` cannot be
            // overloaded on its return type alone.
            let mutable named : Map<string, string * Modifier> = Map.empty
            let mutable explicits : (string * Modifier) list = []

            for signature, owners, _ in members do
                let key = parameterKey signature
                let above = Map.tryFind key inherited
                let sameSignatureAbove = above |> Option.filter (fun (s, _) -> s = signature)

                let aboveIsAbstract =
                    above |> Option.map (fun (_, d) -> d.Abstract) |> Option.defaultValue false

                let canOverride =
                    sameSignatureAbove
                    |> Option.map (fun (_, d) -> d.Public && d.Overridable)
                    |> Option.defaultValue false

                let explicitChoices =
                    owners
                    |> List.filter (fun owner -> List.contains owner closure)
                    |> List.map Modifier.Explicit

                if named.ContainsKey key then
                    ()
                elif aboveIsAbstract then
                    // An inherited abstract member can only be overridden, and a concrete class must.
                    if sameSignatureAbove.IsSome && (not isAbstract || random.Next 2 = 0) then
                        named <- Map.add key (signature, Modifier.Override) named
                elif random.Next 3 = 0 then
                    let choices =
                        [
                            Modifier.Plain
                            Modifier.Virtual
                            Modifier.New
                            Modifier.NewVirtual
                            Modifier.ProtectedVirtual
                        ]
                        @ (if canOverride then
                               [ Modifier.Override ; Modifier.SealedOverride ]
                           else
                               [])
                        @ (if isAbstract then [ Modifier.Abstract ] else [])
                        @ explicitChoices

                    match choices.[random.Next choices.Length] with
                    | Modifier.Explicit _ as modifier -> explicits <- (signature, modifier) :: explicits
                    | modifier -> named <- Map.add key (signature, modifier) named

            let nearest (signature : string) : Declaration option =
                match Map.tryFind (parameterKey signature) named with
                | Some (s, modifier) -> if s = signature then effect modifier else None
                | None ->
                    Map.tryFind (parameterKey signature) inherited
                    |> Option.bind (fun (s, d) -> if s = signature then Some d else None)

            // Every interface method this class lists and no default body covers must be implemented,
            // by a public member here or above, or explicitly here.
            for signature, owners, required in members do
                if required then
                    for owner in owners do
                        if List.contains owner closure then
                            let explicitHere = List.contains (signature, Modifier.Explicit owner) explicits

                            let publicNearest =
                                nearest signature |> Option.map _.Public |> Option.defaultValue false

                            if not explicitHere && not publicNearest then
                                let key = parameterKey signature

                                if not (named.ContainsKey key) && random.Next 2 = 0 then
                                    let modifier =
                                        if random.Next 2 = 0 then
                                            Modifier.Plain
                                        else
                                            Modifier.Virtual

                                    named <- Map.add key (signature, modifier) named
                                else
                                    explicits <- (signature, Modifier.Explicit owner) :: explicits

            let header =
                let modifier = if isAbstract then "abstract " else ""

                let bases =
                    (if level = 0 then [] else [ $"H%i{index}_C%i{level - 1}" ]) @ listedNames

                match bases with
                | [] -> $"public %s{modifier}class %s{name}"
                | bases ->
                    let joined = String.Join (", ", bases)
                    $"public %s{modifier}class %s{name} : %s{joined}"

            source.AppendLine(header).AppendLine "{" |> ignore<StringBuilder>

            for KeyValue (_, (signature, modifier)) in named do
                source.Append("    ").AppendLine (render signature modifier)
                |> ignore<StringBuilder>

            for signature, modifier in List.rev explicits do
                source.Append("    ").AppendLine (render signature modifier)
                |> ignore<StringBuilder>

            source.AppendLine "}" |> ignore<StringBuilder>

            for KeyValue (key, (signature, modifier)) in named do
                match effect modifier with
                | Some declaration -> inherited <- Map.add key (signature, declaration) inherited
                | None -> ()

        source.ToString ()

    let private references : MetadataReference list =
        let runtimeDirectory =
            Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory ()

        Directory.GetFiles (runtimeDirectory, "*.dll")
        |> Array.filter (fun path ->
            // Native images in the runtime directory are not metadata.
            try
                use stream = File.OpenRead path
                use reader = new System.Reflection.PortableExecutable.PEReader (stream)
                reader.HasMetadata
            with _ ->
                false
        )
        |> Array.map (fun path -> MetadataReference.CreateFromFile path :> MetadataReference)
        |> List.ofArray

    /// Compile every hierarchy into one image, dropping any that do not compile -- most random
    /// shapes leave an interface method unimplemented, or override something that is not virtual --
    /// until what is left compiles cleanly.
    let rec private compileSurvivors (hierarchies : (int * string) list) : (int * string) list * byte[] =
        let trees =
            (CSharpSyntaxTree.ParseText (interfaceSource, path = "Interfaces.cs"))
            :: (hierarchies
                |> List.map (fun (index, source) -> CSharpSyntaxTree.ParseText (source, path = $"H%i{index}.cs")))

        let compilation =
            CSharpCompilation.Create (
                "InterfaceDispatchCorpus",
                trees,
                references,
                CSharpCompilationOptions OutputKind.DynamicallyLinkedLibrary
            )

        use stream = new MemoryStream ()
        let result = compilation.Emit stream

        if result.Success then
            hierarchies, stream.ToArray ()
        else
            let broken =
                result.Diagnostics
                |> Seq.filter (fun diagnostic -> diagnostic.Severity = DiagnosticSeverity.Error)
                |> Seq.map (fun diagnostic -> diagnostic.Location.SourceTree.FilePath)
                |> Set.ofSeq

            if broken.Contains "Interfaces.cs" then
                failwith (
                    "the shared interface source does not compile: "
                    + String.Join ("\n", result.Diagnostics)
                )

            hierarchies
            |> List.filter (fun (index, _) -> not (broken.Contains $"H%i{index}.cs"))
            |> compileSurvivors

    let private generated : (int * string) list * byte[] =
        let random = Random 1380

        [ 0..2999 ]
        |> List.map (fun index -> index, hierarchy index random)
        |> compileSurvivors

    /// The generated corpus, and a handful of fixed generic shapes whose signatures only coincide
    /// once closed, against the host CLR.
    [<Test>]
    let ``generated hierarchies' interface dispatch agrees with the host CLR`` () : unit =
        let survivors, image = generated
        // Floor well below the ~survivor count, so that a generator gone mostly-invalid is caught.
        survivors.Length |> shouldBeGreaterThan 200

        let dumped =
            let _, loggerFactory = LoggerFactory.makeTest ()
            use stream = new MemoryStream (image)
            Assembly.read loggerFactory None stream

        let context =
            Runtime.Loader.AssemblyLoadContext ("interfaceDispatchOracle", isCollectible = true)

        try
            // Nothing in the image executes: `GetInterfaceMap` reads the method tables alone.
            let hostAssembly =
                use stream = new MemoryStream (image)
                context.LoadFromStream stream

            let hostAssemblies =
                Map.ofList
                    [
                        corelib.DefinitionFullName, typeof<obj>.Assembly
                        dumped.DefinitionFullName, hostAssembly
                    ]

            let sourceOf = survivors |> Map.ofList

            let typeInfoOf (host : Type) =
                dumped.TypeDefs.[MetadataTokens.TypeDefinitionHandle (host.MetadataToken &&& 0xFFFFFF)]

            let receivers : (IlMachineState -> IlMachineState * ConcreteTypeHandle * Type) list =
                let nonGeneric =
                    hostAssembly.GetTypes ()
                    |> Array.filter (fun t -> t.Name.StartsWith 'H' && not t.IsInterface)
                    |> Array.map (fun host ->
                        fun state ->
                            let state, handle = concretizeDefinition state dumped (typeInfoOf host) []
                            state, handle, host
                    )
                    |> List.ofArray

                let closedAt (definition : string) (argument : Type) =
                    fun (state : IlMachineState) ->
                        let host = hostAssembly.GetType(definition).MakeGenericType argument

                        let argumentInfo =
                            corelib.TypeDefs.[MetadataTokens.TypeDefinitionHandle (argument.MetadataToken &&& 0xFFFFFF)]

                        let state, argumentHandle = concretizeDefinition state corelib argumentInfo []

                        let state, handle =
                            concretizeDefinition
                                state
                                dumped
                                (typeInfoOf (host.GetGenericTypeDefinition ()))
                                [ argumentHandle ]

                        state, handle, host

                nonGeneric
                @ [
                    for definition in [ "Gen1`1" ; "Gen2`1" ; "Gen3`1" ; "Gen4`1" ] do
                        for argument in [ typeof<int> ; typeof<string> ] do
                            closedAt definition argument
                ]

            let mutable outcome =
                {
                    Compared = 0
                    ClassImplemented = 0
                    Failures = []
                }

            let mutable state = stateOf [ dumped ]

            for receiver in receivers do
                let afterConcretizing, handle, host = receiver state

                let afterComparing, one =
                    compareReceiver hostAssemblies afterConcretizing handle host

                state <- afterComparing

                let failures =
                    match one.Failures with
                    | [] -> []
                    | failures ->
                        // The source is what makes a failure reproducible by hand.
                        let index =
                            if host.Name.StartsWith 'H' then
                                Some (int (host.Name.Substring (1, host.Name.IndexOf '_' - 1)))
                            else
                                None

                        let source =
                            match index with
                            | Some index -> sourceOf.[index]
                            | None -> interfaceSource

                        failures |> List.map (fun failure -> $"%s{failure}\n%s{source}")

                outcome <-
                    {
                        Compared = outcome.Compared + one.Compared
                        ClassImplemented = outcome.ClassImplemented + one.ClassImplemented
                        Failures = failures @ outcome.Failures
                    }

            TestContext.Progress.WriteLine (
                $"generated corpus: %i{survivors.Length} hierarchies, %i{outcome.Compared} comparisons, %i{outcome.ClassImplemented} class-implemented"
            )

            report "generated" outcome.Failures
            outcome.Compared |> shouldBeGreaterThan 1000
            outcome.ClassImplemented |> shouldBeGreaterThan 500
        finally
            context.Unload ()
