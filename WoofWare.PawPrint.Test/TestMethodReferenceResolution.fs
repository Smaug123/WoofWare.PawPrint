namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Collections.Immutable
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// `MethodReferenceResolution` against the real runtime's own answer. For every method-shaped
/// MemberRef row in a set of shared-framework assemblies, `Module.ResolveMethod` on the test host
/// (the same framework files; asserted below) says which MethodDef CoreCLR binds it to, and the
/// resolver must say the same thing.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMethodReferenceResolution =

    /// Chosen for shape rather than size: CoreLib's MemberRefs are almost all instantiations of its
    /// own types, System.Linq and System.Collections call across into CoreLib through generic
    /// instantiations and inherited members, and System.Text.Json and System.Private.Xml are large
    /// ordinary libraries.
    let assemblyNames : string list =
        [
            "System.Private.CoreLib"
            "System.Linq"
            "System.Collections"
            "System.Text.Json"
            "System.Private.Xml"
        ]

    /// Is `mb`, which reflection resolved a MemberRef to, the runtime-supplied array method
    /// `accessor` names?
    let arrayAccessorIs (accessor : ArrayAccessor) (mb : MethodBase) : bool =
        mb.DeclaringType.IsArray
        && match accessor with
           | ArrayAccessor.Get -> mb.Name = "Get"
           | ArrayAccessor.Set -> mb.Name = "Set"
           | ArrayAccessor.Address -> mb.Name = "Address"
           | ArrayAccessor.Constructor arity -> mb.Name = ".ctor" && mb.GetParameters().Length = arity

    /// What the real runtime says a MemberRef binds to.
    [<RequireQualifiedAccess>]
    type private Oracle =
        | Method of MethodBase
        /// `ResolveMethod` threw `MissingMethodException`: nothing of that name and signature is
        /// found where CoreCLR looks.
        | Missing
        /// The reference's parent mentions a type variable, no method's IL uses it to supply a
        /// context, and `object` violates the variable's constraints, so this row says nothing.
        | NoAnswer of exn

    /// A generic context a MemberRef, of a method or a field, is actually used in: the type and
    /// method type parameters of some method whose IL names it, directly or through a MethodSpec.
    /// Those parameters satisfy
    /// whatever constraints the reference's parent places on them, because the compiler checked
    /// exactly that where it emitted the use.
    let contextsOfUse
        (analysed : DumpedAssembly)
        : System.Collections.Generic.IReadOnlyDictionary<MemberReferenceHandle, MethodDefinitionHandle>
        =
        let referenced (token : MetadataToken) : MemberReferenceHandle option =
            match token with
            | MetadataToken.MemberReference h -> Some h
            | MetadataToken.MethodSpecification h ->
                match analysed.MethodSpecs.[h].Method with
                | MetadataToken.MemberReference h -> Some h
                | _ -> None
            | _ -> None

        seq {
            for KeyValue (user, method) in analysed.Methods do
                match method.Body with
                | MethodBody.Il body ->
                    for op, _ in body.Instructions do
                        match op with
                        | IlOp.UnaryMetadataToken (_, MetadataOperand.FromMetadata t) ->
                            match referenced t.Token with
                            | Some h -> yield h, user
                            | None -> ()
                        | _ -> ()
                | _ -> ()
        }
        |> Seq.distinctBy fst
        |> readOnlyDict

    /// Failing a context of use, `object` for every variable, which loads wherever the parent does
    /// not constrain them.
    let private placeholderArguments : Type[] = Array.create 16 typeof<obj>

    let private askReflection (m : Module) (context : MethodDefinitionHandle option) (token : int) : Oracle =
        let resolve (typeArgs : Type[]) (methodArgs : Type[]) : Oracle =
            try
                Oracle.Method (m.ResolveMethod (token, typeArgs, methodArgs))
            with :? MissingMethodException ->
                Oracle.Missing

        try
            resolve null null
        with
        | :? ArgumentException
        | :? BadImageFormatException
        | :? TypeLoadException as e ->
            let inContext =
                context
                |> Option.bind (fun user ->
                    let user =
                        m.ResolveMethod (
                            MetadataTokens.GetToken (MethodDefinitionHandle.op_Implicit user : EntityHandle)
                        )

                    let methodArgs =
                        if user.IsGenericMethodDefinition then
                            user.GetGenericArguments ()
                        else
                            [||]

                    try
                        Some (resolve (user.DeclaringType.GetGenericArguments ()) methodArgs)
                    with _ ->
                        None
                )

            match inContext with
            | Some answer -> answer
            | None ->
                try
                    resolve placeholderArguments placeholderArguments
                with _ ->
                    Oracle.NoAnswer e

    [<TestCaseSource(nameof assemblyNames)>]
    let ``every method MemberRef resolves where the real runtime binds it`` (assemblyName : string) : unit =
        let frameworkDir = HostImage.frameworkDirectory
        let runtimeDirs = HostImage.runtimeDirs
        let _, loggerFactory = LoggerFactory.makeTest ()

        let reflected = Assembly.Load (AssemblyName assemblyName)

        // The oracle is only an oracle if it read the same bytes.
        FrameworkUnderTest.resolveLinks (Path.GetDirectoryName reflected.Location)
        |> shouldEqual (FrameworkUnderTest.resolveLinks frameworkDir)

        let corelib =
            Assembly.readFile loggerFactory (Path.Combine (frameworkDir, "System.Private.CoreLib.dll"))

        let analysed = Assembly.readFile loggerFactory reflected.Location
        let baseClassTypes = BaseClassTypes.ofCorelib corelib
        let loaded = LoadedAssemblies.ofAssemblies [ corelib ; analysed ]

        let mutable ctx : TypeConcretization.ConcretizationContext<DumpedAssembly> =
            {
                ConcreteTypes = Corelib.concretizeAll loaded baseClassTypes AllConcreteTypes.Empty
                LoadedAssemblies = loaded
                BaseTypes = baseClassTypes
            }

        let contexts = contextsOfUse analysed
        let failures = ResizeArray<string> ()
        let mutable agreed = 0
        let mutable noAnswer = 0

        for KeyValue (handle, reference) in analysed.Members do
            match reference.Signature with
            | MemberSignature.Field _ -> ()
            | MemberSignature.Method _ ->
                let token =
                    MetadataTokens.GetToken (MemberReferenceHandle.op_Implicit handle : EntityHandle)

                let describe = $"%s{reference.PrettyName} (0x%08x{token})"

                let ctx', ours =
                    MethodReferenceResolution.resolve
                        loggerFactory
                        runtimeDirs
                        ctx
                        (ctx.LoadedAssemblies.ByDefinitionName analysed.DefinitionFullName)
                        handle

                ctx <- ctx'

                match
                    askReflection
                        reflected.ManifestModule
                        (match contexts.TryGetValue handle with
                         | true, user -> Some user
                         | false, _ -> None)
                        token,
                    ours
                with
                | Oracle.NoAnswer _, _ -> noAnswer <- noAnswer + 1
                | Oracle.Method mb, MethodReferenceTarget.Defined (declaringAssembly, method, _) ->
                    let theirs = mb.Module.Assembly.FullName, mb.MetadataToken

                    let ours =
                        declaringAssembly.DefinitionFullName,
                        MetadataTokens.GetToken (MethodDefinitionHandle.op_Implicit method : EntityHandle)

                    if theirs = ours then
                        agreed <- agreed + 1
                    else
                        failures.Add
                            $"%s{describe}: runtime binds %s{mb.DeclaringType.FullName}::%s{mb.Name} (%A{theirs}), resolver %A{ours}"
                | Oracle.Method mb, MethodReferenceTarget.ArrayMethod (_, accessor) ->
                    if arrayAccessorIs accessor mb then
                        agreed <- agreed + 1
                    else
                        failures.Add
                            $"%s{describe}: runtime binds %s{mb.DeclaringType.FullName}::%s{mb.Name}, resolver an array's %A{accessor}"
                | Oracle.Missing, MethodReferenceTarget.Missing -> agreed <- agreed + 1
                | oracle, ours -> failures.Add $"%s{describe}: runtime %A{oracle}, resolver %A{ours}"

        TestContext.Progress.WriteLine
            $"%s{assemblyName}: %d{agreed} method MemberRefs agree; %d{noAnswer} the runtime could not be asked about"

        if failures.Count > 0 then
            failures |> Seq.truncate 30 |> String.concat Environment.NewLine |> failwith

        // Vacuity guards: a resolver that is never asked anything agrees with everything, and an
        // oracle that cannot be asked is no oracle.
        agreed |> shouldBeGreaterThan 100
        noAnswer * 100 |> shouldBeSmallerThan agreed

    /// Loads each image of `images` by its simple name, so that one refers to another.
    type ImagesContext (images : Map<string, byte[]>) =
        inherit System.Runtime.Loader.AssemblyLoadContext ("Images", isCollectible = true)

        override this.Load (name : AssemblyName) : Assembly =
            match images.TryFind name.Name with
            | Some image -> this.LoadFromStream (new MemoryStream (image))
            | None -> null

    [<Test>]
    let ``a reference whose parent type the provider no longer declares says so`` () : unit =
        let frameworkDir = HostImage.frameworkDirectory
        let runtimeDirs = HostImage.runtimeDirs
        let _, loggerFactory = LoggerFactory.makeTest ()

        let compile (name : string) (references : byte[] list) (text : string) : byte[] =
            Roslyn.compileAssembly
                name
                OutputKind.DynamicallyLinkedLibrary
                (references
                 |> List.map (fun image -> MetadataReference.CreateFromImage (ImmutableArray.CreateRange image)))
                [ text ]

        let version1 =
            compile
                "Provider"
                []
                """
namespace Provider;
public class Gone { public static void M() { } }
public class Kept { public static void M() { } }
"""

        let version2 =
            compile
                "Provider"
                []
                """
namespace Provider;
public class Kept { public static void M() { } }
"""

        let clientImage =
            compile
                "Client"
                [ version1 ]
                """
namespace Client;
public static class Uses
{
    public static void CallGone() { Provider.Gone.M(); }
    public static void CallKept() { Provider.Kept.M(); }
}
"""

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let corelib =
            Assembly.readFile loggerFactory (Path.Combine (frameworkDir, "System.Private.CoreLib.dll"))

        let client = read "Client" clientImage
        let provider = read "Provider" version2
        let baseClassTypes = BaseClassTypes.ofCorelib corelib

        let providerReference =
            client.AssemblyReferences.Values |> Seq.find (fun r -> r.Name.Name = "Provider")

        let loaded =
            LoadedAssemblies.ofAssemblies [ corelib ; client ; provider ]
            |> fun loaded -> fst (loaded.WithBoundReference providerReference provider)

        let ctx : TypeConcretization.ConcretizationContext<DumpedAssembly> =
            {
                ConcreteTypes = Corelib.concretizeAll loaded baseClassTypes AllConcreteTypes.Empty
                LoadedAssemblies = loaded
                BaseTypes = baseClassTypes
            }

        let context =
            new ImagesContext (Map.ofList [ "Provider", version2 ; "Client", clientImage ])

        try
            let reflected = context.LoadFromAssemblyName (AssemblyName "Client")
            let mutable checkedParents = Set.empty

            for KeyValue (handle, reference) in client.Members do
                match reference.Parent with
                | MetadataToken.TypeReference parent when client.TypeRefs.[parent].Namespace = "Provider" ->
                    let parentName = client.TypeRefs.[parent].Name
                    checkedParents <- checkedParents.Add parentName

                    let token =
                        MetadataTokens.GetToken (MemberReferenceHandle.op_Implicit handle : EntityHandle)

                    let runtimeMisses =
                        try
                            reflected.ManifestModule.ResolveMethod token |> ignore<MethodBase>
                            false
                        with :? TypeLoadException ->
                            true

                    let _, ours =
                        MethodReferenceResolution.resolve loggerFactory runtimeDirs ctx client handle

                    match ours with
                    | MethodReferenceTarget.ParentTypeMissing _ ->
                        if not runtimeMisses then
                            failwith $"%s{parentName}.M: the runtime binds it, the resolver says its parent is missing"
                    | other ->
                        if runtimeMisses then
                            failwith
                                $"%s{parentName}.M: the runtime cannot load its parent, the resolver says %A{other}"
                | _ -> ()

            checkedParents |> shouldEqual (Set.ofList [ "Gone" ; "Kept" ])
        finally
            context.Unload ()

    /// What `Module.ResolveMethod` does with a MemberRef.
    [<RequireQualifiedAccess>]
    type private Bound =
        /// It binds the method with this token, in the module of this name.
        | Method of moduleName : string * token : int
        /// It throws an exception of this name.
        | Throws of exceptionName : string

    /// References into an assembly compiled against a provider that declared `Gone`, run against one
    /// that does not, and without the assembly `Optional` at all. CoreCLR compares two type
    /// references in a signature by their names before it resolves either (`CompareTypeTokens`), and
    /// a reference into a bound assembly that lacks the type compares unequal to everything, so a
    /// reference whose signature names `Gone` binds no method, and one naming a type of another name
    /// passes over an overload that names `Gone` or `Optional.Thing` without resolving it. Each pair
    /// of overloads comes in both orders, so that the search meets the one naming the missing type
    /// first, whichever end of the method table it starts from.
    [<Test>]
    let ``a reference whose signature names a type the provider no longer declares binds no method`` () : unit =
        let frameworkDir = HostImage.frameworkDirectory
        let runtimeDirs = HostImage.runtimeDirs
        let _, loggerFactory = LoggerFactory.makeTest ()

        let compile (name : string) (references : byte[] list) (text : string) : byte[] =
            Roslyn.compileAssembly
                name
                OutputKind.DynamicallyLinkedLibrary
                (references
                 |> List.map (fun image -> MetadataReference.CreateFromImage (ImmutableArray.CreateRange image)))
                [ text ]

        let provider (complete : bool) : string =
            "namespace Provider; public class Kept { }"
            + (if complete then " public class Gone { }" else "")

        let completeProvider = compile "Provider" [] (provider true)
        let incompleteProvider = compile "Provider" [] (provider false)
        let optional = compile "Optional" [] "namespace Optional; public class Thing { }"

        let middleImage =
            compile
                "Middle"
                [ completeProvider ; optional ]
                """
namespace Middle;
public static class Api
{
    public static void TakesGone(Provider.Gone x) { }
    public static Provider.Gone ReturnsGone() => null;
    public static void TakesListOfGone(System.Collections.Generic.List<Provider.Gone> x) { }
    public static void GoneFirst(Provider.Gone x) { }
    public static void GoneFirst(Provider.Kept x) { }
    public static void KeptFirst(Provider.Kept x) { }
    public static void KeptFirst(Provider.Gone x) { }
    public static void ThingFirst(Optional.Thing x) { }
    public static void ThingFirst(Provider.Kept x) { }
    public static void KeptBeforeThing(Provider.Kept x) { }
    public static void KeptBeforeThing(Optional.Thing x) { }
}
"""

        let clientImage =
            compile
                "Client"
                [ completeProvider ; optional ; middleImage ]
                """
namespace Client;
public static class Uses
{
    public static void TakesGone() { Middle.Api.TakesGone(null); }
    public static void ReturnsGone() { Middle.Api.ReturnsGone(); }
    public static void TakesListOfGone() { Middle.Api.TakesListOfGone(null); }
    public static void GoneFirstGone() { Middle.Api.GoneFirst((Provider.Gone)null); }
    public static void GoneFirstKept() { Middle.Api.GoneFirst((Provider.Kept)null); }
    public static void KeptFirstGone() { Middle.Api.KeptFirst((Provider.Gone)null); }
    public static void KeptFirstKept() { Middle.Api.KeptFirst((Provider.Kept)null); }
    public static void ThingFirstKept() { Middle.Api.ThingFirst((Provider.Kept)null); }
    public static void KeptBeforeThingKept() { Middle.Api.KeptBeforeThing((Provider.Kept)null); }
}
"""

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let corelib =
            Assembly.readFile loggerFactory (Path.Combine (frameworkDir, "System.Private.CoreLib.dll"))

        let client = read "Client" clientImage
        let middle = read "Middle" middleImage
        let incomplete = read "Provider" incompleteProvider
        let baseClassTypes = BaseClassTypes.ofCorelib corelib

        let referenceTo (from : DumpedAssembly) (name : string) =
            from.AssemblyReferences.Values |> Seq.find (fun r -> r.Name.Name = name)

        let loaded =
            let loaded =
                LoadedAssemblies.ofAssemblies [ corelib ; client ; middle ; incomplete ]

            let loaded, _ = loaded.WithBoundReference (referenceTo client "Provider") incomplete
            let loaded, _ = loaded.WithBoundReference (referenceTo client "Middle") middle
            fst (loaded.WithBoundReference (referenceTo middle "Provider") incomplete)

        let ctx : TypeConcretization.ConcretizationContext<DumpedAssembly> =
            {
                ConcreteTypes = Corelib.concretizeAll loaded baseClassTypes AllConcreteTypes.Empty
                LoadedAssemblies = loaded
                BaseTypes = baseClassTypes
            }

        // Each caller, and whether the reference it makes binds a method.
        let callers =
            [
                "TakesGone", false
                "ReturnsGone", false
                "TakesListOfGone", false
                "GoneFirstGone", false
                "GoneFirstKept", true
                "KeptFirstGone", false
                "KeptFirstKept", true
                "ThingFirstKept", true
                "KeptBeforeThingKept", true
            ]

        // The MemberRef each caller's body makes.
        let referenceOf (caller : string) : MemberReferenceHandle =
            let method =
                client.Methods.Values |> Seq.filter (fun m -> m.Name = caller) |> Seq.exactlyOne

            match method.Body with
            | MethodBody.Il body ->
                body.Instructions
                |> List.pick (fun (op, _) ->
                    match op with
                    | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Call,
                                               MetadataOperand.FromMetadata {
                                                                                Token = MetadataToken.MemberReference handle
                                                                            }) -> Some handle
                    | _ -> None
                )
            | body -> failwith $"%s{caller}: no IL body: %A{body}"

        let context =
            new ImagesContext (
                Map.ofList
                    [
                        "Provider", incompleteProvider
                        "Middle", middleImage
                        "Client", clientImage
                    ]
            )

        try
            let reflected = context.LoadFromAssemblyName (AssemblyName "Client")

            let failures =
                [
                    for caller, binds in callers do
                        let handle = referenceOf caller

                        let token =
                            MetadataTokens.GetToken (MemberReferenceHandle.op_Implicit handle : EntityHandle)

                        let host =
                            try
                                let bound = reflected.ManifestModule.ResolveMethod token
                                Bound.Method (bound.Module.Assembly.GetName().Name, bound.MetadataToken)
                            with e ->
                                Bound.Throws (e.GetType().FullName)

                        let _, target =
                            MethodReferenceResolution.resolve loggerFactory runtimeDirs ctx client handle

                        let ours =
                            match target with
                            | MethodReferenceTarget.Defined (assembly, method, _) ->
                                Bound.Method (
                                    assembly.Name.Name,
                                    MetadataTokens.GetToken (MethodDefinitionHandle.op_Implicit method : EntityHandle)
                                )
                            | MethodReferenceTarget.Missing -> Bound.Throws "System.MissingMethodException"
                            | other -> Bound.Throws $"%A{other}"

                        match host with
                        | Bound.Method _ when not binds -> yield $"%s{caller}: the host binds %A{host}"
                        | Bound.Throws _ when binds -> yield $"%s{caller}: the host throws %A{host}"
                        | _ -> ()

                        if ours <> host then
                            yield $"%s{caller}: the host says %A{host}, the resolver %A{ours}"
                ]

            if not failures.IsEmpty then
                failures |> String.concat Environment.NewLine |> failwith
        finally
            context.Unload ()
