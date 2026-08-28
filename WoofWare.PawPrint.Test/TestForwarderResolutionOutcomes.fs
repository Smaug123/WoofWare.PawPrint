namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open FsUnitTyped
open Microsoft.CodeAnalysis
open Microsoft.Extensions.Logging
open NUnit.Framework
open WoofWare.PawPrint

/// <summary>
/// What following a type forwarder reports when the chain does not arrive at a type.
/// </summary>
/// <remarks>
/// Three shapes, and a caller reporting to a guest has to tell them apart: real .NET answers a
/// forwarder whose target assembly is missing with <c>FileNotFoundException</c>, and one whose
/// target binds but declares no such type with a plain absence. Both used to reach the same place
/// here — a host <c>failwith</c> inside the resolver, which no caller could react to.
/// </remarks>
[<TestFixture>]
module TestForwarderResolutionOutcomes =

    /// The assembly the forwarder points at, in the layout where it declares the type.
    let private libSource =
        """
namespace Fwd;

public sealed class Target
{
}
"""

    /// The same assembly *name*, so it binds under the reference the facade carries, declaring
    /// something else. This is the "assembly present, type absent" layout.
    let private libWithoutTargetSource =
        """
namespace Fwd;

public sealed class Other
{
}
"""

    let private facadeSource =
        """
using System.Runtime.CompilerServices;

[assembly: TypeForwardedTo(typeof(Fwd.Target))]

namespace FwdFacade;

public sealed class Marker
{
}
"""

    let private libName = "ForwarderOutcomes.Lib"
    let private facadeName = "ForwarderOutcomes.Facade"

    /// The shared framework, so that a forwarder which *does* arrive can have its target's base
    /// chain primed the way production primes it.
    let private runtimeDir : string =
        Path.GetDirectoryName typeof<obj>.Assembly.Location

    /// The library's own base class, in a third assembly. Only the `BaseMissing` layout keeps it
    /// off disk.
    let private baseSource =
        """
namespace Fwd;

public class Root
{
}
"""

    /// As `libSource`, but the forwarded type derives from a class in a third assembly, so that
    /// arriving at the type is not the last thing that has to load.
    let private libDerivedSource =
        """
namespace Fwd;

public sealed class Target : Root
{
}
"""

    /// The same base assembly *name*, declaring something other than `Root`, so the library's
    /// base-type reference resolves to an assembly that binds and does not contain it.
    let private baseWithoutRootSource =
        """
namespace Fwd;

public class Stranger
{
}
"""

    let private baseName = "ForwarderOutcomes.Base"

    /// Which image, if any, is written under the library's name in the directory resolution
    /// searches. `NotPresent` leaves the facade's reference unbindable; `BaseMissing` writes a
    /// library that binds and declares the type, but whose base class assembly is absent.
    type private LibLayout =
        | DeclaresTheType
        | DeclaresSomethingElse
        | NotPresent
        | BaseMissing
        | BaseTypeGone

    /// Lays the facade and the chosen library image (or none) out on disk, and hands the body
    /// everything it needs to follow the facade's forwarder for `Fwd.Target`.
    ///
    /// Only the facade starts loaded, so the walk has to bind the library itself — which is the
    /// step that fails in the `NotPresent` layout.
    let private withLayout
        (layout : LibLayout)
        (body : ILoggerFactory -> string list -> DumpedAssembly -> ExportedType -> LoadedAssemblies -> 'a)
        : 'a
        =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let baseImage =
            Roslyn.compileAssembly baseName OutputKind.DynamicallyLinkedLibrary [] [ baseSource ]

        let libImage =
            match layout with
            | LibLayout.BaseMissing
            | LibLayout.BaseTypeGone ->
                Roslyn.compileAssembly
                    libName
                    OutputKind.DynamicallyLinkedLibrary
                    [ MetadataReference.CreateFromImage baseImage ]
                    [ libDerivedSource ]
            | _ -> Roslyn.compileAssembly libName OutputKind.DynamicallyLinkedLibrary [] [ libSource ]

        let facadeImage =
            Roslyn.compileAssembly
                facadeName
                OutputKind.DynamicallyLinkedLibrary
                [
                    MetadataReference.CreateFromImage libImage
                    MetadataReference.CreateFromImage baseImage
                ]
                [ facadeSource ]

        let tempDir = Path.Combine (Path.GetTempPath (), Path.GetRandomFileName ())
        Directory.CreateDirectory tempDir |> ignore

        try
            let libOnDisk =
                match layout with
                | LibLayout.DeclaresTheType
                | LibLayout.BaseMissing
                | LibLayout.BaseTypeGone -> Some libImage
                | LibLayout.DeclaresSomethingElse ->
                    Roslyn.compileAssembly libName OutputKind.DynamicallyLinkedLibrary [] [ libWithoutTargetSource ]
                    |> Some
                | LibLayout.NotPresent -> None

            // Written for every layout but the one it is named for, so `BaseMissing` differs from
            // `DeclaresTheType` in exactly one file's presence.
            match layout with
            | LibLayout.BaseMissing -> ()
            | LibLayout.BaseTypeGone ->
                // Binds under the name the library's reference asks for, and does not declare the
                // base class the library was compiled against.
                Roslyn.compileAssembly baseName OutputKind.DynamicallyLinkedLibrary [] [ baseWithoutRootSource ]
                |> fun bytes -> File.WriteAllBytes (Path.Combine (tempDir, baseName + ".dll"), bytes)
            | LibLayout.DeclaresTheType
            | LibLayout.DeclaresSomethingElse
            | LibLayout.NotPresent -> File.WriteAllBytes (Path.Combine (tempDir, baseName + ".dll"), baseImage)

            match libOnDisk with
            | Some bytes -> File.WriteAllBytes (Path.Combine (tempDir, libName + ".dll"), bytes)
            | None -> ()

            let facadePath = Path.Combine (tempDir, facadeName + ".dll")
            File.WriteAllBytes (facadePath, facadeImage)

            let facade = Assembly.readFile loggerFactory facadePath

            let exported =
                facade.TryGetTopLevelExportedType (Some "Fwd") "Target"
                |> Option.defaultWith (fun () ->
                    failwith $"%s{facadeName} has no top-level exported type Fwd.Target; the forwarder did not compile"
                )

            body loggerFactory [ tempDir ; runtimeDir ] facade exported (LoadedAssemblies.ofAssemblies [ facade ])
        finally
            try
                if Directory.Exists tempDir then
                    Directory.Delete (tempDir, true)
            with
            | :? IOException
            | :? UnauthorizedAccessException -> ()

    let private resolveUnder (layout : LibLayout) : LoadedAssemblies * ExportedTypeResolution =
        withLayout
            layout
            (fun loggerFactory dirs facade exported assemblies ->
                TypeResolution.tryResolveTypeFromExport
                    loggerFactory
                    dirs
                    facade
                    exported
                    ImmutableArray.Empty
                    assemblies
            )

    [<Test>]
    let ``a forwarder that arrives reports the assembly that declares the type`` () : unit =
        let assemblies, outcome = resolveUnder LibLayout.DeclaresTheType

        match outcome with
        | ExportedTypeResolution.Forwarded (definingAssembly, typeInfo) ->
            definingAssembly.Name.Name |> shouldEqual libName
            typeInfo.Name |> shouldEqual "Target"
            typeInfo.Namespace |> shouldEqual "Fwd"
            // The walk's loads are kept: the caller goes on to use this context.
            assemblies.ContainsDefinition definingAssembly.Name |> shouldEqual true
        | other -> failwith $"expected the forwarder to arrive, got %O{other}"

    [<Test>]
    let ``a forwarder whose target assembly cannot be bound names that reference`` () : unit =
        let assemblies, outcome = resolveUnder LibLayout.NotPresent

        match outcome with
        | ExportedTypeResolution.AssemblyUnavailable reference ->
            // Named, so a caller can report *which* assembly went missing, as the real runtime's
            // FileNotFoundException does.
            reference.Name.Name |> shouldEqual libName
            // Nothing was invented on the way out: the reference really is still unbound.
            assemblies.TryResolveReference reference |> Option.isNone |> shouldEqual true
        | other -> failwith $"expected an unbindable target assembly, got %O{other}"

    [<Test>]
    let ``a forwarder into an assembly that lacks the type reports absence, not a failed bind`` () : unit =
        let _assemblies, outcome = resolveUnder LibLayout.DeclaresSomethingElse

        match outcome with
        | ExportedTypeResolution.TypeAbsent (TypeResolutionMiss.TopLevelTypeAbsent (searchedIn, ns, name)) ->
            // The assembly that was searched is the forwarder's *target*, not the facade: a miss
            // reported against the facade would send a caller looking in the wrong metadata.
            searchedIn |> shouldContainText libName
            searchedIn |> shouldNotContainText facadeName
            ns |> shouldEqual (Some "Fwd")
            name |> shouldEqual "Target"
        | other -> failwith $"expected the type to be absent from a bound assembly, got %O{other}"

    [<Test>]
    let ``a forwarder that arrives at a type whose base class cannot load reports that assembly`` () : unit =
        // Arriving is not enough. The caller is handed a `TypeInfo` and will immediately run pure
        // walks over its base chain — `isValueType`, `signatureTypeKind` — which cannot load and
        // fail hard on an unloaded link. So the base chain is primed before handing anything back,
        // and a bind that fails during priming has to be reported, not thrown: real .NET answers
        // `Assembly.GetType(name, throwOnError: false)` with null here, exactly as it does when the
        // forwarder's own target is missing.
        let _assemblies, outcome = resolveUnder LibLayout.BaseMissing

        match outcome with
        | ExportedTypeResolution.AssemblyUnavailable reference ->
            // The *base's* assembly, not the forwarder's target, which did bind.
            reference.Name.Name |> shouldEqual baseName
            reference.Name.Name |> shouldNotEqual libName
        | other -> failwith $"expected the base class's assembly to be reported, got %O{other}"

    [<Test>]
    let ``a forwarder whose target's base type is absent is not the same outcome as a failed bind`` () : unit =
        // Measured on .NET 10 before being implemented: with the base assembly *missing*,
        // `Assembly.GetType(name, throwOnError: false)` answers null and `true` throws
        // `FileNotFoundException`; with the base assembly present but not declaring the base type,
        // it throws `TypeLoadException` at *both* settings, because `RuntimeAssembly.GetTypeCore`
        // catches only `FileNotFoundException`. Collapsing the two into one outcome would make one
        // of those answers wrong, so they are reported separately.
        let assemblies, outcome = resolveUnder LibLayout.BaseTypeGone

        match outcome with
        | ExportedTypeResolution.BaseTypeAbsent (TypeResolutionMiss.TopLevelTypeAbsent (searchedIn, _, name)) ->
            // The assembly searched for the base class is the base's own, and what is missing from
            // it is the base class itself.
            searchedIn |> shouldContainText baseName
            name |> shouldEqual "Root"

            // That assembly did bind on the way to this failure, and the walk kept it. A guest can
            // enumerate loaded assemblies, so a context that dropped it would be a load the guest
            // watched happen and then un-happen. `searchedIn` is that assembly's own definition
            // identity, which is exactly the key the context is filed under.
            assemblies.TryByDefinitionName searchedIn |> Option.isSome |> shouldEqual true
        | other -> failwith $"expected the base type to be reported absent, got %O{other}"

    /// Both failure outcomes are new information; every existing caller goes through the wrapper,
    /// which still terminates on them because it has no way to report either.
    [<TestCase(true)>]
    [<TestCase(false)>]
    let ``the terminating wrapper still crashes, and says which failure it was``
        (targetAssemblyIsPresent : bool)
        : unit
        =
        let layout =
            if targetAssemblyIsPresent then
                LibLayout.DeclaresSomethingElse
            else
                LibLayout.NotPresent

        let message =
            withLayout
                layout
                (fun loggerFactory dirs facade exported assemblies ->
                    let exn =
                        Assert.Throws<Exception> (fun () ->
                            TypeResolution.resolveTypeFromExport
                                loggerFactory
                                dirs
                                facade
                                exported
                                ImmutableArray.Empty
                                assemblies
                            |> ignore<LoadedAssemblies * DumpedAssembly * TypeInfo<TypeDefn, TypeDefn>>
                        )

                    exn.Message
                )

        // Deliberately distinguishing the two, rather than merely asserting that something threw:
        // the wrapper reports different facts on its two arms, and a caller reading the crash has
        // to be able to tell "no such DLL" from "that DLL has no such type".
        if targetAssemblyIsPresent then
            message |> shouldContainText "does not arrive"
            message |> shouldContainText "is not declared in"
        else
            message |> shouldContainText "Could not find a readable DLL"
            message |> shouldContainText libName
