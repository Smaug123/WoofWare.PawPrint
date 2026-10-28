namespace WoofWare.PawPrint.Test

open System
open System.IO
open NUnit.Framework

/// <summary>
/// Locates the netstandard2.1 build of FSharp.Core, and the runtime directory holding the
/// facades it references.
/// </summary>
/// <remarks>
/// That build is the standard fixture for base-chain-crosses-into-an-unloaded-assembly
/// regressions: it names BCL primitives (System.Object, System.ValueType) through TypeRefs
/// scoped to <c>netstandard</c>, which is a pure type-forwarding facade that nothing else in
/// a normal run drags in. So a type read out of it has a base chain that is unresolvable
/// until someone loads netstandard, which is exactly the condition every consumer of this
/// module exists to pin.
/// </remarks>
[<RequireQualifiedAccess>]
module Netstandard21FSharpCore =

    let private locate () : string =
        let nugetRoot =
            match Environment.GetEnvironmentVariable "NUGET_PACKAGES" with
            | null
            | "" -> Path.Combine (Environment.GetFolderPath Environment.SpecialFolder.UserProfile, ".nuget", "packages")
            | value -> value

        let packageRoots =
            seq {
                yield nugetRoot

                match Environment.GetEnvironmentVariable "NUGET_FALLBACK_PACKAGES" with
                | null
                | "" -> ()
                | value -> yield! value.Split (';', StringSplitOptions.RemoveEmptyEntries)
            }

        let candidate =
            packageRoots
            |> Seq.collect (fun (root : string) ->
                let fsharpCoreDir = Path.Combine (root, "fsharp.core")

                if Directory.Exists fsharpCoreDir then
                    Directory.EnumerateDirectories fsharpCoreDir
                else
                    Seq.empty
            )
            |> Seq.choose (fun versionDir ->
                let dll = Path.Combine (versionDir, "lib", "netstandard2.1", "FSharp.Core.dll")

                if File.Exists dll then Some dll else None
            )
            |> Seq.tryHead

        match candidate with
        | Some dll -> dll
        | None ->
            Assert.Ignore
                $"No FSharp.Core/*/lib/netstandard2.1/FSharp.Core.dll found in package roots %A{Seq.toList packageRoots}; skipping netstandard2.1 base-chain regression."

            failwith "unreachable"

    /// Path to a netstandard2.1 FSharp.Core.dll. Forces an <c>Assert.Ignore</c> if neither the
    /// NuGet cache nor its fallback folders hold one, so a checkout without it still passes.
    let path : string Lazy = System.Lazy<_>.Create locate

    let corelibPath : string = HostImage.coreLibPath

    /// netstandard.dll and every BCL facade these tests may need ship alongside
    /// System.Private.CoreLib in every Microsoft.NETCore.App runtime we test against; a
    /// directory-scoped loader can resolve them on demand.
    let runtimeDir : string = Path.GetDirectoryName corelibPath

    let assertNetstandardAvailable () : unit =
        let candidate = Path.Combine (runtimeDir, "netstandard.dll")

        if not (File.Exists candidate) then
            Assert.Ignore $"netstandard.dll not found next to corelib at %s{candidate}"

    /// True if the load context holds netstandard (under any version).
    let isLoaded (definitionNames : string seq) : bool =
        definitionNames
        |> Seq.exists (fun (n : string) -> n.StartsWith ("netstandard,", StringComparison.OrdinalIgnoreCase))
