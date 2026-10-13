namespace WoofWare.PawPrint.Test

open System.IO

/// The test host's own CoreLib, as an image for PawPrint to read in a test.
///
/// This is not the framework under test. Guests run on `FrameworkUnderTest`, and the fixtures that
/// assert a shape of CoreLib read that framework's images. The test host always runs the oldest
/// supported runtime, so on every leg this is that runtime's CoreLib. A same-image oracle, which
/// compares what PawPrint reads from an image with what the host's reflection says about the same
/// types, must read this one, because it is the image the host has loaded.
///
/// This is the only place in the test project that locates the host's CoreLib file
/// (`TestFrameworkLocatorRatchet` enforces it).
[<RequireQualifiedAccess>]
module HostImage =

    /// The path of the `System.Private.CoreLib.dll` the test host has loaded.
    let coreLibPath : string =
        match typeof<obj>.Assembly.Location with
        | null
        | "" -> failwith "The test host's CoreLib has no file location, so there is no image of it to read"
        | path -> path

    /// The test host's shared framework directory, which holds `coreLibPath`. The framework
    /// assemblies in it are the ones built with that CoreLib, so a fixture that reads `coreLibPath`
    /// and resolves further framework types along a runtime-dir list puts this directory on it.
    let frameworkDirectory : string =
        match Path.GetDirectoryName coreLibPath with
        | null -> failwith $"The test host's CoreLib path %s{coreLibPath} has no directory"
        | dir -> dir
