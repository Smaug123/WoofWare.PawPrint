namespace WoofWare.PawPrint.Test

open System.Runtime.InteropServices
open NUnit.Framework
open WoofWare.PawPrint

/// The kernel *this test process* is running on, in the vocabulary the emulated
/// kernel uses for the kernel it impersonates.
///
/// Tests reach for this in two quite different situations, and it is worth keeping
/// them apart. Some measure the host's own shim or libc to check a modelled fact
/// against the thing being modelled, and must skip where there is nothing to
/// measure. Others decide whether a differential comparison against the real
/// runtime is *meaningful* for a guest that describes one particular kernel.
[<RequireQualifiedAccess>]
module HostPlatform =

    /// `None` on a host whose flavour PawPrint does not model at all — Windows,
    /// or any Unix that is neither Linux nor Darwin.
    let flavour () : SimulatedUnixFlavour option =
        if RuntimeInformation.IsOSPlatform OSPlatform.OSX then
            Some SimulatedUnixFlavour.Darwin
        elif RuntimeInformation.IsOSPlatform OSPlatform.Linux then
            Some SimulatedUnixFlavour.Linux
        else
            None

    /// The preset for a flavour. Only the flavour is consumed from it, so the
    /// architecture each preset is named for need not match this host.
    let platformOf (flavour : SimulatedUnixFlavour) : SimulatedUnixPlatform =
        match flavour with
        | SimulatedUnixFlavour.Darwin -> SimulatedUnixPlatform.macOsArm64
        | SimulatedUnixFlavour.Linux -> SimulatedUnixPlatform.linuxX64

    /// Run `action` against this host's flavour, or skip the test where PawPrint
    /// models no such host. For tests that *measure* the host; a test that merely
    /// wants to know whether a comparison is valid should branch on `flavour ()`
    /// rather than skipping, so that it still asserts something everywhere.
    let onUnixHost (action : SimulatedUnixFlavour -> unit) : unit =
        match flavour () with
        | None -> Assert.Ignore $"no Unix host to measure (%s{RuntimeInformation.OSDescription})"
        | Some flavour -> action flavour
