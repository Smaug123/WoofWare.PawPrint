namespace WoofWare.PosixKernel.Test

open System
open System.Runtime.InteropServices
open System.Text
open System.Text.RegularExpressions
open NUnit.Framework
open WoofWare.PosixKernel

/// Reading a Linux kernel's version out of the release string `uname -r`
/// prints.
[<RequireQualifiedAccess>]
module LinuxRelease =

    /// The version a release such as `6.17.0-1022-azure` or `6.18.5` begins with,
    /// or `None` for one that does not begin `major.minor`. A missing patch
    /// level is 0, as the kernel's own Makefile prints a `SUBLEVEL` of 0.
    let version (release : string) : LinuxKernelVersion option =
        let m = Regex.Match (release, @"^([0-9]+)\.([0-9]+)(?:\.([0-9]+))?")

        if not m.Success then
            None
        else
            let part (i : int) : uint32 =
                if m.Groups.[i].Success then
                    UInt32.Parse m.Groups.[i].Value
                else
                    0u

            Some
                {
                    Major = part 1
                    Minor = part 2
                    Patch = part 3
                }

/// The kernel *this test process* is running on, in the vocabulary the emulated
/// kernel uses for the kernel it impersonates.
///
/// Tests reach for this in two quite different situations, and it is worth keeping
/// them apart. Some measure the host's own libc, or .NET's `System.Native`
/// library, to check a modelled fact against the thing being modelled, and must
/// skip where there is nothing to measure. Others decide whether a comparison
/// against this host is *meaningful* for a model that describes one particular
/// kernel.
[<RequireQualifiedAccess>]
module HostPlatform =

    [<DllImport("libc", EntryPoint = "uname", SetLastError = true)>]
    extern int private hostUname(byte[] buffer)

    /// `None` on a host whose flavour the library does not model at all —
    /// Windows, or any Unix that is neither Linux nor Darwin.
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

    /// Run `action` against this host's flavour, or skip the test where the
    /// library models no such host. For tests that *measure* the host; a test
    /// that merely wants to know whether a comparison is valid should branch on
    /// `flavour ()` rather than skipping, so that it still asserts something
    /// everywhere.
    let onUnixHost (action : SimulatedUnixFlavour -> unit) : unit =
        match flavour () with
        | None -> Assert.Ignore $"no Unix host to measure (%s{RuntimeInformation.OSDescription})"
        | Some flavour -> action flavour

    /// This host process's architecture, or `None` for one the library does not
    /// model.
    let architecture () : SimulatedUnixArchitecture option =
        match RuntimeInformation.ProcessArchitecture with
        | Architecture.X64 -> Some SimulatedUnixArchitecture.X64
        | Architecture.Arm64 -> Some SimulatedUnixArchitecture.Arm64
        | _ -> None

    /// The preset built for this flavour and architecture, or `None` where the
    /// library has none.
    let presetFor
        (flavour : SimulatedUnixFlavour)
        (architecture : SimulatedUnixArchitecture)
        : SimulatedUnixPlatform option
        =
        match flavour, architecture with
        | SimulatedUnixFlavour.Linux, SimulatedUnixArchitecture.X64 -> Some SimulatedUnixPlatform.linuxX64
        | SimulatedUnixFlavour.Linux, SimulatedUnixArchitecture.Arm64 -> Some SimulatedUnixPlatform.linuxArm64
        | SimulatedUnixFlavour.Darwin, SimulatedUnixArchitecture.Arm64 -> Some SimulatedUnixPlatform.macOsArm64
        | SimulatedUnixFlavour.Darwin, SimulatedUnixArchitecture.X64 -> None

    /// Run `action` against the preset for this host's flavour and architecture,
    /// for tests that measure a fact the architecture decides. Skips where the
    /// library models no such host; whether the host's *page size* is the
    /// preset's is `TestArchitectureAgainstHost`'s question, not a reason to skip.
    let onUnixHostPreset (action : SimulatedUnixPlatform -> unit) : unit =
        onUnixHost (fun flavour ->
            match architecture () with
            | None -> Assert.Ignore $"no architecture the library models (%O{RuntimeInformation.ProcessArchitecture})"
            | Some architecture ->

            match presetFor flavour architecture with
            | None -> Assert.Ignore $"the library has no %O{flavour} %O{architecture} platform to compare against"
            | Some platform -> action platform
        )

    /// This Linux host's `uname -r`. Linux's `struct utsname` is six 65-byte
    /// fields, and the release is the third.
    let private linuxRelease () : string =
        let buffer = Array.zeroCreate<byte> (6 * 65)

        if hostUname buffer <> 0 then
            failwith $"uname failed with errno %d{Marshal.GetLastPInvokeError ()}"

        let field = ReadOnlySpan<byte> (buffer, 2 * 65, 65)
        let length = field.IndexOf 0uy

        if length < 0 then
            failwith "uname's release field is not NUL-terminated"

        Encoding.ASCII.GetString (field.Slice (0, length))

    /// Run `action` against a model of the kernel this host is running: the
    /// preset for its flavour and architecture, but running this host's own
    /// Linux version and reporting its own release. For tests of a fact that
    /// changed between kernel versions, which must hold on whichever kernel the
    /// suite runs on. Skips where `onUnixHostPreset` does.
    let onUnixHostKernel (action : SimulatedUnixPlatform -> unit) : unit =
        onUnixHostPreset (fun preset ->
            match SimulatedUnixPlatform.kernel preset with
            | SimulatedUnixKernel.Darwin -> action preset
            | SimulatedUnixKernel.Linux _ ->

            let release = linuxRelease ()

            match LinuxRelease.version release with
            | None -> failwith $"this host's release %s{release} does not begin with a Linux version"
            | Some version ->
                SimulatedUnixPlatform.createOrFail
                    "HostPlatform.onUnixHostKernel"
                    (SimulatedUnixKernel.Linux version)
                    (SimulatedUnixPlatform.architecture preset)
                    (SimulatedUnixPlatform.pageSize preset)
                    release
                |> action
        )
