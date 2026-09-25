namespace WoofWare.PosixKernel.Test

open System
open System.Runtime.InteropServices
open NUnit.Framework
open WoofWare.PosixKernel

/// The premise every Darwin comparison against the host rests on: the host runs
/// the Darwin release whose behaviour `SimulatedUnixPlatform.macOsArm64` models.
///
/// Darwin's syscall answers move between major releases: Darwin 27 changed what
/// `rmdir` gives a path ending in "..", and what `connect` answers on a refused
/// socket. On any other major, the host-equality tests here and the compared
/// `*Darwin.cs` guests in `WoofWare.PawPrint.Test` would fail on rows that
/// PawPrint models correctly, and name neither the release nor the cause. This
/// test names both.
///
/// Only the major release is compared, which is the granularity at which a
/// change has been seen. Linux is not compared at all: `linuxX64` names a GitHub
/// runner's kernel, and CI's runner image moves that kernel on its own schedule,
/// so pinning it would fail CI whenever the image moved.
[<TestFixture>]
module TestDarwinReleaseAgainstHost =

    let private major (context : string) (release : string) : int =
        match Int32.TryParse (release.Split('.').[0]) with
        | true, value -> value
        | false, _ -> failwith $"%s{context}: release %s{release} does not start with a numeric major version"

    /// Darwin's `struct utsname`: five `char[_SYS_NAMELEN]` fields, 256 bytes each.
    [<Literal>]
    let private DarwinNameLength = 256

    [<DllImport("libc", EntryPoint = "uname", SetLastError = true)>]
    extern int private hostUname(byte[] buffer)

    /// `uname -r` on this Darwin host. Not `RuntimeInformation.OSDescription`,
    /// which on macOS reports the product version rather than the kernel's.
    let private darwinHostRelease () : string =
        let buffer = Array.zeroCreate<byte> (5 * DarwinNameLength)

        if hostUname buffer <> 0 then
            failwith $"uname failed with errno %d{Marshal.GetLastPInvokeError ()}"

        let field (index : int) : string =
            let start = index * DarwinNameLength
            let length = Array.IndexOf (buffer, 0uy, start, DarwinNameLength) - start
            Text.Encoding.ASCII.GetString (buffer, start, length)

        if field 0 <> "Darwin" then
            failwith $"expected uname's sysname to be Darwin, got %s{field 0}"

        field 2

    [<Test>]
    let ``the Darwin preset models this Darwin host's major release`` () : unit =
        match HostPlatform.flavour () with
        | Some SimulatedUnixFlavour.Darwin -> ()
        | other -> Assert.Ignore $"not a Darwin host (%O{other})"

        let hostRelease = darwinHostRelease ()

        let modelledRelease =
            SimulatedUnixPlatform.unixRelease SimulatedUnixPlatform.macOsArm64

        if
            major "host" hostRelease
            <> major "SimulatedUnixPlatform.macOsArm64" modelledRelease
        then
            failwith
                $"This host runs Darwin %s{hostRelease}, but SimulatedUnixPlatform.macOsArm64 models Darwin %s{modelledRelease}. Every comparison of the Darwin flavour against this host is measuring the wrong kernel. Re-measure the Darwin flavour here (the host-equality tests in this project, and the Guest fixtures whose Oracle is WhenHostMatchesEmulatedFlavour), model what moved, and bump macOsArm64's release."
