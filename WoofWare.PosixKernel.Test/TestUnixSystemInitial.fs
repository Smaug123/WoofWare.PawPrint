namespace WoofWare.PosixKernel.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixSystem.initial`: which fields the platform fixes, and which it
/// deliberately does not.
///
/// The distinction is the whole point of the constructor. Before it existed
/// every fixture built the record by hand, and all ten wrote Linux's `SoMaxConn`
/// and `Tmpfs` under both flavours — a Darwin machine the library's own
/// `EmulatedFileSystemType.isReportableUnder` says cannot exist.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnixSystemInitial =

    let private platforms : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

    // ------------------------------------------------------------------
    // What the platform fixes
    // ------------------------------------------------------------------

    /// Stated as literals rather than by calling the same derivation the
    /// constructor calls: a row that asked `defaultSoMaxConn` what to expect
    /// would agree with any constructor at all, including one that ignored the
    /// platform.
    [<TestCase("linux", 4096)>]
    [<TestCase("darwin", 128)>]
    let ``somaxconn is the flavour's`` (flavour : string, expected : int) : unit =
        let platform =
            match flavour with
            | "linux" -> SimulatedUnixPlatform.linuxX64
            | "darwin" -> SimulatedUnixPlatform.macOsArm64
            | other -> failwith $"unknown flavour %s{other}"

        let system : UnixSystem<int, string> = UnixSystem.initial platform
        system.Machine.SoMaxConn |> shouldEqual expected

    [<TestCase("linux")>]
    [<TestCase("darwin")>]
    let ``the filesystem type is one that flavour can report`` (flavour : string) : unit =
        let platform, expected =
            match flavour with
            | "linux" -> SimulatedUnixPlatform.linuxX64, EmulatedFileSystemType.Tmpfs
            | "darwin" -> SimulatedUnixPlatform.macOsArm64, EmulatedFileSystemType.Apfs
            | other -> failwith $"unknown flavour %s{other}"

        let system : UnixSystem<int, string> = UnixSystem.initial platform
        system.Machine.FileSystemType |> shouldEqual expected

        // The rule the pair exists to satisfy, asserted directly: a machine
        // claiming a type its flavour never mounts would hand a guest a fact no
        // real system could tell it.
        EmulatedFileSystemType.isReportableUnder
            (SimulatedUnixPlatform.flavour system.Machine.UnixPlatform)
            system.Machine.FileSystemType
        |> shouldEqual true

    [<TestCaseSource(nameof platforms)>]
    let ``the platform asked for is the platform reported`` (platform : SimulatedUnixPlatform) : unit =
        let system : UnixSystem<int, string> = UnixSystem.initial platform
        system.Machine.UnixPlatform |> shouldEqual platform

    // ------------------------------------------------------------------
    // What it deliberately does not fix
    // ------------------------------------------------------------------

    /// `UserAddressLimit` is a property of the machine's paging depth rather
    /// than of its kernel, and the ephemeral range is a sysctl either flavour
    /// can be set to anything. Both are documented as configuration rather than
    /// derivations, so this row exists to catch a later "helpful" derivation
    /// that would quietly change what a caller gets.
    [<Test>]
    let ``the machine-shaped and sysctl fields do not vary by flavour`` () : unit =
        let linux : UnixSystem<int, string> =
            UnixSystem.initial SimulatedUnixPlatform.linuxX64

        let darwin : UnixSystem<int, string> =
            UnixSystem.initial SimulatedUnixPlatform.macOsArm64

        darwin.Machine.UserAddressLimit |> shouldEqual linux.Machine.UserAddressLimit

        darwin.Machine.EphemeralPortRange
        |> shouldEqual linux.Machine.EphemeralPortRange

        darwin.Machine.NextEphemeralPort |> shouldEqual linux.Machine.NextEphemeralPort

    // ------------------------------------------------------------------
    // The pairs that must start consistent
    // ------------------------------------------------------------------

    /// The current directory and the inode it names have to agree from the
    /// start: `checkInvariants` rejects a process whose stored path is not the
    /// one that reaches its held inode, and a constructor that built the
    /// filesystem twice would produce exactly that.
    [<TestCaseSource(nameof platforms)>]
    let ``the current directory is the root of this system's own filesystem``
        (platform : SimulatedUnixPlatform)
        : unit
        =
        let system : UnixSystem<int, string> = UnixSystem.initial platform

        system.Process.CurrentDirectoryInode
        |> shouldEqual (VirtualFileSystem.root system.Machine.FileSystem)

        system.Process.CurrentDirectory |> shouldEqual AbsoluteUnixPath.root

    /// Every rule at once, which is the cheapest statement that a fresh system
    /// is a system at all.
    [<TestCaseSource(nameof platforms)>]
    let ``a fresh system is sound`` (platform : SimulatedUnixPlatform) : unit =
        let system : UnixSystem<int, string> = UnixSystem.initial platform
        UnixSystem.checkInvariants system |> shouldEqual []

    /// Only the three standard streams, which is what "before anything has
    /// happened to it" means for the descriptor table.
    [<TestCaseSource(nameof platforms)>]
    let ``only the standard streams are open`` (platform : SimulatedUnixPlatform) : unit =
        let system : UnixSystem<int, string> = UnixSystem.initial platform

        for fd, role in
            [
                0, FileDescriptorRole.StandardInput
                1, FileDescriptorRole.StandardOutput
                2, FileDescriptorRole.StandardError
            ] do
            match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
            | Some (OpenFileTarget.StandardStream actual) -> actual |> shouldEqual role
            | other -> failwith $"fd %d{fd} is %A{other}, not the standard stream %O{role}"

        FileDescriptorRegistry.tryFind 3 system.Process.FileDescriptors
        |> shouldEqual None
