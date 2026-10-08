namespace WoofWare.PosixKernel.Test

open WoofWare.PosixKernel

/// Booting a machine whose first process a test describes, for a test that
/// means every value it passes to be admitted: a refusal fails the test, named
/// by the refusal's own `describe`.
[<RequireQualifiedAccess>]
module internal Launched =

    /// A process on `platform` launched with `streams`, its leader `leader`
    /// on `cpu`.
    let launch<'Task when 'Task : comparison>
        (platform : SimulatedUnixPlatform)
        (streams : Map<int, LaunchDescriptor>)
        (leader : 'Task)
        (cpu : CpuId)
        : ProcessLaunch<'Task>
        =
        ProcessLaunch.create platform streams leader cpu
        |> Configured.expectOk LaunchTableRefusal.describe

    /// `image` booted with a first process launched with `streams`, its leader
    /// `leader` on `cpu`, as `configure` configures the launch.
    let bootWith<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (configure : ProcessLaunch<'Task> -> ProcessLaunch<'Task>)
        (streams : Map<int, LaunchDescriptor>)
        (leader : 'Task)
        (cpu : CpuId)
        (image : UnixBootImage<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        let launch = launch (UnixBootImage.platform image) streams leader cpu |> configure

        UnixBootImage.boot launch image |> Configured.expectOk LaunchRefusal.describe

    /// `bootWith` with the launch as `ProcessLaunch.create` makes it.
    let boot<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (streams : Map<int, LaunchDescriptor>)
        (leader : 'Task)
        (cpu : CpuId)
        (image : UnixBootImage<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        bootWith id streams leader cpu image

    /// `ProcessLaunch.withCredentials`, which the test means to admit them.
    let credentials<'Task when 'Task : comparison>
        (credentials : Credentials)
        (launch : ProcessLaunch<'Task>)
        : ProcessLaunch<'Task>
        =
        ProcessLaunch.withCredentials credentials launch
        |> Configured.expectOk CredentialsRefusal.describe

    /// `ProcessLaunch.withUmask`, which the test means to admit it.
    let umask<'Task when 'Task : comparison>
        (umask : PermissionBits)
        (launch : ProcessLaunch<'Task>)
        : ProcessLaunch<'Task>
        =
        ProcessLaunch.withUmask umask launch
        |> Configured.expectOk UmaskRefusal.describe

    /// `UnixBootImage.withProcessId`, which the test means to admit it.
    let processId<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (pid : ProcessId)
        (image : UnixBootImage<'Task, 'Handler>)
        : UnixBootImage<'Task, 'Handler>
        =
        UnixBootImage.withProcessId pid image
        |> Configured.expectOk ProcessIdRefusal.describe

    /// `UnixBootImage.withLeaderThreadId`, which the test means to admit it.
    let leaderThreadId<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (id : uint64)
        (image : UnixBootImage<'Task, 'Handler>)
        : UnixBootImage<'Task, 'Handler>
        =
        UnixBootImage.withLeaderThreadId id image
        |> Configured.expectOk LeaderThreadIdRefusal.describe

    /// `UnixBootImage.withFileSystem`, which the test means to admit the seed.
    let fileSystem<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (createdAt : UnixTimestamp)
        (defaultOwner : InodeOwner)
        (seed : Map<DirectoryEntryName, SeedEntry>)
        (image : UnixBootImage<'Task, 'Handler>)
        : UnixBootImage<'Task, 'Handler>
        =
        UnixBootImage.withFileSystem createdAt defaultOwner seed image
        |> Configured.expectOk (sprintf "%A")

    /// `system` with the machine's record of the directories processes stand
    /// in saying where its process stands: for a test that has moved the
    /// process's current directory by hand, on a machine holding that process
    /// alone.
    let restand<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        { system with
            Machine =
                { system.Machine with
                    CurrentDirectories = Map.ofList [ system.Process.CurrentDirectoryInode, 1 ]
                }
        }
