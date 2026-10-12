namespace WoofWare.PawPrint.Test

open WoofWare.PosixKernel

/// Booting a machine whose first process a test describes, for a test that
/// means every value it passes to be admitted: a refusal throws, which fails
/// the test.
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

        UnixBootImage.boot launch image

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
