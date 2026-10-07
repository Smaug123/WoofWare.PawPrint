namespace WoofWare.PawPrint.Test

open WoofWare.PosixKernel

/// Booting a machine whose first process a test describes, for a test that
/// means every value it passes to be admitted: a refusal fails the test, named
/// by the refusal's own `describe`.
[<RequireQualifiedAccess>]
module internal Launched =

    /// The value a setter returned, for a value the test means it to admit.
    let expectOk<'Value, 'Refusal> (describe : 'Refusal -> string) (result : Result<'Value, 'Refusal>) : 'Value =
        match result with
        | Ok value -> value
        | Error refusal ->
            failwith $"test bug: the kernel refused a value the test means it to admit: %s{describe refusal}"

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
        |> expectOk LaunchTableRefusal.describe

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

        UnixBootImage.boot launch image |> expectOk LaunchRefusal.describe

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
        |> expectOk CredentialsRefusal.describe
