namespace WoofWare.PosixKernel.Test

open WoofWare.PosixKernel

/// The descriptor table `UnixSystem.pipedStandardStreams` launches a process
/// with, for tests whose subject is the table rather than the launch.
[<RequireQualifiedAccess>]
module LaunchedStreams =

    /// The end of its pipe that descriptor `fd` of `UnixSystem.pipedStandardStreams`
    /// names: the read end for 0, the write end for 1 and 2.
    let private endOf (fd : int) : PipeEnd =
        match fd with
        | 0 -> PipeEnd.Read
        | 1
        | 2 -> PipeEnd.Write
        | fd -> failwith $"LaunchedStreams: descriptor %d{fd} is not one pipedStandardStreams launches"

    /// Descriptors 0, 1 and 2, each onto a pipe of its own, numbered as
    /// `UnixSystem.initial` numbers them.
    let registry : FileDescriptorRegistry =
        [ 0 ; 1 ; 2 ]
        |> List.map (fun fd -> fd, (PipeId (int64 fd), endOf fd))
        |> Map.ofList
        |> FileDescriptorRegistry.ofLaunchedPipes

    /// The description `registry` gives descriptor `fd`. The access mode is
    /// derived from the pipe end here rather than passed in, so that a test
    /// comparing against this cannot assert a mode of its own choosing.
    let description (fd : int) : OpenFileDescription =
        let pipeEnd = endOf fd

        {
            Target = OpenFileTarget.Pipe (PipeId (int64 fd), pipeEnd)
            AccessMode =
                match pipeEnd with
                | PipeEnd.Read -> FileAccessMode.ReadOnly
                | PipeEnd.Write -> FileAccessMode.WriteOnly
            NonBlocking = false
            Flock = None
            Status = OpenFileStatus.none
        }
