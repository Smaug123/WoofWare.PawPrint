namespace WoofWare.PosixKernel.Test

open WoofWare.PosixKernel

/// The syscalls whose only refusals are rows a filesystem the caller owns
/// throughout never reaches, answered: a refusal fails the test that made the
/// call, naming it.
[<RequireQualifiedAccess>]
module internal Answered =

    let openPath<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (flags : OpenFlags)
        (path : UnixPath)
        (mode : int)
        (system : UnixSystem<'Task, 'Handler>)
        : SyscallAnswer * UnixSystem<'Task, 'Handler>
        =
        match OpenFlagWords.openPath flags (PathArg.ofPath path) mode system with
        | Ok answer -> answer
        | Error refusal -> failwith $"open(%O{path}) was refused: %s{OpenRefusal.describe refusal}"

    let unlink<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : UnixPath)
        (system : UnixSystem<'Task, 'Handler>)
        : SyscallAnswer * UnixSystem<'Task, 'Handler>
        =
        match UnixNamespace.unlink (PathArg.ofPath path) system with
        | Ok answer -> answer
        | Error refusal -> failwith $"unlink(%O{path}) was refused: %s{RemovalRefusal.describe refusal}"

    let rmdir<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : UnixPath)
        (system : UnixSystem<'Task, 'Handler>)
        : SyscallAnswer * UnixSystem<'Task, 'Handler>
        =
        match UnixNamespace.rmdir (PathArg.ofPath path) system with
        | Ok answer -> answer
        | Error refusal -> failwith $"rmdir(%O{path}) was refused: %s{RemovalRefusal.describe refusal}"

    let mkdir<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : PathArgumentBytes)
        (mode : int)
        (system : UnixSystem<'Task, 'Handler>)
        : SyscallAnswer * UnixSystem<'Task, 'Handler>
        =
        match UnixNamespace.mkdir path mode system with
        | Ok answer -> answer
        | Error refusal -> failwith $"mkdir was refused: %s{PathRefusal.describe refusal}"

    let chdir<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : PathArgumentBytes)
        (system : UnixSystem<'Task, 'Handler>)
        : SyscallAnswer * UnixSystem<'Task, 'Handler>
        =
        match UnixPathResolution.chdir path system with
        | Ok answer -> answer
        | Error refusal -> failwith $"chdir was refused: %s{PathRefusal.describe refusal}"

    let statfs<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : PathArgumentBytes)
        (system : UnixSystem<'Task, 'Handler>)
        : FileSystemStatisticsAnswer
        =
        match UnixPathResolution.statfs path system with
        | Ok answer -> answer
        | Error refusal -> failwith $"statfs was refused: %s{PathRefusal.describe refusal}"

    /// A path walk's outcome as the errno a syscall answers, failing the test
    /// on a refusal: for a walk through a filesystem the test built, which
    /// mounts nothing this kernel refuses to walk.
    let errno<'a> (result : Result<'a, PathFailure>) : Result<'a, UnixError> =
        match result with
        | Ok value -> Ok value
        | Error (PathFailure.Errno error) -> Error error
        | Error (PathFailure.Refused refusal) ->
            failwith $"expected a walk to answer, but it was refused: %s{PathRefusal.describe refusal}"

    /// The answer of a rule or verdict whose refusals the test that asked it
    /// cannot reach, failing that test on a refusal.
    let unrefused<'answer, 'refusal> (result : Result<'answer, 'refusal>) : 'answer =
        match result with
        | Ok answer -> answer
        | Error refusal -> failwith $"expected an answer, but it was refused: %A{refusal}"

    /// `dup(2)`, which must not reach the descriptor bound.
    let dup<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (system : UnixSystem<'Task, 'Handler>)
        : SyscallAnswer * UnixSystem<'Task, 'Handler>
        =
        match UnixDescriptor.dup fd system with
        | Ok answer -> answer
        | Error refusal -> failwith $"dup(%d{fd}) was refused: %s{DescriptorLimitRefusal.describe refusal}"
