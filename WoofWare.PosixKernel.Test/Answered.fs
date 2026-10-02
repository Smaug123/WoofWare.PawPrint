namespace WoofWare.PosixKernel.Test

open WoofWare.PosixKernel

/// The syscalls whose only refusals are rows a filesystem the caller owns
/// throughout never reaches, answered: a refusal fails the test that made the
/// call, naming it.
[<RequireQualifiedAccess>]
module Answered =

    let openPath<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (flags : OpenFlags)
        (path : UnixPath)
        (mode : int)
        (system : UnixSystem<'Task, 'Handler>)
        : SyscallAnswer * UnixSystem<'Task, 'Handler>
        =
        match UnixNamespace.openPath flags (PathArg.ofPath path) mode system with
        | Ok answer -> answer
        | Error refusal -> failwith $"open(%O{path}) was refused: %s{OpenRefusal.describe refusal}"

    let unlink<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : UnixPath)
        (system : UnixSystem<'Task, 'Handler>)
        : SyscallAnswer * UnixSystem<'Task, 'Handler>
        =
        match UnixNamespace.unlink (PathArg.ofPath path) system with
        | Ok answer -> answer
        | Error refusal -> failwith $"unlink(%O{path}) was refused: %s{StickyRefusal.describe refusal}"

    let rmdir<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : UnixPath)
        (system : UnixSystem<'Task, 'Handler>)
        : SyscallAnswer * UnixSystem<'Task, 'Handler>
        =
        match UnixNamespace.rmdir (PathArg.ofPath path) system with
        | Ok answer -> answer
        | Error refusal -> failwith $"rmdir(%O{path}) was refused: %s{StickyRefusal.describe refusal}"

    /// The answer of a rule or verdict whose refusals the test that asked it
    /// cannot reach, failing that test on a refusal.
    let unrefused<'answer, 'refusal> (result : Result<'answer, 'refusal>) : 'answer =
        match result with
        | Ok answer -> answer
        | Error refusal -> failwith $"expected an answer, but it was refused: %A{refusal}"
