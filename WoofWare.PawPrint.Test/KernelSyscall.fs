namespace WoofWare.PawPrint.Test

open WoofWare.PawPrint
open WoofWare.PosixKernel

/// Syscalls driven against a whole `EmulatedKernel`, for fixtures that hold one
/// rather than a bare `UnixSystem`.
///
/// One definition rather than one per fixture, because each of these is a
/// projection, a library call and a write-back, and a copy that dropped the
/// write-back would leave its fixture asserting against a state the syscall
/// never produced — silently, and only in that fixture.
[<RequireQualifiedAccess>]
module KernelSyscall =

    /// `close(2)`. A refusal crashes, as it does in the handlers that serve a
    /// guest; an errno comes back, because that is an answer.
    let close (fd : int) (kernel : EmulatedKernel) : Result<EmulatedKernel, UnixError> =
        match UnixSystem.close fd (EmulatedKernel.unix kernel) with
        | Error refusal -> failwith $"close of fd %d{fd} refused: %s{CloseRefusal.describe refusal}"
        | Ok (SyscallAnswer.Failed error, _) -> Error error
        | Ok (SyscallAnswer.Completed _, system) -> Ok (EmulatedKernel.withUnix system kernel)
