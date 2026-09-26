namespace WoofWare.PosixKernel.Test

open WoofWare.PosixKernel

/// Opening and reading a directory the way `opendir(3)` and `readdir(3)` do it
/// over the library's primitives, for fixtures whose subject is something else.
[<RequireQualifiedAccess>]
module DirectoryReading =

    /// `opendir(3)`'s open: `O_RDONLY|O_DIRECTORY|O_CLOEXEC`.
    let flags : OpenFlags =
        {
            Access = FileAccessMode.ReadOnly
            Create = false
            Exclusive = false
            Truncate = false
            NoFollow = false
            CloseOnExec = true
            Synchronous = false
            Directory = true
        }

    /// Open `path` as `opendir` does, answering the descriptor or the errno.
    let openDirectory<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (path : UnixPath)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<int, UnixError> * UnixSystem<'Task, 'Handler>
        =
        match UnixNamespace.openPath flags path 0 system with
        | SyscallAnswer.Completed fd, system -> Ok (int fd), system
        | SyscallAnswer.Failed error, system -> Error error, system

    /// Every entry `fd` yields until end-of-directory. A failure or a refusal
    /// fails the fixture, as does a position that never reaches the end.
    let drain<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (fd : int)
        (system : UnixSystem<'Task, 'Handler>)
        : DirectoryRecord list * UnixSystem<'Task, 'Handler>
        =
        let rec go (fuel : int) (acc : DirectoryRecord list) (system : UnixSystem<'Task, 'Handler>) =
            if fuel <= 0 then
                failwith $"fd %d{fd} did not reach end-of-directory after %d{List.length acc} entries"

            match UnixNamespace.readDirectoryEntry fd system with
            | Ok (ReadDirectoryAnswer.EndOfDirectory, system) -> List.rev acc, system
            | Ok (ReadDirectoryAnswer.Entry record, system) -> go (fuel - 1) (record :: acc) system
            | Ok (ReadDirectoryAnswer.Failed error, _) -> failwith $"reading fd %d{fd} failed with %O{error}"
            | Error refusal -> failwith $"reading fd %d{fd} was refused: %s{ReadDirectoryRefusal.describe refusal}"

        go 10000 [] system
