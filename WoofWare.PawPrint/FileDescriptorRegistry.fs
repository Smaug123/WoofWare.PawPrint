namespace WoofWare.PawPrint

/// Underlying "open file description" that a file descriptor points at.
/// POSIX distinguishes between fds (per-process table entries) and OFDs
/// (the underlying object the fd references). `dup` allocates a fresh fd
/// pointing at the same OFD; that's why we keep this concept explicit even
/// though the current set of roles is closed and small.
[<RequireQualifiedAccess>]
type FileDescriptorRole =
    | StandardInput
    | StandardOutput
    | StandardError

type FileDescriptorEntry =
    {
        Role : FileDescriptorRole
        /// True if PawPrint owns the underlying resource and `close` should
        /// release it. False for fds 0/1/2 that the simulated process
        /// inherits at startup (the host owns those, as real Unix would).
        OwnsResource : bool
    }

/// In-memory model of a Unix per-process file descriptor table. Stdin/
/// stdout/stderr (fds 0/1/2) are pre-seeded at machine boot, matching the
/// kernel's behaviour of populating these slots at `exec` time. `dup`
/// allocates the lowest non-negative integer not currently in use,
/// matching `dup(2)`.
type FileDescriptorRegistry =
    private
        {
            Entries : Map<int, FileDescriptorEntry>
        }

[<RequireQualifiedAccess>]
type FileDescriptorDupError =
    /// The supplied fd is not a live entry in the table. `dup(2)` reports
    /// this as `EBADF`; the SystemNative_Dup handler translates this into
    /// a -1 return and `LastSystemError = EBADF`.
    | BadFd

[<RequireQualifiedAccess>]
type FileDescriptorCloseError =
    /// The supplied fd is not a live entry in the table. `close(2)` reports
    /// this as `EBADF`.
    | BadFd

[<RequireQualifiedAccess>]
module FileDescriptorRegistry =
    let private stdinEntry : FileDescriptorEntry =
        {
            Role = FileDescriptorRole.StandardInput
            OwnsResource = false
        }

    let private stdoutEntry : FileDescriptorEntry =
        {
            Role = FileDescriptorRole.StandardOutput
            OwnsResource = false
        }

    let private stderrEntry : FileDescriptorEntry =
        {
            Role = FileDescriptorRole.StandardError
            OwnsResource = false
        }

    /// Empty registry seeded with stdin (fd 0), stdout (fd 1), stderr
    /// (fd 2). The host owns these descriptors, so `OwnsResource = false`.
    let initial : FileDescriptorRegistry =
        {
            Entries =
                Map.empty
                |> Map.add 0 stdinEntry
                |> Map.add 1 stdoutEntry
                |> Map.add 2 stderrEntry
        }

    let tryFind (fd : int) (registry : FileDescriptorRegistry) : FileDescriptorEntry option =
        Map.tryFind fd registry.Entries

    /// Lowest non-negative integer not currently used as a key in `entries`.
    /// O(n) in the number of live fds, which is fine: process fd tables are
    /// small (typically a handful, rarely more than a few hundred), and the
    /// interpreter is not a performance-critical workload.
    let private lowestFree (entries : Map<int, FileDescriptorEntry>) : int =
        let rec scan (candidate : int) =
            if Map.containsKey candidate entries then
                scan (candidate + 1)
            else
                candidate

        scan 0

    /// Mirrors `dup(2)`: allocate the lowest non-negative fd not in use,
    /// sharing the role of `oldFd`. A duplicate is independent of its
    /// source for close purposes — the new entry has `OwnsResource = true`
    /// because the simulated process is the one that minted it (the host's
    /// underlying stream is not duplicated; only the table entry is). When
    /// `oldFd` is not a live entry, returns `Error BadFd`, matching the
    /// `EBADF` behaviour of `dup(2)`.
    let dup
        (oldFd : int)
        (registry : FileDescriptorRegistry)
        : Result<int * FileDescriptorRegistry, FileDescriptorDupError>
        =
        match Map.tryFind oldFd registry.Entries with
        | None -> Error FileDescriptorDupError.BadFd
        | Some source ->
            let newFd = lowestFree registry.Entries

            let newEntry =
                {
                    Role = source.Role
                    OwnsResource = true
                }

            let entries = Map.add newFd newEntry registry.Entries

            Ok (
                newFd,
                {
                    Entries = entries
                }
            )

    /// Remove an entry from the table. Mirrors `close(2)`: returns
    /// `Error BadFd` (= `EBADF`) when `fd` is not currently live. Wired
    /// into the interpreter via the `SystemNative_Close` handler in
    /// `NativeSystemNative.fs`; the in-house property tests drive
    /// close+dup cycles directly against this function to exercise the
    /// `lowestFree` invariant against the gap structure that close
    /// produces.
    let close
        (fd : int)
        (registry : FileDescriptorRegistry)
        : Result<FileDescriptorRegistry, FileDescriptorCloseError>
        =
        if Map.containsKey fd registry.Entries then
            Ok
                { registry with
                    Entries = Map.remove fd registry.Entries
                }
        else
            Error FileDescriptorCloseError.BadFd
