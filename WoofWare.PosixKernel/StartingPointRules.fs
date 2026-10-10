namespace WoofWare.PosixKernel

/// The directory a relative path given to a `*at` syscall starts from: its
/// `dirfd` argument, decoded.
[<RequireQualifiedAccess>]
type internal AtDirectory =
    /// `AT_FDCWD`: the process's current directory.
    | CurrentDirectory
    /// Any other value, which the call looks up in the descriptor table if it
    /// needs a starting directory at all.
    | Descriptor of fd : int

[<RequireQualifiedAccess>]
module AtDirectory =

    // `<fcntl.h>`'s numbering, measured by `at-dirfd.c` on Linux 6.18.5 and
    // Darwin 27.0.
    let private linuxAtFdCwd : int = -100
    let private darwinAtFdCwd : int = -2

    /// `AT_FDCWD` in `flavour`'s numbering: -100 on Linux, -2 on Darwin.
    let atFdCwd (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> linuxAtFdCwd
        | SimulatedUnixFlavour.Darwin -> darwinAtFdCwd

    /// What a `*at` syscall's raw `dirfd` names under `flavour`: its own
    /// `AT_FDCWD`, or a descriptor. Each flavour's `AT_FDCWD` is merely a
    /// descriptor number nothing holds under the other.
    let internal decode (flavour : SimulatedUnixFlavour) (dirfd : int) : AtDirectory =
        if dirfd = atFdCwd flavour then
            AtDirectory.CurrentDirectory
        else
            AtDirectory.Descriptor dirfd

/// What a `*at` syscall does with an empty path, once its flag word is read.
[<RequireQualifiedAccess>]
type EmptyPathMeaning =
    /// The call is about the object `dirfd` names, which can be a regular file
    /// as well as a directory: Linux's `AT_EMPTY_PATH`.
    | NamesStartingPoint
    /// The call walks the empty path, which fails as the flavour's
    /// `StartingPointRules.EmptyPath` says.
    | Walked

/// When a walk of the empty path fails, relative to looking `dirfd` up.
[<RequireQualifiedAccess>]
type EmptyPathRule =
    /// ENOENT, before `dirfd` is looked at, so a `dirfd` that names nothing
    /// does not matter. It is the copy-in's answer, so it also comes before
    /// `openat` allocates the descriptor it would return. Linux.
    | NoSuchEntryBeforeDescriptor
    /// ENOENT, but only once `dirfd` has been found to name a directory: a
    /// `dirfd` naming nothing, or naming something other than a directory,
    /// fails as it would for any relative path. Darwin.
    | NoSuchEntryAfterDescriptor

/// What a `*at` syscall answers when `dirfd` is open but names no directory,
/// and the call would start a walk from it.
[<RequireQualifiedAccess>]
type NonDirectoryDescriptorRule =
    /// ENOTDIR, whatever the descriptor names. Linux.
    | NotADirectory
    /// ENOTDIR for a descriptor on a filesystem object (a regular file or a
    /// device), and ENOTSUP for one on anything else (a pipe, a socket or an
    /// event queue). Darwin.
    | NotSupportedOffTheFileSystem

/// How a flavour's `*at` syscalls find the directory a path starts from.
///
/// Shared by every such call: measured, nineteen of them, crossed with
/// thirteen kinds of `dirfd`, agree on both fields within each flavour. In
/// every call the path is copied in first, and a rooted path never looks at
/// `dirfd` at all.
type StartingPointRules =
    {
        /// When the empty path's ENOENT comes.
        EmptyPath : EmptyPathRule
        /// What an open `dirfd` naming no directory answers.
        NonDirectory : NonDirectoryDescriptorRule
    }

[<RequireQualifiedAccess>]
module internal StartingPointRules =

    /// What a `*at` syscall answers under `rules` for a `dirfd` open on
    /// `target`, which names no directory.
    ///
    /// Throws for a directory, which is a place to start rather than a
    /// failure.
    let nonDirectoryAnswer (rules : StartingPointRules) (target : OpenFileTarget) : UnixError =
        match target with
        | OpenFileTarget.Directory (inode, _) ->
            failwith
                $"StartingPointRules.nonDirectoryAnswer: the descriptor names directory inode %O{inode}, which is where a walk starts rather than a failure (this is a bug in this library)."
        | OpenFileTarget.File _
        | OpenFileTarget.CharacterDevice _ -> UnixError.ENOTDIR
        | OpenFileTarget.Pipe _
        | OpenFileTarget.Socket _
        | OpenFileTarget.Epoll _
        | OpenFileTarget.Kqueue _ ->
            match rules.NonDirectory with
            | NonDirectoryDescriptorRule.NotADirectory -> UnixError.ENOTDIR
            | NonDirectoryDescriptorRule.NotSupportedOffTheFileSystem -> UnixError.ENOTSUP
