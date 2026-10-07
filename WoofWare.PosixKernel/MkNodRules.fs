namespace WoofWare.PosixKernel

/// The file-type field of a `mknod(2)` mode word, `mode & S_IFMT`, which both
/// flavours number alike. Only those four bits are read for the type, and
/// only the low twelve for the permissions; every other bit is ignored.
[<RequireQualifiedAccess>]
type NodeTypeField =
    /// 0: no type named. Linux makes a regular file of it.
    | Unset
    /// `S_IFIFO`, 0o010000.
    | Fifo
    /// `S_IFCHR`, 0o020000.
    | CharacterDevice
    /// `S_IFDIR`, 0o040000.
    | Directory
    /// `S_IFBLK`, 0o060000.
    | BlockDevice
    /// `S_IFREG`, 0o100000.
    | Regular
    /// `S_IFLNK`, 0o120000.
    | SymbolicLink
    /// `S_IFSOCK`, 0o140000.
    | Socket
    /// Any other value of the field, as it stands in the mode word: 0o030000, 0o050000, 0o070000,
    /// 0o110000, 0o130000, 0o150000, 0o160000 (Darwin's `S_IFWHT`) or 0o170000.
    | Unnamed of bits : int

[<RequireQualifiedAccess>]
module NodeTypeField =
    /// The type field of a raw `mknod(2)` mode word.
    let ofMode (mode : int) : NodeTypeField =
        // Measured by `mknodat-rules.c` (WIDE) on Linux 6.18.5 and Darwin
        // 27.0: bits above the sixteen of `mode_t` change nothing on either.
        match mode &&& 0o170000 with
        | 0 -> NodeTypeField.Unset
        | 0o010000 -> NodeTypeField.Fifo
        | 0o020000 -> NodeTypeField.CharacterDevice
        | 0o040000 -> NodeTypeField.Directory
        | 0o060000 -> NodeTypeField.BlockDevice
        | 0o100000 -> NodeTypeField.Regular
        | 0o120000 -> NodeTypeField.SymbolicLink
        | 0o140000 -> NodeTypeField.Socket
        | bits -> NodeTypeField.Unnamed bits

/// How a flavour's `mknod(2)` treats the type of node it is asked for, and so
/// what it does before it reads its path.
[<RequireQualifiedAccess>]
type MkNodRules =
    /// Linux's. The type is screened first, ahead of the path's copy-in and
    /// the `dirfd`, whoever asks: a directory is EPERM, and a field naming
    /// no type it makes (a symbolic link among them) is EINVAL. Any other
    /// type goes on to walk its path, never following a final link, under
    /// `trailingSeparator`, and is created as `open(O_CREAT|O_EXCL)` creates;
    /// a character or block device needs privilege only once its name has
    /// been found free and its directory writable.
    | TypeBeforePath of trailingSeparator : TrailingSeparatorPolicy
    /// Darwin's. A FIFO is `mkfifo(2)`'s business; any other type, a regular
    /// file's included, is EPERM before the path is copied in for a caller
    /// without privilege.
    | PrivilegeBeforePath

/// What Linux's `mknod(2)` goes on to make once the type field has passed
/// its screen.
[<RequireQualifiedAccess>]
type MkNodNode =
    /// `S_IFREG`, or a type field of 0.
    | RegularFile
    /// `S_IFIFO`.
    | Fifo
    /// `S_IFSOCK`.
    | Socket
    /// `S_IFCHR`, standing for the raw device number `dev`.
    | CharacterDevice of dev : uint32
    /// `S_IFBLK`, standing for the raw device number `dev`.
    | BlockDevice of dev : uint32

/// Why this kernel will not answer a `mknod(2)` or `mknodat(2)`.
[<RequireQualifiedAccess>]
type MkNodRefusal =
    /// This kernel will not resolve the pathname.
    | Path of refusal : PathRefusal
    /// The call would make a FIFO, which this library does not model. On
    /// Linux every check that would fail the call has already passed. On
    /// Darwin the call is `mkfifo(2)`, whose rules this library has not
    /// measured, and it is refused before the path is read.
    | Fifo
    /// The call would make a socket inode, which this library does not model.
    /// Every check that would fail the call has already passed.
    | Socket
    /// The call would make a character device node for the raw device number
    /// `dev`: by a privileged caller, or, for `dev` 0, which Linux makes for
    /// anyone as an overlay filesystem's whiteout, by any caller. Every check
    /// that would fail the call has already passed.
    | CharacterDevice of dev : uint32
    /// The call would make a block device node for the raw device number
    /// `dev`, by a privileged caller. Every check that would fail the call
    /// has already passed.
    | BlockDevice of dev : uint32
    /// A privileged caller on Darwin asked for a type other than a FIFO,
    /// which has not been measured. Refused before the path is read.
    | UnmeasuredPrivilegedCaller

[<RequireQualifiedAccess>]
module MkNodRefusal =
    /// What this kernel knows about why it will not answer. A client adds which
    /// entry point asked, and with which pathname.
    let describe (refusal : MkNodRefusal) : string =
        match refusal with
        | MkNodRefusal.Path refusal -> PathRefusal.describe refusal
        | MkNodRefusal.Fifo -> "the call would make a FIFO, which this library does not model."
        | MkNodRefusal.Socket -> "the call would make a socket inode, which this library does not model."
        | MkNodRefusal.CharacterDevice dev ->
            $"the call would make a character device node for device 0x%x{dev}, which this library does not model."
        | MkNodRefusal.BlockDevice dev ->
            $"the call would make a block device node for device 0x%x{dev}, which this library does not model."
        | MkNodRefusal.UnmeasuredPrivilegedCaller ->
            "a privileged caller on Darwin asked mknod for a type other than a FIFO, which has not been measured."

/// What a `mknod(2)` does before it reads its path.
[<RequireQualifiedAccess>]
type MkNodScreen =
    /// Answer with this errno. The path is never read.
    | Fails of error : UnixError
    /// This kernel will not answer. The path is never read.
    | Refused of refusal : MkNodRefusal
    /// Copy the path in and walk it under `trailingSeparator`, to make `node`
    /// if `MkNodRules.verdict` says so.
    | Walks of node : MkNodNode * trailingSeparator : TrailingSeparatorPolicy

/// What a `mknod(2)` does once its path has resolved.
[<RequireQualifiedAccess>]
type MkNodVerdict =
    /// Answer the caller with this errno.
    | Refuse of error : UnixError
    /// Bind a new empty regular file under `name` in `directory`.
    | CreateRegularFile of directory : InodeNumber * name : DirectoryEntryName
    /// This kernel will not answer.
    | Refused of refusal : MkNodRefusal

[<RequireQualifiedAccess>]
module MkNodRules =
    /// What a `mknod(2)` under `rules`, by a caller with `credentials`, does
    /// with the raw `mode` and `dev` before it reads its path: only the type
    /// field of `mode` is read here (`NodeTypeField.ofMode`).
    let screen (rules : MkNodRules) (credentials : Credentials) (mode : int) (dev : uint32) : MkNodScreen =
        // Measured by `mknodat-rules.c` (ORDER, TYPE) on Linux 6.18.5, root
        // and uid 1000, and Darwin 27.0, uid 501: each answer here beats a
        // NULL path, an unreadable one, PATH_MAX bytes with no NUL, the empty
        // path, -1 and a file as `dirfd`, an existing name and an unwritable
        // directory.
        let field = NodeTypeField.ofMode mode

        match rules with
        | MkNodRules.TypeBeforePath trailingSeparator ->
            let walks (node : MkNodNode) =
                MkNodScreen.Walks (node, trailingSeparator)

            match field with
            | NodeTypeField.Unset
            | NodeTypeField.Regular -> walks MkNodNode.RegularFile
            | NodeTypeField.Fifo -> walks MkNodNode.Fifo
            | NodeTypeField.Socket -> walks MkNodNode.Socket
            | NodeTypeField.CharacterDevice -> walks (MkNodNode.CharacterDevice dev)
            | NodeTypeField.BlockDevice -> walks (MkNodNode.BlockDevice dev)
            | NodeTypeField.Directory -> MkNodScreen.Fails UnixError.EPERM
            | NodeTypeField.SymbolicLink
            | NodeTypeField.Unnamed _ -> MkNodScreen.Fails UnixError.EINVAL
        | MkNodRules.PrivilegeBeforePath ->
            match field with
            | NodeTypeField.Fifo -> MkNodScreen.Refused MkNodRefusal.Fifo
            | NodeTypeField.Unset
            | NodeTypeField.Regular
            | NodeTypeField.Socket
            | NodeTypeField.CharacterDevice
            | NodeTypeField.BlockDevice
            | NodeTypeField.Directory
            | NodeTypeField.SymbolicLink
            | NodeTypeField.Unnamed _ ->
                match Credentials.privilege credentials with
                | CallerPrivilege.Unprivileged -> MkNodScreen.Fails UnixError.EPERM
                | CallerPrivilege.Privileged -> MkNodScreen.Refused MkNodRefusal.UnmeasuredPrivilegedCaller

    /// What a Linux `mknod(2)` of `node`, by a caller with `credentials`,
    /// owes, given what `CreatingOpenRules.verdict` decided for an
    /// `O_CREAT|O_EXCL` open of the path it walked.
    ///
    /// Every refusal that verdict gives is `mknod`'s too, in its order: an
    /// existing name or a path naming no final component is EEXIST, a free
    /// name with a trailing separator is ENOENT, an orphaned directory is
    /// ENOENT, and an unwritable directory EACCES. Only then does the type
    /// matter: a regular file is made, a character or block device by a
    /// caller without privilege is EPERM, and anything else is a node this
    /// library does not model.
    let verdict (credentials : Credentials) (node : MkNodNode) (creation : CreatingOpenVerdict) : MkNodVerdict =
        // Measured by `mknodat-rules.c` (PATH, PATHTYPE, TYPE) on Linux 6.18.5,
        // root and uid 1000, ext4 and tmpfs: every type gets the same errno as
        // a regular file wherever a regular file is not made, and a device's
        // EPERM comes after EEXIST, ENOENT and EACCES. Linux lets anyone make
        // a character device numbered 0, an overlay filesystem's whiteout.
        match creation with
        | CreatingOpenVerdict.Refuse error -> MkNodVerdict.Refuse error
        | CreatingOpenVerdict.OpenExisting inode ->
            failwith
                $"MkNodRules.verdict: an O_CREAT|O_EXCL verdict said to open existing inode %O{inode}, which it never says (this is a bug in this library)."
        | CreatingOpenVerdict.Create (directory, name) ->

        match node with
        | MkNodNode.RegularFile -> MkNodVerdict.CreateRegularFile (directory, name)
        | MkNodNode.Fifo -> MkNodVerdict.Refused MkNodRefusal.Fifo
        | MkNodNode.Socket -> MkNodVerdict.Refused MkNodRefusal.Socket
        | MkNodNode.CharacterDevice dev ->
            match Credentials.privilege credentials with
            | CallerPrivilege.Unprivileged when dev <> 0u -> MkNodVerdict.Refuse UnixError.EPERM
            | CallerPrivilege.Unprivileged
            | CallerPrivilege.Privileged -> MkNodVerdict.Refused (MkNodRefusal.CharacterDevice dev)
        | MkNodNode.BlockDevice dev ->
            match Credentials.privilege credentials with
            | CallerPrivilege.Unprivileged -> MkNodVerdict.Refuse UnixError.EPERM
            | CallerPrivilege.Privileged -> MkNodVerdict.Refused (MkNodRefusal.BlockDevice dev)
