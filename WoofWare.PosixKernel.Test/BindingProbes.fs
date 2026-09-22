namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open WoofWare.PosixKernel

/// One syscall from `docs/plans/2026-09-20-unix-path-bytes/darwin-eilseq-is-last.c`
/// and its siblings, as data, so that the same row can be put to the model and
/// to a real kernel. Paths are relative to the probe tree's root.
[<RequireQualifiedAccess>]
type BindingProbeCall =
    | Mkdir of path : byte list
    | OpenCreate of path : byte list
    | OpenRead of path : byte list
    /// `access(F_OK)` on a host; a following `stat` in the model.
    | Exists of path : byte list
    | ReadLink of path : byte list
    | Unlink of path : byte list
    | RmDir of path : byte list
    | Rename of source : byte list * destination : byte list

/// The rows that place EILSEQ among the other refusals of a binding, and the
/// tree they run in.
[<RequireQualifiedAccess>]
module BindingProbes =

    let text (s : string) : byte list = List.ofArray (PathText.bytes s)

    let private ff : byte list = [ 0xFFuy ]

    /// Built fresh for every row, since a row that succeeds changes it:
    /// `d` holding a file `f`; `ro`, unwritable and empty; `rosrc`, unwritable
    /// and holding `f`; `a` holding `b`; `m`, an unwritable directory to move;
    /// and `q`, somewhere to move it.
    let tree : Map<DirectoryEntryName, SeedEntry> =
        let name (s : string) =
            DirectoryEntryName.parseOrFail "probe tree" s

        let mode (bits : int) =
            PermissionBits.parseOrFail "probe tree" bits

        let file = SeedEntry.file ImmutableArray.Empty

        Map.ofList
            [
                name "d", SeedEntry.directory (Map.ofList [ name "f", file ])
                name "ro", SeedEntry.Directory (Map.empty, mode 0o555)
                name "rosrc", SeedEntry.Directory (Map.ofList [ name "f", file ], mode 0o555)
                name "a", SeedEntry.directory (Map.ofList [ name "b", SeedEntry.directory Map.empty ])
                name "m", SeedEntry.Directory (Map.empty, mode 0o555)
                name "q", SeedEntry.directory Map.empty
            ]

    /// Each row with what Darwin/APFS answered, measured at uid 501. Every `\xff`
    /// row whose answer is not EILSEQ sits beside a control showing the same
    /// refusal for a UTF-8 name: the encoding check is the last step of every
    /// binding, so everything else wins.
    let rows : (string * BindingProbeCall * UnixError option) list =
        let a300 = List.replicate 300 (byte 'a')
        let ff300 = List.replicate 300 0xFFuy

        [
            // Binding (plan §1.2).
            "mkdir(d/\\xff)", BindingProbeCall.Mkdir (text "d/" @ ff), Some UnixError.EILSEQ
            "open(d/\\xff, O_CREAT)", BindingProbeCall.OpenCreate (text "d/" @ ff), Some UnixError.EILSEQ
            "rename(d/f, d/\\xff)", BindingProbeCall.Rename (text "d/f", text "d/" @ ff), Some UnixError.EILSEQ
            "mkdir(d/\\xff/)", BindingProbeCall.Mkdir (text "d/" @ ff @ text "/"), Some UnixError.EILSEQ
            "rename(a, d/\\xff/)", BindingProbeCall.Rename (text "a", text "d/" @ ff @ text "/"), Some UnixError.EILSEQ
            "mkdir(d/é)", BindingProbeCall.Mkdir (text "d/é"), None

            // The two-by-two (plan §1.3): NAME_MAX, then write, then encoding.
            "mkdir(d/300a)", BindingProbeCall.Mkdir (text "d/" @ a300), Some UnixError.ENAMETOOLONG
            "mkdir(d/300xff)", BindingProbeCall.Mkdir (text "d/" @ ff300), Some UnixError.EILSEQ
            "mkdir(ro/300a)", BindingProbeCall.Mkdir (text "ro/" @ a300), Some UnixError.ENAMETOOLONG
            "mkdir(ro/300xff)", BindingProbeCall.Mkdir (text "ro/" @ ff300), Some UnixError.EACCES

            // Every earlier refusal of each verdict beats the encoding.
            "open(ro/g, O_CREAT)", BindingProbeCall.OpenCreate (text "ro/g"), Some UnixError.EACCES
            "open(ro/\\xff, O_CREAT)", BindingProbeCall.OpenCreate (text "ro/" @ ff), Some UnixError.EACCES
            "open(d/g/, O_CREAT)", BindingProbeCall.OpenCreate (text "d/g/"), Some UnixError.ENOENT
            "open(d/\\xff/, O_CREAT)", BindingProbeCall.OpenCreate (text "d/" @ ff @ text "/"), Some UnixError.ENOENT
            "rename(nx, d/\\xff)", BindingProbeCall.Rename (text "nx", text "d/" @ ff), Some UnixError.ENOENT
            "rename(d/f, ro/g)", BindingProbeCall.Rename (text "d/f", text "ro/g"), Some UnixError.EACCES
            "rename(d/f, ro/\\xff)", BindingProbeCall.Rename (text "d/f", text "ro/" @ ff), Some UnixError.EACCES
            "rename(rosrc/f, d/\\xff)", BindingProbeCall.Rename (text "rosrc/f", text "d/" @ ff), Some UnixError.EACCES
            "rename(d/f, d/g/)", BindingProbeCall.Rename (text "d/f", text "d/g/"), Some UnixError.ENOENT
            "rename(d/f, d/\\xff/)",
            BindingProbeCall.Rename (text "d/f", text "d/" @ ff @ text "/"),
            Some UnixError.ENOENT
            "rename(a, a/b/g)", BindingProbeCall.Rename (text "a", text "a/b/g"), Some UnixError.EINVAL
            "rename(a, a/b/\\xff)", BindingProbeCall.Rename (text "a", text "a/b/" @ ff), Some UnixError.EINVAL
            "rename(m, q/g)", BindingProbeCall.Rename (text "m", text "q/g"), Some UnixError.EACCES
            "rename(m, q/\\xff)", BindingProbeCall.Rename (text "m", text "q/" @ ff), Some UnixError.EACCES

            // Looking a name up is not binding it (plan §1.2): the name is absent.
            "open(d/\\xff, O_RDONLY)", BindingProbeCall.OpenRead (text "d/" @ ff), Some UnixError.ENOENT
            "access(d/\\xff)", BindingProbeCall.Exists (text "d/" @ ff), Some UnixError.ENOENT
            "readlink(d/\\xff)", BindingProbeCall.ReadLink (text "d/" @ ff), Some UnixError.ENOENT
            "open(\\xff/f, O_RDONLY)", BindingProbeCall.OpenRead (ff @ text "/f"), Some UnixError.ENOENT
            "unlink(d/\\xff)", BindingProbeCall.Unlink (text "d/" @ ff), Some UnixError.ENOENT
            "rmdir(d/\\xff)", BindingProbeCall.RmDir (text "d/" @ ff), Some UnixError.ENOENT
            "rename(d/\\xff, d/g)", BindingProbeCall.Rename (text "d/" @ ff, text "d/g"), Some UnixError.ENOENT
            "open(d/f/\\xff, O_RDONLY)", BindingProbeCall.OpenRead (text "d/f/" @ ff), Some UnixError.ENOTDIR
        ]

    /// Valid UTF-8 names that APFS refuses to bind and the model's Darwin binds
    /// (plan §1.1.1). A faithful model — a `BindableEntryNames.AppleUnicode`
    /// case — is what would stop admitting them.
    let overAdmitted : (string * byte list) list =
        [
            "U+FFFF, a noncharacter", text (string (char 0xFFFF))
            "U+1FFFD, unassigned", text "\U0001FFFD"
            "33 combining marks", text (String.replicate 33 (string (char 0x0301)))
        ]

    let private creating : OpenFlags =
        {
            Access = FileAccessMode.WriteOnly
            Create = true
            Exclusive = false
            Truncate = false
            NoFollow = false
            CloseOnExec = false
            Synchronous = false
        }

    let private reading : OpenFlags =
        { creating with
            Access = FileAccessMode.ReadOnly
            Create = false
        }

    let private rooted (path : byte list) : UnixPath =
        match UnixByteString.ofBytes (ImmutableArray.CreateRange (UnixPathText.separatorByte :: path)) with
        | Ok bytes -> UnixPath.ofByteString bytes
        | Error defect -> failwith $"probe path %s{UnixByteString.describe defect}"

    let private ofAnswer (answer : SyscallAnswer) : UnixError option =
        match answer with
        | SyscallAnswer.Failed error -> Some error
        | SyscallAnswer.Completed _ -> None

    /// What the model answers to `call`: its errno, or `None` for success.
    let runModel (call : BindingProbeCall) (system : UnixSystem<int, string>) : UnixError option =
        match call with
        | BindingProbeCall.Mkdir path -> UnixNamespace.mkdir (rooted path) 0o777 system |> fst |> ofAnswer
        | BindingProbeCall.OpenCreate path ->
            UnixNamespace.openPath creating (rooted path) 0o666 system |> fst |> ofAnswer
        | BindingProbeCall.OpenRead path -> UnixNamespace.openPath reading (rooted path) 0 system |> fst |> ofAnswer
        | BindingProbeCall.Exists path ->
            match UnixPathResolution.stat SymlinkPolicy.Follow (rooted path) system with
            | FileStatusAnswer.Reported _ -> None
            | FileStatusAnswer.Failed error -> Some error
        | BindingProbeCall.ReadLink path ->
            match UnixNamespace.readlink (rooted path) UserBuffer.Mapped 8192 system with
            | Ok (ReadLinkAnswer.Reported _) -> None
            | Ok (ReadLinkAnswer.Failed error) -> Some error
            | Error refusal -> failwith $"readlink refused its buffer: %A{refusal}"
        | BindingProbeCall.Unlink path -> UnixNamespace.unlink (rooted path) system |> fst |> ofAnswer
        | BindingProbeCall.RmDir path -> UnixNamespace.rmdir (rooted path) system |> fst |> ofAnswer
        | BindingProbeCall.Rename (source, destination) ->
            let argument (path : byte list) =
                PathArgumentBytes.Bytes (ImmutableArray.CreateRange (UnixPathText.separatorByte :: path))

            match UnixNamespace.rename (argument source) (argument destination) system with
            | Ok (answer, _) -> ofAnswer answer
            | Error refusal -> failwith $"rename refused its arguments: %A{refusal}"
