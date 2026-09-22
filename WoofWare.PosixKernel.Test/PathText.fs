namespace WoofWare.PosixKernel.Test

open WoofWare.PosixKernel

/// The .NET string a path value names, for tests whose paths are all valid
/// UTF-8 by construction: comparing against a literal, or handing the name to
/// the host filesystem. Fails loudly, naming the bytes, on one that is not.
[<RequireQualifiedAccess>]
module PathText =
    let ofName (name : DirectoryEntryName) : string =
        match DirectoryEntryName.tryToString name with
        | Some text -> text
        | None -> failwith $"PathText.ofName: \"%s{DirectoryEntryName.toEscaped name}\" is not valid UTF-8"

    let ofPath (path : UnixPath) : string =
        match UnixPath.tryToString path with
        | Some text -> text
        | None -> failwith $"PathText.ofPath: \"%s{UnixPath.toEscaped path}\" is not valid UTF-8"

    let ofAbsolute (path : AbsoluteUnixPath) : string =
        match AbsoluteUnixPath.tryToString path with
        | Some text -> text
        | None -> failwith $"PathText.ofAbsolute: \"%s{AbsoluteUnixPath.toEscaped path}\" is not valid UTF-8"

    let ofTarget (target : SymlinkTarget) : string =
        match SymlinkTarget.tryToString target with
        | Some text -> text
        | None -> failwith $"PathText.ofTarget: \"%s{SymlinkTarget.toEscaped target}\" is not valid UTF-8"

    /// The bytes of `text`'s UTF-8 encoding, as the kernel stores a name built from it.
    let bytes (text : string) : byte[] = UnixPathText.utf8.GetBytes text
