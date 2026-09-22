namespace WoofWare.PosixKernel

/// Which names a platform's filesystem is willing to bind: create, or rename
/// onto. Looking a name up is unaffected, since a name the filesystem could not
/// have bound is simply absent.
[<RequireQualifiedAccess>]
type BindableEntryNames =
    /// Any NUL-free byte string, as on Linux/ext4.
    | AnyBytes
    /// Only names that are valid UTF-8.
    ///
    /// This approximates Darwin/APFS. APFS also refuses some valid UTF-8 names:
    /// Unicode noncharacters, at least one unassigned code point, and combining
    /// sequences longer than 32 characters. This case admits all of those.
    | StrictUtf8

[<RequireQualifiedAccess>]
module BindableEntryNames =
    /// Whether a filesystem following `rule` will bind `name`. A binding it
    /// refuses is EILSEQ, and is the last refusal of the verdict that binds.
    ///
    /// `symlink(2)` binds its link name too, and on Darwin refuses one that is
    /// not UTF-8 with EILSEQ (measured; see §1.2 of
    /// `docs/plans/2026-09-20-unix-path-bytes.md`). There is no guest-reachable
    /// `symlink` to apply this to yet; one should.
    let admits (rule : BindableEntryNames) (name : DirectoryEntryName) : bool =
        match rule with
        | BindableEntryNames.AnyBytes -> true
        | BindableEntryNames.StrictUtf8 -> (DirectoryEntryName.tryToString name).IsSome
