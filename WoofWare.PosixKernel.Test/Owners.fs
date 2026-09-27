namespace WoofWare.PosixKernel.Test

open WoofWare.PosixKernel

/// Owners for inodes a test builds by hand.
[<RequireQualifiedAccess>]
module Owners =

    /// The effective user and group of a freshly-minted Linux process, which is
    /// who owns everything `UnixSystem.initial` gives that process: the owner a
    /// hand-built filesystem needs when its rows are about something else.
    let linuxDefault : InodeOwner =
        InodeOwner.ofProcess (UnixSystem.defaultCredentials SimulatedUnixFlavour.Linux)

    /// The credentials of the process `linuxDefault` names: the caller for
    /// whom every inode of a hand-built filesystem is its own.
    let linuxDefaultCaller : Credentials =
        UnixSystem.defaultCredentials SimulatedUnixFlavour.Linux

    /// Root, who owns nothing `linuxDefault` owns and is exempt from the
    /// permission rules anyway.
    let root : Credentials =
        Credentials.ofIds UserId.root (GroupId.parseOrFail "Owners.root" 0u) []

    /// The caller a test that states only a privilege means: root if
    /// privileged, and otherwise the owner of everything `linuxDefault` owns.
    let caller (privilege : CallerPrivilege) : Credentials =
        match privilege with
        | CallerPrivilege.Privileged -> root
        | CallerPrivilege.Unprivileged -> linuxDefaultCaller

    /// How a caller with `privilege` stands towards an inode it owns and whose
    /// group it is in: the standing every caller has towards everything in a
    /// filesystem it built itself.
    let owning (privilege : CallerPrivilege) : Standing =
        {
            Privilege = privilege
            Owns = true
            InGroup = true
        }
