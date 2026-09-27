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
