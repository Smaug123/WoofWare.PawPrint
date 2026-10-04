namespace WoofWare.PosixKernel.Test

open WoofWare.PosixKernel

/// Permission bits for symbolic links a test builds by hand, which store the
/// bits they were created with.
[<RequireQualifiedAccess>]
module SymlinkModes =

    /// What Linux creates every link with, whatever the umask: the bits a
    /// hand-built filesystem needs when its rows are about something else.
    let linux : PermissionBits = PermissionBits.parseOrFail "SymlinkModes.linux" 0o777

    /// What `platform` gives a link created under umask 022, as a seeded link
    /// and every probe this suite replays were.
    let at022 (platform : SimulatedUnixPlatform) : PermissionBits =
        SimulatedUnixPlatform.symlinkCreationPermissions platform SeedEntry.symlinkCreatorsUmask
