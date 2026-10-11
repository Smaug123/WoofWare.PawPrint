namespace WoofWare.PosixKernel

/// The filesystem a machine holds (`UnixMachineState.CommittedFileSystem`).
///
/// Its representation is private to this file, so only two functions make
/// one: `CommittedFileSystem.initial`, which boot calls, and
/// `CommittedFileSystem.commit`, which every syscall that changes the
/// filesystem calls. A graph `commit` stores is the `After` of a
/// `VfsMutation`, which only a `VirtualFileSystem` mutator makes, so `commit`
/// sees the changes that mutator reported.
type internal CommittedFileSystem =
    private
        {
            Graph : VirtualFileSystem
        }

[<RequireQualifiedAccess>]
module internal CommittedFileSystem =

    /// The inode graph.
    let graph (committed : CommittedFileSystem) : VirtualFileSystem = committed.Graph

    /// A filesystem no syscall has changed: the graph a machine boots with
    /// (`UnixSystem.initial`, `UnixBootImage.withFileSystem`), or one a test
    /// forges a machine around. A syscall's change goes through `commit`.
    let initial (graph : VirtualFileSystem) : CommittedFileSystem =
        {
            Graph = graph
        }

    /// The filesystem after `mutation`, which must have been made from
    /// `committed`'s own graph: a mutation of any other graph would discard
    /// whatever `committed` holds that it does not.
    let commit (mutation : VfsMutation) (committed : CommittedFileSystem) : CommittedFileSystem =
        // By reference rather than by value: the graph a mutator was handed is
        // the very object this holds, and comparing two maps of every inode
        // would cost a walk of the filesystem per syscall.
        if not (LanguagePrimitives.PhysicalEquality (VfsMutation.before mutation) committed.Graph) then
            failwith
                "CommittedFileSystem.commit: the mutation was made from a graph other than the one this machine holds, so committing it would lose every change between the two (this is a bug in the caller)."

        {
            Graph = VfsMutation.after mutation
        }
