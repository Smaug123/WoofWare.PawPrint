namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// What `opendir(3)` answered.
[<RequireQualifiedAccess>]
type DirectoryStreamOpen =
    /// A stream, named by the native block whose address the guest holds as
    /// its `DIR*`.
    | Opened of NativeMemoryBlockId
    /// No stream, and the errno `opendir` set.
    | Failed of UnixError

/// What `SystemNative_OpenDir` and `SystemNative_CloseDir` do to the kernel,
/// apart from decoding their arguments and reporting their results.
///
/// A guest's `DIR*` is the address of a native block, whose bytes are also the
/// `d_name` buffer each `readdir` refills, and the stream reads through a
/// descriptor onto the directory, as `opendir(3)` and `closedir(3)` do on a real
/// libc. `EmulatedKernel.DirectoryStreamFds` binds the one to the other.
[<RequireQualifiedAccess>]
module DirectoryStream =

    /// How big the `d_name` buffer inside one directory stream is.
    ///
    /// Fixed for the stream's life, because its address *is* the `DIR*` the
    /// guest holds. 1024 is what Darwin's `struct dirent` declares
    /// (`__DARWIN_MAXPATHLEN`), and it bounds every name either modelled kernel
    /// can store: Linux permits 255 bytes, and Darwin 255 UTF-16 code units,
    /// which is at most 765 bytes.
    let nameBufferBytes : int = 1024

    /// `opendir(3)` of `path`, on `thread`'s behalf: open the directory, and
    /// allocate the block that stands for the stream and bind it to the
    /// descriptor. A failed open allocates and binds nothing.
    let opendir
        (thread : ThreadId)
        (path : PathArgumentBytes)
        (kernel : EmulatedKernel)
        : DirectoryStreamOpen * EmulatedKernel
        =
        let operation = "SystemNative_OpenDir"

        // `Follow`, and a trailing separator that merely records its demand:
        // measured on both kernels, `opendir` follows a final symlink and a
        // trailing separator changes no row at all — "ld" and "ld/" both
        // succeed, "f" and "f/" are both ENOTDIR. Nothing reads
        // `TrailingSeparatorDemanded`, because a directory is demanded
        // outright whether the separator was there or not.
        // `opendir(3)` is this open: glibc's is
        // `openat(O_RDONLY|O_NONBLOCK|O_CLOEXEC|O_DIRECTORY)`, measured (see
        // `OpenFlagsPal.directoryStream`).
        let flags = OpenFlagsPal.directoryStream kernel.UnixPlatform

        let opened =
            try
                UnixNamespace.openPath (kernel.Caller thread) path flags 0 kernel.System
            with :? UnmodelledException as e ->
                KernelRefusal.failAdvising
                    operation
                    "This open asks only for O_RDONLY|O_NONBLOCK|O_DIRECTORY|O_CLOEXEC, which the kernel models."
                    e

        match opened with
        | OpenOutcome.WouldBlock condition, _ ->
            failwith
                $"%s{operation}: thread %O{thread}'s open with O_DIRECTORY would sleep until %A{condition}, but an open that sleeps is of a FIFO, which O_DIRECTORY refuses with ENOTDIR before it could sleep (this is a bug in the kernel library)."
        | OpenOutcome.Answered (DescriptorAnswer.Failed error), system ->
            DirectoryStreamOpen.Failed error, EmulatedKernel.withUnix system kernel
        | OpenOutcome.Answered (DescriptorAnswer.Returned fd), system ->

        // One allocation per stream rather than one per entry, because
        // `DirectoryEntry.Name` points into the stream's own storage and stays
        // valid only until the next `readdir`.
        let block, pool =
            NativeMemoryPool.allocate MemoryBlockInitialization.ZeroInitialized nameBufferBytes kernel.NativeMemoryPool

        let pool =
            NativeMemoryPool.writeBytes block 0 (Array.zeroCreate nameBufferBytes) pool

        let kernel =
            { EmulatedKernel.withUnix system kernel with
                NativeMemoryPool = pool
            }

        DirectoryStreamOpen.Opened block, EmulatedKernel.withDirectoryStreamFd block (int fd) kernel

    /// `closedir(3)` of the stream `block` names, on `thread`'s behalf: unbind and free the
    /// block, and close its descriptor. The stream is released whatever the close answers,
    /// as a real `closedir` frees its `DIR` whatever `close` answers.
    ///
    /// The close fails only with EBADF, which a guest reaches by closing the
    /// stream's descriptor behind its back; descriptor numbers are guessable.
    /// A `block` that names no open stream is refused, as
    /// `EmulatedKernel.directoryStreamFd` refuses it.
    let closedir
        (thread : ThreadId)
        (block : NativeMemoryBlockId)
        (kernel : EmulatedKernel)
        : Result<unit, UnixError> * EmulatedKernel
        =
        let fd = EmulatedKernel.directoryStreamFd block kernel

        let kernel =
            { EmulatedKernel.withoutDirectoryStream block kernel with
                NativeMemoryPool = NativeMemoryPool.free block kernel.NativeMemoryPool
            }

        match UnixDescriptor.close (kernel.Caller thread) fd kernel.System with
        | SyscallAnswer.Completed _, system -> Ok (), EmulatedKernel.withUnix system kernel
        | SyscallAnswer.Failed error, system -> Error error, EmulatedKernel.withUnix system kernel
