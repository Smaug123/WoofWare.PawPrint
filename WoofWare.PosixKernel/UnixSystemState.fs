namespace WoofWare.PosixKernel

/// Everything one simulated POSIX process is, as a syscall sees it: the machine
/// it runs on, its own per-process state, and its tasks.
///
/// Generic in what names a task and what a signal handler is, for the same
/// reason `SignalState` is: those are the client's identities and this library
/// never learns them.
///
/// Opaque: a client reads it through the queries in the `UnixSystem` module
/// and changes it only through syscalls, `UnixSystem.advanceClock` and
/// `UnixSystem.writePidMaxSysctl`.
type UnixSystem<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    internal
        {
            Machine : UnixMachineState
            Process : UnixProcessState<'Task, 'Handler>
            Tasks : Map<'Task, UnixTaskState>
            /// The process's first task, which it started with: its thread-group
            /// leader.
            ///
            /// Always in `Tasks`. It cannot exit while another task lives
            /// (`UnixTaskLifecycle.exitThread` refuses that), so it is a task for as
            /// long as the process is running. On Linux its thread ID is the process ID.
            Leader : 'Task
        }

/// Reading and writing a `UnixSystem`'s process's descriptor table together
/// with the machine's open file descriptions it names.
[<RequireQualifiedAccess>]
module internal UnixSystemState =
    /// The process's descriptor table, read against the machine's open file
    /// descriptions.
    let fileDescriptors<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (system : UnixSystem<'Task, 'Handler>)
        : FileDescriptorRegistry
        =
        FileDescriptorRegistry.ofTables system.Process.FileDescriptors system.Machine.OpenFiles

    /// `system` with `registry`'s descriptor table as the process's, and its open
    /// file descriptions as the machine's.
    let withFileDescriptors<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (registry : FileDescriptorRegistry)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        { system with
            Machine =
                { system.Machine with
                    OpenFiles = FileDescriptorRegistry.openFiles registry
                }
            Process =
                { system.Process with
                    FileDescriptors = FileDescriptorRegistry.descriptorTable registry
                }
        }

    /// `system` with its machine's open file descriptions rewritten by `f`.
    let mapOpenFiles<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (f : OpenFileTable -> OpenFileTable)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        { system with
            Machine =
                { system.Machine with
                    OpenFiles = f system.Machine.OpenFiles
                }
        }

/// A simulated process and the machine it runs on before either has run: what
/// `UnixSystem.initial` makes, which the setters in `UnixBootImage` configure
/// and `UnixBootImage.boot` turns into the `UnixSystem` that syscalls take.
///
/// Opaque, so that the only way to the system inside it is `boot`, after which
/// no setter applies.
type UnixBootImage<'Task, 'Handler when 'Task : comparison and 'Handler : equality> =
    internal
        {
            System : UnixSystem<'Task, 'Handler>
        }

/// What the entry point returns, for a request this kernel could answer.
[<RequireQualifiedAccess>]
type SyscallAnswer =
    /// The entry point returns this.
    | Completed of answer : int64
    /// The entry point returns its failure sentinel, and the client stores
    /// `error` wherever its libc keeps errno.
    ///
    /// A failure still changes the system in general: `flock` advances the
    /// descriptor table before it can discover the conflict that fails it.
    | Failed of error : UnixError
