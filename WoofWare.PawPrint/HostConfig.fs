namespace WoofWare.PawPrint

open System.Collections.Immutable

/// Everything the host supplies that describes the simulated process itself, as opposed to
/// which of its possible thread interleavings we are exploring: where its framework comes from,
/// what its kernel looks like, what it was invoked with.
///
/// Split out of `HostConfig` so that the schedule seed can be handed over separately. Two runs
/// of the same image that share a `GuestConfig` but differ in seed are the *same program*
/// explored along different schedules — and, up to the first point at which more than one thread
/// is Runnable, they are bit-for-bit the same execution. A harness that wants to compute that
/// shared prefix once and fan out over seeds therefore needs to name "everything except the
/// seed", and needs it to be impossible to pass a seed in by accident. See
/// `docs/plans/2026-08-10-fork-point-snapshots.md`.
///
/// Every field here is part of a run's replay contract, as is `HostConfig.PctSeed`.
type GuestConfig =
    {
        /// Directories on the real host machine searched, in order, for framework assemblies. Binding is
        /// by simple name and takes the first hit, so putting a runtime pack at
        /// the head of this list selects that pack's CoreLib flavour.
        DotnetRuntimeDirs : ImmutableArray<string>
        /// The simulated process's kernel-visible state: environment, processor
        /// count, virtual clock, platform identity.
        Kernel : KernelConfig
        /// The guest's command-line arguments, as `Main` receives them — i.e.
        /// excluding the program name. Distinct from `Kernel.Environment`
        /// because the runtime hands these to `Main` directly rather than the
        /// guest reading them back through a syscall.
        ///
        /// An element containing a NUL is refused when the run starts: a Unix
        /// process's arguments are NUL-terminated C strings, so no `execve`
        /// could have produced one, and the guest would otherwise see the value
        /// silently truncated at it.
        Argv : string list
        /// How the host names the assembly it is launching: what CoreCLR passes
        /// to `ExecuteAssembly`, and hence what the guest reads back as
        /// `Environment.GetCommandLineArgs().[0]`.
        ///
        /// This is *not* the executable that started the process — under
        /// `dotnet app.dll` that is the muxer, and it is reported separately as
        /// `KernelConfig.ProcessPath`. CoreCLR forwards this string verbatim
        /// (only a single-file bundle substitutes its own path), and its own
        /// comment in `corhost.cpp` records that it need not match the command
        /// line the process was really invoked with: `Foo arg1` may be reported
        /// as `Full_path_to_Foo arg1`.
        ///
        /// `None` takes the file name the compiler stamped into the image
        /// (`DumpedAssembly.ScopeName`, such as `"Guest.dll"`). That is a real
        /// fact about the image rather than an invented path, and since a bare
        /// file name is a shape CoreCLR itself contemplates here, a host with no
        /// meaningful path to give need not invent one. A command line is
        /// installed either way: every route to `Main` runs through
        /// `ExecuteAssembly`, which refuses a null assembly path outright, so
        /// "a guest running with no command line at all" is not a state to model.
        ///
        /// Refused if it contains a NUL, for the reason given on `Argv`.
        AssemblyPath : string option
        /// Properties to seed `System.AppContext` with before any guest code
        /// runs, as `hostpolicy` does from `runtimeOptions.configProperties` in
        /// the app's `runtimeconfig.json`. This is where feature switches like
        /// `System.Diagnostics.Tracing.EventSource.IsSupported` come from.
        ///
        /// Host policy rather than kernel state, hence its home here and not on
        /// `Kernel`: nothing about these is visible to the guest as a syscall,
        /// and the guest can overwrite any of them with `AppContext.SetData`.
        ///
        /// Not the whole of what the guest sees: `Program.prepare` lays these
        /// over `AppContextProperties.runtimeBaseline`, which describes the
        /// runtime rather than the host and so is not a host's to withhold. A
        /// name appearing in both is taken from here — see
        /// `AppContextProperties.withRuntimeBaseline` for why that direction.
        AppContext : AppContextProperties
    }

    /// A host that expresses no preference beyond where to find the framework:
    /// default kernel state, no guest arguments, and no AppContext properties of
    /// its own. Such a guest is still seeded with
    /// `AppContextProperties.runtimeBaseline`, which is PawPrint's rather than
    /// the host's.
    static member Default (dotnetRuntimeDirs : ImmutableArray<string>) : GuestConfig =
        {
            DotnetRuntimeDirs = dotnetRuntimeDirs
            Kernel = KernelConfig.Default
            Argv = []
            AssemblyPath = None
            AppContext = AppContextProperties.empty
        }

/// Everything the host supplies to configure one run of a guest program, as
/// distinct from the program image itself (which `Program.prepare` takes
/// separately, as a stream plus the path it came from).
///
/// Every field here is part of a run's replay contract. Two runs with equal
/// `HostConfig` over the same image will produce the same trace.
///
/// The seed is the *only* thing at this level: it is the one input that selects
/// among the schedules of an otherwise fixed program, and separating it is what lets a
/// schedule-sweeping harness say "this program, under all of these seeds" without being able to
/// name a seed in the part that is shared. See `GuestConfig`.
type HostConfig =
    {
        /// The simulated process: framework, kernel, arguments, AppContext.
        Guest : GuestConfig
        /// Seed for the PCT scheduler. `None` runs the deterministic default
        /// schedule; `Some` explores a randomised-but-reproducible interleaving.
        PctSeed : uint64 option
    }

    /// A host that expresses no preference beyond where to find the framework:
    /// `GuestConfig.Default`, run under the deterministic default schedule.
    static member Default (dotnetRuntimeDirs : ImmutableArray<string>) : HostConfig =
        {
            Guest = GuestConfig.Default dotnetRuntimeDirs
            PctSeed = None
        }
