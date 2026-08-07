namespace WoofWare.PawPrint

open System.Collections.Immutable

/// Everything the host supplies to configure one run of a guest program, as
/// distinct from the program image itself (which `Program.prepare` takes
/// separately, as a stream plus the path it came from).
///
/// Every field here is part of a run's replay contract. Two runs with equal
/// `HostConfig` over the same image will produce the same trace.
type HostConfig =
    {
        /// Directories on the real host machine searched, in order, for framework assemblies. Binding is
        /// by simple name and takes the first hit, so putting a runtime pack at
        /// the head of this list selects that pack's CoreLib flavour.
        DotnetRuntimeDirs : ImmutableArray<string>
        /// The simulated process's kernel-visible state: environment, processor
        /// count, virtual clock, platform identity.
        Kernel : KernelConfig
        /// Seed for the PCT scheduler. `None` runs the deterministic default
        /// schedule; `Some` explores a randomised-but-reproducible interleaving.
        PctSeed : uint64 option
        /// The guest's command-line arguments, as `Main` receives them — i.e.
        /// excluding the program name. Distinct from `Kernel.Environment`
        /// because the runtime hands these to `Main` directly rather than the
        /// guest reading them back through a syscall.
        Argv : string list
    }

    /// A host that expresses no preference beyond where to find the framework:
    /// default kernel state, the deterministic default schedule, and no guest
    /// arguments.
    static member Default (dotnetRuntimeDirs : ImmutableArray<string>) : HostConfig =
        {
            DotnetRuntimeDirs = dotnetRuntimeDirs
            Kernel = KernelConfig.Default
            PctSeed = None
            Argv = []
        }
