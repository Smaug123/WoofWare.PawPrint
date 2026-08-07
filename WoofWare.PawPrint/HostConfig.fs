namespace WoofWare.PawPrint

open System.Collections.Immutable

/// Everything the host supplies to configure one run of a guest program, as
/// distinct from the program image itself (which `Program.prepare` takes
/// separately, as a stream plus the path it came from).
///
/// The distinction that matters is between this and `KernelConfig`.
/// `KernelConfig` is the *simulated process's* kernel-visible state — what the
/// guest would learn by asking the operating system, and which
/// `EmulatedKernel` therefore holds as data rather than reading from the host.
/// `HostConfig` is one level out: it is how the host launches that process at
/// all, and includes `KernelConfig` as the part of the launch that describes the
/// kernel.
///
/// Every field here is part of a run's replay contract. Two runs with equal
/// `HostConfig` over the same image must produce the same trace, so nothing in
/// here may be filled in from an ambient host read.
type HostConfig =
    {
        /// Directories searched, in order, for framework assemblies. Binding is
        /// by simple name and takes the first hit, so putting a runtime pack at
        /// the head of this list selects that pack's CoreLib flavour — see
        /// `TestLinuxCoreLibFlavour` and the CoreLib-flavour section of
        /// AGENTS.md.
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
