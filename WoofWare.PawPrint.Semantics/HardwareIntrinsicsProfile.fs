namespace WoofWare.PawPrint

/// A CoreLib class that exposes hardware intrinsics: an instruction set such as
/// `System.Runtime.Intrinsics.X86.Sse41`, a class nested in one (`Avx512F.VL`, `Sse41.X64`), or a
/// vector API such as `System.Runtime.Intrinsics.Vector128` or `System.Numerics.Vector`. CoreCLR's
/// JIT answers each class's capability query separately, so a nested class is its own entry.
type IntrinsicClass =
    {
        Namespace : string
        /// The class, then each class nested inside it, outermost first.
        Path : string list
    }

    override this.ToString () : string =
        let path = String.concat "+" this.Path
        $"%s{this.Namespace}.%s{path}"

/// The virtual CPU a run executes on, as the guest observes it: the answers CoreCLR's JIT gives to
/// the capability queries of the hardware-intrinsic classes.
type HardwareIntrinsicsProfile =
    {
        /// The classes whose static `IsSupported` is true. Every other class's is false, and an
        /// instruction of a class in neither this set nor `IsHardwareAccelerated` throws
        /// `PlatformNotSupportedException`.
        IsSupported : Set<IntrinsicClass>
        /// The vector APIs whose static `IsHardwareAccelerated` is true: `Vector64` to `Vector512`
        /// in `System.Runtime.Intrinsics`, and `System.Numerics.Vector`.
        IsHardwareAccelerated : Set<IntrinsicClass>
    }

[<RequireQualifiedAccess>]
module HardwareIntrinsicsProfile =
    /// A CPU with no instruction set beyond the scalar ones IL needs: every capability query
    /// answers false, so the BCL takes its portable paths.
    let ScalarOnly : HardwareIntrinsicsProfile =
        {
            IsSupported = Set.empty
            IsHardwareAccelerated = Set.empty
        }
