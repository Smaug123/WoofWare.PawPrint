namespace WoofWare.PawPrint

open System.Reflection.PortableExecutable

/// <summary>
/// The pair <c>PEDecoder::GetPEKindAndMachine</c> computes: a <c>CorPEKind</c> bitfield
/// (<c>corhdr.h</c>) and an <c>IMAGE_FILE_MACHINE_*</c> value, which the managed caller
/// casts to <c>PortableExecutableKinds</c> and <c>ImageFileMachine</c> respectively.
/// </summary>
type PEKindAndMachine =
    {
        PEKind : int
        Machine : int
    }

/// <summary>
/// The architecture a candidate assembly presents to the binder, as
/// <c>AssemblyBinderCommon::TranslatePEToArchitectureType</c> reads it off the PE kind.
/// </summary>
[<RequireQualifiedAccess>]
type ImageArchitecture =
    /// Processor-agnostic IL: <c>peMSIL</c>.
    | Msil
    | I386
    | Amd64
    | Arm
    | Arm64

/// What CoreCLR reads off an image's headers about the kind of PE it is.
[<RequireQualifiedAccess>]
module PEImageKind =
    /// <summary>
    /// <c>PEDecoder::GetPEKindAndMachine</c> (<c>coreclr/inc/pedecoder.inl</c>), reproduced
    /// over the header fields it reads. <paramref name="context"/> only names the caller in
    /// the one failure below.
    /// </summary>
    /// <remarks>
    /// CoreCLR's leading <c>HasContents() &amp;&amp; HasNTHeaders()</c> guard, and its
    /// <c>pe32Unmanaged</c> arms, are both absent here because
    /// <see cref="PEImageHeaders"/> can only describe a managed image with NT headers — an
    /// image without either never becomes a <c>DumpedAssembly</c>. So is
    /// <c>PEAssembly::GetPEKindAndMachine</c>'s <c>IsReflectionEmit</c> case, which reports
    /// <c>(0, 0)</c>: PawPrint has no reflection-emitted assemblies to report it for.
    /// </remarks>
    let peKindAndMachine (context : string) (headers : PEImageHeaders) : PEKindAndMachine =
        // CorPEKind, corhdr.h.
        let peILonly = 0x1
        let pe32BitRequired = 0x2
        let pe32Plus = 0x4
        let pe32BitPreferred = 0x10

        let corFlags = int headers.CorFlags
        let isPE32Plus = headers.IsPE32Plus

        let mutable kind = if isPE32Plus then pe32Plus else 0
        let mutable machine = int headers.Machine

        if corFlags &&& int CorFlags.ILOnly <> 0 then
            kind <- kind ||| peILonly

            // CoreCLR compiles this under `HOST_64BIT`, which every platform PawPrint
            // simulates is. It undoes the Windows shim's promotion of a PE32 IL-only header
            // to PE32+ in memory — an artifact of a loaded layout, so a file read faithfully
            // should never present the combination at all. Reproduced anyway: where CoreCLR
            // has a defined answer, matching it costs nothing.
            if isPE32Plus && machine = int Machine.I386 then
                kind <- kind &&& ~~~pe32Plus

        // COR_IS_32BIT_REQUIRED / COR_IS_32BIT_PREFERRED (corhdr.h). The two flags are one
        // two-bit field, not two independent bits: 32BITREQUIRED alone means "x86-only",
        // and both together mean "platform-neutral but prefers to run 32-bit".
        let is32BitFlag (flag : CorFlags) : bool =
            corFlags &&& (int CorFlags.Requires32Bit ||| int CorFlags.Prefers32Bit) = int flag

        if is32BitFlag CorFlags.Requires32Bit then
            kind <- kind ||| pe32BitRequired
        elif is32BitFlag (CorFlags.Requires32Bit ||| CorFlags.Prefers32Bit) then
            kind <- kind ||| pe32BitPreferred

        // "compensate for MC++ peculiarity", says CoreCLR: a managed PE32 image that is
        // neither IL-only nor flagged 32-bit would otherwise be indistinguishable from one
        // with no COR header at all.
        if kind = 0 then
            kind <- pe32BitRequired

        match headers.ReadyToRunHeader with
        | None -> ()
        | Some readyToRun ->
            // READYTORUN_FLAG_PLATFORM_NEUTRAL_SOURCE: the IL this image was compiled from
            // was platform-neutral, so report what that IL would have reported and the
            // assembly's name still looks the way it did before the AOT step.
            if readyToRun.Flags &&& 0x1u <> 0u then
                kind <- peILonly
                machine <- int Machine.I386
            else

            // Otherwise CoreCLR would first rewrite an `IMAGE_FILE_MACHINE_NATIVE_NI`
            // machine — the running runtime's own architecture XORed with an OS
            // discriminator — back to `IMAGE_FILE_MACHINE_NATIVE`. Both are fixed when that
            // *native* runtime is compiled, and PawPrint has no such identity to answer
            // with.
            //
            // `SimulatedUnixPlatform` is not that identity, though two of its three cases do
            // name an architecture: it models what the guest could learn by asking the OS,
            // not which runtime build is executing. Do not source the machine from the
            // kernel — the CoreLib flavour comes from the runtime-dir list, not the kernel,
            // so a guest could read a linux-x64 native identity while interpreting a
            // macOS-arm64 CoreLib; and the `Custom` case has no architecture at all.
            //
            // Every ReadyToRun assembly in a shipped shared framework sets the flag above,
            // so refusing here costs only images whose answer we would have to invent.
            failwith
                $"%s{context}: image has a ReadyToRun header without READYTORUN_FLAG_PLATFORM_NEUTRAL_SOURCE (flags 0x%08X{readyToRun.Flags}, R2R version %d{readyToRun.MajorVersion}.%d{readyToRun.MinorVersion}), so its machine depends on which architecture's native runtime is executing (CoreCLR's IMAGE_FILE_MACHINE_NATIVE), which PawPrint does not model"

        {
            PEKind = kind
            Machine = machine
        }

    /// <summary>
    /// <c>AssemblyBinderCommon::TranslatePEToArchitectureType</c> over the pair
    /// <see cref="peKindAndMachine"/> computes.
    /// </summary>
    /// <remarks>
    /// IL-only, PE32, not 32-bit-required and built for I386 is processor-agnostic; a PE32+
    /// image takes the machine's architecture whether IL-only or not; and a PE32 image that is
    /// not agnostic is I386 or ARM. The combinations CoreCLR reports as
    /// <c>ERROR_BAD_FORMAT</c> -- PE32+ with 32-bit-required, or a machine outside that set --
    /// are refused here, since the binder has no answer to give for them either.
    /// </remarks>
    let architectureOfImage (context : string) (kind : PEKindAndMachine) : ImageArchitecture =
        let peILonly = 0x1
        let pe32BitRequired = 0x2
        let pe32Plus = 0x4

        let ilOnly = kind.PEKind &&& peILonly <> 0
        let is32BitRequired = kind.PEKind &&& pe32BitRequired <> 0
        let isPE32Plus = kind.PEKind &&& pe32Plus <> 0

        if
            ilOnly
            && not isPE32Plus
            && not is32BitRequired
            && kind.Machine = int Machine.I386
        then
            ImageArchitecture.Msil
        elif isPE32Plus then
            if is32BitRequired then
                failwith
                    $"%s{context}: image is PE32+ and marked 32-bit-required, which CoreCLR refuses as ERROR_BAD_FORMAT"
            elif kind.Machine = int Machine.Arm64 then
                ImageArchitecture.Arm64
            elif kind.Machine = int Machine.Amd64 then
                ImageArchitecture.Amd64
            else
                failwith
                    $"%s{context}: PE32+ image is built for machine 0x%04X{kind.Machine}, which CoreCLR refuses as ERROR_BAD_FORMAT"
        elif kind.Machine = int Machine.I386 then
            ImageArchitecture.I386
        elif kind.Machine = int Machine.ArmThumb2 then
            ImageArchitecture.Arm
        else
            failwith
                $"%s{context}: PE32 image is built for machine 0x%04X{kind.Machine}, which CoreCLR refuses as ERROR_BAD_FORMAT"
