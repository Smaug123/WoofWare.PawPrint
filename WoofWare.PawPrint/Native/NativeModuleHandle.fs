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
/// QCalls on <c>System.ModuleHandle</c> that answer questions about the module's image
/// rather than resolving anything out of it. The <c>ModuleHandle_Resolve*</c> entry points
/// live in <c>NativeRuntimeTypeQCall</c> instead, next to the type-resolution machinery
/// they share.
/// </summary>
[<RequireQualifiedAccess>]
module NativeModuleHandle =
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
            // discriminator — back to `IMAGE_FILE_MACHINE_NATIVE`. Both of those are fixed
            // at the *native runtime's* compile time, from its target architecture and OS,
            // and PawPrint has no such identity: it is a managed interpreter, and
            // `SimulatedUnixPlatform` deliberately models the OS the guest can observe
            // rather than an architecture anything is built for.
            //
            // This is the only place that identity is observable, because the branch above
            // overwrites the machine unconditionally — so refusing here costs exactly the
            // images whose answer we would have to invent. Every ReadyToRun assembly in a
            // shipped shared framework sets the flag above.
            failwith
                $"%s{context}: image has a ReadyToRun header without READYTORUN_FLAG_PLATFORM_NEUTRAL_SOURCE (flags 0x%08X{readyToRun.Flags}, R2R version %d{readyToRun.MajorVersion}.%d{readyToRun.MinorVersion}), so its machine depends on which architecture's native runtime is executing (CoreCLR's IMAGE_FILE_MACHINE_NATIVE), which PawPrint does not model"

        {
            PEKind = kind
            Machine = machine
        }

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "ModuleHandle_GetMDStreamVersion",
          "System.Private.CoreLib",
          "System",
          "ModuleHandle",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallModule",
                                              qCallModuleGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            qCallModuleGenerics.IsEmpty
            ->
            let operation = "ModuleHandle_GetMDStreamVersion"

            if instruction.Arguments.Length <> 1 then
                failwith $"%s{operation}: expected one native argument, got %d{instruction.Arguments.Length}"

            let assemblyFullName =
                NativeCall.qCallModuleToAssemblyFullName
                    operation
                    state
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let assembly =
                state.LoadedAssembly' assemblyFullName
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")

            // CoreCLR returns `pModule->GetMDImport()->GetMetadataStreamVersion()`, which both
            // metadata-import implementations define as `m_Schema.m_minor | (m_Schema.m_major
            // << 16)` — the major and minor bytes of the table stream header, packed into one
            // Int32 with the major in the *senior* half. So a 2.0 image reports 0x20000, which
            // is `MD_STREAM_VER_2` in metadata.h, and a 1.x one 0x1000x (`MD_STREAM_VER_1X`).
            //
            // The packing lives here rather than on `DumpedAssembly.MetadataTableStreamVersion`
            // for the same reason `GetFlags`'s `afPublicKey` synthesis does: that member says
            // what the file format says, and this is one particular CoreCLR API's encoding of
            // it, reproduced at the seam that reproduces CoreCLR APIs.
            //
            // Note this is keyed by a module while PawPrint models one module per assembly, so
            // it resolves through the assembly. That is what every other `QCallModule` handler
            // here does, and for `Assembly.GetName()` — which asks the *manifest* module — it
            // is exactly right regardless.
            let version = assembly.MetadataTableStreamVersion
            let packed = version.Minor ||| (version.Major <<< 16)

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 packed)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "ModuleHandle_GetPEKind",
          "System.Private.CoreLib",
          "System",
          "ModuleHandle",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallModule",
                                              qCallModuleGenerics)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ],
          MethodReturnType.Void when qCallModuleGenerics.IsEmpty ->
            let operation = "ModuleHandle_GetPEKind"

            if instruction.Arguments.Length <> 3 then
                failwith $"%s{operation}: expected three native arguments, got %d{instruction.Arguments.Length}"

            let assemblyFullName =
                NativeCall.qCallModuleToAssemblyFullName
                    operation
                    state
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let assembly =
                state.LoadedAssembly' assemblyFullName
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")

            // CoreCLR reaches `PEDecoder::GetPEKindAndMachine` through
            // `pModule->GetPEAssembly()->GetPEKindAndMachine`, so the answer is a function of
            // the image's headers alone — nothing here comes from the metadata tables, unlike
            // the rest of the QCalls behind `Assembly.GetName()`.
            let kindAndMachine =
                peKindAndMachine $"%s{operation}: assembly %s{assemblyFullName}" assembly.PEImageHeaders

            let writeOut (argIndex : int) (argName : string) (value : int) (state : IlMachineState) =
                let target =
                    NativeCall.managedPointerOfPointerArgument operation argName instruction.Arguments.[argIndex]

                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    target
                    (CliType.Numeric (CliNumericType.Int32 value))

            let state =
                state
                |> writeOut 1 "peKind" kindAndMachine.PEKind
                |> writeOut 2 "machine" kindAndMachine.Machine

            NativeHandlerResult.completed state |> Some
        | _ -> None
