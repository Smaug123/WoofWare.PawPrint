namespace WoofWare.PawPrint

open System.Collections.Immutable
open Microsoft.Extensions.Logging

/// Installs the process's command line the way a real runtime host does, so that
/// `Environment.GetCommandLineArgs()` and `Environment.CommandLine` answer from the same
/// place `Main`'s arguments came from.
///
/// On Unix there is no native to implement: CoreLib's `GetCommandLineArgsNative()` is
/// `return Array.Empty<string>()`, reached only by a library hosted from native code. The
/// real answer lives in the static `Environment.s_commandLineArgs`, which the VM fills from
/// `CorHost2::ExecuteAssembly` by calling
///
///     private static unsafe string[] Environment.InitializeCommandLineArgs(char* exePath, int argc, char** argv)
///
/// That method is ordinary managed IL — it news up two arrays, `new string(char*)`s each
/// entry, assigns `s_commandLineArgs`, and *returns* the arguments `Main` is to receive — so
/// PawPrint runs CoreLib's own code here. Running it rather than reproducing it is what makes
/// the two arrays agree by construction, exactly as they do upstream: they are built by one
/// pass over one input.
///
/// This module only *builds the call*; installing and pumping it is `Program.prepare`'s
/// business, because that is where the entry thread's frame lifecycle is managed.
[<RequireQualifiedAccess>]
module CommandLineArgsInit =

    /// What PawPrint wants `Environment::InitializeCommandLineArgs` for, phrased to complete
    /// "PawPrint calls it to …" in `HostStartupCall`'s rejections.
    [<Literal>]
    let private Purpose =
        "install the process's command line, which is what Environment.GetCommandLineArgs reads"

    /// Build the call that installs `exePath` and `argv` as the process's command line,
    /// returning the machine state with the argument buffers allocated and a frame ready to
    /// be installed and run. The frame's return value is the `string[]` that `Main` must be
    /// given.
    ///
    /// `exePath` is how the host names the assembly it is launching, which is what CoreCLR
    /// passes: for a non-bundled app `SetCommandLineArgs` forwards `ExecuteAssembly`'s
    /// `pwzAssemblyPath` verbatim, so `GetCommandLineArgs()[0]` names the *managed assembly*
    /// and not the executable that started the process. (Those differ: under `dotnet app.dll`
    /// the executable is the muxer. `Environment.ProcessPath` is where that one is reported.)
    /// See `GuestConfig.AssemblyPath`, which is where a host chooses it.
    ///
    /// There is no arm that declines to install a command line. `ExecuteAssembly` is the only
    /// route to `Main` and it refuses a null assembly path (`E_POINTER`), so a guest that runs
    /// `Main` while `Environment.GetCommandLineArgs()` reports nothing is a state no real
    /// runtime reaches. CoreLib's empty-array fallback exists for a *library* hosted from
    /// native code, which is not what PawPrint does.
    let prepareCall
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (exePath : string)
        (argv : string list)
        (state : IlMachineState)
        : IlMachineState * MethodState
        =
        let exePathPointer, state = HostStartupCall.allocateWideString exePath state

        let argPointers, state =
            (state, argv)
            ||> List.mapFold (fun state arg -> HostStartupCall.allocateWideString arg state)

        // Allocated even when there are no arguments, so that the callee is handed a real
        // pointer with a zero count rather than a null it does not expect. CoreCLR passes
        // hostpolicy's `argv` unconditionally, and the managed body indexes it only under
        // `i < argc`, so the block is legitimately never read in that case.
        let pArgv, state = HostStartupCall.allocatePointerArray argPointers state

        let method =
            HostStartupCall.findCorelibStaticMethod
                baseClassTypes
                "System"
                "Environment"
                "InitializeCommandLineArgs"
                3
                Purpose

        let args =
            ImmutableArray.CreateRange
                [
                    CliType.RuntimePointer (CliRuntimePointer.Managed exePathPointer)
                    CliType.Numeric (CliNumericType.Int32 (List.length argv))
                    CliType.RuntimePointer (CliRuntimePointer.Managed pArgv)
                ]

        HostStartupCall.buildFrame loggerFactory baseClassTypes method args Purpose state
