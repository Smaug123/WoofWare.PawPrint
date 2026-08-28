namespace WoofWare.PawPrint

open System.Collections.Immutable
open Microsoft.Extensions.Logging

/// Seeds `System.AppContext` the way a real runtime host does, so that feature switches
/// declared in `runtimeconfig.json` — `System.Diagnostics.Tracing.EventSource.IsSupported`
/// and friends — are in place before any guest or BCL code can latch them.
///
/// CoreCLR does this from `CorHost2::CreateAppDomainWithManager`, which calls
///
///     internal static unsafe void AppContext.Setup(char** pNames, char** pValues, int count)
///
/// with two arrays of NUL-terminated UTF-16 strings that `hostpolicy` allocated. `Setup` is
/// ordinary managed IL — it news up a `Dictionary<string, object>`, walks the arrays doing
/// pointer arithmetic, and `new string(char*)`s each entry — so PawPrint runs CoreLib's own
/// code here. The host's contribution, and the only thing this module synthesises, is the
/// two `char**` buffers.
///
/// This module only *builds the call*; installing and pumping it is `Program.prepare`'s
/// business, because that is where the entry thread's frame lifecycle is managed.
[<RequireQualifiedAccess>]
module AppContextSeed =

    /// What PawPrint wants `AppContext::Setup` for, phrased to complete "PawPrint calls it
    /// to …" in `HostStartupCall`'s rejections.
    [<Literal>]
    let private Purpose =
        "install the host's configuration properties, which is what AppContext.GetData reads"

    /// Build the call to `AppContext.Setup` that seeds `properties`, returning the machine
    /// state with the argument buffers allocated and a frame ready to be installed and run.
    ///
    /// `None` when there is nothing to seed, which skips the call rather than making it with a
    /// count of zero; the difference is not observable to the guest.
    ///
    /// The native blocks allocated here are never freed. `hostpolicy`'s arrays
    /// outlive the call too, and a guest is entitled to have kept a `char*` into one — so
    /// freeing them would turn a legal (if strange) guest into a use-after-free report.
    let prepareCall
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (properties : AppContextProperties)
        (state : IlMachineState)
        : (IlMachineState * MethodState) option
        =
        if AppContextProperties.isEmpty properties then
            // Skipping differs from calling with a count of zero internally — `Setup`
            // assigns a fresh dictionary to `s_dataStore`, where skipping leaves it null —
            // but not observably: `GetData` returns null for a null store, and `SetData`
            // lazily installs one. So this buys the cheaper path without changing what a
            // guest can see.
            None
        else

        // Sorted, because `Map.toList` is ordered by key: the layout of the `char**` arrays
        // is then a function of the property set alone, not of any traversal order, which is
        // what makes two runs with the same `HostConfig` produce identical machine states.
        let entries = AppContextProperties.toMap properties |> Map.toList

        let namePointers, state =
            (state, entries)
            ||> List.mapFold (fun state (name, _) -> HostStartupCall.allocateWideString name state)

        let valuePointers, state =
            (state, entries)
            ||> List.mapFold (fun state (_, value) -> HostStartupCall.allocateWideString value state)

        let pNames, state = HostStartupCall.allocatePointerArray namePointers state
        let pValues, state = HostStartupCall.allocatePointerArray valuePointers state

        let setup =
            HostStartupCall.findCorelibStaticMethod baseClassTypes "System" "AppContext" "Setup" 3 Purpose

        let args =
            ImmutableArray.CreateRange
                [
                    CliType.RuntimePointer (CliRuntimePointer.Managed pNames)
                    CliType.RuntimePointer (CliRuntimePointer.Managed pValues)
                    CliType.Numeric (CliNumericType.Int32 (List.length entries))
                ]

        HostStartupCall.buildFrame loggerFactory baseClassTypes setup args Purpose state
        |> Some
