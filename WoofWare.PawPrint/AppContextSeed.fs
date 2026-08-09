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
/// two `char**` buffers; producing those is the host doing its job, not a stand-in for
/// managed code.
///
/// This module only *builds the call*; installing and pumping it is `Program.prepare`'s
/// business, because that is where the entry thread's frame lifecycle is managed.
[<RequireQualifiedAccess>]
module AppContextSeed =

    /// Bytes in a `char**` slot. A native-int cell is 8 bytes wide, and `sizeof ptr[char]`
    /// in the guest's IL agrees, so the stride we write matches the stride `Setup` reads. A
    /// disagreement would leave every entry but the first unreadable, which is what
    /// `AppContextConfigProperties.cs` (five properties, all asserted) would catch.
    [<Literal>]
    let private PointerSize = 8

    /// Write `s` as NUL-terminated UTF-16 into a fresh native-heap block, and return a
    /// pointer to its first code unit — a `char*` as CoreLib's `Setup` will consume it.
    ///
    /// A .NET string is already UTF-16, so each `char` becomes one 2-byte cell verbatim;
    /// no encoding decision arises. Interior NULs do not arise either: `AppContextProperties`
    /// guarantees they have been truncated already, at the same point hostpolicy truncates
    /// them (assigning a `char_t*` into a `pal::string_t`). That matters for *names* rather
    /// than values — truncating late would let two names that a real host merges into one
    /// property arrive here as two, and `Setup`'s `Dictionary.Add` would throw on the
    /// duplicate.
    let private allocateWideString (s : string) (state : IlMachineState) : ManagedPointerSource * IlMachineState =
        let ptr, state =
            IlMachineState.allocateNativeMemory MemoryBlockInitialization.ZeroInitialized ((s.Length + 1) * 2) state

        let blockId =
            match ptr with
            | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (blockId, 0), []) -> blockId
            | other ->
                failwith
                    $"logic error: allocateNativeMemory returned %O{other} rather than a byref to byte 0 of a fresh native block"

        let pool =
            (IlMachineState.getNativeMemoryPool state, seq { 0 .. s.Length - 1 })
            ||> Seq.fold (fun pool i -> NativeMemoryPool.writeCell blockId (i * 2) (CliType.ofChar s.[i]) pool)

        // The terminator is left implicit: the block is zero-initialised, and a run of two
        // zero bytes is exactly the NUL code unit `wcslen` stops at.
        ptr, IlMachineState.setNativeMemoryPool pool state

    /// Build one `char**`: a block of pointer cells, one per string.
    ///
    /// The cells hold pointers, not synthesised bit patterns, so that when the guest's
    /// `ldind.i` reads a slot back it gets a pointer with its provenance intact rather than
    /// an integer we would then have to re-interpret.
    ///
    /// Native-int rather than `CliType.RuntimePointer`, because indirect memory in this
    /// machine is untyped: `stind.i` coerces a pointer to
    /// `Numeric (NativeInt (ManagedPointer …))` (`EvalStack.toCliTypeCoerced`), and
    /// `ldind.i` reads with a matching native-int template. Writing these cells the way a
    /// guest's own `stind.i` would have written them is what lets `Setup`'s `ldind.i` find
    /// them; a `RuntimePointer` cell is the right shape for a *typed* pointer slot such as a
    /// parameter, but it is not what an untyped indirect load expects.
    let private allocatePointerArray
        (targets : ManagedPointerSource list)
        (state : IlMachineState)
        : ManagedPointerSource * IlMachineState
        =
        let ptr, state =
            IlMachineState.allocateNativeMemory
                MemoryBlockInitialization.ZeroInitialized
                (List.length targets * PointerSize)
                state

        let blockId =
            match ptr with
            | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (blockId, 0), []) -> blockId
            | other ->
                failwith
                    $"logic error: allocateNativeMemory returned %O{other} rather than a byref to byte 0 of a fresh native block"

        let pool =
            (IlMachineState.getNativeMemoryPool state, List.indexed targets)
            ||> List.fold (fun pool (i, target) ->
                NativeMemoryPool.writeCell
                    blockId
                    (i * PointerSize)
                    (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer target)))
                    pool
            )

        ptr, IlMachineState.setNativeMemoryPool pool state

    /// Locate `System.AppContext::Setup`, the entry point the VM calls to install the host's
    /// properties.
    let private findSetupMethod
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        =
        let corelib = baseClassTypes.Corelib

        let appContext =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, ty)) ->
                if ty.Namespace = "System" && ty.Name = "AppContext" then
                    Some ty
                else
                    None
            )
            |> Seq.toList

        let appContext =
            match appContext with
            | [ single ] -> single
            | [] -> failwith "Could not find System.AppContext in CoreLib; cannot seed AppContext properties."
            | _ :: _ :: _ ->
                failwith
                    "Found several System.AppContext type definitions in CoreLib; cannot seed AppContext properties."

        // `Setup` is non-generic, static, and unique: it is the VM's private entry point, so
        // there are no overloads to disambiguate. Matching on arity as well as name keeps the
        // failure loud (and correctly attributed) if a future CoreLib changes its shape.
        let candidates =
            appContext.Methods
            |> List.filter (fun m -> m.Name = "Setup" && m.IsStatic && MethodInfo.arity m = 3)

        match candidates with
        | [ single ] -> single
        | [] ->
            failwith
                "Could not find the static 3-argument System.AppContext::Setup in CoreLib. CoreCLR calls this from CorHost2::CreateAppDomainWithManager to install the host's config properties; if its signature has changed, AppContextSeed needs updating to match."
        | _ :: _ :: _ ->
            failwith
                "Found several static 3-argument System.AppContext::Setup methods in CoreLib; expected exactly one."

    /// Build the call to `AppContext.Setup` that seeds `properties`, returning the machine
    /// state with the argument buffers allocated and a frame ready to be installed and run.
    ///
    /// `None` when there is nothing to seed, which skips the call rather than making it with a
    /// count of zero. The two differ internally — `Setup` assigns a fresh dictionary to
    /// `s_dataStore`, where skipping leaves it null — but not observably: `GetData` returns
    /// null for a null store, and `SetData` lazily installs one. So this buys the cheaper path
    /// without changing what a guest can see.
    ///
    /// The native blocks allocated here are deliberately never freed. `hostpolicy`'s arrays
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
            None
        else

        // Sorted, because `Map.toList` is ordered by key: the layout of the `char**` arrays
        // is then a function of the property set alone, not of any traversal order, which is
        // what makes two runs with the same `HostConfig` produce identical machine states.
        let entries = AppContextProperties.toMap properties |> Map.toList

        let namePointers, state =
            (state, entries)
            ||> List.mapFold (fun state (name, _) -> allocateWideString name state)

        let valuePointers, state =
            (state, entries)
            ||> List.mapFold (fun state (_, value) -> allocateWideString value state)

        let pNames, state = allocatePointerArray namePointers state
        let pValues, state = allocatePointerArray valuePointers state

        let setup = findSetupMethod baseClassTypes

        // AppContext is non-generic and `Setup` is non-generic, so there is nothing to
        // substitute in either position.
        let state, concretizedSetup, _ =
            ExecutionConcretization.concretizeMethodWithTypeGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                setup
                None
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                state

        let args =
            ImmutableArray.CreateRange
                [
                    CliType.RuntimePointer (CliRuntimePointer.Managed pNames)
                    CliType.RuntimePointer (CliRuntimePointer.Managed pValues)
                    CliType.Numeric (CliNumericType.Int32 (List.length entries))
                ]

        let methodState =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    baseClassTypes
                    state._LoadedAssemblies
                    baseClassTypes.Corelib
                    concretizedSetup
                    ImmutableArray.Empty
                    args
                    None
            with
            | Ok methodState -> methodState
            | Error e ->
                failwith
                    $"Failed to build a call frame for System.AppContext::Setup while seeding %i{List.length entries} AppContext properties: %O{e}"

        Some (state, methodState)
