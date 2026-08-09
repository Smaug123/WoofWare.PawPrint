namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The AppContext baseline PawPrint supplies on every run, declaring that it does not
/// support dynamic code.
///
/// This is a claim about the runtime, not about any guest: PawPrint has no JIT and no
/// Reflection.Emit, and `RuntimeFeature.IsDynamicCodeSupported` is exactly the switch the
/// BCL consults before reaching for either. NativeAOT reports the same profile, so the BCL's
/// fallbacks for it are well travelled rather than exotic.
///
/// The guest-visible half of this contract lives in `sourcesImpure`
/// (`DynamicCodeUnsupportedByDefault.cs`, `DynamicCodeSupportedOverride.cs`); what is pinned
/// here is the composition itself, which those cannot see directly.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDynamicCodeSupport =

    let private switchName =
        "System.Runtime.CompilerServices.RuntimeFeature.IsDynamicCodeSupported"

    [<Test>]
    let ``the runtime baseline declares dynamic code unsupported`` () : unit =
        AppContextProperties.runtimeBaseline
        |> AppContextProperties.toMap
        |> Map.tryFind switchName
        |> shouldEqual (Some "false")

    /// Spelled out rather than asserted as "contains the switch", because every property in
    /// the baseline is seeded into every guest and so is part of PawPrint's replay contract.
    /// Adding one should require saying so here.
    [<Test>]
    let ``the runtime baseline declares exactly one property`` () : unit =
        AppContextProperties.runtimeBaseline
        |> AppContextProperties.toMap
        |> Map.toList
        |> shouldEqual [ switchName, "false" ]

    /// The whole point of a *baseline*: a host that expresses no preference still gets it.
    [<Test>]
    let ``a host expressing no preference still seeds the baseline`` () : unit =
        AppContextProperties.withRuntimeBaseline AppContextProperties.empty
        |> AppContextProperties.toMap
        |> Map.tryFind switchName
        |> shouldEqual (Some "false")

    /// Precedence, in the direction that matters: the host's own properties win. A guest
    /// whose `runtimeconfig.json` declares the switch true must observe true, because
    /// `AppContextSeed` is otherwise a faithful reproduction of hostpolicy and would stop
    /// being one if PawPrint overwrote a value the guest's configuration genuinely contains.
    [<Test>]
    let ``an explicit host value overrides the baseline`` () : unit =
        let hostSaysTrue = AppContextProperties.ofMap (Map.ofList [ switchName, "true" ])

        AppContextProperties.withRuntimeBaseline hostSaysTrue
        |> AppContextProperties.toMap
        |> Map.tryFind switchName
        |> shouldEqual (Some "true")

    /// Applying the baseline must not disturb anything else the host asked for, and must be
    /// idempotent — `Program.prepare` applies it once, but nothing in the type prevents a
    /// host from having done so already.
    [<Test>]
    let ``the baseline preserves other properties and is idempotent`` () : unit =
        let property (pairs : (NonNull<string> * NonNull<string>) list) : bool =
            let hostValues =
                pairs
                |> List.map (fun (NonNull k, NonNull v) -> k, v)
                // Two constraints on *names* only, both preconditions of `ofMap` rather than
                // of the function under test: a name that survives here must not be the
                // switch (or there is nothing for the baseline to add), and two names that
                // collide after NUL truncation make `ofMap` throw by design. Values are left
                // entirely unconstrained — `ofMap` truncates those at NUL too, and the
                // expectation below is taken from what it actually produced rather than from
                // the input, so the full alphabet stays in play instead of being narrowed to
                // dodge a policy that is tested in its own right elsewhere.
                |> List.filter (fun (k, _) -> k <> switchName && not (k.Contains '\000'))
                |> Map.ofList

            let host = AppContextProperties.ofMap hostValues
            let hostMap = AppContextProperties.toMap host

            let once = AppContextProperties.withRuntimeBaseline host
            let twice = AppContextProperties.withRuntimeBaseline once

            let onceMap = AppContextProperties.toMap once

            let preservesHost =
                hostMap |> Map.forall (fun k v -> Map.tryFind k onceMap = Some v)

            let addsTheSwitch = Map.tryFind switchName onceMap = Some "false"

            let addsNothingElse = onceMap.Count = hostMap.Count + 1

            preservesHost
            && addsTheSwitch
            && addsNothingElse
            && AppContextProperties.toMap twice = onceMap

        Check.QuickThrowOnFailure property
