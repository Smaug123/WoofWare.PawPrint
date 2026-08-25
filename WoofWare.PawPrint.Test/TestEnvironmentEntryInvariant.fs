namespace WoofWare.PawPrint.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// A real process's environment is a list of `name=value` strings, not a map; the
/// map every environment API presents is a *view* of that list, split at each
/// entry's first `=`. CoreCLR makes the view total by refusing to look up a name
/// that is empty or contains `=` (`GetEnvironmentVariableA` in
/// `pal/src/misc/environ.cpp`) and by discarding, in
/// `Environment.GetEnvironmentVariables`, any entry whose first `=` is not after
/// the first character. So the names the view can yield are exactly the
/// non-empty, `=`-free ones.
///
/// Measured against real .NET with a hand-built `envp`, which is the only way to
/// get such an entry into a process (`Environment.SetEnvironmentVariable` refuses
/// to create one):
///
///   entry `A=B=C`     -> `GetEnvironmentVariable "A"` = "B=C", `"A=B"` = null,
///                        and enumeration yields the key `A` and no key `A=B`
///   entry `=C`        -> invisible to both APIs
///   `DUP=1`, `DUP=2`  -> both APIs report `DUP` = "1"
///
/// PawPrint stores the map rather than the list, so it *can* hold a name that
/// view could never yield — and such a name has no consistent behaviour to
/// model, since the table would have to answer one lookup two ways at once.
/// These tests pin the boundary that keeps those tables out.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEnvironmentEntryInvariant =
    /// A NUL code unit as a string, so no source file has to contain one.
    let private nul : string = string (char 0)

    /// One entry per rejected shape, each provoking that shape alone: the empty
    /// name has no `=` and no NUL, the `=` name has no NUL, the NUL-name row has
    /// no `=`, and the null rows are null in exactly one position. So removing any
    /// single check leaves exactly one row passing where it should fail.
    ///
    /// The null rows matter because `Map<string, string>` really does admit them —
    /// F#'s comparer sorts a null key first, and a C# consumer of this package has
    /// nothing stopping it — so without them the rule would dereference null and
    /// abort a run with a bare NullReferenceException instead of naming the knob.
    /// `Map admits the null entries these tests rely on` below checks that premise
    /// rather than assuming it.
    let private rejected : (string * string * string) list =
        [
            "null name", null, "value"
            "null value", "A", null
            "empty name", "", "value"
            "'=' in name", "A=B", "value"
            "NUL in name", "A" + nul + "B", "value"
            "NUL in value", "A", "va" + nul + "ue"
        ]

    [<Test>]
    let ``Map admits the null entries these tests rely on`` () : unit =
        // If a future FSharp.Core refused a null key, the null rows above would be
        // testing an unconstructible input and would silently stop covering
        // anything. This is the premise, asserted.
        let withNullName = Map.ofList [ null, "value" ]
        withNullName |> Map.count |> shouldEqual 1
        withNullName |> Map.containsKey null |> shouldEqual true

        let withNullValue : Map<string, string> = Map.ofList [ "A", null ]
        withNullValue |> Map.count |> shouldEqual 1
        withNullValue |> Map.tryFind "A" |> shouldEqual (Some (null : string))

    /// Shapes a real `environ` really can hold, so the rule cannot be satisfied
    /// by refusing everything. A value may contain `=` (that is what an entry
    /// `A=B=C` *is*), a value may be empty (`FOO=`), and either half may hold
    /// non-ASCII.
    let private accepted : (string * string) list =
        [
            "PLAIN", "1"
            "EMPTY_VALUE", ""
            "EQUALS_IN_VALUE", "a=b=c"
            "lower.case-name_1", "v"
            "é中", "\U0001F436"
        ]

    [<Test>]
    let ``the entry rule names what is wrong`` () : unit =
        for description, name, value in rejected do
            match UnixProcessState.environmentEntryProblem name value with
            | None -> failwith $"expected %s{description} to be rejected, but the rule accepted it"
            | Some problem -> problem |> shouldNotEqual ""

    [<Test>]
    let ``the entry rule accepts what a real environ can hold`` () : unit =
        for name, value in accepted do
            match UnixProcessState.environmentEntryProblem name value with
            | None -> ()
            | Some problem -> failwith $"expected (%A{name}, %A{value}) to be accepted, but the rule said: %s{problem}"

    [<Test>]
    let ``the kernel accepts an overlay a real environ can hold`` () : unit =
        let kernel =
            EmulatedKernel.initial
            |> EmulatedKernel.mapProcess (UnixProcessState.withEnvironment "test" (Map.ofList accepted))

        for name, value in accepted do
            kernel.Environment |> Map.tryFind name |> shouldEqual (Some value)

        // The seeded default survives an unrelated overlay.
        kernel.Environment
        |> Map.tryFind "DOTNET_SYSTEM_GLOBALIZATION_INVARIANT"
        |> shouldEqual (Some "1")

    [<Test>]
    let ``applying a KernelConfig rejects an unrepresentable entry`` () : unit =
        // The boundary that matters. Without it the two environment APIs disagree
        // for the same table, and — worse — they disagree *asymmetrically*:
        // `GetEnvironmentVariable "A=B"` would quietly answer with the value,
        // where real .NET answers null, while `GetEnvironmentVariables` would
        // abort in the block writer. One silently wrong path, one loud one.
        //
        // `KernelConfig.applyTo` is the path every host takes, so this is where
        // the rejection has to fire. A future `applyTo` that assigned
        // `Environment` by record-copy instead of through
        // `UnixProcessState.withEnvironment` would bypass the rule, and nothing
        // in this repository would notice: the rule itself belongs to the
        // library, whose own tests cannot see how PawPrint reaches it.
        //
        // The message names the knob because this call site passes that name;
        // asserting it here is what stops the name drifting to one no host has
        // heard of.
        for description, name, value in rejected do
            let config =
                { KernelConfig.Default with
                    Environment = Map.ofList [ name, value ]
                }

            let exn =
                Assert.Throws<System.Exception> (fun () ->
                    EmulatedKernel.initial |> KernelConfig.applyTo config |> ignore<EmulatedKernel>
                )

            exn.Message |> shouldContainText "KernelConfig.Environment"
            description |> shouldNotEqual ""

    [<Test>]
    let ``the default KernelConfig applies cleanly`` () : unit =
        // The control for the config-path test above: `KernelConfig.Default` and
        // `defaultEnvironment` must themselves satisfy the rule, or every run
        // would fail.
        let kernel = EmulatedKernel.initial |> KernelConfig.applyTo KernelConfig.Default

        for KeyValue (name, value) in kernel.Environment do
            match UnixProcessState.environmentEntryProblem name value with
            | None -> ()
            | Some problem -> failwith $"the default kernel environment holds an unrepresentable entry: %s{problem}"

        kernel.Environment |> Map.isEmpty |> shouldEqual false
