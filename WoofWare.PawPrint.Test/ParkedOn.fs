namespace WoofWare.PawPrint.Test

open System
open NUnit.Framework
open WoofWare.PawPrint

/// Parking for a test outside the `Guest` fixtures: a test that cannot pass on some runtimes yet,
/// skipped on exactly those runtimes and run on the rest.
///
/// The `Guest` fixtures park their cases through a per-fixture function of the runtime instead (for
/// example `TestPureCases.onRuntime`), whose exhaustive `match` makes a new runtime decide every
/// park before it compiles.
[<RequireQualifiedAccess>]
module ParkedOn =

    /// Whether a test parked on `runtimes` is parked when `runtime` is under test. Fails if
    /// `runtimes` is empty, which would be a park that parks nothing.
    let applies (runtimes : EmulatedRuntime list) (runtime : EmulatedRuntime) : bool =
        if List.isEmpty runtimes then
            failwith "a test was parked on no runtime at all; name at least one runtime, or delete the park"

        List.contains runtime runtimes

    /// Park the calling test on `runtimes`: when the runtime under test is one of them, stop the test
    /// with `Assert.Ignore`, reporting `reason`; otherwise return, and the test runs.
    ///
    /// `reason` says why the test cannot pass on those runtimes and what would un-park it. It must
    /// not be blank, and `runtimes` must not be empty.
    let check (runtimes : EmulatedRuntime list) (reason : string) : unit =
        if String.IsNullOrWhiteSpace reason then
            failwith "a test was parked without a reason; say why it cannot pass and what would un-park it"

        if applies runtimes (FrameworkUnderTest.runtime ()) then
            Assert.Ignore $"Parked on %s{FrameworkUnderTest.name (FrameworkUnderTest.runtime ())}: %s{reason}"
