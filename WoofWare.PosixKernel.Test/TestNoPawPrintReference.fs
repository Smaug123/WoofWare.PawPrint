namespace WoofWare.PosixKernel.Test

open System.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The invariant the whole extraction rests on: `WoofWare.PosixKernel` is a
/// POSIX process simulator that knows nothing about the CLR, and the only
/// mechanical statement of "knows nothing about" is that it does not reference
/// the assemblies which do.
///
/// A test rather than a CI step, so that it fails on the machine of whoever
/// broke it rather than ten minutes later. Assembly references are resolved by
/// the compiler from what the code actually uses, so an `open` that turned out
/// to be load-bearing shows up here even when a human reading the diff would
/// not have noticed it.
[<TestFixture>]
module TestNoPawPrintReference =

    /// Assemblies of this repository's own, none of which this one may depend
    /// on. Matched by prefix so a future `WoofWare.PawPrint.Anything` is caught
    /// without this list being updated; `WoofWare.PosixKernel` itself is of
    /// course exempt, being the assembly under test.
    let private forbiddenPrefix = "WoofWare.PawPrint"

    [<Test>]
    let ``the library references no PawPrint assembly`` () =
        let assy = typeof<UnixError>.Assembly

        assy.GetName().Name |> shouldEqual "WoofWare.PosixKernel"

        let offenders =
            assy.GetReferencedAssemblies ()
            |> Array.map (fun (r : AssemblyName) -> r.Name)
            |> Array.filter (fun (name : string) ->
                not (isNull name)
                && name.StartsWith (forbiddenPrefix, System.StringComparison.Ordinal)
            )
            |> Array.toList

        offenders |> shouldEqual []

    /// The converse, so that the assertion above cannot pass because the
    /// reference list is empty for some unrelated reason (a trimmed assembly, a
    /// reflection API that returned nothing). If this ever fails, the test
    /// above has stopped being evidence of anything.
    [<Test>]
    let ``the reference list is non-empty`` () =
        typeof<UnixError>.Assembly.GetReferencedAssemblies ()
        |> Array.isEmpty
        |> shouldEqual false
