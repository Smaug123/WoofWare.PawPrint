namespace WoofWare.PawPrint.Test

open NUnit.Framework
open FsUnitTyped

/// The compiler the tests build their guests with.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestRoslyn =

    /// A framework reference holds a copy of its assembly's image in native memory, which only its
    /// finalizer frees and which the GC does not count. Made afresh for each compilation, the
    /// suite's hundreds of compilations held tens of gigabytes of them at once, beyond what a CI
    /// runner has.
    [<Test>]
    let ``every compilation shares one set of framework references`` () : unit =
        let first = Roslyn.metadataReferences []
        let second = Roslyn.metadataReferences []

        first.Length |> shouldBeGreaterThan 100
        second.Length |> shouldEqual first.Length

        for a, b in Array.zip first second do
            if not (obj.ReferenceEquals (a, b)) then
                failwith $"Two compilations each made their own reference to %s{a.Display}"
