namespace WoofWare.Pawprint.Test

open FsUnitTyped
open NUnit.Framework

[<TestFixture>]
module TestThing =
    [<Test>]
    let ``foo is a thing`` () = 1 |> shouldEqual 1
