namespace WoofWare.PawPrint.Test

open System
open System.Reflection
open FsUnitTyped
open NUnit.Framework
open NUnit.Framework.Interfaces

/// NUnit runs a fixture that says nothing about parallelism on its single non-parallel worker,
/// one fixture after another, and nothing reports that this is happening: a hundred small fixtures
/// left to that worker keep a 4-vCPU CI runner on one core for two minutes. So every fixture must
/// choose, with `Parallelizable` or `NonParallelizable`, and a fixture that genuinely
/// shares process-wide state says so where a reader can see it.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFixturesDeclareParallelism =

    let private buildsTests (m : MethodInfo) : bool =
        m.GetCustomAttributes (true)
        |> Array.exists (fun a -> a :? ITestBuilder || a :? ISimpleTestBuilder)

    let private isFixture (t : Type) : bool =
        t.IsDefined (typeof<TestFixtureAttribute>, false)
        || t.GetMethods (
            BindingFlags.Public
            ||| BindingFlags.NonPublic
            ||| BindingFlags.Instance
            ||| BindingFlags.Static
            ||| BindingFlags.DeclaredOnly
           )
           |> Array.exists buildsTests

    [<Test>]
    let ``every fixture declares whether it runs in parallel`` () : unit =
        let fixtures = Assembly.GetExecutingAssembly().GetTypes () |> Array.filter isFixture

        // Guards against the predicate matching nothing, which would pass vacuously.
        fixtures.Length |> shouldBeGreaterThan 200

        fixtures
        // `NonParallelizableAttribute` derives from `ParallelizableAttribute`, so this admits both.
        |> Array.filter (fun t -> not (t.IsDefined (typeof<ParallelizableAttribute>, false)))
        |> Array.map (fun t -> t.FullName)
        |> Array.sort
        |> shouldBeEmpty
