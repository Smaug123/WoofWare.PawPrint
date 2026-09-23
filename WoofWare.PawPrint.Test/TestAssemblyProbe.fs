namespace WoofWare.PawPrint.Test

open System
open System.IO
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Names the test assembly, whose image the probe tests copy about.
type private ProbeTestMarker = class end

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestAssemblyProbe =

    let private testAssemblySimpleName = "WoofWare.PawPrint.Test"

    /// A fresh directory, deleted afterwards, into which the test copies what it needs.
    let private withTempRoot (body : string -> unit) : unit =
        let root =
            Path.Combine (Path.GetTempPath (), "PawPrint-" + Guid.NewGuid().ToString "N")

        Directory.CreateDirectory root |> ignore<DirectoryInfo>

        try
            body root
        finally
            Directory.Delete (root, true)

    let private copyTestAssembly (dir : string) (fileName : string) : unit =
        Directory.CreateDirectory dir |> ignore<DirectoryInfo>
        File.Copy (typeof<ProbeTestMarker>.Assembly.Location, Path.Combine (dir, fileName))

    let private probedName (dirs : string list) (simpleName : string) : string option =
        let _messages, loggerFactory = LoggerFactory.makeTest ()

        AssemblyProbe.tryReadFromRuntimeDirs loggerFactory dirs simpleName
        |> Option.map (fun assy -> assy.Name.Name)

    let private recase (upper : bool list) (s : string) : string =
        let upper = List.toArray upper

        s
        |> String.mapi (fun i c ->
            if i < upper.Length && upper.[i] then
                Char.ToUpperInvariant c
            else
                Char.ToLowerInvariant c
        )

    [<Test>]
    let ``any casing of the request finds any casing of the file`` () =
        let caseFlags (length : int) : Gen<bool list> =
            Gen.listOfLength length (Gen.elements [ true ; false ])

        let gen =
            gen {
                let! fileCase = caseFlags (testAssemblySimpleName.Length + 4)
                let! requestCase = caseFlags testAssemblySimpleName.Length
                return fileCase, requestCase
            }

        let property (fileCase : bool list, requestCase : bool list) : unit =
            withTempRoot (fun root ->
                copyTestAssembly root (recase fileCase (testAssemblySimpleName + ".dll"))

                probedName [ root ] (recase requestCase testAssemblySimpleName)
                |> shouldEqual (Some testAssemblySimpleName)
            )

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 30, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``a name no file carries is a miss`` () =
        withTempRoot (fun root ->
            copyTestAssembly root (testAssemblySimpleName + ".dll")
            probedName [ root ] "WoofWare.PawPrint.Tes" |> shouldEqual None
            probedName [ root ] (testAssemblySimpleName + "X") |> shouldEqual None
        )

    [<Test>]
    let ``a directory that does not exist is skipped`` () =
        withTempRoot (fun root ->
            let present = Path.Combine (root, "present")
            copyTestAssembly present (testAssemblySimpleName + ".dll")

            probedName [ Path.Combine (root, "absent") ; present ] testAssemblySimpleName
            |> shouldEqual (Some testAssemblySimpleName)
        )

    /// The first directory holding the name is the binding, so a later one is never read: here
    /// a later directory holds a file of that name which is not an image at all.
    [<Test>]
    let ``a later directory is not read once an earlier one holds the name`` () =
        withTempRoot (fun root ->
            let first = Path.Combine (root, "first")
            let second = Path.Combine (root, "second")
            copyTestAssembly first (testAssemblySimpleName + ".dll")
            Directory.CreateDirectory second |> ignore<DirectoryInfo>
            File.WriteAllText (Path.Combine (second, testAssemblySimpleName + ".dll"), "not an image")

            probedName [ first ; second ] testAssemblySimpleName
            |> shouldEqual (Some testAssemblySimpleName)
        )

    /// Two files differing only by case are a collision however the request spells the name, so
    /// the answer cannot depend on the request's casing. Only a case-sensitive filesystem can
    /// hold the pair.
    [<Test>]
    let ``files differing only by case are refused whatever the request's casing`` () =
        withTempRoot (fun root ->
            copyTestAssembly root (testAssemblySimpleName + ".dll")
            let lower = testAssemblySimpleName.ToLowerInvariant () + ".dll"

            if File.Exists (Path.Combine (root, lower)) then
                Assert.Ignore "the filesystem is case-insensitive, so the pair cannot exist"

            copyTestAssembly root lower

            for name in [ testAssemblySimpleName ; testAssemblySimpleName.ToLowerInvariant () ] do
                let exn =
                    Assert.Throws<Exception> (fun () -> probedName [ root ] name |> ignore<string option>)

                exn.Message |> shouldContainText "differing only by case"
        )
