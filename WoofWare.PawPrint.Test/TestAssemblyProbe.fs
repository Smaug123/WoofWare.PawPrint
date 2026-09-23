namespace WoofWare.PawPrint.Test

open System
open System.IO
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestAssemblyProbe =

    /// The image the tests copy about: a small one, because `Assembly.readFile` keeps every parse
    /// it makes, keyed by full path, for the life of the process, and each test here reads a
    /// fresh copy. A copy of the ten-megabyte test assembly per read is more than the test host's
    /// capped heap can keep. This image also sits in the test host's output directory, which is
    /// its current directory.
    let private imageSimpleName = "WoofWare.PawPrint.Logging"

    let private imagePath : string =
        typeof<WoofWare.PawPrint.Logging.LoggingConfig>.Assembly.Location

    /// A fresh directory, deleted afterwards, into which the test copies what it needs.
    let private withTempRoot (body : string -> unit) : unit =
        let root =
            Path.Combine (Path.GetTempPath (), "PawPrint-" + Guid.NewGuid().ToString "N")

        Directory.CreateDirectory root |> ignore<DirectoryInfo>

        try
            body root
        finally
            Directory.Delete (root, true)

    let private copyImage (dir : string) (fileName : string) : unit =
        Directory.CreateDirectory dir |> ignore<DirectoryInfo>
        File.Copy (imagePath, Path.Combine (dir, fileName))

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
                let! fileCase = caseFlags (imageSimpleName.Length + 4)
                let! requestCase = caseFlags imageSimpleName.Length
                return fileCase, requestCase
            }

        let property (fileCase : bool list, requestCase : bool list) : unit =
            withTempRoot (fun root ->
                copyImage root (recase fileCase (imageSimpleName + ".dll"))

                probedName [ root ] (recase requestCase imageSimpleName)
                |> shouldEqual (Some imageSimpleName)
            )

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 30, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``a name no file carries is a miss`` () =
        withTempRoot (fun root ->
            copyImage root (imageSimpleName + ".dll")
            probedName [ root ] "WoofWare.PawPrint.Tes" |> shouldEqual None
            probedName [ root ] (imageSimpleName + "X") |> shouldEqual None
        )

    /// The empty runtime-dir entry, which is how `Path.GetDirectoryName` spells a bare file name's
    /// directory, is the current directory. Tested as a pure mapping because the current
    /// directory is process-wide and this fixture runs in parallel.
    [<TestCase("", ".")>]
    [<TestCase(".", ".")>]
    [<TestCase("sub", "sub")>]
    [<TestCase("/abs/dir", "/abs/dir")>]
    let ``the empty runtime dir is the current directory`` (dir : string) (expected : string) =
        AssemblyProbe.runtimeDirPath dir |> shouldEqual expected

    /// End to end through the probe: the test host's current directory is its output directory,
    /// which holds the image. Nothing in the suite changes the current directory.
    [<Test>]
    let ``the empty runtime dir finds an assembly in the current directory`` () =
        if not (File.Exists (imageSimpleName + ".dll")) then
            Assert.Ignore "the test host's current directory does not hold the image"

        probedName [ "" ] imageSimpleName |> shouldEqual (Some imageSimpleName)

    [<Test>]
    let ``a directory that does not exist is skipped`` () =
        withTempRoot (fun root ->
            let present = Path.Combine (root, "present")
            copyImage present (imageSimpleName + ".dll")

            probedName [ Path.Combine (root, "absent") ; present ] imageSimpleName
            |> shouldEqual (Some imageSimpleName)
        )

    /// The first directory holding the name is the binding, so a later one is never read: here
    /// a later directory holds a file of that name which is not an image at all.
    [<Test>]
    let ``a later directory is not read once an earlier one holds the name`` () =
        withTempRoot (fun root ->
            let first = Path.Combine (root, "first")
            let second = Path.Combine (root, "second")
            copyImage first (imageSimpleName + ".dll")
            Directory.CreateDirectory second |> ignore<DirectoryInfo>
            File.WriteAllText (Path.Combine (second, imageSimpleName + ".dll"), "not an image")

            probedName [ first ; second ] imageSimpleName
            |> shouldEqual (Some imageSimpleName)
        )

    /// A directory that exists but cannot be listed is not the same as one that does not exist:
    /// skipping it would bind from a later directory, and the runtime-dir order is what picks the
    /// CoreLib flavour. Here the directory sits beneath an ancestor without search permission,
    /// for which `Directory.Exists` answers false.
    [<Test>]
    let ``a runtime dir that cannot be searched is an error, not a miss`` () =
        if OperatingSystem.IsWindows () then
            Assert.Ignore "Unix permissions"

        withTempRoot (fun root ->
            let locked = Path.Combine (root, "locked")
            let inner = Path.Combine (locked, "inner")
            let later = Path.Combine (root, "later")
            copyImage inner (imageSimpleName + ".dll")
            copyImage later (imageSimpleName + ".dll")

            File.SetUnixFileMode (locked, UnixFileMode.None)

            try
                if Directory.Exists inner then
                    Assert.Ignore "permissions are not enforced for this user"

                Assert.Throws<UnauthorizedAccessException> (fun () ->
                    probedName [ inner ; later ] imageSimpleName |> ignore<string option>
                )
                |> ignore<UnauthorizedAccessException>
            finally
                File.SetUnixFileMode (
                    locked,
                    UnixFileMode.UserRead ||| UnixFileMode.UserWrite ||| UnixFileMode.UserExecute
                )
        )

    /// A directory entry that lists under the name but cannot be opened, such as a dangling
    /// symlink, holds nothing, and the probe goes on to the next directory.
    [<Test>]
    let ``a dangling symlink is skipped in favour of a later directory`` () =
        withTempRoot (fun root ->
            let first = Path.Combine (root, "first")
            let second = Path.Combine (root, "second")
            Directory.CreateDirectory first |> ignore<DirectoryInfo>

            File.CreateSymbolicLink (
                Path.Combine (first, imageSimpleName + ".dll"),
                Path.Combine (root, "nowhere.dll")
            )
            |> ignore<FileSystemInfo>

            copyImage second (imageSimpleName + ".dll")

            probedName [ first ; second ] imageSimpleName
            |> shouldEqual (Some imageSimpleName)
        )

    /// Two files differing only by case are a collision however the request spells the name, so
    /// the answer cannot depend on the request's casing. Only a case-sensitive filesystem can
    /// hold the pair.
    [<Test>]
    let ``files differing only by case are refused whatever the request's casing`` () =
        withTempRoot (fun root ->
            copyImage root (imageSimpleName + ".dll")
            let lower = imageSimpleName.ToLowerInvariant () + ".dll"

            if File.Exists (Path.Combine (root, lower)) then
                Assert.Ignore "the filesystem is case-insensitive, so the pair cannot exist"

            copyImage root lower

            for name in [ imageSimpleName ; imageSimpleName.ToLowerInvariant () ] do
                let exn =
                    Assert.Throws<Exception> (fun () -> probedName [ root ] name |> ignore<string option>)

                exn.Message |> shouldContainText "differing only by case"
        )
