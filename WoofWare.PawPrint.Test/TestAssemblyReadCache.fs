namespace WoofWare.PawPrint.Test

open System
open System.IO
open FsUnitTyped
open Microsoft.CodeAnalysis
open Microsoft.Extensions.Logging
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
module TestAssemblyReadCache =
    let private readAssembly
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (path : string)
        : DumpedAssembly
        =
        global.WoofWare.PawPrint.AssemblyApi.readFile loggerFactory path

    let private compileDll (assemblyName : string) : byte[] =
        Roslyn.compileAssembly
            assemblyName
            OutputKind.DynamicallyLinkedLibrary
            []
            [ "public static class C { public static int Value() => 1; }" ]

    [<Test>]
    let ``readFile reuses cached parse for repeated files`` () : unit =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, firstLoggerFactory = LoggerFactory.makeTest ()
        let _, secondLoggerFactory = LoggerFactory.makeTest ()

        let first = readAssembly firstLoggerFactory corelibPath
        let second = readAssembly secondLoggerFactory corelibPath

        first.Name.FullName |> shouldEqual second.Name.FullName
        Object.ReferenceEquals (first.PeReader, second.PeReader) |> shouldEqual true

    [<Test>]
    let ``readFile gives cache hits the caller's logger`` () : unit =
        let corelibPath = typeof<obj>.Assembly.Location
        let firstLogs, firstLoggerFactory = LoggerFactory.makeTest ()
        let secondLogs, secondLoggerFactory = LoggerFactory.makeTest ()

        let first = readAssembly firstLoggerFactory corelibPath
        let second = readAssembly secondLoggerFactory corelibPath

        first.Logger.LogInformation "first cached assembly logger"
        second.Logger.LogInformation "second cached assembly logger"

        let firstMessages = firstLogs () |> List.map (fun line -> line.Message)
        let secondMessages = secondLogs () |> List.map (fun line -> line.Message)

        firstMessages
        |> List.contains "first cached assembly logger"
        |> shouldEqual true

        firstMessages
        |> List.contains "second cached assembly logger"
        |> shouldEqual false

        secondMessages
        |> List.contains "first cached assembly logger"
        |> shouldEqual false

        secondMessages
        |> List.contains "second cached assembly logger"
        |> shouldEqual true

    [<Test>]
    let ``disposing a readFile result does not poison the cache`` () : unit =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        let first = readAssembly loggerFactory corelibPath

        (first :> IDisposable).Dispose ()

        let second = readAssembly loggerFactory corelibPath

        second.Name.FullName |> shouldEqual first.Name.FullName
        second.PeReader.GetMetadata().Length > 0 |> shouldEqual true

    [<Test>]
    let ``read does not cache non-file streams by original path`` () : unit =
        let firstBytes = compileDll "AssemblyReadCacheMemoryFirst"
        let secondBytes = compileDll "AssemblyReadCacheMemorySecond"
        let _, loggerFactory = LoggerFactory.makeTest ()

        use firstStream = new MemoryStream (firstBytes)
        use secondStream = new MemoryStream (secondBytes)

        let first =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory (Some "same-original-path.dll") firstStream

        let second =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory (Some "same-original-path.dll") secondStream

        first.Name.Name |> shouldEqual "AssemblyReadCacheMemoryFirst"
        second.Name.Name |> shouldEqual "AssemblyReadCacheMemorySecond"
        Object.ReferenceEquals (first.PeReader, second.PeReader) |> shouldEqual false

    [<Test>]
    let ``read invalidates cached file when file changes`` () : unit =
        let suffix = Guid.NewGuid().ToString "N"

        let tempPath =
            Path.Combine (Path.GetTempPath (), $"PawPrintAssemblyCache-%s{suffix}.dll")

        let firstBytes = compileDll "AssemblyReadCacheFileFirst"
        let secondBytes = compileDll "AssemblyReadCacheFileSecond"
        let _, loggerFactory = LoggerFactory.makeTest ()

        try
            File.WriteAllBytes (tempPath, firstBytes)
            File.SetLastWriteTimeUtc (tempPath, DateTime.UtcNow.AddMinutes -2.0)
            let first = readAssembly loggerFactory tempPath

            File.WriteAllBytes (tempPath, secondBytes)
            File.SetLastWriteTimeUtc (tempPath, DateTime.UtcNow.AddMinutes 2.0)
            let second = readAssembly loggerFactory tempPath

            first.Name.Name |> shouldEqual "AssemblyReadCacheFileFirst"
            second.Name.Name |> shouldEqual "AssemblyReadCacheFileSecond"
            Object.ReferenceEquals (first.PeReader, second.PeReader) |> shouldEqual false
        finally
            if File.Exists tempPath then
                File.Delete tempPath
