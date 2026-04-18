namespace WoofWare.PawPrint.Test

open System
open System.IO
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp

[<RequireQualifiedAccess>]
module Roslyn =

    let private metadataReferences (extraReferences : MetadataReference list) : MetadataReference[] =
        let runtimeDir = Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory ()

        let runtimeReferences =
            Directory.GetFiles (runtimeDir, "*.dll")
            |> Array.map (fun path -> MetadataReference.CreateFromFile path :> MetadataReference)

        Array.append runtimeReferences (extraReferences |> List.toArray)

    let compileAssembly
        (assemblyName : string)
        (outputKind : OutputKind)
        (extraReferences : MetadataReference list)
        (sources : string list)
        : byte[]
        =
        let parseOptions =
            CSharpParseOptions.Default.WithLanguageVersion LanguageVersion.Preview

        let syntaxTrees : SyntaxTree[] =
            sources
            |> List.mapi (fun idx src ->
                let fileName = $"File{idx}.cs"
                CSharpSyntaxTree.ParseText (src, parseOptions, fileName)
            )
            |> List.toArray

        let compilationOptions = CSharpCompilationOptions(outputKind).WithAllowUnsafe true

        let compilation =
            CSharpCompilation.Create (
                assemblyName = assemblyName,
                syntaxTrees = syntaxTrees,
                references = metadataReferences extraReferences,
                options = compilationOptions
            )

        use peStream = new MemoryStream ()

        let emitResult = compilation.Emit peStream

        if emitResult.Success then
            peStream.ToArray ()
        else
            let diagnostics =
                emitResult.Diagnostics
                |> Seq.filter (fun d -> d.Severity = DiagnosticSeverity.Error)
                |> Seq.map (fun d -> d.ToString ())
                |> String.concat Environment.NewLine

            failwith $"Compilation failed:\n{diagnostics}"

    /// Compiles the supplied C# source strings into an in-memory PE image.
    /// Raises if compilation fails.
    let compile (sources : string list) : byte[] =
        compileAssembly "PawPrintTestAssembly" OutputKind.ConsoleApplication [] sources
