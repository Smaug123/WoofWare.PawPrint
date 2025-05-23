namespace WoofWare.PawPrint.Test

open System
open System.IO
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp

[<RequireQualifiedAccess>]
module Roslyn =

    /// Compiles the supplied C# source strings into an in-memory PE image.
    /// Raises if compilation fails.
    let compile (sources : string list) : byte[] =
        // Create a syntax tree per source snippet.
        let parseOptions =
            CSharpParseOptions.Default.WithLanguageVersion LanguageVersion.Preview

        let syntaxTrees : SyntaxTree[] =
            sources
            |> List.mapi (fun idx src ->
                let fileName = $"File{idx}.cs"
                CSharpSyntaxTree.ParseText (src, parseOptions, fileName)
            )
            |> List.toArray

        // Reference every assembly found in the runtime directory – crude but
        // guarantees we can resolve System.* et al.
        let runtimeDir = Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory ()

        let metadataReferences : MetadataReference[] =
            Directory.GetFiles (runtimeDir, "*.dll")
            |> Array.map (fun path -> MetadataReference.CreateFromFile path :> MetadataReference)

        let compilationOptions = CSharpCompilationOptions OutputKind.ConsoleApplication

        let compilation =
            CSharpCompilation.Create (
                assemblyName = "PawPrintTestAssembly",
                syntaxTrees = syntaxTrees,
                references = metadataReferences,
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
