namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Text
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.Emit
open Microsoft.CodeAnalysis.Text

[<RequireQualifiedAccess>]
module Roslyn =

    let private metadataReferences (extraReferences : MetadataReference list) : MetadataReference[] =
        let runtimeDir = Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory ()

        let runtimeReferences =
            Directory.GetFiles (runtimeDir, "*.dll")
            |> Array.map (fun path -> MetadataReference.CreateFromFile path :> MetadataReference)

        Array.append runtimeReferences (extraReferences |> List.toArray)

    /// Whether the emitted image carries an embedded portable PDB.
    ///
    /// Off by default, and opt-in per case rather than globally, because debug information is
    /// *guest-observable*: with symbols present the real runtime appends `in File0.cs:line N` to
    /// every frame of `Exception.StackTrace`, while PawPrint does not. Every differential case
    /// that inspects a stack trace today happens to use positive substring checks and so
    /// survives either way, but that is luck rather than design — turning this on wholesale
    /// would silently widen a divergence the corpus is merely tolerating.
    [<RequireQualifiedAccess>]
    type DebugSymbols =
        | Embedded
        | None

    let private compileCore
        (assemblyName : string)
        (outputKind : OutputKind)
        (extraReferences : MetadataReference list)
        (resources : ResourceDescription list)
        (symbols : DebugSymbols)
        (sources : string list)
        : byte[]
        =
        let parseOptions =
            CSharpParseOptions.Default.WithLanguageVersion LanguageVersion.Preview

        let syntaxTrees : SyntaxTree[] =
            sources
            |> List.mapi (fun idx src ->
                let fileName = $"File{idx}.cs"

                // The source text must carry an encoding or emitting debug information fails
                // with CS8055: a PDB records each document's encoding so that a consumer can
                // re-read the file. Supplying it unconditionally costs nothing when no PDB is
                // emitted, and keeps a single parse path.
                CSharpSyntaxTree.ParseText (SourceText.From (src, Encoding.UTF8), parseOptions, fileName)
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

        // `Embedded` puts the PDB inside the PE, so there is no `pdbStream` to supply and no
        // second file for a caller to keep track of.
        let emitOptions =
            match symbols with
            | DebugSymbols.None -> EmitOptions ()
            | DebugSymbols.Embedded -> EmitOptions().WithDebugInformationFormat DebugInformationFormat.Embedded

        let emitResult =
            compilation.Emit (peStream, manifestResources = resources, options = emitOptions)

        if emitResult.Success then
            peStream.ToArray ()
        else
            let diagnostics =
                emitResult.Diagnostics
                |> Seq.filter (fun d -> d.Severity = DiagnosticSeverity.Error)
                |> Seq.map (fun d -> d.ToString ())
                |> String.concat Environment.NewLine

            failwith $"Compilation failed:\n{diagnostics}"

    let compileAssemblyWithResources
        (assemblyName : string)
        (outputKind : OutputKind)
        (extraReferences : MetadataReference list)
        (resources : ResourceDescription list)
        (sources : string list)
        : byte[]
        =
        compileCore assemblyName outputKind extraReferences resources DebugSymbols.None sources

    let compileAssembly
        (assemblyName : string)
        (outputKind : OutputKind)
        (extraReferences : MetadataReference list)
        (sources : string list)
        : byte[]
        =
        compileAssemblyWithResources assemblyName outputKind extraReferences [] sources

    /// Compiles the supplied C# source strings into an in-memory PE image.
    /// Raises if compilation fails.
    let compile (sources : string list) : byte[] =
        compileAssembly "PawPrintTestAssembly" OutputKind.ConsoleApplication [] sources

    /// As `compile`, but the image carries an embedded portable PDB. See `DebugSymbols` for why
    /// this is a separate entry point rather than the default.
    let compileWithSymbols (sources : string list) : byte[] =
        compileCore "PawPrintTestAssembly" OutputKind.ConsoleApplication [] [] DebugSymbols.Embedded sources
