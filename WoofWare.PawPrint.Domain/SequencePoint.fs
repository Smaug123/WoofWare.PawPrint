namespace WoofWare.PawPrint

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open Microsoft.Extensions.Logging

/// <summary>
/// A span in a source document, as recorded by a portable PDB sequence point.
/// </summary>
type SourceLocation =
    {
        /// <summary>
        /// The document path exactly as the PDB records it, which is whatever the compiler was
        /// told the file was called: an absolute path on the machine that built the assembly for
        /// an ordinary build, or a <c>/_/</c>-rooted path when the compilation set
        /// <c>ContinuousIntegrationBuild</c>. Deliberately not resolved against the host
        /// filesystem — it frequently names a machine that is not this one, and PawPrint has no
        /// business reading host files to find out.
        /// </summary>
        DocumentPath : string

        StartLine : int
        StartColumn : int
        EndLine : int
        EndColumn : int
    }

/// <summary>
/// One portable-PDB sequence point: the start of a range of IL which the compiler has either
/// attributed to a source span, or explicitly declared to have none.
/// </summary>
[<RequireQualifiedAccess>]
type SequencePoint =
    /// <summary>
    /// IL from this point up to the next sequence point corresponds to this source span.
    /// </summary>
    | Source of SourceLocation

    /// <summary>
    /// IL from this point up to the next sequence point has deliberately been given no source
    /// attribution: the <c>0xFEEFEE</c> hidden-line sentinel, which compilers emit over
    /// generated code and which debuggers step through rather than halting in.
    /// </summary>
    /// <remarks>
    /// Represented rather than discarded at parse time. Discarding it would leave the
    /// *preceding* source span apparently covering the hidden range, so compiler-generated IL
    /// would be attributed to whichever user line happened to precede it.
    /// </remarks>
    | Hidden

/// <summary>
/// The sequence points of a single method, indexed for lookup by IL offset.
/// </summary>
type MethodSequencePoints =
    private
        {
            /// <summary>
            /// Ascending by IL offset. Where the PDB records several points at one offset, the
            /// order in which they were read is preserved, and <c>resolve</c> reports the last
            /// of them — the one in effect from that offset onwards.
            /// </summary>
            _Points : ImmutableArray<int * SequencePoint>
        }

[<RequireQualifiedAccess>]
module MethodSequencePoints =

    /// <summary>
    /// Index the supplied points for lookup. Sorting is stable, so where several points share an
    /// IL offset the input order decides which one <c>resolve</c> reports.
    /// </summary>
    let ofSeq (points : (int * SequencePoint) seq) : MethodSequencePoints =
        {
            _Points = points |> Seq.sortBy fst |> ImmutableArray.CreateRange
        }

    let toList (points : MethodSequencePoints) : (int * SequencePoint) list = List.ofSeq points._Points

    let isEmpty (points : MethodSequencePoints) : bool = points._Points.IsEmpty

    /// <summary>
    /// The source span covering <paramref name="ilOffset" />: the last sequence point at or
    /// before it. <c>None</c> when no sequence point precedes the offset (so the compiler
    /// attributed nothing to it), or when the covering point is <c>Hidden</c>.
    /// </summary>
    let resolve (ilOffset : int) (points : MethodSequencePoints) : SourceLocation option =
        let points = points._Points

        // Last index whose offset is <= ilOffset. Binary search rather than a linear scan
        // because a method's sequence point list is as long as its statement count, and
        // formatting a stack trace resolves one of these per frame.
        let mutable lo = 0
        let mutable hi = points.Length - 1
        let mutable found = -1

        while lo <= hi do
            let mid = lo + (hi - lo) / 2

            if fst points.[mid] <= ilOffset then
                found <- mid
                lo <- mid + 1
            else
                hi <- mid - 1

        if found < 0 then
            None
        else
            match snd points.[found] with
            | SequencePoint.Source loc -> Some loc
            | SequencePoint.Hidden -> None

/// <summary>
/// Reading of portable PDB debug information accompanying a PE image.
/// </summary>
[<RequireQualifiedAccess>]
module PortablePdb =

    let private describe (originalPath : string option) : string =
        match originalPath with
        | None -> "<in-memory image>"
        | Some path -> path

    /// <summary>
    /// The one side-by-side PDB path PawPrint will consider for an assembly read from
    /// <paramref name="assemblyPath" />: the conventional <c>.pdb</c> beside it, which is what
    /// the SDK's default <c>DebugType=portable</c> emits.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Deliberately a single fixed path rather than a search. It is also the assembly parse
    /// cache's notion of "the symbols for this assembly" (see <c>AssemblyFileCacheKey</c>), and
    /// that cache can only stay honest about symbols changing underneath it if the file a read
    /// could have consulted is exactly this one.
    /// </para>
    /// <para>
    /// <c>None</c> when <paramref name="assemblyPath" /> cannot be interpreted as a path at all.
    /// Callers hand us whatever they were given, which for an in-memory image may be a
    /// descriptive label rather than a filename; that must cost the symbols and nothing more.
    /// </para>
    /// </remarks>
    let sidecarPath (assemblyPath : string) : string option =
        try
            Some (Path.GetFullPath (Path.ChangeExtension (assemblyPath, ".pdb")))
        with
        | :? ArgumentException
        | :? NotSupportedException
        | :? PathTooLongException -> None

    /// <summary>
    /// Open this image's portable PDB, if it has one: a PDB embedded in the PE itself, or —
    /// when we know where the image was read from — the side-by-side <c>.pdb</c> beside it.
    /// </summary>
    let private tryOpenReaderProvider
        (logger : ILogger)
        (peReader : PEReader)
        (originalPath : string option)
        : MetadataReaderProvider option
        =
        let embedded =
            peReader.ReadDebugDirectory ()
            |> Seq.tryFind (fun entry -> entry.Type = DebugDirectoryEntryType.EmbeddedPortablePdb)

        match embedded with
        | Some entry -> Some (peReader.ReadEmbeddedPortablePdbDebugDirectoryData entry)
        | None ->

        match originalPath, Option.bind sidecarPath originalPath with
        | None, _ -> None
        | Some path, None ->
            logger.LogDebug ("Not looking for symbols: {AssemblyPath} is not usable as a filesystem path", path)

            None
        | Some path, Some wanted ->

        // TryOpenAssociatedPortablePdb checks that the PDB it is given actually belongs to this
        // image (the CodeView GUID must match), so a stale .pdb left beside a rebuilt assembly is
        // rejected rather than silently misattributing every line in it. That check is the whole
        // reason to route through SRM rather than opening the file ourselves.
        //
        // Its *search*, on the other hand, we want no part of: it offers candidate paths derived
        // from the image's own CodeView entry, including the absolute path on the machine that
        // built it. So the provider ignores what it is asked for and answers with the one file we
        // are willing to read. Filtering SRM's candidates by comparing paths would instead have
        // to decide whether this filesystem is case-sensitive — and get it right, or lose the
        // symbols of any assembly opened under different casing from the name the compiler
        // recorded.
        let opener =
            Func<string, Stream> (fun _candidate ->
                try
                    if File.Exists wanted then
                        File.OpenRead wanted :> Stream
                    else
                        null
                with
                | :? IOException
                | :? UnauthorizedAccessException ->
                    // "No readable PDB beside the assembly" is the overwhelmingly common case,
                    // not an error: null tells SRM there is nothing here.
                    null
            )

        match peReader.TryOpenAssociatedPortablePdb (path, opener) with
        | true, provider, _ when not (isNull provider) -> Some provider
        | _ ->
            logger.LogDebug ("No portable PDB found for assembly at {AssemblyPath}", path)
            None

    /// <summary>
    /// Sequence points for every method in this image that has any, keyed by method definition
    /// handle. Empty when the image carries no debug information, which is the normal case for
    /// the shared framework.
    /// </summary>
    /// <remarks>
    /// Parses eagerly and closes the PDB before returning, so the result is plain immutable data
    /// with no lifetime relationship to <paramref name="peReader" />: it stays valid after the
    /// <c>DumpedAssembly</c> holding it — and hence its <c>PEReader</c> — has been disposed.
    /// </remarks>
    let private readCore
        (logger : ILogger)
        (peReader : PEReader)
        (originalPath : string option)
        : ImmutableDictionary<ComparableMethodDefinitionHandle, MethodSequencePoints>
        =
        match tryOpenReaderProvider logger peReader originalPath with
        | None -> ImmutableDictionary.Empty
        | Some provider ->

        use provider = provider
        let pdb = provider.GetMetadataReader ()

        // One document typically backs every method in the assembly, and each sequence point
        // names it, so resolving the blob-encoded name per point would dominate this parse.
        let documentNames = Dictionary<DocumentHandle, string> ()

        let documentName (handle : DocumentHandle) : string =
            match documentNames.TryGetValue handle with
            | true, name -> name
            | false, _ ->
                let name = pdb.GetString (pdb.GetDocument handle).Name
                documentNames.[handle] <- name
                name

        let result = ImmutableDictionary.CreateBuilder ()

        for handle in pdb.MethodDebugInformation do
            let info = pdb.GetMethodDebugInformation handle

            if not info.SequencePointsBlob.IsNil then
                let points =
                    info.GetSequencePoints ()
                    |> Seq.map (fun point ->
                        if point.IsHidden then
                            point.Offset, SequencePoint.Hidden
                        else
                            let location =
                                {
                                    DocumentPath = documentName point.Document
                                    StartLine = point.StartLine
                                    StartColumn = point.StartColumn
                                    EndLine = point.EndLine
                                    EndColumn = point.EndColumn
                                }

                            point.Offset, SequencePoint.Source location
                    )
                    |> MethodSequencePoints.ofSeq

                if not (MethodSequencePoints.isEmpty points) then
                    // The MethodDebugInformation table is defined to be row-for-row parallel to
                    // the PE's MethodDef table; ToDefinitionHandle is that correspondence. Doing
                    // the row arithmetic by hand here would fail silently, by attributing one
                    // method's lines to its neighbour.
                    result.Add (ComparableMethodDefinitionHandle.Make (handle.ToDefinitionHandle ()), points)

        result.ToImmutable ()

    /// <summary>
    /// Sequence points for every method in this image that has any, keyed by method definition
    /// handle. Empty when the image carries no debug information, which is the normal case for
    /// the shared framework.
    /// </summary>
    /// <remarks>
    /// Parses eagerly and closes the PDB before returning, so the result is plain immutable data
    /// with no lifetime relationship to <paramref name="peReader" />: it stays valid after the
    /// <c>DumpedAssembly</c> holding it — and hence its <c>PEReader</c> — has been disposed.
    /// </remarks>
    let readSequencePoints
        (logger : ILogger)
        (peReader : PEReader)
        (originalPath : string option)
        : ImmutableDictionary<ComparableMethodDefinitionHandle, MethodSequencePoints>
        =
        // Symbols are a diagnostic aid, never a prerequisite, and `Assembly.read` calls this
        // unconditionally. Damaged or unreadable debug information must therefore cost us the
        // symbols and nothing else: failing here would refuse an assembly that the real runtime
        // runs perfectly well, which is a far worse outcome than an unattributed stack frame.
        //
        // The guard has to span the parse and not merely the open. A PDB whose header and
        // CodeView GUID are intact but whose heaps are damaged opens cleanly and then throws
        // from `GetDocumentName` part-way through enumeration.
        // Logged at Warning, unlike the far more common "this assembly has no PDB at all", which
        // stays at Debug: symbols that are present but unusable are rare, are never what the
        // author intended, and are exactly what someone asking "why has this frame no line
        // number?" needs to be told.
        try
            readCore logger peReader originalPath
        with
        | :? BadImageFormatException as e ->
            logger.LogWarning (
                "Ignoring malformed debug information for assembly at {AssemblyPath}: {DebugInfoError}",
                describe originalPath,
                e.Message
            )

            ImmutableDictionary.Empty
        | :? IOException as e ->
            logger.LogWarning (
                "Could not read debug information for assembly at {AssemblyPath}: {DebugInfoError}",
                describe originalPath,
                e.Message
            )

            ImmutableDictionary.Empty
        | :? UnauthorizedAccessException as e ->
            logger.LogWarning (
                "Not permitted to read debug information for assembly at {AssemblyPath}: {DebugInfoError}",
                describe originalPath,
                e.Message
            )

            ImmutableDictionary.Empty
        // `sidecarPath` has already rejected paths it cannot normalise, so these are a backstop
        // rather than a known route: SRM validates the path it is handed too, and the invariant
        // that matters is that *nothing* about optional symbols can refuse an assembly.
        | :? ArgumentException as e ->
            logger.LogWarning (
                "Could not look for debug information for assembly at {AssemblyPath}: {DebugInfoError}",
                describe originalPath,
                e.Message
            )

            ImmutableDictionary.Empty
        | :? NotSupportedException as e ->
            logger.LogWarning (
                "Could not look for debug information for assembly at {AssemblyPath}: {DebugInfoError}",
                describe originalPath,
                e.Message
            )

            ImmutableDictionary.Empty
