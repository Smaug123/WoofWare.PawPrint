namespace WoofWare.PawPrint

open System
open System.Reflection.Metadata

/// <summary>
/// The pair of case-insensitive substring filters IlDump takes on its command
/// line. <c>None</c> means "no filter given", which matches everything.
/// </summary>
type IlDumpFilter =
    {
        /// Matched against the fully-qualified type name.
        Type : string option

        /// Matched against member names: fields, properties, events, methods.
        Member : string option
    }

[<RequireQualifiedAccess>]
module IlDumpFilter =

    /// <summary>
    /// Build a filter from raw command-line arguments. An empty argument is
    /// normalised to <c>None</c>: the only way to spell "every type, this member"
    /// on the command line is to pass an empty type argument, and that must mean
    /// "no type narrowing" rather than "a filter which happens to match all types"
    /// — the two differ in whether unmatched types still report their existence.
    /// </summary>
    let make (typeFilter : string option) (memberFilter : string option) : IlDumpFilter =
        let normalise (s : string option) : string option =
            match s with
            | Some "" -> None
            | other -> other

        {
            Type = normalise typeFilter
            Member = normalise memberFilter
        }

/// <summary>
/// Rendering for IlDump's default mode: a whole type as a sequence of lines —
/// the type header, a one-line summary of each field, property and event, and
/// the full IL of each method.
/// </summary>
/// <remarks>
/// This lives alongside <see cref="AttributeFormatting"/> rather than in the
/// IlDump executable so that it is a pure <c>string list</c>-returning function
/// the test suite can exercise directly against a real assembly.
/// </remarks>
[<RequireQualifiedAccess>]
module IlDumpRendering =

    /// Whether a name passes a filter. A filter of `None` (the user gave no such
    /// argument) admits everything.
    let matchesFilter (filter : string option) (name : string) : bool =
        match filter with
        | None -> true
        | Some filter -> name.Contains (filter, StringComparison.OrdinalIgnoreCase)

    /// Whether a type passes the filter's type component.
    let typeMatches
        (assembly : DumpedAssembly)
        (filter : IlDumpFilter)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : bool
        =
        matchesFilter filter.Type (IlFormatting.qualifyTypeName assembly.TypeDefs typeInfo)

    /// <summary>
    /// The lines IlDump's default mode emits for a single type which has already
    /// passed <see cref="typeMatches"/>, or the empty list if the type should
    /// contribute no output at all.
    /// </summary>
    /// <remarks>
    /// The type header is emitted whenever the caller narrowed the dump by type,
    /// even if no member matched: that is what makes "this type exists but has no
    /// such member" distinguishable from "there is no such type". Without a type
    /// filter the header is emitted only when some member matched, so that a bare
    /// member search across an assembly does not emit a header for each of its
    /// thousands of types.
    /// </remarks>
    let formatTypeLines
        (assembly : DumpedAssembly)
        (filter : IlDumpFilter)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : string list
        =
        let qualified = IlFormatting.qualifyTypeName assembly.TypeDefs typeInfo

        let fieldLines =
            typeInfo.Fields
            |> List.filter (fun f -> matchesFilter filter.Member f.Name)
            |> List.map (AttributeFormatting.fieldHeader assembly qualified)

        // Properties live only in the metadata reader: there is no domain type.
        let propertyLines =
            let mr = assembly.PeReader.GetMetadataReader ()
            let typeDef = mr.GetTypeDefinition typeInfo.TypeDefHandle

            [
                for propHandle in typeDef.GetProperties () do
                    let name = mr.GetString ((mr.GetPropertyDefinition propHandle).Name)

                    if matchesFilter filter.Member name then
                        yield AttributeFormatting.propertyHeader qualified name
            ]

        let eventLines =
            typeInfo.Events
            |> Seq.filter (fun e -> matchesFilter filter.Member e.Name)
            |> Seq.map (AttributeFormatting.eventHeader qualified)
            |> List.ofSeq

        // Methods are the only members with a body, so each becomes its own
        // blank-line-separated block rather than a single summary line.
        let methodBlocks =
            typeInfo.Methods
            |> List.filter (fun m -> matchesFilter filter.Member m.Name)
            |> List.map (IlFormatting.formatMethodLines assembly qualified)

        let summaryLines = fieldLines @ propertyLines @ eventLines

        if
            List.isEmpty summaryLines
            && List.isEmpty methodBlocks
            && Option.isNone filter.Type
        then
            []
        else
            [
                yield AttributeFormatting.typeHeader assembly typeInfo
                yield! summaryLines

                for block in methodBlocks do
                    yield ""
                    yield! block
            ]
