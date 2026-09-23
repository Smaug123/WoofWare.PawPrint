namespace WoofWare.PawPrint.Test

open System
open System.IO
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// <summary>
/// What a case-insensitive type-name lookup answers, and when it declines to answer at all.
/// </summary>
/// <remarks>
/// CoreCLR folds a character below 0x80 with a plain <c>A</c>-<c>Z</c> map and sends everything
/// else to the platform's casing table, and it breaks a tie between two names that fold alike by
/// an internal ordering. PawPrint reproduces the first exactly and refuses both of the others,
/// because guessing either would risk answering with a *different type* than the real runtime.
/// These pin the boundary between the three.
/// </remarks>
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestCaseInsensitiveTypeLookup =

    let private source =
        """
namespace Ci
{
    public class Target { }

    // Two names that fold alike, so a case-insensitive query for either is ambiguous.
    public class Ambig { }
    public class AMBIG { }

    // `Cafe` and `Café` are the same length and differ only where the second has a character
    // PawPrint cannot fold, so a query for one cannot be told apart from a query for the other.
    public class Cafe { }
    public class Café { }

    // A second name PawPrint cannot fold, colliding with the same query. Which of the two the
    // refusal names has to be stable across runs, so there must be more than one to choose from.
    public class CafÉ { }

    public class Outer
    {
        public class Inner { }
        public class INNER { }
        public class Solo { }

        // Carries a character outside ASCII, so a query spelling that character identically needs
        // no casing table to match it.
        public class Straße { }
    }
}

namespace CiOther
{
    public class Thing { }
}
"""

    let private facadeSource =
        """
using System.Runtime.CompilerServices;

[assembly: TypeForwardedTo(typeof(Ci.Target))]

namespace CiFacade;

public class Marker
{
}
"""

    let private libName = "CaseInsensitive.Lib"
    let private facadeName = "CaseInsensitive.Facade"

    /// The compiled library and facade, read as PawPrint reads them.
    let private withAssemblies (body : DumpedAssembly -> DumpedAssembly -> 'a) : 'a =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let libImage =
            Roslyn.compileAssembly libName OutputKind.DynamicallyLinkedLibrary [] [ source ]

        let facadeImage =
            Roslyn.compileAssembly
                facadeName
                OutputKind.DynamicallyLinkedLibrary
                [ MetadataReference.CreateFromImage libImage ]
                [ facadeSource ]

        let tempDir = Path.Combine (Path.GetTempPath (), Path.GetRandomFileName ())
        Directory.CreateDirectory tempDir |> ignore

        try
            let libPath = Path.Combine (tempDir, libName + ".dll")
            let facadePath = Path.Combine (tempDir, facadeName + ".dll")
            File.WriteAllBytes (libPath, libImage)
            File.WriteAllBytes (facadePath, facadeImage)

            body (Assembly.readFile loggerFactory libPath) (Assembly.readFile loggerFactory facadePath)
        finally
            try
                if Directory.Exists tempDir then
                    Directory.Delete (tempDir, true)
            with
            | :? IOException
            | :? UnauthorizedAccessException -> ()

    /// The default candidate set: every definition in the assembly.
    let private everyDefinition (_ : TypeInfo<GenericParamFromMetadata, TypeDefn>) : bool = true

    let private foundName (result : Result<TypeInfo<GenericParamFromMetadata, TypeDefn> option, _>) : string =
        match result with
        | Ok (Some typeInfo) -> $"%s{typeInfo.Namespace}.%s{typeInfo.Name}"
        | other -> failwith $"expected to find a type, got %O{other}"

    [<TestCase("Ci", "Target")>]
    [<TestCase("ci", "target")>]
    [<TestCase("CI", "TARGET")>]
    [<TestCase("cI", "tArGeT")>]
    [<TestCase("ciother", "thing")>]
    let ``ASCII case folds in both the namespace and the name`` (ns : string, name : string) : unit =
        // The exact-case row is deliberately in here too: an ignore-case lookup has to be a
        // superset of the case-sensitive one, not an alternative to it.
        let expected =
            if ns.ToLowerInvariant () = "ciother" then
                "CiOther.Thing"
            else
                "Ci.Target"

        withAssemblies (fun lib _facade -> lib.TryGetTopLevelTypeDefIgnoreCase ns name everyDefinition |> foundName)
        |> shouldEqual expected

    /// A miss, as `Ok None` rather than an equality comparison: `TypeInfo` carries fields with no
    /// structural equality, so the option cannot be compared directly.
    let private isMiss (result : Result<'a option, CaseInsensitiveLookupRefusal>) : bool =
        match result with
        | Ok None -> true
        | Ok (Some _) -> false
        | Error refusal -> failwith $"expected a miss, got %O{refusal}"

    [<Test>]
    let ``a name nothing folds to is simply absent`` () : unit =
        withAssemblies (fun lib _facade ->
            lib.TryGetTopLevelTypeDefIgnoreCase "ci" "nosuchtype" everyDefinition |> isMiss
        )
        |> shouldEqual true

    [<Test>]
    let ``two names that fold alike are refused rather than picked between`` () : unit =
        // Measured on .NET 10: `GetType("ci.ambig", ignoreCase: true)` returns one of these
        // deterministically, and reversing the two types' metadata order does not change which —
        // so the rule is an internal hash ordering, and any guess PawPrint made would be a guess
        // about which *type* to hand back.
        match withAssemblies (fun lib _facade -> lib.TryGetTopLevelTypeDefIgnoreCase "ci" "ambig" everyDefinition) with
        | Error (CaseInsensitiveLookupRefusal.Ambiguous (folded, candidates)) ->
            folded |> shouldEqual "ci.ambig"
            // Deliberately not sorted here: the payload itself must come back in a stable order,
            // and these scans walk an `ImmutableDictionary` whose enumeration order follows
            // per-process string hashes. Sorting in the assertion would hide that.
            candidates |> shouldEqual [ "Ci.AMBIG" ; "Ci.Ambig" ]
        | other -> failwith $"expected an ambiguity refusal, got %O{other}"

    [<Test>]
    let ``a definition the caller excludes cannot make a query ambiguous`` () : unit =
        // The exclusion has to happen inside the scan. `Ci.Ambig` and `Ci.AMBIG` collide, but a
        // caller searching only one of them has an unambiguous query — and a caller that filtered
        // the *result* instead would already have been told its query was ambiguous with a
        // definition it never considered a candidate. This is how the class loader's omission of
        // the module pseudo-row has to work.
        withAssemblies (fun lib _facade ->
            let notShouty (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>) = typeInfo.Name <> "AMBIG"

            lib.TryGetTopLevelTypeDefIgnoreCase "ci" "ambig" notShouty |> foundName
        )
        |> shouldEqual "Ci.Ambig"

    [<Test>]
    let ``a query PawPrint cannot fold is refused where it could collide`` () : unit =
        withAssemblies (fun lib _facade ->
            // `Ci.Cafe` is the same length and agrees everywhere the query is ASCII; whether `é`
            // folds to `e` is the host's casing table's business.
            match lib.TryGetTopLevelTypeDefIgnoreCase "ci" "café" everyDefinition with
            | Error (CaseInsensitiveLookupRefusal.NotAsciiFoldable (query, candidate, offending)) ->
                query |> shouldEqual "ci.café"
                candidate |> shouldEqual "Ci.Cafe"
                offending |> shouldEqual 'é'
            | other -> failwith $"expected a fold refusal, got %O{other}"

            // The namespace is compared on the same terms as the name.
            match lib.TryGetTopLevelTypeDefIgnoreCase "çi" "target" everyDefinition with
            | Error (CaseInsensitiveLookupRefusal.NotAsciiFoldable (query, candidate, offending)) ->
                query |> shouldEqual "çi.target"
                candidate |> shouldEqual "Ci.Target"
                offending |> shouldEqual 'ç'
            | other -> failwith $"expected a fold refusal, got %O{other}"
        )

    [<Test>]
    let ``a query PawPrint cannot fold is a plain miss when nothing could collide`` () : unit =
        // Real .NET answers null for all of these: no name in scope has the query's length, and
        // CoreCLR's fold maps one UTF-16 unit to one. Refusing would stop a guest over a lookup
        // whose answer is not in doubt.
        withAssemblies (fun lib facade ->
            let outer =
                lib.TryGetTopLevelTypeDef "Ci" "Outer"
                |> Option.defaultWith (fun () -> failwith "Ci.Outer is missing")

            [
                lib.TryGetTopLevelTypeDefIgnoreCase "ci" "ünknown" everyDefinition |> isMiss
                lib.TryGetNestedTypeDefIgnoreCase outer.TypeDefHandle "ünknown" |> isMiss
                facade.TryGetTopLevelExportedTypeIgnoreCase (Some "ci") "ünknown" |> isMiss
            ]
        )
        |> shouldEqual [ true ; true ; true ]

    [<Test>]
    let ``a character outside ASCII spelled identically needs no casing table`` () : unit =
        // The host's fold is a function of the character, so two identical characters fold alike
        // whatever it is; only the ASCII positions need folding here.
        withAssemblies (fun lib _facade ->
            let outer =
                lib.TryGetTopLevelTypeDef "Ci" "Outer"
                |> Option.defaultWith (fun () -> failwith "Ci.Outer is missing")

            match lib.TryGetNestedTypeDefIgnoreCase outer.TypeDefHandle "STRAßE" with
            | Ok (Some found) -> found.Name
            | other -> failwith $"expected the nested type, got %O{other}"
        )
        |> shouldEqual "Straße"

    [<Test>]
    let ``a candidate PawPrint cannot fold outranks an otherwise clean match`` () : unit =
        // `Ci.Cafe` folds to the query exactly. `Ci.Café` might also fold to it — that turns on a
        // casing table PawPrint does not have — and if it does there are two matches, so the
        // honest answer is neither of them rather than the one we happen to be sure of.
        match withAssemblies (fun lib _facade -> lib.TryGetTopLevelTypeDefIgnoreCase "ci" "cafe" everyDefinition) with
        | Error (CaseInsensitiveLookupRefusal.NotAsciiFoldable (query, candidate, offending)) ->
            query |> shouldEqual "ci.cafe"
            // Two candidates could collide here; the one named is the first by ordinal name, not
            // whichever the dictionary happened to yield.
            candidate |> shouldEqual "Ci.CafÉ"
            offending |> shouldEqual 'É'
        | other -> failwith $"expected the unfoldable candidate to win, got %O{other}"

    [<Test>]
    let ``a name PawPrint cannot fold does not poison lookups it could not have collided with`` () : unit =
        // The narrowing that makes the refusal above tolerable: `Ci.Café` is in this assembly, and
        // it stops nothing else, because CoreCLR's mapping is one character to one character and
        // so a name of a different length can never fold to this query.
        withAssemblies (fun lib _facade ->
            lib.TryGetTopLevelTypeDefIgnoreCase "ci" "target" everyDefinition |> foundName
        )
        |> shouldEqual "Ci.Target"

    [<Test>]
    let ``nested names fold too, and are ambiguous on the same terms`` () : unit =
        withAssemblies (fun lib _facade ->
            let outer =
                lib.TryGetTopLevelTypeDef "Ci" "Outer"
                |> Option.defaultWith (fun () -> failwith "Ci.Outer is missing")

            // By simple name: a nested type carries no namespace of its own in metadata.
            match lib.TryGetNestedTypeDefIgnoreCase outer.TypeDefHandle "solo" with
            | Ok (Some solo) -> solo.Name |> shouldEqual "Solo"
            | other -> failwith $"expected the nested type, got %O{other}"

            match lib.TryGetNestedTypeDefIgnoreCase outer.TypeDefHandle "inner" with
            | Error (CaseInsensitiveLookupRefusal.Ambiguous (folded, candidates)) ->
                folded |> shouldEqual "inner"
                candidates |> shouldEqual [ "INNER" ; "Inner" ]
            | other -> failwith $"expected a nested ambiguity refusal, got %O{other}"

            // A nested name is scoped to its declaring type: `Target` is top-level, not under
            // `Outer`, so folding must not reach it.
            lib.TryGetNestedTypeDefIgnoreCase outer.TypeDefHandle "target"
            |> isMiss
            |> shouldEqual true
        )

    [<Test>]
    let ``a forwarder row folds like a type definition does`` () : unit =
        // CoreCLR's case-insensitive walk goes through the same class loader as the exact one, so
        // a facade answers a folded query with the type it forwards.
        withAssemblies (fun _lib facade ->
            match facade.TryGetTopLevelExportedTypeIgnoreCase (Some "ci") "target" with
            | Ok (Some exported) ->
                exported.Name |> shouldEqual "Target"
                exported.Namespace |> shouldEqual (Some "Ci")
            | other -> failwith $"expected the forwarder row, got %O{other}"

            facade.TryGetTopLevelExportedTypeIgnoreCase (Some "ci") "nosuchtype"
            |> isMiss
            |> shouldEqual true
        )

    /// <summary>
    /// Every fold CoreCLR could apply over a small alphabet: <c>A</c>-<c>Z</c> to <c>a</c>-<c>z</c>
    /// and the rest of ASCII to itself, as <c>SIMPLE_DOWNCASE</c> fixes it, and each character
    /// outside ASCII to anything at all, because that is the host's casing table.
    /// </summary>
    let private admissibleFolds (nonAscii : char list) : (char -> char) list =
        // Two ASCII targets and two outside it, so a fold can send a non-ASCII character onto an
        // ASCII one (as U+0130 goes to `i`), onto another non-ASCII one, or leave it distinct.
        let targets = [ 'a' ; 'b' ; 'é' ; 'ñ' ]

        let rec assign (cs : char list) : Map<char, char> list =
            match cs with
            | [] -> [ Map.empty ]
            | c :: rest ->
                [
                    for m in assign rest do
                        for t in targets -> Map.add c t m
                ]

        assign nonAscii
        |> List.map (fun m (c : char) ->
            if int c >= 0x80 then
                m.[c]
            elif c >= 'A' && c <= 'Z' then
                char (int c - int 'A' + int 'a')
            else
                c
        )

    [<Test>]
    let ``the fold comparison is sound against every fold the host could apply`` () : unit =
        let alphabet = [ 'a' ; 'A' ; 'b' ; 'é' ; 'É' ; 'İ' ]
        let folds = admissibleFolds (alphabet |> List.filter (fun c -> int c >= 0x80))

        let rec stringsOfLength (n : int) : string list =
            if n = 0 then
                [ "" ]
            else
                [
                    for s in stringsOfLength (n - 1) do
                        for c in alphabet -> s + string c
                ]

        let strings =
            [
                for n in 0..3 do
                    yield! stringsOfLength n
            ]

        let hasAsciiMismatch (q : string) (c : string) : bool =
            Seq.zip q c
            |> Seq.exists (fun (x, y) -> int x < 0x80 && int y < 0x80 && folds.Head x <> folds.Head y)

        for query in strings do
            for candidate in strings do
                let equalUnder =
                    folds |> List.map (fun f -> String.map f query = String.map f candidate)

                let context = $"query %s{query}, candidate %s{candidate}"

                match AsciiCaseFold.compare query candidate with
                | FoldComparison.Matches ->
                    if not (List.forall id equalUnder) then
                        failwith $"%s{context}: called Matches, but some fold tells them apart"
                | FoldComparison.CannotMatch ->
                    if List.exists id equalUnder then
                        failwith $"%s{context}: called CannotMatch, but some fold makes them equal"
                | FoldComparison.MightMatch offending ->
                    // Matches is exact: anything every fold agrees on must be called that.
                    if List.forall id equalUnder then
                        failwith $"%s{context}: every fold agrees, but it was called MightMatch"

                    if
                        int offending < 0x80
                        || not (query.Contains offending || candidate.Contains offending)
                    then
                        failwith
                            $"%s{context}: MightMatch named %c{offending}, which is not a non-ASCII character of either"

                    // The narrowing: neither a length difference nor a mismatch at two ASCII
                    // characters can be left in doubt.
                    if query.Length <> candidate.Length then
                        failwith $"%s{context}: lengths differ, so this should be CannotMatch"

                    if hasAsciiMismatch query candidate then
                        failwith $"%s{context}: two ASCII characters differ, so this should be CannotMatch"
