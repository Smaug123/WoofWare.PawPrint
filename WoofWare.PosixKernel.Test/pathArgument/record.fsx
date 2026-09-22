// Recorded `PathArgument.parse`'s outcomes into recorded.tsv, run against the
// build of 30551a14, at which that function still decoded its bytes as UTF-8.
// `TestUnixPathBytes` holds every later build to this table.
//
// To re-run: build WoofWare.PosixKernel at that commit into some directory, and
// point the `#r` below at the DLL there. Do not re-record the table from a later
// build; that would compare the new implementation with itself.
#r "WoofWare.PosixKernel.dll"

open System
open System.Collections.Immutable
open System.Text
open WoofWare.PosixKernel

let escape (s: string) : string =
    let b = StringBuilder()

    for c in s do
        match c with
        | '\\' -> b.Append "\\\\" |> ignore
        | '\t' -> b.Append "\\t" |> ignore
        | '\n' -> b.Append "\\n" |> ignore
        | '\r' -> b.Append "\\r" |> ignore
        | '\000' -> b.Append "\\0" |> ignore
        | c -> b.Append c |> ignore

    b.ToString()

let rng = Random 20260922

let alphabet =
    [| "a"; "b"; "/"; "/"; "."; ".."; "é"; "中"; "\U0001F436"; " "; "\\" |]

let randomPath () : string =
    let n = rng.Next 12
    let nul = rng.Next 10 = 0
    let body = String.concat "" [ for _ in 1..n -> alphabet.[rng.Next alphabet.Length] ]
    // Only at a character boundary: splitting a surrogate pair would make the
    // input unencodable, which is not the domain being recorded.
    let boundaries =
        [ 0 .. body.Length ]
        |> List.filter (fun i -> i = body.Length || not (Char.IsLowSurrogate body.[i]))

    if nul then
        body.Insert(boundaries.[rng.Next boundaries.Length], "\000")
    else
        body

let nearLimit (limit: int) : string list =
    [ for total in [ limit - 2; limit - 1; limit; limit + 1 ] do
          // ASCII up to a 3-byte character that straddles the boundary, so the
          // byte count and the character count disagree right at the limit.
          yield String.replicate (total - 3) "a" + "中" ]

let corpus: string list =
    [ yield ""
      yield "/"
      yield "//"
      yield "."
      yield ".."
      yield "a\000b"
      for _ in 1..300 do
          yield randomPath ()
      yield! nearLimit 1024
      yield! nearLimit 4096 ]
    |> List.distinct

let render (outcome: Result<PathArgument, PathArgumentRefusal>) (input: byte[]) : string =
    match outcome with
    | Ok(PathArgument.Parsed path) ->
        let bytes = UnixPathText.utf8.GetBytes(UnixPath.toString path)

        if bytes = input then
            "Parsed"
        else
            failwith "parsed path differs from its input"
    | Ok(PathArgument.Failed error) -> sprintf "Failed %A" error
    | Error PathArgumentRefusal.NotUtf8 -> "NotUtf8"
    | Error(PathArgumentRefusal.InteriorNul offset) -> sprintf "InteriorNul %d" offset

let lines =
    [ for flavour, platform in
          [ "linux", SimulatedUnixPlatform.linuxX64
            "darwin", SimulatedUnixPlatform.macOsArm64 ] do
          let limits = SimulatedUnixPlatform.pathLimits platform

          for input in corpus do
              let bytes = UnixPathText.utf8.GetBytes input
              let outcome = PathArgument.parse limits (ImmutableArray.CreateRange bytes)
              yield sprintf "%s\t%s\t%s" flavour (render outcome bytes) (escape input) ]

IO.File.WriteAllLines(IO.Path.Combine(__SOURCE_DIRECTORY__, "recorded.tsv"), lines)
printfn "%d rows" lines.Length

lines
|> List.map (fun l -> l.Split('\t').[1].Split(' ').[0])
|> List.countBy id
|> printfn "%A"
