namespace WoofWare.PosixKernel.Test

open System
open System.Diagnostics
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `WoofWare.PosixKernel/README.md` is the package's README, and the only
/// account of the library a client reads before its first call. These tests
/// hold it to the library: every name it gives one of the library's members
/// resolves to a public one, every signature it shows is the function's own,
/// every syscall family's functions are all named, every refusal has the
/// `describe` it promises, and every F# code block runs and prints what its
/// `// Prints:` trailer says.
[<TestFixture>]
module TestReadme =

    let private readme : Lazy<string> =
        lazy
            let assembly = Assembly.GetExecutingAssembly ()
            let name = "WoofWare.PosixKernel.Test.README.md"

            match assembly.GetManifestResourceStream name with
            | null -> failwith $"expected the README embedded as %s{name}"
            | stream ->
                use reader = new StreamReader (stream)
                reader.ReadToEnd ()

    let private fence : Regex =
        Regex (@"^```(\w*)\r?\n(.*?)^```\s*$", RegexOptions.Multiline ||| RegexOptions.Singleline)

    /// The README without its fenced code blocks, whose backticks are not code spans.
    let private prose (text : string) : string = fence.Replace (text, "")

    /// The F# code blocks, in order.
    let private codeBlocks (text : string) : string list =
        [
            for m in fence.Matches text do
                if m.Groups.[1].Value = "fsharp" then
                    yield m.Groups.[2].Value
        ]

    let private codeSpan : Regex = Regex "`([^`\n]+)`"

    let private codeSpans (text : string) : string list =
        [
            for m in codeSpan.Matches text do
                yield m.Groups.[1].Value
        ]

    let private dottedIdentifier : Regex =
        Regex @"(?<![\w.'])([A-Z][A-Za-z0-9_]*(?:\.[A-Za-z_][A-Za-z0-9_]*)*)"

    /// Names a code span may use that are not the library's: F# and .NET's own.
    let private foreignNames : Set<string> =
        Set.ofList
            [
                "Ok"
                "Error"
                "Some"
                "None"
                "Result"
                "Map"
                "Set"
                "ImmutableArray"
            ]

    /// What is wrong with the names `span` gives the library: a dotted name
    /// whose first part names one of the library's types or modules must name a
    /// public member in full, and a single capitalised name that is not all
    /// capitals (`SIGPIPE`, `O_NONBLOCK`) must be a type, or a member or union
    /// case of one.
    let private nameFaults (names : Map<string, bool>) (lastParts : Set<string>) (span : string) : string list =
        [
            for m in dottedIdentifier.Matches span do
                let name = m.Groups.[1].Value
                let parts = name.Split '.'

                if parts.Length > 1 then
                    if Map.containsKey parts.[0] names then
                        match Map.tryFind name names with
                        | Some true -> ()
                        | Some false -> yield $"`%s{span}` names `%s{name}`, which a client cannot reach"
                        | None -> yield $"`%s{span}` names `%s{name}`, which the library does not have"
                elif
                    name |> Seq.exists Char.IsLower
                    && not (foreignNames.Contains name)
                    && not (Map.containsKey name names)
                    && not (lastParts.Contains name)
                then
                    yield $"`%s{span}` names `%s{name}`, which the library does not have"
        ]

    [<Test>]
    let ``every name the README gives the library is one a client can reach`` () : unit =
        let names = LibraryNames.namesInSource typeof<UnixError>.Assembly

        let lastParts =
            names
            |> Map.toSeq
            |> Seq.map (fun (name : string, _) -> name.Substring (name.LastIndexOf '.' + 1))
            |> Set.ofSeq

        let spans = codeSpans (prose readme.Value)

        // Controls, so that this cannot pass by reading nothing: the README names
        // these, and a name the library lacks is caught.
        spans |> shouldContain "UnixWait.wakes"
        nameFaults names lastParts "UnixWait.wakez" |> List.length |> shouldEqual 1

        nameFaults names lastParts "UnixMachineState.processorCount"
        |> List.length
        |> shouldEqual 1

        nameFaults names lastParts "WaitForIt" |> List.length |> shouldEqual 1

        spans
        |> List.collect (nameFaults names lastParts)
        |> List.distinct
        |> shouldEqual []

    /// The type arguments a README signature leaves out.
    let private elidedTypeArguments : Set<string> = Set.ofList [ "Task" ; "Handler" ]

    /// How the README writes a type: as F# source would, without the type
    /// arguments `'Task` and `'Handler` (`UnixSystem<'Task, 'Handler>` is
    /// `UnixSystem`), except as the arguments of a `Map` or `Set`.
    let rec private render (postfixArgument : bool) (t : Type) : string =
        let parenthesised (s : string) : string =
            if postfixArgument then $"(%s{s})" else s

        if t.IsGenericParameter then
            $"'%s{t.Name}"
        elif t.IsArray then
            $"%s{render true (t.GetElementType ())}[]"
        else
            let name = LibraryNames.withoutArity t.Name

            let arguments = if t.IsGenericType then t.GetGenericArguments () else [||]

            match name with
            | "Tuple"
            | "ValueTuple" -> arguments |> Array.map (render true) |> String.concat " * " |> parenthesised
            | "FSharpFunc" -> $"(%s{render true arguments.[0]} -> %s{render false arguments.[1]})"
            | "FSharpOption" -> $"%s{render true arguments.[0]} option"
            | "FSharpValueOption" -> $"%s{render true arguments.[0]} voption"
            | "FSharpList" -> $"%s{render true arguments.[0]} list"
            | _ ->
                let name =
                    match name with
                    | "FSharpResult" -> "Result"
                    | "FSharpSet" -> "Set"
                    | "FSharpMap" -> "Map"
                    | "Int16" -> "int16"
                    | "Int32" -> "int"
                    | "Int64" -> "int64"
                    | "UInt16" -> "uint16"
                    | "UInt32" -> "uint32"
                    | "UInt64" -> "uint64"
                    | "Boolean" -> "bool"
                    | "Byte" -> "byte"
                    | "String" -> "string"
                    | "Unit" -> "unit"
                    | other -> other

                let kept =
                    if name = "Map" || name = "Set" then
                        arguments
                    else
                        arguments
                        |> Array.filter (fun (a : Type) ->
                            not (a.IsGenericParameter && elidedTypeArguments.Contains a.Name)
                        )

                if kept.Length = 0 then
                    name
                else
                    let shown = kept |> Array.map (render false) |> String.concat ", "
                    $"%s{name}<%s{shown}>"

    /// The public static methods of the types a dotted name's owner names
    /// whose name in source is `name`, or which are named `name` by the compiler.
    let private methodsOf (dotted : string) (sourceName : string -> MethodInfo -> bool) : MethodInfo list =
        let split = dotted.LastIndexOf '.'
        let owner = dotted.Substring (0, split)
        let name = dotted.Substring (split + 1)

        typeof<UnixError>.Assembly.GetExportedTypes ()
        |> Array.filter (fun (t : Type) -> LibraryNames.dottedName t = owner)
        |> Array.collect (fun (t : Type) ->
            t.GetMethods (BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly)
        )
        |> Array.filter (sourceName name)
        |> List.ofArray

    /// The module functions a dotted name could mean.
    let private methodsNamed (dotted : string) : MethodInfo list =
        methodsOf
            dotted
            (fun (name : string) (m : MethodInfo) -> LibraryNames.sourceName m = name && not m.IsSpecialName)

    /// Whether a dotted name is a union case with fields, whose arguments in
    /// the README are a pattern's variables, which the reader names.
    let private isUnionCase (dotted : string) : bool =
        methodsOf
            dotted
            (fun (name : string) (m : MethodInfo) ->
                m.Name = $"New%s{name}"
                && LibraryNames.kindOf m = Some SourceConstructFlags.UnionCase
            )
        |> List.isEmpty
        |> not

    /// A code span that shows a call: a dotted name, then at least one
    /// argument, each as `(name : type)`, or each as a bare name.
    let private typedCall : Regex =
        Regex @"^([A-Z]\w*(?:\.\w+)+)((?:\s+\(\w+ : [^()]*(?:\([^()]*\)[^()]*)*\))+)$"

    let private typedArgument : Regex = Regex @"\((\w+) : ((?:[^()]|\([^()]*\))*)\)"

    let private bareCall : Regex = Regex @"^([A-Z]\w*(?:\.\w+)+)((?:\s+[a-z]\w*)+)$"

    /// The function `dotted` names, if it shows `shown` (each argument's name,
    /// with its type where the README gives one) of it: every argument, or every
    /// argument but the last, which is the system, image, launch or machine the
    /// function acts on. `Error` says what disagrees.
    let private matchCall (dotted : string) (shown : (string * string option) list) : Result<MethodInfo, string> =
        match methodsNamed dotted with
        | [ m ] ->
            let parameters =
                m.GetParameters ()
                |> Array.map (fun (p : ParameterInfo) -> p.Name, render false p.ParameterType)
                |> List.ofArray

            let fits (expected : (string * string) list) : bool =
                expected.Length = shown.Length
                && List.forall2
                    (fun (name : string, rendered : string) (shownName : string, shownType : string option) ->
                        name = shownName
                        && (shownType |> Option.forall (fun (s : string) -> s = rendered))
                    )
                    expected
                    shown

            let allButLast =
                if parameters.IsEmpty then
                    []
                else
                    List.take (parameters.Length - 1) parameters

            if fits parameters || fits allButLast then
                Ok m
            else
                let actual =
                    parameters
                    |> List.map (fun (name : string, rendered : string) -> $"(%s{name} : %s{rendered})")
                    |> String.concat " "

                Error $"`%s{dotted}` takes %s{actual}"
        | [] -> Error $"`%s{dotted}` is no public function"
        | several -> Error $"`%s{dotted}` names %d{several.Length} methods"

    /// A call the span shows, as the function's name and its arguments.
    let private parseCall (span : string) : (string * (string * string option) list) option =
        let typed = typedCall.Match span

        if typed.Success then
            let arguments =
                [
                    for a in typedArgument.Matches typed.Groups.[2].Value do
                        yield a.Groups.[1].Value, Some (a.Groups.[2].Value.Trim ())
                ]

            Some (typed.Groups.[1].Value, arguments)
        else
            let bare = bareCall.Match span

            if bare.Success then
                let arguments =
                    bare.Groups.[2].Value.Split ([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map (fun (a : string) -> a, None)
                    |> List.ofArray

                Some (bare.Groups.[1].Value, arguments)
            else
                None

    [<Test>]
    let ``every call the README shows has the arguments it shows`` () : unit =
        // Controls: a right shape, a wrong order, and a wrong type.
        matchCall "UnixReadWrite.read" [ "task", None ; "fd", None ; "buffer", None ; "count", None ]
        |> Result.isOk
        |> shouldEqual true

        matchCall "UnixReadWrite.read" [ "fd", None ; "task", None ; "buffer", None ; "count", None ]
        |> Result.isOk
        |> shouldEqual false

        matchCall "UnixReadWrite.read" [ "task", Some "'Task" ; "fd", Some "int64" ; "buffer", None ; "count", None ]
        |> Result.isOk
        |> shouldEqual false

        let calls = codeSpans (prose readme.Value) |> List.choose parseCall

        // The README shows well over a hundred: its tables of syscalls are most of them.
        calls.Length |> shouldBeGreaterThan 100

        calls
        |> List.filter (fun (dotted, _) -> not (isUnionCase dotted))
        |> List.choose (fun (dotted, shown) ->
            match matchCall dotted shown with
            | Ok _ -> None
            | Error fault -> Some fault
        )
        |> List.distinct
        |> shouldEqual []

    /// Each row of every table whose columns are `Call | Function | Answers`:
    /// its function cell and its answer cell, both without their backticks.
    let private answerRows (text : string) : (string * string) list =
        let cells (line : string) : string list =
            line.Trim().Trim('|').Split '|'
            |> Array.map (fun (c : string) -> c.Trim ())
            |> List.ofArray

        let unquote (cell : string) : string =
            match codeSpans cell with
            | [ only ] when cell = $"`%s{only}`" -> only
            | _ -> failwith $"expected one code span and nothing else in the cell %s{cell}"

        let lines = text.Split '\n' |> Array.map (fun (l : string) -> l.TrimEnd '\r')

        [
            for i in 0 .. lines.Length - 3 do
                if
                    lines.[i].StartsWith "|"
                    && cells lines.[i] = [ "Call" ; "Function" ; "Answers" ]
                then
                    let mutable j = i + 2

                    while j < lines.Length && lines.[j].StartsWith "|" do
                        match cells lines.[j] with
                        | [ _ ; f ; answer ] -> yield unquote f, unquote answer
                        | other -> failwith $"expected three cells in the row %A{other}"

                        j <- j + 1
        ]

    [<Test>]
    let ``every syscall table gives each function's own answer`` () : unit =
        let rows = answerRows (prose readme.Value)
        rows.Length |> shouldBeGreaterThan 100

        rows
        |> List.choose (fun (call : string, answer : string) ->
            // A function cell with no argument shows a function of the system alone.
            let parsed =
                match parseCall call with
                | Some parsed -> Some parsed
                | None when Regex.IsMatch (call, @"^[A-Z]\w*(?:\.\w+)+$") -> Some (call, [])
                | None -> None

            match parsed with
            | None -> Some $"`%s{call}` is not a call"
            | Some (dotted, shown) ->
                match matchCall dotted shown with
                | Error fault -> Some fault
                | Ok m ->
                    let actual = render false m.ReturnType

                    if actual = answer then
                        None
                    else
                        Some $"`%s{dotted}` answers `%s{actual}`, not `%s{answer}`"
        )
        |> shouldEqual []

    /// The modules whose functions are the syscalls, and the queries a client
    /// needs beside them. The README names each of their public functions.
    let private syscallModules : string list =
        [
            "UnixDescriptor"
            "UnixPathResolution"
            "UnixNamespace"
            "UnixReadWrite"
            "UnixPipe"
            "UnixSocket"
            "UnixConnection"
            "UnixPoll"
            "UnixKqueue"
            "UnixSignal"
            "UnixClock"
            "UnixEntropy"
            "UnixCredentials"
            "UnixTaskLifecycle"
            "UnixScheduling"
            "UnixWait"
            "SimulatedMachine"
            "EndedProcess"
        ]

    [<Test>]
    let ``the README names every function of every syscall family`` () : unit =
        let named =
            codeSpans (prose readme.Value)
            |> List.collect (fun (span : string) ->
                [
                    for m in dottedIdentifier.Matches span do
                        yield m.Groups.[1].Value
                ]
            )
            |> Set.ofList

        let functions =
            typeof<UnixError>.Assembly.GetExportedTypes ()
            |> Array.filter (fun (t : Type) ->
                LibraryNames.kindOf t = Some SourceConstructFlags.Module
                && List.contains (LibraryNames.dottedName t) syscallModules
            )
            |> Array.collect (fun (t : Type) ->
                t.GetMethods (BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly)
                |> Array.map (fun (m : MethodInfo) -> $"%s{LibraryNames.dottedName t}.%s{LibraryNames.sourceName m}")
            )
            |> Set.ofArray

        // Every module is found, so that a renamed one cannot make this pass.
        functions
        |> Set.map (fun (f : string) -> f.Substring (0, f.LastIndexOf '.'))
        |> shouldEqual (Set.ofList syscallModules)

        Set.difference functions named |> Set.toList |> shouldEqual []

    /// The error type of a public function's `Result`, and its success type,
    /// if it answers one.
    let private resultParts (t : Type) : (Type * Type) option =
        if t.IsGenericType && t.GetGenericTypeDefinition () = typedefof<Result<int, int>> then
            let arguments = t.GetGenericArguments ()
            Some (arguments.[0], arguments.[1])
        else
            None

    let private hasDescribe (t : Type) : bool =
        let definition = if t.IsGenericType then t.GetGenericTypeDefinition () else t

        typeof<UnixError>.Assembly.GetExportedTypes ()
        |> Array.filter (fun (m : Type) ->
            LibraryNames.kindOf m = Some SourceConstructFlags.Module
            && LibraryNames.dottedName m = LibraryNames.dottedName definition
        )
        |> Array.exists (fun (m : Type) ->
            m.GetMethods (BindingFlags.Public ||| BindingFlags.Static)
            |> Array.exists (fun (d : MethodInfo) ->
                d.Name = "describe"
                && d.ReturnType = typeof<string>
                && (
                    match d.GetParameters () with
                    | [| p |] ->
                        let parameter =
                            if p.ParameterType.IsGenericType then
                                p.ParameterType.GetGenericTypeDefinition ()
                            else
                                p.ParameterType

                        parameter = definition
                    | _ -> false
                )
            )
        )

    /// The error types of the parsers that build a value from raw input. They
    /// say what was wrong with the input, and are not refusals: the README
    /// says so.
    let private parseErrors : Set<string> =
        Set.ofList
            [
                "AbsoluteUnixPathError"
                "FileNameError"
                "PathFailure"
                "SimulatedUnixPlatformError"
                "SymlinkTargetError"
                "UnixByteStringDefect"
                "UnixPathError"
                "UnixPathTextDefect"
            ]

    /// The functions whose `Error` is an errno rather than a refusal, which the
    /// README names.
    let private errnoAsError : Set<string> =
        Set.ofList
            [
                "UnixSignal.sigaction"
                "UnixSignal.sigactionSyscall"
                "UnixSignal.pthreadSigmask"
                "UnixSignal.rtSigprocmask"
            ]

    [<Test>]
    let ``every refusal a public function answers has a describe`` () : unit =
        let functions =
            typeof<UnixError>.Assembly.GetExportedTypes ()
            |> Array.filter (fun (t : Type) -> LibraryNames.kindOf t = Some SourceConstructFlags.Module)
            |> Array.collect (fun (t : Type) ->
                t.GetMethods (BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly)
                |> Array.map (fun (m : MethodInfo) ->
                    $"%s{LibraryNames.dottedName t}.%s{LibraryNames.sourceName m}", m.ReturnType
                )
            )
            |> List.ofArray

        let refusals =
            functions
            |> List.choose (fun (name : string, returned : Type) ->
                resultParts returned |> Option.map (fun (_, error) -> name, error)
            )

        // Controls: a refusal with a describe, and the setter whose refusal had none.
        hasDescribe typeof<ReadRefusal> |> shouldEqual true
        refusals |> List.map fst |> shouldContain "UnixBootImage.withFileSystem"

        refusals
        |> List.filter (fun (_, error : Type) -> error = typeof<UnixError>)
        |> List.map fst
        |> Set.ofList
        |> shouldEqual errnoAsError

        refusals
        |> List.filter (fun (_, error : Type) ->
            error <> typeof<UnixError>
            && not (parseErrors.Contains (LibraryNames.withoutArity error.Name))
            && not (hasDescribe error)
        )
        |> List.map (fun (name : string, error : Type) -> $"%s{name} refuses with %s{error.Name}")
        |> shouldEqual []

    /// The lines a code block says it prints: the comment lines after a line
    /// `// Prints:` at its end.
    let private expectedOutput (block : string) : string list =
        let lines =
            block.TrimEnd().Split '\n' |> Array.map (fun (l : string) -> l.TrimEnd '\r')

        match Array.tryFindIndexBack (fun (l : string) -> l = "// Prints:") lines with
        | None -> failwith $"expected the code block to end with a `// Prints:` comment:\n%s{block}"
        | Some i ->
            lines.[i + 1 ..]
            |> Array.map (fun (l : string) -> l.Substring 3)
            |> List.ofArray

    let private codeBlockCases () : TestCaseData seq =
        codeBlocks readme.Value
        |> List.mapi (fun (i : int) (block : string) -> TestCaseData(i, block).SetName $"README code block %d{i}")
        |> Seq.ofList

    [<Test>]
    let ``the README has code blocks`` () : unit =
        codeBlocks readme.Value |> List.length |> shouldBeGreaterThan 2

    /// Each block is a script on its own, run by `dotnet fsi` against the
    /// library this test was built with, as a client pasting it would run it.
    [<TestCaseSource(nameof codeBlockCases)>]
    let ``README code block runs and prints what it says`` (index : int) (block : string) : unit =
        let expected = expectedOutput block
        expected |> shouldNotEqual []

        let directory = Directory.CreateTempSubdirectory "readme-block"

        try
            let script = Path.Combine (directory.FullName, $"block%d{index}.fsx")
            let library = typeof<UnixError>.Assembly.Location
            File.WriteAllText (script, $"#r @\"%s{library}\"\n%s{block}")

            let info = ProcessStartInfo "dotnet"
            info.ArgumentList.Add "fsi"
            info.ArgumentList.Add script
            info.RedirectStandardOutput <- true
            info.RedirectStandardError <- true
            info.UseShellExecute <- false
            info.Environment.["DOTNET_NOLOGO"] <- "1"
            info.Environment.["DOTNET_CLI_TELEMETRY_OPTOUT"] <- "1"

            use proc = Process.Start info
            let stdout = proc.StandardOutput.ReadToEndAsync ()
            let stderr = proc.StandardError.ReadToEndAsync ()

            if not (proc.WaitForExit (TimeSpan.FromMinutes 5.0)) then
                proc.Kill true
                failwith $"dotnet fsi did not finish the README's code block %d{index} in five minutes"

            let output = stdout.Result

            if proc.ExitCode <> 0 then
                failwith
                    $"dotnet fsi exited %d{proc.ExitCode} on the README's code block %d{index}:\n%s{output}\n%s{stderr.Result}"

            output.TrimEnd().Split '\n'
            |> Array.map (fun (l : string) -> l.TrimEnd '\r')
            |> List.ofArray
            |> shouldEqual expected
        finally
            directory.Delete true
