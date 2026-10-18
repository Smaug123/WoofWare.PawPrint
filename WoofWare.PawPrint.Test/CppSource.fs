namespace WoofWare.PawPrint.Test

open System
open System.Collections.Generic
open System.Text
open System.Text.RegularExpressions

/// A routine `CppSource` finds in a file of the runtime's source: a C++ function, or an assembly
/// routine or macro.
type SourceRoutine =
    {
        /// Its name, qualified by the classes its definition is nested in and by the qualifier the
        /// definition spells, such as `ObjHeader::EnterObjMonitor`.
        Name : string
        /// The names it calls, each with the qualifier the call spells, such as `GetThread` or
        /// `CoreLibBinder::GetMethod`. A call through a pointer or a reference is by the member's
        /// name alone. A variable declared with arguments, such as `MethodDescCallSite site(id)`,
        /// calls its type's constructor, which is named by its type. Calls the text does not spell
        /// are absent: a destructor, an implicit conversion, an overloaded operator, and a
        /// constructor run by a declaration without arguments.
        Calls : Set<string>
        /// Its text, without comments, string and character literals, or preprocessor lines.
        Text : string
    }

/// Reads routines out of the runtime's C++ and assembly by their text alone, with no preprocessor
/// and no name resolution. Both arms of every `#if` are read, so a routine calls whatever either
/// arm calls.
[<RequireQualifiedAccess>]
module CppSource =

    /// The text without comments or string and character literals, with one space for each
    /// comment and `""` for each literal. Each preprocessor line is kept whole, on one line, with
    /// the lines its trailing backslashes continue it onto.
    let private withoutComments (text : string) : string =
        let result = StringBuilder text.Length
        let mutable i = 0
        let mutable atLineStart = true

        while i < text.Length do
            let c = text.[i]
            let next = if i + 1 < text.Length then text.[i + 1] else '\000'

            if atLineStart && c = '#' then
                while i < text.Length && not (text.[i] = '\n' && text.[i - 1] <> '\\') do
                    if
                        text.[i] <> '\n'
                        && not (text.[i] = '\\' && i + 1 < text.Length && text.[i + 1] = '\n')
                    then
                        result.Append text.[i] |> ignore<StringBuilder>

                    i <- i + 1
            elif c = '/' && next = '/' then
                while i < text.Length && text.[i] <> '\n' do
                    i <- i + 1
            elif c = '/' && next = '*' then
                i <- i + 2

                while i < text.Length
                      && not (text.[i] = '*' && i + 1 < text.Length && text.[i + 1] = '/') do
                    i <- i + 1

                i <- i + 2
                result.Append ' ' |> ignore<StringBuilder>
            // A quote after a digit separates the digits of a number, as in 1'000.
            elif c = '"' || (c = '\'' && not (i > 0 && Char.IsDigit text.[i - 1])) then
                i <- i + 1

                while i < text.Length && text.[i] <> c do
                    if text.[i] = '\\' then
                        i <- i + 1

                    i <- i + 1

                i <- i + 1
                result.Append "\"\"" |> ignore<StringBuilder>
            else
                result.Append c |> ignore<StringBuilder>

                if c = '\n' then
                    atLineStart <- true
                elif not (Char.IsWhiteSpace c) then
                    atLineStart <- false

                i <- i + 1

        result.ToString ()

    /// A line of text, or a conditional group of the preprocessor: `#if` and its arms.
    type private Conditional =
        | Line of string
        | Group of arms : Conditional list list

    let private directive : Regex = Regex @"^\s*#\s*(\w*)"

    /// The lines as conditional groups, each line a preprocessor line only if it opens, divides
    /// or closes a group. An unclosed group ends at the end of the text.
    let private conditionals (lines : string list) : Conditional list =
        // For each open group, outermost last, its finished arms and the items of its current
        // arm; the text outside every group is the bottom entry's one arm.
        let mutable open' : (Conditional list list * Conditional list) list = [ ([], []) ]

        let close () : unit =
            match open' with
            | (arms, items) :: (parentArms, parentItems) :: rest ->
                let group = Conditional.Group (List.rev (List.rev items :: arms))
                open' <- (parentArms, group :: parentItems) :: rest
            | _ -> failwith "unreachable: the text outside every group cannot close"

        for line in lines do
            match (directive.Match line).Groups.[1].Value with
            | "if"
            | "ifdef"
            | "ifndef" -> open' <- ([], []) :: open'
            | "elif"
            | "elifdef"
            | "elifndef"
            | "else" ->
                match open' with
                | [ _ ] -> () // A stray divider divides nothing.
                | (arms, items) :: rest -> open' <- (List.rev items :: arms, []) :: rest
                | [] -> failwith "unreachable: the text outside every group is always open"
            | "endif" ->
                match open' with
                | [ _ ] -> ()
                | _ -> close ()
            | _ when directive.IsMatch line -> ()
            | _ ->
                match open' with
                | (arms, items) :: rest -> open' <- (arms, Conditional.Line line :: items) :: rest
                | [] -> failwith "unreachable: the text outside every group is always open"

        // An unclosed group ends at the end of the text.
        while open'.Length > 1 do
            close ()

        match open' with
        | [ ([], items) ] -> List.rev items
        | _ -> failwith "unreachable: the text outside every group has one arm"

    let rec private netBraces (items : Conditional list) : int =
        items
        |> List.sumBy (fun item ->
            match item with
            | Conditional.Line line ->
                (line |> Seq.filter ((=) '{') |> Seq.length)
                - (line |> Seq.filter ((=) '}') |> Seq.length)
            | Conditional.Group arms ->
                match arms |> List.map netBraces with
                | nets when nets |> List.forall ((=) 0) -> 0
                | first :: _ -> first
                | [] -> 0
        )

    /// The arms' text in order. Where some arm of a group opens more braces than it closes, or
    /// closes more than it opens, as `#if A` `if (a) {` `#else` `if (b) {` `#endif` does, the
    /// braces of every arm but the first are blanked, so the braces nest as in the first arm.
    let rec private render (items : Conditional list) : string list =
        items
        |> List.collect (fun item ->
            match item with
            | Conditional.Line line -> [ line ]
            | Conditional.Group arms ->
                let balanced = arms |> List.forall (fun arm -> netBraces arm = 0)

                arms
                |> List.mapi (fun index arm ->
                    let lines = render arm

                    if balanced || index = 0 then
                        lines
                    else
                        lines |> List.map (fun line -> line.Replace('{', ' ').Replace ('}', ' '))
                )
                |> List.concat
        )

    /// The text without comments, string and character literals, or preprocessor lines, with
    /// one space for each comment and `""` for each literal. Every arm of every `#if` is kept;
    /// where the arms of one would nest braces differently, every arm's braces but the first's are
    /// blanked.
    let strip (text : string) : string =
        (withoutComments text).Split '\n'
        |> List.ofArray
        |> conditionals
        |> render
        |> String.concat "\n"

    /// Words that a `(` can follow without the text calling anything by that name.
    let private notCalls : Set<string> =
        set
            [
                "new"
                "void"
                "alignas"
                "alignof"
                "case"
                "catch"
                "const_cast"
                "decltype"
                "defined"
                "delete"
                "do"
                "dynamic_cast"
                "else"
                "for"
                "if"
                "noexcept"
                "operator"
                "reinterpret_cast"
                "return"
                "sizeof"
                "static_assert"
                "static_cast"
                "switch"
                "throw"
                "typeid"
                "while"
            ]

    /// A name, with any `::`-separated qualifier, then any template arguments without nested
    /// angle brackets, then `(`.
    let private call : Regex =
        Regex @"([A-Za-z_]\w*(?:\s*::\s*~?[A-Za-z_]\w*)*)\s*(?:<[^<>;(){}]*>\s*)?\("

    /// A type then a variable it declares with arguments, as in `MethodDescCallSite site(id)`.
    let private declaration : Regex =
        Regex @"([A-Za-z_]\w*(?:\s*::\s*[A-Za-z_]\w*)*)(?:\s*<[^<>;(){}]*>)?\s+([A-Za-z_]\w*)\s*\("

    let private whitespace : Regex = Regex @"\s+"

    /// Words that can precede a call without being a type it declares a variable of.
    let private notTypes : Set<string> =
        Set.union notCalls (set [ "const" ; "co_return" ; "goto" ; "new" ; "volatile" ])

    /// The calls `text` spells, as `SourceRoutine.Calls` describes them.
    let calls (text : string) : Set<string> =
        let called =
            [
                for m in call.Matches text do
                    let name = whitespace.Replace (m.Groups.[1].Value, "")

                    if not (notCalls.Contains name) then
                        yield name
            ]

        let declared =
            [
                for m in declaration.Matches text do
                    let typeName = whitespace.Replace (m.Groups.[1].Value, "")

                    if not (notTypes.Contains typeName) then
                        yield typeName, m.Groups.[2].Value
            ]

        let variables = declared |> List.map snd |> Set.ofList

        // A variable declared with arguments is a constructor call, not a call by its own name;
        // a later call by that name is through the variable's `operator()`, which is not spelled.
        Set.union
            (Set.ofList called |> Set.filter (fun name -> not (variables.Contains name)))
            (declared |> List.map fst |> Set.ofList)

    /// The index of the `}` that closes the `{` at `start`.
    let private closing (text : string) (start : int) : int =
        let mutable depth = 0
        let mutable i = start
        let mutable found = -1

        while found < 0 && i < text.Length do
            match text.[i] with
            | '{' -> depth <- depth + 1
            | '}' ->
                depth <- depth - 1

                if depth = 0 then
                    found <- i
            | _ -> ()

            i <- i + 1

        if found < 0 then
            failwith $"No brace closes the one at offset %d{start}"

        found

    /// The index of the `)` that closes the `(` at `start`.
    let private closingParen (text : string) (start : int) : int =
        let mutable depth = 0
        let mutable i = start
        let mutable found = -1

        while found < 0 && i < text.Length do
            match text.[i] with
            | '(' -> depth <- depth + 1
            | ')' ->
                depth <- depth - 1

                if depth = 0 then
                    found <- i
            | _ -> ()

            i <- i + 1

        found

    let private scopeHeader : Regex =
        Regex @"(?:^|\s)(?:class|struct|union)\s+(?:[A-Z_]+\s+)*([A-Za-z_]\w*)[^()]*$"

    let private namespaceOrLinkage : Regex =
        Regex @"^(?:(?:inline\s+)?namespace\b[^()]*|extern\s*""""|enum\b[^()]*)$"

    /// The name before the first `(` of a function definition's header, with its qualifier.
    let private definedName : Regex =
        Regex @"(~?[A-Za-z_]\w*(?:\s*::\s*~?[A-Za-z_]\w*)*)\s*$"

    /// A macro that defines a function, as `FCIMPL1(INT32, ObjectNative::TryGetHashCode, Object* obj)`
    /// does, taking its return type and then its name.
    let private definingMacro : Regex = Regex @"^[A-Z][A-Z0-9]*IMPL\w*$"

    /// Attributes a function's header can spell before its name, whose parentheses are not its
    /// parameters'.
    let private attribute : Regex =
        Regex @"\b(?:__declspec|__attribute__|DECLSPEC_\w+|alignas)\s*\((?:[^()]|\([^()]*\))*\)"

    let private templateArguments : Regex = Regex @"<[^<>]*>"

    /// The text before a header's first `(`, without template arguments.
    let private beforeParameters (header : string) (paren : int) : string =
        let mutable before = header.Substring (0, paren)
        let mutable previous = ""

        while before <> previous do
            previous <- before
            before <- templateArguments.Replace (before, "")

        before

    /// The C++ functions `text` defines, after `strip`. A function defined in a class's body is
    /// qualified by the class.
    let definitions (text : string) : SourceRoutine list =
        let text = strip text
        let found = ResizeArray<SourceRoutine> ()
        // The class each enclosing brace opens, or None for any other scope.
        let scopes = Stack<string option> ()
        let mutable headerStart = 0
        let mutable i = 0

        // Whether the semicolon at `semicolon` ends a defining macro's header that a body follows,
        // as `FCIMPL1(...);` then `{` compiles, the macro opening no scope of its own.
        let endsDefiningHeader (semicolon : int) : bool =
            let header = text.Substring(headerStart, semicolon - headerStart).Trim ()
            let paren = header.IndexOf '('

            let next =
                let mutable j = semicolon + 1

                while j < text.Length && Char.IsWhiteSpace text.[j] do
                    j <- j + 1

                j

            paren > 0
            && definingMacro.IsMatch (
                whitespace.Replace ((definedName.Match (beforeParameters header paren)).Value, "")
            )
            && next < text.Length
            && text.[next] = '{'

        while i < text.Length do
            match text.[i] with
            | ';' ->
                if not (endsDefiningHeader i) then
                    headerStart <- i + 1

                i <- i + 1
            | '}' ->
                if scopes.Count = 0 then
                    failwith $"An unopened brace closes at offset %d{i}"

                scopes.Pop () |> ignore<string option>
                headerStart <- i + 1
                i <- i + 1
            | '{' ->
                let header =
                    attribute.Replace(text.Substring (headerStart, i - headerStart), "").Trim ()

                let scope = scopeHeader.Match header
                let paren = header.IndexOf '('
                let isOperator = Regex.IsMatch (header, @"\boperator\b")

                // An `=` before the parameters is an initialiser's, as in `auto f = [](int a) {`.
                if
                    not isOperator
                    && (scope.Success
                        || namespaceOrLinkage.IsMatch header
                        || paren < 0
                        || (beforeParameters header paren).Contains '=')
                then
                    scopes.Push (if scope.Success then Some scope.Groups.[1].Value else None)
                    headerStart <- i + 1
                    i <- i + 1
                else
                    let close = closing text i

                    let name =
                        let spelled =
                            if isOperator then
                                ""
                            else
                                whitespace.Replace ((definedName.Match (beforeParameters header paren)).Value, "")

                        if definingMacro.IsMatch spelled then
                            let arguments = header.Substring(paren + 1).Split ','

                            if arguments.Length >= 2 then
                                whitespace.Replace (arguments.[1].Trim().TrimEnd ')', "")
                            else
                                ""
                        else
                            spelled

                    if name <> "" && not (notCalls.Contains name) then
                        let enclosing = scopes |> Seq.choose id |> Seq.rev |> List.ofSeq

                        let qualified =
                            match enclosing with
                            | [] -> name
                            | classes -> String.concat "::" classes + "::" + name

                        // A constructor's initialisers follow its parameters.
                        let parametersEnd = closingParen header paren

                        let initialisers =
                            if parametersEnd < 0 then
                                ""
                            else
                                header.Substring (parametersEnd + 1)

                        let body = text.Substring (i, close - i + 1)

                        found.Add
                            {
                                Name = qualified
                                Calls = calls (initialisers + "\n" + body)
                                Text = body
                            }

                    headerStart <- close + 1
                    i <- close + 1
            | _ -> i <- i + 1

        if scopes.Count <> 0 then
            failwith $"%d{scopes.Count} braces are open at the end of the text"

        List.ofSeq found

    let private routineStart : Regex =
        Regex (@"^\s*(?:LEAF|NESTED)_ENTRY\s+(\w+)", RegexOptions.Multiline)

    let private macroStart : Regex =
        Regex (@"^\s*\.macro\s+(\w+)", RegexOptions.Multiline)

    /// A routine `C_FUNC` names, or one a branch names directly.
    let private assemblyCall : Regex =
        Regex @"C_FUNC\((\w+)\)|\b(?:bl|b|call|jmp)\s+([A-Za-z_]\w*)\s*$"

    /// A branch through a register or memory, whose target the text does not name.
    let private indirectCall : Regex =
        Regex @"(?:^|\s)(?:blr|br)\s|\b(?:call|jmp)\s+(?:\*|\[|qword\b|r[a-z0-9]+\b|e[a-z]{2}\b)"

    /// A macro the line invokes: the word that starts it, if upper-case or spelled with an
    /// underscore, which no instruction of x64 or arm64 is, and not a label.
    let private macroUse : Regex = Regex @"^\s*([A-Z][A-Z0-9_]+|[a-z]\w*_\w*)\b(?!\s*:)"

    /// The routines and macros a GNU assembler file (`.S`) defines, after `strip`. A routine
    /// calls each routine it branches to by name, and each macro it invokes; a branch through a
    /// register is a call of `<indirect>`.
    let assemblyRoutines (text : string) : SourceRoutine list =
        let lines = (strip text).Split '\n'

        let routine (name : string) (body : string list) : SourceRoutine =
            let called =
                [
                    for line in body do
                        if indirectCall.IsMatch line then
                            yield "<indirect>"

                        for m in assemblyCall.Matches line do
                            let target =
                                if m.Groups.[1].Success then
                                    m.Groups.[1].Value
                                else
                                    m.Groups.[2].Value

                            if target <> "LOCAL_LABEL" then
                                yield target

                        let used = macroUse.Match line

                        if used.Success && used.Groups.[1].Value <> "LOCAL_LABEL" then
                            yield used.Groups.[1].Value
                ]

            {
                Name = name
                Calls = Set.ofList called |> Set.remove name
                Text = String.concat "\n" body
            }

        [
            for index, line in Seq.indexed lines do
                let started = routineStart.Match line

                if started.Success then
                    let name = started.Groups.[1].Value

                    let finished = Regex $@"^\s*(?:LEAF|NESTED)_END\s+%s{name}\b"

                    let body =
                        lines
                        |> Seq.skip (index + 1)
                        |> Seq.takeWhile (fun line -> not (finished.IsMatch line))
                        |> List.ofSeq

                    yield routine name body

                let macro = macroStart.Match line

                if macro.Success then
                    let body =
                        lines
                        |> Seq.skip (index + 1)
                        |> Seq.takeWhile (fun line -> not (Regex.IsMatch (line, @"^\s*\.endm\b")))
                        |> List.ofSeq

                    yield routine macro.Groups.[1].Value body
        ]

    /// Whether a symbol, as the contract table cites one, names what a call spells: the call's
    /// name with its qualifier, or the symbol qualified further. A call spelling a type, as a
    /// declaration with arguments does, is also named by that type's constructor.
    let names (symbol : string) (called : string) : bool =
        let named (spelled : string) : bool =
            symbol = spelled || symbol.EndsWith ("::" + spelled, StringComparison.Ordinal)

        let typeName = called.Substring (called.LastIndexOf ':' + 1)
        named called || named (called + "::" + typeName)
