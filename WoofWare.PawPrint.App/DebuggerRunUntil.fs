namespace WoofWare.PawPrint

open System
open System.Text.Json

[<RequireQualifiedAccess>]
module DebuggerRunUntil =
    let private threadIdValue (threadId : ThreadId) : int =
        match threadId with
        | ThreadId.ThreadId i -> i

    let private frameIdValue (frameId : FrameId) : int =
        match frameId with
        | FrameId.FrameId i -> i

    [<RequireQualifiedAccess>]
    type DebuggerSessionStatus =
        | Running
        | Finished
        | Deadlocked

    [<RequireQualifiedAccess>]
    type StringMatcher =
        | Exact of string
        | Contains of string
        | StartsWith of string
        | EndsWith of string

    [<RequireQualifiedAccess>]
    type RunUntilPredicate =
        | And of RunUntilPredicate list
        | Or of RunUntilPredicate list
        | Not of RunUntilPredicate
        | Thread of ThreadId * RunUntilPredicate
        | AnyThread of RunUntilPredicate
        | SessionStatusChanged
        | ActiveMethodMatches of StringMatcher
        | FrameDepthAtLeast of int
        | ThreadStatusChanged
        | RepeatedActiveLocation of repeatCount : int

    type RunUntilRequest =
        {
            MaxSteps : int
            RecordLimit : int
            Predicate : RunUntilPredicate
        }

    type ParseError =
        {
            Path : string
            Message : string
        }

    type ThreadSnapshot =
        {
            Thread : ThreadId
            Status : ThreadStatus
            ActiveFrame : FrameId
            ActiveMethod : string
            ActiveIlOffset : int
            ActiveInstruction : string option
            FrameDepth : int
        }

    type SessionSnapshot =
        {
            Status : DebuggerSessionStatus
            Threads : Map<ThreadId, ThreadSnapshot>
        }

    type ActiveLocation =
        {
            Method : string
            IlOffset : int
            Instruction : string option
        }

    type EvaluationState =
        {
            BaselineStatus : DebuggerSessionStatus
            BaselineThreadStatuses : Map<ThreadId, ThreadStatus>
            ActiveLocationCounts : Map<string * ThreadId * ActiveLocation, int>
        }

    type PredicateMatch =
        {
            Path : string
            Kind : string
            Detail : string
            Thread : int option
            Frame : int option
            Method : string option
            IlOffset : int option
            Instruction : string option
            FrameDepth : int option
            RepeatCount : int option
            StatusBefore : string option
            StatusAfter : string option
        }

    type EvaluationResult =
        {
            State : EvaluationState
            Matches : PredicateMatch list
        }

    let private parseError (path : string) (message : string) : Result<'a, ParseError list> =
        Error
            [
                {
                    Path = path
                    Message = message
                }
            ]

    let private tryProperty (name : string) (element : JsonElement) : JsonElement option =
        let mutable property = Unchecked.defaultof<JsonElement>

        if
            element.ValueKind = JsonValueKind.Object
            && element.TryGetProperty (name, &property)
        then
            Some property
        else
            None

    let private requireProperty
        (path : string)
        (name : string)
        (element : JsonElement)
        : Result<JsonElement, ParseError list>
        =
        match tryProperty name element with
        | Some property -> Ok property
        | None -> parseError path $"missing required property '%s{name}'"

    let private requireStringProperty
        (path : string)
        (name : string)
        (element : JsonElement)
        : Result<string, ParseError list>
        =
        match requireProperty path name element with
        | Error errors -> Error errors
        | Ok property ->
            if property.ValueKind = JsonValueKind.String then
                match property.GetString () with
                | null -> parseError $"%s{path}.%s{name}" "expected a non-null string"
                | value -> Ok value
            else
                parseError $"%s{path}.%s{name}" "expected a string"

    let private parsePositiveIntProperty
        (path : string)
        (name : string)
        (maximum : int)
        (element : JsonElement)
        : Result<int, ParseError list>
        =
        match requireProperty path name element with
        | Error errors -> Error errors
        | Ok property ->
            let mutable value = 0

            if
                property.ValueKind = JsonValueKind.Number
                && property.TryGetInt32 &value
                && value > 0
            then
                Ok (min value maximum)
            else
                parseError $"%s{path}.%s{name}" "expected a positive integer"

    let private parseNonNegativeIntProperty
        (path : string)
        (name : string)
        (maximum : int)
        (element : JsonElement)
        : Result<int, ParseError list>
        =
        match requireProperty path name element with
        | Error errors -> Error errors
        | Ok property ->
            let mutable value = 0

            if
                property.ValueKind = JsonValueKind.Number
                && property.TryGetInt32 &value
                && value >= 0
            then
                Ok (min value maximum)
            else
                parseError $"%s{path}.%s{name}" "expected a non-negative integer"

    let private parseOptionalPositiveIntProperty
        (path : string)
        (name : string)
        (defaultValue : int)
        (maximum : int)
        (element : JsonElement)
        : Result<int, ParseError list>
        =
        match tryProperty name element with
        | None -> Ok defaultValue
        | Some property ->
            let mutable value = 0

            if
                property.ValueKind = JsonValueKind.Number
                && property.TryGetInt32 &value
                && value > 0
            then
                Ok (min value maximum)
            else
                parseError $"%s{path}.%s{name}" "expected a positive integer"

    let private collectResults (results : Result<'a, ParseError list> list) : Result<'a list, ParseError list> =
        let folder
            (result : Result<'a, ParseError list>)
            (state : 'a list * ParseError list)
            : 'a list * ParseError list
            =
            let values, errors = state

            match result with
            | Ok value -> value :: values, errors
            | Error nextErrors -> values, nextErrors @ errors

        let values, errors = (results, ([], [])) ||> List.foldBack folder

        match errors with
        | [] -> Ok values
        | errors -> Error errors

    let private parseStringMatcher (path : string) (element : JsonElement) : Result<StringMatcher, ParseError list> =
        if element.ValueKind <> JsonValueKind.Object then
            parseError path "expected an object"
        else
            match requireStringProperty path "kind" element, requireStringProperty path "value" element with
            | Ok kind, Ok value ->
                if String.IsNullOrEmpty value then
                    parseError $"%s{path}.value" "expected a non-empty string"
                else
                    match kind with
                    | "exact" -> Ok (StringMatcher.Exact value)
                    | "contains" -> Ok (StringMatcher.Contains value)
                    | "startsWith" -> Ok (StringMatcher.StartsWith value)
                    | "endsWith" -> Ok (StringMatcher.EndsWith value)
                    | _ -> parseError $"%s{path}.kind" $"unknown string matcher kind '%s{kind}'"
            | Error errors, Ok _
            | Ok _, Error errors -> Error errors
            | Error left, Error right -> Error (left @ right)

    let rec private parsePredicate
        (path : string)
        (isThreadScoped : bool)
        (element : JsonElement)
        : Result<RunUntilPredicate, ParseError list>
        =
        if element.ValueKind <> JsonValueKind.Object then
            parseError path "expected an object"
        else
            match requireStringProperty path "kind" element with
            | Error errors -> Error errors
            | Ok kind ->
                match kind with
                | "and"
                | "or" ->
                    match requireProperty path "conditions" element with
                    | Error errors -> Error errors
                    | Ok conditions ->
                        if conditions.ValueKind <> JsonValueKind.Array then
                            parseError $"%s{path}.conditions" "expected an array"
                        else
                            let children = conditions.EnumerateArray () |> Seq.toList

                            if List.isEmpty children then
                                parseError $"%s{path}.conditions" "expected at least one condition"
                            else
                                children
                                |> List.mapi (fun i child ->
                                    parsePredicate $"%s{path}.conditions[%d{i}]" isThreadScoped child
                                )
                                |> collectResults
                                |> Result.map (fun children ->
                                    if kind = "and" then
                                        RunUntilPredicate.And children
                                    else
                                        RunUntilPredicate.Or children
                                )
                | "not" ->
                    match requireProperty path "condition" element with
                    | Error errors -> Error errors
                    | Ok condition ->
                        condition
                        |> parsePredicate $"%s{path}.condition" isThreadScoped
                        |> Result.map RunUntilPredicate.Not
                | "thread" ->
                    match
                        parseNonNegativeIntProperty path "thread" Int32.MaxValue element,
                        requireProperty path "condition" element
                    with
                    | Ok thread, Ok condition ->
                        condition
                        |> parsePredicate $"%s{path}.condition" true
                        |> Result.map (fun condition -> RunUntilPredicate.Thread (ThreadId.ThreadId thread, condition))
                    | Error errors, Ok _
                    | Ok _, Error errors -> Error errors
                    | Error left, Error right -> Error (left @ right)
                | "anyThread" ->
                    match requireProperty path "condition" element with
                    | Error errors -> Error errors
                    | Ok condition ->
                        condition
                        |> parsePredicate $"%s{path}.condition" true
                        |> Result.map RunUntilPredicate.AnyThread
                | "sessionStatusChanged" -> Ok RunUntilPredicate.SessionStatusChanged
                | "activeMethodMatches" ->
                    if not isThreadScoped then
                        parseError path "activeMethodMatches must be inside a thread or anyThread condition"
                    else
                        match requireProperty path "match" element with
                        | Error errors -> Error errors
                        | Ok matcher ->
                            matcher
                            |> parseStringMatcher $"%s{path}.match"
                            |> Result.map RunUntilPredicate.ActiveMethodMatches
                | "frameDepthAtLeast" ->
                    if not isThreadScoped then
                        parseError path "frameDepthAtLeast must be inside a thread or anyThread condition"
                    else
                        element
                        |> parsePositiveIntProperty path "depth" Int32.MaxValue
                        |> Result.map RunUntilPredicate.FrameDepthAtLeast
                | "threadStatusChanged" ->
                    if not isThreadScoped then
                        parseError path "threadStatusChanged must be inside a thread or anyThread condition"
                    else
                        Ok RunUntilPredicate.ThreadStatusChanged
                | "repeatedActiveLocation" ->
                    if not isThreadScoped then
                        parseError path "repeatedActiveLocation must be inside a thread or anyThread condition"
                    else
                        element
                        |> parsePositiveIntProperty path "repeatCount" Int32.MaxValue
                        |> Result.map RunUntilPredicate.RepeatedActiveLocation
                | _ -> parseError $"%s{path}.kind" $"unknown predicate kind '%s{kind}'"

    let parseRequestJson (json : string) : Result<RunUntilRequest, ParseError list> =
        if String.IsNullOrWhiteSpace json then
            parseError "$" "expected a JSON request body"
        else
            try
                use document = JsonDocument.Parse json
                let root = document.RootElement

                if root.ValueKind <> JsonValueKind.Object then
                    parseError "$" "expected a JSON object"
                else
                    match
                        parseOptionalPositiveIntProperty "$" "maxSteps" 10000 1000000 root,
                        parseOptionalPositiveIntProperty "$" "recordLimit" 20 1000 root,
                        requireProperty "$" "until" root
                    with
                    | Ok maxSteps, Ok recordLimit, Ok until ->
                        until
                        |> parsePredicate "$.until" false
                        |> Result.map (fun predicate ->
                            {
                                MaxSteps = maxSteps
                                RecordLimit = recordLimit
                                Predicate = predicate
                            }
                        )
                    | Error errors, Ok _, Ok _
                    | Ok _, Error errors, Ok _
                    | Ok _, Ok _, Error errors -> Error errors
                    | Error left, Error right, Ok _ -> Error (left @ right)
                    | Error left, Ok _, Error right -> Error (left @ right)
                    | Ok _, Error left, Error right -> Error (left @ right)
                    | Error first, Error second, Error third -> Error (first @ second @ third)
            with :? JsonException as ex ->
                parseError "$" $"invalid JSON: %s{ex.Message}"

    let matchesString (matcher : StringMatcher) (value : string) : bool =
        match matcher with
        | StringMatcher.Exact expected -> String.Equals (value, expected, StringComparison.Ordinal)
        | StringMatcher.Contains expected -> value.Contains (expected, StringComparison.Ordinal)
        | StringMatcher.StartsWith expected -> value.StartsWith (expected, StringComparison.Ordinal)
        | StringMatcher.EndsWith expected -> value.EndsWith (expected, StringComparison.Ordinal)

    let sessionStatusName (status : DebuggerSessionStatus) : string =
        match status with
        | DebuggerSessionStatus.Running -> "running"
        | DebuggerSessionStatus.Finished -> "finished"
        | DebuggerSessionStatus.Deadlocked -> "deadlocked"

    let threadStatusName (status : ThreadStatus) : string =
        match status with
        | ThreadStatus.Runnable -> "runnable"
        | ThreadStatus.BlockedOnJoin target -> $"blockedOnJoin:%d{threadIdValue target}"
        | ThreadStatus.BlockedOnClassInit blocker -> $"blockedOnClassInit:%d{threadIdValue blocker}"
        | ThreadStatus.Terminated -> "terminated"

    let initialEvaluationState (snapshot : SessionSnapshot) : EvaluationState =
        {
            BaselineStatus = snapshot.Status
            BaselineThreadStatuses = snapshot.Threads |> Map.map (fun _ thread -> thread.Status)
            ActiveLocationCounts = Map.empty
        }

    let private basicMatch (path : string) (kind : string) (detail : string) : PredicateMatch =
        {
            Path = path
            Kind = kind
            Detail = detail
            Thread = None
            Frame = None
            Method = None
            IlOffset = None
            Instruction = None
            FrameDepth = None
            RepeatCount = None
            StatusBefore = None
            StatusAfter = None
        }

    let private threadMatch
        (path : string)
        (kind : string)
        (detail : string)
        (thread : ThreadSnapshot)
        : PredicateMatch
        =
        { basicMatch path kind detail with
            Thread = Some (threadIdValue thread.Thread)
            Frame = Some (frameIdValue thread.ActiveFrame)
            Method = Some thread.ActiveMethod
            IlOffset = Some thread.ActiveIlOffset
            Instruction = thread.ActiveInstruction
        }

    let rec private evaluatePredicate
        (path : string)
        (scopedThread : ThreadId option)
        (snapshot : SessionSnapshot)
        (state : EvaluationState)
        (predicate : RunUntilPredicate)
        : EvaluationState * PredicateMatch list
        =
        match predicate with
        | RunUntilPredicate.And children ->
            let mutable state = state

            let childMatches =
                children
                |> List.mapi (fun i child ->
                    let nextState, matches =
                        evaluatePredicate $"%s{path}.conditions[%d{i}]" scopedThread snapshot state child

                    state <- nextState
                    matches
                )

            if childMatches |> List.forall (not << List.isEmpty) then
                state, childMatches |> List.concat
            else
                state, []
        | RunUntilPredicate.Or children ->
            let mutable state = state

            let childMatches =
                children
                |> List.mapi (fun i child ->
                    let nextState, matches =
                        evaluatePredicate $"%s{path}.conditions[%d{i}]" scopedThread snapshot state child

                    state <- nextState
                    matches
                )

            let matches = childMatches |> List.concat

            if List.isEmpty matches then state, [] else state, matches
        | RunUntilPredicate.Not child ->
            let nextState, matches =
                evaluatePredicate $"%s{path}.condition" scopedThread snapshot state child

            if List.isEmpty matches then
                nextState, [ basicMatch path "not" "child predicate did not match" ]
            else
                nextState, []
        | RunUntilPredicate.Thread (threadId, child) ->
            match snapshot.Threads |> Map.tryFind threadId with
            | None -> state, []
            | Some _ -> evaluatePredicate $"%s{path}.condition" (Some threadId) snapshot state child
        | RunUntilPredicate.AnyThread child ->
            let mutable state = state

            let matches =
                snapshot.Threads
                |> Map.toList
                |> List.collect (fun (threadId, _) ->
                    let nextState, matches =
                        evaluatePredicate $"%s{path}.condition" (Some threadId) snapshot state child

                    state <- nextState
                    matches
                )

            state, matches
        | RunUntilPredicate.SessionStatusChanged ->
            if snapshot.Status = state.BaselineStatus then
                state, []
            else
                state,
                [
                    { basicMatch path "sessionStatusChanged" "session status changed" with
                        StatusBefore = Some (sessionStatusName state.BaselineStatus)
                        StatusAfter = Some (sessionStatusName snapshot.Status)
                    }
                ]
        | RunUntilPredicate.ActiveMethodMatches matcher ->
            match
                scopedThread
                |> Option.bind (fun threadId -> snapshot.Threads |> Map.tryFind threadId)
            with
            | None -> state, []
            | Some thread when matchesString matcher thread.ActiveMethod ->
                state, [ threadMatch path "activeMethodMatches" "active method matched" thread ]
            | Some _ -> state, []
        | RunUntilPredicate.FrameDepthAtLeast depth ->
            match
                scopedThread
                |> Option.bind (fun threadId -> snapshot.Threads |> Map.tryFind threadId)
            with
            | None -> state, []
            | Some thread when thread.FrameDepth >= depth ->
                state,
                [
                    { threadMatch path "frameDepthAtLeast" $"frame depth is at least %d{depth}" thread with
                        FrameDepth = Some thread.FrameDepth
                    }
                ]
            | Some _ -> state, []
        | RunUntilPredicate.ThreadStatusChanged ->
            match
                scopedThread
                |> Option.bind (fun threadId -> snapshot.Threads |> Map.tryFind threadId)
            with
            | None -> state, []
            | Some thread ->
                let before = state.BaselineThreadStatuses |> Map.tryFind thread.Thread

                if before = Some thread.Status then
                    state, []
                else
                    state,
                    [
                        { threadMatch path "threadStatusChanged" "thread status changed" thread with
                            StatusBefore = before |> Option.map threadStatusName
                            StatusAfter = Some (threadStatusName thread.Status)
                        }
                    ]
        | RunUntilPredicate.RepeatedActiveLocation repeatCount ->
            match
                scopedThread
                |> Option.bind (fun threadId -> snapshot.Threads |> Map.tryFind threadId)
            with
            | None -> state, []
            | Some thread ->
                let location =
                    {
                        Method = thread.ActiveMethod
                        IlOffset = thread.ActiveIlOffset
                        Instruction = thread.ActiveInstruction
                    }

                let key = path, thread.Thread, location

                let count =
                    (state.ActiveLocationCounts |> Map.tryFind key |> Option.defaultValue 0) + 1

                let nextState =
                    { state with
                        ActiveLocationCounts = state.ActiveLocationCounts |> Map.add key count
                    }

                if count >= repeatCount then
                    nextState,
                    [
                        { threadMatch path "repeatedActiveLocation" $"active location repeated %d{count} times" thread with
                            RepeatCount = Some count
                        }
                    ]
                else
                    nextState, []

    let evaluate
        (state : EvaluationState)
        (snapshot : SessionSnapshot)
        (predicate : RunUntilPredicate)
        : EvaluationResult
        =
        let state, matches = evaluatePredicate "$.until" None snapshot state predicate

        {
            State = state
            Matches = matches
        }
