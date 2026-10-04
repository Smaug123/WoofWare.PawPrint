namespace WoofWare.PawPrint

open System
open System.IO

/// An exception type of CoreLib's, by namespace and name.
type ExceptionName =
    private
        {
            _Namespace : string
            _Name : string
        }

    /// The type's namespace, such as `System.IO`.
    member this.Namespace : string = this._Namespace

    /// The type's name within its namespace, such as `IOException`.
    member this.Name : string = this._Name

    /// The namespace and name joined by a dot.
    member this.FullName : string = this._Namespace + "." + this._Name

    override this.ToString () : string = this.FullName

[<RequireQualifiedAccess>]
module ExceptionName =

    /// The exception type a full name such as `System.OutOfMemoryException` names: a namespace and a
    /// name joined by the last dot, neither empty. A nested type cannot be named.
    let parse (fullName : string) : ExceptionName option =
        let dot = fullName.LastIndexOf '.'

        if
            dot <= 0
            || dot = fullName.Length - 1
            || fullName |> Seq.exists (fun c -> Char.IsWhiteSpace c || c = '/' || c = '+')
        then
            None
        else
            Some
                {
                    _Namespace = fullName.Substring (0, dot)
                    _Name = fullName.Substring (dot + 1)
                }

/// What a method CoreCLR implements in native code (`NativeMethod`) can do to its caller, as an
/// analyser needs it. Unrecoverable failures (`StackOverflowException`, `OutOfMemoryException` from
/// an operation that allocates nothing, and a fault in native code, which ends the process) are out
/// of scope, as for `OpcodeFaults`.
type NativeContract =
    {
        /// Calling the method can raise exceptions of exactly these CoreLib types, and nothing else.
        /// The empty list is a positive claim that it cannot fault.
        Raises : ExceptionName list
        /// Whether the method can return. False would mean every call raises or ends the process.
        CanReturn : bool
        /// What is known of its result's nullness.
        Result : ResultNullness
    }

/// A method of CoreLib's that CoreCLR implements in native code, as the contract table names it.
[<RequireQualifiedAccess>]
type TabulatedNative =
    /// An FCall: the `InternalCall` method of this name on this top-level type (by namespace and
    /// name joined with a dot), the only one of that name the type declares, which `vm/ecalllist.h`
    /// binds to C++ by its name.
    | FCall of declaringType : string * method : string
    /// A QCall: a P/Invoke into `QCall` with this entry point, the name of the C++ function that
    /// `vm/qcallentrypoints.cpp` lists.
    | QCall of entryPoint : string

/// A file of the pinned runtime source, relative to `src/coreclr`, and a symbol defined there.
type NativeSource =
    {
        Path : string
        Symbol : string
    }

/// One row of the contract table: a native method, what it can do, and where in the runtime's
/// source that was read from.
type NativeContractRow =
    {
        Native : TabulatedNative
        Contract : NativeContract
        /// The functions whose code the contract was read from, the first being the one the
        /// method is bound to.
        Sources : NativeSource list
        /// Why the contract holds, in a sentence.
        Reason : string
    }

/// The contracts of CoreLib's native methods, as checked in beside this file
/// (`NativeContractTable.tsv`, embedded). Every native method a row names runs no managed code.
[<RequireQualifiedAccess>]
module NativeContractTable =

    let private parse (line : string) : NativeContractRow =
        let fail (why : string) : 'a =
            failwith $"NativeContractTable: %s{why} in %s{line}"

        match line.Split '\t' with
        | [| kind ; name ; raises ; returns ; result ; sources ; reason |] ->
            if
                [ kind ; name ; raises ; returns ; result ; sources ]
                |> List.exists (Seq.exists Char.IsWhiteSpace)
            then
                fail "white space in a field before the reason"

            let native =
                match kind with
                | "FCall" ->
                    match name.Split "::" with
                    | [| declaringType ; method |] when declaringType <> "" && method <> "" ->
                        TabulatedNative.FCall (declaringType, method)
                    | _ -> fail $"an FCall named %s{name}, not Type::Method"
                | "QCall" -> TabulatedNative.QCall name
                | other -> fail $"unknown kind %s{other}"

            let raises =
                match raises with
                | "-" -> []
                | raises ->
                    raises.Split '|'
                    |> Array.map (fun fullName ->
                        match ExceptionName.parse fullName with
                        | Some exceptionName -> exceptionName
                        | None -> fail $"no exception type named %s{fullName}"
                    )
                    |> List.ofArray

            let canReturn =
                match returns with
                | "returns" -> true
                | "never" -> false
                | other -> fail $"unknown returns %s{other}"

            let result =
                match result with
                | "none" -> ResultNullness.NotAReference
                | "nonnull" -> ResultNullness.NonNull
                | "maybenull" -> ResultNullness.MaybeNull
                | other -> fail $"unknown result %s{other}"

            let sources =
                sources.Split '|'
                |> Array.map (fun source ->
                    // A symbol may be qualified (`Thread::GetExposedObject`); a path has no colon.
                    let colon = source.IndexOf ':'

                    if colon <= 0 || colon = source.Length - 1 then
                        fail $"a source %s{source}, not path:symbol"

                    {
                        Path = source.Substring (0, colon)
                        Symbol = source.Substring (colon + 1)
                    }
                )
                |> List.ofArray

            if String.IsNullOrWhiteSpace reason then
                fail "no reason"

            {
                Native = native
                Contract =
                    {
                        Raises = raises
                        CanReturn = canReturn
                        Result = result
                    }
                Sources = sources
                Reason = reason
            }
        | _ -> fail "not seven tab-separated fields"

    /// The rows of a table in the checked-in format, whatever its line endings. A line starting
    /// with `#` is a comment.
    let ofText (text : string) : NativeContractRow list =
        use reader = new StringReader (text)

        let rec lines (acc : string list) : string list =
            match reader.ReadLine () with
            | null -> List.rev acc
            | "" -> lines acc
            | line when line.StartsWith '#' -> lines acc
            | line -> lines (line :: acc)

        lines [] |> List.map parse

    /// Every row, in the order the checked-in table lists them.
    let rows : Lazy<NativeContractRow list> =
        lazy
            (let assembly = typeof<NativeContractRow>.Assembly
             let name = "WoofWare.PawPrint.Semantics.NativeContractTable.tsv"

             use stream =
                 match assembly.GetManifestResourceStream name with
                 | null -> failwith $"NativeContractTable: %s{assembly.FullName} embeds no %s{name}"
                 | stream -> stream

             use reader = new StreamReader (stream)
             ofText (reader.ReadToEnd ()))

    let private index (key : NativeContractRow -> 'key option) : Lazy<Map<'key, NativeContractRow>> =
        lazy
            (rows.Force ()
             |> List.choose (fun row -> key row |> Option.map (fun key -> key, row))
             |> List.groupBy fst
             |> List.map (fun (key, rows) ->
                 match rows with
                 | [ _, row ] -> key, row
                 | _ -> failwith $"NativeContractTable: %A{key} has %d{rows.Length} rows"
             )
             |> Map.ofList)

    /// The rows for FCalls, by declaring type and method name.
    let fcalls : Lazy<Map<string * string, NativeContractRow>> =
        index (fun row ->
            match row.Native with
            | TabulatedNative.FCall (declaringType, method) -> Some (declaringType, method)
            | TabulatedNative.QCall _ -> None
        )

    /// The rows for QCalls, by entry point.
    let qcalls : Lazy<Map<string, NativeContractRow>> =
        index (fun row ->
            match row.Native with
            | TabulatedNative.QCall entryPoint -> Some entryPoint
            | TabulatedNative.FCall _ -> None
        )
