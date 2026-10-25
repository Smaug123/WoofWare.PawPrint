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

/// What a native method passes a managed method as one of its arguments, as a row of the contract
/// table spells it.
[<RequireQualifiedAccess>]
type NativeArgument =
    /// A value the row does not name, which may be anything the parameter's declared type allows.
    | Unnamed

/// A call a native method can make of a method of CoreLib's, whose exceptions escape the native
/// method to its caller.
type ManagedCall =
    {
        /// The top-level type declaring the method, by namespace and name joined with a dot.
        DeclaringType : string
        /// The method's name, which no other method of the type has.
        Method : string
        /// What the native passes as each of the method's arguments, `this` first.
        Arguments : NativeArgument list
    }

/// Managed code a native method can call, whose exceptions escape the native method to its caller.
[<RequireQualifiedAccess>]
type NativeCall =
    /// A call of this method of CoreLib's.
    | Method of ManagedCall
    /// CoreCLR's lookup of one of CoreLib's resource strings by name (`ResMgrGetString`, excep.cpp),
    /// by which it gives the message to an exception it throws with a resource name, as
    /// `COMPlusThrow(kind, W("name"))` does. The CoreLib method it calls differs between runtimes:
    /// .NET 10's `Environment.GetResourceStringLocal(string)`, .NET 11's
    /// `Environment.GetResourceString(char*, string*, Exception*)`.
    | ResourceMessage

[<RequireQualifiedAccess>]
module NativeCall =

    /// The CoreLib method a call runs on `runtime`.
    let resolve (runtime : EmulatedRuntime) (call : NativeCall) : ManagedCall =
        match call with
        | NativeCall.Method call -> call
        | NativeCall.ResourceMessage ->
            match runtime with
            | EmulatedRuntime.Net10 ->
                {
                    DeclaringType = "System.Environment"
                    Method = "GetResourceStringLocal"
                    Arguments = [ NativeArgument.Unnamed ]
                }
            | EmulatedRuntime.Net11 ->
                {
                    DeclaringType = "System.Environment"
                    Method = "GetResourceString"
                    Arguments = List.replicate 3 NativeArgument.Unnamed
                }

/// A claim about the program under which a native method's contract holds, and which an analyser
/// must be allowed to make before it relies on the contract.
[<RequireQualifiedAccess>]
type NativeAssumption =
    /// Every type the native loads is found without running the program's `AssemblyResolve` or
    /// `Resolving` handlers, which are managed code, is intact, and is defined as its name says:
    /// each is named by the metadata of a type already loaded, such as a generic parameter's
    /// constraint.
    | NamedTypesLoad
    /// The metadata of every loaded image is well formed, so that CoreCLR's metadata readers fail
    /// on none of it: the HRESULT a reader returns for a bad token, a row past a table's end, a
    /// heap index out of range or a malformed blob, which the native turns into an exception by
    /// `IfFailThrow` or `ThrowHR`, does not arise.
    | MetadataWellFormed

/// What a method CoreCLR implements in native code (`NativeMethod`) can do to its caller, as an
/// analyser needs it. Unrecoverable failures (`StackOverflowException`, `OutOfMemoryException` from
/// an operation that allocates nothing, and a fault in native code, which ends the process) are out
/// of scope, as for `OpcodeFaults`.
type NativeContract =
    {
        /// The method can raise exceptions of exactly these CoreLib types, and nothing else. CoreCLR
        /// makes each by running its parameterless constructor, whose own exceptions escape in its
        /// place. The empty list is a positive claim that it cannot fault.
        Raises : ExceptionName list
        /// Whether the method can return. False would mean every call raises or ends the process.
        CanReturn : bool
        /// What is known of its result's nullness.
        Result : ResultNullness
        /// The managed code it can call, each any number of times, besides the constructors of the
        /// exceptions it raises. The empty list is a positive claim that it runs no other managed
        /// code.
        Runs : NativeCall list
        /// The claims under which all of the above holds. Where one fails, the method may run
        /// other managed code and raise anything.
        Assumes : NativeAssumption list
    }

/// A method of CoreLib's that CoreCLR implements in native code, as the contract table names it.
[<RequireQualifiedAccess>]
type TabulatedNative =
    /// A static FCall: the static `InternalCall` method of this name on this top-level type (by
    /// namespace and name joined with a dot), the only `InternalCall` of that name the type
    /// declares, which `vm/ecalllist.h` binds to C++ by its name.
    | FCall of declaringType : string * method : string
    /// An instance FCall: the `InternalCall` instance method of this name on this top-level type,
    /// the only `InternalCall` of that name the type declares, which `vm/ecalllist.h` binds to C++
    /// by its name, and which passes the receiver as the C++ function's first argument.
    | InstanceFCall of declaringType : string * method : string
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
/// (`NativeContractTable.tsv`, embedded). Every native method a row names runs no managed code but
/// the methods its row says it calls, and the parameterless constructor of each exception it
/// raises, which CoreCLR runs to make it, and whose own exceptions escape in its place.
[<RequireQualifiedAccess>]
module NativeContractTable =

    let private parse (line : string) : NativeContractRow =
        let fail (why : string) : 'a =
            failwith $"NativeContractTable: %s{why} in %s{line}"

        match line.Split '\t' with
        | [| kind ; name ; raises ; returns ; result ; runs ; assumes ; sources ; reason |] ->
            if
                [ kind ; name ; raises ; returns ; result ; runs ; assumes ; sources ]
                |> List.exists (Seq.exists Char.IsWhiteSpace)
            then
                fail "white space in a field before the reason"

            let native =
                match kind with
                | "FCall"
                | "InstanceFCall" ->
                    match name.Split "::" with
                    | [| declaringType ; method |] when declaringType <> "" && method <> "" ->
                        if kind = "FCall" then
                            TabulatedNative.FCall (declaringType, method)
                        else
                            TabulatedNative.InstanceFCall (declaringType, method)
                    | _ -> fail $"an %s{kind} named %s{name}, not Type::Method"
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

            let runs =
                match runs with
                | "-" -> []
                | runs ->
                    runs.Split '|'
                    |> Array.map (fun call ->
                        if call = "ResourceMessage" then
                            NativeCall.ResourceMessage
                        else

                        let opening = call.IndexOf '('

                        if opening < 0 || not (call.EndsWith ')') then
                            fail $"a call %s{call}, not Type::Method(arguments)"

                        let isName (name : string) : bool =
                            name <> "" && name.IndexOfAny [| '(' ; ')' ; ',' ; ':' |] < 0

                        let declaringType, method =
                            match call.Substring(0, opening).Split "::" with
                            | [| declaringType ; method |] when isName declaringType && isName method ->
                                declaringType, method
                            | _ -> fail $"a call %s{call}, not Type::Method(arguments)"

                        let arguments =
                            match call.Substring (opening + 1, call.Length - opening - 2) with
                            | "" -> []
                            | arguments ->
                                arguments.Split ','
                                |> Array.map (fun argument ->
                                    match argument with
                                    | "?" -> NativeArgument.Unnamed
                                    | other -> fail $"an argument %s{other}, not ?"
                                )
                                |> List.ofArray

                        NativeCall.Method
                            {
                                DeclaringType = declaringType
                                Method = method
                                Arguments = arguments
                            }
                    )
                    |> List.ofArray

            let assumes =
                match assumes with
                | "-" -> []
                | assumes ->
                    let parsed =
                        assumes.Split '|'
                        |> Array.map (fun assumption ->
                            match assumption with
                            | "NamedTypesLoad" -> NativeAssumption.NamedTypesLoad
                            | "MetadataWellFormed" -> NativeAssumption.MetadataWellFormed
                            | other -> fail $"no assumption named %s{other}"
                        )
                        |> List.ofArray

                    if List.distinct parsed <> parsed then
                        fail $"an assumption named twice in %s{assumes}"

                    parsed

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
                        Runs = runs
                        Assumes = assumes
                    }
                Sources = sources
                Reason = reason
            }
        | _ -> fail "not nine tab-separated fields"

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

    /// The rows for static FCalls, by declaring type and method name.
    let fcalls : Lazy<Map<string * string, NativeContractRow>> =
        index (fun row ->
            match row.Native with
            | TabulatedNative.FCall (declaringType, method) -> Some (declaringType, method)
            | TabulatedNative.InstanceFCall _
            | TabulatedNative.QCall _ -> None
        )

    /// The rows for instance FCalls, by declaring type and method name.
    let instanceFCalls : Lazy<Map<string * string, NativeContractRow>> =
        index (fun row ->
            match row.Native with
            | TabulatedNative.InstanceFCall (declaringType, method) -> Some (declaringType, method)
            | TabulatedNative.FCall _
            | TabulatedNative.QCall _ -> None
        )

    /// The rows for QCalls, by entry point.
    let qcalls : Lazy<Map<string, NativeContractRow>> =
        index (fun row ->
            match row.Native with
            | TabulatedNative.QCall entryPoint -> Some entryPoint
            | TabulatedNative.FCall _
            | TabulatedNative.InstanceFCall _ -> None
        )
