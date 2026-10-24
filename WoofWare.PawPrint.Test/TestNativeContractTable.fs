namespace WoofWare.PawPrint.Test

#nowarn "9"

open System
open System.IO
open System.Reflection
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open Microsoft.FSharp.NativeInterop
open NUnit.Framework
open WoofWare.PawPrint

/// `NativeContractTable` holds what CoreLib's native methods can do, read from the runtime's C++.
/// These tests hold each row to the CoreLibs' metadata, which must declare and recognise the native
/// it names, and to the pinned runtime source, which must define what it cites; every call that
/// cited code makes must be to code the row cites too, or to code `NativeContractCallees.tsv`
/// vouches for.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNativeContractTable =

    let coreLibs : TestCaseData list = TestIntrinsicBody.coreLibs

    let private rows : NativeContractRow list = NativeContractTable.rows.Force ()

    [<Test>]
    let ``the table has rows of every kind, each native once`` () : unit =
        let fcalls = NativeContractTable.fcalls.Force ()
        let instanceFCalls = NativeContractTable.instanceFCalls.Force ()
        let qcalls = NativeContractTable.qcalls.Force ()
        fcalls.Count |> shouldBeGreaterThan 0
        instanceFCalls.Count |> shouldBeGreaterThan 0
        qcalls.Count |> shouldBeGreaterThan 0
        fcalls.Count + instanceFCalls.Count + qcalls.Count |> shouldEqual rows.Length

    [<TestCaseSource(nameof coreLibs)>]
    let ``every exception a row raises is a CoreLib exception type`` (which : AuditedCoreLib) : unit =
        let corelib = TestIntrinsicBody.coreLib which

        for row in rows do
            for name in row.Contract.Raises do
                match corelib.TryGetTopLevelTypeDef name.Namespace name.Name with
                | Some _ -> ()
                | None -> failwith $"%A{row.Native} raises %O{name}, which this CoreLib does not declare"

                // The host's CoreLib says what it derives from.
                let hostType = typeof<obj>.Assembly.GetType name.FullName

                if isNull hostType || not (typeof<Exception>.IsAssignableFrom hostType) then
                    failwith $"%A{row.Native} raises %O{name}, which is not an exception type of the host's CoreLib"

    [<TestCaseSource(nameof coreLibs)>]
    let ``every row names a native of the CoreLib, which is recognised as that row`` (which : AuditedCoreLib) : unit =
        Net11Frontier.park Net11Frontier.nativeContractTable

        let corelib = TestIntrinsicBody.coreLib which

        let natives =
            [
                for KeyValue (handle, method) in corelib.Methods do
                    let declaringType = corelib.TypeDefs.[method.RequiredDeclaringType.Definition.Get]

                    match method.Body, method.TryNativeImport with
                    | MethodBody.InternalCall, _ when not declaringType.IsNested ->
                        let fullName = declaringType.Namespace + "." + declaringType.Name

                        if method.IsStatic then
                            yield TabulatedNative.FCall (fullName, method.Name), handle
                        else
                            yield TabulatedNative.InstanceFCall (fullName, method.Name), handle
                    | MethodBody.PInvoke, Some import when import.ModuleName = "QCall" ->
                        yield TabulatedNative.QCall import.EntryPointName, handle
                    | _ -> ()
            ]

        for row in rows do
            let handles =
                natives |> List.filter (fun (native, _) -> native = row.Native) |> List.map snd

            match row.Native, handles with
            | _, [] -> failwith $"No method of this CoreLib is %A{row.Native}"
            | TabulatedNative.FCall _, _ :: _ :: _
            | TabulatedNative.InstanceFCall _, _ :: _ :: _ ->
                failwith $"%A{row.Native} names %d{handles.Length} methods, which CoreCLR would bind alike"
            | _ ->
                for handle in handles do
                    match NativeMethod.recognise corelib handle with
                    | Some (NativeMethod.Tabulated recognised) when recognised = row -> ()
                    | other -> failwith $"A method that is %A{row.Native} is recognised as %A{other}"

    /// The namespace and name a `ManagedCall` names its declaring type by.
    let private namespaceAndName (declaringType : string) : string * string =
        match declaringType.LastIndexOf '.' with
        | -1 -> "", declaringType
        | dot -> declaringType.Substring (0, dot), declaringType.Substring (dot + 1)

    [<TestCaseSource(nameof coreLibs)>]
    let ``every method a row runs is the CoreLib's one method of its name, taking an argument per spelling``
        (which : AuditedCoreLib)
        : unit
        =
        Net11Frontier.park Net11Frontier.nativeContractTable

        let corelib = TestIntrinsicBody.coreLib which

        for row in rows do
            for call in row.Contract.Runs |> List.map (NativeCall.resolve (FrameworkUnderTest.runtime ())) do
                let ns, name = namespaceAndName call.DeclaringType

                let declaringType =
                    match corelib.TryGetTopLevelTypeDef ns name with
                    | Some declaringType -> declaringType
                    | None ->
                        failwith $"%A{row.Native} runs a method of %s{call.DeclaringType}, which this CoreLib lacks"

                match declaringType.Methods |> List.filter (fun method -> method.Name = call.Method) with
                | [ method ] ->
                    declaringType.Generics |> shouldBeEmpty
                    method.Signature.GenericParameterCount |> shouldEqual 0

                    let arity =
                        (if method.IsStatic then 0 else 1) + method.Signature.ParameterTypes.Length

                    if arity <> call.Arguments.Length then
                        failwith
                            $"%A{row.Native} passes %s{call.DeclaringType}::%s{call.Method} %d{call.Arguments.Length} arguments, and it takes %d{arity}"
                | methods ->
                    failwith
                        $"%A{row.Native} runs %s{call.DeclaringType}::%s{call.Method}, of which this CoreLib has %d{methods.Length}"

    /// What each `g_*NS` macro of `vm/namespace.h` expands to, by the name between `g_` and `NS`.
    let private bindersNamespaces (namespaceHeader : string) : Map<string, string> =
        let definitions =
            Text.RegularExpressions.Regex.Matches (
                namespaceHeader,
                @"^#define\s+g_(\w+)NS\s+(.+?)\s*$",
                Text.RegularExpressions.RegexOptions.Multiline
            )
            |> Seq.map (fun m -> m.Groups.[1].Value, m.Groups.[2].Value)
            |> Map.ofSeq

        let rec expand (name : string) : string =
            Text.RegularExpressions.Regex.Matches (definitions.[name], @"g_(\w+)NS|""([^""]*)""")
            |> Seq.map (fun token ->
                if token.Groups.[1].Success then
                    expand token.Groups.[1].Value
                else
                    token.Groups.[2].Value
            )
            |> String.concat ""

        definitions |> Map.map (fun name _ -> expand name)

    /// The pinned runtime source's `src/coreclr`.
    let private coreclr () : string =
        Path.Combine (FrameworkUnderTest.requireRuntimeSource (), "src", "coreclr")

    /// The IDs by which the runtime's binder, in `vm/corelib.h`, names the method a row runs.
    let private binderIds (call : ManagedCall) : string list =
        let read (path : string) : string =
            File.ReadAllText (Path.Combine (coreclr (), path))

        let corelibHeader = read "vm/corelib.h"
        let namespaces = bindersNamespaces (read "vm/namespace.h")

        // Each class the binder names, by its full name, with its ID. A use of the macro starts a
        // line; its definitions and the comments describing it do not.
        let classes : (string * string) list =
            Text.RegularExpressions.Regex.Matches (
                corelibHeader,
                @"^DEFINE_CLASS\(\s*(\w+)\s*,\s*(\w+)\s*,\s*([^\s,)]+)\s*\)",
                Text.RegularExpressions.RegexOptions.Multiline
            )
            |> Seq.map (fun m -> namespaces.[m.Groups.[2].Value] + "." + m.Groups.[3].Value, m.Groups.[1].Value)
            |> List.ofSeq

        // Each method the binder names, by its class's ID and its own name, with its ID.
        let methods : ((string * string) * string) list =
            Text.RegularExpressions.Regex.Matches (
                corelibHeader,
                @"^DEFINE_METHOD\(\s*(\w+)\s*,\s*(\w+)\s*,\s*([^\s,]+)\s*,",
                Text.RegularExpressions.RegexOptions.Multiline
            )
            |> Seq.map (fun m -> (m.Groups.[1].Value, m.Groups.[3].Value), m.Groups.[2].Value)
            |> List.ofSeq

        classes.Length |> shouldBeGreaterThan 0
        methods.Length |> shouldBeGreaterThan 0

        [
            for fullName, classId in classes do
                if fullName = call.DeclaringType then
                    for (ofClass, name), methodId in methods do
                        if ofClass = classId && name = call.Method then
                            yield $"METHOD__%s{classId}__%s{methodId}"
        ]

    /// CoreCLR calls a CoreLib method from native code through its binder, which names the methods
    /// it can call in `vm/corelib.h`, each by an ID `METHOD__<class>__<method>`.
    [<Test>]
    let ``every method a row runs is one the runtime's binder names, and the row cites code that calls it`` () : unit =
        Net11Frontier.park Net11Frontier.nativeContractTable

        let read (path : string) : string =
            File.ReadAllText (Path.Combine (coreclr (), path))

        for row in rows do
            for call in row.Contract.Runs |> List.map (NativeCall.resolve (FrameworkUnderTest.runtime ())) do
                let binderIds = binderIds call

                if binderIds.IsEmpty then
                    failwith
                        $"%A{row.Native} runs %s{call.DeclaringType}::%s{call.Method}, which vm/corelib.h does not name"

                let cited =
                    row.Sources
                    |> List.exists (fun source -> binderIds |> List.exists (read source.Path).Contains)

                if not cited then
                    failwith
                        $"%A{row.Native} cites no source that calls %s{call.DeclaringType}::%s{call.Method} by %A{binderIds}"

    let private routinesByPath =
        Collections.Concurrent.ConcurrentDictionary<string, Lazy<SourceRoutine list>> ()

    /// Each routine the file at `path` under `src/coreclr` defines.
    let private routinesOf (path : string) : SourceRoutine list =
        routinesByPath
            .GetOrAdd(
                path,
                fun path ->
                    lazy
                        (let text = File.ReadAllText (Path.Combine (coreclr (), path))

                         if
                             path.EndsWith (".S", StringComparison.Ordinal)
                             || path.EndsWith (".inc", StringComparison.Ordinal)
                         then
                             CppSource.assemblyRoutines text
                         else
                             CppSource.definitions text)
            )
            .Force ()

    /// The macros that a header of the pinned source's `vm` defines as exactly `name`.
    let private aliasesOf (name : string) : string list =
        let definition =
            Text.RegularExpressions.Regex (
                $@"^\s*#define\s+(\w+)\s+%s{Text.RegularExpressions.Regex.Escape name}\s*$",
                Text.RegularExpressions.RegexOptions.Multiline
            )

        [
            for header in Directory.GetFiles (Path.Combine (coreclr (), "vm"), "*.h") do
                for m in definition.Matches (File.ReadAllText header) do
                    yield m.Groups.[1].Value
        ]

    [<Test>]
    let ``every row cites code the pinned runtime source defines, and binds the native to the first`` () : unit =
        Net11Frontier.park Net11Frontier.nativeContractTable

        let coreclr =
            Path.Combine (FrameworkUnderTest.requireRuntimeSource (), "src", "coreclr")

        let read (path : string) : string =
            File.ReadAllText (Path.Combine (coreclr, path))

        let ecalls = read "vm/ecalllist.h"
        let qcallEntryPoints = read "vm/qcallentrypoints.cpp"

        for row in rows do
            for source in row.Sources do
                if not (File.Exists (Path.Combine (coreclr, source.Path))) then
                    failwith $"%A{row.Native} cites %s{source.Path}, which the pinned source lacks"

                if
                    not (
                        routinesOf source.Path
                        |> List.exists (fun routine -> CppSource.names routine.Name source.Symbol)
                    )
                then
                    failwith $"%A{row.Native} cites %s{source.Symbol} in %s{source.Path}, which does not define it"

            let bound = (List.head row.Sources).Symbol

            match row.Native with
            | TabulatedNative.FCall (_, method)
            | TabulatedNative.InstanceFCall (_, method) ->
                // A binding may name the function by a macro that a header of `vm` defines as its
                // name, as `WriteBarrier_Helper` is `JIT_WriteBarrier_Callable`, and in the global
                // namespace by `::`.
                let byName =
                    bound :: aliasesOf bound
                    |> List.exists (fun name ->
                        ecalls.Contains $"FCFuncElement(\"%s{method}\", %s{name})"
                        || ecalls.Contains $"FCFuncElement(\"%s{method}\", ::%s{name})"
                    )

                // `FCDynamic` binds the method to a slot whose implementation is assigned at
                // startup, defaulting to the one `vm/ecall.h` names.
                let dynamic = $"FCDynamic(\"%s{method}\", ECall::%s{method})"

                if not (byName || ecalls.Contains dynamic) then
                    failwith $"vm/ecalllist.h binds %s{method} to neither %s{bound} nor a dynamic slot"
            | TabulatedNative.QCall entryPoint ->
                bound |> shouldEqual entryPoint

                if not (qcallEntryPoints.Contains $"DllImportEntry(%s{entryPoint})") then
                    failwith $"vm/qcallentrypoints.cpp lists no %s{entryPoint}"

    /// What a C++ function or macro that `NativeContractCallees.tsv` vouches for can run.
    [<RequireQualifiedAccess>]
    type private VouchedRuns =
        /// No managed code.
        | Nothing
        /// No managed code but the constructor of the exception it throws.
        | Exception
        /// Whatever it likes: the cited code calls it only on a path the native never takes.
        | Unreached
        /// No managed code, when every type it loads is found as `NativeAssumption.NamedTypesLoad`
        /// says, without the program's assembly resolution handlers; the row must hold under it.
        | Loads

    /// What a row of `NativeContractCallees.tsv` vouches for.
    [<RequireQualifiedAccess>]
    type private Vouched =
        /// The calls of a function or macro, wherever they are made, by the name they spell.
        /// `path` is the file under `src/coreclr` that defines it, or None for the C library or the
        /// operating system; `spelledAs` is the text by which the file defines it, where a macro
        /// pastes its name together, as `_GETFLD(GenericParam,Flags)` defines
        /// `getFlagsOfGenericParam`.
        | Callee of path : string option * symbol : string * spelledAs : string option
        /// The branches through a register or memory that the assembly routine `routine` of the
        /// file `path` makes, whose targets its text does not name.
        | IndirectBranchesOf of path : string * routine : string

    /// A row of `NativeContractCallees.tsv`.
    type private Vouch =
        {
            Vouched : Vouched
            Runs : VouchedRuns
        }

    /// How a vouch for a routine's indirect branches spells its symbol, after the routine's name.
    let private indirectBranchesSuffix : string = "::" + CppSource.indirectBranch

    let private vouches : Lazy<Vouch list> =
        lazy
            (let assembly = typeof<Vouch>.Assembly

             use stream =
                 assembly.GetManifestResourceStream "WoofWare.PawPrint.Test.NativeContractCallees.tsv"

             use reader = new StreamReader (stream)

             [
                 for line in reader.ReadToEnd().Split '\n' do
                     if line.Trim () <> "" && not (line.StartsWith '#') then
                         match line.Split '\t' with
                         | [| callee ; runs ; reason |] when reason.Trim () <> "" ->
                             let colon = callee.IndexOf ':'

                             if colon <= 0 || colon = callee.Length - 1 then
                                 failwith $"A vouch names %s{callee}, which is not path:symbol"

                             let path =
                                 match callee.Substring (0, colon) with
                                 | "-" -> None
                                 | path -> Some path

                             let named = callee.Substring (colon + 1)

                             let vouched =
                                 if named.EndsWith (indirectBranchesSuffix, StringComparison.Ordinal) then
                                     let routine = named.Substring (0, named.Length - indirectBranchesSuffix.Length)

                                     match path with
                                     | Some path when Text.RegularExpressions.Regex.IsMatch (routine, @"^\w+$") ->
                                         Vouched.IndirectBranchesOf (path, routine)
                                     | _ ->
                                         failwith
                                             $"A vouch names %s{callee}, which is not path:routine%s{indirectBranchesSuffix}"
                                 else
                                     match named.Split ('=', 2) with
                                     | [| symbol ; spelling |] when symbol <> "" && spelling <> "" ->
                                         Vouched.Callee (path, symbol, Some spelling)
                                     | [| symbol |] -> Vouched.Callee (path, symbol, None)
                                     | _ ->
                                         failwith
                                             $"A vouch names %s{callee}, which is not path:symbol or path:symbol=spelling"

                             let runs =
                                 match runs with
                                 | "nothing" -> VouchedRuns.Nothing
                                 | "exception" -> VouchedRuns.Exception
                                 | "unreached" -> VouchedRuns.Unreached
                                 | "loads" -> VouchedRuns.Loads
                                 | other -> failwith $"%s{callee} is vouched to run %s{other}"

                             yield
                                 {
                                     Vouched = vouched
                                     Runs = runs
                                 }
                         | _ -> failwith $"Malformed vouch: %s{line}"
             ])

    /// The routines a row cites, each with the citation naming it.
    let private citedRoutines (row : NativeContractRow) : (NativeSource * SourceRoutine) list =
        [
            for source in row.Sources do
                match
                    routinesOf source.Path
                    |> List.filter (fun routine -> CppSource.names routine.Name source.Symbol)
                with
                | [] -> failwith $"%A{row.Native} cites %s{source.Symbol}, which %s{source.Path} does not define"
                | routines ->
                    for routine in routines do
                        yield source, routine
        ]

    /// Whether `vouch` covers the call of `called` that the routine `routine` of the file `path`
    /// makes. A vouch for a callee covers its calls wherever they are, and one for a routine's
    /// indirect branches covers those alone.
    let private covers (vouch : Vouch) (path : string) (routine : string) (called : string) : bool =
        match vouch.Vouched with
        | Vouched.Callee (_, symbol, _) -> called <> CppSource.indirectBranch && CppSource.names symbol called
        | Vouched.IndirectBranchesOf (vouchedPath, vouchedRoutine) ->
            called = CppSource.indirectBranch
            && vouchedPath = path
            && vouchedRoutine = routine

    [<Test>]
    let ``a vouch for a routine's indirect branches covers those alone, and a vouch for a callee covers none``
        ()
        : unit
        =
        // A vouch for a callee spelled as an indirect branch, too, which must cover no branch.
        let word : Gen<string> =
            Gen.elements [ "A" ; "B" ; "vm/a.S" ; "vm/b.S" ; CppSource.indirectBranch ]

        let called : Gen<string> =
            Gen.oneof [ word ; Gen.constant CppSource.indirectBranch ]

        let property
            (vouchedPath : string, vouchedRoutine : string, (path : string, routine : string, called : string))
            : unit
            =
            let indirect =
                {
                    Vouched = Vouched.IndirectBranchesOf (vouchedPath, vouchedRoutine)
                    Runs = VouchedRuns.Nothing
                }

            let callee =
                {
                    Vouched = Vouched.Callee (Some vouchedPath, vouchedRoutine, None)
                    Runs = VouchedRuns.Nothing
                }

            covers indirect path routine called
            |> shouldEqual (
                called = CppSource.indirectBranch
                && path = vouchedPath
                && routine = vouchedRoutine
            )

            covers callee path routine called
            |> shouldEqual (called <> CppSource.indirectBranch && called = vouchedRoutine)

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 400,
            Prop.forAll (Arb.fromGen (Gen.zip3 word word (Gen.zip3 word word called))) property
        )

    /// What runs managed code from the VM: a call site of the runtime's binder, or the routine
    /// every such call goes through.
    let private managedCall : Text.RegularExpressions.Regex =
        Text.RegularExpressions.Regex
            @"\b(?:MethodDescCallSite\s+\w+|PREPARE_NONVIRTUAL_CALLSITE\w*|PREPARE_VIRTUAL_CALLSITE\w*|UnmanagedCallersOnlyCaller\s+\w+)\s*\(|\bCallDescrWorker\w*"

    /// The names a routine spells a managed call with, which the binder ID it passes says the
    /// target of, rather than the name.
    let private managedCallNames : Text.RegularExpressions.Regex =
        Text.RegularExpressions.Regex
            @"^(?:MethodDescCallSite|UnmanagedCallersOnlyCaller|PREPARE_\w*CALLSITE\w*|CALL_MANAGED_METHOD\w*|Call(?:_Ret\w+)?)$"

    [<Test>]
    let ``every call a routine a row cites makes is to one the row cites or one vouched for`` () : unit =
        Net11Frontier.park Net11Frontier.nativeContractTable

        let vouches = vouches.Force ()

        let uncovered =
            [
                for row in rows do
                    let cited = row.Sources |> List.map (fun source -> source.Symbol)

                    for source, routine in citedRoutines row do
                        for called in routine.Calls do
                            // The next test holds each managed call to the methods the row runs.
                            let covered =
                                (managedCall.IsMatch routine.Text && managedCallNames.IsMatch called)
                                || cited |> List.exists (fun symbol -> CppSource.names symbol called)
                                || vouches
                                   |> List.exists (fun vouch -> covers vouch source.Path routine.Name called)

                            if not covered then
                                yield $"%A{row.Native}: %s{source.Path}:%s{routine.Name} calls %s{called}"
            ]
            |> List.distinct

        if not uncovered.IsEmpty then
            failwith (
                $"%d{uncovered.Length} calls are to routines neither cited nor vouched for:\n"
                + String.concat "\n" uncovered
            )

    [<Test>]
    let ``every managed call a routine a row cites makes is of a method the row runs`` () : unit =
        Net11Frontier.park Net11Frontier.nativeContractTable

        for row in rows do
            let runs =
                row.Contract.Runs
                |> List.map (NativeCall.resolve (FrameworkUnderTest.runtime ()))
                |> List.collect binderIds
                |> Set.ofList

            for source, routine in citedRoutines row do
                for site in managedCall.Matches routine.Text do
                    let arguments =
                        let start = site.Index + site.Length
                        let finish = routine.Text.IndexOf (')', start)

                        if finish < 0 then
                            ""
                        else
                            routine.Text.Substring (start, finish - start)

                    let ids =
                        Text.RegularExpressions.Regex.Matches (arguments, @"\bMETHOD__\w+")
                        |> Seq.map (fun id -> id.Value)
                        |> List.ofSeq

                    if ids.IsEmpty || ids |> List.exists (fun id -> not (runs.Contains id)) then
                        failwith
                            $"%A{row.Native} cites %s{source.Path}:%s{routine.Name}, which calls managed code at `%s{site.Value}%s{arguments}`, and runs %A{runs}"

    [<Test>]
    let ``every vouch names code the pinned source defines, which makes no managed call itself`` () : unit =
        Net11Frontier.park Net11Frontier.nativeContractTable

        let problems =
            [
                for vouch in vouches.Force () do
                    match vouch.Vouched with
                    | Vouched.Callee (None, _, _) -> ()
                    | Vouched.Callee (Some path, symbol, _)
                    | Vouched.IndirectBranchesOf (path, symbol) when not (File.Exists (Path.Combine (coreclr (), path))) ->
                        yield $"%s{symbol} is vouched for in %s{path}, which the pinned source lacks"
                    | Vouched.Callee (Some path, symbol, spelledAs) ->
                        let text = File.ReadAllText (Path.Combine (coreclr (), path))

                        let mentioned =
                            match spelledAs with
                            | Some spelling -> text.Contains spelling
                            | None ->
                                text.Contains symbol
                                || routinesOf path
                                   |> List.exists (fun routine -> CppSource.names routine.Name symbol)

                        if not mentioned then
                            yield $"%s{symbol} is vouched for in %s{path}, which neither defines nor mentions it"

                        for routine in routinesOf path do
                            if
                                vouch.Runs <> VouchedRuns.Unreached
                                && CppSource.names routine.Name symbol
                                && managedCall.IsMatch routine.Text
                            then
                                yield $"%s{path}:%s{routine.Name} is vouched for, and calls managed code"
                    | Vouched.IndirectBranchesOf (path, routine) ->
                        let branches =
                            routinesOf path
                            |> List.exists (fun defined ->
                                defined.Name = routine && defined.Calls.Contains CppSource.indirectBranch
                            )

                        if not branches then
                            yield
                                $"The indirect branches of %s{routine} are vouched for, and %s{path} defines no %s{routine} that makes one"
            ]

        if not problems.IsEmpty then
            failwith (String.concat "\n" problems)

    [<Test>]
    let ``every vouch is for a call that a routine a row cites makes`` () : unit =
        Net11Frontier.park Net11Frontier.nativeContractTable

        let called =
            rows
            |> List.collect citedRoutines
            |> List.collect (fun (source, routine) ->
                routine.Calls
                |> Seq.map (fun called -> source.Path, routine.Name, called)
                |> List.ofSeq
            )
            |> List.distinct

        let unused =
            vouches.Force ()
            |> List.filter (fun vouch ->
                not (
                    called
                    |> List.exists (fun (path, routine, called) -> covers vouch path routine called)
                )
            )
            |> List.map (fun vouch -> $"%A{vouch.Vouched}")
            |> String.concat ", "

        if unused <> "" then
            failwith $"No routine a row cites calls these, which are vouched for: %s{unused}"

    /// What CoreLib's `CastHelpers.ChkCastAny_NoCacheLookup`, which the cast helpers fall back to
    /// and which asks the QCall `IsInstanceOf_NoCacheLookup` to throw, raises on the real runtime
    /// casting `value` to `target`: the full name of the exception's type, if it raises one.
    let private castFallbackRaises (target : Type) (value : obj) : string option =
        let fallback =
            typeof<obj>.Assembly
                .GetType("System.Runtime.CompilerServices.CastHelpers", true)
                .GetMethod ("ChkCastAny_NoCacheLookup", BindingFlags.Static ||| BindingFlags.NonPublic)

        let handle =
            Pointer.Box (
                NativePtr.toVoidPtr (NativePtr.ofNativeInt<byte> target.TypeHandle.Value),
                typeof<Void>.MakePointerType ()
            )

        // Unwrapped, since wrapping makes a `TargetInvocationException`.
        try
            // A static method, so no receiver.
            fallback.Invoke ((null : obj), BindingFlags.DoNotWrapExceptions, null, [| handle ; value |], null)
            |> ignore<obj>

            None
        with e ->
            Some (e.GetType().FullName)

    [<Test>]
    let ``what the cast helpers' fallback raises on the real runtime is in its row`` () : unit =
        let row = (NativeContractTable.qcalls.Force ()).["IsInstanceOf_NoCacheLookup"]

        let raises =
            row.Contract.Raises |> List.map (fun name -> name.FullName) |> Set.ofList

        let raised = castFallbackRaises typeof<IDisposable> (box "not disposable")
        raised |> shouldEqual (Some "System.InvalidCastException")
        raises.Contains raised.Value |> shouldEqual true

        castFallbackRaises typeof<IComparable> (box "comparable") |> shouldEqual None

    [<Test>]
    let ``what the unbox helpers' throw raises on the real runtime is in its row`` () : unit =
        let row = (NativeContractTable.qcalls.Force ()).["ThrowInvalidCastException"]

        let raises =
            row.Contract.Raises |> List.map (fun name -> name.FullName) |> Set.ofList

        let voidPointer = typeof<Void>.MakePointerType ()

        // The overload taking both types' handles, which `Unbox_TypeTest` calls.
        let throwing =
            typeof<obj>.Assembly
                .GetType("System.Runtime.CompilerServices.CastHelpers", true)
                .GetMethod (
                    "ThrowInvalidCastException",
                    BindingFlags.Static ||| BindingFlags.NonPublic,
                    [| voidPointer ; voidPointer |]
                )

        let handle (ty : Type) : obj =
            Pointer.Box (NativePtr.toVoidPtr (NativePtr.ofNativeInt<byte> ty.TypeHandle.Value), voidPointer)

        let raised =
            try
                // A static method, so no receiver.
                throwing.Invoke (
                    (null : obj),
                    BindingFlags.DoNotWrapExceptions,
                    null,
                    [| handle typeof<string> ; handle typeof<int> |],
                    null
                )
                |> ignore<obj>

                None
            with e ->
                Some (e.GetType().FullName)

        raised |> shouldEqual (Some "System.InvalidCastException")
        raises.Contains raised.Value |> shouldEqual true
        row.Contract.CanReturn |> shouldEqual false

    /// Stores a reference through a reference to a location.
    type private Store = delegate of byref<obj> * obj -> unit

    [<Test>]
    let ``what the cast helpers' write barrier raises on the real runtime is in its row`` () : unit =
        let row =
            (NativeContractTable.fcalls.Force ()).[("System.Runtime.CompilerServices.CastHelpers", "WriteBarrier")]

        let raises =
            row.Contract.Raises |> List.map (fun name -> name.FullName) |> Set.ofList

        let writeBarrier =
            typeof<obj>.Assembly
                .GetType("System.Runtime.CompilerServices.CastHelpers", true)
                .GetMethod ("WriteBarrier", BindingFlags.Static ||| BindingFlags.NonPublic)

        // A private method of CoreLib, which only code that skips visibility checks can call.
        let store =
            let method =
                Emit.DynamicMethod (
                    "Store",
                    typeof<Void>,
                    [| typeof<obj>.MakeByRefType () ; typeof<obj> |],
                    typeof<obj>.Module,
                    true
                )

            let il = method.GetILGenerator ()
            il.Emit Emit.OpCodes.Ldarg_0
            il.Emit Emit.OpCodes.Ldarg_1
            il.Emit (Emit.OpCodes.Call, writeBarrier)
            il.Emit Emit.OpCodes.Ret
            method.CreateDelegate typeof<Store> :?> Store

        // An array's element, as StelemRef stores into. The barrier is the GC's unchecked one,
        // which marks the GC's tables for the location without checking that it is in the GC heap;
        // after it has stored into a local, a fault in it ends the process on macOS arm64 rather
        // than raising.
        let locations : obj[] = Array.zeroCreate 1
        let stored = obj ()
        store.Invoke (&locations.[0], stored)
        Object.ReferenceEquals (locations.[0], stored) |> shouldEqual true
        row.Contract.CanReturn |> shouldEqual true

        let raised =
            try
                store.Invoke (&Runtime.CompilerServices.Unsafe.NullRef<obj> (), stored)
                None
            with e ->
                Some (e.GetType().FullName)

        raised |> shouldEqual (Some "System.NullReferenceException")
        raises.Contains raised.Value |> shouldEqual true

    /// The rows whose cited code calls a routine vouched for as loading types, by native.
    let private loadingRows () : Set<string> =
        let loading =
            vouches.Force () |> List.filter (fun vouch -> vouch.Runs = VouchedRuns.Loads)

        rows
        |> List.filter (fun row ->
            citedRoutines row
            |> List.exists (fun (source, routine) ->
                routine.Calls
                |> Seq.exists (fun called ->
                    loading
                    |> List.exists (fun vouch -> covers vouch source.Path routine.Name called)
                )
            )
        )
        |> List.map (fun row -> $"%A{row.Native}")
        |> Set.ofList

    [<Test>]
    let ``a row holds under named types loading exactly when its code loads types`` () : unit =
        Net11Frontier.park Net11Frontier.nativeContractTable

        let assuming =
            rows
            |> List.filter (fun row -> row.Contract.Assumes |> List.contains NativeAssumption.NamedTypesLoad)
            |> List.map (fun row -> $"%A{row.Native}")
            |> Set.ofList

        assuming |> shouldEqual (loadingRows ())

    [<Test>]
    let ``the table's reader refuses a malformed row`` () : unit =
        let row (fields : string list) : string = String.concat "\t" fields

        let good =
            [
                "QCall"
                "Entry"
                "System.OutOfMemoryException"
                "returns"
                "none"
                "System.Threading.SynchronizationContext::InvokeWaitMethodHelper(?,?,?,?)"
                "NamedTypesLoad"
                "vm/file.cpp:Entry"
                "Because."
            ]

        NativeContractTable.ofText (row good) |> List.length |> shouldEqual 1

        let replacing (index : int) (field : string) : string =
            good
            |> List.mapi (fun i existing -> if i = index then field else existing)
            |> row

        let malformed =
            [
                row (List.take 8 good)
                replacing 0 "Call"
                replacing 0 "FCall"
                replacing 0 "InstanceFCall"
                replacing 2 "OutOfMemoryException"
                replacing 2 "System.OutOfMemoryException|"
                replacing 3 "sometimes"
                replacing 4 "null"
                replacing 5 ""
                replacing 5 "System.Threading.SynchronizationContext::InvokeWaitMethodHelper"
                replacing 5 "InvokeWaitMethodHelper(?)"
                replacing 5 "System.Threading.SynchronizationContext::(?)"
                replacing 5 "::InvokeWaitMethodHelper(?)"
                replacing 5 "System.Threading.SynchronizationContext::InvokeWaitMethodHelper(?"
                replacing 5 "System.Threading.SynchronizationContext::InvokeWaitMethodHelper(?)x"
                replacing 5 "System.Threading.SynchronizationContext::InvokeWaitMethodHelper(?,,?)"
                replacing 5 "System.Threading.SynchronizationContext::InvokeWaitMethodHelper(?,0)"
                replacing 5 "System.Threading.SynchronizationContext::InvokeWaitMethodHelper(?)|"
                replacing 5 "System.Threading.SynchronizationContext::InvokeWaitMethodHelper(?)|-"
                replacing 6 ""
                replacing 6 "NamedTypes"
                replacing 6 "NamedTypesLoad|"
                replacing 6 "NamedTypesLoad|NamedTypesLoad"
                replacing 6 "NamedTypesLoad|-"
                replacing 7 "vm/file.cpp"
                replacing 7 ":Entry"
                replacing 1 "Entry Point"
                replacing 8 " "
            ]

        for line in malformed do
            Assert.Throws<exn> (fun () -> NativeContractTable.ofText line |> ignore<NativeContractRow list>)
            |> ignore<exn>

    [<Test>]
    let ``the table's reader tells a static FCall from an instance one`` () : unit =
        let read (kind : string) : TabulatedNative =
            let line =
                String.concat
                    "\t"
                    [
                        kind
                        "System.Ty::Method"
                        "-"
                        "returns"
                        "none"
                        "-"
                        "-"
                        "vm/file.cpp:Entry"
                        "Because."
                    ]

            match NativeContractTable.ofText line with
            | [ read ] -> read.Native
            | read -> failwith $"%s{line} read as %d{read.Length} rows"

        read "FCall" |> shouldEqual (TabulatedNative.FCall ("System.Ty", "Method"))

        read "InstanceFCall"
        |> shouldEqual (TabulatedNative.InstanceFCall ("System.Ty", "Method"))

    /// A row's `runs` field as the table spells it.
    let private spellRuns (calls : NativeCall list) : string =
        match calls with
        | [] -> "-"
        | calls ->
            calls
            |> List.map (fun call ->
                match call with
                | NativeCall.Method call ->
                    let arguments =
                        call.Arguments
                        |> List.map (fun argument ->
                            match argument with
                            | NativeArgument.Unnamed -> "?"
                        )
                        |> String.concat ","

                    $"%s{call.DeclaringType}::%s{call.Method}(%s{arguments})"
                | NativeCall.ResourceMessage -> "ResourceMessage"
            )
            |> String.concat "|"

    [<Test>]
    let ``the table's reader reads back the calls a row runs`` () : unit =
        // A name as the table spells a type's or a method's: no white space, and none of the
        // characters that delimit the field.
        let name : Gen<string> =
            gen {
                let! head = Gen.elements [ 'A' ; 'b' ; '_' ; '<' ]
                let! tail = Gen.listOf (Gen.elements [ 'x' ; 'Y' ; '0' ; '.' ; '`' ; '>' ; '_' ])
                return System.String (Array.ofList (head :: tail))
            }

        let call : Gen<NativeCall> =
            gen {
                let! declaringType = name
                let! method = name
                let! arity = Gen.choose (0, 5)

                return
                    NativeCall.Method
                        {
                            DeclaringType = declaringType
                            Method = method
                            Arguments = List.replicate arity NativeArgument.Unnamed
                        }
            }

        let property (calls : NativeCall list) : unit =
            let line =
                String.concat
                    "\t"
                    [
                        "QCall"
                        "Entry"
                        "-"
                        "returns"
                        "none"
                        spellRuns calls
                        "-"
                        "vm/file.cpp:Entry"
                        "Because."
                    ]

            match NativeContractTable.ofText line with
            | [ read ] -> read.Contract.Runs |> shouldEqual calls
            | read -> failwith $"%s{line} read as %d{read.Length} rows"

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 400, Prop.forAll (Arb.fromGen (Gen.listOf call)) property)

    [<Test>]
    let ``the table's reader reads back the assumptions a row holds under`` () : unit =
        let read (assumes : string) : NativeAssumption list =
            let line =
                String.concat
                    "\t"
                    [
                        "QCall"
                        "Entry"
                        "-"
                        "returns"
                        "none"
                        "-"
                        assumes
                        "vm/file.cpp:Entry"
                        "Because."
                    ]

            match NativeContractTable.ofText line with
            | [ read ] -> read.Contract.Assumes
            | read -> failwith $"%s{line} read as %d{read.Length} rows"

        read "-" |> shouldEqual []
        read "NamedTypesLoad" |> shouldEqual [ NativeAssumption.NamedTypesLoad ]

    /// An image named `name` that disables runtime marshalling, declaring natives a row names and
    /// others that differ from one in a single respect: FCalls on `System.Environment`,
    /// `System.Runtime.InteropServices.Marshal`, `System.String` and
    /// `System.Runtime.CompilerServices.MethodTable`, and P/Invokes into `QCall` on `Interop`.
    let private fabricateNatives (name : string) : byte[] =
        let builder =
            System.Reflection.Emit.PersistedAssemblyBuilder (System.Reflection.AssemblyName name, typeof<obj>.Assembly)

        builder.SetCustomAttribute (
            System.Reflection.Emit.CustomAttributeBuilder (
                typeof<Runtime.CompilerServices.DisableRuntimeMarshallingAttribute>.GetConstructor [||],
                [||]
            )
        )

        let modul = builder.DefineDynamicModule name

        let staticClass (fullName : string) =
            modul.DefineType (
                fullName,
                System.Reflection.TypeAttributes.Public
                ||| System.Reflection.TypeAttributes.Abstract
                ||| System.Reflection.TypeAttributes.Sealed
            )

        let fcall
            (ty : System.Reflection.Emit.TypeBuilder)
            (isStatic : bool)
            (method : string)
            (returns : Type)
            (parameters : Type[])
            =
            let attributes =
                System.Reflection.MethodAttributes.Public
                ||| (if isStatic then
                         System.Reflection.MethodAttributes.Static
                     else
                         System.Reflection.MethodAttributes.PrivateScope)

            (ty.DefineMethod (method, attributes, returns, parameters)).SetImplementationFlags
                System.Reflection.MethodImplAttributes.InternalCall

        let environment = staticClass "System.Environment"
        fcall environment true "get_CurrentManagedThreadId" typeof<int> [||]
        environment.CreateType () |> ignore<Type>

        // Two FCalls of one name, which CoreCLR binds by name alone.
        let marshal = staticClass "System.Runtime.InteropServices.Marshal"
        fcall marshal true "GetLastPInvokeError" typeof<int> [||]
        fcall marshal true "GetLastPInvokeError" typeof<int> [| typeof<int> |]
        marshal.CreateType () |> ignore<Type>

        // An instance FCall of a name a row gives a static one.
        let instance =
            modul.DefineType ("System.String", System.Reflection.TypeAttributes.Public)

        fcall instance false "FastAllocateString" typeof<string> [| typeof<int> |]
        instance.CreateType () |> ignore<Type>

        // An instance FCall of a name a row gives an instance one, and a static FCall of another.
        let methodTable =
            modul.DefineType ("System.Runtime.CompilerServices.MethodTable", System.Reflection.TypeAttributes.Public)

        fcall methodTable false "GetNumInstanceFieldBytes" typeof<uint32> [||]
        fcall methodTable true "GetPrimitiveCorElementType" typeof<int> [||]
        methodTable.CreateType () |> ignore<Type>

        let interop = staticClass "Interop"

        let qcall (method : string) (entryPoint : string) (parameters : Type[]) =
            let m =
                interop.DefinePInvokeMethod (
                    method,
                    "QCall",
                    entryPoint,
                    System.Reflection.MethodAttributes.Public
                    ||| System.Reflection.MethodAttributes.Static
                    ||| System.Reflection.MethodAttributes.PinvokeImpl,
                    System.Reflection.CallingConventions.Standard,
                    typeof<Void>,
                    parameters,
                    Runtime.InteropServices.CallingConvention.Winapi,
                    Runtime.InteropServices.CharSet.Ansi
                )

            m.SetImplementationFlags System.Reflection.MethodImplAttributes.PreserveSig

        let pointer = typeof<byte>.MakePointerType ()
        qcall "Clear" "Buffer_Clear" [| pointer ; typeof<unativeint> |]
        // A value its stub would have to convert.
        qcall "ClearObject" "Buffer_Clear" [| typeof<obj> ; typeof<unativeint> |]
        qcall "Unlisted" "Buffer_Unlisted" [| pointer ; typeof<unativeint> |]
        interop.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    [<Test>]
    let ``only a CoreLib's one FCall of a name, static or instance as its row says, or its plain QCall of an entry point, is that row``
        ()
        : unit
        =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let recognised (name : string) : (string * TabulatedNative option) list =
            use stream = new MemoryStream (fabricateNatives name)
            let image = Assembly.read loggerFactory None stream

            [
                // The default constructor of the class `String`.
                for KeyValue (handle, method) in image.Methods |> Seq.filter (fun m -> m.Value.Name <> ".ctor") do
                    let declaringType = image.TypeDefs.[method.RequiredDeclaringType.Definition.Get]

                    let native =
                        match NativeMethod.recognise image handle with
                        | Some (NativeMethod.Tabulated row) -> Some row.Native
                        | Some other -> failwith $"%s{method.Name} is recognised as %A{other}"
                        | None -> None

                    yield $"%s{declaringType.Name}::%s{method.Name}/%d{method.Signature.ParameterTypes.Length}", native
            ]
            |> List.sort

        recognised "System.Private.CoreLib"
        |> shouldEqual
            [
                "Environment::get_CurrentManagedThreadId/0",
                Some (TabulatedNative.FCall ("System.Environment", "get_CurrentManagedThreadId"))
                "Interop::Clear/2", Some (TabulatedNative.QCall "Buffer_Clear")
                "Interop::ClearObject/2", None
                "Interop::Unlisted/2", None
                "Marshal::GetLastPInvokeError/0", None
                "Marshal::GetLastPInvokeError/1", None
                "MethodTable::GetNumInstanceFieldBytes/0",
                Some (
                    TabulatedNative.InstanceFCall (
                        "System.Runtime.CompilerServices.MethodTable",
                        "GetNumInstanceFieldBytes"
                    )
                )
                "MethodTable::GetPrimitiveCorElementType/0", None
                "String::FastAllocateString/1", None
            ]

        // Another assembly's natives are its own, whatever their names.
        recognised "Elsewhere" |> List.choose snd |> shouldEqual []
