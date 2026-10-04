namespace WoofWare.PawPrint.Test

open System
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `NativeContractTable` holds what CoreLib's native methods can do, read from the runtime's C++.
/// These tests hold each row to the CoreLibs' metadata, which must declare and recognise the native
/// it names, and to the pinned runtime source, which must define what it cites.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNativeContractTable =

    let coreLibs : TestCaseData list = TestIntrinsicBody.coreLibs

    let private rows : NativeContractRow list = NativeContractTable.rows.Force ()

    /// The pinned runtime source only exists inside the Nix devshell, so a plain `dotnet test` in a
    /// non-Nix checkout skips rather than fails.
    let private requireRuntimeSrc () : string =
        match Environment.GetEnvironmentVariable "DOTNET_RUNTIME_SRC" with
        | null
        | "" ->
            Assert.Ignore
                "DOTNET_RUNTIME_SRC is unset; run under `nix develop` to check against pinned upstream sources."

            failwith "unreachable: Assert.Ignore did not throw"
        | dir -> dir

    [<Test>]
    let ``the table has rows of both kinds, each native once`` () : unit =
        let fcalls = NativeContractTable.fcalls.Force ()
        let qcalls = NativeContractTable.qcalls.Force ()
        fcalls.Count |> shouldBeGreaterThan 0
        qcalls.Count |> shouldBeGreaterThan 0
        fcalls.Count + qcalls.Count |> shouldEqual rows.Length

    [<TestCaseSource(nameof coreLibs)>]
    let ``every exception a row raises is a CoreLib exception type`` (which : string) : unit =
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
    let ``every row names a native of the CoreLib, which is recognised as that row`` (which : string) : unit =
        let corelib = TestIntrinsicBody.coreLib which

        let natives =
            [
                for KeyValue (handle, method) in corelib.Methods do
                    let declaringType = corelib.TypeDefs.[method.RequiredDeclaringType.Definition.Get]

                    match method.Body, method.TryNativeImport with
                    | MethodBody.InternalCall, _ when not declaringType.IsNested ->
                        yield
                            TabulatedNative.FCall (declaringType.Namespace + "." + declaringType.Name, method.Name),
                            handle
                    | MethodBody.PInvoke, Some import when import.ModuleName = "QCall" ->
                        yield TabulatedNative.QCall import.EntryPointName, handle
                    | _ -> ()
            ]

        for row in rows do
            let handles =
                natives |> List.filter (fun (native, _) -> native = row.Native) |> List.map snd

            match row.Native, handles with
            | _, [] -> failwith $"No method of this CoreLib is %A{row.Native}"
            | TabulatedNative.FCall _, _ :: _ :: _ ->
                failwith $"%A{row.Native} names %d{handles.Length} methods, which CoreCLR would bind alike"
            | _ ->
                for handle in handles do
                    match NativeMethod.recognise corelib handle with
                    | Some (NativeMethod.Tabulated recognised) when recognised = row -> ()
                    | other -> failwith $"A method that is %A{row.Native} is recognised as %A{other}"

    [<Test>]
    let ``every row cites code the pinned runtime source defines, and binds the native to the first`` () : unit =
        let coreclr = Path.Combine (requireRuntimeSrc (), "src", "coreclr")

        let read (path : string) : string =
            File.ReadAllText (Path.Combine (coreclr, path))

        let ecalls = read "vm/ecalllist.h"
        let qcallEntryPoints = read "vm/qcallentrypoints.cpp"

        for row in rows do
            for source in row.Sources do
                if not (File.Exists (Path.Combine (coreclr, source.Path))) then
                    failwith $"%A{row.Native} cites %s{source.Path}, which the pinned source lacks"

                if not ((read source.Path).Contains source.Symbol) then
                    failwith $"%A{row.Native} cites %s{source.Symbol} in %s{source.Path}, which does not mention it"

            let bound = (List.head row.Sources).Symbol

            match row.Native with
            | TabulatedNative.FCall (_, method) ->
                // `FCDynamic` binds the method to a slot whose implementation is assigned at
                // startup, defaulting to the one `vm/ecall.h` names.
                let byName = $"FCFuncElement(\"%s{method}\", %s{bound})"
                let dynamic = $"FCDynamic(\"%s{method}\", ECall::%s{method})"

                if not (ecalls.Contains byName || ecalls.Contains dynamic) then
                    failwith $"vm/ecalllist.h binds %s{method} to neither %s{bound} nor a dynamic slot"
            | TabulatedNative.QCall entryPoint ->
                bound |> shouldEqual entryPoint

                if not (qcallEntryPoints.Contains $"DllImportEntry(%s{entryPoint})") then
                    failwith $"vm/qcallentrypoints.cpp lists no %s{entryPoint}"

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
                row (List.take 6 good)
                replacing 0 "Call"
                replacing 0 "FCall"
                replacing 2 "OutOfMemoryException"
                replacing 2 "System.OutOfMemoryException|"
                replacing 3 "sometimes"
                replacing 4 "null"
                replacing 5 "vm/file.cpp"
                replacing 5 ":Entry"
                replacing 1 "Entry Point"
                replacing 6 " "
            ]

        for line in malformed do
            Assert.Throws<exn> (fun () -> NativeContractTable.ofText line |> ignore<NativeContractRow list>)
            |> ignore<exn>

    /// An image named `name` that disables runtime marshalling, declaring natives a row names and
    /// others that differ from one in a single respect: FCalls on `System.Environment` and
    /// `System.Runtime.InteropServices.Marshal`, and P/Invokes into `QCall` on `Interop`.
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
    let ``only a CoreLib's one static FCall of a name, or its plain QCall of an entry point, is that row`` () : unit =
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
                "String::FastAllocateString/1", None
            ]

        // Another assembly's natives are its own, whatever their names.
        recognised "Elsewhere" |> List.choose snd |> shouldEqual []
