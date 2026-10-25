namespace WoofWare.PawPrint.Test

open System
open System.Collections.Concurrent
open System.IO
open System.Reflection
open System.Reflection.Emit
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// A CoreLib image whose contents a fixture audits.
[<RequireQualifiedAccess>]
type AuditedCoreLib =
    /// The framework under test's CoreLib.
    | UnderTest
    /// The pinned linux-x64 CoreLib (`FrameworkUnderTest.requireLinuxFrameworkDirectory`), which is what CI and production
    /// run.
    | PinnedLinuxX64

/// `IntrinsicBody` says what CoreCLR runs for an `[Intrinsic]` method: its own IL, a JIT
/// expansion (the IL calls itself), a VM-substituted body (CoreLib's IL cannot return), code the
/// JIT puts in place of every call, or no IL at all. The interpreter runs `OwnIl` as it stands,
/// refuses the two placeholders unless `Intrinsics.call` implements them, and refuses a call
/// replaced at every call site.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestIntrinsicBody =

    let private readCache = ConcurrentDictionary<string, Lazy<DumpedAssembly>> ()

    let private readCoreLib (path : string) : DumpedAssembly =
        let read () =
            let _, loggerFactory = LoggerFactory.makeTest ()
            Assembly.readFile loggerFactory path

        readCache.GetOrAdd(path, (fun _ -> lazy (read ()))).Value

    /// The framework under test's CoreLib (whose flavour is the host's), and the pinned linux-x64
    /// one, which is what CI and production run.
    let coreLibs : TestCaseData list =
        [
            TestCaseData(AuditedCoreLib.UnderTest).SetArgDisplayNames "CoreLib under test"
            TestCaseData(AuditedCoreLib.PinnedLinuxX64).SetArgDisplayNames "pinned linux-x64 CoreLib"
        ]

    let coreLib (which : AuditedCoreLib) : DumpedAssembly =
        match which with
        | AuditedCoreLib.UnderTest -> readCoreLib (FrameworkUnderTest.coreLibPath ())
        | AuditedCoreLib.PinnedLinuxX64 ->
            let dir = FrameworkUnderTest.requireLinuxFrameworkDirectory ()
            let corelib = readCoreLib (Path.Combine (dir, "System.Private.CoreLib.dll"))

            // A leg audits only its own runtime's images.
            FrameworkUnderTest.checkServes (FrameworkUnderTest.runtime ()) corelib
            |> Result.mapError FrameworkUnderTest.describe
            |> shouldEqual (Ok ())

            corelib

    /// A parameter type as the transcription below spells it, after corelib.h's signature names.
    let rec private shape (ty : TypeDefn) : string =
        match ty with
        | TypeDefn.Modified m -> shape m.Unmodified
        | TypeDefn.PrimitiveType PrimitiveType.Int32 -> "int32"
        | TypeDefn.PrimitiveType PrimitiveType.UInt32 -> "uint32"
        | TypeDefn.PrimitiveType PrimitiveType.Byte -> "uint8"
        | TypeDefn.PrimitiveType PrimitiveType.IntPtr -> "nint"
        | TypeDefn.PrimitiveType PrimitiveType.UIntPtr -> "nuint"
        | TypeDefn.PrimitiveType PrimitiveType.Object -> "object"
        | TypeDefn.PrimitiveType PrimitiveType.Double -> "float64"
        | TypeDefn.Pointer TypeDefn.Void -> "void*"
        | TypeDefn.GenericMethodParameter i -> $"T%d{i}"
        | TypeDefn.Byref inner -> "ref " + shape inner
        | other -> $"%O{other}"

    /// A method as the transcription names it: class, method, and parameter shapes, where `None`
    /// is corelib.h's `NoSig` (every overload of that name).
    type private Named =
        {
            Namespace : string
            Class : string
            Method : string
            Parameters : string list option
        }

    let private unsafe (name : string) (parameters : string list option) : Named =
        {
            Namespace = "System.Runtime.CompilerServices"
            Class = "Unsafe"
            Method = name
            Parameters = parameters
        }

    /// Every method whose body CoreCLR's VM may substitute: the `METHOD__*` binders the four
    /// `getILIntrinsicImplementationFor*` functions in jitinterface.cpp test for, spelled as
    /// corelib.h defines them, at the pinned runtime.
    let private vmSubstituted : Named list =
        [
            unsafe "AsPointer" None
            unsafe "IsNullRef" None
            unsafe "NullRef" None
            unsafe "AsRef" (Some [ "ref T0" ])
            unsafe "As" (Some [ "ref T0" ])
            unsafe "As" (Some [ "object" ])
            unsafe "Add" (Some [ "ref T0" ; "int32" ])
            unsafe "Add" (Some [ "ref T0" ; "nint" ])
            unsafe "Add" (Some [ "ref T0" ; "nuint" ])
            unsafe "Add" (Some [ "void*" ; "int32" ])
            unsafe "ByteOffset" None
            unsafe "AddByteOffset" (Some [ "ref T0" ; "nint" ])
            unsafe "AddByteOffset" (Some [ "ref T0" ; "nuint" ])
            unsafe "AreSame" None
            unsafe "Copy" (Some [ "void*" ; "ref T0" ])
            unsafe "Copy" (Some [ "ref T0" ; "void*" ])
            unsafe "CopyBlock" (Some [ "void*" ; "void*" ; "uint32" ])
            unsafe "CopyBlock" (Some [ "ref uint8" ; "ref uint8" ; "uint32" ])
            unsafe "CopyBlockUnaligned" (Some [ "void*" ; "void*" ; "uint32" ])
            unsafe "CopyBlockUnaligned" (Some [ "ref uint8" ; "ref uint8" ; "uint32" ])
            unsafe "IsAddressGreaterThan" None
            unsafe "IsAddressLessThan" None
            unsafe "InitBlockUnaligned" (Some [ "ref uint8" ; "uint8" ; "uint32" ])
            unsafe "InitBlock" (Some [ "void*" ; "uint8" ; "uint32" ])
            unsafe "InitBlock" (Some [ "ref uint8" ; "uint8" ; "uint32" ])
            unsafe "InitBlockUnaligned" (Some [ "void*" ; "uint8" ; "uint32" ])
            unsafe "ReadUnaligned" (Some [ "ref uint8" ])
            unsafe "WriteUnaligned" (Some [ "ref uint8" ; "T0" ])
            unsafe "ReadUnaligned" (Some [ "void*" ])
            unsafe "WriteUnaligned" (Some [ "void*" ; "T0" ])
            unsafe "Read" None
            unsafe "SkipInit" (Some [ "ref T0" ])
            unsafe "Subtract" (Some [ "ref T0" ; "int32" ])
            unsafe "Subtract" (Some [ "ref T0" ; "nint" ])
            unsafe "Subtract" (Some [ "ref T0" ; "nuint" ])
            unsafe "Subtract" (Some [ "void*" ; "int32" ])
            unsafe "SubtractByteOffset" (Some [ "ref T0" ; "nint" ])
            unsafe "SubtractByteOffset" (Some [ "ref T0" ; "nuint" ])
            unsafe "Unbox" None
            unsafe "Write" None
            {
                Namespace = "System.Threading"
                Class = "Interlocked"
                Method = "CompareExchange"
                Parameters = Some [ "ref T0" ; "T0" ; "T0" ]
            }
            // corelib.h also binds `CopyConstruct`, but only under `FEATURE_IJW`, which is Windows'.
            for name in [ "IsBitwiseEquatable" ; "EnumEquals" ; "EnumCompareTo" ] do
                {
                    Namespace = "System.Runtime.CompilerServices"
                    Class = "RuntimeHelpers"
                    Method = name
                    Parameters = None
                }
            {
                Namespace = "System"
                Class = "Activator"
                Method = "CreateInstance"
                Parameters = Some []
            }
        ]

    /// Members of `vmSubstituted` whose CoreLib IL is nonetheless a complete implementation, read
    /// and judged by hand: the VM's body is an optimisation of it, or (for the generic and
    /// `EnumEquals`-style entries) the VM substitutes only for some instantiations and runs this IL
    /// for the rest.
    let private reviewedFallbacks : Named list =
        [
            unsafe "IsNullRef" None
            unsafe "NullRef" None
            unsafe "Read" None
            unsafe "Write" None
            unsafe "InitBlockUnaligned" (Some [ "ref uint8" ; "uint8" ; "uint32" ])
            {
                Namespace = "System.Threading"
                Class = "Interlocked"
                Method = "CompareExchange"
                Parameters = Some [ "ref T0" ; "T0" ; "T0" ]
            }
            for name in [ "EnumEquals" ; "EnumCompareTo" ] do
                {
                    Namespace = "System.Runtime.CompilerServices"
                    Class = "RuntimeHelpers"
                    Method = name
                    Parameters = None
                }
            {
                Namespace = "System"
                Class = "Activator"
                Method = "CreateInstance"
                Parameters = Some []
            }
        ]

    type private Method =
        {
            Handle : System.Reflection.Metadata.MethodDefinitionHandle
            Namespace : string
            Class : string
            Name : string
            Parameters : string list
            GenericArity : int
        }

    let private methodsOf (assembly : DumpedAssembly) : Method list =
        [
            for KeyValue (_, ty) in assembly.TypeDefs do
                for m in ty.Methods do
                    match m.TryMetadata with
                    | Some facts ->
                        {
                            Handle = facts.Handle
                            Namespace = ty.Namespace
                            Class = ty.Name
                            Name = m.Name
                            Parameters = m.Signature.ParameterTypes |> List.map shape
                            GenericArity = m.Signature.GenericParameterCount
                        }
                    | None -> ()
        ]

    let private names (named : Named) (m : Method) : bool =
        m.Namespace = named.Namespace
        && m.Class = named.Class
        && m.Name = named.Method
        && (
            match named.Parameters with
            | None -> true
            | Some ps -> ps = m.Parameters
        )

    let private describe (m : Method) : string =
        let ps = m.Parameters |> String.concat ", "
        $"%s{m.Namespace}.%s{m.Class}::%s{m.Name}(%s{ps})"

    let private find (assembly : DumpedAssembly) (ns : string) (cls : string) (name : string) (ps : string list) =
        match
            methodsOf assembly
            |> List.filter (fun m -> m.Namespace = ns && m.Class = cls && m.Name = name && m.Parameters = ps)
        with
        | [ m ] -> m.Handle
        | found -> failwith $"expected one %s{ns}.%s{cls}::%s{name}(%A{ps}), found %d{found.Length}"

    [<TestCaseSource(nameof coreLibs)>]
    let ``named intrinsics classify as CoreCLR treats them`` (which : AuditedCoreLib) : unit =
        let corelib = coreLib which

        let expect (ns : string) (cls : string) (name : string) (ps : string list) (expected : IntrinsicBody) =
            let handle = find corelib ns cls name ps
            IntrinsicBody.isIntrinsic corelib handle |> shouldEqual true
            IntrinsicBody.classify corelib handle |> shouldEqual expected

        // Placeholders that call themselves, whatever the architecture.
        expect
            "System.Runtime.CompilerServices"
            "RuntimeHelpers"
            "GetMethodTable"
            [ "object" ]
            (IntrinsicBody.JitExpansion (JitExpansion.Primitive IntrinsicPrimitive.MethodTableOf))

        expect
            "System.Threading"
            "Volatile"
            "ReadBarrier"
            []
            (IntrinsicBody.JitExpansion (JitExpansion.Primitive IntrinsicPrimitive.ReadBarrier))
        // Placeholders that throw, which the VM replaces.
        expect "System.Runtime.CompilerServices" "Unsafe" "As" [ "object" ] IntrinsicBody.VmSubstitution
        expect "System.Runtime.CompilerServices" "RuntimeHelpers" "IsBitwiseEquatable" [] IntrinsicBody.VmSubstitution
        // Intrinsics whose IL CoreCLR runs whenever it does not expand them.
        expect "System.Runtime.CompilerServices" "Unsafe" "SizeOf" [] IntrinsicBody.OwnIl
        expect "System.Runtime.CompilerServices" "Unsafe" "Read" [ "void*" ] IntrinsicBody.OwnIl
        expect "System" "String" "get_Length" [] IntrinsicBody.OwnIl
        expect "System.Threading" "Interlocked" "And" [ "ref int32" ; "int32" ] IntrinsicBody.OwnIl
        expect "System" "Math" "Acos" [ "float64" ] IntrinsicBody.NoIl

    [<TestCaseSource(nameof coreLibs)>]
    let ``exactly one architecture's base ISA query is a JIT expansion`` (which : AuditedCoreLib) : unit =
        // A CoreLib is compiled for one architecture: that architecture's hardware-intrinsic
        // classes carry placeholder IL, and every other's is a stub returning `false`.
        let corelib = coreLib which

        let classify (ns : string) (cls : string) =
            IntrinsicBody.classify corelib (find corelib ns cls "get_IsSupported" [])

        let x86 = classify "System.Runtime.Intrinsics.X86" "X86Base"
        let arm = classify "System.Runtime.Intrinsics.Arm" "ArmBase"

        let query (ns : string) (cls : string) =
            IntrinsicBody.JitExpansion (
                JitExpansion.IsSupportedQuery
                    {
                        Namespace = ns
                        Path = [ cls ]
                    }
            )

        if x86 = query "System.Runtime.Intrinsics.X86" "X86Base" then
            arm |> shouldEqual IntrinsicBody.OwnIl
        elif arm = query "System.Runtime.Intrinsics.Arm" "ArmBase" then
            x86 |> shouldEqual IntrinsicBody.OwnIl
        else
            failwith $"expected one IsSupported query and one OwnIl, got X86Base %A{x86} and ArmBase %A{arm}"

    [<TestCaseSource(nameof coreLibs)>]
    let ``VmSubstitution is exactly the VM's substitutions whose IL is a placeholder`` (which : AuditedCoreLib) : unit =
        let corelib = coreLib which
        let methods = methodsOf corelib

        // Every transcribed entry names something, so a typo cannot make the checks below vacuous.
        for named in vmSubstituted do
            if not (methods |> List.exists (names named)) then
                failwith $"the transcription names %A{named}, which this CoreLib does not define"

        let failures =
            [
                for m in methods do
                    if IntrinsicBody.isIntrinsic corelib m.Handle then
                        let onList = vmSubstituted |> List.exists (fun n -> names n m)
                        let reviewed = reviewedFallbacks |> List.exists (fun n -> names n m)

                        match IntrinsicBody.classify corelib m.Handle with
                        | IntrinsicBody.VmSubstitution when not onList ->
                            yield $"%s{describe m}: VmSubstitution, but the VM does not substitute it"
                        | IntrinsicBody.OwnIl when onList && not reviewed ->
                            yield
                                $"%s{describe m}: the VM substitutes it, yet it was classified OwnIl without its IL being reviewed as a fallback"
                        | IntrinsicBody.VmSubstitution when reviewed ->
                            yield $"%s{describe m}: reviewed as a working fallback, yet its IL cannot return"
                        | _ -> ()
            ]

        if not failures.IsEmpty then
            failwith (String.concat "\n" failures)

    /// What CoreCLR does with a call to each of the named intrinsics its JIT handles before testing
    /// whether to optimise (`impIntrinsic`, importercalls.cpp, from `GetStubContext` up to
    /// `betterToExpand`), by the `NI_` name the JIT gives it, across the supported runtimes.
    type private EveryCallSite =
        /// The JIT replaces every call with code of its own: `IntrinsicBody.CallSiteExpansion`.
        | Replaced
        /// Read and judged by hand: what runs is the method's IL, for this reason.
        | RunsIl of reason : string

    let private everyCallSite : (string * Named * EveryCallSite) list =
        let named (ns : string) (cls : string) (name : string) : Named =
            {
                Namespace = ns
                Class = cls
                Method = name
                Parameters = None
            }

        let stubHelpers = named "System.StubHelpers" "StubHelpers"
        let asyncHelpers = named "System.Runtime.CompilerServices" "AsyncHelpers"
        let runtimeHelpers = named "System.Runtime.CompilerServices" "RuntimeHelpers"

        let declined =
            EveryCallSite.RunsIl
                "the JIT records the name for its own pattern matching and declines to expand the call (it returns null), so the call runs the IL"

        [
            "NI_System_StubHelpers_GetStubContext", stubHelpers "GetStubContext", EveryCallSite.Replaced
            "NI_System_StubHelpers_NextCallReturnAddress", stubHelpers "NextCallReturnAddress", EveryCallSite.Replaced
            "NI_System_StubHelpers_AsyncCallContinuation", stubHelpers "AsyncCallContinuation", EveryCallSite.Replaced
            "NI_System_Runtime_CompilerServices_AsyncHelpers_AsyncCallContinuation",
            asyncHelpers "AsyncCallContinuation",
            EveryCallSite.Replaced
            "NI_System_Runtime_CompilerServices_AsyncHelpers_AsyncSuspend",
            asyncHelpers "AsyncSuspend",
            EveryCallSite.Replaced
            "NI_System_Runtime_CompilerServices_AsyncHelpers_TailAwait",
            asyncHelpers "TailAwait",
            EveryCallSite.Replaced
            "NI_System_Runtime_CompilerServices_RuntimeHelpers_SetNextCallGenericContext",
            runtimeHelpers "SetNextCallGenericContext",
            EveryCallSite.Replaced
            "NI_System_Runtime_CompilerServices_RuntimeHelpers_SetNextCallAsyncContinuation",
            runtimeHelpers "SetNextCallAsyncContinuation",
            EveryCallSite.Replaced
            "NI_System_Runtime_CompilerServices_RuntimeHelpers_IsRuntimeAsync",
            runtimeHelpers "IsRuntimeAsync",
            EveryCallSite.RunsIl
                "the JIT replaces the call with whether the method being compiled is runtime-async (`compIsAsync`), and PawPrint runs no method as runtime-async, which is what the IL's `false` answers"
            "NI_System_Runtime_CompilerServices_AsyncHelpers_Await", asyncHelpers "Await", declined
            "NI_System_Runtime_CompilerServices_AsyncHelpers_AwaitAwaiter", asyncHelpers "AwaitAwaiter", declined
            "NI_System_Runtime_CompilerServices_AsyncHelpers_UnsafeAwaitAwaiter",
            asyncHelpers "UnsafeAwaitAwaiter",
            declined
            "NI_System_Runtime_CompilerServices_AsyncHelpers_Suspend", asyncHelpers "Suspend", declined
            "NI_System_Runtime_CompilerServices_AsyncHelpers_TransparentSuspend",
            asyncHelpers "TransparentSuspend",
            declined
        ]

    [<Test>]
    let ``the JIT expands at every call site no named intrinsic the transcription lacks`` () : unit =
        let importer =
            File.ReadAllLines (
                Path.Combine (FrameworkUnderTest.requireRuntimeSource (), "src", "coreclr", "jit", "importercalls.cpp")
            )

        let start =
            importer
            |> Array.findIndex (fun line -> line.Trim () = "if (ni == NI_System_StubHelpers_GetStubContext)")

        let finish =
            importer
            |> Array.skip start
            |> Array.findIndex (fun line -> line.Trim () = "bool betterToExpand = false;")

        let named =
            importer
            |> Array.skip start
            |> Array.take finish
            |> Array.collect (fun line ->
                System.Text.RegularExpressions.Regex.Matches (line, "NI_[A-Za-z0-9_]+")
                |> Seq.map (fun m -> m.Value)
                |> Array.ofSeq
            )
            |> Set.ofArray

        // A truncated or misplaced window would pass vacuously.
        named
        |> Set.contains "NI_System_StubHelpers_NextCallReturnAddress"
        |> shouldEqual true

        let transcribed = everyCallSite |> List.map (fun (ni, _, _) -> ni) |> Set.ofList
        Set.difference named transcribed |> Set.toList |> shouldEqual []

    [<Test>]
    let ``every intrinsic the every-call-site transcription names is one a supported CoreLib defines`` () : unit =
        let corelibs =
            EmulatedRuntime.supported
            |> List.map (fun runtime ->
                let corelib = readCoreLib (FrameworkUnderTest.requireCoreLibPathOf runtime)
                corelib, methodsOf corelib
            )

        let unnamed =
            everyCallSite
            |> List.filter (fun (_, named, _) ->
                corelibs
                |> List.exists (fun (corelib, methods) ->
                    methods
                    |> List.exists (fun m -> names named m && IntrinsicBody.isIntrinsic corelib m.Handle)
                )
                |> not
            )
            |> List.map (fun (ni, _, _) -> ni)

        unnamed |> shouldEqual []

    [<TestCaseSource(nameof coreLibs)>]
    let ``CallSiteExpansion is exactly the intrinsics the JIT replaces at every call`` (which : AuditedCoreLib) : unit =
        let corelib = coreLib which

        let replaced =
            everyCallSite
            |> List.choose (fun (_, named, kind) ->
                match kind with
                | EveryCallSite.Replaced -> Some named
                | EveryCallSite.RunsIl _ -> None
            )

        let failures =
            [
                for m in methodsOf corelib do
                    if IntrinsicBody.isIntrinsic corelib m.Handle then
                        let onList = replaced |> List.exists (fun named -> names named m)

                        match IntrinsicBody.classify corelib m.Handle, onList with
                        | IntrinsicBody.CallSiteExpansion, false ->
                            yield $"%s{describe m}: CallSiteExpansion, but the JIT does not replace every call to it"
                        | other, true when other <> IntrinsicBody.CallSiteExpansion ->
                            yield $"%s{describe m}: the JIT replaces every call to it, yet it classifies as %A{other}"
                        | _ -> ()
            ]

        failures |> shouldEqual []

    [<TestCaseSource(nameof coreLibs)>]
    let ``VmSubstitution has a stub for exactly the Unsafe methods corelib.h binds`` (which : AuditedCoreLib) : unit =
        let corelib = coreLib which
        let methods = methodsOf corelib

        let rows = vmSubstituted |> List.filter (fun n -> n.Class = "Unsafe")

        // The binder takes one method per row, so a row that names several (a `NoSig` row whose
        // name is overloaded) would need the binder's choice among them modelled.
        for row in rows do
            match methods |> List.filter (names row) with
            | [ _ ] -> ()
            | found -> failwith $"corelib.h's row %A{row} names %d{found.Length} methods in this CoreLib"

        let bound =
            methods
            |> List.filter (fun m -> rows |> List.exists (fun row -> names row m))
            |> List.map describe
            |> Set.ofList

        let stubbed =
            methods
            |> List.filter (fun m -> (VmSubstitution.unsafeStub corelib m.Handle).IsSome)
            |> List.map describe
            |> Set.ofList

        bound.Count |> shouldEqual rows.Length
        stubbed |> shouldEqual bound

    [<TestCaseSource(nameof coreLibs)>]
    let ``every Unsafe placeholder runs the VM's stub`` (which : AuditedCoreLib) : unit =
        let corelib = coreLib which

        let placeholders =
            methodsOf corelib
            |> List.filter (fun m ->
                m.Class = "Unsafe"
                && IntrinsicBody.isIntrinsic corelib m.Handle
                && IntrinsicBody.classify corelib m.Handle = IntrinsicBody.VmSubstitution
            )

        placeholders |> List.length |> shouldBeGreaterThan 20

        for m in placeholders do
            match IntrinsicBody.substitutedBody corelib m.Handle, VmSubstitution.unsafeStub corelib m.Handle with
            | Some substituted, Some stub -> substituted.Instructions.Length |> shouldEqual stub.Instructions.Length
            | _ -> failwith $"%s{describe m}: a placeholder with no stub to run"

    /// The class `corelib.h` binds as `STRUCTURE_MARSHALER` on `runtime`, whose intrinsics' bodies
    /// `getMethodInfoWorker` (jitinterface.cpp) asks `TryGenerateStructMarshallingMethod` to
    /// replace: its namespace and name, or `None` where the runtime has no such row.
    let private structureMarshalerClass (runtime : EmulatedRuntime) : (string * string) option =
        let corelibH =
            Path.Combine (FrameworkUnderTest.requireRuntimeSourceOf runtime, "src", "coreclr", "vm", "corelib.h")

        let rows =
            File.ReadAllLines corelibH
            |> Array.map _.Trim()
            |> Array.filter _.StartsWith("DEFINE_CLASS(STRUCTURE_MARSHALER,")
            |> List.ofArray

        match rows with
        | [] -> None
        | [ row ] ->
            match row.Substring(row.IndexOf '(' + 1).TrimEnd(')').Split ',' |> Array.map _.Trim() with
            // `g_StubHelpersNS` is `System.StubHelpers`.
            | [| _ ; "StubHelpers" ; name |] -> Some ("System.StubHelpers", name)
            | _ -> failwith $"corelib.h's STRUCTURE_MARSHALER row is not of a shape this test reads: %s{row}"
        | several -> failwith $"corelib.h has %d{several.Length} STRUCTURE_MARSHALER rows"

    /// The bodies `TryGenerateStructMarshallingMethod` (dllimport.cpp) generates on `runtime`, each
    /// as the name `corelib.h` binds the method to and the operation its `marshalFlags` select.
    let private structureMarshalerBodies (runtime : EmulatedRuntime) : (string * StructMarshalOperation) list =
        let vm =
            Path.Combine (FrameworkUnderTest.requireRuntimeSourceOf runtime, "src", "coreclr", "vm")

        let boundNames =
            File.ReadAllLines (Path.Combine (vm, "corelib.h"))
            |> Array.map _.Trim()
            |> Array.filter _.StartsWith("DEFINE_METHOD(STRUCTURE_MARSHALER,")
            |> Array.map (fun row ->
                match row.Substring(row.IndexOf '(' + 1).TrimEnd(')').Split ',' |> Array.map _.Trim() with
                | [| _ ; binding ; name ; _ |] -> binding, name
                | _ ->
                    failwith $"corelib.h's STRUCTURE_MARSHALER method row is not of a shape this test reads: %s{row}"
            )
            |> Map.ofArray

        let dllimport = File.ReadAllLines (Path.Combine (vm, "dllimport.cpp"))

        let operationOf (flags : string) : StructMarshalOperation =
            match flags with
            | "MARSHAL_FLAG_IN | MARSHAL_FLAG_NO_CLEANUP" -> StructMarshalOperation.Marshal
            | "MARSHAL_FLAG_OUT | MARSHAL_FLAG_NO_CLEANUP" -> StructMarshalOperation.Unmarshal
            | "MARSHAL_FLAG_IN | MARSHAL_FLAG_OUT | MARSHAL_FLAG_CLEANUP_ONLY" -> StructMarshalOperation.Cleanup
            | other -> failwith $"dllimport.cpp selects marshal flags this test does not read: %s{other}"

        match
            dllimport
            |> Array.tryFindIndex _.Contains("StructMarshalStubs::TryGenerateStructMarshallingMethod(")
        with
        | None -> []
        | Some start ->
            let body =
                dllimport.[start..]
                |> Array.takeWhile (fun line -> not (line.StartsWith "}"))
                |> Array.map _.Trim()

            let pattern =
                System.Text.RegularExpressions.Regex @"CoreLibBinder::GetMethod\(METHOD__STRUCTURE_MARSHALER__(\w+)\)"

            [
                for i in 0 .. body.Length - 1 do
                    let m = pattern.Match body.[i]

                    if m.Success then
                        let flags =
                            body.[i + 1 ..]
                            |> Array.find _.StartsWith("marshalFlags = ")
                            |> fun line -> line.Substring("marshalFlags = ".Length).TrimEnd ';'

                        let binding = m.Groups.[1].Value

                        match Map.tryFind binding boundNames with
                        | Some name -> name, operationOf flags
                        | None ->
                            failwith
                                $"dllimport.cpp names METHOD__STRUCTURE_MARSHALER__%s{binding}, which corelib.h does not bind"
            ]

    let runtimes : TestCaseData list = FrameworkUnderTest.everyRuntime

    [<TestCaseSource(nameof runtimes)>]
    let ``StructMarshalling is exactly the bodies TryGenerateStructMarshallingMethod generates, with their operations``
        (runtime : EmulatedRuntime)
        : unit
        =
        let corelib = readCoreLib (FrameworkUnderTest.requireCoreLibPathOf runtime)
        let methods = methodsOf corelib
        let bodies = structureMarshalerBodies runtime

        let expected =
            match structureMarshalerClass runtime with
            | None ->
                bodies |> shouldEqual []
                Map.empty
            | Some (ns, cls) ->
                bodies |> List.length |> shouldEqual 3

                bodies
                |> List.map (fun (name, operation) ->
                    match
                        methods
                        |> List.filter (fun m -> m.Namespace = ns && m.Class = cls && m.Name = name)
                    with
                    | [ m ] ->
                        IntrinsicBody.isIntrinsic corelib m.Handle |> shouldEqual true
                        describe m, operation
                    | found -> failwith $"expected one %s{ns}.%s{cls}::%s{name}, found %d{found.Length}"
                )
                |> Map.ofList

        methods
        |> List.choose (fun m ->
            if IntrinsicBody.isIntrinsic corelib m.Handle then
                match IntrinsicBody.classify corelib m.Handle with
                | IntrinsicBody.StructMarshalling operation -> Some (describe m, operation)
                | _ -> None
            else
                None
        )
        |> Map.ofList
        |> shouldEqual expected

    [<Test>]
    let ``the pinned linux-x64 CoreLib has thousands of JIT expansions`` () : unit =
        // A floor, not a snapshot: if resolving a self-call through a MemberRef or MethodSpec
        // regressed, this is where the count would collapse. The census measured 3,737 at 10.0.7.
        let corelib = coreLib AuditedCoreLib.PinnedLinuxX64

        let expansions =
            methodsOf corelib
            |> List.filter (fun m ->
                IntrinsicBody.isIntrinsic corelib m.Handle
                && (
                    match IntrinsicBody.classify corelib m.Handle with
                    | IntrinsicBody.JitExpansion _ -> true
                    | _ -> false
                )
            )
            |> List.length

        expansions |> shouldBeGreaterThan 3_700

    // -- What each placeholder asks the JIT for, and the IL that answers it on a given CPU --

    /// The class `method` is declared on, walked from the NestedClass rows independently of
    /// `IntrinsicBody`.
    let private classOf
        (assembly : DumpedAssembly)
        (method : System.Reflection.Metadata.MethodDefinitionHandle)
        : IntrinsicClass
        =
        let rec walk (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>) (path : string list) =
            if ty.IsNested then
                walk assembly.TypeDefs.[ty.DeclaringType] (ty.Name :: path)
            else
                {
                    Namespace = ty.Namespace
                    Path = ty.Name :: path
                }

        walk assembly.TypeDefs.[assembly.Methods.[method].RequiredDeclaringType.Definition.Get] []

    /// Every intrinsic in `corelib` whose IL is a JIT expansion, with what it expands to.
    let private expansionsOf (corelib : DumpedAssembly) : (Method * JitExpansion) list =
        methodsOf corelib
        |> List.choose (fun m ->
            if IntrinsicBody.isIntrinsic corelib m.Handle then
                match IntrinsicBody.classify corelib m.Handle with
                | IntrinsicBody.JitExpansion expansion -> Some (m, expansion)
                | _ -> None
            else
                None
        )

    [<Test>]
    let ``hardware-intrinsic placeholders classify by the question they ask the JIT`` () : unit =
        let corelib = coreLib AuditedCoreLib.PinnedLinuxX64

        let expansionOf (ns : string) (cls : string) (name : string) =
            IntrinsicBody.classify corelib (find corelib ns cls name [])

        let x86 (path : string list) : IntrinsicClass =
            {
                Namespace = "System.Runtime.Intrinsics.X86"
                Path = path
            }

        expansionOf "System.Runtime.Intrinsics.X86" "X86Base" "get_IsSupported"
        |> shouldEqual (IntrinsicBody.JitExpansion (JitExpansion.IsSupportedQuery (x86 [ "X86Base" ])))

        expansionOf "System.Runtime.Intrinsics.X86" "X86Base" "Pause"
        |> shouldEqual (IntrinsicBody.JitExpansion (JitExpansion.HardwareInstruction (x86 [ "X86Base" ])))

        expansionOf "System.Runtime.Intrinsics" "Vector128" "get_IsHardwareAccelerated"
        |> shouldEqual (
            IntrinsicBody.JitExpansion (
                JitExpansion.IsHardwareAcceleratedQuery
                    {
                        Namespace = "System.Runtime.Intrinsics"
                        Path = [ "Vector128" ]
                    }
            )
        )

        expansionOf "System.Numerics" "Vector" "get_IsHardwareAccelerated"
        |> shouldEqual (
            IntrinsicBody.JitExpansion (
                JitExpansion.IsHardwareAcceleratedQuery
                    {
                        Namespace = "System.Numerics"
                        Path = [ "Vector" ]
                    }
            )
        )

        // A nested class is its own instruction set, with its own query.
        expansionsOf corelib
        |> List.filter (fun (m, _) ->
            m.Name = "get_IsSupported"
            && classOf corelib m.Handle = x86 [ "Avx512F" ; "VL" ]
        )
        |> List.map snd
        |> shouldEqual [ JitExpansion.IsSupportedQuery (x86 [ "Avx512F" ; "VL" ]) ]

    [<TestCaseSource(nameof coreLibs)>]
    let ``every hardware-intrinsic placeholder's self-call is answered on the scalar-only profile``
        (which : AuditedCoreLib)
        : unit
        =
        let corelib = coreLib which
        let expansions = expansionsOf corelib

        expansions
        |> List.filter (fun (_, e) ->
            match e with
            | JitExpansion.IsSupportedQuery _
            | JitExpansion.IsHardwareAcceleratedQuery _
            | JitExpansion.HardwareInstruction _ -> true
            | JitExpansion.Primitive _
            | JitExpansion.Unrecognised -> false
        )
        |> List.length
        |> shouldBeGreaterThan 1_000

        let failures =
            [
                for m, expansion in expansions do
                    let ownClass (c : IntrinsicClass) =
                        if c <> classOf corelib m.Handle then
                            [ $"%s{describe m}: names the class %O{c}, not its own" ]
                        else
                            []

                    match expansion, IntrinsicBody.expandSelfCall HardwareIntrinsicsProfile.ScalarOnly expansion with
                    | JitExpansion.Primitive p, SelfCallExpansion.Primitive p' when p = p' -> ()
                    | JitExpansion.IsSupportedQuery c, SelfCallExpansion.Constant false
                    | JitExpansion.IsHardwareAcceleratedQuery c, SelfCallExpansion.Constant false
                    | JitExpansion.HardwareInstruction c, SelfCallExpansion.ThrowPlatformNotSupported ->
                        yield! ownClass c
                    | _, answer -> yield $"%s{describe m}: %A{expansion} expands to %A{answer}"
            ]

        if not failures.IsEmpty then
            failwith (String.concat "\n" failures)

    [<TestCaseSource(nameof coreLibs)>]
    let ``a placeholder's self-call follows the profile`` (which : AuditedCoreLib) : unit =
        let corelib = coreLib which

        let supporting (c : IntrinsicClass) =
            { HardwareIntrinsicsProfile.ScalarOnly with
                IsSupported = Set.singleton c
            }

        let accelerating (c : IntrinsicClass) =
            { HardwareIntrinsicsProfile.ScalarOnly with
                IsHardwareAccelerated = Set.singleton c
            }

        let expand (profile : HardwareIntrinsicsProfile) (e : JitExpansion) = IntrinsicBody.expandSelfCall profile e

        let cpusWithInstructionSets =
            [
                supporting
                    {
                        Namespace = "System.Runtime.Intrinsics.X86"
                        Path = [ "X86Base" ]
                    }
                supporting
                    {
                        Namespace = "System.Runtime.Intrinsics.Arm"
                        Path = [ "AdvSimd" ]
                    }
                accelerating
                    {
                        Namespace = "System.Runtime.Intrinsics"
                        Path = [ "Vector128" ]
                    }
            ]

        let failures =
            [
                for m, expansion in expansionsOf corelib do
                    match expansion with
                    | JitExpansion.IsSupportedQuery c ->
                        if expand (supporting c) expansion <> SelfCallExpansion.Constant true then
                            yield $"%s{describe m}: not true on a CPU supporting %O{c}"

                        if expand (accelerating c) expansion <> SelfCallExpansion.Constant false then
                            yield $"%s{describe m}: IsSupported answered from the IsHardwareAccelerated set"
                    | JitExpansion.IsHardwareAcceleratedQuery c ->
                        if expand (accelerating c) expansion <> SelfCallExpansion.Constant true then
                            yield $"%s{describe m}: not true on a CPU accelerating %O{c}"

                        if expand (supporting c) expansion <> SelfCallExpansion.Constant false then
                            yield $"%s{describe m}: IsHardwareAccelerated answered from the IsSupported set"
                    | JitExpansion.HardwareInstruction c ->
                        // A CPU that has the instruction runs it.
                        for profile in [ supporting c ; accelerating c ] do
                            if expand profile expansion <> SelfCallExpansion.HardwareInstruction c then
                                yield $"%s{describe m}: not the JIT's own code on a CPU that has %O{c}"
                    | JitExpansion.Primitive p ->
                        // The profile does not decide what a primitive computes.
                        for profile in cpusWithInstructionSets do
                            if expand profile expansion <> SelfCallExpansion.Primitive p then
                                yield $"%s{describe m}: not the primitive on a CPU with instruction sets"
                    | JitExpansion.Unrecognised -> yield $"%s{describe m}: unrecognised"
            ]

        if not failures.IsEmpty then
            failwith (String.concat "\n" failures)

    /// The JIT expansions on every runtime's pinned linux-x64 CoreLib that are not hardware
    /// intrinsics, each with the operation CoreCLR's JIT emits for it.
    let private linuxPrimitivesEverywhere : (string * IntrinsicPrimitive) list =
        [
            "System.Double::ConvertToIntegerNative(float64)",
            IntrinsicPrimitive.ConvertToIntegerNative FloatWidth.Double
            "System.Double::MultiplyAddEstimate(float64, float64, float64)",
            IntrinsicPrimitive.MultiplyAddEstimate FloatWidth.Double
            "System.Math::ReciprocalEstimate(float64)", IntrinsicPrimitive.ReciprocalEstimate FloatWidth.Double
            "System.Math::ReciprocalSqrtEstimate(float64)", IntrinsicPrimitive.ReciprocalSqrtEstimate FloatWidth.Double
            "System.MathF::ReciprocalEstimate(single)", IntrinsicPrimitive.ReciprocalEstimate FloatWidth.Single
            "System.MathF::ReciprocalSqrtEstimate(single)", IntrinsicPrimitive.ReciprocalSqrtEstimate FloatWidth.Single
            "System.Runtime.CompilerServices.RuntimeHelpers::GetMethodTable(object)", IntrinsicPrimitive.MethodTableOf
            "System.Runtime.CompilerServices.RuntimeHelpers::IsReferenceOrContainsReferences()",
            IntrinsicPrimitive.IsReferenceOrContainsReferences
            "System.Runtime.CompilerServices.StaticsHelpers::VolatileReadAsByref(ref nint)",
            IntrinsicPrimitive.VolatileReadByref
            "System.Runtime.InteropServices.MemoryMarshal::GetArrayDataReference(arr[<method param 0>])",
            IntrinsicPrimitive.ArrayDataReference
            "System.Single::ConvertToIntegerNative(single)", IntrinsicPrimitive.ConvertToIntegerNative FloatWidth.Single
            "System.Single::MultiplyAddEstimate(single, single, single)",
            IntrinsicPrimitive.MultiplyAddEstimate FloatWidth.Single
            "System.Threading.Interlocked::CompareExchange(ref int32, int32, int32)",
            IntrinsicPrimitive.AtomicCompareExchange (AtomicOperand.Int32, AtomicLocation.Byref)
            "System.Threading.Interlocked::CompareExchange(ref int64, int64, int64)",
            IntrinsicPrimitive.AtomicCompareExchange (AtomicOperand.Int64, AtomicLocation.Byref)
            "System.Threading.Interlocked::CompareExchange(ref uint16, uint16, uint16)",
            IntrinsicPrimitive.AtomicCompareExchange (AtomicOperand.UInt16, AtomicLocation.Byref)
            "System.Threading.Interlocked::CompareExchange(ref uint8, uint8, uint8)",
            IntrinsicPrimitive.AtomicCompareExchange (AtomicOperand.UInt8, AtomicLocation.Byref)
            "System.Threading.Interlocked::Exchange(ref int32, int32)",
            IntrinsicPrimitive.AtomicExchange AtomicOperand.Int32
            "System.Threading.Interlocked::Exchange(ref int64, int64)",
            IntrinsicPrimitive.AtomicExchange AtomicOperand.Int64
            "System.Threading.Interlocked::Exchange(ref uint16, uint16)",
            IntrinsicPrimitive.AtomicExchange AtomicOperand.UInt16
            "System.Threading.Interlocked::Exchange(ref uint8, uint8)",
            IntrinsicPrimitive.AtomicExchange AtomicOperand.UInt8
            "System.Threading.Interlocked::ExchangeAdd(ref int32, int32)",
            IntrinsicPrimitive.AtomicAdd AtomicAddOperand.Int32
            "System.Threading.Interlocked::ExchangeAdd(ref int64, int64)",
            IntrinsicPrimitive.AtomicAdd AtomicAddOperand.Int64
            "System.Threading.Interlocked::MemoryBarrier()", IntrinsicPrimitive.FullBarrier
            "System.Threading.Thread::FastPollGC()", IntrinsicPrimitive.GcPoll
            "System.Threading.Volatile::ReadBarrier()", IntrinsicPrimitive.ReadBarrier
            "System.Threading.Volatile::WriteBarrier()", IntrinsicPrimitive.WriteBarrier
        ]

    /// The JIT expansions on `runtime`'s pinned linux-x64 CoreLib that are not hardware
    /// intrinsics, each with the operation CoreCLR's JIT emits for it.
    let private linuxPrimitives (runtime : EmulatedRuntime) : (string * IntrinsicPrimitive) list =
        match runtime with
        | EmulatedRuntime.Net10 -> linuxPrimitivesEverywhere
        | EmulatedRuntime.Net11 ->
            linuxPrimitivesEverywhere
            @ [
                "System.Threading.Interlocked::CompareExchange(ptr[int32], int32, int32)",
                IntrinsicPrimitive.AtomicCompareExchange (AtomicOperand.Int32, AtomicLocation.Pointer)
            ]

    [<Test>]
    let ``the linux-x64 CoreLib's primitive JIT expansions are the JIT's own operations`` () : unit =
        let corelib = coreLib AuditedCoreLib.PinnedLinuxX64

        let found =
            expansionsOf corelib
            |> List.choose (fun (m, e) ->
                match e with
                | JitExpansion.Primitive p -> Some (describe m, p)
                | _ -> None
            )
            |> Set.ofList

        let expected = Set.ofList (linuxPrimitives (FrameworkUnderTest.runtime ()))
        let unexpected = Set.difference found expected
        let missing = Set.difference expected found

        if not unexpected.IsEmpty || not missing.IsEmpty then
            let show (entries : Set<string * IntrinsicPrimitive>) =
                entries |> Seq.map (fun (m, p) -> $"  %s{m} as %A{p}") |> String.concat "\n"

            failwith $"primitives not on the list:\n%s{show unexpected}\nlisted but not found:\n%s{show missing}"

    [<TestCaseSource(nameof coreLibs)>]
    let ``every JIT expansion in CoreLib is recognised`` (which : AuditedCoreLib) : unit =
        let corelib = coreLib which

        let unrecognised =
            expansionsOf corelib
            |> List.filter (fun (_, e) -> e = JitExpansion.Unrecognised)
            |> List.map (fst >> describe)

        if not unrecognised.IsEmpty then
            failwith (String.concat "\n" unrecognised)

    /// Whether a parameter or return type can hold a null: a reference, a byref or a pointer.
    let rec private canBeNull (ty : TypeDefn) : bool =
        match ty with
        | TypeDefn.Modified m -> canBeNull m.Unmodified
        | TypeDefn.PrimitiveType PrimitiveType.Object
        | TypeDefn.PrimitiveType PrimitiveType.String
        | TypeDefn.Byref _
        | TypeDefn.Pointer _
        | TypeDefn.OneDimensionalArrayLowerBoundZero _
        | TypeDefn.Array _ -> true
        | _ -> false

    [<TestCaseSource(nameof coreLibs)>]
    let ``every primitive's contract fits the signature of each method that performs it``
        (which : AuditedCoreLib)
        : unit
        =
        let corelib = coreLib which

        let primitives =
            methodsOf corelib
            |> List.choose (fun m -> IntrinsicPrimitive.recognise corelib m.Handle |> Option.map (fun p -> m, p))

        primitives |> List.length |> shouldBeGreaterThan 25

        let failures =
            [
                for m, p in primitives do
                    let definition = corelib.Methods.[m.Handle]
                    let parameters = definition.Signature.ParameterTypes
                    let contract = IntrinsicPrimitive.contract p

                    for _, condition in contract.Raises do
                        match condition with
                        | FaultCondition.ArgumentNull i ->
                            if i >= parameters.Length || not (canBeNull parameters.[i]) then
                                yield $"%s{describe m}: %A{p} faults on a null argument %d{i}, which cannot be null"
                        | FaultCondition.ArgumentMisaligned i ->
                            if i >= parameters.Length || not (canBeNull parameters.[i]) then
                                yield
                                    $"%s{describe m}: %A{p} faults on a misaligned argument %d{i}, which addresses nothing"

                    match contract.Result, definition.Signature.ReturnType with
                    | ResultNullness.NotAReference, MethodReturnType.Returns ty when canBeNull ty ->
                        yield $"%s{describe m}: %A{p} returns %O{ty}, but its contract says no reference"
                    | (ResultNullness.NonNull | ResultNullness.MaybeNull), MethodReturnType.Returns ty when
                        not (canBeNull ty)
                        ->
                        yield $"%s{describe m}: %A{p} returns %O{ty}, but its contract describes a reference"
                    | (ResultNullness.NonNull | ResultNullness.MaybeNull), MethodReturnType.Void ->
                        yield $"%s{describe m}: %A{p} returns nothing, but its contract describes a reference"
                    | _ -> ()
            ]

        if not failures.IsEmpty then
            failwith (String.concat "\n" failures)

    // -- Fabricated images: each route by which a body can name itself, and what does not count --

    let private fabricatedName = "IntrinsicBodies"

    let private staticMethod : MethodAttributes =
        MethodAttributes.Public
        ||| MethodAttributes.Static
        ||| MethodAttributes.HideBySig

    let private fabricate () : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName fabricatedName, typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule fabricatedName

        // CoreLib's own `internal sealed class IntrinsicAttribute`, referenced through a MemberRef.
        let intrinsic =
            let ty =
                typeof<obj>.Assembly.GetType ("System.Runtime.CompilerServices.IntrinsicAttribute", true)

            let ctor =
                ty.GetConstructor (
                    BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic,
                    Type.EmptyTypes
                )

            CustomAttributeBuilder (ctor, Array.empty)

        let methods =
            modul.DefineType ("Methods", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        // Ordinary IL: `ldc.i4.s 42; ret`.
        let ordinary =
            methods.DefineMethod ("Ordinary", staticMethod, typeof<int>, Type.EmptyTypes)

        ordinary.SetCustomAttribute intrinsic
        let il = ordinary.GetILGenerator ()
        il.Emit (OpCodes.Ldc_I4_S, 42y)
        il.Emit OpCodes.Ret

        // Names itself by MethodDef.
        let selfDirect =
            methods.DefineMethod ("SelfDirect", staticMethod, typeof<int>, Type.EmptyTypes)

        selfDirect.SetCustomAttribute intrinsic
        let il = selfDirect.GetILGenerator ()
        il.Emit (OpCodes.Call, selfDirect)
        il.Emit OpCodes.Ret

        // Names itself by MethodSpec.
        let selfGeneric =
            methods.DefineMethod ("SelfGeneric", staticMethod, typeof<int>, Type.EmptyTypes)

        let t = selfGeneric.DefineGenericParameters [| "T" |]
        selfGeneric.SetCustomAttribute intrinsic
        let il = selfGeneric.GetILGenerator ()
        il.Emit (OpCodes.Call, selfGeneric.MakeGenericMethod [| t.[0] :> Type |])
        il.Emit OpCodes.Ret

        // Calls a same-named overload, which is not itself.
        let overloadTarget =
            methods.DefineMethod ("CallsOverload", staticMethod, typeof<int>, [| typeof<int64> |])

        let il = overloadTarget.GetILGenerator ()
        il.Emit (OpCodes.Ldc_I4_0)
        il.Emit OpCodes.Ret

        let callsOverload =
            methods.DefineMethod ("CallsOverload", staticMethod, typeof<int>, [| typeof<int> |])

        callsOverload.SetCustomAttribute intrinsic
        let il = callsOverload.GetILGenerator ()
        il.Emit OpCodes.Ldarg_0
        il.Emit OpCodes.Conv_I8
        il.Emit (OpCodes.Call, overloadTarget)
        il.Emit OpCodes.Ret

        // Cannot return, but is not one of CoreLib's VM-substituted classes.
        let alwaysThrows =
            methods.DefineMethod ("AlwaysThrows", staticMethod, typeof<int>, Type.EmptyTypes)

        alwaysThrows.SetCustomAttribute intrinsic
        let il = alwaysThrows.GetILGenerator ()
        il.Emit (OpCodes.Newobj, typeof<InvalidOperationException>.GetConstructor Type.EmptyTypes)
        il.Emit OpCodes.Throw

        let notIntrinsic =
            methods.DefineMethod ("NotIntrinsic", staticMethod, typeof<int>, Type.EmptyTypes)

        let il = notIntrinsic.GetILGenerator ()
        il.Emit OpCodes.Ldc_I4_0
        il.Emit OpCodes.Ret

        methods.CreateType () |> ignore<Type>

        // A virtual method that calls itself: CoreCLR's importer exempts a virtual callee.
        let virtuals =
            modul.DefineType ("Virtuals", TypeAttributes.Public ||| TypeAttributes.Class)

        virtuals.DefineDefaultConstructor MethodAttributes.Public
        |> ignore<ConstructorBuilder>

        let selfVirtual =
            virtuals.DefineMethod (
                "SelfVirtual",
                MethodAttributes.Public
                ||| MethodAttributes.Virtual
                ||| MethodAttributes.HideBySig,
                typeof<int>,
                Type.EmptyTypes
            )

        selfVirtual.SetCustomAttribute intrinsic
        let il = selfVirtual.GetILGenerator ()
        il.Emit OpCodes.Ldarg_0
        il.Emit (OpCodes.Call, selfVirtual)
        il.Emit OpCodes.Ret
        virtuals.CreateType () |> ignore<Type>

        // A generic type whose method names itself through a MemberRef on `Generic<!0>`.
        let generic =
            modul.DefineType ("Generic", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let genericParameters = generic.DefineGenericParameters [| "T" |]

        let selfOnGeneric =
            generic.DefineMethod ("SelfOnGeneric", staticMethod, typeof<int>, Type.EmptyTypes)

        selfOnGeneric.SetCustomAttribute intrinsic
        let il = selfOnGeneric.GetILGenerator ()

        let instantiated = generic.MakeGenericType [| genericParameters.[0] :> Type |]

        il.Emit (OpCodes.Call, TypeBuilder.GetMethod (instantiated, selfOnGeneric))
        il.Emit OpCodes.Ret
        generic.CreateType () |> ignore<Type>

        // A type-level `[Intrinsic]`, with one ordinary member and one placeholder.
        let intrinsicType =
            modul.DefineType (
                "IntrinsicType",
                TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed
            )

        intrinsicType.SetCustomAttribute intrinsic

        let plain =
            intrinsicType.DefineMethod ("Plain", staticMethod, typeof<int>, Type.EmptyTypes)

        let il = plain.GetILGenerator ()
        il.Emit (OpCodes.Ldc_I4_7)
        il.Emit OpCodes.Ret

        let placeholder =
            intrinsicType.DefineMethod ("Placeholder", staticMethod, typeof<int>, Type.EmptyTypes)

        let il = placeholder.GetILGenerator ()
        il.Emit (OpCodes.Call, placeholder)
        il.Emit OpCodes.Ret
        intrinsicType.CreateType () |> ignore<Type>

        // CoreLib's name for a VM-substituting class, declared outside CoreLib.
        let impostor =
            modul.DefineType (
                "System.Runtime.CompilerServices.Unsafe",
                TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed
            )

        let impostorThrows =
            impostor.DefineMethod ("As", staticMethod, typeof<obj>, [| typeof<obj> |])

        impostorThrows.SetCustomAttribute intrinsic
        let il = impostorThrows.GetILGenerator ()
        il.Emit (OpCodes.Newobj, typeof<PlatformNotSupportedException>.GetConstructor Type.EmptyTypes)
        il.Emit OpCodes.Throw
        impostor.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    let private fabricated : Lazy<byte[]> = lazy (fabricate ())

    let private readFabricated () : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = new MemoryStream (fabricated.Force ())
        Assembly.read loggerFactory None stream

    let fabricatedCases : TestCaseData list =
        [
            TestCaseData ("Methods", "Ordinary", ([] : string list), IntrinsicBody.OwnIl)
            TestCaseData (
                "Methods",
                "SelfDirect",
                ([] : string list),
                IntrinsicBody.JitExpansion JitExpansion.Unrecognised
            )
            TestCaseData (
                "Methods",
                "SelfGeneric",
                ([] : string list),
                IntrinsicBody.JitExpansion JitExpansion.Unrecognised
            )
            TestCaseData ("Methods", "CallsOverload", [ "int32" ], IntrinsicBody.OwnIl)
            TestCaseData ("Methods", "AlwaysThrows", ([] : string list), IntrinsicBody.OwnIl)
            TestCaseData ("Virtuals", "SelfVirtual", ([] : string list), IntrinsicBody.OwnIl)
            TestCaseData (
                "Generic",
                "SelfOnGeneric",
                ([] : string list),
                IntrinsicBody.JitExpansion JitExpansion.Unrecognised
            )
            TestCaseData ("IntrinsicType", "Plain", ([] : string list), IntrinsicBody.OwnIl)
            TestCaseData (
                "IntrinsicType",
                "Placeholder",
                ([] : string list),
                IntrinsicBody.JitExpansion JitExpansion.Unrecognised
            )
            TestCaseData ("Unsafe", "As", [ "object" ], IntrinsicBody.OwnIl)
        ]
        |> List.map (fun c -> c.SetArgDisplayNames $"%O{c.Arguments.[0]}::%O{c.Arguments.[1]} is %O{c.Arguments.[3]}")

    [<TestCaseSource(nameof fabricatedCases)>]
    let ``fabricated intrinsics classify by how their bodies name themselves``
        (cls : string)
        (name : string)
        (parameters : string list)
        (expected : IntrinsicBody)
        : unit
        =
        let assembly = readFabricated ()

        let m =
            methodsOf assembly
            |> List.filter (fun m -> m.Class = cls && m.Name = name && m.Parameters = parameters)
            |> List.exactlyOne

        IntrinsicBody.isIntrinsic assembly m.Handle |> shouldEqual true
        IntrinsicBody.classify assembly m.Handle |> shouldEqual expected

    [<Test>]
    let ``a method with no Intrinsic attribute on it or its type is not an intrinsic`` () : unit =
        let assembly = readFabricated ()

        let m =
            methodsOf assembly
            |> List.filter (fun m -> m.Class = "Methods" && m.Name = "NotIntrinsic")
            |> List.exactlyOne

        IntrinsicBody.isIntrinsic assembly m.Handle |> shouldEqual false

    // -- A body naming itself through a TypeRef back to its own assembly --

    let private selfByTypeRefName = "SelfByTypeRef"

    /// An image whose `[Intrinsic]` `Methods::Self` calls `target`. Building it once with no target,
    /// loading that build, and building again with the loaded `Self` as the target makes the call
    /// a MemberRef whose parent is a TypeRef scoped to an AssemblyRef naming this very assembly.
    let private fabricateSelfByTypeRef (target : System.Reflection.MethodInfo option) : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName selfByTypeRefName, typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule selfByTypeRefName

        let intrinsic =
            let ty =
                typeof<obj>.Assembly.GetType ("System.Runtime.CompilerServices.IntrinsicAttribute", true)

            let ctor =
                ty.GetConstructor (
                    BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic,
                    Type.EmptyTypes
                )

            CustomAttributeBuilder (ctor, Array.empty)

        let methods =
            modul.DefineType ("Methods", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let self = methods.DefineMethod ("Self", staticMethod, typeof<int>, Type.EmptyTypes)
        self.SetCustomAttribute intrinsic
        let il = self.GetILGenerator ()

        match target with
        | None -> il.Emit OpCodes.Ldc_I4_0
        | Some target -> il.Emit (OpCodes.Call, target)

        il.Emit OpCodes.Ret
        methods.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    let private selfByTypeRef : Lazy<byte[]> =
        lazy
            (let first =
                System.Runtime.Loader
                    .AssemblyLoadContext(selfByTypeRefName, true)
                    .LoadFromStream (new MemoryStream (fabricateSelfByTypeRef None))

             fabricateSelfByTypeRef (Some (first.GetType("Methods").GetMethod "Self")))

    [<Test>]
    let ``a body naming itself through a TypeRef to its own assembly is a JIT expansion`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = new MemoryStream (selfByTypeRef.Force ())
        let assembly = Assembly.read loggerFactory None stream

        // The shape under test: the call's parent is a TypeRef, not the TypeDef.
        assembly.Members.Values
        |> Seq.exists (fun m ->
            match m.Parent with
            | MetadataToken.TypeReference _ -> m.PrettyName = "Self"
            | _ -> false
        )
        |> shouldEqual true

        let m =
            methodsOf assembly
            |> List.filter (fun m -> m.Class = "Methods" && m.Name = "Self")
            |> List.exactlyOne

        IntrinsicBody.classify assembly m.Handle
        |> shouldEqual (IntrinsicBody.JitExpansion JitExpansion.Unrecognised)

    [<Test>]
    let ``a placeholder naming itself through a TypeRef is refused rather than recursed into`` () : unit =
        let driver =
            """
public static class Driver
{
    public static int Main() => Methods.Self();
}
"""

        match
            FabricatedGuest.runOnPawPrintBounded
                selfByTypeRefName
                (selfByTypeRef.Force ())
                "SelfByTypeRefDriver"
                driver
                50_000L
        with
        | FabricatedOutcome.Exited code -> failwith $"expected PawPrint to refuse Self, but the guest exited %d{code}"
        | FabricatedOutcome.Failed e ->
            let rec innermost (e : exn) : exn =
                match e.InnerException with
                | null -> e
                | inner -> innermost inner

            (innermost e).Message |> shouldContainText "calls itself"

    // -- End to end: what the interpreter does with each classification --

    let rec private innermost (e : exn) : exn =
        match e.InnerException with
        | null -> e
        | inner -> innermost inner

    [<Test>]
    let ``an Intrinsic method whose IL is its semantics runs that IL`` () : unit =
        let driver =
            """
public static class Driver
{
    public static int Main() => Methods.Ordinary() == 42 ? 0 : 1;
}
"""

        FabricatedGuest.run fabricatedName (fabricated.Force ()) "OrdinaryIntrinsicDriver" driver 0

    [<Test>]
    let ``an ordinary member of a type-level Intrinsic type runs its IL`` () : unit =
        let driver =
            """
public static class Driver
{
    public static int Main() => IntrinsicType.Plain() == 7 ? 0 : 1;
}
"""

        FabricatedGuest.run fabricatedName (fabricated.Force ()) "TypeLevelIntrinsicDriver" driver 0

    /// The real runtime honours `[Intrinsic]` only in CoreLib, so on it these guests simply
    /// recurse; only PawPrint is run. The step bound is what a regression would hit instead of the
    /// refusal: a guest this small starts up and exits within about 5,000 steps.
    let private assertRefused (driverName : string) (driver : string) (methodName : string) : unit =
        match FabricatedGuest.runOnPawPrintBounded fabricatedName (fabricated.Force ()) driverName driver 50_000L with
        | FabricatedOutcome.Exited code ->
            failwith $"expected PawPrint to refuse %s{methodName}, but the guest exited %d{code}"
        | FabricatedOutcome.Failed e ->
            let message = (innermost e).Message
            message |> shouldContainText "TODO: implement JIT intrinsic"
            message |> shouldContainText methodName
            message |> shouldContainText "calls itself"

    [<Test>]
    let ``a self-calling Intrinsic placeholder is refused rather than recursed into`` () : unit =
        let driver =
            """
public static class Driver
{
    public static int Main() => Methods.SelfDirect();
}
"""

        assertRefused "SelfCallingIntrinsicDriver" driver "SelfDirect"

    [<Test>]
    let ``a self-calling placeholder on a type-level Intrinsic type is refused when reached through a delegate``
        ()
        : unit
        =
        let driver =
            """
public static class Driver
{
    public static int Main()
    {
        System.Func<int> placeholder = IntrinsicType.Placeholder;
        return placeholder();
    }
}
"""

        assertRefused "DelegateToPlaceholderDriver" driver "Placeholder"

    /// `Unsafe.IsAddressGreaterThan<T>` has no implementation in `Intrinsics.call`, and its CoreLib
    /// body is `throw new PlatformNotSupportedException()`: real .NET substitutes
    /// `ldarg.0; ldarg.1; cgt.un; ret`, and so must PawPrint, on the `calli` route as on any
    /// other. Running the placeholder would hand the guest a catchable exception real .NET never
    /// raises. Array elements are ordered by index, so real .NET returns 1.
    let private callsSubstitutedIntrinsicThroughCalli =
        """
using System;
using System.Runtime.CompilerServices;

unsafe class CallsSubstitutedIntrinsicThroughCalli
{
    static int Main()
    {
        int[] a = new int[2];
        delegate*<in int, in int, bool> greater = &Unsafe.IsAddressGreaterThan<int>;
        try
        {
            return greater(in a[1], in a[0]) ? 1 : 2;
        }
        catch (PlatformNotSupportedException)
        {
            return 3;
        }
    }
}
"""

    [<Test>]
    let ``a VM-substituted intrinsic reached by calli runs the VM's stub`` () : unit =
        let name = "CallsSubstitutedIntrinsicThroughCalli.cs"
        let image = Roslyn.compileWithSymbols [ callsSubstitutedIntrinsicThroughCalli ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        use _loggerFactoryResource = loggerFactory
        let dotnetRuntimes = FrameworkUnderTest.runtimeDirs ()
        use peImage = new MemoryStream (image)

        match
            BoundedRun.runWith
                loggerFactory
                BoundedRun.defaultMaxSteps
                name
                (Some name)
                peImage
                (HostConfig.Default dotnetRuntimes)
            |> ExpectRun.ended
        with
        | RunOutcome.NormalExit (state, _, _) -> state.LatchedExitCode |> shouldEqual 1
        | other -> failwith $"expected the guest to exit normally, got %O{other}"

    /// The capability-query getters of `corelib`, each with whether CoreCLR's JIT compiles a call
    /// of it to a constant: an `IsSupported` or `IsHardwareAccelerated` getter, static and
    /// parameterless, returning `bool`, on a class that is not generic in a hardware-intrinsic
    /// namespace. The JIT answers one (`lookupNamedIntrinsic`, importercalls.cpp) only when the VM
    /// marks it intrinsic, which it does when the getter carries `[Intrinsic]` itself, or when its
    /// class does and is a hardware-intrinsic class of the architecture the CoreLib was built for
    /// (`fIsHardwareIntrinsic`, methodtablebuilder.cpp). Decided from names and attributes, never
    /// from IL.
    let private capabilityGetters (corelib : DumpedAssembly) : (Method * bool) list =
        let parentOf (handle : System.Reflection.Metadata.MemberReferenceHandle) : TypeRef =
            match corelib.Members.[handle].Parent with
            | MetadataToken.TypeReference r -> corelib.TypeRefs.[r]
            | other -> failwith $"an attribute constructor's parent %O{other} is not a TypeRef"

        // A CoreLib is built for one architecture, whose base instruction set's query calls itself.
        let ownNamespace =
            [
                "System.Runtime.Intrinsics.X86", "X86Base"
                "System.Runtime.Intrinsics.Arm", "ArmBase"
            ]
            |> List.filter (fun (ns, cls) ->
                match IntrinsicBody.classify corelib (find corelib ns cls "get_IsSupported" []) with
                | IntrinsicBody.JitExpansion _ -> true
                | _ -> false
            )
            |> List.exactlyOne
            |> fst

        let isHardwareIntrinsicNamespace (ns : string) : bool =
            ns = "System.Numerics"
            || ns = "System.Runtime.Intrinsics"
            || ns.StartsWith ("System.Runtime.Intrinsics.", StringComparison.Ordinal)

        let rec isGeneric (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>) : bool =
            not ty.Generics.IsEmpty
            || (ty.IsNested && isGeneric corelib.TypeDefs.[ty.DeclaringType])

        [
            for m in methodsOf corelib do
                let definition = corelib.Methods.[m.Handle]

                let declaringType =
                    corelib.TypeDefs.[definition.RequiredDeclaringType.Definition.Get]

                let intrinsicClass = classOf corelib m.Handle

                if
                    (m.Name = "get_IsSupported" || m.Name = "get_IsHardwareAccelerated")
                    && definition.IsStatic
                    && m.Parameters.IsEmpty
                    && m.GenericArity = 0
                    && definition.Signature.ReturnType = MethodReturnType.Returns (
                        TypeDefn.PrimitiveType PrimitiveType.Boolean
                    )
                    && not (isGeneric declaringType)
                    && isHardwareIntrinsicNamespace intrinsicClass.Namespace
                then
                    let markedIntrinsic =
                        MethodInfo.isJITIntrinsic parentOf corelib.Methods definition
                        || (intrinsicClass.Namespace = ownNamespace
                            && MethodInfo.hasIntrinsicAttribute parentOf corelib.Methods declaringType.Attributes)

                    yield m, markedIntrinsic
        ]

    [<TestCaseSource(nameof runtimes)>]
    let ``every capability query the JIT answers has a constant result on a scalar-only CPU, on every runtime``
        (runtime : EmulatedRuntime)
        : unit
        =
        // Each runtime's CoreLib for the host's architecture and for linux-x64, so that both
        // architectures' classes are read as their own and as another architecture's.
        let paths =
            [
                FrameworkUnderTest.requireCoreLibPathOf runtime
                Path.Combine (FrameworkUnderTest.requireLinuxFrameworkDirectoryOf runtime, "System.Private.CoreLib.dll")
            ]

        let failures =
            [
                for path in paths do
                    let corelib = readCoreLib path
                    let getters = capabilityGetters corelib

                    // About 100 in each CoreLib at the pinned runtimes.
                    if (getters |> List.filter snd |> List.length) < 90 then
                        yield $"%s{path}: only %d{getters.Length} capability queries"

                    for m, answered in getters do
                        let expected = if answered then Some false else None

                        let actual =
                            IntrinsicBody.constantResult HardwareIntrinsicsProfile.ScalarOnly corelib m.Handle

                        if actual <> expected then
                            yield
                                $"%s{path}: %O{classOf corelib m.Handle}::%s{m.Name}: %A{actual}, expected %A{expected}"
            ]

        if not failures.IsEmpty then
            failures |> List.truncate 40 |> String.concat "\n" |> failwith

    /// Every public capability query of the test host's CoreLib, by reflection: a static `bool`
    /// property `IsSupported` or `IsHardwareAccelerated` that a public class which is not generic
    /// declares, in a hardware-intrinsic namespace; with its class, as `IntrinsicClass` spells it.
    let private hostQueries () : (PropertyInfo * IntrinsicClass) list =
        let rec path (ty : Type) : string list =
            match ty.DeclaringType with
            | null -> [ ty.Name ]
            | outer -> path outer @ [ ty.Name ]

        [
            for ty in typeof<obj>.Assembly.GetExportedTypes () do
                let ns =
                    match ty.Namespace with
                    | null -> ""
                    | ns -> ns

                if
                    (ns = "System.Numerics"
                     || ns = "System.Runtime.Intrinsics"
                     || ns.StartsWith ("System.Runtime.Intrinsics.", StringComparison.Ordinal))
                    && not ty.IsGenericType
                then
                    for name in [ "IsSupported" ; "IsHardwareAccelerated" ] do
                        match
                            ty.GetProperty (
                                name,
                                BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly
                            )
                        with
                        | null -> ()
                        | property when property.PropertyType = typeof<bool> ->
                            yield
                                property,
                                {
                                    Namespace = ns
                                    Path = path ty
                                }
                        | _ -> ()
        ]

    [<Test>]
    let ``the host's JIT compiles a call of each public capability query to its constant result`` () : unit =
        // The probe is a call with a variable argument list, which CoreCLR refuses with
        // InvalidProgramException where it compiles one, except on Windows.
        if OperatingSystem.IsWindows () then
            Assert.Ignore "CoreCLR compiles a vararg call on Windows, so it cannot tell a folded branch here."

        let queries = hostQueries ()

        // C# that names each query's class, and the diagnostics its attributes raise there.
        let rec enclosing (ty : Type) : Type list =
            match ty.DeclaringType with
            | null -> [ ty ]
            | outer -> ty :: enclosing outer

        let suppressed =
            [
                for property, _ in queries do
                    for ty in enclosing property.DeclaringType do
                        for attribute in ty.GetCustomAttributes<Diagnostics.CodeAnalysis.ExperimentalAttribute> () do
                            yield attribute.DiagnosticId

                        for attribute in ty.GetCustomAttributes<ObsoleteAttribute> () do
                            yield "CS0618"

                            match attribute.DiagnosticId with
                            | null -> ()
                            | id -> yield id
            ]
            |> List.distinct

        let source =
            let probes =
                queries
                |> List.mapi (fun i (property, c) ->
                    let name = String.concat "." (c.Namespace :: c.Path)

                    $"    public static void Probe%d{i}() {{ if (global::%s{name}.%s{property.Name}) Variadic(1, __arglist(2)); }}"
                )
                |> String.concat "\n"

            let pragma =
                if suppressed.IsEmpty then
                    ""
                else
                    let ids = String.concat ", " suppressed
                    $"#pragma warning disable %s{ids}"

            $"""
%s{pragma}
namespace HostQueries;

public static class Probes
{{
    public static void Variadic(int a, __arglist) {{ }}
%s{probes}
}}
"""

        let image =
            Roslyn.compileOptimizedAssembly
                "HostQueries"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                [ source ]

        // Whether calling each probe raised InvalidProgramException: tier 0 first, and then fully
        // optimised, as CoreCLR compiles code in a collectible context.
        let raised (collectible : bool) : bool list =
            let context =
                Runtime.Loader.AssemblyLoadContext ($"HostQueries%b{collectible}", isCollectible = collectible)

            try
                let probes =
                    context.LoadFromStream(new MemoryStream (image)).GetType ("HostQueries.Probes", true)

                queries
                |> List.mapi (fun i _ ->
                    try
                        probes.GetMethod($"Probe%d{i}").Invoke ((null : obj), [||]) |> ignore<obj>
                        false
                    with :? TargetInvocationException as e when (e.InnerException :? InvalidProgramException) ->
                        true
                )
            finally
                if collectible then
                    context.Unload ()

        let tier0 = raised false
        let optimised = raised true

        let answers =
            queries
            |> List.map (fun (property, c) -> property, c, property.GetValue (null : obj) :?> bool)

        let profile =
            {
                IsSupported =
                    answers
                    |> List.filter (fun (p, _, answer) -> answer && p.Name = "IsSupported")
                    |> List.map (fun (_, c, _) -> c)
                    |> Set.ofList
                IsHardwareAccelerated =
                    answers
                    |> List.filter (fun (p, _, answer) -> answer && p.Name = "IsHardwareAccelerated")
                    |> List.map (fun (_, c, _) -> c)
                    |> Set.ofList
            }

        let corelib = readCoreLib HostImage.coreLibPath

        let outcomes =
            List.zip3 answers tier0 optimised
            |> List.map (fun ((property, c, answer), raisedAtTier0, raisedOptimised) ->
                let handle =
                    match
                        methodsOf corelib
                        |> List.filter (fun m ->
                            m.Name = $"get_%s{property.Name}"
                            && m.Parameters.IsEmpty
                            && classOf corelib m.Handle = c
                        )
                    with
                    | [ m ] -> m.Handle
                    | found -> failwith $"expected one %O{c}::get_%s{property.Name}, found %d{found.Length}"

                // A branch the JIT folds away compiles no call in it; one it compiles, it imports.
                let expected =
                    match raisedAtTier0, raisedOptimised, answer with
                    | false, false, false -> Ok (Some false)
                    | true, true, true -> Ok (Some true)
                    | true, true, false -> Ok None
                    | _ ->
                        Error
                            $"%O{c}.%s{property.Name} answers %b{answer}; the call compiled at tier 0 %b{raisedAtTier0}, optimised %b{raisedOptimised}"

                c, property.Name, expected, IntrinsicBody.constantResult profile corelib handle
            )

        let failures =
            [
                for c, name, expected, actual in outcomes do
                    match expected with
                    | Error message -> yield message
                    | Ok expected when expected <> actual -> yield $"%O{c}.%s{name}: %A{actual}, expected %A{expected}"
                    | Ok _ -> ()
            ]

        if not failures.IsEmpty then
            failures |> List.truncate 40 |> String.concat "\n" |> failwith

        // Vacuity floors that hold whatever the CPU: about 100 public queries, of which those of the
        // other architecture's classes (some 20 Arm ones, or some 70 X86 ones) fold to false.
        outcomes |> List.length |> shouldBeGreaterThan 80

        outcomes
        |> List.filter (fun (_, _, expected, _) -> expected = Ok (Some false))
        |> List.length
        |> shouldBeGreaterThan 15

    [<TestCaseSource(nameof coreLibs)>]
    let ``every capability query returns the constant its profile gives, and nothing else does``
        (which : AuditedCoreLib)
        : unit
        =
        let corelib = coreLib which
        let expansions = expansionsOf corelib

        let answeringTrue =
            {
                IsSupported =
                    expansions
                    |> List.choose (fun (_, e) ->
                        match e with
                        | JitExpansion.IsSupportedQuery c -> Some c
                        | _ -> None
                    )
                    |> Set.ofList
                IsHardwareAccelerated =
                    expansions
                    |> List.choose (fun (_, e) ->
                        match e with
                        | JitExpansion.IsHardwareAcceleratedQuery c -> Some c
                        | _ -> None
                    )
                    |> Set.ofList
            }

        let queries =
            expansions
            |> List.choose (fun (m, e) ->
                match e with
                | JitExpansion.IsSupportedQuery _
                | JitExpansion.IsHardwareAcceleratedQuery _ -> Some m.Handle
                | _ -> None
            )
            |> System.Collections.Generic.HashSet

        // Another architecture's classes, whose queries CoreLib makes stubs returning false, and the
        // JIT answers false.
        let stubs =
            capabilityGetters corelib
            |> List.filter (fun (m, answered) -> answered && not (queries.Contains m.Handle))
            |> List.map (fun (m, _) -> m.Handle)
            |> System.Collections.Generic.HashSet

        // A CPU that claims every stub's class as well, which no CPU of this CoreLib's architecture
        // can: the stub returns false whatever the profile says.
        let claimingStubs =
            let classes =
                stubs |> Seq.map (fun handle -> handle, classOf corelib handle) |> List.ofSeq

            let named (getter : string) =
                classes
                |> List.filter (fun (handle, _) -> corelib.Methods.[handle].Name = getter)
                |> List.map snd
                |> Set.ofList

            {
                IsSupported = Set.union answeringTrue.IsSupported (named "get_IsSupported")
                IsHardwareAccelerated =
                    Set.union answeringTrue.IsHardwareAccelerated (named "get_IsHardwareAccelerated")
            }

        let failures =
            [
                for m in methodsOf corelib do
                    for profile, answer, stubAnswer in
                        [
                            HardwareIntrinsicsProfile.ScalarOnly, false, Some false
                            answeringTrue, true, Some false
                            claimingStubs, true, None
                        ] do
                        let expected =
                            if queries.Contains m.Handle then Some answer
                            elif stubs.Contains m.Handle then stubAnswer
                            else None

                        let actual = IntrinsicBody.constantResult profile corelib m.Handle

                        if actual <> expected then
                            yield $"%s{describe m}: %A{actual}, expected %A{expected}"
            ]

        if not failures.IsEmpty then
            failures |> List.truncate 40 |> String.concat "\n" |> failwith

        queries.Count |> shouldBeGreaterThan 20
        stubs.Count |> shouldBeGreaterThan 15

    /// An image that calls itself System.Private.CoreLib, so that its getters in a hardware-intrinsic
    /// namespace are capability queries, each of whose IL does something different with its call to
    /// itself.
    let private fabricateQueries () : byte[] =
        let name = "System.Private.CoreLib"
        let builder = PersistedAssemblyBuilder (AssemblyName name, typeof<obj>.Assembly)
        let modul = builder.DefineDynamicModule name

        let intrinsic =
            let ty =
                typeof<obj>.Assembly.GetType ("System.Runtime.CompilerServices.IntrinsicAttribute", true)

            let ctor =
                ty.GetConstructor (
                    BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic,
                    Type.EmptyTypes
                )

            CustomAttributeBuilder (ctor, Array.empty)

        let query (className : string) (emit : ILGenerator -> MethodBuilder -> unit) =
            let ty =
                modul.DefineType (
                    $"System.Runtime.Intrinsics.Arm.%s{className}",
                    TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed
                )

            let getter =
                ty.DefineMethod (
                    "get_IsSupported",
                    staticMethod ||| MethodAttributes.SpecialName,
                    typeof<bool>,
                    Type.EmptyTypes
                )

            getter.SetCustomAttribute intrinsic
            emit (getter.GetILGenerator ()) getter
            ty.CreateType () |> ignore<Type>

        query
            "Returned"
            (fun il self ->
                il.Emit (OpCodes.Call, self)
                il.Emit OpCodes.Ret
            )

        query
            "Negated"
            (fun il self ->
                il.Emit (OpCodes.Call, self)
                il.Emit OpCodes.Ldc_I4_0
                il.Emit OpCodes.Ceq
                il.Emit OpCodes.Ret
            )

        query
            "Discarded"
            (fun il self ->
                il.Emit (OpCodes.Call, self)
                il.Emit OpCodes.Pop
                il.Emit OpCodes.Ldc_I4_1
                il.Emit OpCodes.Ret
            )

        // Getters that do not call themselves, as another architecture's queries do not. Each
        // carries `[Intrinsic]` itself, or else only its class does.
        let stub (className : string) (getterMarked : bool) (emit : ILGenerator -> unit) =
            let ty =
                modul.DefineType (
                    $"System.Runtime.Intrinsics.Wasm.%s{className}",
                    TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed
                )

            let getter =
                ty.DefineMethod (
                    "get_IsSupported",
                    staticMethod ||| MethodAttributes.SpecialName,
                    typeof<bool>,
                    Type.EmptyTypes
                )

            if getterMarked then
                getter.SetCustomAttribute intrinsic
            else
                ty.SetCustomAttribute intrinsic

            emit (getter.GetILGenerator ())
            ty.CreateType () |> ignore<Type>

        stub
            "Stub"
            true
            (fun il ->
                il.Emit OpCodes.Ldc_I4_0
                il.Emit OpCodes.Ret
            )

        stub
            "TrueStub"
            true
            (fun il ->
                il.Emit OpCodes.Ldc_I4_1
                il.Emit OpCodes.Ret
            )

        stub
            "Computed"
            true
            (fun il ->
                il.Emit OpCodes.Ldc_I4_1
                il.Emit OpCodes.Ldc_I4_1
                il.Emit OpCodes.Sub
                il.Emit OpCodes.Ret
            )

        stub
            "ClassMarked"
            false
            (fun il ->
                il.Emit OpCodes.Ldc_I4_0
                il.Emit OpCodes.Ret
            )

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    [<Test>]
    let ``a query that does not call itself answers a constant only as a marked stub returning false`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = new MemoryStream (fabricateQueries ())
        let image = Assembly.read loggerFactory None stream

        let answers =
            [
                for className in [ "Stub" ; "TrueStub" ; "Computed" ; "ClassMarked" ] do
                    let handle =
                        find image "System.Runtime.Intrinsics.Wasm" className "get_IsSupported" []

                    let intrinsicClass =
                        {
                            Namespace = "System.Runtime.Intrinsics.Wasm"
                            Path = [ className ]
                        }

                    IntrinsicBody.classify image handle |> shouldEqual IntrinsicBody.OwnIl

                    let claiming =
                        { HardwareIntrinsicsProfile.ScalarOnly with
                            IsSupported = Set.singleton intrinsicClass
                        }

                    yield
                        className,
                        IntrinsicBody.constantResult HardwareIntrinsicsProfile.ScalarOnly image handle,
                        IntrinsicBody.constantResult claiming image handle
            ]

        // A stub returns false whatever the profile claims, so it answers only where the profile
        // agrees.
        answers
        |> shouldEqual
            [
                "Stub", Some false, None
                "TrueStub", None, None
                "Computed", None, None
                "ClassMarked", None, None
            ]

    [<Test>]
    let ``a capability query that does more than return its call to itself has no constant result`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = new MemoryStream (fabricateQueries ())
        let image = Assembly.read loggerFactory None stream

        let answers =
            [
                for className in [ "Returned" ; "Negated" ; "Discarded" ] do
                    let handle =
                        find image "System.Runtime.Intrinsics.Arm" className "get_IsSupported" []

                    let intrinsicClass =
                        {
                            Namespace = "System.Runtime.Intrinsics.Arm"
                            Path = [ className ]
                        }

                    // Each is a query, so only its IL decides whether it answers a constant.
                    IntrinsicBody.classify image handle
                    |> shouldEqual (IntrinsicBody.JitExpansion (JitExpansion.IsSupportedQuery intrinsicClass))

                    let supporting =
                        { HardwareIntrinsicsProfile.ScalarOnly with
                            IsSupported = Set.singleton intrinsicClass
                        }

                    yield
                        className,
                        IntrinsicBody.constantResult HardwareIntrinsicsProfile.ScalarOnly image handle,
                        IntrinsicBody.constantResult supporting image handle
            ]

        answers
        |> shouldEqual
            [
                "Returned", Some false, Some true
                "Negated", None, None
                "Discarded", None, None
            ]
