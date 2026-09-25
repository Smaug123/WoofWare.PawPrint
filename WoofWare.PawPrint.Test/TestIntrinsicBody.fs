namespace WoofWare.PawPrint.Test

open System
open System.Collections.Concurrent
open System.IO
open System.Reflection
open System.Reflection.Emit
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `IntrinsicBody` says what CoreCLR runs for an `[Intrinsic]` method: its own IL, a JIT
/// expansion (the IL calls itself), a VM-substituted body (CoreLib's IL cannot return), or no IL
/// at all. The interpreter runs `OwnIl` as it stands and refuses the two placeholders unless
/// `Intrinsics.call` implements them.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestIntrinsicBody =

    let private readCache = ConcurrentDictionary<string, Lazy<DumpedAssembly>> ()

    let private readCoreLib (path : string) : DumpedAssembly =
        let read () =
            let _, loggerFactory = LoggerFactory.makeTest ()
            Assembly.readFile loggerFactory path

        readCache.GetOrAdd(path, (fun _ -> lazy (read ()))).Value

    /// The CoreLib the suite runs on (whose flavour is the host's), and the pinned linux-x64 one,
    /// which is what CI and production run.
    let coreLibs : TestCaseData list =
        [
            TestCaseData("host").SetArgDisplayNames "host CoreLib"
            TestCaseData("linux-x64").SetArgDisplayNames "pinned linux-x64 CoreLib"
        ]

    let private coreLib (which : string) : DumpedAssembly =
        match which with
        | "host" -> readCoreLib typeof<obj>.Assembly.Location
        | "linux-x64" ->
            match Environment.GetEnvironmentVariable "DOTNET_LINUX_FRAMEWORK_DIR" with
            | null
            | "" ->
                Assert.Ignore "DOTNET_LINUX_FRAMEWORK_DIR is unset; run inside the Nix devshell"
                failwith "unreachable"
            | dir -> readCoreLib (Path.Combine (dir, "System.Private.CoreLib.dll"))
        | other -> failwith $"unknown CoreLib %s{other}"

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
    let ``named intrinsics classify as CoreCLR treats them`` (which : string) : unit =
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
            (IntrinsicBody.JitExpansion JitExpansion.Primitive)

        expect "System.Threading" "Volatile" "ReadBarrier" [] (IntrinsicBody.JitExpansion JitExpansion.Primitive)
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
    let ``exactly one architecture's base ISA query is a JIT expansion`` (which : string) : unit =
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
    let ``VmSubstitution is exactly the VM's substitutions whose IL is a placeholder`` (which : string) : unit =
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

    [<TestCaseSource(nameof coreLibs)>]
    let ``VmSubstitution has a stub for exactly the Unsafe methods corelib.h binds`` (which : string) : unit =
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
    let ``every Unsafe placeholder runs the VM's stub`` (which : string) : unit =
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

    [<Test>]
    let ``the pinned linux-x64 CoreLib has thousands of JIT expansions`` () : unit =
        // A floor, not a snapshot: if resolving a self-call through a MemberRef or MethodSpec
        // regressed, this is where the count would collapse. The census measured 3,737 at 10.0.7.
        let corelib = coreLib "linux-x64"

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
        let corelib = coreLib "linux-x64"

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
        (which : string)
        : unit
        =
        let corelib = coreLib which
        let expansions = expansionsOf corelib

        expansions
        |> List.filter (fun (_, e) -> e <> JitExpansion.Primitive)
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
                    | JitExpansion.Primitive, SelfCallExpansion.JitCode -> ()
                    | JitExpansion.IsSupportedQuery c, SelfCallExpansion.Constant false
                    | JitExpansion.IsHardwareAcceleratedQuery c, SelfCallExpansion.Constant false
                    | JitExpansion.HardwareInstruction c, SelfCallExpansion.ThrowPlatformNotSupported ->
                        yield! ownClass c
                    | _, answer -> yield $"%s{describe m}: %A{expansion} expands to %A{answer}"
            ]

        if not failures.IsEmpty then
            failwith (String.concat "\n" failures)

    [<TestCaseSource(nameof coreLibs)>]
    let ``a placeholder's self-call follows the profile`` (which : string) : unit =
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
                            if expand profile expansion <> SelfCallExpansion.JitCode then
                                yield $"%s{describe m}: not the JIT's own code on a CPU that has %O{c}"
                    | JitExpansion.Primitive -> ()
            ]

        if not failures.IsEmpty then
            failwith (String.concat "\n" failures)

    /// The JIT expansions on the pinned linux-x64 CoreLib that are not hardware intrinsics: each
    /// is an operation CoreCLR's JIT emits code for itself, so no IL can stand in for it.
    let private linuxPrimitives : string list =
        [
            "System.Double::ConvertToIntegerNative(float64)"
            "System.Double::MultiplyAddEstimate(float64, float64, float64)"
            "System.Math::ReciprocalEstimate(float64)"
            "System.Math::ReciprocalSqrtEstimate(float64)"
            "System.MathF::ReciprocalEstimate(single)"
            "System.MathF::ReciprocalSqrtEstimate(single)"
            "System.Runtime.CompilerServices.RuntimeHelpers::GetMethodTable(object)"
            "System.Runtime.CompilerServices.RuntimeHelpers::IsReferenceOrContainsReferences()"
            "System.Runtime.CompilerServices.StaticsHelpers::VolatileReadAsByref(ref nint)"
            "System.Runtime.InteropServices.MemoryMarshal::GetArrayDataReference(arr[<method param 0>])"
            "System.Single::ConvertToIntegerNative(single)"
            "System.Single::MultiplyAddEstimate(single, single, single)"
            "System.Threading.Interlocked::CompareExchange(ref int32, int32, int32)"
            "System.Threading.Interlocked::CompareExchange(ref int64, int64, int64)"
            "System.Threading.Interlocked::CompareExchange(ref uint16, uint16, uint16)"
            "System.Threading.Interlocked::CompareExchange(ref uint8, uint8, uint8)"
            "System.Threading.Interlocked::Exchange(ref int32, int32)"
            "System.Threading.Interlocked::Exchange(ref int64, int64)"
            "System.Threading.Interlocked::Exchange(ref uint16, uint16)"
            "System.Threading.Interlocked::Exchange(ref uint8, uint8)"
            "System.Threading.Interlocked::ExchangeAdd(ref int32, int32)"
            "System.Threading.Interlocked::ExchangeAdd(ref int64, int64)"
            "System.Threading.Interlocked::MemoryBarrier()"
            "System.Threading.Thread::FastPollGC()"
            "System.Threading.Volatile::ReadBarrier()"
            "System.Threading.Volatile::WriteBarrier()"
        ]

    [<Test>]
    let ``the linux-x64 CoreLib's primitive JIT expansions are the JIT's own operations`` () : unit =
        let corelib = coreLib "linux-x64"

        let found =
            expansionsOf corelib
            |> List.filter (fun (_, e) -> e = JitExpansion.Primitive)
            |> List.map (fst >> describe)
            |> Set.ofList

        let expected = Set.ofList linuxPrimitives
        let unexpected = Set.difference found expected
        let missing = Set.difference expected found

        if not unexpected.IsEmpty || not missing.IsEmpty then
            let unexpected = unexpected |> Seq.map (sprintf "  %s") |> String.concat "\n"
            let missing = missing |> Seq.map (sprintf "  %s") |> String.concat "\n"
            failwith $"primitives not on the list:\n%s{unexpected}\nlisted but not primitives:\n%s{missing}"

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
                IntrinsicBody.JitExpansion JitExpansion.Primitive
            )
            TestCaseData (
                "Methods",
                "SelfGeneric",
                ([] : string list),
                IntrinsicBody.JitExpansion JitExpansion.Primitive
            )
            TestCaseData ("Methods", "CallsOverload", [ "int32" ], IntrinsicBody.OwnIl)
            TestCaseData ("Methods", "AlwaysThrows", ([] : string list), IntrinsicBody.OwnIl)
            TestCaseData ("Virtuals", "SelfVirtual", ([] : string list), IntrinsicBody.OwnIl)
            TestCaseData (
                "Generic",
                "SelfOnGeneric",
                ([] : string list),
                IntrinsicBody.JitExpansion JitExpansion.Primitive
            )
            TestCaseData ("IntrinsicType", "Plain", ([] : string list), IntrinsicBody.OwnIl)
            TestCaseData (
                "IntrinsicType",
                "Placeholder",
                ([] : string list),
                IntrinsicBody.JitExpansion JitExpansion.Primitive
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
        |> shouldEqual (IntrinsicBody.JitExpansion JitExpansion.Primitive)

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
        with
        | RunOutcome.NormalExit (state, _) -> state.LatchedExitCode |> shouldEqual 1
        | other -> failwith $"expected the guest to exit normally, got %O{other}"
