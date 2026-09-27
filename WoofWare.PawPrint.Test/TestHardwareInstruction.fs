namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection.Metadata.Ecma335
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// `HardwareInstruction.contract` against its two authorities: the JIT's own tables in the pinned
/// runtime source, which the checked-in table must reproduce, and the real runtime, which must raise
/// nothing a contract leaves out when every instruction this CPU has is called with null,
/// misaligned and valid addresses and with every immediate value.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestHardwareInstruction =

    let private runtimeSrc : string option =
        match Environment.GetEnvironmentVariable "DOTNET_RUNTIME_SRC" with
        | null
        | "" -> None
        | dir -> Some dir

    /// The pinned runtime source only exists inside the Nix devshell, so a plain `dotnet test` in a
    /// non-Nix checkout skips rather than fails.
    let private requireRuntimeSrc () : string =
        match runtimeSrc with
        | Some dir -> dir
        | None ->
            Assert.Ignore
                "DOTNET_RUNTIME_SRC is unset; run under `nix develop` to check against pinned upstream sources."

            failwith "unreachable: Assert.Ignore did not throw"

    /// The JIT's tables, and the architecture each is compiled for.
    let private headers : (JitTarget * string) list =
        [
            JitTarget.Arm64, "hwintrinsiclistarm64.h"
            JitTarget.Arm64, "hwintrinsiclistarm64sve.h"
            JitTarget.X64, "hwintrinsiclistxarch.h"
        ]

    /// `HARDWARE_INTRINSIC(isa, name, size, numArgs, {instructions}, category, flags)`.
    let private rowPattern =
        Regex
            @"^HARDWARE_INTRINSIC\(\s*(\w+)\s*,\s*(\w+)\s*,\s*-?\d+\s*,\s*-?\d+\s*,\s*\{([^}]*)\}\s*,\s*(\w+)\s*,\s*(.*)\)\s*$"

    let private rowsOfHeader (target : JitTarget) (path : string) : HardwareIntrinsicRow list =
        File.ReadAllLines path
        |> Seq.filter (fun line -> line.StartsWith "HARDWARE_INTRINSIC(")
        |> Seq.map (fun line ->
            let m = rowPattern.Match line

            if not m.Success then
                failwith $"%s{path}: a row this does not parse: %s{line}"

            {
                Target = target
                InstructionSet = m.Groups.[1].Value
                Name = m.Groups.[2].Value
                Instructions =
                    m.Groups.[3].Value.Split ','
                    |> Seq.map (fun instruction -> instruction.Trim ())
                    |> Seq.filter (fun instruction -> instruction <> "INS_invalid")
                    |> Set.ofSeq
                Category = m.Groups.[4].Value
                Flags = m.Groups.[5].Value.Split '|' |> Seq.map (fun flag -> flag.Trim ()) |> Set.ofSeq
            }
        )
        |> List.ofSeq

    /// Also how the checked-in table is made: on a difference, the regenerated table is written out
    /// and the failure says where to copy it from.
    [<Test>]
    let ``the checked-in table is the JIT's, as the pinned runtime source states it`` () : unit =
        let jit = Path.Combine (requireRuntimeSrc (), "src", "coreclr", "jit")

        let expected =
            headers
            |> List.collect (fun (target, file) -> rowsOfHeader target (Path.Combine (jit, file)))
            |> List.distinct
            |> List.sortBy HardwareIntrinsicTable.format

        // A table in an older format fails to parse, and is regenerated like any other difference.
        let checkedIn =
            try
                Ok (HardwareIntrinsicTable.rows.Force ())
            with e ->
                Error e.Message

        if checkedIn <> Ok expected then
            let regenerated =
                Path.Combine (TestContext.CurrentContext.WorkDirectory, "HardwareIntrinsicTable.tsv")

            let text = expected |> List.map HardwareIntrinsicTable.format |> String.concat "\n"

            File.WriteAllText (regenerated, text + "\n")

            failwith
                $"WoofWare.PawPrint.Semantics/HardwareIntrinsicTable.tsv is not the JIT's table at the pinned runtime. Regenerated: cp %s{regenerated} WoofWare.PawPrint.Semantics/HardwareIntrinsicTable.tsv"

        expected.Length |> shouldBeGreaterThan 2000

    [<Test>]
    let ``the table reads the same whatever its line endings, and refuses stray whitespace`` () : unit =
        let rows = HardwareIntrinsicTable.rows.Force ()
        let lines = rows |> List.map HardwareIntrinsicTable.format

        HardwareIntrinsicTable.ofText (String.concat "\r\n" lines + "\r\n")
        |> shouldEqual rows

        HardwareIntrinsicTable.ofText (String.concat "\n" lines) |> shouldEqual rows

        Assert.Throws<Exception> (fun () ->
            HardwareIntrinsicTable.ofText (List.head lines + " \n")
            |> ignore<HardwareIntrinsicRow list>
        )
        |> ignore<exn>

    let private readCoreLib (path : string) : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory path

    let private hostCoreLib () : DumpedAssembly =
        readCoreLib (Path.Combine (FrameworkUnderTest.sharedFrameworkDirectory (), "System.Private.CoreLib.dll"))

    /// The architecture the host's JIT compiles for, and the namespace of the instruction sets it
    /// expands.
    let private requireHostTarget () : JitTarget * string =
        match RuntimeInformation.ProcessArchitecture with
        | Architecture.Arm64 -> JitTarget.Arm64, "System.Runtime.Intrinsics.Arm"
        | Architecture.X64 -> JitTarget.X64, "System.Runtime.Intrinsics.X86"
        | other ->
            Assert.Ignore $"No JIT table describes the host's architecture, %O{other}."
            failwith "unreachable: Assert.Ignore did not throw"

    /// Every hardware-instruction placeholder in `corelib`, with the class it is declared on.
    let private placeholders
        (corelib : DumpedAssembly)
        : (IntrinsicClass * string * Reflection.Metadata.MethodDefinitionHandle) list
        =
        [
            for KeyValue (handle, method) in corelib.Methods do
                if IntrinsicBody.isIntrinsic corelib handle then
                    match IntrinsicBody.classify corelib handle with
                    | IntrinsicBody.JitExpansion (JitExpansion.HardwareInstruction intrinsicClass) ->
                        yield intrinsicClass, method.Name, handle
                    | _ -> ()
        ]

    let private x86 (path : string list) : IntrinsicClass =
        {
            Namespace = "System.Runtime.Intrinsics.X86"
            Path = path
        }

    let private rowSets (target : JitTarget) (intrinsicClass : IntrinsicClass) (name : string) : string list option =
        HardwareInstruction.rows target intrinsicClass name
        |> Option.map (List.map (fun row -> row.InstructionSet))

    [<Test>]
    let ``the JIT finds some x86 classes' rows by what the CPU supports, and some in a unifying search`` () : unit =
        HardwareInstruction.instructionSet JitTarget.X64 (x86 [ "AvxVnniInt8" ])
        |> shouldEqual (Some (JitInstructionSet.ByCpuSupport ("AVXVNNIINT", "AVXVNNIINT_V512")))

        rowSets JitTarget.X64 (x86 [ "AvxVnniInt16" ]) "MultiplyWideningAndAdd"
        |> shouldEqual (Some [ "AVXVNNIINT" ; "AVXVNNIINT_V512" ])

        // Both instruction sets a CPU might pick have the same one for their 512-bit class.
        HardwareInstruction.instructionSet JitTarget.X64 (x86 [ "AvxVnniInt8" ; "V512" ])
        |> shouldEqual (Some (JitInstructionSet.Fixed "AVXVNNIINT_V512"))

        // AVX10v1 searches the AVX-512 instruction sets in turn, and AVX512 and AVX512v3 both have
        // `Compress`.
        rowSets JitTarget.X64 (x86 [ "Avx10v1" ]) "Compress"
        |> shouldEqual (Some [ "AVX512" ])

        rowSets JitTarget.X64 (x86 [ "Avx10v1" ; "V512" ]) "Compress"
        |> shouldEqual (Some [ "AVX512" ])

        rowSets JitTarget.X64 (x86 [ "Avx512Vbmi2" ]) "Compress"
        |> shouldEqual (Some [ "AVX512v3" ])

    /// Every placeholder of `corelib` has a row the JIT may expand it from, on every CPU; and at
    /// most `maxUnknown` have no contract, because such a row is a helper or special intrinsic.
    let private checkCoverage (target : JitTarget) (corelib : DumpedAssembly) (maxUnknown : int) : unit =
        let found = placeholders corelib

        let describe (placeholders : (IntrinsicClass * string * _) list) : string =
            placeholders
            |> List.truncate 40
            |> List.map (fun (c, name, _) -> $"%O{c}::%s{name}")
            |> String.concat Environment.NewLine

        let withoutRow =
            found
            |> List.filter (fun (intrinsicClass, name, _) ->
                HardwareInstruction.rows target intrinsicClass name |> Option.isNone
            )

        if not withoutRow.IsEmpty then
            failwithf
                "%d of %d placeholders have no row, including:\n%s"
                withoutRow.Length
                found.Length
                (describe withoutRow)

        let unknown =
            found
            |> List.filter (fun (intrinsicClass, name, _) ->
                HardwareInstruction.contract target intrinsicClass name = InstructionContract.Unknown
            )

        if unknown.Length > maxUnknown then
            failwithf
                "%d of %d placeholders have no contract, including:\n%s"
                unknown.Length
                found.Length
                (describe unknown)

        found.Length |> shouldBeGreaterThan 1000

    [<Test>]
    let ``every arm64 hardware placeholder has a contract, but for the JIT's helper and special ones`` () : unit =
        match requireHostTarget () with
        | JitTarget.Arm64, _ -> ()
        | _ -> Assert.Ignore "The host CoreLib is not an arm64 one, whose placeholders the Arm64 table describes."

        // Measured on the .NET 10 CoreLib: 65 of the 4,869 placeholders are the JIT's helper or
        // special intrinsics.
        checkCoverage JitTarget.Arm64 (hostCoreLib ()) 100

    [<Test>]
    let ``every x64 hardware placeholder has a contract, but for the JIT's helper and special ones`` () : unit =
        let corelib =
            readCoreLib (LinuxCoreLibFlavour.corelibPath (LinuxCoreLibFlavour.requireLinuxFramework ()))

        // Measured on the .NET 10 linux-x64 CoreLib: 139 of the 3,646 placeholders are the JIT's helper or
        // special intrinsics.
        checkCoverage JitTarget.X64 corelib 200

    /// Calls every public static method of every class in `namespace` whose `IsSupported` is true on
    /// this CPU: once with every pointer valid and aligned to 64 bytes and every other argument its
    /// default, then with each pointer null and one byte past aligned in turn, and with each
    /// `[ConstantExpected]` operand at every value its type can hold (or a range around zero, for a
    /// wide type).
    ///
    /// Arguments: the output file, the namespace, the index of the first call to make, and a file of
    /// `token<TAB>parameter` lines whose parameter's alternatives are not tried. It appends to the
    /// output a `#mvid` line naming CoreLib's module version id, a `#method` line per method it
    /// enumerates and a `#base` line per method whose first call (every pointer valid, every other
    /// argument its default) returned or raised, and one line per distinct outcome of each method:
    /// its metadata token, class, name, and the exception type or `ok`. Before each
    /// call it writes the call's index, token, parameter and arguments to `<output>.progress`, so
    /// that a call that kills the process can be found and stepped past.
    let private sweepSource : string =
        """
using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.InteropServices;

public static unsafe class Sweep
{
    static string PathOf(Type t) => t.DeclaringType == null ? t.Name : PathOf(t.DeclaringType) + "+" + t.Name;

    static bool IsSupported(Type t)
    {
        var p = t.GetProperty("IsSupported", BindingFlags.Public | BindingFlags.Static);
        return p != null && p.PropertyType == typeof(bool) && (bool)p.GetValue(null)!;
    }

    static List<object> ImmediateValues(Type t)
    {
        if (t.IsEnum)
        {
            return ImmediateValues(Enum.GetUnderlyingType(t)).Select(v => Enum.ToObject(t, v)).ToList();
        }
        var values = new List<object>();
        if (t == typeof(byte)) { for (int i = 0; i <= 255; i++) values.Add((byte)i); return values; }
        if (t == typeof(sbyte)) { for (int i = -128; i <= 127; i++) values.Add((sbyte)i); return values; }
        for (long i = -260; i <= 260; i++)
        {
            try { values.Add(Convert.ChangeType(i, t)); } catch (OverflowException) { }
        }
        return values;
    }

    static byte* buffer;

    static string Describe(object? a) => a is Pointer p ? (Pointer.Unbox(p) == null ? "null" : Pointer.Unbox(p) == buffer ? "valid" : "misaligned") : a?.ToString() ?? "null";

    public static int Main(string[] args)
    {
        var output = args[0];
        var ns = args[1];
        var startAt = long.Parse(args[2]);
        var skipped = new HashSet<string>(File.ReadAllLines(args[3]), StringComparer.Ordinal);
        const int size = 1 << 16;
        buffer = (byte*)NativeMemory.AlignedAlloc(size, 64);
        NativeMemory.Clear(buffer, size);
        var seen = new HashSet<string>(StringComparer.Ordinal);
        using var results = new StreamWriter(output, append: true) { AutoFlush = true };
        using var progress = new StreamWriter(output + ".progress") { AutoFlush = true };
        results.WriteLine($"#mvid\t{typeof(object).Assembly.ManifestModule.ModuleVersionId}");
        long callIndex = 0;

        var types = typeof(object).Assembly.GetTypes()
            .Where(t => t.Namespace == ns && (t.IsPublic || t.IsNestedPublic) && IsSupported(t))
            .OrderBy(t => t.MetadataToken);

        foreach (var type in types)
        {
            foreach (var method in type.GetMethods(BindingFlags.Public | BindingFlags.Static | BindingFlags.DeclaredOnly).OrderBy(m => m.MetadataToken))
            {
                if (method.IsSpecialName || method.ContainsGenericParameters) continue;
                var parameters = method.GetParameters();
                if (parameters.Any(p => p.ParameterType.IsByRef)) continue;

                var defaults = new object?[parameters.Length];
                var alternatives = new List<object?>[parameters.Length];
                for (int i = 0; i < parameters.Length; i++)
                {
                    var t = parameters[i].ParameterType;
                    if (t.IsPointer)
                    {
                        defaults[i] = Pointer.Box(buffer, t);
                        alternatives[i] = new List<object?> { Pointer.Box(null, t), Pointer.Box(buffer + 1, t) };
                    }
                    else if (parameters[i].GetCustomAttribute<ConstantExpectedAttribute>() != null)
                    {
                        defaults[i] = Activator.CreateInstance(t);
                        alternatives[i] = ImmediateValues(t).Cast<object?>().ToList();
                    }
                    else
                    {
                        defaults[i] = t.IsValueType ? Activator.CreateInstance(t) : null;
                        alternatives[i] = new List<object?>();
                    }
                }

                // Every call has an index, whether or not this run makes it, so that an index names
                // the same call in every run.
                void Run(int parameter, object?[] arguments)
                {
                    var index = callIndex++;
                    if (index < startAt || skipped.Contains($"{method.MetadataToken}\t{parameter}")) return;
                    progress.WriteLine($"{index}\t{method.MetadataToken}\t{parameter}\t{PathOf(type)}::{method} ({string.Join(", ", arguments.Select(Describe))})");
                    string outcome;
                    try
                    {
                        method.Invoke(null, arguments);
                        outcome = "ok";
                    }
                    catch (TargetInvocationException e)
                    {
                        outcome = e.InnerException!.GetType().FullName!;
                    }
                    var line = $"{method.MetadataToken}\t{PathOf(type)}\t{method.Name}\t{outcome}";
                    if (seen.Add(line)) results.WriteLine(line);
                    if (parameter == -1) results.WriteLine($"#base\t{method.MetadataToken}");
                }

                results.WriteLine($"#method\t{method.MetadataToken}");
                Run(-1, (object?[])defaults.Clone());
                for (int i = 0; i < parameters.Length; i++)
                {
                    foreach (var alternative in alternatives[i])
                    {
                        var arguments = (object?[])defaults.Clone();
                        arguments[i] = alternative;
                        Run(i, arguments);
                    }
                }
            }
        }

        return 0;
    }
}
"""

    /// A call the sweep made that killed the process, rather than returning or raising.
    type private Crash =
        {
            Token : string
            Parameter : string
            Call : string
            Result : RealRuntimeResult
        }

    /// Runs the sweep over `ns` to completion, stepping past each call that kills the process: the
    /// rest of that operand's values for that method are not tried. Returns the output's lines and
    /// the crashes.
    let private sweep (ns : string) : string list * Crash list =
        let image =
            Roslyn.compileAssembly "HardwareSweep" OutputKind.ConsoleApplication [] [ sweepSource ]

        let stem =
            Path.Combine (TestContext.CurrentContext.WorkDirectory, $"sweep-%O{Guid.NewGuid ()}")

        let output = stem + ".tsv"
        let skipFile = stem + ".skip"

        let rec go (attempt : int) (startAt : int64) (skipped : string list) (crashes : Crash list) =
            // Bound the restarts: a sweep that crashes this often is not measuring anything.
            if attempt > 200 then
                failwith $"The sweep crashed %d{crashes.Length} times; the last were %A{List.truncate 5 crashes}"

            File.WriteAllLines (skipFile, skipped)

            match
                RealRuntime.executeWithTimeout
                    (TimeSpan.FromMinutes 10.0)
                    [| output ; ns ; string startAt ; skipFile |]
                    image
            with
            | RealRuntimeResult.NormalExit 0 -> crashes
            | result ->
                let last =
                    match File.ReadLines (output + ".progress") |> Seq.tryLast with
                    | Some last -> last
                    | None -> failwith $"The sweep failed before making any call: %A{result}"

                match last.Split ('\t', 4) with
                | [| index ; token ; parameter ; call |] ->
                    let skipped =
                        if parameter = "-1" then
                            skipped
                        else
                            $"%s{token}\t%s{parameter}" :: skipped

                    go
                        (attempt + 1)
                        (int64 index + 1L)
                        skipped
                        ({
                            Token = token
                            Parameter = parameter
                            Call = call
                            Result = result
                         }
                         :: crashes)
                | _ -> failwith $"The sweep wrote a malformed progress line: %s{last}"

        try
            let crashes = go 0 0L [] []
            List.ofArray (File.ReadAllLines output), List.rev crashes
        finally
            for path in [ output ; output + ".progress" ; skipFile ] do
                File.Delete path

    [<Test>]
    let ``no hardware instruction raises on the real runtime what its contract leaves out`` () : unit =
        let target, ns = requireHostTarget ()

        // The vacuity floors below are measured with AVX2, which every x64 CI runner has; the
        // sweep, which inherits this process's CPU and `DOTNET_Enable*` switches, reaches too few
        // instructions without it to meet them.
        if target = JitTarget.X64 && not Runtime.Intrinsics.X86.Avx2.IsSupported then
            Assert.Ignore "This x64 host does not support AVX2, below which the sweep's vacuity floors are not met."

        let corelib = hostCoreLib ()
        let lines, crashes = sweep ns

        for crash in crashes do
            TestContext.Progress.WriteLine
                $"The real runtime died, rather than raising, on %s{crash.Call}: %A{crash.Result}"

        // The oracle is only an oracle if it ran the same CoreLib.
        for line in lines do
            if line.StartsWith "#mvid\t" then
                Guid.Parse (line.Substring 6) |> shouldEqual corelib.ModuleVersionId

        // Every method's first call was made, in some run: it returned, raised, or killed the process.
        let tagged (tag : string) =
            lines
            |> List.choose (fun line ->
                if line.StartsWith (tag + "\t") then
                    Some (line.Substring (tag.Length + 1))
                else
                    None
            )
            |> Set.ofList

        let firstCallCrashed =
            crashes
            |> List.filter (fun crash -> crash.Parameter = "-1")
            |> List.map (fun crash -> crash.Token)
            |> Set.ofList

        let neverCalled =
            Set.difference (tagged "#method") (Set.union (tagged "#base") firstCallCrashed)

        if not neverCalled.IsEmpty then
            failwith
                $"The sweep never made the first call of %d{neverCalled.Count} methods, including %A{Seq.truncate 5 neverCalled}"

        tagged "#method" |> Set.count |> shouldBeGreaterThan 1000

        let failures = ResizeArray<string> ()
        let mutable swept = Set.empty
        let mutable observed = Map.empty<InstructionFault, int>

        for line in lines |> List.filter (fun line -> not (line.StartsWith "#")) |> List.distinct do
            match line.Split '\t' with
            | [| token ; path ; name ; outcome |] ->
                let handle = MetadataTokens.MethodDefinitionHandle (int token &&& 0xFFFFFF)
                corelib.Methods.[handle].Name |> shouldEqual name

                let placeholder =
                    if IntrinsicBody.isIntrinsic corelib handle then
                        match IntrinsicBody.classify corelib handle with
                        | IntrinsicBody.JitExpansion (JitExpansion.HardwareInstruction intrinsicClass) ->
                            Some intrinsicClass
                        | _ -> None
                    else
                        None

                match placeholder with
                | None -> ()
                | Some intrinsicClass ->

                match HardwareInstruction.contract target intrinsicClass name with
                | InstructionContract.Unknown -> ()
                | InstructionContract.Raises faults ->
                    swept <- Set.add (ComparableMethodDefinitionHandle.Make handle) swept

                    let fault =
                        match outcome with
                        | "ok" -> None
                        | "System.NullReferenceException" -> Some (Ok InstructionFault.NullAddress)
                        | "System.ArgumentOutOfRangeException" -> Some (Ok InstructionFault.ImmediateOutOfRange)
                        | "System.DivideByZeroException" -> Some (Ok InstructionFault.ZeroDivisor)
                        | "System.OverflowException" -> Some (Ok InstructionFault.QuotientOverflow)
                        | other -> Some (Error other)

                    match fault with
                    | None -> ()
                    | Some (Error other) -> failures.Add $"%s{path}::%s{name} raised %s{other}, which no contract names"
                    | Some (Ok fault) ->
                        observed <- observed |> Map.change fault (fun n -> Some (1 + Option.defaultValue 0 n))

                        if not (faults.Contains fault) then
                            failures.Add
                                $"%s{path}::%s{name} raised %A{fault}, which its contract %A{faults} leaves out"
            | _ -> failwith $"The sweep wrote a malformed line: %s{line}"

        if failures.Count > 0 then
            failures
            |> Seq.distinct
            |> Seq.truncate 40
            |> String.concat Environment.NewLine
            |> failwith

        // Vacuity: the sweep reached the instructions, and provoked each fault it can in many of
        // them. Its arguments never make a quotient overflow. Measured, in methods swept and methods
        // raising each fault: on an Apple M-series CPU, 2,663 swept, 520 NullAddress and 635
        // ImmediateOutOfRange; under Rosetta's x86-64, which stops at AVX2, 1,240 swept, 174
        // NullAddress, 54 ImmediateOutOfRange and 6 ZeroDivisor.
        let minimumSwept, minimumObserved =
            match target with
            | JitTarget.Arm64 ->
                1000,
                [
                    InstructionFault.NullAddress, 100
                    InstructionFault.ImmediateOutOfRange, 100
                ]
            | JitTarget.X64 ->
                1000,
                [
                    InstructionFault.NullAddress, 100
                    InstructionFault.ImmediateOutOfRange, 40
                    InstructionFault.ZeroDivisor, 0
                ]

        swept.Count |> shouldBeGreaterThan minimumSwept

        for fault, minimum in minimumObserved do
            Map.tryFind fault observed
            |> Option.defaultValue 0
            |> shouldBeGreaterThan minimum
