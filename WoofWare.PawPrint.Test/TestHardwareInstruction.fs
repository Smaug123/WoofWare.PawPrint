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

    /// The header `runtime`'s JIT includes for its tables when compiling for `target`
    /// (hwintrinsic.cpp), which includes the rest.
    let private rootHeader (runtime : EmulatedRuntime) (target : JitTarget) : string =
        match runtime, target with
        | EmulatedRuntime.Net10, JitTarget.Arm64 -> "hwintrinsiclistarm64.h"
        | EmulatedRuntime.Net10, JitTarget.X64 -> "hwintrinsiclistxarch.h"
        | EmulatedRuntime.Net11, _ -> "hwintrinsiclist.h"

    /// The preprocessor symbols the JIT is compiled with for `target`, as far as its tables test them.
    let private predefined (target : JitTarget) : Set<string> =
        match target with
        | JitTarget.Arm64 -> Set.ofList [ "FEATURE_HW_INTRINSICS" ; "TARGET_ARM64" ; "TARGET_64BIT" ]
        | JitTarget.X64 -> Set.ofList [ "FEATURE_HW_INTRINSICS" ; "TARGET_XARCH" ; "TARGET_AMD64" ; "TARGET_64BIT" ]

    /// .NET 10's `HARDWARE_INTRINSIC(isa, name, size, numArgs, {instructions}, category, flags)`.
    let private bracedRow =
        Regex
            @"^HARDWARE_INTRINSIC\(\s*(\w+)\s*,\s*(\w+)\s*,\s*-?\d+\s*,\s*-?\d+\s*,\s*\{([^}]*)\}\s*,\s*(\w+)\s*,\s*(.*)\)\s*$"

    /// .NET 11's `HARDWARE_INTRINSIC(isa, name, size, numArgs, instruction × 10, intCost, fltCost,
    /// category, flags)`.
    let private costedRow =
        Regex
            @"^HARDWARE_INTRINSIC\(\s*(\w+)\s*,\s*(\w+)\s*,\s*-?\d+\s*,\s*-?\d+\s*,((?:\s*\w+\s*,){10})\s*-?\d+\s*,\s*-?\d+\s*,\s*(\w+)\s*,\s*(.*)\)\s*$"

    /// A `#if` or `#elif` condition: `defined(X)` and `!defined(X)` joined by `||` or `&&`, the only
    /// forms the tables use; anything else is refused rather than guessed at.
    let private holds (defined : Set<string>) (path : string) (condition : string) : bool =
        let condition =
            match condition.IndexOf "//" with
            | -1 -> condition
            | comment -> condition.Substring (0, comment)

        let atom (text : string) : bool =
            let m = Regex.Match (text, @"^\s*(!?)\s*defined\s*\(\s*(\w+)\s*\)\s*$")

            if not m.Success then
                failwith $"%s{path}: a preprocessor condition this does not read: %s{condition}"

            (m.Groups.[1].Value = "") = defined.Contains m.Groups.[2].Value

        condition.Split "||"
        |> Array.exists (fun conjunction -> conjunction.Split "&&" |> Array.forall atom)

    /// One branch of an open conditional: whether this branch is live, and whether any branch of
    /// the conditional has been.
    type private Branch =
        {
            Live : bool
            Taken : bool
        }

    /// The rows `runtime`'s JIT compiles into its tables for `target`, reading `jit`'s headers as
    /// the preprocessor does: following `#include`s and the conditionals on the target, and
    /// dropping a flag the headers define as `(0)` for it.
    let private rowsOf (runtime : EmulatedRuntime) (jit : string) (target : JitTarget) : HardwareIntrinsicRow list =
        let rowPattern =
            match runtime with
            | EmulatedRuntime.Net10 -> bracedRow
            | EmulatedRuntime.Net11 -> costedRow

        let directive = Regex @"^\s*#\s*(\w+)\s*(.*)$"

        // Every symbol the headers have defined so far, with its definition.
        let mutable defines : Map<string, string> =
            predefined target
            |> Seq.map (fun symbol -> symbol, "")
            |> Map.ofSeq
            |> Map.add "HARDWARE_INTRINSIC" ""

        let rows = ResizeArray<HardwareIntrinsicRow> ()

        let rec read (file : string) : unit =
            let path = Path.Combine (jit, file)
            let mutable branches : Branch list = []

            let live () =
                branches |> List.forall (fun branch -> branch.Live)

            let isDefined () = defines |> Map.keys |> Set.ofSeq

            for line in File.ReadAllLines path do
                let m = directive.Match line

                if m.Success then
                    let rest = m.Groups.[2].Value.Trim ()

                    match m.Groups.[1].Value, branches with
                    | "if", _ ->
                        let live = live () && holds (isDefined ()) path rest

                        branches <-
                            {
                                Live = live
                                Taken = live
                            }
                            :: branches
                    | ("ifdef" | "ifndef") as kind, _ ->
                        let symbol = rest.Split(' ').[0]
                        let live = (kind = "ifdef") = defines.ContainsKey symbol

                        branches <-
                            {
                                Live = live
                                Taken = live
                            }
                            :: branches
                    | "elif", branch :: outer ->
                        let live =
                            not branch.Taken
                            && List.forall (fun b -> b.Live) outer
                            && holds (isDefined ()) path rest

                        branches <-
                            {
                                Live = live
                                Taken = branch.Taken || live
                            }
                            :: outer
                    | "else", branch :: outer ->
                        branches <-
                            {
                                Live = not branch.Taken
                                Taken = true
                            }
                            :: outer
                    | "endif", _ :: outer -> branches <- outer
                    | _ when not (live ()) -> ()
                    | "include", _ ->
                        match Regex.Match (rest, "^\"(hwintrinsiclist\w*\.h)\"$") with
                        | included when included.Success -> read included.Groups.[1].Value
                        | _ -> failwith $"%s{path}: an include this does not follow: %s{line}"
                    | "define", _ ->
                        let parts = rest.Split ((null : char[]), 2, StringSplitOptions.RemoveEmptyEntries)
                        let definition = if parts.Length > 1 then parts.[1].Trim () else ""
                        defines <- defines.Add (parts.[0], definition)
                    | "undef", _ -> defines <- defines.Remove rest
                    | "error", _ -> failwith $"%s{path}: the preprocessor would stop at %s{line}"
                    | _ -> failwith $"%s{path}: a directive this does not read: %s{line}"
                elif live () && line.StartsWith "HARDWARE_INTRINSIC(" then
                    if not (defines.ContainsKey "HARDWARE_INTRINSIC") then
                        failwith $"%s{path}: a row after HARDWARE_INTRINSIC is undefined: %s{line}"

                    let m = rowPattern.Match line

                    if not m.Success then
                        failwith $"%s{path}: a row this does not parse: %s{line}"

                    let flags =
                        m.Groups.[5].Value.Split '|'
                        |> Seq.map (fun flag -> flag.Trim ())
                        |> Seq.filter (fun flag ->
                            match defines.TryFind flag with
                            | None -> true
                            | Some "(0)" -> false
                            | Some other -> failwith $"%s{path}: flag %s{flag} is defined as %s{other}"
                        )
                        |> Set.ofSeq

                    rows.Add
                        {
                            Target = target
                            InstructionSet = m.Groups.[1].Value
                            Name = m.Groups.[2].Value
                            Instructions =
                                m.Groups.[3].Value.Split ','
                                |> Seq.map (fun instruction -> instruction.Trim ())
                                |> Seq.filter (fun instruction -> instruction <> "" && instruction <> "INS_invalid")
                                |> Set.ofSeq
                            Category = m.Groups.[4].Value
                            Flags = flags
                        }

            if not branches.IsEmpty then
                failwith $"%s{path}: %d{branches.Length} conditionals are open at its end"

        read (rootHeader runtime target)
        List.ofSeq rows

    /// Also how each checked-in table is made: on a difference, the regenerated table is written
    /// out and the failure says where to copy it from.
    let runtimes : TestCaseData list = FrameworkUnderTest.everyRuntime

    /// The reader is what the checked-in tables are held to, so it is held to a header whose
    /// conditionals overlap where the JIT's do not: a live `#elif` after a taken branch, a flag
    /// defined as `(0)` on one target only, an `#include`, and a row after `#undef`.
    [<Test>]
    let ``the header reader preprocesses as the C preprocessor does`` () : unit =
        let dir =
            Path.Combine (TestContext.CurrentContext.WorkDirectory, $"headers-%O{Guid.NewGuid ()}")

        Directory.CreateDirectory dir |> ignore<DirectoryInfo>

        let row (isa : string) (name : string) (flags : string) : string =
            let instructions = String.replicate 9 "INS_invalid, "
            $"HARDWARE_INTRINSIC(%s{isa}, %s{name}, 16, 2, INS_a, %s{instructions}1, -1, HW_Category_SIMD, %s{flags})"

        let write (file : string) (lines : string list) : unit =
            File.WriteAllLines (Path.Combine (dir, file), lines)

        try
            write
                "hwintrinsiclist.h"
                [
                    "#ifndef HARDWARE_INTRINSIC"
                    "#error Define HARDWARE_INTRINSIC before including this file"
                    "#endif"
                    "#ifdef FEATURE_HW_INTRINSICS"
                    "#if !defined(TARGET_XARCH)"
                    "#define HW_Flag_Zero   (0)"
                    "#endif"
                    "#if defined(TARGET_64BIT) // every target here"
                    row "A" "First" "HW_Flag_X|HW_Flag_Zero"
                    "#elif defined(TARGET_ARM64)"
                    row "A" "Shadowed" "HW_Flag_X"
                    "#else"
                    row "A" "Never" "HW_Flag_X"
                    "#endif"
                    "#if defined(TARGET_ARM64) && defined(TARGET_XARCH)"
                    row "A" "Neither" "HW_Flag_X"
                    "#elif defined(TARGET_ARM64) || defined(TARGET_XARCH)"
                    row "A" "Either" "HW_Flag_X"
                    "#endif"
                    "#include \"hwintrinsiclistinner.h\""
                    "#endif // FEATURE_HW_INTRINSICS"
                    "#undef HARDWARE_INTRINSIC"
                ]

            write "hwintrinsiclistinner.h" [ row "B" "Inner" "HW_Flag_NoFlag" ]

            for target in JitTarget.all do
                let rows = rowsOf EmulatedRuntime.Net11 dir target

                rows
                |> List.map (fun row -> row.InstructionSet, row.Name, row.Flags)
                |> shouldEqual
                    [
                        "A",
                        "First",
                        (match target with
                         | JitTarget.Arm64 -> Set.ofList [ "HW_Flag_X" ]
                         | JitTarget.X64 -> Set.ofList [ "HW_Flag_X" ; "HW_Flag_Zero" ])
                        "A", "Either", Set.ofList [ "HW_Flag_X" ]
                        "B", "Inner", Set.ofList [ "HW_Flag_NoFlag" ]
                    ]

                rows
                |> List.iter (fun row -> row.Instructions |> shouldEqual (Set.ofList [ "INS_a" ]))

            // A row the preprocessor would not expand, and a condition this does not read, are refused.
            write "hwintrinsiclistinner.h" [ "#undef HARDWARE_INTRINSIC" ; row "B" "Late" "HW_Flag_NoFlag" ]

            Assert.Throws<Exception> (fun () ->
                rowsOf EmulatedRuntime.Net11 dir JitTarget.X64
                |> ignore<HardwareIntrinsicRow list>
            )
            |> ignore<exn>

            write "hwintrinsiclistinner.h" [ "#if TARGET_ARM64 > 0" ; "#endif" ]

            Assert.Throws<Exception> (fun () ->
                rowsOf EmulatedRuntime.Net11 dir JitTarget.X64
                |> ignore<HardwareIntrinsicRow list>
            )
            |> ignore<exn>
        finally
            Directory.Delete (dir, true)

    [<TestCaseSource(nameof runtimes)>]
    let ``each checked-in table is the JIT's, as that runtime's pinned source states it``
        (runtime : EmulatedRuntime)
        : unit
        =
        let jit =
            Path.Combine (FrameworkUnderTest.requireRuntimeSourceOf runtime, "src", "coreclr", "jit")

        let expected =
            JitTarget.all
            |> List.collect (rowsOf runtime jit)
            |> List.sortBy HardwareIntrinsicTable.format

        let file = $"HardwareIntrinsicTable.%A{runtime}.tsv"

        // A table in an older format fails to parse, and is regenerated like any other difference.
        let checkedIn =
            try
                Ok (HardwareIntrinsicTable.rows runtime)
            with e ->
                Error e.Message

        if checkedIn <> Ok expected then
            let regenerated = Path.Combine (TestContext.CurrentContext.WorkDirectory, file)

            let text = expected |> List.map HardwareIntrinsicTable.format |> String.concat "\n"

            File.WriteAllText (regenerated, text + "\n")

            failwith
                $"WoofWare.PawPrint.Semantics/%s{file} is not the JIT's table at %A{runtime}'s pinned source. Regenerated: cp %s{regenerated} WoofWare.PawPrint.Semantics/%s{file}"

        expected.Length |> shouldBeGreaterThan 2000

    [<TestCaseSource(nameof runtimes)>]
    let ``each table reads the same whatever its line endings, and refuses stray whitespace``
        (runtime : EmulatedRuntime)
        : unit
        =
        let rows = HardwareIntrinsicTable.rows runtime
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

    let private arm (path : string list) : IntrinsicClass =
        {
            Namespace = "System.Runtime.Intrinsics.Arm"
            Path = path
        }

    let private rowSets
        (runtime : EmulatedRuntime)
        (target : JitTarget)
        (intrinsicClass : IntrinsicClass)
        (name : string)
        : string list option
        =
        HardwareInstruction.rows runtime target intrinsicClass name
        |> Option.map (List.map (fun row -> row.InstructionSet))

    [<TestCaseSource(nameof runtimes)>]
    let ``the JIT finds some x86 classes' rows by what the CPU supports, and some in a unifying search``
        (runtime : EmulatedRuntime)
        : unit
        =
        HardwareInstruction.instructionSet runtime JitTarget.X64 (x86 [ "AvxVnniInt8" ])
        |> shouldEqual (Some (JitInstructionSet.ByCpuSupport ("AVXVNNIINT", "AVXVNNIINT_V512")))

        rowSets runtime JitTarget.X64 (x86 [ "AvxVnniInt16" ]) "MultiplyWideningAndAdd"
        |> shouldEqual (Some [ "AVXVNNIINT" ; "AVXVNNIINT_V512" ])

        // Both instruction sets a CPU might pick have the same one for their 512-bit class.
        HardwareInstruction.instructionSet runtime JitTarget.X64 (x86 [ "AvxVnniInt8" ; "V512" ])
        |> shouldEqual (Some (JitInstructionSet.Fixed "AVXVNNIINT_V512"))

        // AVX10v1 searches the AVX-512 instruction sets in turn, and AVX512 and AVX512v3 both have
        // `Compress`.
        rowSets runtime JitTarget.X64 (x86 [ "Avx10v1" ]) "Compress"
        |> shouldEqual (Some [ "AVX512" ])

        rowSets runtime JitTarget.X64 (x86 [ "Avx10v1" ; "V512" ]) "Compress"
        |> shouldEqual (Some [ "AVX512" ])

        rowSets runtime JitTarget.X64 (x86 [ "Avx512Vbmi2" ]) "Compress"
        |> shouldEqual (Some [ "AVX512v3" ])

    [<Test>]
    let ``.NET 11's JIT names other instruction sets than .NET 10's for some classes`` () : unit =
        let both (target : JitTarget) (intrinsicClass : IntrinsicClass) =
            HardwareInstruction.instructionSet EmulatedRuntime.Net10 target intrinsicClass,
            HardwareInstruction.instructionSet EmulatedRuntime.Net11 target intrinsicClass

        let fixedSet (isa : string) = Some (JitInstructionSet.Fixed isa)

        // X86Base absorbs SSE3 to SSE4.2 and POPCNT.
        for name in [ "Popcnt" ; "Sse3" ; "Ssse3" ; "Sse41" ; "Sse42" ] do
            both JitTarget.X64 (x86 [ name ])
            |> shouldEqual (fixedSet "SSE42", fixedSet "X86Base")

            both JitTarget.X64 (x86 [ name ; "X64" ])
            |> shouldEqual (fixedSet "SSE42_X64", fixedSet "X86Base_X64")

        // Without AVX-VNNI, .NET 11's `AvxVnni` is AVX512v3's EVEX encoding.
        both JitTarget.X64 (x86 [ "AvxVnni" ])
        |> shouldEqual (fixedSet "AVXVNNI", Some (JitInstructionSet.ByCpuSupport ("AVXVNNI", "AVX512v3")))

        both JitTarget.X64 (x86 [ "AvxVnni" ; "X64" ])
        |> shouldEqual (fixedSet "AVXVNNI_X64", Some (JitInstructionSet.ByCpuSupport ("AVXVNNI_X64", "AVX512v3_X64")))

        both JitTarget.X64 (x86 [ "AvxVnni" ; "V512" ])
        |> shouldEqual (None, fixedSet "AVX512v3")

        both JitTarget.X64 (x86 [ "Avx512Bmm" ])
        |> shouldEqual (None, fixedSet "AVX512BMM")

        for name in [ "Sha3" ; "Sm4" ; "SveAes" ; "SveSha3" ; "SveSm4" ] do
            both JitTarget.Arm64 (arm [ name ]) |> shouldEqual (None, fixedSet name)

            both JitTarget.Arm64 (arm [ name ; "Arm64" ])
            |> shouldEqual (None, fixedSet (name + "_Arm64"))

    /// Every placeholder of `corelib` has a row `runtime`'s JIT may expand it from, on every CPU;
    /// and at most `maxUnknown` have no contract, because such a row is a helper or special
    /// intrinsic.
    let private checkCoverage
        (runtime : EmulatedRuntime)
        (target : JitTarget)
        (corelib : DumpedAssembly)
        (maxUnknown : int)
        : unit
        =
        EmulatedRuntime.ofCoreLib corelib |> shouldEqual runtime
        let found = placeholders corelib

        let describe (placeholders : (IntrinsicClass * string * _) list) : string =
            placeholders
            |> List.truncate 40
            |> List.map (fun (c, name, _) -> $"%O{c}::%s{name}")
            |> String.concat Environment.NewLine

        let withoutRow =
            found
            |> List.filter (fun (intrinsicClass, name, _) ->
                HardwareInstruction.rows runtime target intrinsicClass name |> Option.isNone
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
                HardwareInstruction.contract runtime target intrinsicClass name = InstructionContract.Unknown
            )

        if unknown.Length > maxUnknown then
            failwithf
                "%d of %d placeholders have no contract, including:\n%s"
                unknown.Length
                found.Length
                (describe unknown)

        found.Length |> shouldBeGreaterThan 1000

    [<TestCaseSource(nameof runtimes)>]
    let ``every arm64 hardware placeholder has a contract, but for the JIT's helper and special ones``
        (runtime : EmulatedRuntime)
        : unit
        =
        match requireHostTarget () with
        | JitTarget.Arm64, _ -> ()
        | _ -> Assert.Ignore "The host CoreLib is not an arm64 one, whose placeholders the Arm64 table describes."

        // Measured: 65 of the 4,869 placeholders of the .NET 10 CoreLib are the JIT's helper or
        // special intrinsics, and 65 of .NET 11's 5,220.
        checkCoverage runtime JitTarget.Arm64 (readCoreLib (FrameworkUnderTest.requireCoreLibPathOf runtime)) 100

    [<TestCaseSource(nameof runtimes)>]
    let ``every x64 hardware placeholder has a contract, but for the JIT's helper and special ones``
        (runtime : EmulatedRuntime)
        : unit
        =
        let corelib =
            readCoreLib (LinuxCoreLibFlavour.corelibPath (FrameworkUnderTest.requireLinuxFrameworkDirectoryOf runtime))

        // Measured: 139 of the 3,646 placeholders of the .NET 10 linux-x64 CoreLib are the JIT's
        // helper or special intrinsics, and 86 of .NET 11's 3,659.
        checkCoverage runtime JitTarget.X64 corelib 200

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

                match HardwareInstruction.contract (EmulatedRuntime.ofCoreLib corelib) target intrinsicClass name with
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
