namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// A seeded corpus of generated struct shapes, and what real .NET's `Marshal.OffsetOf` says about
/// every field of each: the oracle both fixtures below hold PawPrint to.
///
/// The corpus is compiled once by Roslyn and loaded into this process, where the host's own
/// `Marshal.OffsetOf` answers. Loading it in-process is safe for the reason
/// `TestBaseChainLayout` gives: it is a library with no entry point and no static state, so it
/// cannot touch a process-global.
module internal MarshalOffsetOfCorpus =

    let corpusNamespace : string = "PawPrint.MarshalOffsetOf"

    /// Field types, as C# spells them. Every one is a shape `CliValueType.TryComputeNativeLayout`
    /// sizes, so a disagreement is a wrong answer rather than a refusal.
    let fieldKinds : string list =
        [
            "byte"
            "sbyte"
            "short"
            "ushort"
            "int"
            "uint"
            "long"
            "ulong"
            "float"
            "double"
            "nint"
            "nuint"
            "E8"
            "E16"
            "E32"
            "E64"
            // CoreLib structs. `DateTime` is auto-layout, and marshals as a field only through
            // CoreCLR's `MARSHAL_TYPE_DATE` shortcut; `decimal` is not blittable as a field.
            "decimal"
            "DateTime"
            "Guid"
            "TimeSpan"
            // CoreCLR stamps these 16-byte alignment by name, over the 8 their fields imply.
            "Int128"
            "UInt128"
            "NestSeq"
            "NestPacked"
            "NestExplicit"
            "NestSized"
            "NestDate"
            "NestGeneric<long>"
            "NestGeneric<E16>"
            // Auto-layout, so any struct holding one cannot be marshalled at all.
            "NestAuto"
            // A four-byte BOOL, and a `char` whose width the containing struct's `CharSet` decides.
            "bool"
            "char"
            // The same, with the width fixed by the nested struct's own `CharSet` or `[MarshalAs]`.
            "NestBoolU1"
            "NestCharUnicode"
            "NestCharAuto"
            "NestCharU1"
            "NestCharU2"
            // Each of these declares a field CoreCLR refuses, so cannot itself be marshalled, but
            // is laid out as a field all the same, the refused field taking one byte.
            "NestVariantBool"
            "NestIllegalWide"
            "NestHoldsAuto"
            "NestR4Int"
            "NestI8Date"
            "NestPackedIllegal"
            // And a struct holding one of those, which can be marshalled.
            "NestHoldsIllegal"
        ]

    let private fixedCorpus : string =
        """
using System;
using System.Runtime.InteropServices;

namespace PawPrint.MarshalOffsetOf;

public enum E8 : byte { A = 1 }
public enum E16 : short { A = 1 }
public enum E32 { A = 1 }
public enum E64 : long { A = 1 }

public struct NestSeq { public byte T; public int K; }
[StructLayout(LayoutKind.Sequential, Pack = 1)]
public struct NestPacked { public byte T; public long K; }
[StructLayout(LayoutKind.Explicit)]
public struct NestExplicit { [FieldOffset(1)] public uint K; [FieldOffset(0)] public byte T; }
[StructLayout(LayoutKind.Sequential, Size = 13)]
public struct NestSized { public byte T; }
public struct NestDate { public byte T; public DateTime D; }
public struct NestGeneric<T> { public byte T0; public T V; }
[StructLayout(LayoutKind.Auto)]
public struct NestAuto { public int A; public int B; }
public struct NestBoolU1 { public byte T; [MarshalAs(UnmanagedType.U1)] public bool K; }
[StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
public struct NestCharUnicode { public byte T; public char K; }
[StructLayout(LayoutKind.Sequential, CharSet = CharSet.Auto)]
public struct NestCharAuto { public byte T; public char K; }
[StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
public struct NestCharU1 { public byte T; [MarshalAs(UnmanagedType.U1)] public char K; }
public struct NestCharU2 { public byte T; [MarshalAs(UnmanagedType.U2)] public char K; }
public struct NestVariantBool { public byte T; [MarshalAs(UnmanagedType.VariantBool)] public bool K; }
public struct NestIllegalWide { public long L; [MarshalAs(UnmanagedType.VariantBool)] public bool K; public byte T; }
public struct NestHoldsAuto { public int A; public NestAuto K; }
public struct NestR4Int { public short T; [MarshalAs(UnmanagedType.R4)] public int K; }
public struct NestI8Date { [MarshalAs(UnmanagedType.I8)] public DateTime K; public int T; }
[StructLayout(LayoutKind.Sequential, Pack = 2)]
public struct NestPackedIllegal { public long L; [MarshalAs(UnmanagedType.VariantBool)] public bool K; }
public struct NestHoldsIllegal { public byte T; public NestIllegalWide K; }
"""

    [<RequireQualifiedAccess>]
    type ShapeLayout =
        /// `[StructLayout(Sequential, Pack = pack, Size = size)]`; 0 omits the argument.
        | Sequential of pack : int * size : int
        /// `[StructLayout(Explicit, Size = size)]`, with each field's offset alongside it.
        | Explicit of size : int
        /// `[StructLayout(Auto)]`, which has no native layout whatever the fields.
        | Auto

    type Shape =
        {
            Name : string
            Layout : ShapeLayout
            /// The `CharSet` argument to `[StructLayout]`, which decides a `char` field's native
            /// width; `None` omits it.
            CharSet : CharSet option
            /// Field type as C# spells it, and its `[FieldOffset]` under explicit layout.
            Fields : (string * int option) list
        }

    let private genCharSet : Gen<CharSet option> =
        Gen.elements [ None ; None ; Some CharSet.Ansi ; Some CharSet.Unicode ; Some CharSet.Auto ]

    let private genShape : Gen<ShapeLayout * CharSet option * (string * int option) list> =
        gen {
            let! count = Gen.choose (1, 5)
            let! kinds = Gen.listOfLength count (Gen.elements fieldKinds)
            let! size = Gen.elements [ 0 ; 0 ; 0 ; 1 ; 7 ; 13 ; 40 ]
            let! charSet = genCharSet

            let! layout =
                Gen.frequency
                    [
                        6, Gen.constant "sequential"
                        3, Gen.constant "explicit"
                        1, Gen.constant "auto"
                    ]

            match layout with
            | "explicit" ->
                // None of these fields is a reference, so overlap and misalignment are both legal,
                // and CoreCLR takes the offsets as written.
                let! offsets = Gen.listOfLength count (Gen.choose (0, 24))
                return ShapeLayout.Explicit size, charSet, List.zip kinds (offsets |> List.map Some)
            | "auto" -> return ShapeLayout.Auto, charSet, kinds |> List.map (fun kind -> kind, None)
            | _ ->
                let! pack = Gen.elements [ 0 ; 0 ; 1 ; 2 ; 4 ; 8 ; 16 ]
                return ShapeLayout.Sequential (pack, size), charSet, kinds |> List.map (fun kind -> kind, None)
        }

    let fieldName (index : int) : string = $"f%d{index}"

    let render (shape : Shape) : string =
        let sizeArg (size : int) : string =
            if size = 0 then "" else $", Size = %d{size}"

        let charSetArg =
            match shape.CharSet with
            | None -> ""
            | Some charSet -> $", CharSet = CharSet.%O{charSet}"

        let attribute =
            match shape.Layout with
            | ShapeLayout.Sequential (pack, size) ->
                let packArg = if pack = 0 then "" else $", Pack = %d{pack}"
                $"[StructLayout(LayoutKind.Sequential%s{packArg}%s{sizeArg size}%s{charSetArg})]"
            | ShapeLayout.Explicit size -> $"[StructLayout(LayoutKind.Explicit%s{sizeArg size}%s{charSetArg})]"
            | ShapeLayout.Auto -> $"[StructLayout(LayoutKind.Auto%s{charSetArg})]"

        let fields =
            shape.Fields
            |> List.mapi (fun i (kind, offset) ->
                let offsetAttr =
                    match offset with
                    | None -> ""
                    | Some o -> $"[FieldOffset(%d{o})] "

                $"%s{offsetAttr}public %s{kind} %s{fieldName i};"
            )
            |> String.concat " "

        $"%s{attribute} public struct %s{shape.Name} {{ %s{fields} }}"

    /// Shapes whose real .NET answers the vacuity guards rely on, so that those do not depend on
    /// what the generator happened to draw.
    let private pinnedShapes : (ShapeLayout * CharSet option * (string * int option) list) list =
        [
            // `DateTime` marshals as an 8-byte date, 8-aligned.
            ShapeLayout.Sequential (0, 0), None, [ "byte", None ; "DateTime", None ; "byte", None ]
            // A field with no native layout makes its container unmarshalable.
            ShapeLayout.Sequential (0, 0), None, [ "int", None ; "NestAuto", None ]
            ShapeLayout.Explicit 0, None, [ "long", Some 0 ; "E32", Some 3 ; "decimal", Some 5 ]
            ShapeLayout.Sequential (0, 0), None, [ "byte", None ; "Int128", None ]
            // A `bool` is a 4-byte BOOL, and a `char` is 1 byte or 2 by the struct's `CharSet`.
            ShapeLayout.Sequential (0, 0), None, [ "byte", None ; "bool", None ; "char", None ; "byte", None ]
            ShapeLayout.Sequential (0, 0),
            Some CharSet.Unicode,
            [ "byte", None ; "bool", None ; "char", None ; "byte", None ]
            // A struct CoreCLR cannot marshal, because a field of its own is refused, is laid out
            // as a field nonetheless.
            ShapeLayout.Sequential (0, 0), None, [ "byte", None ; "NestIllegalWide", None ; "byte", None ]
            // The seeded sample happens not to draw a `float` under explicit layout.
            ShapeLayout.Explicit 0, None, [ "float", Some 3 ; "NestPackedIllegal", Some 1 ]
        ]

    /// A fixed, seeded sample rather than a fresh FsCheck run: the whole corpus has to be compiled
    /// before either runtime can look at any of it. A failure names the offending shape's source,
    /// which is all it takes to reproduce it.
    let shapes : Shape list =
        pinnedShapes
        @ List.ofArray (Gen.sampleWithSeed (Rnd 0x4F66667365744F66UL) 10 300 genShape)
        |> List.mapi (fun i (layout, charSet, fields) ->
            {
                Name = $"Shape%d{i}"
                Layout = layout
                CharSet = charSet
                Fields = fields
            }
        )

    /// The corpus's C# source: the fixed declarations, and one struct per shape.
    let sources : string list =
        let generated =
            shapes
            |> List.map render
            |> String.concat "\n"
            |> fun body ->
                $"using System;\nusing System.Runtime.InteropServices;\nnamespace %s{corpusNamespace};\n%s{body}\n"

        [ fixedCorpus ; generated ]

    let corpusBytes : byte array =
        Roslyn.compileAssembly corpusNamespace Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary [] sources

    let private corpusRuntimeAssembly : System.Reflection.Assembly =
        System.Reflection.Assembly.Load corpusBytes

    let hostType (shape : Shape) : Type =
        corpusRuntimeAssembly.GetType $"%s{corpusNamespace}.%s{shape.Name}"
        |> Option.ofObj
        |> Option.defaultWith (fun () -> failwith $"corpus does not contain %s{shape.Name}")

    /// What real .NET's `Marshal.OffsetOf` does for one field.
    [<RequireQualifiedAccess>]
    type HostAnswer =
        | Offset of int
        /// `ArgumentException` with this message: CoreCLR's `IDS_CANNOT_MARSHAL`.
        | CannotMarshal of message : string

    let hostAnswer (shape : Shape) (index : int) : HostAnswer =
        try
            HostAnswer.Offset (int (Marshal.OffsetOf (hostType shape, fieldName index)))
        with :? ArgumentException as e when e.GetType () = typeof<ArgumentException> ->
            HostAnswer.CannotMarshal e.Message

    /// Every field of every shape, with real .NET's answer for it.
    let hostAnswers : (Shape * int * HostAnswer) list =
        [
            for shape in shapes do
                for index in 0 .. shape.Fields.Length - 1 do
                    yield shape, index, hostAnswer shape index
        ]

/// `CliType.TryComputeMarshalFieldOffset`, the function `MarshalNative_OffsetOf` answers with, run
/// directly over the whole corpus and compared with real .NET field by field.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMarshalOffsetOf =
    open MarshalOffsetOfCorpus

    let private loggerFactory = snd (LoggerFactory.makeTest ())

    let private corelibPath : string = typeof<obj>.Assembly.Location
    let private runtimeDir : string = Path.GetDirectoryName corelibPath
    let private corelib : DumpedAssembly = Assembly.readFile loggerFactory corelibPath
    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private corpusAssembly : DumpedAssembly =
        use stream = new MemoryStream (corpusBytes)
        AssemblyApi.read loggerFactory (Some $"%s{corpusNamespace}.dll") stream

    let private baseState : IlMachineState =
        let dirs = ImmutableArray.CreateRange [ runtimeDir ]
        let state = IlMachineState.initial loggerFactory dirs corelib

        let state =
            { state with
                ConcreteTypes = Corelib.concretizeAll state._LoadedAssemblies bct AllConcreteTypes.Empty
            }

        state.WithLoadedAssembly corpusAssembly

    /// PawPrint's answer for every field of `shape`, from the type's zero value built through the
    /// same entry point every allocation site uses.
    let private pawPrintAnswers (shape : Shape) : Result<int, MarshalSizeError> list =
        let typeInfo =
            corpusAssembly.TypeDefs
            |> Seq.map (fun kvp -> kvp.Value)
            |> Seq.filter (fun ti -> ti.Name = shape.Name)
            |> Seq.exactlyOne

        let state, handle =
            IlMachineTypeResolution.concretizeType
                loggerFactory
                bct
                baseState
                typeInfo.AssemblyFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (typeInfo.Identity, System.Reflection.Metadata.SignatureTypeKind.ValueType))

        let zero, state = IlMachineState.cliTypeZeroOfHandle state bct handle

        shape.Fields
        |> List.mapi (fun index _ ->
            let field =
                typeInfo.Fields
                |> List.filter (fun f -> f.Name = fieldName index)
                |> List.exactlyOne

            CliType.TryComputeMarshalFieldOffset
                state.ConcreteTypes
                state._LoadedAssemblies
                bct
                handle
                zero
                (ComparableFieldDefinitionHandle.Make field.Handle)
        )

    [<Test>]
    let ``Marshal field offsets agree with real .NET`` () : unit =
        let failures =
            hostAnswers
            |> List.groupBy (fun (shape, _, _) -> shape.Name)
            |> List.choose (fun (_, answers) ->
                let shape, _, _ = List.head answers
                let expected = answers |> List.map (fun (_, _, answer) -> answer)
                let actual = pawPrintAnswers shape

                let agrees (host : HostAnswer) (pawPrint : Result<int, MarshalSizeError>) : bool =
                    match host, pawPrint with
                    | HostAnswer.Offset h, Result.Ok p -> h = p
                    | HostAnswer.CannotMarshal _, Result.Error (MarshalSizeError.NotMarshalable _) -> true
                    | _, _ -> false

                if List.forall2 agrees expected actual then
                    None
                else
                    Some $"%s{render shape}\n  real .NET: %A{expected}\n  PawPrint:  %A{actual}"
            )

        match failures with
        | [] -> ()
        | _ ->
            let described = String.concat "\n" failures
            failwith $"%d{failures.Length} of %d{shapes.Length} shapes disagree:\n%s{described}"

    [<Test>]
    let ``the corpus reaches every field kind, and real .NET both answers and refuses`` () : unit =
        // Vacuity guard: a sample that stopped drawing some kind, or whose every shape real .NET
        // refused, would pass the comparison above while testing much less than it claims.
        let kindsUnder (predicate : ShapeLayout -> bool) : Set<string> =
            shapes
            |> List.filter (fun shape -> predicate shape.Layout)
            |> List.collect (fun shape -> shape.Fields |> List.map fst)
            |> Set.ofList

        let isExplicit (layout : ShapeLayout) : bool =
            match layout with
            | ShapeLayout.Explicit _ -> true
            | ShapeLayout.Sequential _
            | ShapeLayout.Auto -> false

        let isSequential (layout : ShapeLayout) : bool =
            match layout with
            | ShapeLayout.Sequential _ -> true
            | ShapeLayout.Explicit _
            | ShapeLayout.Auto -> false

        let expected = Set.ofList fieldKinds
        Set.difference expected (kindsUnder isExplicit) |> shouldEqual Set.empty
        Set.difference expected (kindsUnder isSequential) |> shouldEqual Set.empty

        let offsets, refusals =
            hostAnswers
            |> List.partition (fun (_, _, answer) ->
                match answer with
                | HostAnswer.Offset _ -> true
                | HostAnswer.CannotMarshal _ -> false
            )

        offsets.Length |> shouldBeGreaterThan 500
        refusals.Length |> shouldBeGreaterThan 50

        hostAnswer shapes.[0] 2 |> shouldEqual (HostAnswer.Offset 16)
        hostAnswer shapes.[2] 2 |> shouldEqual (HostAnswer.Offset 5)
        // Not 8, which is what `Int128`'s own two `ulong`s would imply.
        hostAnswer shapes.[3] 1 |> shouldEqual (HostAnswer.Offset 16)
        // The BOOL is 4-aligned and 4 wide, and the `char` after it 1 wide or 2.
        [ 1 ; 2 ; 3 ]
        |> List.map (hostAnswer shapes.[4])
        |> shouldEqual [ HostAnswer.Offset 4 ; HostAnswer.Offset 8 ; HostAnswer.Offset 9 ]

        [ 1 ; 2 ; 3 ]
        |> List.map (hostAnswer shapes.[5])
        |> shouldEqual [ HostAnswer.Offset 4 ; HostAnswer.Offset 8 ; HostAnswer.Offset 10 ]

        // Every `CharSet` spelling reaches a `char` field, directly rather than through a nested
        // struct, somewhere the offset comparison sees it.
        shapes
        |> List.filter (fun shape -> shape.Layout <> ShapeLayout.Auto)
        |> List.filter (fun shape -> shape.Fields |> List.exists (fun (kind, _) -> kind = "char"))
        |> List.map _.CharSet
        |> Set.ofList
        |> shouldEqual (Set.ofList [ None ; Some CharSet.Ansi ; Some CharSet.Unicode ; Some CharSet.Auto ])

        // `NestIllegalWide` is `{ long; illegal; byte }`: 16 bytes, 8-aligned, the refused field
        // one byte at offset 8.
        [ 0 ; 1 ; 2 ]
        |> List.map (hostAnswer shapes.[6])
        |> shouldEqual [ HostAnswer.Offset 0 ; HostAnswer.Offset 8 ; HostAnswer.Offset 24 ]

        match hostAnswer shapes.[1] 0 with
        | HostAnswer.CannotMarshal _ -> ()
        | HostAnswer.Offset offset ->
            failwith $"real .NET placed a field of a struct holding an auto-layout struct at %d{offset}"

/// The same corpus, but through `Marshal.OffsetOf` itself as a guest calls it: the managed wrapper's
/// reflection lookup, the `FieldDesc` it hands the QCall, and the `ArgumentException` it raises,
/// message included.
///
/// Real .NET's answers are baked into the generated guest, which returns 0 if PawPrint agrees on
/// every field and otherwise one more than the index of the first field it disagrees on. The same
/// image also runs under real .NET, where it must return 0: that is what shows the baked-in table
/// describes the very types the guest declares.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
// Runs a guest under the interpreter; see AGENTS.md for why such fixtures are `Explicit`.
[<Category("Guest")>]
[<Explicit>]
module TestMarshalOffsetOfGuest =
    open MarshalOffsetOfCorpus

    /// Interpreting `Marshal.OffsetOf` costs tens of milliseconds a call, so the guest takes a
    /// prefix of the corpus rather than all of it.
    let private guestShapeCount : int = 80

    let private guestAnswers : (Shape * int * HostAnswer) list =
        let names =
            shapes
            |> List.truncate guestShapeCount
            |> List.map (fun shape -> shape.Name)
            |> Set.ofList

        hostAnswers |> List.filter (fun (shape, _, _) -> names.Contains shape.Name)

    let private csharpStringLiteral (s : string) : string =
        if s |> Seq.exists (fun c -> c = '"' || c = '\\' || Char.IsControl c) then
            failwith $"cannot embed %s{s} in a C# literal without escaping it"

        $"\"%s{s}\""

    let private guestSource : string =
        let checks =
            guestAnswers
            |> List.mapi (fun i (shape, index, answer) ->
                let call =
                    match answer with
                    | HostAnswer.Offset offset ->
                        $"Offset(typeof(%s{shape.Name}), \"%s{fieldName index}\", %d{offset})"
                    | HostAnswer.CannotMarshal message ->
                        $"Refused(typeof(%s{shape.Name}), \"%s{fieldName index}\", %s{csharpStringLiteral message})"

                $"        if (!%s{call}) return %d{i + 1};"
            )
            |> String.concat "\n"

        $"""
using System;
using System.Runtime.InteropServices;

namespace %s{corpusNamespace};

public static class OffsetOfSweep
{{
    static bool Offset(Type t, string field, int expected)
    {{
        return Marshal.OffsetOf(t, field) == (IntPtr)expected;
    }}

    static bool Refused(Type t, string field, string message)
    {{
        try
        {{
            Marshal.OffsetOf(t, field);
            return false;
        }}
        catch (ArgumentException e)
        {{
            return e.GetType() == typeof(ArgumentException) && e.Message == message && e.ParamName == null;
        }}
    }}

    public static int Main(string[] args)
    {{
%s{checks}
        return 0;
    }}
}}
"""

    let private describe (exitCode : int) : string =
        if exitCode = 0 then
            "agreed on every field"
        elif exitCode < 1 || exitCode > guestAnswers.Length then
            $"returned %d{exitCode}, which names no field"
        else
            let shape, index, answer = guestAnswers.[exitCode - 1]

            $"disagreed first on %s{fieldName index} of\n  %s{render shape}\n  where real .NET's answer is %A{answer}"

    [<Test>]
    let ``Marshal.OffsetOf in a guest agrees with real .NET`` () : unit =
        let image =
            Roslyn.compileAssembly
                "PawPrintTestAssembly"
                Microsoft.CodeAnalysis.OutputKind.ConsoleApplication
                []
                (sources @ [ guestSource ])

        let sourceName = "MarshalOffsetOfSweep"

        let messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ]

        use _loggerFactoryResource = loggerFactory

        let realResult, pawPrintExitCode =
            DifferentialOracle.alongsideInterpreted
                (fun () -> RealRuntime.executeWithRealRuntime [||] image)
                (fun () ->
                    use peImage = new MemoryStream (image)

                    let outcome =
                        try
                            Program.run
                                loggerFactory
                                (Some sourceName)
                                peImage
                                (HostConfig.Default (FrameworkUnderTest.runtimeDirs ()))
                        with _ ->
                            for message in messages () do
                                Console.Error.WriteLine $"{message}"

                            reraise ()

                    match outcome with
                    | RunOutcome.UndefinedValueObserved (_, _, observation) ->
                        failwith $"guest used an undefined value: %O{observation}"
                    | RunOutcome.NormalExit (terminalState, _)
                    | RunOutcome.ProcessExit (terminalState, _) -> terminalState.LatchedExitCode
                    | RunOutcome.GuestUnhandledException (_, _, exn) ->
                        failwith $"%s{sourceName}: guest threw an unhandled exception: %O{exn.ExceptionObject}"
                    | RunOutcome.Aborted (_, _, fatal) -> failwith $"%s{sourceName}: guest aborted: %O{fatal}"
                    | RunOutcome.SignalTerminated (_, signal) ->
                        failwith $"%s{sourceName}: guest was signalled: %O{signal}"
                )

        match realResult with
        | RealRuntimeResult.NormalExit 0 -> ()
        | other ->
            failwith
                $"real .NET did not agree with its own in-process answers (%O{other}); the baked-in table is not describing the guest's types"

        if pawPrintExitCode <> 0 then
            failwith $"PawPrint %s{describe pawPrintExitCode}"
