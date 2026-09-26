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

/// A seeded corpus of generated chains of classes, each class deriving from the one before, and
/// what real .NET's `Marshal.SizeOf` and `Marshal.OffsetOf` say about every class and field in
/// them: the oracle both fixtures below hold PawPrint to.
///
/// As in `MarshalOffsetOfCorpus`, the corpus is a library with no entry point and no static state,
/// so loading it into this process to ask the host's own `Marshal` cannot touch a process-global.
module internal MarshalLayoutClassCorpus =

    let corpusNamespace : string = "PawPrint.MarshalLayoutClass"

    /// Field types, as C# spells them. None is a reference, which PawPrint does not marshal.
    let fieldKinds : string list =
        [
            "byte"
            "short"
            "ushort"
            "int"
            "long"
            "float"
            "double"
            "nint"
            "E16"
            // A four-byte BOOL, and a `char` whose width the declaring class's `CharSet` decides:
            // either makes the class not blittable, so its answers come from its native layout.
            "bool"
            "char"
            "decimal"
            "DateTime"
            "Int128"
            "NestPacked"
            "NestSized"
            // A struct CoreCLR lays out as a field but cannot marshal on its own.
            "NestIllegalWide"
            // A field CoreCLR refuses, which makes the class declaring it, and every class
            // deriving from that, unmarshalable.
            "NestAuto"
        ]

    let private fixedCorpus : string =
        """
using System;
using System.Runtime.InteropServices;

namespace PawPrint.MarshalLayoutClass;

public enum E16 : short { A = 1 }

[StructLayout(LayoutKind.Sequential, Pack = 1)]
public struct NestPacked { public byte T; public long K; }
[StructLayout(LayoutKind.Sequential, Size = 13)]
public struct NestSized { public byte T; }
public struct NestIllegalWide { public long L; [MarshalAs(UnmanagedType.VariantBool)] public bool K; public byte T; }
[StructLayout(LayoutKind.Auto)]
public struct NestAuto { public int A; public int B; }
"""

    [<RequireQualifiedAccess>]
    type LevelLayout =
        /// `[StructLayout(Sequential, Pack = pack, Size = size)]`; 0 omits the argument.
        | Sequential of pack : int * size : int
        /// `[StructLayout(Explicit, Size = size)]`, with each field's offset alongside it.
        | Explicit of size : int
        /// No `[StructLayout]` at all, which for a class is auto layout.
        | Auto

    /// One class of a chain: it derives from the level before it, or from `object` if first.
    type Level =
        {
            Layout : LevelLayout
            CharSet : CharSet option
            /// Field type as C# spells it, and its `[FieldOffset]` under explicit layout.
            Fields : (string * int option) list
        }

    type Chain =
        {
            Name : string
            /// Base first.
            Levels : Level list
        }

    let className (chain : Chain) (level : int) : string = $"%s{chain.Name}_%d{level}"

    /// Named for the level that declares it, so that a derived class's field never hides one it
    /// inherits, and `Type.GetField` on the most derived class finds every field of the chain.
    let fieldName (level : int) (index : int) : string = $"l%d{level}f%d{index}"

    let private genCharSet : Gen<CharSet option> =
        Gen.elements [ None ; None ; Some CharSet.Ansi ; Some CharSet.Unicode ]

    let private genLayout : Gen<LevelLayout> =
        gen {
            let! size = Gen.elements [ 0 ; 0 ; 0 ; 1 ; 3 ; 13 ]

            let! layout =
                Gen.frequency
                    [
                        6, Gen.constant "sequential"
                        2, Gen.constant "explicit"
                        1, Gen.constant "auto"
                    ]

            match layout with
            | "explicit" -> return LevelLayout.Explicit size
            | "auto" -> return LevelLayout.Auto
            | _ ->
                let! pack = Gen.elements [ 0 ; 0 ; 1 ; 2 ; 4 ; 16 ]
                return LevelLayout.Sequential (pack, size)
        }

    let private genLevel (layout : LevelLayout) : Gen<Level> =
        gen {
            // An empty level is common, because a base with no fields contributes nothing to the
            // classes deriving from it, although it is one byte on its own.
            let! count = Gen.frequency [ 1, Gen.constant 0 ; 4, Gen.choose (1, 3) ]
            let! kinds = Gen.listOfLength count (Gen.elements fieldKinds)
            let! charSet = genCharSet

            match layout with
            | LevelLayout.Explicit _ ->
                // No field is a reference, so overlap and misalignment are both legal.
                let! offsets = Gen.listOfLength count (Gen.choose (0, 16))

                return
                    {
                        Layout = layout
                        CharSet = charSet
                        Fields = List.zip kinds (offsets |> List.map Some)
                    }
            | LevelLayout.Sequential _
            | LevelLayout.Auto ->
                return
                    {
                        Layout = layout
                        CharSet = charSet
                        Fields = kinds |> List.map (fun kind -> kind, None)
                    }
        }

    let private genLevels : Gen<Level list> =
        gen {
            let! depth = Gen.choose (1, 3)
            let! layouts = Gen.listOfLength depth genLayout

            // A class with layout whose base has none is refused by CoreCLR's type loader
            // (`HasLayoutMetadata`, methodtablebuilder.cpp), which would make the whole corpus
            // unloadable; so once a level is auto, so is every level deriving from it.
            let layouts =
                layouts
                |> List.scan
                    (fun (seenAuto, _) layout ->
                        let seenAuto = seenAuto || layout = LevelLayout.Auto
                        seenAuto, (if seenAuto then LevelLayout.Auto else layout)
                    )
                    (false, LevelLayout.Auto)
                |> List.tail
                |> List.map snd

            let! levels = layouts |> List.map genLevel |> Gen.sequenceToList
            return levels
        }

    let render (chain : Chain) : string =
        let sizeArg (size : int) : string =
            if size = 0 then "" else $", Size = %d{size}"

        chain.Levels
        |> List.mapi (fun level spec ->
            let charSetArg =
                match spec.CharSet with
                | None -> ""
                | Some charSet -> $", CharSet = CharSet.%O{charSet}"

            let attribute =
                match spec.Layout with
                | LevelLayout.Sequential (pack, size) ->
                    let packArg = if pack = 0 then "" else $", Pack = %d{pack}"
                    $"[StructLayout(LayoutKind.Sequential%s{packArg}%s{sizeArg size}%s{charSetArg})] "
                | LevelLayout.Explicit size -> $"[StructLayout(LayoutKind.Explicit%s{sizeArg size}%s{charSetArg})] "
                | LevelLayout.Auto -> ""

            let baseClause =
                if level = 0 then
                    ""
                else
                    $" : %s{className chain (level - 1)}"

            let fields =
                spec.Fields
                |> List.mapi (fun i (kind, offset) ->
                    let offsetAttr =
                        match offset with
                        | None -> ""
                        | Some o -> $"[FieldOffset(%d{o})] "

                    $"%s{offsetAttr}public %s{kind} %s{fieldName level i};"
                )
                |> String.concat " "

            $"%s{attribute}public class %s{className chain level}%s{baseClause} {{ %s{fields} }}"
        )
        |> String.concat "\n"

    /// Chains whose real .NET answers the vacuity guards rely on, so that those do not depend on
    /// what the generator happened to draw.
    let private pinnedChains : Level list list =
        let seq (fields : string list) : Level =
            {
                Layout = LevelLayout.Sequential (0, 0)
                CharSet = None
                Fields = fields |> List.map (fun kind -> kind, None)
            }

        [
            // An empty base is one byte on its own, but contributes nothing to a derived class.
            [ seq [] ; seq [ "byte" ; "int" ] ]
            // A base's declared `Size` counts in full, and a derived class's `Size` is added to it.
            [
                {
                    Layout = LevelLayout.Sequential (0, 12)
                    CharSet = None
                    Fields = [ "long", None ]
                }
                {
                    Layout = LevelLayout.Sequential (0, 3)
                    CharSet = None
                    Fields = [ "byte", None ]
                }
            ]
            // A base's alignment rounds up a derived class with no field that wide.
            [ seq [ "long" ] ; seq [ "byte" ] ]
            // `bool` makes the derived class non-blittable: a four-byte BOOL after the base.
            [ seq [ "byte" ; "short" ] ; seq [ "bool" ; "byte" ] ]
            // An illegal field in the base makes every class deriving from it unmarshalable.
            [ seq [ "NestAuto" ] ; seq [ "int" ] ]
            // A class with no layout of its own still answers for the fields it inherits.
            [
                seq [ "int" ; "byte" ]
                {
                    Layout = LevelLayout.Auto
                    CharSet = None
                    Fields = [ "long", None ]
                }
            ]
            // An explicit class over a base, which PawPrint refuses.
            [
                seq [ "int" ]
                {
                    Layout = LevelLayout.Explicit 0
                    CharSet = None
                    Fields = [ "short", Some 2 ]
                }
            ]
        ]

    /// A fixed, seeded sample rather than a fresh FsCheck run: the whole corpus has to be compiled
    /// before either runtime can look at any of it. A failure names the offending chain's source,
    /// which is all it takes to reproduce it.
    let chains : Chain list =
        pinnedChains
        @ List.ofArray (Gen.sampleWithSeed (Rnd 0x436C61737343686EUL) 10 200 genLevels)
        |> List.mapi (fun i levels ->
            {
                Name = $"Chain%d{i}"
                Levels = levels
            }
        )

    /// The corpus's C# source: the fixed declarations, and the classes of every chain.
    let sources : string list =
        let generated =
            chains
            |> List.map render
            |> String.concat "\n"
            |> fun body ->
                $"using System;\nusing System.Runtime.InteropServices;\nnamespace %s{corpusNamespace};\n%s{body}\n"

        [ fixedCorpus ; generated ]

    let corpusBytes : byte array =
        Roslyn.compileAssembly corpusNamespace Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary [] sources

    let private corpusRuntimeAssembly : System.Reflection.Assembly =
        System.Reflection.Assembly.Load corpusBytes

    let hostType (chain : Chain) (level : int) : Type =
        let name = className chain level

        corpusRuntimeAssembly.GetType $"%s{corpusNamespace}.%s{name}"
        |> Option.ofObj
        |> Option.defaultWith (fun () -> failwith $"corpus does not contain %s{name}")

    /// What real .NET's `Marshal.SizeOf` or `Marshal.OffsetOf` does.
    [<RequireQualifiedAccess>]
    type HostAnswer =
        | Value of int
        /// `ArgumentException` with this message: CoreCLR's `IDS_CANNOT_MARSHAL`.
        | CannotMarshal of message : string

    let private ask (f : unit -> int) : HostAnswer =
        try
            HostAnswer.Value (f ())
        with :? ArgumentException as e when e.GetType () = typeof<ArgumentException> ->
            HostAnswer.CannotMarshal e.Message

    let hostSize (chain : Chain) (level : int) : HostAnswer =
        ask (fun () -> Marshal.SizeOf (hostType chain level))

    /// `Marshal.OffsetOf(t, f)` for the class `t` at `askedOf`, and a field `f` which that class or
    /// a base of it declares.
    let hostOffset (chain : Chain) (askedOf : int) (declaredBy : int) (index : int) : HostAnswer =
        ask (fun () -> int (Marshal.OffsetOf (hostType chain askedOf, fieldName declaredBy index)))

    let private isExplicit (spec : Level) : bool =
        match spec.Layout with
        | LevelLayout.Explicit _ -> true
        | LevelLayout.Sequential _
        | LevelLayout.Auto -> false

    /// The classes PawPrint refuses to answer for at all, rather than risk a wrong answer: those
    /// whose own base chain (below `object`) is two classes or more, of which any is explicit.
    ///
    /// For a blittable class, CoreCLR answers from its *managed* layout rather than its native one,
    /// and in these shapes the two differ: an explicit class's declared offsets are biased by twice
    /// its base's size, and a sequential class over an explicit base is laid out automatically.
    /// PawPrint computes only the native layout, and does not decide blittability.
    ///
    /// A class with no layout is refused by CoreCLR too, so it is not one of these.
    let refusedByPawPrint (chain : Chain) (level : int) : bool =
        let levels = List.truncate (level + 1) chain.Levels

        (List.last levels).Layout <> LevelLayout.Auto
        && levels.Length >= 2
        && List.exists isExplicit levels

    /// The classes whose *size* PawPrint refuses: those above, and a lone explicit class, whose
    /// managed size, if it is blittable, is the extent of its fields rather than its native size.
    let sizeRefusedByPawPrint (chain : Chain) (level : int) : bool =
        let levels = List.truncate (level + 1) chain.Levels
        (List.last levels).Layout <> LevelLayout.Auto && List.exists isExplicit levels

    /// Every class of every chain, with real .NET's `SizeOf` of it and `OffsetOf` of each field it
    /// declares itself.
    let hostAnswers : (Chain * int * HostAnswer * HostAnswer list) list =
        [
            for chain in chains do
                for level in 0 .. chain.Levels.Length - 1 do
                    let offsets =
                        chain.Levels.[level].Fields
                        |> List.mapi (fun index _ -> hostOffset chain level level index)

                    yield chain, level, hostSize chain level, offsets
        ]

/// `NativeMarshal.classMarshalLayout`, the class half of what `MarshalNative_SizeOfHelper` and
/// `MarshalNative_OffsetOf` answer with, run directly over the whole corpus and compared with real
/// .NET class by class.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMarshalLayoutClass =
    open MarshalLayoutClassCorpus

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

    /// PawPrint's size of the class at `level`, and the offset of each field it declares itself.
    let private pawPrintAnswers
        (chain : Chain)
        (level : int)
        : Result<int, MarshalSizeError> * Result<int, MarshalSizeError> list
        =
        let typeInfo =
            corpusAssembly.TypeDefs
            |> Seq.map (fun kvp -> kvp.Value)
            |> Seq.filter (fun ti -> ti.Name = className chain level)
            |> Seq.exactlyOne

        let state, handle =
            IlMachineTypeResolution.concretizeType
                loggerFactory
                bct
                baseState
                typeInfo.AssemblyFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (typeInfo.Identity, System.Reflection.Metadata.SignatureTypeKind.Class))

        let _, layout = NativeMarshal.classMarshalLayout loggerFactory bct state handle

        let offsets =
            chain.Levels.[level].Fields
            |> List.mapi (fun index _ ->
                let field =
                    typeInfo.Fields
                    |> List.filter (fun f -> f.Name = fieldName level index)
                    |> List.exactlyOne

                layout
                |> Result.map (fun layout ->
                    CliType.MarshalFieldOffset
                        handle
                        (ComparableFieldDefinitionHandle.Make field.Handle)
                        layout.Placements
                )
            )

        layout |> Result.bind _.Size |> Result.map _.Size, offsets

    let private agrees (host : HostAnswer) (pawPrint : Result<int, MarshalSizeError>) : bool =
        match host, pawPrint with
        | HostAnswer.Value h, Result.Ok p -> h = p
        | HostAnswer.CannotMarshal _, Result.Error (MarshalSizeError.NotMarshalable _) -> true
        | _, _ -> false

    let private isNotImplemented (pawPrint : Result<int, MarshalSizeError>) : bool =
        match pawPrint with
        | Result.Error (MarshalSizeError.NotImplemented _) -> true
        | Result.Ok _
        | Result.Error (MarshalSizeError.NotMarshalable _) -> false

    [<Test>]
    let ``Marshal sizes and field offsets of classes agree with real .NET`` () : unit =
        let failures =
            hostAnswers
            |> List.choose (fun (chain, level, hostSize, hostOffsets) ->
                let size, offsets = pawPrintAnswers chain level

                let ok =
                    if refusedByPawPrint chain level then
                        isNotImplemented size && List.forall isNotImplemented offsets
                    elif sizeRefusedByPawPrint chain level then
                        // Unless a field makes the class unmarshalable, which decides the size too.
                        (isNotImplemented size || agrees hostSize size)
                        && (
                            match size with
                            | Result.Ok _ -> false
                            | Result.Error _ -> true
                        )
                        && List.forall2 agrees hostOffsets offsets
                    else
                        agrees hostSize size && List.forall2 agrees hostOffsets offsets

                if ok then
                    None
                else
                    Some
                        $"%s{className chain level} of\n%s{render chain}\n  real .NET: size %A{hostSize}, offsets %A{hostOffsets}\n  PawPrint:  size %A{size}, offsets %A{offsets}"
            )

        match failures with
        | [] -> ()
        | _ ->
            let described = String.concat "\n" failures
            failwith $"%d{failures.Length} of %d{hostAnswers.Length} classes disagree:\n%s{described}"

    [<Test>]
    let ``the class corpus reaches every field kind, and real .NET both answers and refuses`` () : unit =
        // Vacuity guard: a sample that stopped drawing some kind, or whose every class real .NET
        // refused, or whose every answer PawPrint refused, would pass the comparison above while
        // testing much less than it claims.
        let answeredDerived =
            hostAnswers
            |> List.filter (fun (chain, level, _, _) -> level >= 1 && not (refusedByPawPrint chain level))

        answeredDerived
        |> List.collect (fun (chain, level, _, _) -> chain.Levels.[level].Fields |> List.map fst)
        |> Set.ofList
        |> shouldEqual (Set.ofList fieldKinds)

        let values, refusals =
            answeredDerived
            |> List.collect (fun (_, _, size, offsets) -> size :: offsets)
            |> List.partition (fun answer ->
                match answer with
                | HostAnswer.Value _ -> true
                | HostAnswer.CannotMarshal _ -> false
            )

        values.Length |> shouldBeGreaterThan 150
        refusals.Length |> shouldBeGreaterThan 30

        hostAnswers
        |> List.filter (fun (chain, level, _, _) -> refusedByPawPrint chain level)
        |> List.length
        |> shouldBeGreaterThan 10

        let pinned (index : int) : Chain = chains.[index]

        // The empty base: one byte on its own, nothing to the class deriving from it.
        hostSize (pinned 0) 0 |> shouldEqual (HostAnswer.Value 1)
        hostOffset (pinned 0) 1 1 0 |> shouldEqual (HostAnswer.Value 0)
        hostSize (pinned 0) 1 |> shouldEqual (HostAnswer.Value 8)
        // `Size = 12` over a `long`, then `Size = 3` over a `byte` after it: 12 + 3.
        hostSize (pinned 1) 0 |> shouldEqual (HostAnswer.Value 12)
        hostOffset (pinned 1) 1 1 0 |> shouldEqual (HostAnswer.Value 12)
        hostSize (pinned 1) 1 |> shouldEqual (HostAnswer.Value 15)
        // The base's 8-byte alignment rounds the derived class from 9 bytes to 16.
        hostSize (pinned 2) 1 |> shouldEqual (HostAnswer.Value 16)
        // The BOOL is four bytes, four-aligned after the base's four.
        hostOffset (pinned 3) 1 1 0 |> shouldEqual (HostAnswer.Value 4)
        hostOffset (pinned 3) 1 1 1 |> shouldEqual (HostAnswer.Value 8)

        match hostSize (pinned 4) 1, hostOffset (pinned 4) 1 1 0 with
        | HostAnswer.CannotMarshal _, HostAnswer.CannotMarshal _ -> ()
        | size, offset ->
            failwith $"real .NET answered for a class deriving from an unmarshalable one: %A{size}, %A{offset}"

        // The auto-layout class answers for the field it inherits, and refuses its own.
        hostOffset (pinned 5) 1 0 1 |> shouldEqual (HostAnswer.Value 4)

        match hostOffset (pinned 5) 1 1 0 with
        | HostAnswer.CannotMarshal _ -> ()
        | HostAnswer.Value offset -> failwith $"real .NET placed a field of an auto-layout class at %d{offset}"

        // An explicit class's offsets over a blittable base are biased by twice the base's size,
        // which is why PawPrint refuses the shape: its native layout would say 6.
        refusedByPawPrint (pinned 6) 1 |> shouldEqual true
        hostOffset (pinned 6) 1 1 0 |> shouldEqual (HostAnswer.Value 10)

/// The same corpus, but through `Marshal.SizeOf` and `Marshal.OffsetOf` themselves as a guest calls
/// them: the managed wrappers, the QCalls, the declaring-type redirection `OffsetOf` makes for an
/// inherited field, and the `ArgumentException`, message included.
///
/// As in `TestMarshalOffsetOfGuest`, real .NET's answers are baked into the generated guest, which
/// returns 0 if PawPrint agrees on every question and otherwise one more than the index of the
/// first it disagrees on; the same image also runs under real .NET, where it must return 0.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
// Runs a guest under the interpreter; see AGENTS.md for why such fixtures are `Explicit`.
[<Category("Guest")>]
[<Explicit>]
module TestMarshalLayoutClassGuest =
    open MarshalLayoutClassCorpus

    /// Interpreting `Marshal.OffsetOf` costs tens of milliseconds a call, so the guest takes a
    /// prefix of the corpus rather than all of it.
    let private guestChainCount : int = 60

    [<RequireQualifiedAccess>]
    type private Question =
        | SizeOf of chain : Chain * level : int
        /// `OffsetOf` asked of the most derived class of the chain, for a field of any level.
        | OffsetOf of chain : Chain * declaredBy : int * index : int

    let private questions : (Question * HostAnswer) list =
        [
            for chain in List.truncate guestChainCount chains do
                let last = chain.Levels.Length - 1

                for level in 0..last do
                    if not (refusedByPawPrint chain level) then
                        if not (sizeRefusedByPawPrint chain level) then
                            yield Question.SizeOf (chain, level), hostSize chain level

                        for index in 0 .. chain.Levels.[level].Fields.Length - 1 do
                            yield Question.OffsetOf (chain, level, index), hostOffset chain last level index
        ]

    let private csharpStringLiteral (s : string) : string =
        if s |> Seq.exists (fun c -> c = '"' || c = '\\' || Char.IsControl c) then
            failwith $"cannot embed %s{s} in a C# literal without escaping it"

        $"\"%s{s}\""

    let private guestSource : string =
        let checks =
            questions
            |> List.mapi (fun i (question, answer) ->
                let call =
                    match question, answer with
                    | Question.SizeOf (chain, level), HostAnswer.Value size ->
                        $"Size(typeof(%s{className chain level}), %d{size})"
                    | Question.SizeOf (chain, level), HostAnswer.CannotMarshal message ->
                        $"SizeRefused(typeof(%s{className chain level}), %s{csharpStringLiteral message})"
                    | Question.OffsetOf (chain, declaredBy, index), HostAnswer.Value offset ->
                        let asked = className chain (chain.Levels.Length - 1)
                        $"Offset(typeof(%s{asked}), \"%s{fieldName declaredBy index}\", %d{offset})"
                    | Question.OffsetOf (chain, declaredBy, index), HostAnswer.CannotMarshal message ->
                        let asked = className chain (chain.Levels.Length - 1)

                        $"OffsetRefused(typeof(%s{asked}), \"%s{fieldName declaredBy index}\", %s{csharpStringLiteral message})"

                $"        if (!%s{call}) return %d{i + 1};"
            )
            |> String.concat "\n"

        $"""
using System;
using System.Runtime.InteropServices;

namespace %s{corpusNamespace};

public static class LayoutClassSweep
{{
    static bool Size(Type t, int expected)
    {{
        return Marshal.SizeOf(t) == expected;
    }}

    static bool Offset(Type t, string field, int expected)
    {{
        return Marshal.OffsetOf(t, field) == (IntPtr)expected;
    }}

    static bool Refused(ArgumentException e, string message)
    {{
        return e.GetType() == typeof(ArgumentException) && e.Message == message && e.ParamName == null;
    }}

    static bool SizeRefused(Type t, string message)
    {{
        try
        {{
            Marshal.SizeOf(t);
            return false;
        }}
        catch (ArgumentException e)
        {{
            return Refused(e, message);
        }}
    }}

    static bool OffsetRefused(Type t, string field, string message)
    {{
        try
        {{
            Marshal.OffsetOf(t, field);
            return false;
        }}
        catch (ArgumentException e)
        {{
            return Refused(e, message);
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
            "agreed on every question"
        elif exitCode < 1 || exitCode > questions.Length then
            $"returned %d{exitCode}, which names no question"
        else
            let question, answer = questions.[exitCode - 1]

            let chain =
                match question with
                | Question.SizeOf (chain, _)
                | Question.OffsetOf (chain, _, _) -> chain

            $"disagreed first on %A{question} of\n%s{render chain}\n  where real .NET's answer is %A{answer}"

    [<Test>]
    let ``Marshal.SizeOf and OffsetOf of classes in a guest agree with real .NET`` () : unit =
        let image =
            Roslyn.compileAssembly
                "PawPrintTestAssembly"
                Microsoft.CodeAnalysis.OutputKind.ConsoleApplication
                []
                (sources @ [ guestSource ])

        let sourceName = "MarshalLayoutClassSweep"

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
