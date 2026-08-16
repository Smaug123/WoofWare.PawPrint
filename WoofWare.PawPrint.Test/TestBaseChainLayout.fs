namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// CoreCLR lays a parent type out first and starts a derived type's own fields *after* the
/// parent's instance size (`HandleAutoLayout`, methodtablebuilder.cpp:8283-8296), so a base
/// field's offset is a property of the base alone: it must not depend on what derives from it
/// (issue #994).
///
/// The oracle here is real .NET itself. One Roslyn-compiled corpus is read by PawPrint *and*
/// loaded into this process, and the corpus carries its own probe methods that report each type's
/// absolute field offsets. Both byrefs a probe takes are interior pointers into the same object,
/// so they relocate together and the difference is stable; `RawData`'s single `byte` sits at
/// offset 0 of an object's field area, which is what turns a field reference into an absolute
/// offset. This is the same in-process trick `TestFieldIdAgreement` uses, and it is safe for the
/// same reason: a library corpus with no entry point and no static state cannot touch a
/// process-global, unlike a guest under test.
///
/// A guest program cannot reach any of this. Both routes to a reference type's first field are
/// shut: `Unsafe.As&lt;RawData&gt;(obj).Data` stops in `CliValueType.FindFieldById`, and
/// `Unsafe.ByteOffset` over a heap object's field byref stops in `Intrinsics.extractByteLocation`,
/// which handles every `ByrefRoot` case but not a heap-field root. So this fixture, not a
/// `sourcesPure` case, is what pins the behaviour.
[<TestFixture>]
module TestBaseChainLayout =

    let private loggerFactory = snd (LoggerFactory.makeTest ())

    let private corelibPath : string = typeof<obj>.Assembly.Location
    let private runtimeDir : string = Path.GetDirectoryName corelibPath
    let private corelib : DumpedAssembly = Assembly.readFile loggerFactory corelibPath
    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    /// Every field is named for the type that declares it, so a name-keyed lookup over the whole
    /// flattened storage list is unambiguous even where a base and a derived type would otherwise
    /// collide.
    ///
    /// The shapes are chosen one per rule, so a failure names the rule it broke rather than
    /// merely reporting that something moved:
    ///
    ///  * `D1`/`D2` -- the issue's headline: a base field must keep offset 0 whatever derives.
    ///  * `D3`/`GD` -- back-filling. When the parent's size is not 8-aligned, auto layout places
    ///    *small* derived fields into the gap before starting its largest-first buckets
    ///    (methodtablebuilder.cpp:8347-8428). `D3.C` lands at 1 and `GD.C` at 9.
    ///  * `D4` -- three levels, so a mid-chain size feeds the next level's start.
    ///  * `Mixed` -- single level, declared `Auto`: the row the old reference-type gate got wrong.
    ///  * `SD` -- sequential over a managed-sequential parent stays sequential and inherits its
    ///    alignment.
    ///  * `GD` -- declared `Sequential` but promoted to auto because its *parent* holds a
    ///    reference, which is the half of the promotion rule that reads through the chain.
    ///  * `XD` -- declared `Sequential` promoted to auto because its parent is explicit and so is
    ///    not managed-sequential. The only non-GC route into that arm.
    ///  * `ED` -- auto over an explicit parent.
    ///  * `D10` -- a value-class field, placed after every primitive bucket.
    ///  * `D11` -- an empty base contributes 0, not 1.
    ///  * `ZD` -- a zero-sized sequential base contributes 0, not the 1 byte it was padded to.
    ///  * `ExpD2`/`NExpD2` -- a *parent's* GC-ness routes an intermediate level onto the auto
    ///    path, where its size is not rounded. The pair differ only in whether the base holds a
    ///    reference, and real .NET puts the leaf at 9 and 16 respectively.
    ///  * `AD2` -- a sequential type inherits its parent's alignment requirement, which is the
    ///    only thing rounding `AD` up from 9 bytes to 16 and so putting `AD2_C` at 16.
    ///  * `PD2` -- a declared `ClassLayout.Size` mid-chain is a floor *relative to the parent*,
    ///    so `PD` is 20 bytes and `PD2.R` lands at 20.
    let private corpusSource : string =
        """
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace PawPrint.BaseChainLayout;

internal sealed class RawData { public byte Data; }

public class B1 { public int B1_I; }
public class D1 : B1 { public object D1_O; }

public class B2 { public int B2_I; }
public class D2 : B2 { public long D2_L; }

public class B3 { public byte B3_A; }
public class D3 : B3 { public long D3_L; public byte D3_C; public int D3_N; }

public class B4 { public byte B4_A; }
public class M4 : B4 { public int M4_I; }
public class D4 : M4 { public object D4_O; public byte D4_Z; }

public class B5 { public object B5_O; }
public class D5 : B5 { public int D5_I; public byte D5_C; }

public class Mixed { public byte Mx_B; public int Mx_I; public long Mx_L; public short Mx_S; }

[StructLayout(LayoutKind.Sequential)] public class SB { public byte SB_A; public int SB_I; }
[StructLayout(LayoutKind.Sequential)] public class SD : SB { public byte SD_C; public long SD_L; }

[StructLayout(LayoutKind.Sequential)] public class GB { public byte GB_A; public object GB_O; }
[StructLayout(LayoutKind.Sequential)] public class GD : GB { public byte GD_C; public int GD_I; }

[StructLayout(LayoutKind.Explicit)] public class EB { [FieldOffset(0)] public int EB_A; [FieldOffset(4)] public int EB_B; }
public class ED : EB { public byte ED_C; public long ED_L; }

[StructLayout(LayoutKind.Explicit)] public class XB { [FieldOffset(0)] public int XB_A; [FieldOffset(4)] public int XB_B; }
[StructLayout(LayoutKind.Sequential)] public class XD : XB { public byte XD_C; public long XD_L; }

public struct S3 { public byte S3_A; public byte S3_B; public byte S3_C; }
public class B10 { public byte B10_X; }
public class D10 : B10 { public S3 D10_V; public int D10_I; }

public class B11 { }
public class D11 : B11 { public byte D11_A; public long D11_L; }

// An explicit-layout type that both inherits bytes and declares its own offsets. PawPrint
// refuses this rather than guessing; see the refusal test below.
[StructLayout(LayoutKind.Sequential)] public class QB { public long QB_P; }
[StructLayout(LayoutKind.Explicit)] public class QD : QB { [FieldOffset(0)] public int QD_A; }

// A sequential base with no fields is bumped to 1 byte by `SetInstanceBytesSize`, but contributes
// 0 to a derived type -- "we need to remove the padding, but ONLY for inheritance situations"
// (`TryGetParentLayoutInfo`). Without that carve-out `ZD_A` would land at 1 and `ZD_B` at 8.
[StructLayout(LayoutKind.Sequential)] public class ZB { }
[StructLayout(LayoutKind.Sequential)] public class ZD : ZB { public byte ZD_A; public int ZD_B; }

// An explicit level with no offsets of its own, over a parent that holds a reference. The
// parent's GC-ness is what routes this level onto the auto path, where it keeps its parent's
// unrounded size of 9; treated as sequential it would round to 16 and push `ExpD2_Z` to 16.
// `NExpD2` is the control: same shape, reference-free parent, and there 16 is the right answer.
[StructLayout(LayoutKind.Sequential)] public class GcB { public byte GcB_A; public object GcB_O; }
[StructLayout(LayoutKind.Explicit)] public class ExpD : GcB { }
public class ExpD2 : ExpD { public byte ExpD2_Z; }

[StructLayout(LayoutKind.Sequential)] public class NoGcB { public byte NoGcB_A; public long NoGcB_L; }
[StructLayout(LayoutKind.Explicit)] public class NExpD : NoGcB { }
public class NExpD2 : NExpD { public byte NExpD2_Z; }

// The parent's alignment requirement (8, from the long) exceeds anything the children declare,
// so it is the only thing that can round their sizes up: without it `AD` would be 9 bytes and
// `AD2_C` would land at 9 rather than 16.
[StructLayout(LayoutKind.Sequential)] public class AB { public long AB_A; }
[StructLayout(LayoutKind.Sequential)] public class AD : AB { public byte AD_B; }
[StructLayout(LayoutKind.Sequential)] public class AD2 : AD { public byte AD2_C; }

[StructLayout(LayoutKind.Sequential)] public class PB { public long PB_P; }
[StructLayout(LayoutKind.Sequential, Size = 12)] public class PD : PB { public int PD_Q; }
[StructLayout(LayoutKind.Sequential)] public class PD2 : PD { public byte PD2_R; }

public static class LayoutProbe
{
    private static ref byte B<T>(ref T r) => ref Unsafe.As<T, byte>(ref r);

    private static int Off(object obj, ref byte field)
        => (int)Unsafe.ByteOffset(ref Unsafe.As<RawData>(obj).Data, ref field);

    public static int[] P_D1() { var x = new D1(); return new[] { Off(x, ref B(ref x.B1_I)), Off(x, ref B(ref x.D1_O)) }; }
    public static int[] P_D2() { var x = new D2(); return new[] { Off(x, ref B(ref x.B2_I)), Off(x, ref B(ref x.D2_L)) }; }
    public static int[] P_D3() { var x = new D3(); return new[] { Off(x, ref B(ref x.B3_A)), Off(x, ref B(ref x.D3_L)), Off(x, ref B(ref x.D3_C)), Off(x, ref B(ref x.D3_N)) }; }
    public static int[] P_D4() { var x = new D4(); return new[] { Off(x, ref B(ref x.B4_A)), Off(x, ref B(ref x.M4_I)), Off(x, ref B(ref x.D4_O)), Off(x, ref B(ref x.D4_Z)) }; }
    public static int[] P_D5() { var x = new D5(); return new[] { Off(x, ref B(ref x.B5_O)), Off(x, ref B(ref x.D5_I)), Off(x, ref B(ref x.D5_C)) }; }
    public static int[] P_Mixed() { var x = new Mixed(); return new[] { Off(x, ref B(ref x.Mx_B)), Off(x, ref B(ref x.Mx_I)), Off(x, ref B(ref x.Mx_L)), Off(x, ref B(ref x.Mx_S)) }; }
    public static int[] P_SD() { var x = new SD(); return new[] { Off(x, ref B(ref x.SB_A)), Off(x, ref B(ref x.SB_I)), Off(x, ref B(ref x.SD_C)), Off(x, ref B(ref x.SD_L)) }; }
    public static int[] P_GD() { var x = new GD(); return new[] { Off(x, ref B(ref x.GB_A)), Off(x, ref B(ref x.GB_O)), Off(x, ref B(ref x.GD_C)), Off(x, ref B(ref x.GD_I)) }; }
    public static int[] P_ED() { var x = new ED(); return new[] { Off(x, ref B(ref x.EB_A)), Off(x, ref B(ref x.EB_B)), Off(x, ref B(ref x.ED_C)), Off(x, ref B(ref x.ED_L)) }; }
    public static int[] P_XD() { var x = new XD(); return new[] { Off(x, ref B(ref x.XB_A)), Off(x, ref B(ref x.XB_B)), Off(x, ref B(ref x.XD_C)), Off(x, ref B(ref x.XD_L)) }; }
    public static int[] P_D10() { var x = new D10(); return new[] { Off(x, ref B(ref x.B10_X)), Off(x, ref B(ref x.D10_V)), Off(x, ref B(ref x.D10_I)) }; }
    public static int[] P_D11() { var x = new D11(); return new[] { Off(x, ref B(ref x.D11_A)), Off(x, ref B(ref x.D11_L)) }; }
    public static int[] P_ZD() { var x = new ZD(); return new[] { Off(x, ref B(ref x.ZD_A)), Off(x, ref B(ref x.ZD_B)) }; }
    public static int[] P_ExpD2() { var x = new ExpD2(); return new[] { Off(x, ref B(ref x.GcB_A)), Off(x, ref B(ref x.GcB_O)), Off(x, ref B(ref x.ExpD2_Z)) }; }
    public static int[] P_NExpD2() { var x = new NExpD2(); return new[] { Off(x, ref B(ref x.NoGcB_A)), Off(x, ref B(ref x.NoGcB_L)), Off(x, ref B(ref x.NExpD2_Z)) }; }
    public static int[] P_AD2() { var x = new AD2(); return new[] { Off(x, ref B(ref x.AB_A)), Off(x, ref B(ref x.AD_B)), Off(x, ref B(ref x.AD2_C)) }; }
    public static int[] P_PD2() { var x = new PD2(); return new[] { Off(x, ref B(ref x.PB_P)), Off(x, ref B(ref x.PD_Q)), Off(x, ref B(ref x.PD2_R)) }; }
}
"""

    /// Type name to the field names its probe reports, in the probe's order.
    let private shapes : (string * string list) list =
        [
            "D1", [ "B1_I" ; "D1_O" ]
            "D2", [ "B2_I" ; "D2_L" ]
            "D3", [ "B3_A" ; "D3_L" ; "D3_C" ; "D3_N" ]
            "D4", [ "B4_A" ; "M4_I" ; "D4_O" ; "D4_Z" ]
            "D5", [ "B5_O" ; "D5_I" ; "D5_C" ]
            "Mixed", [ "Mx_B" ; "Mx_I" ; "Mx_L" ; "Mx_S" ]
            "SD", [ "SB_A" ; "SB_I" ; "SD_C" ; "SD_L" ]
            "GD", [ "GB_A" ; "GB_O" ; "GD_C" ; "GD_I" ]
            "ED", [ "EB_A" ; "EB_B" ; "ED_C" ; "ED_L" ]
            "XD", [ "XB_A" ; "XB_B" ; "XD_C" ; "XD_L" ]
            "D10", [ "B10_X" ; "D10_V" ; "D10_I" ]
            "D11", [ "D11_A" ; "D11_L" ]
            "ZD", [ "ZD_A" ; "ZD_B" ]
            "ExpD2", [ "GcB_A" ; "GcB_O" ; "ExpD2_Z" ]
            "NExpD2", [ "NoGcB_A" ; "NoGcB_L" ; "NExpD2_Z" ]
            "AD2", [ "AB_A" ; "AD_B" ; "AD2_C" ]
            "PD2", [ "PB_P" ; "PD_Q" ; "PD2_R" ]
        ]

    let private corpusBytes : byte array =
        Roslyn.compileAssembly
            "PawPrint.BaseChainLayout"
            Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
            []
            [ corpusSource ]

    let private corpusAssembly : DumpedAssembly =
        use stream = new MemoryStream (corpusBytes)
        AssemblyApi.read loggerFactory (Some "PawPrint.BaseChainLayout.dll") stream

    let private corpusRuntimeAssembly : System.Reflection.Assembly =
        System.Reflection.Assembly.Load corpusBytes

    let private probeType : System.Type =
        corpusRuntimeAssembly.GetType "PawPrint.BaseChainLayout.LayoutProbe"
        |> Option.ofObj
        |> Option.defaultWith (fun () -> failwith "corpus does not contain LayoutProbe")

    /// Real .NET's absolute offsets for `typeName`, in the order `shapes` records.
    let private hostOffsets (typeName : string) : int list =
        let method_ =
            probeType.GetMethod ("P_" + typeName, BindingFlags.Public ||| BindingFlags.Static)
            |> Option.ofObj
            |> Option.defaultWith (fun () -> failwith $"corpus has no probe method P_%s{typeName}")

        let noReceiver : obj = null
        let boxed : obj = method_.Invoke (noReceiver, Array.empty<obj>)
        unbox<int[]> boxed |> List.ofArray

    let private baseState : IlMachineState =
        let dirs = ImmutableArray.CreateRange [ runtimeDir ]
        let state = IlMachineState.initial loggerFactory dirs corelib

        let state =
            { state with
                ConcreteTypes = Corelib.concretizeAll state._LoadedAssemblies bct AllConcreteTypes.Empty
            }

        state.WithLoadedAssembly corpusAssembly

    let private typeInfoNamed (name : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        corpusAssembly.TypeDefs
        |> Seq.map (fun kvp -> kvp.Value)
        |> Seq.filter (fun ti -> ti.Name = name)
        |> List.ofSeq
        |> function
            | [ ti ] -> ti
            | [] -> failwith $"corpus has no type named %s{name}"
            | _ -> failwith $"corpus has more than one type named %s{name}"

    /// PawPrint's absolute offsets for `typeName`, by field name.
    let private pawPrintOffsets (typeName : string) (fieldNames : string list) : int list =
        let typeInfo = typeInfoNamed typeName

        let state = baseState

        let state, handle =
            IlMachineTypeResolution.concretizeType
                loggerFactory
                bct
                state
                typeInfo.Assembly
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (typeInfo.Identity, System.Reflection.Metadata.SignatureTypeKind.Class))

        // Deliberately the same entry point every allocation site uses, so this tests the path
        // guests actually take rather than a re-implementation of it.
        let _, storage = IlMachineState.buildInstanceStorage loggerFactory bct state handle

        fieldNames
        |> List.map (fun name -> fst (CliValueType.GetFieldLayout name storage))

    let private shapeNames : string list = shapes |> List.map fst

    let private fieldsOf (typeName : string) : string list =
        shapes
        |> List.tryFind (fun (n, _) -> n = typeName)
        |> Option.map snd
        |> Option.defaultWith (fun () -> failwith $"no shape named %s{typeName}")

    [<TestCaseSource(nameof shapeNames)>]
    let ``field offsets agree with real .NET`` (typeName : string) : unit =
        let fieldNames = fieldsOf typeName
        let expected = hostOffsets typeName
        let actual = pawPrintOffsets typeName fieldNames

        List.zip fieldNames actual |> shouldEqual (List.zip fieldNames expected)

    [<Test>]
    let ``the corpus exercises the rules it claims to`` () : unit =
        // Vacuity guard. Every shape above is chosen for a rule, and a corpus that quietly stopped
        // covering one -- because a compiler change reordered something, or because a shape was
        // edited -- would still pass every case above while testing much less than it says.
        // These are properties of *real .NET's* answers, so they cannot be satisfied by whatever
        // PawPrint happens to do.
        let offsetsOf (name : string) : Map<string, int> =
            List.zip (fieldsOf name) (hostOffsets name) |> Map.ofList

        // Some type must place a *derived* field below an inherited one, or nothing here can tell
        // chain-aware layout from the flat kind.
        let d1 = offsetsOf "D1"
        d1.["B1_I"] |> shouldEqual 0
        (d1.["D1_O"] > d1.["B1_I"]) |> shouldEqual true

        // Some type must be bucketed rather than laid out in declaration order, or the declared-
        // `Auto` half of the rule is untested.
        let mixed = offsetsOf "Mixed"
        (mixed.["Mx_L"] < mixed.["Mx_B"]) |> shouldEqual true

        // Back-filling must actually fire somewhere: a derived field placed *below* the round-up
        // of its parent's size can only have got there that way.
        let d3 = offsetsOf "D3"
        d3.["D3_C"] |> shouldEqual 1
        let gd = offsetsOf "GD"
        gd.["GD_C"] |> shouldEqual 9

        // A three-level chain, so a mid-chain instance size feeds the next level's start.
        let d4 = offsetsOf "D4"
        d4.["M4_I"] |> shouldEqual 4
        (d4.["D4_O"] >= 8) |> shouldEqual true

        // The zero-sized carve-out must be doing something: without it the first derived field
        // would be pushed off offset 0 by the base's padding byte.
        (offsetsOf "ZD").["ZD_A"] |> shouldEqual 0

        // The parent-GC term must be doing something: the pair differ only in whether the base
        // holds a reference, so any answer that ignores it must give them the same leaf offset.
        (offsetsOf "ExpD2").["ExpD2_Z"]
        |> shouldNotEqual (offsetsOf "NExpD2").["NExpD2_Z"]

        // The parent-alignment term must be doing something: `AD`'s own field is one byte, so
        // only its parent's 8-byte requirement can put `AD2_C` past 9.
        let ad2 = offsetsOf "AD2"
        (ad2.["AD2_C"] > ad2.["AD_B"] + 1) |> shouldEqual true

        // A declared `Size` mid-chain must push the next level past its own last field.
        let pd2 = offsetsOf "PD2"
        (pd2.["PD2_R"] > pd2.["PD_Q"] + 4) |> shouldEqual true

    [<Test>]
    let ``an explicit-layout type that inherits bytes is refused, not guessed`` () : unit =
        // Real .NET biases such a type's declared offsets by *twice* the parent's instance size
        // (measured: parent 4 -> first field at 8, parent 8 -> 16, parent 16 -> 32), which looks
        // like `cbAdjustedParentLayoutSize` being applied once in `ReadOffsetsForExplicitLayout`
        // and again in `ValidateExplicitLayout`'s fixup. Rather than reproduce a suspected
        // upstream bug from a handful of measurements, PawPrint refuses.
        //
        // This is a refusal test, so it has to prove the refusal fired *for its own reason* and
        // not because something earlier went wrong: the message names the shape.
        let exn =
            Assert.Throws (fun () -> pawPrintOffsets "QD" [ "QD_A" ] |> ignore<int list>)

        exn.Message |> shouldContainText "refusing to lay out explicit-layout type"
        exn.Message |> shouldContainText "the parent's instance size"
        exn.Message |> shouldContainText "inherits 8 bytes"

        // The control: the same declared offsets with nothing inherited are laid out, so the
        // refusal is about the *inheritance*, not about explicit layout in general.
        pawPrintOffsets "EB" [ "EB_A" ; "EB_B" ] |> shouldEqual [ 0 ; 4 ]
