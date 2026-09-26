namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// An enum-typed field of a struct marshals as the enum's underlying primitive: CoreCLR's
/// `MarshalInfo` reads the field's element type through `PeekElemTypeNormalized`, so the enum's
/// own type, which is auto-layout and so has no native layout, is never consulted.
///
/// The oracle is real .NET. A corpus of generated struct shapes, mixing enums of every underlying
/// width that C# can declare with primitives and with nested structs that themselves hold enums,
/// is compiled once by Roslyn, read by PawPrint through the same metadata path a guest takes, and
/// loaded into this process, where `Marshal.SizeOf` and `Marshal.OffsetOf` give the answers
/// PawPrint's `CliValueType.TryComputeMarshalLayout` must reproduce. Loading the corpus in-process
/// is safe for the reason `TestBaseChainLayout` gives: it is a library with no entry point and no
/// static state, so it cannot touch a process-global.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMarshalEnumFieldLayout =

    let private loggerFactory = snd (LoggerFactory.makeTest ())

    let private corelibPath : string = typeof<obj>.Assembly.Location
    let private runtimeDir : string = Path.GetDirectoryName corelibPath
    let private corelib : DumpedAssembly = Assembly.readFile loggerFactory corelibPath
    let private bct : BaseClassTypes<DumpedAssembly> = BaseClassTypes.ofCorelib corelib

    let private corpusNamespace : string = "PawPrint.MarshalEnumLayout"

    /// The enum field types, as C# spells them. `Outer.NestedE` is nested in a non-generic type and
    /// `Holder<int>.GenericE` in a generic one, which makes the enum itself a generic
    /// instantiation.
    let private enumKinds : string list =
        [
            "E8"
            "ES8"
            "E16"
            "EU16"
            "E32"
            "EU32"
            "E64"
            "EU64"
            "F16"
            "F64"
            "Outer.NestedE"
            "Holder<int>.GenericE"
        ]

    let private primitiveKinds : string list =
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
            // Not blittable: a 4-byte BOOL, and (under these shapes' default Ansi `CharSet`) a
            // one-byte ANSI char.
            "bool"
            "char"
        ]

    /// Structs whose own fields are enums, so an enum is reached through a nested value type, and
    /// one whose UTF-16 `char` keeps it blittable.
    let private nestedKinds : string list =
        [ "NestE16" ; "NestE64" ; "NestExplicit" ; "NestCharUnicode" ]

    let private fixedCorpus : string =
        """
using System;
using System.Runtime.InteropServices;

namespace PawPrint.MarshalEnumLayout;

public enum E8 : byte { A = 1 }
public enum ES8 : sbyte { A = -1 }
public enum E16 : short { A = 1 }
public enum EU16 : ushort { A = 1 }
public enum E32 { A = 1 }
public enum EU32 : uint { A = 1 }
public enum E64 : long { A = 1 }
public enum EU64 : ulong { A = 1 }
[Flags] public enum F16 : ushort { X = 1, Y = 0x8000 }
[Flags] public enum F64 : long { X = 1, Y = 1L << 62 }

public static class Outer { public enum NestedE : ushort { A = 1 } }
public class Holder<T> { public enum GenericE : long { A = 1 } }

public struct NestE16 { public byte T; public E16 K; }
public struct NestE64 { public E8 T; public E64 K; }
[StructLayout(LayoutKind.Explicit)]
public struct NestExplicit { [FieldOffset(1)] public EU32 K; [FieldOffset(0)] public E8 T; }
[StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
public struct NestCharUnicode { public E8 T; public char K; }
"""

    [<RequireQualifiedAccess>]
    type private ShapeLayout =
        /// `[StructLayout(Sequential, Pack = pack, Size = size)]`; 0 omits the argument.
        | Sequential of pack : int * size : int
        /// `[StructLayout(Explicit, Size = size)]`, with each field's offset alongside it.
        | Explicit of size : int

    type private Shape =
        {
            Name : string
            Layout : ShapeLayout
            /// Field type as C# spells it, and its `[FieldOffset]` under explicit layout.
            Fields : (string * int option) list
        }

    let private genFieldKind : Gen<string> =
        Gen.frequency
            [
                4, Gen.elements enumKinds
                2, Gen.elements primitiveKinds
                1, Gen.elements nestedKinds
            ]

    let private genShape : Gen<ShapeLayout * (string * int option) list> =
        gen {
            let! count = Gen.choose (1, 6)
            let! kinds = Gen.listOfLength count genFieldKind
            let! size = Gen.elements [ 0 ; 0 ; 0 ; 1 ; 7 ; 13 ; 40 ]
            let! explicitLayout = Gen.frequency [ 3, Gen.constant false ; 1, Gen.constant true ]

            if explicitLayout then
                // Any offset is legal here: none of these fields is a reference, so overlap and
                // misalignment are both allowed, and CoreCLR takes the offsets as written.
                let! offsets = Gen.listOfLength count (Gen.choose (0, 24))
                return ShapeLayout.Explicit size, List.zip kinds (offsets |> List.map Some)
            else
                let! pack = Gen.elements [ 0 ; 0 ; 1 ; 2 ; 4 ; 8 ; 16 ]
                return ShapeLayout.Sequential (pack, size), kinds |> List.map (fun kind -> kind, None)
        }

    let private fieldName (index : int) : string = $"f%d{index}"

    let private render (shape : Shape) : string =
        let sizeArg (size : int) : string =
            if size = 0 then "" else $", Size = %d{size}"

        let attribute =
            match shape.Layout with
            | ShapeLayout.Sequential (pack, size) ->
                let packArg = if pack = 0 then "" else $", Pack = %d{pack}"
                $"[StructLayout(LayoutKind.Sequential%s{packArg}%s{sizeArg size})]"
            | ShapeLayout.Explicit size -> $"[StructLayout(LayoutKind.Explicit%s{sizeArg size})]"

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

    /// Shapes whose real .NET answers the vacuity guard below relies on, so that it does not
    /// depend on what the generator happened to draw.
    let private pinnedShapes : (ShapeLayout * (string * int option) list) list =
        [
            // Real .NET pads the enum to its own 8-byte alignment.
            ShapeLayout.Sequential (0, 0), [ "byte", None ; "E64", None ]
            ShapeLayout.Sequential (1, 0), [ "E8", None ; "EU32", None ; "EU16", None ]
            ShapeLayout.Explicit 0, [ "E64", Some 0 ; "E32", Some 0 ; "E8", Some 9 ; "NestE16", Some 11 ]
        ]

    /// A fixed, seeded sample rather than a fresh FsCheck run: the whole corpus has to be compiled
    /// before either runtime can look at any of it, and a fixed seed keeps the vacuity guard below
    /// deterministic. A failure names the offending shape's source, which is all it takes to
    /// reproduce it.
    let private shapes : Shape list =
        pinnedShapes
        @ List.ofArray (Gen.sampleWithSeed (Rnd 0x4D61727368616CUL) 10 400 genShape)
        |> List.mapi (fun i (layout, fields) ->
            {
                Name = $"Shape%d{i}"
                Layout = layout
                Fields = fields
            }
        )

    let private corpusBytes : byte array =
        let generated =
            shapes
            |> List.map render
            |> String.concat "\n"
            |> fun body -> $"using System.Runtime.InteropServices;\nnamespace %s{corpusNamespace};\n%s{body}\n"

        Roslyn.compileAssembly
            corpusNamespace
            Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
            []
            [ fixedCorpus ; generated ]

    let private corpusAssembly : DumpedAssembly =
        use stream = new MemoryStream (corpusBytes)
        AssemblyApi.read loggerFactory (Some $"%s{corpusNamespace}.dll") stream

    let private corpusRuntimeAssembly : System.Reflection.Assembly =
        System.Reflection.Assembly.Load corpusBytes

    let private hostType (shape : Shape) : System.Type =
        corpusRuntimeAssembly.GetType $"%s{corpusNamespace}.%s{shape.Name}"
        |> Option.ofObj
        |> Option.defaultWith (fun () -> failwith $"corpus does not contain %s{shape.Name}")

    let private baseState : IlMachineState =
        let dirs = ImmutableArray.CreateRange [ runtimeDir ]
        let state = IlMachineState.initial loggerFactory dirs corelib

        let state =
            { state with
                ConcreteTypes = Corelib.concretizeAll state._LoadedAssemblies bct AllConcreteTypes.Empty
            }

        state.WithLoadedAssembly corpusAssembly

    /// PawPrint's zero value of `shape`, built through the same entry point every allocation site
    /// uses, and the state that knows its types.
    let private pawPrintZero (shape : Shape) : CliValueType * IlMachineState =
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

        match IlMachineState.cliTypeZeroOfHandle state bct handle with
        | CliType.ValueType vt, state -> vt, state
        | other, _ -> failwith $"%s{shape.Name} should be a value type, but its zero is %O{other}"

    [<Test>]
    let ``Marshal layout of structs with enum fields agrees with real .NET`` () : unit =
        let failures =
            shapes
            |> List.choose (fun shape ->
                let t = hostType shape
                let hostSize = Marshal.SizeOf t

                let hostOffsets =
                    shape.Fields
                    |> List.mapi (fun i _ -> fieldName i, int (Marshal.OffsetOf (t, fieldName i)))

                let vt, state = pawPrintZero shape

                let actual =
                    match CliValueType.TryComputeMarshalLayout state.ConcreteTypes state._LoadedAssemblies bct vt with
                    | Result.Error err -> Result.Error err.Reason
                    | Result.Ok (size, placements) ->
                        Result.Ok (size.Size, placements |> List.map (fun p -> p.Field.Name, p.NativeOffset))

                if actual = Result.Ok (hostSize, hostOffsets) then
                    None
                else
                    Some $"%s{render shape}\n  real .NET: %A{(hostSize, hostOffsets)}\n  PawPrint:  %A{actual}"
            )

        match failures with
        | [] -> ()
        | _ ->
            let described = String.concat "\n" failures
            failwith $"%d{failures.Length} of %d{shapes.Length} shapes disagree:\n%s{described}"

    /// Whether real .NET treats `t` as blittable. Only the struct stub of a non-blittable type
    /// runs a Cleanup pass, and that zeroes the image, so `DestroyStructure` over a dirty buffer
    /// leaves it dirty exactly when the type is blittable.
    let private hostIsBlittable (t : System.Type) : bool =
        let size = Marshal.SizeOf t
        let dirty = 0xABuy
        let buffer = Marshal.AllocHGlobal size

        try
            for i in 0 .. size - 1 do
                Marshal.WriteByte (buffer, i, dirty)

            Marshal.DestroyStructure (buffer, t)
            [ 0 .. size - 1 ] |> List.forall (fun i -> Marshal.ReadByte (buffer, i) = dirty)
        finally
            Marshal.FreeHGlobal buffer

    [<Test>]
    let ``Blittability agrees with real .NET, and a blittable struct's managed size is its native size`` () : unit =
        // CoreCLR's `IsFieldBlittable` sees an enum field as its normalised primitive, so a shape
        // is blittable unless it holds a `bool` or an ANSI `char`. A blittable shape takes the
        // memmove arm of `MarshalNative_TryGetStructMarshalStub`, which reports PawPrint's
        // *managed* size as the native size, which is sound only while the two coincide; check
        // that against real .NET's native size too.
        let failures =
            shapes
            |> List.choose (fun shape ->
                let vt, state = pawPrintZero shape

                let blittable =
                    StructMarshalStub.isBlittableStruct
                        state.ConcreteTypes
                        state._LoadedAssemblies
                        bct
                        (CliType.ValueType vt)

                let managedSize = (CliType.SizeOf (CliType.ValueType vt)).Size
                let hostSize = Marshal.SizeOf (hostType shape)
                let hostBlittable = hostIsBlittable (hostType shape)

                if blittable = hostBlittable && (not blittable || managedSize = hostSize) then
                    None
                else
                    Some
                        $"%s{render shape}\n  blittable: %b{blittable} (real .NET: %b{hostBlittable}), PawPrint managed size %d{managedSize}, real .NET native size %d{hostSize}"
            )

        match failures with
        | [] -> ()
        | _ ->
            let described = String.concat "\n" failures
            failwith $"%d{failures.Length} of %d{shapes.Length} shapes disagree:\n%s{described}"

    [<Test>]
    let ``the corpus reaches every enum kind under every layout rule`` () : unit =
        // Vacuity guard: a sample that stopped drawing some kind, or never packed or overlaid
        // fields, would pass the comparisons above while testing much less than they claim.
        let kindsUnder (predicate : ShapeLayout -> bool) : Set<string> =
            shapes
            |> List.filter (fun shape -> predicate shape.Layout)
            |> List.collect (fun shape -> shape.Fields |> List.map fst)
            |> Set.ofList

        let expected = Set.ofList (enumKinds @ nestedKinds)

        let isExplicit (layout : ShapeLayout) : bool =
            match layout with
            | ShapeLayout.Explicit _ -> true
            | ShapeLayout.Sequential _ -> false

        let isPacked (layout : ShapeLayout) : bool =
            match layout with
            | ShapeLayout.Sequential (pack, _) -> pack = 1 || pack = 2
            | ShapeLayout.Explicit _ -> false

        let isNaturallyAligned (layout : ShapeLayout) : bool =
            match layout with
            | ShapeLayout.Sequential (pack, _) -> pack = 0 || pack >= 8
            | ShapeLayout.Explicit _ -> false

        Set.difference expected (kindsUnder isExplicit) |> shouldEqual Set.empty
        Set.difference expected (kindsUnder isPacked) |> shouldEqual Set.empty
        Set.difference expected (kindsUnder isNaturallyAligned) |> shouldEqual Set.empty

        // Real .NET must actually pad before an enum field, or nothing here distinguishes an
        // enum's own alignment from a byte-aligned guess.
        int (Marshal.OffsetOf (hostType shapes.[0], fieldName 1)) |> shouldEqual 8

        // Both blittability answers must occur, and among the blittable shapes some must hold a
        // UTF-16 `char`, or the blittability comparison is one-sided.
        let blittable, notBlittable =
            shapes |> List.partition (fun shape -> hostIsBlittable (hostType shape))

        notBlittable |> shouldNotEqual []

        blittable
        |> List.exists (fun shape -> shape.Fields |> List.exists (fun (kind, _) -> kind = "NestCharUnicode"))
        |> shouldEqual true
