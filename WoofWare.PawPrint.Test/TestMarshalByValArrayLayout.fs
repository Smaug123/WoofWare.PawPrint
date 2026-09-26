namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The native form of a `[MarshalAs(UnmanagedType.ByValArray)]` field's elements is chosen by the
/// field's *managed* element type, and `ArraySubType` is consulted only by the few element types
/// with more than one native form: CoreCLR's `ArrayMarshalInfo::InitElementInfo` (mlinfo.cpp).
/// So `[MarshalAs(ByValArray, SizeConst = 4, ArraySubType = I1)] int[]` is sixteen bytes, not four.
///
/// The oracle is real .NET, as in `TestMarshalEnumFieldLayout`: a corpus pairing every element kind
/// with every `ArraySubType` is compiled once by Roslyn, read by PawPrint through the same metadata
/// path a guest takes, and loaded into this process, where `Marshal.SizeOf` and
/// `Marshal.OffsetOf` give the answers `CliValueType.TryComputeMarshalLayout` must reproduce.
/// Loading the corpus in-process is safe for the reason `TestBaseChainLayout` gives: it is a
/// library with no entry point and no static state, so it cannot touch a process-global.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMarshalByValArrayLayout =

    let private loggerFactory = snd (LoggerFactory.makeTest ())

    let private corelibPath : string = typeof<obj>.Assembly.Location
    let private runtimeDir : string = Path.GetDirectoryName corelibPath
    let private corelib : DumpedAssembly = Assembly.readFile loggerFactory corelibPath
    let private bct : BaseClassTypes<DumpedAssembly> = BaseClassTypes.ofCorelib corelib

    let private corpusNamespace : string = "PawPrint.MarshalByValArrayLayout"

    /// What PawPrint must say about an element kind.
    [<RequireQualifiedAccess>]
    type private Expectation =
        /// Real .NET's answer: the same size and offsets, or `NotMarshalable` where it throws.
        | AgreeWithHost
        /// `NotImplemented`, whatever real .NET says: the element's native size is one PawPrint
        /// cannot compute from the element type alone.
        | Refused

    /// Array element types, as C# spells them.
    let private elementKinds : (string * Expectation) list =
        [
            // Every primitive ignores `ArraySubType` and keeps its own width.
            "byte", Expectation.AgreeWithHost
            "sbyte", Expectation.AgreeWithHost
            "short", Expectation.AgreeWithHost
            "ushort", Expectation.AgreeWithHost
            "int", Expectation.AgreeWithHost
            "uint", Expectation.AgreeWithHost
            "long", Expectation.AgreeWithHost
            "ulong", Expectation.AgreeWithHost
            "float", Expectation.AgreeWithHost
            "double", Expectation.AgreeWithHost
            "nint", Expectation.AgreeWithHost
            "nuint", Expectation.AgreeWithHost
            // As does an enum, as its underlying integer.
            "E8", Expectation.AgreeWithHost
            "E16", Expectation.AgreeWithHost
            "EU32", Expectation.AgreeWithHost
            "E64", Expectation.AgreeWithHost
            // The element types that do consult `ArraySubType`.
            "bool", Expectation.AgreeWithHost
            "char", Expectation.AgreeWithHost
            "string", Expectation.AgreeWithHost
            "object", Expectation.AgreeWithHost
            "System.DateTime", Expectation.AgreeWithHost
            "decimal", Expectation.AgreeWithHost
            // Element types real .NET refuses outright off Windows.
            "Cls", Expectation.AgreeWithHost
            "int[]", Expectation.AgreeWithHost
            "Microsoft.Win32.SafeHandles.SafeFileHandle", Expectation.AgreeWithHost
            "System.Action", Expectation.AgreeWithHost
            "System.IDisposable", Expectation.AgreeWithHost
            // A struct element takes its native layout from the struct, which needs a value of it.
            "Blit", Expectation.Refused
            "NonBlit", Expectation.Refused
            "System.Guid", Expectation.Refused
            "G<int>", Expectation.Refused
            "AutoS", Expectation.Refused
            // CoreCLR sizes a pointer element as its pointee.
            "int*", Expectation.Refused
            "delegate*<void>", Expectation.Refused
        ]

    /// `None` for no `ArraySubType` at all; otherwise the C# expression for it. The explicit
    /// `NATIVE_TYPE_DEFAULT` (0x50) is how the metadata spells "no element type" too.
    let private arraySubTypes : string option list =
        [
            yield None
            for name in Enum.GetNames typeof<UnmanagedType> do
                yield Some $"UnmanagedType.%s{name}"
            yield Some "(UnmanagedType)0x50"
        ]

    let private fixedCorpus : string =
        """
using System;
using System.Runtime.InteropServices;

namespace PawPrint.MarshalByValArrayLayout;

public enum E8 : byte { A = 1 }
public enum E16 : short { A = 1 }
public enum EU32 : uint { A = 1 }
public enum E64 : long { A = 1 }

[StructLayout(LayoutKind.Sequential)] public struct Blit { public int X; public byte Y; }
[StructLayout(LayoutKind.Sequential)] public struct NonBlit { public byte X; public bool B; }
[StructLayout(LayoutKind.Auto)] public struct AutoS { public int X; }
[StructLayout(LayoutKind.Sequential)] public struct G<T> { public T X; }
[StructLayout(LayoutKind.Sequential)] public class Cls { public int X; }
"""

    type private Shape =
        {
            Name : string
            /// The whole C# declaration.
            Source : string
            Element : string
            Expectation : Expectation
        }

    /// `{ byte Head; E[] A; byte Tail; }`: `Head` makes the array's offset show its alignment, and
    /// `Tail` makes its size show.
    let private shapeSource
        (name : string)
        (charSet : string)
        (pack : int)
        (marshalAs : string)
        (fieldType : string)
        : string
        =
        let packArg = if pack = 0 then "" else $", Pack = %d{pack}"

        $"[StructLayout(LayoutKind.Sequential, CharSet = CharSet.%s{charSet}%s{packArg})] public unsafe struct %s{name} {{ public byte Head; [MarshalAs(%s{marshalAs})] public %s{fieldType} A; public byte Tail; }}"

    let private shapes : Shape list =
        let product =
            [
                for element, expectation in elementKinds do
                    // Only a `char` or `string` element reads the struct's `CharSet`.
                    let charSets =
                        match element with
                        | "char"
                        | "string" -> [ "Ansi" ; "Unicode" ]
                        | _ -> [ "Ansi" ]

                    for charSet in charSets do
                        for subType in arraySubTypes do
                            yield element, expectation, charSet, subType
            ]

        let generated =
            product
            |> List.mapi (fun i (element, expectation, charSet, subType) ->
                let name = $"Shape%d{i}"
                // Cycled rather than drawn, so every element kind meets several sizes and packings.
                let sizeConst = [| 3 ; 1 ; 2 ; 5 |].[i % 4]
                let pack = [| 0 ; 0 ; 1 ; 2 ; 4 |].[i % 5]

                let subTypeArg =
                    match subType with
                    | None -> ""
                    | Some s -> $", ArraySubType = %s{s}"

                {
                    Name = name
                    Source =
                        shapeSource
                            name
                            charSet
                            pack
                            $"UnmanagedType.ByValArray, SizeConst = %d{sizeConst}%s{subTypeArg}"
                            $"%s{element}[]"
                    Element = element
                    Expectation = expectation
                }
            )

        let extra (name : string) (marshalAs : string) (fieldType : string) : Shape =
            {
                Name = name
                Source = shapeSource name "Ansi" 0 marshalAs fieldType
                Element = name
                Expectation = Expectation.AgreeWithHost
            }

        generated
        @ [
            // The vacuity guard below pins real .NET's answer for this one.
            extra "IntAsI1" "UnmanagedType.ByValArray, SizeConst = 3, ArraySubType = UnmanagedType.I1" "int[]"
            // Roslyn writes a `SizeConst` of 1 when the attribute gives none.
            extra "NoSizeConst" "UnmanagedType.ByValArray" "int[]"
            extra "ZeroSizeConst" "UnmanagedType.ByValArray, SizeConst = 0" "int[]"
            // The array's rank is not consulted.
            extra "MultiDimensional" "UnmanagedType.ByValArray, SizeConst = 3, ArraySubType = UnmanagedType.I1" "int[,]"
            extra "NotAnArray" "UnmanagedType.ByValArray, SizeConst = 3" "int"
        ]

    let private corpusBytes : byte array =
        let generated =
            shapes
            |> List.map _.Source
            |> String.concat "\n"
            |> fun body ->
                $"using System;\nusing System.Runtime.InteropServices;\nnamespace %s{corpusNamespace};\n%s{body}\n"

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

    let private hostType (shape : Shape) : Type =
        corpusRuntimeAssembly.GetType $"%s{corpusNamespace}.%s{shape.Name}"
        |> Option.ofObj
        |> Option.defaultWith (fun () -> failwith $"corpus does not contain %s{shape.Name}")

    let private fieldNames : string list = [ "Head" ; "A" ; "Tail" ]

    /// Real .NET's size and field offsets, or `None` where it throws `ArgumentException`.
    let private hostLayout (shape : Shape) : (int * (string * int) list) option =
        let t = hostType shape

        try
            let size = Marshal.SizeOf t

            let offsets =
                fieldNames |> List.map (fun name -> name, int (Marshal.OffsetOf (t, name)))

            Some (size, offsets)
        with :? ArgumentException ->
            None

    let private baseState : IlMachineState =
        let dirs = ImmutableArray.CreateRange [ runtimeDir ]
        let state = IlMachineState.initial loggerFactory dirs corelib

        let state =
            { state with
                ConcreteTypes = Corelib.concretizeAll state._LoadedAssemblies bct AllConcreteTypes.Empty
            }

        state.WithLoadedAssembly corpusAssembly

    let private typeDefsByName : Map<string, TypeInfo<GenericParamFromMetadata, TypeDefn>> =
        corpusAssembly.TypeDefs
        |> Seq.map (fun kvp -> kvp.Value.Name, kvp.Value)
        |> Map.ofSeq

    let private pawPrintLayout (shape : Shape) : Result<int * (string * int) list, MarshalSizeError> =
        let typeInfo = typeDefsByName.[shape.Name]

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
        | CliType.ValueType vt, state ->
            CliValueType.TryComputeMarshalLayout state.ConcreteTypes state._LoadedAssemblies bct vt
            |> Result.map (fun (size, placements) ->
                size.Size, placements |> List.map (fun p -> p.Field.Name, p.NativeOffset)
            )
        | other, _ -> failwith $"%s{shape.Name} should be a value type, but its zero is %O{other}"

    /// Each shape's disagreement with its expectation, if any.
    let private disagreement (shape : Shape) : string option =
        let actual = pawPrintLayout shape

        match shape.Expectation with
        | Expectation.AgreeWithHost ->
            match hostLayout shape, actual with
            | Some host, Result.Ok actual when host = actual -> None
            | None, Result.Error (MarshalSizeError.NotMarshalable _) -> None
            | host, actual -> Some $"%s{shape.Source}\n  real .NET: %A{host}\n  PawPrint:  %A{actual}"
        | Expectation.Refused ->
            match actual with
            | Result.Error (MarshalSizeError.NotImplemented _) -> None
            | actual -> Some $"%s{shape.Source}\n  expected NotImplemented, got %A{actual}"

    [<Test>]
    let ``Marshal layout of structs with ByValArray fields agrees with real .NET`` () : unit =
        let failures = shapes |> List.choose disagreement

        match failures with
        | [] -> ()
        | _ ->
            let described = failures |> List.truncate 40 |> String.concat "\n"
            failwith $"%d{failures.Length} of %d{shapes.Length} shapes disagree:\n%s{described}"

    [<Test>]
    let ``the corpus reaches the cases the comparison depends on`` () : unit =
        // Vacuity guard: the comparison is only as strong as what real .NET does with the corpus.
        let hostAnswers =
            shapes
            |> List.filter (fun shape -> shape.Expectation = Expectation.AgreeWithHost)
            |> List.map (fun shape -> shape, hostLayout shape)

        let answered (element : string) : int =
            hostAnswers
            |> List.filter (fun (shape, layout) -> shape.Element = element && layout.IsSome)
            |> List.length

        let refused (element : string) : int =
            hostAnswers
            |> List.filter (fun (shape, layout) -> shape.Element = element && layout.IsNone)
            |> List.length

        // A primitive element is sized by every ArraySubType, including every scalar of another
        // width; an element that honours ArraySubType also refuses some of them.
        answered "int" |> shouldEqual arraySubTypes.Length
        answered "E64" |> shouldEqual arraySubTypes.Length

        for element in [ "string" ; "object" ; "System.DateTime" ; "decimal" ] do
            answered element |> shouldBeGreaterThan 0
            refused element |> shouldBeGreaterThan 0

        for element in [ "Cls" ; "int[]" ; "Microsoft.Win32.SafeHandles.SafeFileHandle" ] do
            answered element |> shouldEqual 0

        // `[MarshalAs(ByValArray, SizeConst = 3, ArraySubType = I1)] int[]` is twelve bytes at offset
        // four, so the Tail after it is at sixteen.
        let intI1 = shapes |> List.find (fun shape -> shape.Name = "IntAsI1")

        hostLayout intI1 |> shouldEqual (Some (20, [ "Head", 0 ; "A", 4 ; "Tail", 16 ]))
