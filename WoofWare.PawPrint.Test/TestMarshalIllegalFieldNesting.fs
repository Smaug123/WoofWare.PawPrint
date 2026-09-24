namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// A field whose managed/native pairing CoreCLR's `MarshalInfo` refuses is not left out of its
/// struct's native layout: it becomes a `NativeFieldCategory::ILLEGAL` descriptor, one byte wide
/// and 1-aligned. That makes its *own* struct unmarshalable, so `Marshal.SizeOf` and
/// `Marshal.OffsetOf` refuse it, but a struct that merely *holds* that struct is laid out as
/// usual, the held struct contributing its native size. Marshalling the holder then fails when
/// CoreCLR builds the struct stub, which builds the held struct's stub too.
///
/// This sweeps field types of every kind, under no `[MarshalAs]` and under every `UnmanagedType`.
/// Each pairing is the middle field `F` of `ShapeN { byte A; F; byte B; }`, and each `ShapeN` is in
/// turn the middle field of `HolderN { byte A; ShapeN S; byte B; }`, so the shape's own layout, and
/// the size and alignment it contributes one level down, all show in the offsets.
///
/// The oracle is real .NET, in-process, for the reason `TestMarshalEnumFieldLayout` gives: the
/// corpus is a library with no entry point and no static state.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMarshalIllegalFieldNesting =

    let private loggerFactory = snd (LoggerFactory.makeTest ())

    let private corelibPath : string = typeof<obj>.Assembly.Location
    let private runtimeDir : string = Path.GetDirectoryName corelibPath
    let private corelib : DumpedAssembly = Assembly.readFile loggerFactory corelibPath
    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private corpusNamespace : string = "PawPrint.MarshalIllegalNesting"

    /// Reference field types, as C# spells them. PawPrint does not model which native types each
    /// admits, so answers `NotImplemented` for most pairings; see `expectedUnimplemented`.
    let private referenceFieldTypes : string list =
        [ "string" ; "object" ; "int[]" ; "Action" ; "IDisposable" ; "LayoutClass" ]

    /// Field types, as C# spells them.
    let private fieldTypes : string list =
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
            "int*"
            "delegate*<void>"
            "bool"
            "char"
            "E32"
            "DateTime"
            "decimal"
            "Guid"
            "Seq"
            "Auto"
        ]
        @ referenceFieldTypes

    let private fixedCorpus : string =
        """
using System;
using System.Runtime.InteropServices;

namespace PawPrint.MarshalIllegalNesting;

public enum E32 { A = 1 }
public struct Seq { public byte T; public int K; }
[StructLayout(LayoutKind.Auto)]
public struct Auto { public int A; public int B; }
[StructLayout(LayoutKind.Sequential)]
public class LayoutClass { public int A; }
"""

    type private Shape =
        {
            Index : int
            FieldType : string
            /// `None` omits `[MarshalAs]`.
            MarshalAs : UnmanagedType option
        }

        member this.Name : string = $"Shape%d{this.Index}"
        member this.HolderName : string = $"Holder%d{this.Index}"

    let private renderMarshalAs (marshalAs : UnmanagedType option) : string =
        match marshalAs with
        | None -> ""
        // C# insists on these arguments for these two, whatever the field is.
        | Some UnmanagedType.ByValTStr
        | Some UnmanagedType.ByValArray -> $"[MarshalAs(UnmanagedType.%O{marshalAs.Value}, SizeConst = 1)] "
        | Some UnmanagedType.CustomMarshaler -> "[MarshalAs(UnmanagedType.CustomMarshaler, MarshalType = \"Nothing\")] "
        | Some u -> $"[MarshalAs((UnmanagedType)%d{int u})] "

    let private render (shape : Shape) : string =
        let marshalAs = renderMarshalAs shape.MarshalAs

        $"public unsafe struct %s{shape.Name} {{ public byte A; %s{marshalAs}public %s{shape.FieldType} F; public byte B; }}\n"
        + $"public struct %s{shape.HolderName} {{ public byte A; public %s{shape.Name} S; public byte B; }}"

    let private shapes : Shape list =
        let descriptors =
            None
            :: (Enum.GetValues typeof<UnmanagedType>
                |> Seq.cast<UnmanagedType>
                |> Seq.distinct
                // C# refuses to put `VBByRefStr` on a field at all (CS7054). It is named by string
                // because naming the case is itself an obsolescence warning.
                |> Seq.filter (fun u -> Enum.GetName u <> "VBByRefStr")
                |> Seq.map Some
                |> List.ofSeq)

        [
            for fieldType in fieldTypes do
                for marshalAs in descriptors do
                    yield fieldType, marshalAs
        ]
        |> List.mapi (fun i (fieldType, marshalAs) ->
            {
                Index = i
                FieldType = fieldType
                MarshalAs = marshalAs
            }
        )

    let private corpusBytes : byte array =
        let body = shapes |> List.map render |> String.concat "\n"

        Roslyn.compileAssembly
            corpusNamespace
            Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
            []
            [
                fixedCorpus
                $"#pragma warning disable 618\nusing System;\nusing System.Runtime.InteropServices;\nnamespace %s{corpusNamespace};\n%s{body}\n"
            ]

    let private corpusAssembly : DumpedAssembly =
        use stream = new MemoryStream (corpusBytes)
        AssemblyApi.read loggerFactory (Some $"%s{corpusNamespace}.dll") stream

    let private corpusRuntimeAssembly : System.Reflection.Assembly =
        System.Reflection.Assembly.Load corpusBytes

    let private hostType (name : string) : Type =
        corpusRuntimeAssembly.GetType $"%s{corpusNamespace}.%s{name}"
        |> Option.ofObj
        |> Option.defaultWith (fun () -> failwith $"corpus does not contain %s{name}")

    /// What marshalling a type does, as `Marshal.StructureToPtr` and its siblings would find it.
    [<RequireQualifiedAccess>]
    type private Stub =
        /// No stub: the type's managed image is its native image, and CoreLib memmoves it.
        | Blittable
        /// A struct stub, which CoreCLR builds successfully.
        | Synthesised
        /// Building the struct stub fails, because a field somewhere under the type is illegal.
        | Refused

    type private Answer =
        {
            /// The native size and each field's native offset, or `None` when `Marshal.SizeOf`
            /// refuses the type.
            Layout : (int * int list) option
            Stub : Stub
        }

    /// Real .NET's answer for `t`. Only a struct stub runs a Cleanup pass, and that zeroes the
    /// image, so `DestroyStructure` over a dirty buffer leaves it dirty exactly when the type is
    /// blittable; and it builds the stub, so it throws `TypeLoadException` exactly when that fails.
    ///
    /// A type holding a reference field is never blittable, and its Cleanup pass frees whatever
    /// native pointer the field's bytes hold, so it gets a zeroed buffer instead of a dirty one.
    let private hostAnswerOf (holdsReference : bool) (t : Type) : Answer =
        let layout =
            try
                let size = Marshal.SizeOf t

                let offsets =
                    t.GetFields ()
                    |> Array.sortBy _.MetadataToken
                    |> Array.map (fun field -> int (Marshal.OffsetOf (t, field.Name)))
                    |> List.ofArray

                Some (size, offsets)
            with :? ArgumentException ->
                None

        let bufferSize = 512
        let dirty = if holdsReference then 0uy else 0xABuy
        let buffer = Marshal.AllocHGlobal bufferSize

        try
            for i in 0 .. bufferSize - 1 do
                Marshal.WriteByte (buffer, i, dirty)

            let stub =
                try
                    Marshal.DestroyStructure (buffer, t)

                    match layout with
                    | None ->
                        failwith
                            $"%s{t.Name}: real .NET refuses its size but builds its stub, which this oracle does not expect"
                    | Some (size, _) ->
                        if
                            not holdsReference
                            && [ 0 .. size - 1 ] |> List.forall (fun i -> Marshal.ReadByte (buffer, i) = dirty)
                        then
                            Stub.Blittable
                        else
                            Stub.Synthesised
                with :? TypeLoadException ->
                    Stub.Refused

            {
                Layout = layout
                Stub = stub
            }
        finally
            Marshal.FreeHGlobal buffer

    let private baseState : IlMachineState =
        let dirs = ImmutableArray.CreateRange [ runtimeDir ]
        let state = IlMachineState.initial loggerFactory dirs corelib

        let state =
            { state with
                ConcreteTypes = Corelib.concretizeAll state._LoadedAssemblies bct AllConcreteTypes.Empty
            }

        state.WithLoadedAssembly corpusAssembly

    /// PawPrint's answer for the type named `name`, or the reason PawPrint does not implement it.
    let private pawPrintAnswerOf (name : string) : Result<Answer, string> =
        let typeInfo =
            corpusAssembly.TypeDefs
            |> Seq.map (fun kvp -> kvp.Value)
            |> Seq.filter (fun ti -> ti.Name = name)
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

        let vt =
            match zero with
            | CliType.ValueType vt -> vt
            | other -> failwith $"%s{name} should be a value type, but its zero is %O{other}"

        let layout =
            match CliValueType.TryComputeMarshalLayout state.ConcreteTypes state._LoadedAssemblies bct vt with
            | Result.Error (MarshalSizeError.NotMarshalable _) -> Result.Ok None
            | Result.Error (MarshalSizeError.NotImplemented reason) -> Result.Error reason
            | Result.Ok (size, placements) -> Result.Ok (Some (size.Size, placements |> List.map _.NativeOffset))

        let stub =
            if StructMarshalStub.isBlittableStruct state.ConcreteTypes state._LoadedAssemblies bct zero then
                Stub.Blittable
            else
                match StructMarshalStub.tryComputePlan state.ConcreteTypes state._LoadedAssemblies bct zero with
                | Result.Error (MarshalSizeError.NotMarshalable _) -> Stub.Refused
                // A plan PawPrint cannot yet execute is still a stub CoreCLR would build.
                | Result.Error (MarshalSizeError.NotImplemented _)
                | Result.Ok _ -> Stub.Synthesised

        layout
        |> Result.map (fun layout ->
            {
                Layout = layout
                Stub = stub
            }
        )

    /// Whether PawPrint is expected to answer `NotImplemented` for a pairing, rather than a layout:
    /// the pairings it does not model. The test demands PawPrint answers `NotImplemented` for
    /// exactly these, so that the set cannot grow unnoticed.
    let private expectedUnimplemented (fieldType : string) (marshalAs : UnmanagedType option) : bool =
        if List.contains fieldType referenceFieldTypes then
            // Which native types a reference field admits depends on what it references, which
            // PawPrint does not model; but no reference type admits a native number, a
            // `ByValTStr` except on a string, or a `ByValArray` except on an array.
            match marshalAs with
            | Some UnmanagedType.I1
            | Some UnmanagedType.U1
            | Some UnmanagedType.I2
            | Some UnmanagedType.U2
            | Some UnmanagedType.I4
            | Some UnmanagedType.U4
            | Some UnmanagedType.I8
            | Some UnmanagedType.U8
            | Some UnmanagedType.R4
            | Some UnmanagedType.R8
            | Some UnmanagedType.SysInt
            | Some UnmanagedType.SysUInt
            | Some UnmanagedType.Error
            | Some UnmanagedType.ByValTStr -> false
            // The descriptor names no `ArraySubType`, which PawPrint does not default.
            | Some UnmanagedType.ByValArray -> fieldType = "int[]"
            | _ -> true
        else
            // `MARSHAL_TYPE_CURRENCY`: an OLE `CY`, eight bytes. Named by string because naming
            // the case is an obsolescence warning.
            fieldType = "decimal" && marshalAs = Some (Enum.Parse<UnmanagedType> "Currency")

    /// Pairings on which PawPrint's answer deliberately differs from real .NET's.
    let private knownDivergences : Set<string * UnmanagedType option> =
        Set.ofList
            [
                // CoreCLR blits a pointer field. PawPrint's pointer cells carry provenance that has
                // no byte rendering, so it never calls one blittable; its struct stub copies the
                // cell instead.
                "int*", None
            ]

    let private answers : (Shape * Answer * Answer * Result<Answer, string> * Result<Answer, string>) list =
        shapes
        |> List.map (fun shape ->
            shape,
            hostAnswerOf (List.contains shape.FieldType referenceFieldTypes) (hostType shape.Name),
            hostAnswerOf (List.contains shape.FieldType referenceFieldTypes) (hostType shape.HolderName),
            pawPrintAnswerOf shape.Name,
            pawPrintAnswerOf shape.HolderName
        )

    [<Test>]
    let ``fields lay out, refuse and marshal as real .NET does, directly and one struct down`` () : unit =
        let disagreements, unimplemented =
            answers
            |> List.fold
                (fun (disagreements, unimplemented) (shape, hostShape, hostHolder, pawShape, pawHolder) ->
                    let key = shape.FieldType, shape.MarshalAs

                    match pawShape, pawHolder with
                    | Result.Error _, _
                    | _, Result.Error _ -> disagreements, Set.add key unimplemented
                    | Result.Ok pawShape, Result.Ok pawHolder ->
                        let agrees = (pawShape, pawHolder) = (hostShape, hostHolder)
                        let expectedToAgree = not (knownDivergences.Contains key)

                        if agrees <> expectedToAgree then
                            let heading =
                                if expectedToAgree then
                                    render shape
                                else
                                    $"%s{render shape}\n  (a known divergence, which no longer diverges)"

                            let described =
                                $"%s{heading}\n  real .NET: %A{hostShape}\n             %A{hostHolder}\n  PawPrint:  %A{pawShape}\n             %A{pawHolder}"

                            described :: disagreements, unimplemented
                        else
                            disagreements, unimplemented
                )
                ([], Set.empty)

        match disagreements with
        | [] -> ()
        | _ ->
            let described = disagreements |> List.rev |> String.concat "\n"
            failwith $"%d{disagreements.Length} of %d{shapes.Length} pairings disagree:\n%s{described}"

        shapes
        |> List.filter (fun shape -> expectedUnimplemented shape.FieldType shape.MarshalAs)
        |> List.map (fun shape -> shape.FieldType, shape.MarshalAs)
        |> Set.ofList
        |> shouldEqual unimplemented

    [<Test>]
    let ``the sweep sees illegal fields laid out one struct down`` () : unit =
        // Vacuity guard: the comparison above is only as good as the illegal pairings it reaches.
        let illegalButHeld =
            answers
            |> List.filter (fun (_, hostShape, hostHolder, _, _) ->
                hostShape.Layout.IsNone
                && hostHolder.Layout.IsSome
                && hostShape.Stub = Stub.Refused
                && hostHolder.Stub = Stub.Refused
            )

        illegalButHeld.Length |> shouldBeGreaterThan (shapes.Length / 2)

        // Every field type has some illegal pairing, and its holder places a one-byte `F`: the
        // shape is then `{ byte A; F; byte B }` at 1-byte alignment, three bytes in all, so the
        // holder's `B` follows the shape at offset 4.
        illegalButHeld
        |> List.map (fun (shape, _, _, _, _) -> shape.FieldType)
        |> Set.ofList
        |> shouldEqual (Set.ofList fieldTypes)

        illegalButHeld
        |> List.map (fun (_, _, hostHolder, _, _) -> hostHolder.Layout)
        |> List.distinct
        |> shouldEqual [ Some (5, [ 0 ; 1 ; 4 ]) ]

        // A type with no layout of its own is illegal as a field, but is no different from any
        // other illegal field one level further out.
        answers
        |> List.find (fun (shape, _, _, _, _) -> shape.FieldType = "Auto" && shape.MarshalAs.IsNone)
        |> fun (_, hostShape, hostHolder, _, _) -> hostShape.Layout, hostHolder.Layout
        |> shouldEqual (None, Some (5, [ 0 ; 1 ; 4 ]))

        // Each marshalling outcome occurs, so that the stub comparison is not vacuous either.
        answers
        |> List.collect (fun (_, hostShape, hostHolder, _, _) -> [ hostShape.Stub ; hostHolder.Stub ])
        |> Set.ofList
        |> shouldEqual (Set.ofList [ Stub.Blittable ; Stub.Synthesised ; Stub.Refused ])

    let private mixedNamespace : string = "PawPrint.MarshalIllegalMixed"

    /// Each struct declares a field CoreCLR refuses and a field PawPrint cannot size, in both
    /// orders.
    let private mixedBytes : byte array =
        Roslyn.compileAssembly
            mixedNamespace
            Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
            []
            [
                $"""
using System.Runtime.InteropServices;
namespace %s{mixedNamespace};
public struct IllegalFirst {{ [MarshalAs(UnmanagedType.I1)] public int F; public string S; }}
public struct IllegalLast {{ public string S; [MarshalAs(UnmanagedType.I1)] public int F; }}
"""
            ]

    [<TestCase "IllegalFirst">]
    [<TestCase "IllegalLast">]
    let ``a struct with an illegal field of its own is refused, whatever else it holds`` (name : string) : unit =
        let hostType =
            (System.Reflection.Assembly.Load mixedBytes).GetType $"%s{mixedNamespace}.%s{name}"

        Assert.Throws<ArgumentException> (fun () -> Marshal.SizeOf hostType |> ignore)
        |> ignore

        let dumped =
            use stream = new MemoryStream (mixedBytes)
            AssemblyApi.read loggerFactory (Some $"%s{mixedNamespace}.dll") stream

        let typeInfo =
            dumped.TypeDefs
            |> Seq.map (fun kvp -> kvp.Value)
            |> Seq.filter (fun ti -> ti.Name = name)
            |> Seq.exactlyOne

        let state, handle =
            IlMachineTypeResolution.concretizeType
                loggerFactory
                bct
                (baseState.WithLoadedAssembly dumped)
                typeInfo.AssemblyFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (typeInfo.Identity, System.Reflection.Metadata.SignatureTypeKind.ValueType))

        let vt =
            match IlMachineState.cliTypeZeroOfHandle state bct handle with
            | CliType.ValueType vt, _ -> vt
            | other, _ -> failwith $"%s{name} should be a value type, but its zero is %O{other}"

        // PawPrint cannot lay the type out at all, because of the string field...
        match CliValueType.TryComputeNativeLayout state.ConcreteTypes state._LoadedAssemblies bct vt with
        | Result.Error (MarshalSizeError.NotImplemented _) -> ()
        | other -> failwith $"%s{name}: expected PawPrint not to lay out a string field, got %A{other}"

        // ...but it need not, to know that `Marshal.SizeOf` refuses it.
        match CliValueType.TryComputeMarshalLayout state.ConcreteTypes state._LoadedAssemblies bct vt with
        | Result.Error (MarshalSizeError.NotMarshalable _) -> ()
        | other -> failwith $"%s{name}: expected NotMarshalable, got %A{other}"
