namespace WoofWare.PawPrint.Test

open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `CliType.TryReinterpret`, which `Unsafe.BitCast` runs between two distinct value types: the
/// target's numbers and padding take the source's bytes, and each object reference or runtime
/// pointer moves whole into a target field of the same kind at the same offset.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestCliTypeReinterpret =

    // Factory intentionally undisposed: corelib.Logger outlives this scope.
    let private corelib : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory HostImage.coreLibPath

    let private bct : BaseClassTypes<DumpedAssembly> = BaseClassTypes.ofCorelib corelib

    let private allCt : AllConcreteTypes =
        Corelib.concretizeAll (LoadedAssemblies.ofAssemblies [ corelib ]) bct AllConcreteTypes.Empty

    let private handleOf (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>) : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle allCt ty

    let private sourceType : ConcreteTypeHandle = handleOf bct.TypedReference
    let private targetType : ConcreteTypeHandle = handleOf bct.ValueType
    let private nestedType : ConcreteTypeHandle = handleOf bct.Enum
    let private objectHandle : ConcreteTypeHandle = handleOf bct.Object
    let private intPtrHandle : ConcreteTypeHandle = handleOf bct.IntPtr

    /// Element types a pointer field may be a byref to.
    let private elementTypes : ConcreteTypeHandle list =
        [ bct.Int32 ; bct.Int64 ; bct.Byte ; bct.Char ] |> List.map handleOf

    let private field (name : string) (offset : int) (contents : CliType) (fieldType : ConcreteTypeHandle) : CliField =
        {
            Id = FieldId.named name
            Name = name
            Contents = contents
            Offset = Some offset
            Type = fieldType
            MarshallingDescriptor = None
        }

    let private explicitStruct (declared : ConcreteTypeHandle) (size : int) (fields : CliField list) : CliValueType =
        SynthesisedLayoutKind.ofFields
            bct
            allCt
            declared
            (Layout.Custom (size = size, packingSize = 0))
            CharSet.Ansi
            fields

    /// One 8-byte, 8-aligned word of a generated value.
    type private Word =
        /// Numbers at offsets within the word; bytes no number covers are padding. `Nested` puts
        /// them inside a struct of their own, which fills the word.
        | Bytes of numbers : (int * CliType * ConcreteTypeHandle) list * nested : bool
        | Reference of ManagedHeapAddress option
        | Pointer of element : ConcreteTypeHandle * ManagedPointerSource
        /// In a source, a native int with no byte image; in a target, an `IntPtr` field.
        | Handle of NativeIntSource
        /// Only in a source: a native int with no byte image and a long, both spanning the word.
        /// Neither was written after the other, so the one declared last holds the word.
        | Overlaid of handle : NativeIntSource * bits : int64 * handleOnTop : bool

    let private wordFields (index : int) (word : Word) : CliField list =
        let name = $"W%d{index}"
        let offset = 8 * index

        match word with
        | Word.Bytes (numbers, false) ->
            numbers
            |> List.map (fun (local, contents, ty) -> field $"%s{name}_%d{local}" (offset + local) contents ty)
        | Word.Bytes (numbers, true) ->
            let inner =
                numbers
                |> List.map (fun (local, contents, ty) -> field $"%s{name}_%d{local}" local contents ty)
                |> explicitStruct nestedType 8

            [ field name offset (CliType.ValueType inner) nestedType ]
        | Word.Reference address -> [ field name offset (CliType.ObjectRef address) objectHandle ]
        | Word.Pointer (element, pointer) ->
            [
                field
                    name
                    offset
                    (CliType.RuntimePointer (CliRuntimePointer.Managed pointer))
                    (ConcreteTypeHandle.Byref element)
            ]
        | Word.Handle source ->
            [
                field name offset (CliType.Numeric (CliNumericType.NativeInt source)) intPtrHandle
            ]
        | Word.Overlaid (handle, bits, handleOnTop) ->
            let handleField =
                field $"%s{name}_handle" offset (CliType.Numeric (CliNumericType.NativeInt handle)) intPtrHandle

            let longField =
                field
                    $"%s{name}_long"
                    offset
                    (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim bits)))
                    (handleOf bct.Int64)

            if handleOnTop then
                [ longField ; handleField ]
            else
                [ handleField ; longField ]

    let private build (declared : ConcreteTypeHandle) (words : Word list) : CliValueType =
        words
        |> List.mapi wordFields
        |> List.concat
        |> explicitStruct declared (8 * words.Length)

    let private genByte : Gen<byte> = ArbMap.defaults |> ArbMap.generate<byte>

    /// A number of `width` bytes holding arbitrary bits, with the type of a field holding it.
    let private genNumber (width : int) : Gen<CliType * ConcreteTypeHandle> =
        let of' (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>) (make : byte[] -> CliType) =
            Gen.arrayOfLength width genByte
            |> Gen.map (fun bytes -> make bytes, handleOf ty)

        let numeric (template : CliNumericType) (bytes : byte[]) : CliType =
            CliType.OfBytesLike (CliType.Numeric template) bytes

        match width with
        | 1 ->
            Gen.oneof
                [
                    of' bct.SByte (numeric (CliNumericType.Int8 0y))
                    of' bct.Byte (numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy)))
                    of' bct.Boolean (fun bytes -> CliType.Bool bytes.[0])
                ]
        | 2 ->
            Gen.oneof
                [
                    of' bct.Int16 (numeric (CliNumericType.Int16 0s))
                    of' bct.UInt16 (numeric (CliNumericType.UInt16 0us))
                    of' bct.Char (fun bytes -> CliType.Char (bytes.[1], bytes.[0]))
                ]
        | 4 ->
            Gen.oneof
                [
                    of' bct.Int32 (numeric (CliNumericType.Int32 0))
                    of' bct.Single (numeric (CliNumericType.Float32 0.0f))
                ]
        | 8 ->
            Gen.oneof
                [
                    of' bct.Int64 (numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L)))
                    of' bct.Double (numeric (CliNumericType.Float64 0.0))
                ]
        | other -> failwith $"no number is %d{other} bytes wide"

    /// Numbers placed left to right through one word, each at an offset aligned to its width,
    /// with the bytes skipped over left as padding.
    let private genNumbers : Gen<(int * CliType * ConcreteTypeHandle) list> =
        let rec from (position : int) : Gen<(int * CliType * ConcreteTypeHandle) list> =
            if position >= 8 then
                Gen.constant []
            else
                let widths =
                    [ 1 ; 2 ; 4 ; 8 ]
                    |> List.filter (fun w -> position % w = 0 && position + w <= 8)

                Gen.oneof
                    [
                        from (position + 1)
                        gen {
                            let! width = Gen.elements widths
                            let! contents, ty = genNumber width
                            let! rest = from (position + width)
                            return (position, contents, ty) :: rest
                        }
                    ]

        from 0

    let private genBytesWord : Gen<Word> =
        Gen.map2
            (fun numbers nested -> Word.Bytes (numbers, nested))
            (Gen.frequency [ 1, Gen.constant [] ; 4, genNumbers ])
            (Gen.elements [ false ; true ])

    let private genHandle : Gen<NativeIntSource> =
        Gen.elements [ NativeIntSource.FieldHandlePtr 1234L ; NativeIntSource.MethodHandlePtr 99L ]

    let private genPointer : Gen<ManagedPointerSource> =
        Gen.oneof
            [
                Gen.constant ManagedPointerSource.Null
                Gen.choose (0, 15)
                |> Gen.map (fun offset ->
                    ManagedPointerSource.Byref
                        {
                            Root = ByrefRoot.StackMemoryByte (ThreadId 0, FrameId 0, StackMemoryBlockId 0, offset)
                            Projections = []
                        }
                )
            ]

    let private genSourceWord : Gen<Word> =
        Gen.frequency
            [
                4, genBytesWord
                1,
                Gen.elements [ None ; Some (ManagedHeapAddress 3) ; Some (ManagedHeapAddress 4) ]
                |> Gen.map Word.Reference
                2, Gen.map2 (fun e p -> Word.Pointer (e, p)) (Gen.elements elementTypes) genPointer
                1, genHandle |> Gen.map Word.Handle
                1,
                gen {
                    let! handle = genHandle
                    let! bits = ArbMap.defaults |> ArbMap.generate<int64>
                    let! handleOnTop = Gen.elements [ false ; true ]
                    return Word.Overlaid (handle, bits, handleOnTop)
                }
            ]

    /// What a source word holds, which for an overlaid word is the field declared last.
    let private effective (source : Word) : Word =
        match source with
        | Word.Overlaid (handle, _, true) -> Word.Handle handle
        | Word.Overlaid (_, bits, false) ->
            Word.Bytes (
                [
                    0, CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim bits)), handleOf bct.Int64
                ],
                false
            )
        | Word.Bytes _
        | Word.Reference _
        | Word.Pointer _
        | Word.Handle _ -> source

    /// A target word holding what `source` holds, in a field shape of its own choosing.
    let private genCompatibleWord (source : Word) : Gen<Word> =
        match effective source with
        | Word.Overlaid _ -> failwith "unreachable: an effective word is never overlaid"
        | Word.Bytes _ -> Gen.oneof [ genBytesWord ; Gen.constant (Word.Handle (NativeIntSource.Verbatim 0L)) ]
        | Word.Reference None ->
            Gen.oneof
                [
                    genBytesWord
                    Gen.constant (Word.Reference None)
                    Gen.constant (Word.Handle (NativeIntSource.Verbatim 0L))
                ]
        | Word.Reference (Some _) -> Gen.constant (Word.Reference None)
        | Word.Pointer _ ->
            Gen.elements elementTypes
            |> Gen.map (fun e -> Word.Pointer (e, ManagedPointerSource.Null))
        | Word.Handle _ -> Gen.constant (Word.Handle (NativeIntSource.Verbatim 0L))

    /// Whether a target word can take what the source word holds.
    let private compatible (source : Word) (target : Word) : bool =
        match effective source, target with
        | Word.Overlaid _, _
        | _, Word.Overlaid _ -> failwith "unreachable: neither an effective source word nor a target word is overlaid"
        | Word.Bytes _, (Word.Bytes _ | Word.Handle _) -> true
        | Word.Bytes _, (Word.Reference _ | Word.Pointer _) -> false
        // A null reference's bits are zero, which a number may take.
        | Word.Reference None, (Word.Bytes _ | Word.Handle _ | Word.Reference _) -> true
        | Word.Reference None, Word.Pointer _ -> false
        | Word.Reference (Some _), Word.Reference _ -> true
        | Word.Reference (Some _), (Word.Bytes _ | Word.Handle _ | Word.Pointer _) -> false
        | Word.Pointer _, Word.Pointer _ -> true
        | Word.Pointer _, (Word.Bytes _ | Word.Handle _ | Word.Reference _) -> false
        | Word.Handle _, Word.Handle _ -> true
        | Word.Handle _, (Word.Bytes _ | Word.Reference _ | Word.Pointer _) -> false

    /// A source and a target of equal size, the target word by word either holding what the
    /// source holds or a shape drawn independently, which may not.
    let private genPair : Gen<Word list * Word list> =
        gen {
            let! length = Gen.choose (1, 4)
            let! source = Gen.listOfLength length genSourceWord

            let! target =
                source
                |> List.map (fun word ->
                    Gen.frequency
                        [
                            4, genCompatibleWord word
                            1,
                            Gen.oneof
                                [
                                    genBytesWord
                                    Gen.constant (Word.Reference None)
                                    Gen.constant (Word.Pointer (List.head elementTypes, ManagedPointerSource.Null))
                                    Gen.constant (Word.Handle (NativeIntSource.Verbatim 0L))
                                ]
                        ]
                )
                |> Gen.sequenceToList

            return source, target
        }

    let private refusalOffset (refusal : CliReinterpretRefusal) : int =
        match refusal with
        | CliReinterpretRefusal.NoBytesForNumber (_, _, at, _) -> at
        | CliReinterpretRefusal.NothingToMove (offset, _) -> offset
        | CliReinterpretRefusal.PointerBetweenTypes (offset, _, _) -> offset
        | CliReinterpretRefusal.NoBytesForPadding (at, _) -> at
        | CliReinterpretRefusal.MovedFieldOverlaid offset -> offset

    let private reinterpret (source : CliValueType) (target : CliValueType) : Result<CliType, CliReinterpretRefusal> =
        CliType.TryReinterpret (CliType.ValueType target) targetType (CliType.ValueType source) sourceType

    let private fieldNamed (name : string) (value : CliType) : CliType =
        match value with
        | CliType.ValueType vt -> CliValueType.DereferenceField name vt
        | other -> failwith $"expected a value type, got %O{other}"

    let private fieldOf (index : int) (value : CliType) : CliType = fieldNamed $"W%d{index}" value

    /// What the target word at `index` must hold, given the source word it takes its bits from.
    let private checkWord (source : CliType) (result : CliType) (index : int) (sourceWord : Word) (targetWord : Word) =
        let offset = 8 * index

        match targetWord, effective sourceWord with
        | Word.Bytes _, _ ->
            CliType.TryBytesAt offset 8 result
            |> shouldEqual (CliType.TryBytesAt offset 8 source)
        | Word.Reference _, Word.Reference address -> fieldOf index result |> shouldEqual (CliType.ObjectRef address)
        | Word.Pointer (into, _), Word.Pointer (from, pointer) ->
            let expected =
                if into = from then
                    pointer
                else
                    ManagedPointerSource.reinterpretAs into pointer

            fieldOf index result
            |> shouldEqual (CliType.RuntimePointer (CliRuntimePointer.Managed expected))
        | Word.Handle _, Word.Handle handle ->
            fieldOf index result
            |> shouldEqual (CliType.Numeric (CliNumericType.NativeInt handle))
        | Word.Handle _, (Word.Bytes _ | Word.Reference None) ->
            let bits =
                match CliType.TryBytesAt offset 8 source with
                | Ok bytes -> System.BitConverter.ToInt64 (bytes, 0)
                | Error e -> failwith $"the source word %d{index} was expected to have bytes: %s{e.Description}"

            fieldOf index result
            |> shouldEqual (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim bits)))
        | _ -> failwith $"word %d{index}: %O{targetWord} cannot take %O{sourceWord}, yet the reinterpretation succeeded"

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 2000

    [<Test>]
    let ``a reinterpretation succeeds exactly when every target word can take its source word`` () =
        let property (sourceWords : Word list, targetWords : Word list) =
            let source = build sourceType sourceWords
            let target = build targetType targetWords
            let fits = List.map2 compatible sourceWords targetWords

            match reinterpret source target with
            | Ok result ->
                fits |> List.forall id |> shouldEqual true
                List.iteri2 (checkWord (CliType.ValueType source) result) sourceWords targetWords
            | Error refusal ->
                let word = refusalOffset refusal / 8

                if word >= fits.Length || fits.[word] then
                    failwith $"refused at word %d{word}, which can take its source word: %s{refusal.Description}"

        Check.One (config, Prop.forAll (Arb.fromGen genPair) property)

    [<Test>]
    let ``over bytes alone, a reinterpretation is the byte image read back as the target`` () =
        let genBytesPair =
            gen {
                let! length = Gen.choose (1, 4)
                let! source = Gen.listOfLength length genBytesWord
                let! target = Gen.listOfLength length genBytesWord
                return source, target
            }

        let property (sourceWords : Word list, targetWords : Word list) =
            let source = build sourceType sourceWords
            let target = build targetType targetWords

            let expected =
                CliType.OfBytesLike (CliType.ValueType target) (CliValueType.ToBytes source)

            match reinterpret source target with
            | Ok result ->
                // Structural equality calls a NaN unequal to itself, even through `Equals` once it
                // is inside a list, so compare the bits and, separately, the printed shape.
                CliType.ToBytes result |> shouldEqual (CliType.ToBytes expected)
                sprintf "%A" result |> shouldEqual (sprintf "%A" expected)
            | Error refusal -> failwith $"refused a reinterpretation over bytes: %s{refusal.Description}"

        Check.One (config, Prop.forAll (Arb.fromGen genBytesPair) property)

    [<Test>]
    let ``a managed pointer moves only between byrefs, or between fields of one type`` () =
        let pointer =
            ManagedPointerSource.Byref
                {
                    Root = ByrefRoot.StackMemoryByte (ThreadId 0, FrameId 0, StackMemoryBlockId 0, 4)
                    Projections = []
                }

        let int32Handle = handleOf bct.Int32
        let int64Handle = handleOf bct.Int64

        let holding (declared : ConcreteTypeHandle) (fieldType : ConcreteTypeHandle) (p : ManagedPointerSource) =
            explicitStruct declared 8 [ field "P" 0 (CliType.RuntimePointer (CliRuntimePointer.Managed p)) fieldType ]

        let unmanaged = holding sourceType (ConcreteTypeHandle.Pointer int32Handle) pointer

        reinterpret unmanaged (holding targetType (ConcreteTypeHandle.Byref int64Handle) ManagedPointerSource.Null)
        |> shouldEqual (
            Error (
                CliReinterpretRefusal.PointerBetweenTypes (
                    0,
                    ConcreteTypeHandle.Pointer int32Handle,
                    ConcreteTypeHandle.Byref int64Handle
                )
            )
        )

        let moved =
            reinterpret
                unmanaged
                (holding targetType (ConcreteTypeHandle.Pointer int32Handle) ManagedPointerSource.Null)

        moved
        |> Result.map (fieldNamed "P")
        |> shouldEqual (Ok (CliType.RuntimePointer (CliRuntimePointer.Managed pointer)))

    /// A field moved whole holds its value only if no target field written after it overlays it:
    /// the replay that renders the value would put the later field's bytes on top.
    [<Test>]
    let ``a moved field overlaid by a later target field is refused, and one on top survives`` () =
        let handle = NativeIntSource.FieldHandlePtr 1234L
        let int64Handle = handleOf bct.Int64

        let source =
            explicitStruct
                sourceType
                16
                [
                    field "H" 0 (CliType.Numeric (CliNumericType.NativeInt handle)) intPtrHandle
                    field "L" 8 (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 5L))) int64Handle
                ]

        // A 16-byte struct whose one field is at offset 8, so its bytes [0, 8) are padding.
        let padded =
            explicitStruct
                nestedType
                16
                [
                    field "L" 8 (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))) int64Handle
                ]

        let handleField =
            field "H" 0 (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))) intPtrHandle

        let paddedField = field "P" 0 (CliType.ValueType padded) nestedType

        reinterpret source (explicitStruct targetType 16 [ handleField ; paddedField ])
        |> shouldEqual (Error (CliReinterpretRefusal.MovedFieldOverlaid 0))

        reinterpret source (explicitStruct targetType 16 [ paddedField ; handleField ])
        |> Result.map (fun value ->
            match value with
            | CliType.ValueType vt -> CliValueType.DereferenceFieldAt 0 8 vt
            | other -> failwith $"expected a value type, got %O{other}"
        )
        |> shouldEqual (Ok (CliType.Numeric (CliNumericType.NativeInt handle)))
