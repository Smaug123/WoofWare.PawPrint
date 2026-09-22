// Native pointers are how the host runtime's own FCall has to be called: its string out-params
// are `byte*`.
#nowarn "9"
#nowarn "51"

namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.Reflection
open System.Text
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open Microsoft.FSharp.NativeInterop
open NUnit.Framework
open WoofWare.PawPrint

/// Everything one call to `MetaDataImport::GetMarshalAs` is observable by, with its three string
/// pointers given as offsets from the start of the blob (`None` for null). `None` overall means the
/// call returned FALSE having written none of its out-params.
type MarshalAsOutcome =
    {
        UnmanagedType : int
        SafeArraySubType : int
        SafeArrayUserDefinedSubType : int64 option
        ArraySubType : int
        SizeParamIndex : int
        SizeConst : int
        MarshalType : int64 option
        MarshalCookie : int64 option
        IidParamIndex : int
    }

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNativeTypeParamInfo =

    /// The FCall's signature with each `byte*&` spelled `nativeint&`. F# cannot spell a byref to a
    /// pointer (`byref<nativeptr<byte>>` compiles to `IntPtr&`), so the delegate cannot bind to the
    /// FCall directly and `hostGetMarshalAs` bridges the two with a trampoline.
    type private GetMarshalAsDelegate =
        delegate of
            nativeint *
            int *
            byref<int> *
            byref<int> *
            byref<nativeint> *
            byref<int> *
            byref<int> *
            byref<int> *
            byref<nativeint> *
            byref<nativeint> *
            byref<int> ->
                bool

    /// The host runtime's *own* `MetadataImport.GetMarshalAs` FCall — the primitive PawPrint's
    /// handler reimplements, reached by private reflection because CoreLib exposes it nowhere
    /// else. If a future runtime renames or reshapes it, this fails loudly.
    let private hostGetMarshalAs : Lazy<GetMarshalAsDelegate> =
        lazy
            let declaring =
                typeof<obj>.Assembly.GetType "System.Reflection.MetadataImport"
                |> Option.ofObj
                |> Option.defaultWith (fun () ->
                    failwith "System.Reflection.MetadataImport is not present in the host's corelib"
                )

            // The eleven-argument overload is the FCall; the two-argument one is its managed wrapper.
            let fcall =
                declaring.GetMethods (BindingFlags.NonPublic ||| BindingFlags.Static)
                |> Array.filter (fun m -> m.Name = "GetMarshalAs" && m.GetParameters().Length = 11)
                |> Array.tryExactlyOne
                |> Option.defaultWith (fun () ->
                    failwith
                        "the host's corelib has no eleven-argument System.Reflection.MetadataImport.GetMarshalAs; the oracle needs updating for this runtime"
                )

            // Forward all eleven arguments unchanged. `IntPtr&` and `byte*&` are the same thing
            // to the JIT; only the verifier, which a skip-visibility dynamic method bypasses,
            // would tell them apart.
            let parameterTypes =
                // F# gives a private delegate type's `Invoke` assembly visibility.
                typeof<GetMarshalAsDelegate>
                    .GetMethod("Invoke", BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
                    .GetParameters ()
                |> Array.map (fun p -> p.ParameterType)

            let trampoline =
                Emit.DynamicMethod (
                    "CallGetMarshalAs",
                    typeof<bool>,
                    parameterTypes,
                    typeof<GetMarshalAsDelegate>.Module,
                    true
                )

            let il = trampoline.GetILGenerator ()

            for i in 0 .. parameterTypes.Length - 1 do
                il.Emit (Emit.OpCodes.Ldarg, int16 i)

            il.Emit (Emit.OpCodes.Call, fcall)
            il.Emit Emit.OpCodes.Ret

            trampoline.CreateDelegate typeof<GetMarshalAsDelegate> :?> GetMarshalAsDelegate

    /// One call with every out-param seeded to values derived from `seed`: the result, the six
    /// integer out-params, the three pointer out-params as raw addresses, and the address the blob
    /// was pinned at for this call.
    let private hostCall (blob : byte array) (seed : int) : bool * int list * nativeint list * nativeint =
        let getMarshalAs = hostGetMarshalAs.Force ()
        // `fixed` on an empty array yields a null pointer, which the FCall never dereferences
        // because it refuses a zero-length blob first.
        use pinned = fixed blob
        let basis = NativePtr.toNativeInt pinned
        let mutable unmanagedType = seed
        let mutable safeArraySubType = seed + 1
        let mutable safeArrayUserDefinedSubType = nativeint (seed + 2)
        let mutable arraySubType = seed + 3
        let mutable sizeParamIndex = seed + 4
        let mutable sizeConst = seed + 5
        let mutable marshalType = nativeint (seed + 6)
        let mutable marshalCookie = nativeint (seed + 7)
        let mutable iidParamIndex = seed + 8

        let result =
            getMarshalAs.Invoke (
                basis,
                blob.Length,
                &unmanagedType,
                &safeArraySubType,
                &safeArrayUserDefinedSubType,
                &arraySubType,
                &sizeParamIndex,
                &sizeConst,
                &marshalType,
                &marshalCookie,
                &iidParamIndex
            )

        result,
        [
            unmanagedType
            safeArraySubType
            arraySubType
            sizeParamIndex
            sizeConst
            iidParamIndex
        ],
        [ safeArrayUserDefinedSubType ; marshalType ; marshalCookie ],
        basis

    /// The host's verdict. Two calls with different seeds tell "wrote nothing" apart from "wrote a
    /// value equal to the seed", and the FCall's contract is all-or-nothing: FALSE writes no
    /// out-param and TRUE writes every one.
    let private hostOutcome (blob : byte array) : MarshalAsOutcome option =
        let seedOf (seed : int) : int list * nativeint list =
            [ seed ; seed + 1 ; seed + 3 ; seed + 4 ; seed + 5 ; seed + 8 ],
            [ nativeint (seed + 2) ; nativeint (seed + 6) ; nativeint (seed + 7) ]

        let firstResult, firstInts, firstPointers, firstBasis = hostCall blob 0x5A5A5A00
        let secondResult, secondInts, secondPointers, secondBasis = hostCall blob 0x0F0F0F00

        if firstResult <> secondResult then
            failwith $"the host's GetMarshalAs is not deterministic on %s{BitConverter.ToString blob}"

        if not firstResult then
            if
                (firstInts, firstPointers) = seedOf 0x5A5A5A00
                && (secondInts, secondPointers) = seedOf 0x0F0F0F00
            then
                None
            else
                failwith
                    $"the host's GetMarshalAs returned FALSE on %s{BitConverter.ToString blob} but wrote an out-param"
        else

        // A written pointer is null or points into the blob, so it is compared as an offset from
        // wherever the blob was pinned for that call.
        let offsets (basis : nativeint) (pointers : nativeint list) : int64 option list =
            pointers
            |> List.map (fun p -> if p = 0n then None else Some (int64 (p - basis)))

        let firstOffsets = offsets firstBasis firstPointers

        if firstInts <> secondInts || firstOffsets <> offsets secondBasis secondPointers then
            failwith
                $"the host's GetMarshalAs returned TRUE on %s{BitConverter.ToString blob} but left an out-param unwritten"

        match firstInts, firstOffsets with
        | [ unmanagedType ; safeArraySubType ; arraySubType ; sizeParamIndex ; sizeConst ; iidParamIndex ],
          [ safeArrayUserDefinedSubType ; marshalType ; marshalCookie ] ->
            Some
                {
                    UnmanagedType = unmanagedType
                    SafeArraySubType = safeArraySubType
                    SafeArrayUserDefinedSubType = safeArrayUserDefinedSubType
                    ArraySubType = arraySubType
                    SizeParamIndex = sizeParamIndex
                    SizeConst = sizeConst
                    MarshalType = marshalType
                    MarshalCookie = marshalCookie
                    IidParamIndex = iidParamIndex
                }
        | _ -> failwith "unreachable: hostCall returns six ints and three pointers"

    /// PawPrint's verdict: the parse, mapped to out-params exactly as the `GetMarshalAs` handler
    /// writes them.
    let private ourOutcome (blob : byte array) : MarshalAsOutcome option =
        NativeTypeParamInfo.parse (ImmutableArray.CreateRange blob)
        |> Option.map (fun info ->
            {
                UnmanagedType = int info.NativeType
                SafeArraySubType = 0
                SafeArrayUserDefinedSubType = None
                ArraySubType = int info.ArrayElementType
                SizeParamIndex = int info.CountParamIndex
                SizeConst = int info.Additive
                MarshalType = info.MarshalerTypeName |> Option.map (fun s -> int64 s.Offset)
                MarshalCookie = info.Cookie |> Option.map (fun s -> int64 s.Offset)
                IidParamIndex = 0
            }
        )

    let private agreesWithHost (blob : byte array) : unit =
        // PawPrint models a CoreCLR without FEATURE_COMINTEROP, which is every non-Windows one. On
        // a Windows host the COM shapes parse differently, so the host is not this oracle there.
        if OperatingSystem.IsWindows () then
            Assert.Ignore "the host CLR has FEATURE_COMINTEROP, which the emulated platform does not"

        let ours = ourOutcome blob
        let theirs = hostOutcome blob

        if ours <> theirs then
            failwith
                $"blob [%s{BitConverter.ToString blob}]\n  PawPrint: %A{ours}\n  host:     %A{theirs}\n  (PawPrint's parse: %A{NativeTypeParamInfo.parse (ImmutableArray.CreateRange blob)})"

    // ---- the MarshalSpec model and its encoder -------------------------------

    /// An ECMA-335 II.23.2 compressed unsigned integer, written in `Width` bytes. A width larger
    /// than the value needs is a non-canonical encoding, which CoreCLR's decoder accepts.
    type private Compressed =
        {
            Value : uint32
            Width : int
        }

    let private encodeCompressed (c : Compressed) : byte array =
        match c.Width with
        | 1 when c.Value < 0x80u -> [| byte c.Value |]
        | 2 when c.Value < 0x4000u -> [| byte (0x80u ||| (c.Value >>> 8)) ; byte c.Value |]
        | 4 when c.Value < 0x20000000u ->
            [|
                byte (0xC0u ||| (c.Value >>> 24))
                byte (c.Value >>> 16)
                byte (c.Value >>> 8)
                byte c.Value
            |]
        | _ -> failwith $"cannot encode %d{c.Value} in %d{c.Width} bytes"

    /// A length-prefixed string, its prefix written in `PrefixWidth` bytes.
    type private PackedString =
        {
            Bytes : byte array
            PrefixWidth : int
        }

    let private encodePacked (s : PackedString) : byte array =
        Array.append
            (encodeCompressed
                {
                    Value = uint32 s.Bytes.Length
                    Width = s.PrefixWidth
                })
            s.Bytes

    /// A well-formed MarshalSpec, one case per shape `ParseNativeTypeInfo` reads beyond the leading
    /// byte, and one for all the shapes it does not.
    [<RequireQualifiedAccess>]
    type private MarshalSpec =
        | Simple of nativeType : byte
        | FixedSysString of size : Compressed
        | FixedArray of size : Compressed * elementType : Compressed option
        /// Element type, size-param index, additive, flags: each present only if all before it are.
        | Array of items : Compressed list
        | CustomMarshaler of
            guid : PackedString *
            nativeTypeName : PackedString *
            marshaler : PackedString *
            cookie : PackedString

    let private encode (spec : MarshalSpec) : byte array =
        match spec with
        | MarshalSpec.Simple nativeType -> [| nativeType |]
        | MarshalSpec.FixedSysString size -> Array.append [| 0x17uy |] (encodeCompressed size)
        | MarshalSpec.FixedArray (size, elementType) ->
            Array.concat
                [
                    [| 0x1Euy |]
                    encodeCompressed size
                    elementType |> Option.map encodeCompressed |> Option.defaultValue [||]
                ]
        | MarshalSpec.Array items -> Array.append [| 0x2Auy |] (items |> List.map encodeCompressed |> Array.concat)
        | MarshalSpec.CustomMarshaler (guid, nativeTypeName, marshaler, cookie) ->
            Array.concat
                [
                    [| 0x2Cuy |]
                    encodePacked guid
                    encodePacked nativeTypeName
                    encodePacked marshaler
                    encodePacked cookie
                ]

    /// Whether the spec reads nothing after its last item, so that appended bytes are ignored
    /// rather than read as a further item.
    let private isComplete (spec : MarshalSpec) : bool =
        match spec with
        | MarshalSpec.Simple _
        | MarshalSpec.FixedSysString _
        | MarshalSpec.FixedArray (_, Some _)
        | MarshalSpec.CustomMarshaler _ -> true
        | MarshalSpec.FixedArray (_, None) -> false
        | MarshalSpec.Array items -> items.Length = 4

    let private compressedGen : Gen<Compressed> =
        gen {
            // Values drawn across every width's range, not FsCheck's size-bounded default.
            let! value =
                Gen.frequency
                    [
                        4, Gen.choose (0, 0x7F)
                        2, Gen.choose (0x80, 0x3FFF)
                        1, Gen.choose (0x4000, 0x1FFFFFFF)
                    ]
                |> Gen.map uint32

            let minimal =
                if value < 0x80u then 1
                elif value < 0x4000u then 2
                else 4

            let! width = Gen.elements ([ 1 ; 2 ; 4 ] |> List.filter (fun w -> w >= minimal))

            return
                {
                    Value = value
                    Width = width
                }
        }

    let private packedGen : Gen<PackedString> =
        gen {
            let! length = Gen.frequency [ 2, Gen.constant 0 ; 5, Gen.choose (1, 20) ; 1, Gen.choose (0x80, 0x90) ]
            let! bytes = Gen.arrayOfLength length (Gen.choose (0, 255) |> Gen.map byte)

            let minimal = if length < 0x80 then 1 else 2
            let! width = Gen.elements ([ 1 ; 2 ; 4 ] |> List.filter (fun w -> w >= minimal))

            return
                {
                    Bytes = bytes
                    PrefixWidth = width
                }
        }

    let private specGen : Gen<MarshalSpec> =
        Gen.oneof
            [
                Gen.choose (0, 255)
                |> Gen.map byte
                |> Gen.filter (fun b -> b <> 0x17uy && b <> 0x1Euy && b <> 0x2Auy && b <> 0x2Cuy)
                |> Gen.map MarshalSpec.Simple
                compressedGen |> Gen.map MarshalSpec.FixedSysString
                Gen.map2
                    (fun size elementType -> MarshalSpec.FixedArray (size, elementType))
                    compressedGen
                    (Gen.optionOf compressedGen)
                gen {
                    let! count = Gen.choose (0, 4)
                    let! items = Gen.listOfLength count compressedGen
                    return MarshalSpec.Array items
                }
                gen {
                    let! guid = packedGen
                    let! nativeTypeName = packedGen
                    let! marshaler = packedGen
                    let! cookie = packedGen
                    return MarshalSpec.CustomMarshaler (guid, nativeTypeName, marshaler, cookie)
                }
            ]

    let private junkGen : Gen<byte array> =
        Gen.choose (0, 6)
        |> Gen.bind (fun n -> Gen.arrayOfLength n (Gen.choose (0, 255) |> Gen.map byte))

    /// What `ParseNativeTypeInfo` must report for a well-formed spec, stated from the model rather
    /// than from the bytes.
    let private expected (spec : MarshalSpec) : NativeTypeParamInfo =
        let zero (nativeType : byte) : NativeTypeParamInfo =
            {
                NativeType = nativeType
                ArrayElementType = 0u
                CountParamIndex = 0us
                Additive = 0u
                MarshalerTypeName = None
                Cookie = None
            }

        match spec with
        | MarshalSpec.Simple nativeType -> zero nativeType
        | MarshalSpec.FixedSysString size ->
            { zero 0x17uy with
                Additive = size.Value
            }
        | MarshalSpec.FixedArray (size, elementType) ->
            { zero 0x1Euy with
                Additive = size.Value
                ArrayElementType = elementType |> Option.map (fun e -> e.Value) |> Option.defaultValue 0u
            }
        | MarshalSpec.Array items ->
            let item (index : int) : uint32 =
                List.tryItem index items
                |> Option.map (fun c -> c.Value)
                |> Option.defaultValue 0u

            { zero 0x2Auy with
                ArrayElementType = item 0
                CountParamIndex = uint16 (item 1 &&& 0xFFFFu)
                Additive = item 2
            }
        | MarshalSpec.CustomMarshaler (guid, nativeTypeName, marshaler, cookie) ->
            let afterGuid = 1 + guid.PrefixWidth + guid.Bytes.Length

            let afterNativeTypeName =
                afterGuid + nativeTypeName.PrefixWidth + nativeTypeName.Bytes.Length

            let marshalerOffset = afterNativeTypeName + marshaler.PrefixWidth
            let cookieOffset = marshalerOffset + marshaler.Bytes.Length + cookie.PrefixWidth

            { zero 0x2Cuy with
                MarshalerTypeName =
                    Some
                        {
                            Offset = marshalerOffset
                            Length = marshaler.Bytes.Length
                        }
                Cookie =
                    Some
                        {
                            Offset = cookieOffset
                            Length = cookie.Bytes.Length
                        }
            }

    // ---- properties ----------------------------------------------------------

    /// Round trip: every well-formed spec parses to what the model says, and the two strings'
    /// offsets and lengths select exactly the bytes the encoder wrote. Bytes appended to a spec that
    /// reads nothing further do not change the answer.
    [<Test>]
    let ``parse recovers every well-formed spec from its encoding`` () : unit =
        let property (spec : MarshalSpec, junk : byte array) : unit =
            let junk = if isComplete spec then junk else [||]
            let blob = Array.append (encode spec) junk
            let parsed = NativeTypeParamInfo.parse (ImmutableArray.CreateRange blob)

            parsed |> shouldEqual (Some (expected spec))

            match spec, parsed with
            | MarshalSpec.CustomMarshaler (_, _, marshaler, cookie), Some info ->
                let slice (s : MarshalSpecString option) : byte array =
                    let s = Option.get s
                    Array.sub blob s.Offset s.Length

                slice info.MarshalerTypeName |> shouldEqual marshaler.Bytes
                slice info.Cookie |> shouldEqual cookie.Bytes
            | _ -> ()

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 2000,
            Prop.forAll (Arb.fromGen (Gen.zip specGen junkGen)) property
        )

    /// Well-formed specs, then truncated, extended or with one byte replaced some of the time, to
    /// reach every failure arm.
    let private perturbedGen : Gen<byte array> =
        gen {
            let! spec = specGen
            let blob = encode spec

            let! action =
                Gen.frequency
                    [
                        3, Gen.constant 0
                        3, Gen.constant 1
                        2, Gen.constant 2
                        2, Gen.constant 3
                    ]

            match action with
            | 1 ->
                let! keep = Gen.choose (0, blob.Length)
                return Array.sub blob 0 keep
            | 2 ->
                let! junk = junkGen
                return Array.append blob junk
            | 3 ->
                let! index = Gen.choose (0, blob.Length - 1)
                // Bias towards the bytes that change a compressed integer's announced width.
                let! replacement =
                    Gen.frequency [ 1, Gen.elements [ 0x80 ; 0xC0 ; 0xE0 ; 0xFF ] ; 1, Gen.choose (0, 255) ]

                let copy = Array.copy blob
                copy.[index] <- byte replacement
                return copy
            | _ -> return blob
        }

    /// The strongest statement available: on any byte string, PawPrint's parse maps to exactly what
    /// the host runtime's own FCall reports — the verdict, every number, and where each string
    /// pointer points.
    [<Test>]
    let ``agrees with the host runtime on generated blobs`` () : unit =
        Check.One (Config.QuickThrowOnFailure.WithMaxTest 3000, Prop.forAll (Arb.fromGen perturbedGen) agreesWithHost)

    /// The same, over short byte strings with no structure at all beyond a leading byte biased
    /// towards the four shapes that read further.
    [<Test>]
    let ``agrees with the host runtime on arbitrary short blobs`` () : unit =
        let arbitraryGen : Gen<byte array> =
            gen {
                let! lead =
                    Gen.frequency
                        [
                            4, Gen.elements [ 0x17uy ; 0x1Euy ; 0x2Auy ; 0x2Cuy ]
                            1, Gen.choose (0, 255) |> Gen.map byte
                        ]

                let! rest =
                    Gen.choose (0, 10)
                    |> Gen.bind (fun n -> Gen.arrayOfLength n (Gen.choose (0, 255) |> Gen.map byte))

                return Array.append [| lead |] rest
            }

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 3000, Prop.forAll (Arb.fromGen arbitraryGen) agreesWithHost)

    // ---- CoreCLR's specific choices, named -----------------------------------

    [<Test>]
    let ``an empty blob fails`` () : unit =
        NativeTypeParamInfo.parse ImmutableArray.Empty |> shouldEqual None
        agreesWithHost [||]

    [<Test>]
    let ``absent optional fields are zero, not the NativeTypeParamInfo constructor's defaults`` () : unit =
        // The FCall zeroes the struct before parsing, so an LPArray with nothing after its leading
        // byte reports `ArraySubType = 0` rather than NATIVE_TYPE_DEFAULT (0x50), and `SizeConst = 0`
        // rather than the constructor's 1.
        let parsed = NativeTypeParamInfo.parse (ImmutableArray.Create 0x2Auy) |> Option.get
        parsed.ArrayElementType |> shouldEqual 0u
        parsed.Additive |> shouldEqual 0u
        parsed.CountParamIndex |> shouldEqual 0us
        agreesWithHost [| 0x2Auy |]

    [<Test>]
    let ``a size-param index wider than 16 bits is truncated`` () : unit =
        // 0x01020304 in four bytes; CoreCLR's `m_CountParamIdx` is a UINT16.
        let blob = [| 0x2Auy ; 0x50uy ; 0xC1uy ; 0x02uy ; 0x03uy ; 0x04uy |]

        let parsed =
            NativeTypeParamInfo.parse (ImmutableArray.CreateRange blob) |> Option.get

        parsed.CountParamIndex |> shouldEqual 0x0304us
        agreesWithHost blob

    [<Test>]
    let ``a malformed FixedArray element type still succeeds`` () : unit =
        // mlinfo.cpp's one `return TRUE` on a failed check: the size is kept and the element type
        // stays zero.
        let blob = [| 0x1Euy ; 0x04uy ; 0xE0uy |]

        NativeTypeParamInfo.parse (ImmutableArray.CreateRange blob)
        |> Option.map (fun info -> info.Additive, info.ArrayElementType)
        |> shouldEqual (Some (4u, 0u))

        agreesWithHost blob

    [<Test>]
    let ``a FixedArray or FixedSysString without its size fails`` () : unit =
        NativeTypeParamInfo.parse (ImmutableArray.Create 0x1Euy) |> shouldEqual None
        NativeTypeParamInfo.parse (ImmutableArray.Create 0x17uy) |> shouldEqual None
        agreesWithHost [| 0x1Euy |]
        agreesWithHost [| 0x17uy |]

    [<Test>]
    let ``a CustomMarshaler missing its cookie fails`` () : unit =
        let blob =
            Array.concat [ [| 0x2Cuy ; 0x00uy ; 0x00uy ; 0x01uy |] ; Encoding.UTF8.GetBytes "M" ]

        NativeTypeParamInfo.parse (ImmutableArray.CreateRange blob) |> shouldEqual None
        agreesWithHost blob

    [<Test>]
    let ``a CustomMarshaler string longer than the rest of the blob fails`` () : unit =
        // The cookie claims five bytes and one follows; `SafeGetData` refuses that rather than
        // handing back a string that runs off the blob. The cookie rather than an earlier string,
        // because an earlier one's overrun would also leave the next length prefix out of bounds.
        let blob = [| 0x2Cuy ; 0x00uy ; 0x00uy ; 0x01uy ; byte 'M' ; 0x05uy ; byte 'c' |]
        NativeTypeParamInfo.parse (ImmutableArray.CreateRange blob) |> shouldEqual None
        agreesWithHost blob

    [<Test>]
    let ``COM shapes read nothing beyond their leading byte`` () : unit =
        // NATIVE_TYPE_INTF with an IID parameter index, and NATIVE_TYPE_SAFEARRAY with a VARTYPE:
        // both are FEATURE_COMINTEROP arms, absent from the platform PawPrint emulates.
        for blob in [ [| 0x1Cuy ; 0x03uy |] ; [| 0x1Duy ; 0x03uy |] ] do
            NativeTypeParamInfo.parse (ImmutableArray.CreateRange blob)
            |> shouldEqual (
                Some
                    {
                        NativeType = blob.[0]
                        ArrayElementType = 0u
                        CountParamIndex = 0us
                        Additive = 0u
                        MarshalerTypeName = None
                        Cookie = None
                    }
            )

            agreesWithHost blob
