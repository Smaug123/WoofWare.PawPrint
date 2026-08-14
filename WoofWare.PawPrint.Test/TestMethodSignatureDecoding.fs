namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// An enum whose underlying type is not the default `int`, so that
/// `refuses an enum, whatever its underlying type` covers two widths rather than one. Declared
/// here because corelib's enums are almost all Int32-backed.
type ByteBackedEnum =
    | Zero = 0uy
    | One = 1uy

/// <summary>
/// Tests for <see cref="MethodSignatureDecoding" />, the decoder for a MethodDefSig blob that
/// arrived without a metadata row to own it.
/// </summary>
/// <remarks>
/// The oracle throughout is the real <c>System.Reflection.Emit.SignatureHelper</c>, driven with a
/// null module so that it takes exactly the branch <c>DynamicMethod</c> drives it down. That
/// matters: the interesting claims this module makes are about which types
/// <c>SignatureHelper</c> can spell and how, and reading those off the encoder rather than
/// asserting them by hand is what stops the tests agreeing with a wrong belief. It is the same
/// discipline as decoding against a real image rather than a hand-built byte array -- an
/// expectation taken from outside the code under test.
/// </remarks>
[<TestFixture>]
module TestMethodSignatureDecoding =

    /// A real metadata reader for the decoder to resolve tokens against. A blob whose types are
    /// all simple element types never consults it, but the entry point requires one, and
    /// supplying a genuine one means the "not consulted" claim is tested rather than assumed: a
    /// decoder that *did* reach for a token would find a real table and could quietly succeed
    /// against the wrong row, which is exactly the failure a stub reader would mask.
    let private corelibReader : PEReader =
        new PEReader (File.OpenRead typeof<obj>.Assembly.Location)

    let private metadataReader : MetadataReader = corelibReader.GetMetadataReader ()

    let private assemblyName : AssemblyName =
        metadataReader.GetAssemblyDefinition().GetAssemblyName ()

    /// The null module is the whole point: it is what makes `SignatureHelper` take the
    /// `ELEMENT_TYPE_INTERNAL` branch for anything it cannot spell as a simple element type, and
    /// so it is what makes these blobs the ones `DynamicMethod` actually produces.
    let private nullModule : Module = null

    /// Build a MethodDefSig blob exactly as `DynamicMethod` does.
    ///
    /// The trailing `ELEMENT_TYPE_END` is not decoration: `DynamicMethod.GetILGenerator` builds
    /// its signature with the internal `SignatureHelper.GetSignature(true)`
    /// (DynamicMethod.CoreCLR.cs:161-162), and that `true` means "append ELEMENT_TYPE_END"
    /// (SignatureHelper.cs:870-881). Only the `false` overload is public, so appending the byte
    /// here is what makes these blobs the ones PawPrint is actually handed rather than a shape no
    /// dynamic method ever produces. `both the trailing-END and bare forms decode` pins that the
    /// two differ by exactly this byte, so this cannot silently stop reproducing the real thing.
    let private encode (returnType : Type) (parameterTypes : Type list) : byte[] =
        let helper =
            SignatureHelper.GetMethodSigHelper (nullModule, returnType, List.toArray parameterTypes)

        Array.append (helper.GetSignature ()) [| 0x00uy |]

    /// The same blob without the appended terminator: what the public `GetSignature()` returns,
    /// and a perfectly valid MethodDefSig in its own right (ECMA-335 II.23.2.1 has no trailing
    /// END).
    let private encodeBare (returnType : Type) (parameterTypes : Type list) : byte[] =
        SignatureHelper.GetMethodSigHelper(nullModule, returnType, List.toArray parameterTypes).GetSignature ()

    let private decode (blob : byte[]) : MethodSignature<TypeDefn> =
        MethodSignatureDecoding.decode assemblyName metadataReader blob

    /// The types `SignatureHelper.IsSimpleType` accepts, paired with the `TypeDefn` each must
    /// decode to. `void` is absent because it is legal only as a return type; it is covered
    /// separately.
    let private simpleAlphabet : (Type * TypeDefn) list =
        [
            typeof<bool>, TypeDefn.PrimitiveType PrimitiveType.Boolean
            typeof<char>, TypeDefn.PrimitiveType PrimitiveType.Char
            typeof<sbyte>, TypeDefn.PrimitiveType PrimitiveType.SByte
            typeof<byte>, TypeDefn.PrimitiveType PrimitiveType.Byte
            typeof<int16>, TypeDefn.PrimitiveType PrimitiveType.Int16
            typeof<uint16>, TypeDefn.PrimitiveType PrimitiveType.UInt16
            typeof<int32>, TypeDefn.PrimitiveType PrimitiveType.Int32
            typeof<uint32>, TypeDefn.PrimitiveType PrimitiveType.UInt32
            typeof<int64>, TypeDefn.PrimitiveType PrimitiveType.Int64
            typeof<uint64>, TypeDefn.PrimitiveType PrimitiveType.UInt64
            typeof<single>, TypeDefn.PrimitiveType PrimitiveType.Single
            typeof<double>, TypeDefn.PrimitiveType PrimitiveType.Double
            typeof<string>, TypeDefn.PrimitiveType PrimitiveType.String
            typeof<obj>, TypeDefn.PrimitiveType PrimitiveType.Object
            typeof<nativeint>, TypeDefn.PrimitiveType PrimitiveType.IntPtr
            typeof<unativeint>, TypeDefn.PrimitiveType PrimitiveType.UIntPtr
        ]

    [<Test>]
    let ``every simple type round-trips through the real encoder`` () : unit =
        for clrType, expected in simpleAlphabet do
            let decoded = decode (encode clrType [ clrType ])

            decoded.ReturnType |> shouldEqual expected
            decoded.ParameterTypes |> Seq.toList |> shouldEqual [ expected ]

    [<Test>]
    let ``a void return decodes to Void`` () : unit =
        let decoded = decode (encode typeof<Void> [ typeof<int> ])

        decoded.ReturnType |> shouldEqual TypeDefn.Void

        decoded.ParameterTypes
        |> Seq.toList
        |> shouldEqual [ TypeDefn.PrimitiveType PrimitiveType.Int32 ]

    /// The shape the whole dynamic-IL workstream is aimed at: `int -> int`, the signature of the
    /// doubling method the end-to-end guest builds.
    [<Test>]
    let ``the doubling method's signature decodes`` () : unit =
        let decoded = decode (encode typeof<int> [ typeof<int> ])

        decoded.ReturnType |> shouldEqual (TypeDefn.PrimitiveType PrimitiveType.Int32)

        decoded.ParameterTypes
        |> Seq.toList
        |> shouldEqual [ TypeDefn.PrimitiveType PrimitiveType.Int32 ]

        decoded.RequiredParameterCount |> shouldEqual 1
        decoded.GenericParameterCount |> shouldEqual 0

    /// Parameter *order* and *arity* are the two things a decoder can get subtly wrong while
    /// every single-parameter test still passes, so sweep both. The alphabet is the one the
    /// encoder actually admits, which is a real restriction rather than one chosen to make the
    /// property hold: `refuses a type SignatureHelper cannot spell` below pins where the boundary
    /// is.
    [<Test>]
    let ``parameter lists round-trip in order and arity`` () : unit =
        let config = Config.QuickThrowOnFailure.WithMaxTest 300

        let gen : Gen<(Type * TypeDefn) * ((Type * TypeDefn) list)> =
            gen {
                let! ret = Gen.elements simpleAlphabet
                // Up to eight parameters: enough that a decoder mishandling the compressed
                // parameter count, or reversing the list, cannot pass by luck.
                let! count = Gen.choose (0, 8)
                let! parameters = Gen.listOfLength count (Gen.elements simpleAlphabet)
                return ret, parameters
            }

        let property ((ret, parameters) : (Type * TypeDefn) * ((Type * TypeDefn) list)) : bool =
            let decoded = decode (encode (fst ret) (List.map fst parameters))

            decoded.ReturnType = snd ret
            && (decoded.ParameterTypes |> Seq.toList) = List.map snd parameters
            && decoded.RequiredParameterCount = parameters.Length

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)

    /// An enum takes the `ELEMENT_TYPE_INTERNAL` branch like any other value type, and is
    /// therefore refused.
    ///
    /// This one is measured, and the measurement overturned the obvious guess. `IsSimpleType`
    /// classifies by *element type*, and `RuntimeType.GetCorElementType()` for an enum is widely
    /// (and in other CoreCLR contexts correctly) said to be the underlying integer's -- which
    /// would make an enum parameter arrive here indistinguishable from an `int`. It does not: the
    /// encoder spells it `ELEMENT_TYPE_INTERNAL`. Both underlying widths are checked, so this
    /// cannot be passing for a reason peculiar to Int32-backed enums.
    ///
    /// The distinction matters to whoever binds a delegate to such a method: "an enum parameter
    /// decodes to its underlying integer" would have meant enum-typed dynamic methods bind
    /// today, and they do not.
    [<Test>]
    let ``refuses an enum, whatever its underlying type`` () : unit =
        for enumType in [ typeof<DayOfWeek> ; typeof<ByteBackedEnum> ] do
            let blob = encode typeof<int> [ enumType ]

            // Check the premise, so this cannot pass because the encoder started spelling enums
            // some third way: 0x21 is ELEMENT_TYPE_INTERNAL.
            blob |> Array.contains 0x21uy |> shouldEqual true

            let exn = Assert.Throws<Exception> (fun () -> decode blob |> ignore)
            exn.Message |> shouldContainText "ELEMENT_TYPE_INTERNAL"

    /// The boundary of the alphabet, from the far side. A user-defined struct has element type
    /// `ELEMENT_TYPE_VALUETYPE`, which is not simple, so with a null module `SignatureHelper`
    /// emits `ELEMENT_TYPE_INTERNAL` followed by a raw handle -- and that is not a signature
    /// `MetadataReader` can read.
    [<Test>]
    let ``refuses a type SignatureHelper cannot spell`` () : unit =
        let blob = encode typeof<int> [ typeof<DateTime> ]

        // First check the premise, so that this test cannot pass because the encoder quietly
        // started spelling `DateTime` some other way: 0x21 is ELEMENT_TYPE_INTERNAL.
        blob |> Array.contains 0x21uy |> shouldEqual true

        let exn = Assert.Throws<Exception> (fun () -> decode blob |> ignore)

        exn.Message |> shouldContainText "ELEMENT_TYPE_INTERNAL"
        exn.InnerException |> shouldNotEqual null

    [<Test>]
    let ``refuses an empty blob`` () : unit =
        let exn = Assert.Throws<Exception> (fun () -> decode [||] |> ignore)

        exn.Message |> shouldContainText "empty"

    /// A LocalVarSig and a MethodDefSig differ only in their leading calling-convention byte, and
    /// `DynamicResolver` hands PawPrint both. Decoding one as the other would read a *type* as a
    /// parameter count and answer confidently, so the header is checked before decoding starts.
    [<Test>]
    let ``refuses a local variable signature`` () : unit =
        // LOCAL_SIG (0x07), one local, of type int32.
        let localSig = [| 0x07uy ; 0x01uy ; 0x08uy |]

        let exn = Assert.Throws<Exception> (fun () -> decode localSig |> ignore)

        exn.Message |> shouldContainText "LocalVariables"

    /// A vararg signature decodes rather than being rejected: `RequiredParameterCount` says where
    /// the fixed parameters stop. Pinned because `MethodSignatureDecoding` deliberately declines
    /// to filter such signatures -- the decoder's job is to say what the blob contains, and
    /// whether PawPrint can bind a delegate to a vararg method is the *binder's* question. If
    /// that policy is ever moved into the decoder, this test is the one that should change.
    [<Test>]
    let ``a vararg signature decodes faithfully`` () : unit =
        let helper =
            SignatureHelper.GetMethodSigHelper (nullModule, CallingConventions.VarArgs, typeof<int>)

        helper.AddArgument typeof<int>
        helper.AddSentinel ()
        helper.AddArgument typeof<string>

        let decoded = decode (helper.GetSignature ())

        decoded.Header.CallingConvention
        |> shouldEqual SignatureCallingConvention.VarArgs

        decoded.RequiredParameterCount |> shouldEqual 1

        decoded.ParameterTypes
        |> Seq.toList
        |> shouldEqual
            [
                TypeDefn.PrimitiveType PrimitiveType.Int32
                TypeDefn.PrimitiveType PrimitiveType.String
            ]

    /// The same policy claim as above, for generic arity. Hand-built, because `SignatureHelper`
    /// has no way to emit a generic method signature: the calling convention is
    /// `IMAGE_CEE_CS_CALLCONV_GENERIC` (0x10), followed by the arity, then the parameter count.
    [<Test>]
    let ``a generic method signature decodes faithfully`` () : unit =
        // GENERIC (0x10), arity 2, one parameter, returning !!0, taking !!1.
        let genericSig = [| 0x10uy ; 0x02uy ; 0x01uy ; 0x1Euy ; 0x00uy ; 0x1Euy ; 0x01uy |]

        let decoded = decode genericSig

        decoded.GenericParameterCount |> shouldEqual 2
        decoded.ReturnType |> shouldEqual (TypeDefn.GenericMethodParameter 0)

        decoded.ParameterTypes
        |> Seq.toList
        |> shouldEqual [ TypeDefn.GenericMethodParameter 1 ]

    /// Arrays, byrefs and pointers are *not* covered by the simple-type rule, and are spellable
    /// anyway: `AddOneArgTypeHelperWorker` tests `IsByRef`/`IsPointer`/`IsArray` before it ever
    /// reaches `IsSimpleType` (SignatureHelper.cs:392-424), emitting the element type and
    /// recursing. So the alphabet is closed under those constructors rather than being just the
    /// sixteen simple types -- which matters, because a dynamic method taking `int[]` is an
    /// entirely ordinary thing to emit, and reading the summary as "simple types only" would say
    /// it cannot bind.
    ///
    /// The nesting is what makes this non-vacuous: a decoder that dropped one constructor level
    /// would still answer for the single-level cases.
    [<Test>]
    let ``the alphabet is closed under array, byref and pointer`` () : unit =
        let int32Defn = TypeDefn.PrimitiveType PrimitiveType.Int32

        let cases : (Type * TypeDefn) list =
            [
                typeof<int[]>, TypeDefn.OneDimensionalArrayLowerBoundZero int32Defn
                typeof<int[][]>,
                TypeDefn.OneDimensionalArrayLowerBoundZero (TypeDefn.OneDimensionalArrayLowerBoundZero int32Defn)
                typeof<string[]>,
                TypeDefn.OneDimensionalArrayLowerBoundZero (TypeDefn.PrimitiveType PrimitiveType.String)
                typeof<int>.MakeByRefType (), TypeDefn.Byref int32Defn
                typeof<int>.MakePointerType (), TypeDefn.Pointer int32Defn
                typeof<int>.MakePointerType().MakePointerType (), TypeDefn.Pointer (TypeDefn.Pointer int32Defn)
            ]

        for clrType, expected in cases do
            let decoded = decode (encode typeof<int> [ clrType ])

            decoded.ParameterTypes |> Seq.toList |> shouldEqual [ expected ]

    /// The other half of that claim: an array *of* something unspellable is still unspellable, so
    /// the closure is over the alphabet rather than a blanket exemption for arrays.
    [<Test>]
    let ``an array of an unspellable type is still refused`` () : unit =
        let blob = encode typeof<int> [ typeof<DateTime[]> ]

        blob |> Array.contains 0x21uy |> shouldEqual true

        let exn = Assert.Throws<Exception> (fun () -> decode blob |> ignore)
        exn.Message |> shouldContainText "ELEMENT_TYPE_INTERNAL"

    /// The two encoder modes differ by exactly one `ELEMENT_TYPE_END` byte, and both decode to the
    /// same thing. This is what keeps `encode` honest: if the public `GetSignature()` ever started
    /// appending the terminator itself, `encode` would be producing a double-terminated blob that
    /// no dynamic method emits, and this test is what notices.
    [<Test>]
    let ``both the trailing-END and bare forms decode`` () : unit =
        let withEnd = encode typeof<int> [ typeof<int> ; typeof<string> ]
        let bare = encodeBare typeof<int> [ typeof<int> ; typeof<string> ]

        withEnd |> shouldEqual (Array.append bare [| 0x00uy |])

        let expected =
            [
                TypeDefn.PrimitiveType PrimitiveType.Int32
                TypeDefn.PrimitiveType PrimitiveType.String
            ]

        (decode withEnd).ParameterTypes |> Seq.toList |> shouldEqual expected
        (decode bare).ParameterTypes |> Seq.toList |> shouldEqual expected

    /// A valid signature followed by junk is not a valid blob. `DecodeMethodSignature` stops after
    /// the declared parameters, so without an explicit check the tail is silently dropped and a
    /// concatenated or corrupt blob decodes as though it were the first signature in it -- and
    /// these bytes come from guest memory, so that is a real hazard rather than a theoretical one.
    [<Test>]
    let ``refuses trailing bytes after a complete signature`` () : unit =
        let valid = encodeBare typeof<int> [ typeof<int> ]

        // Two extra bytes: one over the single ELEMENT_TYPE_END that is legitimately tolerated.
        let exn =
            Assert.Throws<Exception> (fun () -> decode (Array.append valid [| 0x00uy ; 0x00uy |]) |> ignore)

        exn.Message |> shouldContainText "bytes left over"

        // ...and a single trailing byte that is *not* ELEMENT_TYPE_END is refused too, so the
        // tolerance is for the terminator specifically rather than for "one spare byte".
        let exn =
            Assert.Throws<Exception> (fun () -> decode (Array.append valid [| 0x08uy |]) |> ignore)

        exn.Message |> shouldContainText "ELEMENT_TYPE_END"

    /// A blob claiming more parameters than it carries is truncated, not merely short: the
    /// decoder must not silently return the parameters it managed to read. Distinct from the
    /// trailing-bytes case above, and reached through a different failure inside SRM.
    [<Test>]
    let ``refuses a truncated signature`` () : unit =
        // DEFAULT calling convention, 3 parameters declared, int32 return, only one parameter.
        let truncated = [| 0x00uy ; 0x03uy ; 0x08uy ; 0x08uy |]

        Assert.Throws<Exception> (fun () -> decode truncated |> ignore) |> ignore

    /// A blob declaring more parameters than it has bytes to spell them in is truncated, and must
    /// say so. `SignatureDecoder` already refuses it -- promptly, and without the large allocation
    /// one might expect from a length prefix -- but it refuses it with a bare
    /// `BadImageFormatException`, which this module's catch reports as the likely
    /// `ELEMENT_TYPE_INTERNAL` cause. That message points at a missing encoding rather than at a
    /// truncated blob, so the check exists to name the real problem.
    ///
    /// The assertion is therefore on *which* diagnostic comes back, not on whether one does:
    /// remove the bound and this still throws, just misleadingly.
    [<Test>]
    let ``a parameter count exceeding the blob is reported as truncation`` () : unit =
        // DEFAULT calling convention, then 0x1FFFFFFF as a four-byte compressed integer, then a
        // void return. 536,870,911 parameters, in a six-byte blob.
        let absurd = [| 0x00uy ; 0xDFuy ; 0xFFuy ; 0xFFuy ; 0xFFuy ; 0x01uy |]

        let exn = Assert.Throws<Exception> (fun () -> decode absurd |> ignore)

        exn.Message |> shouldContainText "truncated or corrupt"
        exn.Message |> shouldContainText "536870911"

    /// The bound must not reject legitimate signatures at the boundary. A blob whose parameters
    /// are exactly one byte each is the tightest a real signature gets, so it is where an
    /// off-by-one in the bound would show up.
    [<Test>]
    let ``the parameter-count bound admits a maximally tight signature`` () : unit =
        let parameters = List.replicate 8 typeof<int>

        let decoded = decode (encodeBare typeof<int> parameters)

        decoded.ParameterTypes.Length |> shouldEqual 8
