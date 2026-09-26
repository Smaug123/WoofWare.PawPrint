namespace WoofWare.PawPrint.Test

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Emit
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// An enum whose underlying type is not the default `int`, so that the enum cases cover two widths
/// rather than one. Declared here because corelib's enums are almost all Int32-backed.
type ByteBackedEnum =
    | Zero = 0uy
    | One = 1uy

/// <summary>
/// Tests for <see cref="DynamicSignatureDecoding" />, the decoder for the signature blobs a
/// <c>DynamicMethod</c> hands the runtime.
/// </summary>
/// <remarks>
/// <para>
/// The oracle throughout is the real <c>System.Reflection.Emit.SignatureHelper</c>, driven with a
/// null module so that it takes exactly the branches <c>DynamicMethod</c> drives it down. Where it
/// writes <c>ELEMENT_TYPE_INTERNAL</c> it copies the eight bytes of the host's own type handle;
/// <c>symbolise</c> replaces each such run with eight bytes naming the same type the way PawPrint's
/// guest memory holds them, which is the only difference between a host blob and a guest one.
/// </para>
/// <para>
/// The handle each run names is <c>RuntimeTypeHandleTarget.Closed (Concrete k)</c> for the type at
/// index <c>k</c> of <c>internalUniverse</c>, and <c>internalType</c> answers for it from the host
/// type. So these tests pin the walker; turning a real PawPrint handle into an identity and kind
/// is the interpreter's half, exercised by the guests.
/// </para>
/// </remarks>
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDynamicSignatureDecoding =

    /// Makes `SignatureHelper` take the `ELEMENT_TYPE_INTERNAL` branch for anything it cannot spell
    /// as a simple element type, which is what makes these blobs the ones `DynamicMethod` produces.
    let private nullModule : Module = null

    /// Looked up by name: F# restricts byref-like types as type arguments, `typeof` included.
    let private typedReference : Type =
        typeof<obj>.Assembly.GetType "System.TypedReference"

    let private identityOf (t : Type) : ResolvedTypeIdentity =
        ResolvedTypeIdentity.ofTypeDefinition
            (t.Assembly.GetName ())
            (MetadataTokens.TypeDefinitionHandle t.MetadataToken)

    let private kindOf (t : Type) : SignatureTypeKind =
        if t.IsValueType then
            SignatureTypeKind.ValueType
        else
            SignatureTypeKind.Class

    /// Every type an `ELEMENT_TYPE_INTERNAL` run in these tests may name: the non-generic types and
    /// the generic definitions the generator draws from.
    let private internalUniverse : Type list =
        [
            typeof<DateTime>
            typeof<Guid>
            typeof<DayOfWeek>
            typeof<ByteBackedEnum>
            typeof<Uri>
            typeof<Exception>
            typeof<Version>
            typeof<Environment.SpecialFolder>
            typedefof<List<obj>>
            typedefof<Dictionary<obj, obj>>
            typedefof<KeyValuePair<obj, obj>>
            typedefof<Func<obj, obj>>
            typedefof<ValueTuple<obj, obj, obj>>
            typedefof<Nullable<int>>
        ]

    let private handleFor (index : int) : NativeIntSource =
        NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete index))

    let private internalRun (index : int) : UInt8Source list =
        [ for i in 0..7 -> UInt8Source.NativeIntByte (handleFor index, i) ]

    let private internalType (source : NativeIntSource) : InternalSignatureType =
        match source with
        | NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete index)) ->
            let t = internalUniverse.[index]

            if t.IsGenericTypeDefinition then
                InternalSignatureType.GenericDefinition (identityOf t, kindOf t, t.GetGenericArguments().Length)
            else
                InternalSignatureType.NonGeneric (identityOf t, kindOf t)
        | other -> failwith $"test harness: the walker asked about %O{other}, which no run in these tests names"

    /// The host blob with every `ELEMENT_TYPE_INTERNAL` run replaced by a symbolic one, and how many
    /// runs were replaced.
    ///
    /// Found by content rather than by parsing, so that the harness shares no logic with the walker
    /// under test: a run is `0x21` followed by the eight bytes of a universe type's handle, and eight
    /// bytes of a live pointer do not recur by accident.
    let private symbolise (blob : byte[]) : UInt8Source[] * int =
        let result = blob |> Array.map UInt8Source.Verbatim
        let mutable replaced = 0

        internalUniverse
        |> List.iteri (fun index t ->
            let needle =
                Array.append [| 0x21uy |] (BitConverter.GetBytes (t.TypeHandle.Value.ToInt64 ()))

            for start in 0 .. blob.Length - needle.Length do
                if blob.[start .. start + needle.Length - 1] = needle then
                    internalRun index |> List.iteri (fun i b -> result.[start + 1 + i] <- b)
                    replaced <- replaced + 1
        )

        result, replaced

    let private decodeMethod (blob : UInt8Source[]) : MethodSignature<TypeDefn> =
        DynamicSignatureDecoding.decodeMethod internalType (ImmutableArray.CreateRange blob)

    let private decodeLocals (blob : UInt8Source[]) : TypeDefn list =
        DynamicSignatureDecoding.decodeLocals internalType (ImmutableArray.CreateRange blob)
        |> Seq.toList

    let private verbatim (blob : byte[]) : UInt8Source[] = blob |> Array.map UInt8Source.Verbatim

    /// A method signature blob exactly as `DynamicMethod` builds it: `GetILGenerator` uses the
    /// internal `SignatureHelper.GetSignature(true)`, whose `true` appends `ELEMENT_TYPE_END`
    /// (SignatureHelper.cs:870-881). Only the `false` overload is public, so the byte is appended
    /// here. `both the trailing-END and bare forms decode` pins that the two differ by exactly it.
    let private encodeMethod (returnType : Type) (parameterTypes : Type list) : byte[] =
        let helper =
            SignatureHelper.GetMethodSigHelper (nullModule, returnType, List.toArray parameterTypes)

        Array.append (helper.GetSignature ()) [| 0x00uy |]

    /// The blob without the appended terminator: what the public `GetSignature()` returns, and a
    /// valid MethodDefSig in its own right (ECMA-335 II.23.2.1 has no trailing END).
    let private encodeMethodBare (returnType : Type) (parameterTypes : Type list) : byte[] =
        SignatureHelper.GetMethodSigHelper(nullModule, returnType, List.toArray parameterTypes).GetSignature ()

    /// What `DynamicResolver` hands the runtime for a `DynamicILGenerator`'s locals: the internal
    /// `InternalGetSignatureArray` (DynamicILGenerator.cs:579), called here by reflection because it
    /// differs from the public `GetSignature()` at the end of the blob, which is where the walker is
    /// strictest.
    let private internalGetSignatureArray : MethodInfo =
        typeof<SignatureHelper>
            .GetMethod ("InternalGetSignatureArray", BindingFlags.NonPublic ||| BindingFlags.Instance)

    /// A locals blob exactly as `DynamicILGenerator.DeclareLocal` builds it.
    let private encodeLocals (locals : (Type * bool) list) : byte[] =
        let helper = SignatureHelper.GetLocalVarSigHelper ()

        for t, pinned in locals do
            helper.AddArgument (t, pinned)

        internalGetSignatureArray.Invoke (helper, [||]) :?> byte[]

    /// The types `SignatureHelper.IsSimpleType` accepts, paired with the `TypeDefn` each decodes to.
    /// `void` is absent because it is legal only as a return type or a pointee; `TypedReference` is
    /// absent because no type can be built over it, and is covered on its own.
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

    /// The reference model: what each host type must decode to, written directly over
    /// `System.Type` rather than over the blob.
    let rec private expected (t : Type) : TypeDefn =
        match simpleAlphabet |> List.tryFind (fun (s, _) -> s = t) with
        | Some (_, defn) -> defn
        | None ->

        if t = typeof<Void> then
            TypeDefn.Void
        elif t = typedReference then
            TypeDefn.PrimitiveType PrimitiveType.TypedReference
        elif t.IsByRef then
            TypeDefn.Byref (expected (t.GetElementType ()))
        elif t.IsPointer then
            TypeDefn.Pointer (expected (t.GetElementType ()))
        elif t.IsSZArray then
            TypeDefn.OneDimensionalArrayLowerBoundZero (expected (t.GetElementType ()))
        elif t.IsArray then
            TypeDefn.Array (expected (t.GetElementType ()), t.GetArrayRank ())
        elif t.IsGenericParameter then
            if isNull t.DeclaringMethod then
                TypeDefn.GenericTypeParameter t.GenericParameterPosition
            else
                TypeDefn.GenericMethodParameter t.GenericParameterPosition
        elif t.IsGenericType then
            let definition = t.GetGenericTypeDefinition ()

            TypeDefn.GenericInstantiation (
                TypeDefn.FromDefinition (identityOf definition, kindOf definition),
                t.GetGenericArguments () |> Seq.map expected |> ImmutableArray.CreateRange
            )
        else
            TypeDefn.FromDefinition (identityOf t, kindOf t)

    /// How many `ELEMENT_TYPE_INTERNAL` runs the encoder writes for `t`: one per nominal leaf and
    /// one per instantiated definition. The premise check that `symbolise` found every run.
    let rec private internalRunCount (t : Type) : int =
        if simpleAlphabet |> List.exists (fun (s, _) -> s = t) then
            0
        elif t = typeof<Void> || t = typedReference || t.IsGenericParameter then
            0
        elif t.HasElementType then
            internalRunCount (t.GetElementType ())
        elif t.IsGenericType then
            1 + (t.GetGenericArguments () |> Array.sumBy internalRunCount)
        else
            1

    /// Generic parameters as signature types: `VAR`/`MVAR` and a position, never a run.
    let private genericParameters : Type list =
        [
            typedefof<List<obj>>.GetGenericArguments().[0]
            typedefof<Dictionary<obj, obj>>.GetGenericArguments().[1]
            typeof<System.Linq.Enumerable>.GetMethod("Empty").GetGenericArguments().[0]
            typeof<System.Linq.Enumerable>.GetMethods ()
            |> Array.find (fun m -> m.Name = "Select" && m.GetGenericArguments().Length = 2)
            |> fun m -> m.GetGenericArguments().[1]
        ]

    let private nonGenericNominals : Type list =
        internalUniverse |> List.filter (fun t -> not t.IsGenericTypeDefinition)

    /// Definitions with no constraints, so any generated argument is a legal instantiation.
    let private unconstrainedDefinitions : Type list =
        [
            typedefof<List<obj>>
            typedefof<Dictionary<obj, obj>>
            typedefof<KeyValuePair<obj, obj>>
            typedefof<Func<obj, obj>>
            typedefof<ValueTuple<obj, obj, obj>>
        ]

    /// A type that may be a generic argument or an array element: no byref, pointer, void or
    /// TypedReference at its root.
    let rec private genElement (depth : int) : Gen<Type> =
        let leaf =
            Gen.oneof
                [
                    Gen.elements (List.map fst simpleAlphabet)
                    Gen.elements nonGenericNominals
                    Gen.elements genericParameters
                    // A bare generic definition, which the encoder spells as an instantiation over
                    // its own parameters.
                    Gen.elements unconstrainedDefinitions
                ]

        if depth <= 0 then
            leaf
        else
            Gen.frequency
                [
                    3, leaf
                    1, genAny (depth - 1) |> Gen.map (fun t -> t.MakeArrayType ())
                    1,
                    gen {
                        let! element = genAny (depth - 1)
                        let! rank = Gen.choose (2, 3)
                        return element.MakeArrayType rank
                    }
                    2,
                    gen {
                        let! definition = Gen.elements unconstrainedDefinitions
                        let arity = definition.GetGenericArguments().Length
                        let! arguments = Gen.listOfLength arity (genElement (depth - 1))
                        return definition.MakeGenericType (List.toArray arguments)
                    }
                    1,
                    Gen.elements [ typeof<DateTime> ; typeof<int> ; typeof<DayOfWeek> ]
                    |> Gen.map (fun t -> typedefof<Nullable<int>>.MakeGenericType t)
                ]

    /// Anything but a byref: an element, or a pointer to something (including `void`).
    and private genAny (depth : int) : Gen<Type> =
        if depth <= 0 then
            genElement 0
        else
            Gen.frequency
                [
                    4, genElement depth
                    1,
                    Gen.oneof [ genAny (depth - 1) ; Gen.constant typeof<Void> ]
                    |> Gen.map (fun t -> t.MakePointerType ())
                ]

    /// A parameter or local type: possibly a byref at the root, or a whole `TypedReference`.
    let private genParameter : Gen<Type> =
        Gen.frequency
            [
                6, genAny 3
                2, genAny 3 |> Gen.map (fun t -> t.MakeByRefType ())
                1, Gen.constant typedReference
            ]

    let private genReturn : Gen<Type> =
        Gen.frequency [ 5, genParameter ; 1, Gen.constant typeof<Void> ]

    [<Test>]
    let ``a method signature round-trips through the real encoder`` () : unit =
        let config = Config.QuickThrowOnFailure.WithMaxTest 500

        let gen : Gen<Type * Type list> =
            gen {
                let! ret = genReturn
                let! count = Gen.choose (0, 6)
                let! parameters = Gen.listOfLength count genParameter
                return ret, parameters
            }

        let property ((ret, parameters) : Type * Type list) : unit =
            let blob, replaced = symbolise (encodeMethod ret parameters)

            replaced
            |> shouldEqual (internalRunCount ret + List.sumBy internalRunCount parameters)

            let decoded = decodeMethod blob

            decoded.ReturnType |> shouldEqual (expected ret)

            decoded.ParameterTypes
            |> Seq.toList
            |> shouldEqual (List.map expected parameters)

            decoded.RequiredParameterCount |> shouldEqual parameters.Length
            decoded.GenericParameterCount |> shouldEqual 0

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``a locals signature round-trips through the real encoder`` () : unit =
        let config = Config.QuickThrowOnFailure.WithMaxTest 500

        let gen : Gen<(Type * bool) list> =
            gen {
                let! count = Gen.choose (0, 6)
                return! Gen.listOfLength count (Gen.zip genParameter (Gen.elements [ false ; true ]))
            }

        let property (locals : (Type * bool) list) : unit =
            let blob, replaced = symbolise (encodeLocals locals)

            replaced |> shouldEqual (List.sumBy (fst >> internalRunCount) locals)

            decodeLocals blob
            |> shouldEqual (
                locals
                |> List.map (fun (t, pinned) -> if pinned then TypeDefn.Pinned (expected t) else expected t)
            )

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)

    /// An enum is spelled `ELEMENT_TYPE_INTERNAL` like any other value type, whatever its underlying
    /// integer. Measured, and against the natural guess: `IsSimpleType` classifies by element type,
    /// and an enum's element type is widely (and in other CoreCLR contexts correctly) said to be its
    /// underlying integer's, which would make an enum parameter arrive indistinguishable from that
    /// integer. Two widths, so this cannot pass for a reason peculiar to Int32-backed enums.
    [<Test>]
    let ``an enum decodes to itself, whatever its underlying type`` () : unit =
        for enumType in [ typeof<DayOfWeek> ; typeof<ByteBackedEnum> ] do
            let blob, replaced = symbolise (encodeMethod typeof<int> [ enumType ])
            replaced |> shouldEqual 1

            (decodeMethod blob).ParameterTypes
            |> Seq.toList
            |> shouldEqual [ TypeDefn.FromDefinition (identityOf enumType, SignatureTypeKind.ValueType) ]

    /// A host blob's run is a host pointer: eight numbers rather than eight bytes of one handle. That
    /// is not something the walker can turn back into a type, and it must say so rather than read the
    /// numbers as anything.
    [<Test>]
    let ``refuses an ELEMENT_TYPE_INTERNAL run of plain numbers`` () : unit =
        let blob = encodeMethod typeof<int> [ typeof<DateTime> ]
        blob |> Array.contains 0x21uy |> shouldEqual true

        let exn =
            Assert.Throws<Exception> (fun () -> decodeMethod (verbatim blob) |> ignore)

        exn.Message |> shouldContainText "ELEMENT_TYPE_INTERNAL"

    /// A method taking one parameter whose type is the run `run`, returning int32.
    let private oneParameter (run : UInt8Source list) : UInt8Source[] =
        [ yield! verbatim [| 0x00uy ; 0x01uy ; 0x08uy ; 0x21uy |] ; yield! run ]
        |> List.toArray

    /// The run must be bytes 0 through 7 of one handle, in order. Anything else is a blob the guest
    /// has rearranged, and decoding it to *some* type would be a plausible wrong answer.
    [<Test>]
    let ``refuses a run whose bytes are out of order`` () : unit =
        let run = internalRun 0

        // The first byte out of place, and then two interior ones, which a check on the first byte
        // alone would let through.
        for first, second in [ 0, 1 ; 3, 4 ] do
            let swapped =
                run
                |> List.mapi (fun i b ->
                    if i = first then run.[second]
                    elif i = second then run.[first]
                    else b
                )

            let exn =
                Assert.Throws<Exception> (fun () -> decodeMethod (oneParameter swapped) |> ignore)

            exn.Message |> shouldContainText "ELEMENT_TYPE_INTERNAL"

    [<Test>]
    let ``refuses a run mixing two handles`` () : unit =
        let mixed = List.take 4 (internalRun 0) @ List.skip 4 (internalRun 1)

        let exn =
            Assert.Throws<Exception> (fun () -> decodeMethod (oneParameter mixed) |> ignore)

        exn.Message |> shouldContainText "ELEMENT_TYPE_INTERNAL"

    [<Test>]
    let ``refuses a run with a number in it`` () : unit =
        let run = internalRun 0
        let spoiled = List.take 7 run @ [ UInt8Source.Verbatim 0uy ]

        let exn =
            Assert.Throws<Exception> (fun () -> decodeMethod (oneParameter spoiled) |> ignore)

        exn.Message |> shouldContainText "ELEMENT_TYPE_INTERNAL"

    [<Test>]
    let ``refuses a run cut short by the end of the blob`` () : unit =
        let exn =
            Assert.Throws<Exception> (fun () -> decodeMethod (oneParameter (List.take 5 (internalRun 0))) |> ignore)

        exn.Message |> shouldContainText "truncated"

    /// A byte naming a handle is legal only inside a run. Anywhere else the walker needs a number,
    /// and must refuse rather than skip it.
    [<Test>]
    let ``refuses a handle byte where a number is expected`` () : unit =
        let blob =
            [|
                UInt8Source.Verbatim 0x00uy
                (internalRun 0).[0]
                UInt8Source.Verbatim 0x08uy
            |]

        let exn = Assert.Throws<Exception> (fun () -> decodeMethod blob |> ignore)
        exn.Message |> shouldContainText "byte 0 of"

    /// `GENERICINST` takes a definition and exactly its arity of arguments. `List<>` has one.
    [<Test>]
    let ``refuses an instantiation with the wrong number of arguments`` () : unit =
        let listIndex =
            internalUniverse |> List.findIndex (fun t -> t = typedefof<List<obj>>)

        let blob =
            [
                yield! verbatim [| 0x00uy ; 0x01uy ; 0x08uy ; 0x15uy ; 0x21uy |]
                yield! internalRun listIndex
                yield! verbatim [| 0x02uy ; 0x08uy ; 0x08uy |]
            ]
            |> List.toArray

        let exn = Assert.Throws<Exception> (fun () -> decodeMethod blob |> ignore)
        exn.Message |> shouldContainText "arity"

    [<Test>]
    let ``refuses an instantiation of a non-generic type`` () : unit =
        let blob =
            [
                yield! verbatim [| 0x00uy ; 0x01uy ; 0x08uy ; 0x15uy ; 0x21uy |]
                yield! internalRun 0
                yield! verbatim [| 0x01uy ; 0x08uy |]
            ]
            |> List.toArray

        let exn = Assert.Throws<Exception> (fun () -> decodeMethod blob |> ignore)
        exn.Message |> shouldContainText "GENERICINST"

    /// `SignatureHelper` writes a bare generic definition as an instantiation over its own
    /// parameters, so a run naming a definition anywhere but under `GENERICINST` is not its output.
    [<Test>]
    let ``refuses a generic definition outside an instantiation`` () : unit =
        let listIndex =
            internalUniverse |> List.findIndex (fun t -> t = typedefof<List<obj>>)

        let exn =
            Assert.Throws<Exception> (fun () -> decodeMethod (oneParameter (internalRun listIndex)) |> ignore)

        exn.Message |> shouldContainText "GENERICINST"

    /// A null-module `SignatureHelper` never writes a metadata token. One reaches the walker only
    /// through `DynamicILInfo.SetLocalSignature`, whose tokens are `DynamicScope` indices rather than
    /// rows of any assembly, so reading one against metadata would name an unrelated type.
    [<Test>]
    let ``refuses a type spelled as a metadata token`` () : unit =
        for elementType in [ 0x11uy ; 0x12uy ] do
            // LOCAL_SIG, one local: VALUETYPE or CLASS, then a coded TypeDef token.
            let blob = verbatim [| 0x07uy ; 0x01uy ; elementType ; 0x08uy |]

            let exn = Assert.Throws<Exception> (fun () -> decodeLocals blob |> ignore)
            exn.Message |> shouldContainText "token"

    /// Custom modifiers need a module to spell their types in, and function pointer types have no
    /// branch of their own in `SignatureHelper`; neither can appear in its null-module output.
    [<Test>]
    let ``refuses element types a null-module encoder cannot write`` () : unit =
        for elementType in [ 0x1Fuy ; 0x20uy ; 0x1Buy ] do
            let blob = verbatim [| 0x00uy ; 0x01uy ; 0x08uy ; elementType ; 0x08uy |]

            let exn = Assert.Throws<Exception> (fun () -> decodeMethod blob |> ignore)
            exn.Message |> shouldContainText $"0x%02x{elementType}"

    [<Test>]
    let ``refuses void as a parameter`` () : unit =
        let exn =
            Assert.Throws<Exception> (fun () ->
                decodeMethod (verbatim [| 0x00uy ; 0x01uy ; 0x08uy ; 0x01uy |]) |> ignore
            )

        exn.Message |> shouldContainText "void"

    [<Test>]
    let ``void is legal as a return type and as a pointee`` () : unit =
        let decoded =
            decodeMethod (verbatim (encodeMethod typeof<Void> [ typeof<Void>.MakePointerType () ]))

        decoded.ReturnType |> shouldEqual TypeDefn.Void

        decoded.ParameterTypes
        |> Seq.toList
        |> shouldEqual [ TypeDefn.Pointer TypeDefn.Void ]

    /// `PINNED` qualifies a local, never a parameter.
    [<Test>]
    let ``refuses pinned in a method signature`` () : unit =
        let exn =
            Assert.Throws<Exception> (fun () ->
                decodeMethod (verbatim [| 0x00uy ; 0x01uy ; 0x08uy ; 0x45uy ; 0x08uy |])
                |> ignore
            )

        exn.Message |> shouldContainText "PINNED"

    /// `TypeDefn.Array` records only the rank, so a shape with sizes, or with other than one zero
    /// lower bound per dimension, would compare equal to the plain array of that rank. The encoder
    /// never writes one.
    [<Test>]
    let ``refuses a non-canonical array shape`` () : unit =
        // ARRAY int32, rank 2, one size (3), no lower bounds.
        let blob =
            verbatim
                [|
                    0x00uy
                    0x01uy
                    0x08uy
                    0x14uy
                    0x08uy
                    0x02uy
                    0x01uy
                    0x03uy
                    0x00uy
                |]

        let exn = Assert.Throws<Exception> (fun () -> decodeMethod blob |> ignore)
        exn.Message |> shouldContainText "ArrayShape"

    [<Test>]
    let ``the doubling method's signature decodes`` () : unit =
        let decoded = decodeMethod (verbatim (encodeMethod typeof<int> [ typeof<int> ]))

        decoded.ReturnType |> shouldEqual (TypeDefn.PrimitiveType PrimitiveType.Int32)

        decoded.ParameterTypes
        |> Seq.toList
        |> shouldEqual [ TypeDefn.PrimitiveType PrimitiveType.Int32 ]

        decoded.RequiredParameterCount |> shouldEqual 1
        decoded.GenericParameterCount |> shouldEqual 0

    [<Test>]
    let ``refuses an empty method signature`` () : unit =
        let exn = Assert.Throws<Exception> (fun () -> decodeMethod [||] |> ignore)
        exn.Message |> shouldContainText "empty"

    /// A LocalVarSig and a MethodDefSig differ only in their leading byte, and `DynamicResolver`
    /// hands PawPrint both. Decoding one as the other would read a type as a count.
    [<Test>]
    let ``refuses a locals signature as a method signature, and the reverse`` () : unit =
        let localSig = verbatim [| 0x07uy ; 0x01uy ; 0x08uy |]
        let exn = Assert.Throws<Exception> (fun () -> decodeMethod localSig |> ignore)
        exn.Message |> shouldContainText "LocalVariables"

        let methodSig = verbatim [| 0x00uy ; 0x00uy ; 0x08uy |]
        let exn = Assert.Throws<Exception> (fun () -> decodeLocals methodSig |> ignore)
        exn.Message |> shouldContainText "LOCAL_SIG"

    /// A vararg signature decodes rather than being rejected: `RequiredParameterCount` says where the
    /// fixed parameters stop. Whether a delegate can bind to such a method is the binder's question.
    [<Test>]
    let ``a vararg signature decodes faithfully`` () : unit =
        let helper =
            SignatureHelper.GetMethodSigHelper (nullModule, CallingConventions.VarArgs, typeof<int>)

        helper.AddArgument typeof<int>
        helper.AddSentinel ()
        helper.AddArgument typeof<string>

        let decoded = decodeMethod (verbatim (helper.GetSignature ()))

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

    /// Hand-built, because `SignatureHelper` has no way to emit a generic method signature: GENERIC
    /// (0x10), then the arity, then the parameter count.
    [<Test>]
    let ``a generic method signature decodes faithfully`` () : unit =
        // GENERIC, arity 2, one parameter, returning !!0, taking !!1.
        let decoded =
            decodeMethod (verbatim [| 0x10uy ; 0x02uy ; 0x01uy ; 0x1Euy ; 0x00uy ; 0x1Euy ; 0x01uy |])

        decoded.GenericParameterCount |> shouldEqual 2
        decoded.ReturnType |> shouldEqual (TypeDefn.GenericMethodParameter 0)

        decoded.ParameterTypes
        |> Seq.toList
        |> shouldEqual [ TypeDefn.GenericMethodParameter 1 ]

    /// The two encoder modes differ by exactly one `ELEMENT_TYPE_END`, and both decode. This keeps
    /// `encodeMethod` honest: if the public `GetSignature()` started appending the terminator itself,
    /// `encodeMethod` would produce a double-terminated blob no dynamic method emits.
    [<Test>]
    let ``both the trailing-END and bare forms decode`` () : unit =
        let withEnd = encodeMethod typeof<int> [ typeof<int> ; typeof<string> ]
        let bare = encodeMethodBare typeof<int> [ typeof<int> ; typeof<string> ]

        withEnd |> shouldEqual (Array.append bare [| 0x00uy |])

        let expected =
            [
                TypeDefn.PrimitiveType PrimitiveType.Int32
                TypeDefn.PrimitiveType PrimitiveType.String
            ]

        (decodeMethod (verbatim withEnd)).ParameterTypes
        |> Seq.toList
        |> shouldEqual expected

        (decodeMethod (verbatim bare)).ParameterTypes
        |> Seq.toList
        |> shouldEqual expected

    /// A valid signature followed by junk is not a valid blob: these bytes come from guest memory, so
    /// a concatenated or corrupt blob must not decode as its first signature.
    [<Test>]
    let ``refuses trailing bytes after a complete signature`` () : unit =
        let valid = encodeMethodBare typeof<int> [ typeof<int> ]

        let exn =
            Assert.Throws<Exception> (fun () ->
                decodeMethod (verbatim (Array.append valid [| 0x00uy ; 0x00uy |])) |> ignore
            )

        exn.Message |> shouldContainText "bytes left over"

        // A single trailing byte that is not ELEMENT_TYPE_END is refused too, so the tolerance is for
        // the terminator specifically rather than for "one spare byte".
        let exn =
            Assert.Throws<Exception> (fun () -> decodeMethod (verbatim (Array.append valid [| 0x08uy |])) |> ignore)

        exn.Message |> shouldContainText "ELEMENT_TYPE_END"

    /// The encoder ends a locals blob with `ELEMENT_TYPE_END`, and at a count on the boundary of a
    /// wider compressed integer it sizes the blob for the wider form and writes the narrower, leaving
    /// one or two more zero bytes before that `END`. Those trailing ENDs are the encoder's output and
    /// decode; anything else after the declared locals does not.
    [<TestCase 0>]
    [<TestCase 1>]
    [<TestCase 126>]
    [<TestCase 127>]
    [<TestCase 128>]
    [<TestCase 16383>]
    let ``every locals count the encoder can write decodes`` (count : int) : unit =
        decodeLocals (verbatim (encodeLocals (List.replicate count (typeof<int>, false))))
        |> List.length
        |> shouldEqual count

    [<Test>]
    let ``refuses trailing bytes after a complete locals signature`` () : unit =
        let exn =
            Assert.Throws<Exception> (fun () ->
                decodeLocals (verbatim (Array.append (encodeLocals [ typeof<int>, false ]) [| 0x08uy |]))
                |> ignore
            )

        exn.Message |> shouldContainText "left over"

    [<Test>]
    let ``refuses a truncated signature`` () : unit =
        // DEFAULT, three parameters declared, int32 return, only one parameter.
        let exn =
            Assert.Throws<Exception> (fun () ->
                decodeMethod (verbatim [| 0x00uy ; 0x03uy ; 0x08uy ; 0x08uy |]) |> ignore
            )

        exn.Message |> shouldContainText "truncated"

    /// A declared count larger than the bytes left to spell it in names truncation directly, rather
    /// than failing somewhere inside the first type it cannot find.
    [<Test>]
    let ``a parameter count exceeding the blob is reported as truncation`` () : unit =
        // DEFAULT, then 0x1FFFFFFF as a four-byte compressed integer, then a void return.
        let exn =
            Assert.Throws<Exception> (fun () ->
                decodeMethod (verbatim [| 0x00uy ; 0xDFuy ; 0xFFuy ; 0xFFuy ; 0xFFuy ; 0x01uy |])
                |> ignore
            )

        exn.Message |> shouldContainText "truncated or corrupt"
        exn.Message |> shouldContainText "536870911"

    /// The bound must admit the tightest real signature, where every parameter is one byte.
    [<Test>]
    let ``the parameter-count bound admits a maximally tight signature`` () : unit =
        (decodeMethod (verbatim (encodeMethodBare typeof<int> (List.replicate 8 typeof<int>)))).ParameterTypes.Length
        |> shouldEqual 8

    /// The case every dynamic method declaring no locals produces.
    [<Test>]
    let ``a zero-count locals signature decodes to no locals`` () : unit =
        decodeLocals (verbatim (encodeLocals [])) |> shouldEqual []

    [<Test>]
    let ``a local count exceeding the blob is reported as truncation`` () : unit =
        // LOCAL_SIG, then 0x1FFFFFFF as a four-byte compressed integer.
        let exn =
            Assert.Throws<Exception> (fun () ->
                decodeLocals (verbatim [| 0x07uy ; 0xDFuy ; 0xFFuy ; 0xFFuy ; 0xFFuy |])
                |> ignore
            )

        exn.Message |> shouldContainText "truncated or corrupt"
        exn.Message |> shouldContainText "536870911"

    [<Test>]
    let ``the local-count bound admits a maximally tight signature`` () : unit =
        decodeLocals (verbatim (encodeLocals (List.replicate 8 (typeof<int>, false))))
        |> List.length
        |> shouldEqual 8
