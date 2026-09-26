namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata

/// The type an `ELEMENT_TYPE_INTERNAL` run in a `Reflection.Emit` signature names.
///
/// `SignatureHelper` writes generic parameters, constructed generics, byrefs, pointers and arrays
/// structurally, so a run only ever names a leaf: a type with no generic parameters of its own, or
/// the definition an `ELEMENT_TYPE_GENERICINST` instantiates.
[<RequireQualifiedAccess>]
type InternalSignatureType =
    /// A type with no generic parameters of its own.
    | NonGeneric of identity : ResolvedTypeIdentity * kind : SignatureTypeKind
    /// A generic type definition, which a signature may name only as the type an
    /// `ELEMENT_TYPE_GENERICINST` instantiates.
    | GenericDefinition of identity : ResolvedTypeIdentity * kind : SignatureTypeKind * arity : int

/// <summary>
/// Decoding of the signature blobs a <c>DynamicMethod</c> hands the runtime: its method signature
/// (<c>ModuleHandle.GetDynamicMethod</c>) and its locals signature (<c>DynamicResolver</c>).
/// </summary>
/// <remarks>
/// <para>
/// A method minted by <c>Reflection.Emit</c> has no metadata row, and <c>SignatureHelper</c> builds
/// both blobs with a null module, so it cannot spell a type as a token. Anything that is not a
/// primitive, <c>object</c>, <c>string</c> or <c>TypedReference</c> it writes as
/// <c>ELEMENT_TYPE_INTERNAL</c> followed by the eight bytes of the type's handle
/// (<c>InternalAddRuntimeType</c>). In guest memory those bytes are
/// <see cref="UInt8Source.NativeIntByte" />s naming the handle, which is why the blob arrives as
/// <see cref="UInt8Source" />s rather than as numbers.
/// </para>
/// <para>
/// The walker accepts exactly the alphabet a null-module <c>SignatureHelper</c> writes (see
/// <c>AddOneArgTypeHelperWorker</c>) and refuses everything else by name. In particular it refuses
/// <c>CLASS</c>/<c>VALUETYPE</c> tokens: the only route by which one reaches the runtime is
/// <c>DynamicILInfo.SetLocalSignature</c>, whose tokens are <c>DynamicScope</c> indices rather than
/// rows of any assembly.
/// </para>
/// </remarks>
[<RequireQualifiedAccess>]
module DynamicSignatureDecoding =

    /// Where a type sits, which decides what it may be. ECMA-335 II.23.2: `void` is a return type
    /// or a pointee; a byref and `TypedReference` are a whole return, parameter or local.
    [<RequireQualifiedAccess>]
    type private Slot =
        | Return
        | ParameterOrLocal
        | Pointee
        /// An array element, a generic argument, or a byref's referent.
        | Nested

    type private Blob =
        {
            /// Completes "a ..." in every refusal: which blob this is.
            What : string
            Bytes : ImmutableArray<UInt8Source>
            InternalType : NativeIntSource -> InternalSignatureType
        }

    let private numberAt (blob : Blob) (pos : int) : byte =
        if pos >= blob.Bytes.Length then
            failwith
                $"%s{blob.What} is truncated: it ends after %d{blob.Bytes.Length} byte(s), where byte %d{pos} was expected"

        match blob.Bytes.[pos] with
        | UInt8Source.Verbatim b -> b
        | UInt8Source.NativeIntByte _ as named ->
            failwith
                $"%s{blob.What} has %O{named} at byte %d{pos}, where a number is expected; a byte naming a handle belongs only inside an ELEMENT_TYPE_INTERNAL run"

    /// An ECMA-335 II.23.2 compressed unsigned integer, and the position after it.
    let private compressedAt (blob : Blob) (pos : int) : int * int =
        let first = numberAt blob pos

        if first &&& 0x80uy = 0uy then
            int first, pos + 1
        elif first &&& 0xC0uy = 0x80uy then
            ((int first &&& 0x3F) <<< 8) ||| int (numberAt blob (pos + 1)), pos + 2
        elif first &&& 0xE0uy = 0xC0uy then
            ((int first &&& 0x1F) <<< 24)
            ||| (int (numberAt blob (pos + 1)) <<< 16)
            ||| (int (numberAt blob (pos + 2)) <<< 8)
            ||| int (numberAt blob (pos + 3)),
            pos + 4
        else
            failwith $"%s{blob.What} has 0x%02x{first} at byte %d{pos}, which does not begin a compressed integer"

    /// The handle an `ELEMENT_TYPE_INTERNAL` at `pos - 1` names: bytes 0 to 7 of one native int, in
    /// order. A guest that rearranges or overwrites any of them gets a refusal rather than a
    /// plausible wrong type.
    let private internalRunAt (blob : Blob) (pos : int) : NativeIntSource * int =
        if pos + 8 > blob.Bytes.Length then
            failwith
                $"%s{blob.What} is truncated: the ELEMENT_TYPE_INTERNAL at byte %d{pos - 1} needs eight bytes after it, and the blob ends after %d{blob.Bytes.Length}"

        let refuse (index : int) : 'a =
            failwith
                $"%s{blob.What}: the ELEMENT_TYPE_INTERNAL at byte %d{pos - 1} must be followed by bytes 0 to 7 of one type handle in order, but byte %d{pos + index} is %O{blob.Bytes.[pos + index]}"

        let source =
            match blob.Bytes.[pos] with
            | UInt8Source.NativeIntByte (source, 0) -> source
            | _ -> refuse 0

        for index in 1..7 do
            match blob.Bytes.[pos + index] with
            | UInt8Source.NativeIntByte (other, i) when i = index && other = source -> ()
            | _ -> refuse index

        source, pos + 8

    let private refuseToken (blob : Blob) (code : byte) (pos : int) : 'a =
        failwith
            $"%s{blob.What} spells a type at byte %d{pos} as a metadata token (element type 0x%02x{code}). A null-module SignatureHelper never writes one; a DynamicILInfo local signature does, but its tokens index the method's DynamicScope rather than any assembly's tables, and PawPrint does not yet resolve them"

    let rec private typeAt (blob : Blob) (slot : Slot) (pos : int) : TypeDefn * int =
        let code = numberAt blob pos
        let next = pos + 1

        match code with
        | 0x01uy ->
            match slot with
            | Slot.Return
            | Slot.Pointee -> TypeDefn.Void, next
            | Slot.ParameterOrLocal
            | Slot.Nested ->
                failwith $"%s{blob.What} has void at byte %d{pos}; void is legal only as a return type or a pointee"
        | 0x16uy ->
            match slot with
            | Slot.Return
            | Slot.ParameterOrLocal -> TypeDefn.PrimitiveType PrimitiveType.TypedReference, next
            | Slot.Pointee
            | Slot.Nested ->
                failwith
                    $"%s{blob.What} has TYPEDBYREF at byte %d{pos}; it is legal only as a whole return type, parameter or local"
        | code when
            (code >= 0x02uy && code <= 0x0Euy)
            || code = 0x18uy
            || code = 0x19uy
            || code = 0x1Cuy
            ->
            match PrimitiveType.OfEnum (LanguagePrimitives.EnumOfValue<byte, PrimitiveTypeCode> code) with
            | Some primitive -> TypeDefn.PrimitiveType primitive, next
            | None -> failwith $"BUG: element type 0x%02x{code} is in the primitive range but names no primitive"
        | 0x0Fuy ->
            let pointee, next = typeAt blob Slot.Pointee next
            TypeDefn.Pointer pointee, next
        | 0x10uy ->
            match slot with
            | Slot.Return
            | Slot.ParameterOrLocal ->
                let referent, next = typeAt blob Slot.Nested next
                TypeDefn.Byref referent, next
            | Slot.Pointee
            | Slot.Nested ->
                failwith
                    $"%s{blob.What} has BYREF at byte %d{pos}; a byref is legal only as a whole return type, parameter or local"
        | 0x1Duy ->
            let element, next = typeAt blob Slot.Nested next
            TypeDefn.OneDimensionalArrayLowerBoundZero element, next
        | 0x14uy ->
            let element, next = typeAt blob Slot.Nested next
            let rank, next = compressedAt blob next
            let sizeCount, next = compressedAt blob next

            let next =
                (next, [ 1..sizeCount ])
                ||> List.fold (fun next _ -> compressedAt blob next |> snd)

            let lowerBoundCount, next = compressedAt blob next

            let lowerBounds, next =
                (([], next), [ 1..lowerBoundCount ])
                ||> List.fold (fun (bounds, next) _ ->
                    let bound, next = compressedAt blob next
                    bound :: bounds, next
                )

            // `TypeDefn.Array` records only the rank, so any other shape would compare equal to the
            // plain array of that rank. `SignatureHelper` writes no sizes and one zero lower bound
            // per dimension; this is `TypeDefn.typeProvider`'s rule for metadata signatures too.
            if
                rank < 1
                || sizeCount <> 0
                || lowerBoundCount <> rank
                || lowerBounds |> List.exists ((<>) 0)
            then
                failwith
                    $"%s{blob.What} has an array at byte %d{pos} with a non-canonical ArrayShape (rank %d{rank}, %d{sizeCount} size(s), lower bounds %A{List.rev lowerBounds}); SignatureHelper writes no sizes and one zero lower bound per dimension, and TypeDefn.Array records only the rank"

            TypeDefn.Array (element, rank), next
        | 0x15uy ->
            let definitionCode = numberAt blob next

            match definitionCode with
            | 0x21uy -> ()
            | 0x11uy
            | 0x12uy -> refuseToken blob definitionCode next
            | other ->
                failwith
                    $"%s{blob.What} has GENERICINST at byte %d{pos} followed by element type 0x%02x{other}; SignatureHelper follows it with ELEMENT_TYPE_INTERNAL naming the definition"

            let source, next = internalRunAt blob (next + 1)

            let identity, kind, arity =
                match blob.InternalType source with
                | InternalSignatureType.GenericDefinition (identity, kind, arity) -> identity, kind, arity
                | InternalSignatureType.NonGeneric (identity, _) ->
                    failwith
                        $"%s{blob.What} has GENERICINST at byte %d{pos} instantiating %O{identity}, which has no generic parameters"

            let count, next = compressedAt blob next

            if count <> arity then
                failwith
                    $"%s{blob.What} has GENERICINST at byte %d{pos} instantiating %O{identity}, of arity %d{arity}, with %d{count} argument(s)"

            let arguments = ImmutableArray.CreateBuilder<TypeDefn> count

            let next =
                (next, [ 1..count ])
                ||> List.fold (fun next _ ->
                    let argument, next = typeAt blob Slot.Nested next
                    arguments.Add argument
                    next
                )

            TypeDefn.GenericInstantiation (TypeDefn.FromDefinition (identity, kind), arguments.MoveToImmutable ()), next
        | 0x13uy ->
            let index, next = compressedAt blob next
            TypeDefn.GenericTypeParameter index, next
        | 0x1Euy ->
            let index, next = compressedAt blob next
            TypeDefn.GenericMethodParameter index, next
        | 0x21uy ->
            let source, next = internalRunAt blob next

            match blob.InternalType source with
            | InternalSignatureType.NonGeneric (identity, kind) -> TypeDefn.FromDefinition (identity, kind), next
            | InternalSignatureType.GenericDefinition (identity, _, _) ->
                failwith
                    $"%s{blob.What} names the generic definition %O{identity} at byte %d{pos} outside a GENERICINST; SignatureHelper writes a bare generic definition as an instantiation over its own parameters"
        | 0x11uy
        | 0x12uy -> refuseToken blob code pos
        | 0x45uy ->
            failwith $"%s{blob.What} has PINNED at byte %d{pos}; PINNED qualifies a whole local and nothing else"
        | 0x41uy ->
            failwith
                $"%s{blob.What} has SENTINEL at byte %d{pos}; it may appear only between a vararg method's fixed and optional parameters"
        | other ->
            failwith
                $"%s{blob.What} has element type 0x%02x{other} at byte %d{pos}, which is not in the alphabet a null-module SignatureHelper writes"

    /// <summary>Decode a MethodDefSig (ECMA-335 II.23.2.1).</summary>
    /// <param name="internalType">What each `ELEMENT_TYPE_INTERNAL` run's handle names.</param>
    /// <remarks>
    /// Faithful rather than filtered: a vararg signature comes back with <c>RequiredParameterCount</c>
    /// saying where the fixed parameters stop, and a generic method's arity in
    /// <c>GenericParameterCount</c>. Whether PawPrint can do anything with such a method is the
    /// consumer's question. One trailing <c>ELEMENT_TYPE_END</c> is accepted, because
    /// <c>DynamicMethod</c> builds its signature with <c>GetSignature(true)</c>, which appends one.
    /// </remarks>
    let decodeMethod
        (internalType : NativeIntSource -> InternalSignatureType)
        (blob : ImmutableArray<UInt8Source>)
        : MethodSignature<TypeDefn>
        =
        let blob =
            {
                What = "method signature blob"
                Bytes = blob
                InternalType = internalType
            }

        if blob.Bytes.IsEmpty then
            failwith "method signature blob is empty; every MethodDefSig carries at least a calling-convention byte"

        let header = SignatureHeader (numberAt blob 0)

        if header.Kind <> SignatureKind.Method then
            failwith
                $"expected a method signature (one of the METHOD calling conventions), but the blob's calling convention is %O{header.Kind}"

        let genericParameterCount, pos =
            if header.IsGeneric then compressedAt blob 1 else 0, 1

        let declared, pos = compressedAt blob pos

        // Every parameter occupies at least one byte and so does the return type, so this names a
        // truncated blob before walking into it.
        if declared >= blob.Bytes.Length - pos then
            failwith
                $"method signature blob declares %d{declared} parameter(s) but has only %d{blob.Bytes.Length - pos} byte(s) left to spell them and the return type in; it is truncated or corrupt"

        let returnType, pos = typeAt blob Slot.Return pos
        let parameters = ImmutableArray.CreateBuilder<TypeDefn> declared
        let mutable pos = pos
        let mutable required = None

        for index in 0 .. declared - 1 do
            if numberAt blob pos = 0x41uy then
                if header.CallingConvention <> SignatureCallingConvention.VarArgs then
                    failwith
                        $"method signature blob has SENTINEL at byte %d{pos}, but its calling convention is %O{header.CallingConvention}, not VarArgs"

                if required.IsSome then
                    failwith $"method signature blob has a second SENTINEL at byte %d{pos}"

                required <- Some index
                pos <- pos + 1

            let parameter, next = typeAt blob Slot.ParameterOrLocal pos
            parameters.Add parameter
            pos <- next

        match blob.Bytes.Length - pos with
        | 0 -> ()
        | 1 ->
            let trailing = numberAt blob pos

            if trailing <> 0uy then
                failwith
                    $"method signature blob has one byte left over after its %d{declared} declared parameter(s), and it is 0x%02x{trailing} rather than the ELEMENT_TYPE_END (0x00) that SignatureHelper appends"
        | remaining ->
            failwith
                $"method signature blob has %d{remaining} bytes left over after its %d{declared} declared parameter(s); at most one trailing ELEMENT_TYPE_END is expected, so this blob is truncated, corrupt, or several signatures concatenated"

        MethodSignature (
            header,
            returnType,
            Option.defaultValue declared required,
            genericParameterCount,
            parameters.MoveToImmutable ()
        )

    /// <summary>Decode a LocalVarSig (ECMA-335 II.23.2.6) into one type per local, in declaration order.</summary>
    /// <param name="internalType">What each `ELEMENT_TYPE_INTERNAL` run's handle names.</param>
    let decodeLocals
        (internalType : NativeIntSource -> InternalSignatureType)
        (blob : ImmutableArray<UInt8Source>)
        : ImmutableArray<TypeDefn>
        =
        let blob =
            {
                What = "local variable signature blob"
                Bytes = blob
                InternalType = internalType
            }

        if blob.Bytes.IsEmpty then
            failwith
                "local variable signature blob is empty; every LocalVarSig carries at least the 0x07 calling-convention byte"

        let header = SignatureHeader (numberAt blob 0)

        if header.Kind <> SignatureKind.LocalVariables then
            failwith
                $"expected a local variable signature (LOCAL_SIG, 0x07), but the blob's calling convention is %O{header.Kind}"

        let count, pos = compressedAt blob 1

        if count > blob.Bytes.Length - pos then
            failwith
                $"local variable signature blob declares %d{count} local(s) but has only %d{blob.Bytes.Length - pos} byte(s) left to spell them in; it is truncated or corrupt"

        let locals = ImmutableArray.CreateBuilder<TypeDefn> count
        let mutable pos = pos

        for _ in 1..count do
            let pinned = numberAt blob pos = 0x45uy
            let start = if pinned then pos + 1 else pos
            let local, next = typeAt blob Slot.ParameterOrLocal start
            locals.Add (if pinned then TypeDefn.Pinned local else local)
            pos <- next

        // `InternalGetSignatureArray` ends the blob with ELEMENT_TYPE_END. At 127 and 16383 locals
        // it also sizes the count for the next-wider compressed form and writes the narrower one,
        // leaving one or two more zero bytes before that END (SignatureHelper.cs, the `<` sizing
        // against the `<=` writing).
        let remaining = blob.Bytes.Length - pos

        if
            remaining > 3
            || Seq.init remaining (fun i -> numberAt blob (pos + i)) |> Seq.exists ((<>) 0uy)
        then
            failwith
                $"local variable signature blob has %d{remaining} byte(s) left over after its %d{count} declared local(s); only the encoder's trailing ELEMENT_TYPE_END bytes (at most three) may follow them"

        locals.MoveToImmutable ()
