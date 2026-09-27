namespace WoofWare.PawPrint

/// The memory a never-written byte lives in: a `localloc` block of one frame, or a native-heap
/// block.
[<RequireQualifiedAccess>]
type UninitialisedMemory =
    | Stack of thread : ThreadId * frame : FrameId * block : StackMemoryBlockId
    | Native of block : NativeMemoryBlockId

    override this.ToString () : string =
        match this with
        | UninitialisedMemory.Stack (thread, frame, block) -> $"%O{block} of %O{frame} on thread %O{thread}"
        | UninitialisedMemory.Native block -> $"%O{block}"

/// A byte of guest memory that nothing ever wrote. Every undefined byte of every value descends
/// from one of these, and keeps naming it however far it is copied.
type UninitialisedByte =
    {
        Memory : UninitialisedMemory
        Offset : int
    }

    override this.ToString () : string =
        $"byte %d{this.Offset} of %O{this.Memory}"

/// One byte of a value's image: a number, or a byte whose content is undefined because it
/// descends from memory nothing wrote.
[<RequireQualifiedAccess>]
type ValueByte =
    | Defined of byte
    | Undefined of UninitialisedByte

    override this.ToString () : string =
        match this with
        | ValueByte.Defined b -> $"0x%02x{b}"
        | ValueByte.Undefined origin -> $"<undefined: %O{origin}>"

/// The primitive storage shape an undefined value stands in for: one case per leaf shape of
/// `CliType`. A value type is never undefined as a whole; its fields are, one leaf at a time.
[<RequireQualifiedAccess>]
type UndefinedPrimitive =
    | Int8
    | UInt8
    | Int16
    | UInt16
    | Int32
    | Int64
    | NativeInt
    | Float32
    | Float64
    | NativeFloat
    | Bool
    | Char
    | ObjectRef
    | RuntimePointer

[<RequireQualifiedAccess>]
module UndefinedPrimitive =
    /// The width in bytes of a value of this shape, which is the length of its image.
    let size (kind : UndefinedPrimitive) : int =
        match kind with
        | UndefinedPrimitive.Int8
        | UndefinedPrimitive.UInt8
        | UndefinedPrimitive.Bool -> 1
        | UndefinedPrimitive.Int16
        | UndefinedPrimitive.UInt16
        | UndefinedPrimitive.Char -> 2
        | UndefinedPrimitive.Int32
        | UndefinedPrimitive.Float32 -> 4
        | UndefinedPrimitive.Int64
        | UndefinedPrimitive.NativeInt
        | UndefinedPrimitive.Float64
        | UndefinedPrimitive.NativeFloat
        | UndefinedPrimitive.ObjectRef
        | UndefinedPrimitive.RuntimePointer -> 8

    /// How a value of this shape widens on its way onto the evaluation stack, per ECMA-335
    /// III.1.1.1: `Some true` for a signed integer narrower than 32 bits, `Some false` for an
    /// unsigned one (including `bool` and `char`), and `None` for a shape that does not widen.
    let private widening (kind : UndefinedPrimitive) : bool option =
        match kind with
        | UndefinedPrimitive.Int8
        | UndefinedPrimitive.Int16 -> Some true
        | UndefinedPrimitive.UInt8
        | UndefinedPrimitive.UInt16
        | UndefinedPrimitive.Bool
        | UndefinedPrimitive.Char -> Some false
        | UndefinedPrimitive.Int32
        | UndefinedPrimitive.Int64
        | UndefinedPrimitive.NativeInt
        | UndefinedPrimitive.Float32
        | UndefinedPrimitive.Float64
        | UndefinedPrimitive.NativeFloat
        | UndefinedPrimitive.ObjectRef
        | UndefinedPrimitive.RuntimePointer -> None

    /// Whether this is one of the floating-point shapes, which the evaluation stack keeps apart
    /// from every integer and reference shape.
    let isFloat (kind : UndefinedPrimitive) : bool =
        match kind with
        | UndefinedPrimitive.Float32
        | UndefinedPrimitive.Float64
        | UndefinedPrimitive.NativeFloat -> true
        | UndefinedPrimitive.Int8
        | UndefinedPrimitive.UInt8
        | UndefinedPrimitive.Int16
        | UndefinedPrimitive.UInt16
        | UndefinedPrimitive.Int32
        | UndefinedPrimitive.Int64
        | UndefinedPrimitive.NativeInt
        | UndefinedPrimitive.Bool
        | UndefinedPrimitive.Char
        | UndefinedPrimitive.ObjectRef
        | UndefinedPrimitive.RuntimePointer -> false

    /// The image of a value of shape `kind` once it is on the evaluation stack: the bytes of its
    /// 32-bit slot if it widens, and its own bytes otherwise.
    let internal widenForStack (kind : UndefinedPrimitive) (bytes : ValueByte list) : ValueByte list =
        match widening kind with
        | None -> bytes
        | Some signed ->
            let extension =
                if signed then
                    // The copies of the sign bit are exactly as defined as the byte holding it.
                    match List.last bytes with
                    | ValueByte.Defined top -> ValueByte.Defined (if top >= 0x80uy then 0xFFuy else 0uy)
                    | ValueByte.Undefined origin -> ValueByte.Undefined origin
                else
                    ValueByte.Defined 0uy

            bytes @ List.replicate (4 - List.length bytes) extension

/// A primitive value whose image contains at least one byte nothing ever wrote.
///
/// Holding one is not an error: it may be copied, stored, passed and returned freely, and it is
/// overwritten like any other value. What it may not do is decide anything — a branch, an
/// address, the result of arithmetic — and the interpreter ends the run at the first such use
/// rather than invent the missing bits.
///
/// The image keeps the bytes that *are* defined, so a partly written value copied through
/// storage a chunk at a time arrives with those bytes intact, and a narrower read of only its
/// defined bytes is an ordinary number.
type UndefinedValue =
    private
        {
            _Kind : UndefinedPrimitive
            _Bytes : ValueByte list
        }

    /// The primitive shape this value stands in for.
    member this.Kind : UndefinedPrimitive = this._Kind

    /// The value's little-endian image, exactly `UndefinedPrimitive.size this.Kind` bytes long,
    /// with at least one `ValueByte.Undefined` among them.
    member this.Bytes : ValueByte list = this._Bytes

    /// The never-written bytes this value's undefined bytes descend from, in image order and
    /// without repeats. Never empty.
    member this.Origins : UninitialisedByte list =
        this._Bytes
        |> List.choose (fun b ->
            match b with
            | ValueByte.Undefined origin -> Some origin
            | ValueByte.Defined _ -> None
        )
        |> List.distinct

    override this.ToString () : string =
        let image = this._Bytes |> List.map string<ValueByte> |> String.concat " "
        $"Undefined(%A{this._Kind}: %s{image})"

[<RequireQualifiedAccess>]
module UndefinedValue =
    /// The value of shape `kind` with image `bytes`, or `ValueNone` if every byte is defined, in
    /// which case the caller has an ordinary number. Fails if the image is not as wide as the
    /// shape.
    let tryOfBytes (kind : UndefinedPrimitive) (bytes : ValueByte list) : UndefinedValue voption =
        let expected = UndefinedPrimitive.size kind

        if List.length bytes <> expected then
            failwith
                $"UndefinedValue.tryOfBytes: a %A{kind} is %d{expected} bytes wide, but its image has %d{List.length bytes}"

        let anyUndefined =
            bytes
            |> List.exists (fun b ->
                match b with
                | ValueByte.Undefined _ -> true
                | ValueByte.Defined _ -> false
            )

        if anyUndefined then
            ValueSome
                {
                    _Kind = kind
                    _Bytes = bytes
                }
        else
            ValueNone

    /// The image `value` has once moved into a slot of shape `target`, as `stloc`, `starg`,
    /// `stfld`, `stind`, a call argument or a return does to a value of its stack type.
    ///
    /// A value moves through its evaluation-stack form: a narrow integer is widened to 32 bits
    /// the way `ldind`/`ldloc` widen it (sign or zero, per its shape), a 32-bit one is
    /// sign-extended into a wider integer slot the way CoreCLR's importer does, and the result
    /// is then truncated to the slot. So every byte of the result is either copied from the
    /// value, a copy of its sign byte, or a zero. A float moved into a float slot of another
    /// width is a numeric conversion, and a conversion of anything undefined is undefined in
    /// every byte. Moving between a float shape and an integer or reference one is not a move
    /// the evaluation stack can make, and fails.
    ///
    /// The result may be entirely defined — truncation can drop every undefined byte — which is
    /// why this returns an image rather than an `UndefinedValue`.
    let moveInto (target : UndefinedPrimitive) (value : UndefinedValue) : ValueByte list =
        let source = value._Kind

        if source = target then
            value._Bytes
        else

        match UndefinedPrimitive.isFloat source, UndefinedPrimitive.isFloat target with
        | true, true ->
            match source, target with
            // Both spell a double: the stack's F type holds it as it is.
            | UndefinedPrimitive.Float64, UndefinedPrimitive.NativeFloat
            | UndefinedPrimitive.NativeFloat, UndefinedPrimitive.Float64 -> value._Bytes
            | _ ->
                let origin = List.head value.Origins
                List.replicate (UndefinedPrimitive.size target) (ValueByte.Undefined origin)
        | false, false ->
            let onStack = UndefinedPrimitive.widenForStack source value._Bytes
            let targetSize = UndefinedPrimitive.size target

            if targetSize <= List.length onStack then
                List.truncate targetSize onStack
            else
                // Only a 32-bit value can be narrower than its destination here, and CoreCLR
                // sign-extends it (`impImplicitIorI4Cast`).
                let sign =
                    match List.last onStack with
                    | ValueByte.Defined top -> ValueByte.Defined (if top >= 0x80uy then 0xFFuy else 0uy)
                    | ValueByte.Undefined origin -> ValueByte.Undefined origin

                onStack @ List.replicate (targetSize - List.length onStack) sign
        | _ ->
            failwith
                $"refusing to move the undefined %O{value} into a %A{target} slot: the evaluation stack has no move between a floating-point shape and an integer or reference one"

    /// The bytes `[offset, offset + count)` of `value`'s image. A range that must lie within the
    /// value; a caller asking for anything else has a bug.
    let bytesAt (offset : int) (count : int) (value : UndefinedValue) : ValueByte list =
        let size = UndefinedPrimitive.size value._Kind

        if offset < 0 || count < 0 || count > size - offset then
            failwith
                $"UndefinedValue.bytesAt: range of %d{count} byte(s) at offset %d{offset} is outside the %d{size}-byte %O{value}"

        value._Bytes |> List.skip offset |> List.truncate count

    /// `value` with the bytes at `offset` replaced by `bytes`. The result may be entirely
    /// defined, which is why this returns an image.
    let withBytesAt (offset : int) (bytes : ValueByte list) (value : UndefinedValue) : ValueByte list =
        let size = UndefinedPrimitive.size value._Kind
        let count = List.length bytes

        if offset < 0 || count > size - offset then
            failwith
                $"UndefinedValue.withBytesAt: range of %d{count} byte(s) at offset %d{offset} is outside the %d{size}-byte %O{value}"

        value._Bytes
        |> List.mapi (fun i existing ->
            if i >= offset && i < offset + count then
                List.item (i - offset) bytes
            else
                existing
        )

    /// Fail because `value` reached `site`, which would use its content rather than move it.
    ///
    /// Every such use is meant to be caught before it starts, where the run can end with the
    /// undefined value reported: an instruction's operands by `OperandUse`, a native method's or
    /// intrinsic's arguments before it runs. Reaching this means one of those checks missed a use,
    /// which is an interpreter bug rather than anything the guest did.
    let failUnobserved (site : string) (value : UndefinedValue) : 'a =
        failwith
            $"interpreter bug: the undefined %O{value} reached %s{site}, which uses its content; that use should have ended the run before %s{site} ran"
