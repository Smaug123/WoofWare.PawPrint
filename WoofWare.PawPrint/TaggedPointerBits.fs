namespace WoofWare.PawPrint

open System.Diagnostics

/// Result of a bitwise operation against the low "tag" region of a pointer whose
/// numeric value PawPrint does not model.
///
/// PawPrint never fabricates machine addresses (see
/// `docs/developer/pointers-and-byte-representations.md`), but managed code does
/// stuff tag bits into the low, known-clear bits of a pointer and later strip them
/// back off. `System.WeakReference` tags its GC handle with "tracks resurrection"
/// / "COM aware", and `System.Runtime.InteropServices.GCHandle` tags its handle
/// with "pinned". Such a pointer's value is modelled as
///
///     value = base ||| tag
///
/// where `base` is unknown and non-zero but is guaranteed to have its low
/// `tagWidthBits` bits clear — that is an alignment guarantee the real runtime
/// makes, not a fabricated address — and `tag` is known and lies in
/// `[0, 2^tagWidthBits)`.
///
/// A bitwise operation against such a value is answerable in exactly two shapes,
/// and otherwise not at all.
[<RequireQualifiedAccess>]
type TaggedPointerBitsResult =
    /// Every bit of the unknown base survived unchanged, so the result is still
    /// the same pointer, now carrying this tag. The tag is inside the tag region.
    | Retagged of tag : int64
    /// Every bit of the unknown base was forced to a constant, so the result is
    /// exactly these bits and is no longer a pointer.
    | TagBitsOnly of bits : int64
    /// The result would depend on bits of the base that PawPrint does not model.
    /// Callers must fail loudly rather than guess.
    | NotStatable

/// Decision procedure for bitwise operations against the low tag region of a
/// pointer whose numeric value PawPrint does not model. See
/// `TaggedPointerBitsResult` for the model this is derived from.
///
/// The rule is uniform across the operations: look at what the operation does to
/// each bit of the unknown base. The answer is `Retagged` exactly when every base
/// bit survives unchanged, `TagBitsOnly` exactly when every base bit is forced to
/// a constant, and `NotStatable` otherwise. Because `&&&`, `|||` and `^^^` act
/// independently on each bit position, that reduces to inspecting the operand's
/// high bits:
///
/// * `&&&` preserves bit `i` when the mask bit is 1 and clears it when it is 0;
/// * `|||` preserves bit `i` when the operand bit is 0 and forces it to 1 when it
///   is 1 (1 is OR's absorbing element — this case is easy to miss, and dropping
///   it would refuse an answer the model can give);
/// * `^^^` preserves bit `i` when the operand bit is 0 and inverts it when it is
///   1, and inversion of an unknown bit is never constant, so XOR has no
///   `TagBitsOnly` case at all.
///
/// This is sound (each stated answer holds for *every* admissible base) and
/// complete (a high region that is neither wholly preserved nor wholly forced
/// genuinely differs between two admissible bases, so no single answer exists).
[<RequireQualifiedAccess>]
module TaggedPointerBits =

    /// CoreCLR GC handles are pointers into the handle table, and CoreLib relies
    /// on their alignment to tag them: `System.WeakReferenceHandleTags` takes bit
    /// 0 for "tracks resurrection" ("handles are at least 2-byte aligned") and bit
    /// 1 for "COM aware" ("on COM-supporting platforms a handle is at least
    /// 4-byte aligned"), and `GCHandle` takes bit 0 for "pinned". Two bits is
    /// therefore exactly what CoreLib itself claims, and is also what
    /// `PointerHashSynthesis`'s `((n + 1) <<< 2)` counter scheme already leaves
    /// free for GC handles.
    let gcHandleTagWidthBits : int = 2

    /// Mask selecting the tag region: the low `tagWidthBits` bits.
    let tagMask (tagWidthBits : int) : int64 =
        Debug.Assert (
            tagWidthBits >= 0 && tagWidthBits < 63,
            $"tag width %i{tagWidthBits} out of range; a tag region must fit strictly inside an int64"
        )

        (1L <<< tagWidthBits) - 1L

    let private assertTagInRange (tagWidthBits : int) (tag : int64) : unit =
        Debug.Assert (
            tag &&& ~~~(tagMask tagWidthBits) = 0L,
            $"tag 0x%x{tag} escapes the %i{tagWidthBits}-bit tag region; tags must be produced by TaggedPointerBits"
        )

    /// `(base ||| tag) &&& mask`, where `base` is unknown.
    let bitAnd (tagWidthBits : int) (tag : int64) (mask : int64) : TaggedPointerBitsResult =
        assertTagInRange tagWidthBits tag
        let low = tagMask tagWidthBits
        let high = ~~~low
        let resultTag = tag &&& mask &&& low

        if mask &&& high = high then
            // Every base bit is kept.
            TaggedPointerBitsResult.Retagged resultTag
        elif mask &&& high = 0L then
            // Every base bit is cleared.
            TaggedPointerBitsResult.TagBitsOnly resultTag
        else
            TaggedPointerBitsResult.NotStatable

    /// `(base + offset) &&& mask`, where `base` is unknown but has its low
    /// `alignmentBits` bits clear and `offset` is a known, arbitrary displacement.
    ///
    /// This is the shape a byref takes: PawPrint models a managed pointer as a
    /// container whose start address it does not know, plus a known in-container
    /// byte offset, and the real runtime guarantees the container start is aligned
    /// (see `ManagedPointerSource.tryContainerAlignmentBits`). It reduces to
    /// `bitAnd` because
    ///
    ///     base + offset = (base + (offset &&& ~~~low)) ||| (offset &&& low)
    ///
    /// where `low = tagMask alignmentBits`: the left summand is a sum of two
    /// multiples of `2^alignmentBits`, so it is itself a multiple, hence an equally
    /// admissible base, and it shares no set bit with `offset &&& low`, so `+` and
    /// `|||` agree. `bitAnd` quantifies over *all* admissible bases, so replacing
    /// one by another does not weaken its answer.
    let bitAndOffsetFromAlignedBase (alignmentBits : int) (offset : int64) (mask : int64) : TaggedPointerBitsResult =
        bitAnd alignmentBits (offset &&& tagMask alignmentBits) mask

    /// `(base ||| tag) ||| operand`, where `base` is unknown.
    let bitOr (tagWidthBits : int) (tag : int64) (operand : int64) : TaggedPointerBitsResult =
        assertTagInRange tagWidthBits tag
        let low = tagMask tagWidthBits
        let high = ~~~low
        let resultTag = (tag ||| operand) &&& low

        if operand &&& high = 0L then
            // Every base bit is kept.
            TaggedPointerBitsResult.Retagged resultTag
        elif operand &&& high = high then
            // Every base bit is forced to 1, so the whole value is known.
            TaggedPointerBitsResult.TagBitsOnly (high ||| resultTag)
        else
            TaggedPointerBitsResult.NotStatable

    /// `(base ||| tag) ^^^ operand`, where `base` is unknown. XOR never forces a
    /// base bit to a constant, so there is no `TagBitsOnly` case.
    let bitXor (tagWidthBits : int) (tag : int64) (operand : int64) : TaggedPointerBitsResult =
        assertTagInRange tagWidthBits tag
        let low = tagMask tagWidthBits
        let high = ~~~low

        if operand &&& high = 0L then
            // Every base bit is kept.
            TaggedPointerBitsResult.Retagged ((tag ^^^ operand) &&& low)
        else
            TaggedPointerBitsResult.NotStatable
