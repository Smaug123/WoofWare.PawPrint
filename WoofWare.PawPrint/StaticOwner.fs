namespace WoofWare.PawPrint

/// Which slot of a static field is being addressed.
///
/// A `[ThreadStatic]` field has one slot per thread; every other static has exactly one,
/// shared by the whole process. This is the key under which `IlMachineState._Statics`
/// partitions storage, and it is also recorded in `ByrefRoot.StaticField` so that a managed
/// pointer to a thread-static keeps addressing the slot of the thread that took it.
///
/// Slots are never reclaimed when a thread exits: a byref to a dead thread's slot may still
/// be held, and `ThreadId` allocation is monotonic, so no new thread can inherit a stale
/// slot. The retained storage is bounded by (threads x thread-static fields), and PawPrint
/// has no GC at all, so this adds no new class of leak.
[<RequireQualifiedAccess>]
type StaticOwner =
    /// The single process-wide slot of an ordinary static field.
    | Shared
    /// One thread's slot of a `[ThreadStatic]` field.
    | OwnedBy of thread : ThreadId

    override this.ToString () : string =
        match this with
        | StaticOwner.Shared -> "<shared static slot>"
        | StaticOwner.OwnedBy thread -> $"<thread-static slot of thread %O{thread}>"

[<RequireQualifiedAccess>]
module StaticOwner =
    /// Which slot of `field` the given thread addresses. This is the only production
    /// construction path for a `StaticOwner`: keeping it single means the pairing of
    /// "thread-static field" with "per-thread owner" cannot drift apart at a call site, even
    /// though the type system permits the illegal pairs.
    let forField (thread : ThreadId) (field : FieldInfo<'typeGeneric, 'fieldGeneric>) : StaticOwner =
        if field.IsThreadStatic then
            // `executeLdsflda` consults `peByteRangeForFieldRva` *before* the static-field
            // path, so a field that was both thread-static and RVA-backed would silently get
            // one shared PE-backed value for every thread, bypassing per-thread storage
            // entirely. RVA fields are compiler-synthesised array-initialiser data and never
            // carry `[ThreadStatic]`, so this should never fire.
            if field.HasFieldRVA then
                failwith
                    $"invariant violation: field %O{field} is both [ThreadStatic] and HasFieldRVA; RVA-backed storage is process-wide and cannot be given per-thread slots"

            StaticOwner.OwnedBy thread
        else
            StaticOwner.Shared
