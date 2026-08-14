namespace WoofWare.PawPrint

/// The guest's static-field storage: the one process-wide slot of every ordinary static, and
/// the per-thread slots of every `[ThreadStatic]`.
///
/// The representation is hidden, for the same reason `ManagedHeap`'s is. Statics are shared
/// mutable guest state — a plain `static int` is the canonical data race — so every read and
/// write of one must be attributable to a caller, and a caller that reached into the maps
/// directly would be invisible to that machinery. Until this type existed the only thing
/// asking callers not to was the leading underscore on `IlMachineState._Statics`, which is a
/// convention rather than a check; see `WoofWare.PawPrint/ManagedHeap.fsi` for the same
/// argument made about the heap.
///
/// The `StaticOwner` key is what makes a thread-static's slots separate values rather than
/// one contended location, so it is also the type-level record of which accesses *cannot*
/// race: two threads touching `StaticOwner.OwnedBy` slots are touching different storage.
[<Sealed>]
type StaticStorage

[<RequireQualifiedAccess>]
module StaticStorage =
    /// Storage in which no static has been written yet.
    ///
    /// Not the same as "every static holds its zero": an unwritten slot *misses*, and the
    /// caller supplies the zero. That is what keeps zero-initialisation lazy, and it is why
    /// `get` returns an option rather than a zero of the field's type — this type never sees
    /// a field's type, so it could not construct one.
    val empty : StaticStorage

    /// The value in the slot of `field` (declared by `ty`) belonging to `owner`, or `None`
    /// if that slot has never been written.
    ///
    /// All three of `owner`, `ty` and `field` are part of the slot's identity: a value
    /// written under one owner is not visible under another (that is the whole of
    /// `[ThreadStatic]`), and neither is a value written under a different declaring type or
    /// a different field.
    ///
    /// `field` is scoped to the assembly that defines `ty`'s type; handles from different
    /// assemblies must not be mixed under the same `ty`.
    val get :
        owner : StaticOwner ->
        ty : ConcreteTypeHandle ->
        field : ComparableFieldDefinitionHandle ->
        storage : StaticStorage ->
            CliType option

    /// Overwrite the slot of `field` (declared by `ty`) belonging to `owner`, creating it if
    /// this is its first write. Leaves every other slot alone, including the same field's
    /// slots under other owners.
    val set :
        owner : StaticOwner ->
        ty : ConcreteTypeHandle ->
        field : ComparableFieldDefinitionHandle ->
        value : CliType ->
        storage : StaticStorage ->
            StaticStorage
