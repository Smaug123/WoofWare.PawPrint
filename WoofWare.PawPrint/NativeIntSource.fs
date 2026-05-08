namespace WoofWare.PawPrint

open System
open Checked

/// The delta between the addresses of two locations in memory that aren't within the same ByteStorageIdentity.
type SyntheticCrossArrayOffset =
    private
        {
            _TargetRoot : ByteStorageIdentity
            _TargetOffset : int64
            _SourceRoot : ByteStorageIdentity
            _SourceOffset : int64
        }

module SyntheticCrossArrayOffset =
    let make
        (targetRoot : ByteStorageIdentity)
        (targetOffset : int64)
        (sourceRoot : ByteStorageIdentity)
        (sourceOffset : int64)
        =
        if targetRoot = sourceRoot then
            failwith "not a cross-array offset"

        {
            _TargetRoot = targetRoot
            _TargetOffset = targetOffset
            _SourceRoot = sourceRoot
            _SourceOffset = sourceOffset
        }

    let negate (s : SyntheticCrossArrayOffset) =
        {
            _TargetRoot = s._SourceRoot
            _TargetOffset = s._SourceOffset
            _SourceRoot = s._TargetRoot
            _SourceOffset = s._TargetOffset
        }

    let targetRoot (s : SyntheticCrossArrayOffset) : ByteStorageIdentity = s._TargetRoot
    let targetOffset (s : SyntheticCrossArrayOffset) : int64 = s._TargetOffset
    let sourceRoot (s : SyntheticCrossArrayOffset) : ByteStorageIdentity = s._SourceRoot
    let sourceOffset (s : SyntheticCrossArrayOffset) : int64 = s._SourceOffset

    /// A SyntheticCrossArrayOffset is semantically a difference between memory addresses, so it is a native int.
    /// Various parts of the BCL ask to compare it against integers.
    /// For example, Memmove asks whether the source and dest overlap, by asking whether dest - source < len.
    /// PawPrint doesn't really model the address space as an array of bytes at all, but it *can* reply to the question
    /// "is this delta small", and that's what this function does.
    let internal cltVerbatim (_ : SyntheticCrossArrayOffset) (positiveComparand : int64) =
        if positiveComparand < 0L then
            failwith "cltVerbatim arg must be nonnegative"

        if positiveComparand >= (1L <<< 40) then
            failwith $"cltVerbatim can only compare with small deltas, got %i{positiveComparand}"
        // TODO: it *is* possible for people to do arithmetic on addresses e.g. a PE image.
        // I really hope nobody does that.
        false

    /// A SyntheticCrossArrayOffset is semantically a difference between memory addresses, so it is a native int.
    /// Various parts of the BCL ask to compare it against integers.
    /// For example, Memmove asks whether the source and dest overlap, by asking whether dest - source < len.
    /// PawPrint doesn't really model the address space as an array of bytes at all, but it *can* reply to the question
    /// "is this delta small", and that's what this function does.
    let internal cgtVerbatim (_ : SyntheticCrossArrayOffset) (positiveComparand : int64) =
        if positiveComparand < 0L then
            failwith "cgtVerbatim arg must be nonnegative"

        if positiveComparand >= (1L <<< 40) then
            failwith $"cgtVerbatim can only compare with small deltas, got %i{positiveComparand}"
        // TODO: it *is* possible for people to do arithmetic on addresses e.g. a PE image.
        // I really hope nobody does that.
        true

[<RequireQualifiedAccess>]
type UnsignedNativeIntSource =
    | Verbatim of uint64
    | FromManagedPointer of ManagedPointerSource
    | FromSyntheticCrossArrayStorage of SyntheticCrossArrayOffset

[<RequireQualifiedAccess>]
type RuntimeTypeHandleTarget =
    | Closed of ConcreteTypeHandle
    | OpenGenericTypeDefinition of ResolvedTypeIdentity
    /// A generic type parameter (e.g. T in IEquatable<T>), identified by its declaring
    /// type and zero-based position. Surfaced through reflection as a RuntimeType with
    /// IsGenericParameter = true. Method generic parameters are not yet represented.
    | GenericParameter of declaringType : ResolvedTypeIdentity * position : int

    override this.ToString () : string =
        match this with
        | RuntimeTypeHandleTarget.Closed handle -> string handle
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            $"open generic definition %s{identity.Assembly.Name}/%O{identity.TypeDefinition.Get}"
        | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
            $"generic parameter #%i{position} of %s{declaringType.Assembly.Name}/%O{declaringType.TypeDefinition.Get}"

[<RequireQualifiedAccess>]
[<CustomEquality>]
[<NoComparison>]
type NativeIntSource =
    | Verbatim of int64
    | ManagedPointer of ManagedPointerSource
    | FunctionPointer of MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
    | TypeHandlePtr of RuntimeTypeHandleTarget
    | MethodTablePtr of ConcreteTypeHandle
    | MethodTableAuxiliaryDataPtr of ConcreteTypeHandle
    | MethodHandlePtr of int64
    | FieldHandlePtr of int64
    | AssemblyHandle of string
    | ModuleHandle of string
    | MetadataImportHandle of string
    | GcHandlePtr of GcHandleAddress
    /// Returned by `Unsafe.ByteOffset` or managed-pointer subtraction for two byrefs into distinct byte-addressed
    /// storage containers.
    | SyntheticCrossArrayOffset of SyntheticCrossArrayOffset

    override this.ToString () : string =
        match this with
        | NativeIntSource.Verbatim int64 -> $"%i{int64}"
        | NativeIntSource.ManagedPointer ptr -> $"<managed pointer {ptr}>"
        | NativeIntSource.FunctionPointer methodDefinition ->
            $"<pointer to {methodDefinition.Name} in {methodDefinition.DeclaringType.Assembly.Name}>"
        | NativeIntSource.TypeHandlePtr ptr -> $"<type ID %O{ptr}>"
        | NativeIntSource.MethodTablePtr ptr -> $"<method table for type %O{ptr}>"
        | NativeIntSource.MethodTableAuxiliaryDataPtr ptr -> $"<method table auxiliary data for type %O{ptr}>"
        | NativeIntSource.MethodHandlePtr ptr -> $"<method ID %O{ptr}>"
        | NativeIntSource.FieldHandlePtr ptr -> $"<field ID %O{ptr}>"
        | NativeIntSource.AssemblyHandle name -> $"<assembly %s{name}>"
        | NativeIntSource.ModuleHandle name -> $"<module %s{name}>"
        | NativeIntSource.MetadataImportHandle name -> $"<metadata import for %s{name}>"
        | NativeIntSource.GcHandlePtr handle -> $"<GC handle %O{handle}>"
        | NativeIntSource.SyntheticCrossArrayOffset _ -> "<synthetic cross-storage byte offset>"

    override this.Equals (other : obj) : bool =
        match other with
        | :? NativeIntSource as other ->
            match this, other with
            | NativeIntSource.Verbatim left, NativeIntSource.Verbatim right -> left = right
            | NativeIntSource.ManagedPointer left, NativeIntSource.ManagedPointer right -> left = right
            | NativeIntSource.FunctionPointer left, NativeIntSource.FunctionPointer right ->
                MethodInfo.NominallyEqual left right
            | NativeIntSource.TypeHandlePtr left, NativeIntSource.TypeHandlePtr right -> left = right
            | NativeIntSource.MethodTablePtr left, NativeIntSource.MethodTablePtr right -> left = right
            | NativeIntSource.MethodTableAuxiliaryDataPtr left, NativeIntSource.MethodTableAuxiliaryDataPtr right ->
                left = right
            | NativeIntSource.MethodHandlePtr left, NativeIntSource.MethodHandlePtr right -> left = right
            | NativeIntSource.FieldHandlePtr left, NativeIntSource.FieldHandlePtr right -> left = right
            | NativeIntSource.AssemblyHandle left, NativeIntSource.AssemblyHandle right -> left = right
            | NativeIntSource.ModuleHandle left, NativeIntSource.ModuleHandle right -> left = right
            | NativeIntSource.MetadataImportHandle left, NativeIntSource.MetadataImportHandle right -> left = right
            | NativeIntSource.GcHandlePtr left, NativeIntSource.GcHandlePtr right -> left = right
            | NativeIntSource.SyntheticCrossArrayOffset left, NativeIntSource.SyntheticCrossArrayOffset right ->
                left = right
            | NativeIntSource.Verbatim _, _
            | NativeIntSource.ManagedPointer _, _
            | NativeIntSource.FunctionPointer _, _
            | NativeIntSource.TypeHandlePtr _, _
            | NativeIntSource.MethodTablePtr _, _
            | NativeIntSource.MethodTableAuxiliaryDataPtr _, _
            | NativeIntSource.MethodHandlePtr _, _
            | NativeIntSource.FieldHandlePtr _, _
            | NativeIntSource.AssemblyHandle _, _
            | NativeIntSource.ModuleHandle _, _
            | NativeIntSource.MetadataImportHandle _, _
            | NativeIntSource.GcHandlePtr _, _
            | NativeIntSource.SyntheticCrossArrayOffset _, _ -> false
        | _ -> false

    override this.GetHashCode () : int =
        match this with
        | NativeIntSource.Verbatim int64 -> HashCode.Combine (0, int64)
        | NativeIntSource.ManagedPointer ptr -> HashCode.Combine (1, ptr)
        | NativeIntSource.FunctionPointer methodDefinition ->
            HashCode.Combine (
                2,
                methodDefinition.DeclaringType.Identity,
                methodDefinition.DeclaringType.Generics,
                methodDefinition.Handle,
                methodDefinition.Generics
            )
        | NativeIntSource.TypeHandlePtr ptr -> HashCode.Combine (3, ptr)
        | NativeIntSource.MethodTablePtr ptr -> HashCode.Combine (4, ptr)
        | NativeIntSource.MethodTableAuxiliaryDataPtr ptr -> HashCode.Combine (5, ptr)
        | NativeIntSource.MethodHandlePtr ptr -> HashCode.Combine (6, ptr)
        | NativeIntSource.FieldHandlePtr ptr -> HashCode.Combine (7, ptr)
        | NativeIntSource.AssemblyHandle name -> HashCode.Combine (8, name)
        | NativeIntSource.ModuleHandle name -> HashCode.Combine (9, name)
        | NativeIntSource.MetadataImportHandle name -> HashCode.Combine (10, name)
        | NativeIntSource.GcHandlePtr handle -> HashCode.Combine (11, handle)
        | NativeIntSource.SyntheticCrossArrayOffset s -> HashCode.Combine (12, hash s)

[<RequireQualifiedAccess>]
module NativeIntSource =
    let syntheticCrossStorageByteOffset
        (originStorage : ByteStorageIdentity)
        (originByteOffset : int64)
        (targetStorage : ByteStorageIdentity)
        (targetByteOffset : int64)
        : NativeIntSource
        =
        SyntheticCrossArrayOffset.make targetStorage targetByteOffset originStorage originByteOffset
        |> NativeIntSource.SyntheticCrossArrayOffset

    let isZero (n : NativeIntSource) : bool =
        match n with
        | NativeIntSource.Verbatim i -> i = 0L
        | NativeIntSource.SyntheticCrossArrayOffset s -> SyntheticCrossArrayOffset.cltVerbatim s 1L
        | NativeIntSource.FieldHandlePtr _
        | NativeIntSource.MethodHandlePtr _
        | NativeIntSource.TypeHandlePtr _
        | NativeIntSource.MethodTablePtr _
        | NativeIntSource.MethodTableAuxiliaryDataPtr _
        | NativeIntSource.GcHandlePtr _
        | NativeIntSource.AssemblyHandle _
        | NativeIntSource.MetadataImportHandle _
        | NativeIntSource.ModuleHandle _ -> false
        | NativeIntSource.FunctionPointer _ -> failwith "TODO"
        | NativeIntSource.ManagedPointer src ->
            match src with
            | ManagedPointerSource.Null -> true
            | _ -> false

    let isNonnegative (n : NativeIntSource) : bool =
        match n with
        | NativeIntSource.Verbatim i -> i >= 0L
        | NativeIntSource.SyntheticCrossArrayOffset _ ->
            failwith "Most isNonnegative of cross-array offsets are not meaningful"
        | NativeIntSource.FunctionPointer _ -> failwith "TODO"
        | NativeIntSource.FieldHandlePtr _
        | NativeIntSource.MethodHandlePtr _
        | NativeIntSource.TypeHandlePtr _
        | NativeIntSource.MethodTablePtr _
        | NativeIntSource.MethodTableAuxiliaryDataPtr _
        | NativeIntSource.GcHandlePtr _
        | NativeIntSource.AssemblyHandle _
        | NativeIntSource.MetadataImportHandle _
        | NativeIntSource.ModuleHandle _ -> true
        | NativeIntSource.ManagedPointer _ -> true

    /// True if a < b.
    let isLess (a : NativeIntSource) (b : NativeIntSource) : bool =
        match a, b with
        | NativeIntSource.Verbatim a, NativeIntSource.Verbatim b -> a < b
        | NativeIntSource.SyntheticCrossArrayOffset _, NativeIntSource.Verbatim _
        | NativeIntSource.Verbatim _, NativeIntSource.SyntheticCrossArrayOffset _ ->
            failwith "TODO: cross-array offsets hopefully aren't meaningfully compared with ints"
        | NativeIntSource.SyntheticCrossArrayOffset _, NativeIntSource.SyntheticCrossArrayOffset _ -> failwith "TODO"
        | _, _ -> failwith "TODO"

type CliRuntimePointer =
    | Verbatim of int64
    | TypeHandlePtr of RuntimeTypeHandleTarget
    | FieldRegistryHandle of int64
    | MethodRegistryHandle of int64
    | MethodTablePtr of ConcreteTypeHandle
    | MethodTableAuxiliaryDataPtr of ConcreteTypeHandle
    | Managed of ManagedPointerSource
