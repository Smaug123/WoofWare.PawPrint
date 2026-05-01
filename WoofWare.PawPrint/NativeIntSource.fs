namespace WoofWare.PawPrint

open System
open Checked

[<RequireQualifiedAccess>]
type UnsignedNativeIntSource =
    | Verbatim of uint64
    | FromManagedPointer of ManagedPointerSource

[<RequireQualifiedAccess>]
type RuntimeTypeHandleTarget =
    | Closed of ConcreteTypeHandle
    | OpenGenericTypeDefinition of ResolvedTypeIdentity

    override this.ToString () : string =
        match this with
        | RuntimeTypeHandleTarget.Closed handle -> string handle
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            $"open generic definition %s{identity.Assembly.Name}/%O{identity.TypeDefinition.Get}"

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
    /// Synthetic byte delta returned by `Unsafe.ByteOffset` or managed-pointer
    /// subtraction for two byrefs into distinct byte-addressed storage
    /// containers. We don't model managed object/frame addresses as integers,
    /// so the value is a deterministic sentinel large enough to defeat the
    /// unsigned overlap check `(nuint)offset < len` used by Memmove. The tag
    /// exists so downstream arithmetic (add/sub with anything non-zero) fails
    /// loudly rather than silently composing into a wrong answer; comparisons
    /// and Conv.U/Conv.I treat the payload as if it were a regular `Verbatim`.
    | SyntheticCrossArrayOffset of int64

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
        | NativeIntSource.SyntheticCrossArrayOffset i -> $"<synthetic cross-storage byte offset %i{i}>"

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
        | NativeIntSource.SyntheticCrossArrayOffset i -> HashCode.Combine (12, i)

[<RequireQualifiedAccess>]
module NativeIntSource =
    let internal syntheticCrossStorageSeparation : int64 = 1L <<< 40

    let syntheticCrossStorageByteOffset
        (originStorage : ByteStorageIdentity)
        (originByteOffset : int64)
        (targetStorage : ByteStorageIdentity)
        (targetByteOffset : int64)
        : NativeIntSource
        =
        if originStorage = targetStorage then
            failwith $"syntheticCrossStorageByteOffset called for two byrefs into the same storage: %O{originStorage}"

        // PawPrint heap/frame addresses are not real machine addresses, so
        // there is no honest byte distance between distinct storage
        // containers. Return a deterministic sentinel whose magnitude is
        // large enough to make Memmove's unsigned overlap check fail, while
        // preserving anti-symmetry: offset(a,b) = -offset(b,a).
        let storageOrdering =
            let comparison = ByteStorageIdentity.compare targetStorage originStorage

            if comparison < 0 then -1L
            elif comparison > 0 then 1L
            else 0L

        let storageSeparation = storageOrdering * syntheticCrossStorageSeparation

        NativeIntSource.SyntheticCrossArrayOffset (storageSeparation + (targetByteOffset - originByteOffset))

    let isZero (n : NativeIntSource) : bool =
        match n with
        | NativeIntSource.Verbatim i -> i = 0L
        | NativeIntSource.SyntheticCrossArrayOffset i -> i = 0L
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
        | NativeIntSource.SyntheticCrossArrayOffset i -> i >= 0L
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
        | NativeIntSource.SyntheticCrossArrayOffset a, NativeIntSource.Verbatim b -> a < b
        | NativeIntSource.Verbatim a, NativeIntSource.SyntheticCrossArrayOffset b -> a < b
        | NativeIntSource.SyntheticCrossArrayOffset a, NativeIntSource.SyntheticCrossArrayOffset b -> a < b
        | _, _ -> failwith "TODO"

type CliRuntimePointer =
    | Verbatim of int64
    | TypeHandlePtr of RuntimeTypeHandleTarget
    | FieldRegistryHandle of int64
    | MethodRegistryHandle of int64
    | MethodTablePtr of ConcreteTypeHandle
    | MethodTableAuxiliaryDataPtr of ConcreteTypeHandle
    | Managed of ManagedPointerSource
