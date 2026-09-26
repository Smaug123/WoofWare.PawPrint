namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata

[<RequireQualifiedAccess>]
module Corelib =

    let concretizeAll
        (loaded : LoadedAssemblies)
        (bct : BaseClassTypes<DumpedAssembly>)
        (t : AllConcreteTypes)
        : AllConcreteTypes
        =
        let ctx =
            {
                TypeConcretization.ConcretizationContext.ConcreteTypes = t
                TypeConcretization.ConcretizationContext.LoadedAssemblies = loaded
                TypeConcretization.ConcretizationContext.BaseTypes = bct
            }

        let loader = IAssemblyLoad.alreadyLoadedOnly

        let tys =
            [
                bct.String
                bct.Boolean
                bct.Char
                bct.SByte
                bct.Byte
                bct.Int16
                bct.UInt16
                bct.Int32
                bct.UInt32
                bct.Int64
                bct.UInt64
                bct.Single
                bct.Double
                bct.Array
                bct.Enum
                bct.ValueType
                bct.DelegateType
                bct.Object
                bct.RuntimeTypeHandle
                bct.RuntimeMethodHandle
                bct.RuntimeMethodInfoStub
                bct.RuntimeMethodHandleInternal
                bct.RuntimeFieldHandle
                bct.RuntimeFieldInfoStub
                bct.RuntimeFieldHandleInternal
                bct.RuntimeType
                bct.Void
                bct.TypedReference
                bct.IntPtr
                bct.UIntPtr
            ]

        (ctx, tys)
        ||> List.fold (fun ctx ty ->
            let stk = LoadedTypeInfo.signatureTypeKind ctx.BaseTypes ctx.LoadedAssemblies ty

            let _handle, ctx =
                TypeConcretization.concretizeType
                    ctx
                    loader
                    ty.AssemblyFullName
                    ImmutableArray.Empty
                    ImmutableArray.Empty
                    (TypeDefn.FromDefinition (ty.Identity, stk))

            ctx
        )
        |> _.ConcreteTypes

/// How a single-field wrapper value type flattens onto the eval stack.
///
/// Several BCL types are nominally `struct { single_field }` at metadata level (so `ldfld`,
/// reflection, and heap layout see a one-field struct), but the real CLR's JIT treats them
/// as if they were just the underlying primitive/reference. CLR enums have the same storage
/// shape — a single instance field `value__` at offset 0 — and the same treatment. At the
/// interpreter's eval-stack boundary we mirror that: storage keeps the wrapped struct form;
/// the stack sees the flattened primitive form via the kind below.
///
/// Which types get which kind is decided *nominally*, never from the storage shape: the BCL
/// wrappers by identity (`PrimitiveLikeStruct.kind`), enums by their base type. Two types with
/// identical fields can differ here, so a structural guess is not good enough.
[<RequireQualifiedAccess>]
type PrimitiveLikeKind =
    /// `System.IntPtr`, `System.UIntPtr` — flattens to `EvalStackValue.NativeInt`.
    | FlattenToNativeInt
    /// `System.RuntimeTypeHandle` (field `m_type : RuntimeType`),
    /// `System.RuntimeMethodHandle` (field `m_value : IRuntimeMethodInfo`),
    /// `System.RuntimeFieldHandle` (field `m_ptr : IRuntimeFieldInfo`) —
    /// flattens to `EvalStackValue.ObjectRef`. On CoreCLR these handles are ref-backed:
    /// `ldtoken` imports a managed reference, not a raw pointer.
    | FlattenToObjectRef
    /// `System.RuntimeMethodHandleInternal` (field `m_handle : IntPtr`),
    /// `System.RuntimeFieldHandleInternal` (field `m_handle : IntPtr`) —
    /// flattens to a runtime-pointer-valued `EvalStackValue.NativeInt`.
    | FlattenToRuntimePointer
    /// `System.ByReference`/`System.ByReference<T>` — flattens to `EvalStackValue.ManagedPointer`.
    | FlattenToManagedPointer
    /// A CLR enum over one of the fixed-width integers flattens to the `EvalStackValue` of that
    /// integer. ECMA III.1.8 treats enums as their underlying integer for every numeric/comparison
    /// opcode; the rewrap on pop reconstructs the enum slot around the coerced integer.
    ///
    /// Enum-ness here is nominal — the immediate base type is `System.Enum` — and is decided by
    /// whoever constructs the value, since the classifier sees only a handle and a field list.
    /// Deciding it from the CLR-reserved field name `value__` instead was issue #996: that name is
    /// legal C#, so an ordinary struct could take this kind and be flattened. An enum over
    /// `bool`, `char` or a native int is *not* this kind; see
    /// `CliValueType.EnumUnderlyingIsFlattenable`.
    | EnumLike

[<RequireQualifiedAccess>]
module PrimitiveLikeStruct =
    /// Returns `Some kind` if the concrete type is one of the BCL structs whose storage form is a
    /// single-field wrapper but whose eval-stack form should be the underlying primitive/reference.
    /// Returns `None` for everything else, including user-defined single-field structs.
    let kind (bct : BaseClassTypes<DumpedAssembly>) (ct : ConcreteType<'a>) : PrimitiveLikeKind option =
        if not ct.Generics.IsEmpty then
            // Only ByReference<T> is generic; match it structurally below if present.
            match bct.ByReference with
            | Some br when ct.Identity = br.Identity -> Some PrimitiveLikeKind.FlattenToManagedPointer
            | _ -> None
        else
            let identity = ct.Identity

            if identity = bct.IntPtr.Identity then
                Some PrimitiveLikeKind.FlattenToNativeInt
            elif identity = bct.UIntPtr.Identity then
                Some PrimitiveLikeKind.FlattenToNativeInt
            elif identity = bct.RuntimeTypeHandle.Identity then
                Some PrimitiveLikeKind.FlattenToObjectRef
            elif identity = bct.RuntimeMethodHandle.Identity then
                Some PrimitiveLikeKind.FlattenToObjectRef
            elif identity = bct.RuntimeMethodHandleInternal.Identity then
                Some PrimitiveLikeKind.FlattenToRuntimePointer
            elif identity = bct.RuntimeFieldHandle.Identity then
                Some PrimitiveLikeKind.FlattenToObjectRef
            elif identity = bct.RuntimeFieldHandleInternal.Identity then
                Some PrimitiveLikeKind.FlattenToRuntimePointer
            else
                match bct.ByReference with
                | Some br when identity = br.Identity -> Some PrimitiveLikeKind.FlattenToManagedPointer
                | _ -> None

    let isPrimitiveLike (bct : BaseClassTypes<DumpedAssembly>) (ct : ConcreteType<'a>) : bool =
        kind bct ct |> Option.isSome

    /// Resolve a `ConcreteTypeHandle` through `AllConcreteTypes` and classify it via `kind`.
    /// Returns `None` if the handle does not resolve or the type is not primitive-like.
    let kindFromHandle
        (bct : BaseClassTypes<DumpedAssembly>)
        (allCt : AllConcreteTypes)
        (h : ConcreteTypeHandle)
        : PrimitiveLikeKind option
        =
        match AllConcreteTypes.lookup h allCt with
        | None -> None
        | Some ct -> kind bct ct

/// Structural classification of BCL types that the runtime must special-case at opcode level.
///
/// ECMA-335 calls out a handful of types whose IL semantics differ from "ordinary" CLR types:
/// `IntPtr`/`UIntPtr` are primitive CLI types distinct from user-defined value types (III.1.1.1),
/// and `Nullable\`1` has bespoke box/unbox semantics (III.4.16). Multiple opcodes (`box`,
/// `unbox.any`, `ldobj`, `stobj`, `constrained.` …) need to discriminate these cases.
///
/// Rather than recomputing a name+namespace+assembly comparison at every use site -- which is
/// fragile (assembly check is easy to forget) and proliferates as new opcodes are taught the
/// distinction -- callers compute the kind once via `InternalTypeKind.kind`/`kindFromHandle` and
/// `match` on the result. The classification is identity-based: each variant is anchored on a
/// specific `BaseClassTypes` row, so a hostile assembly defining its own `System.Nullable\`1`
/// cannot accidentally be treated as the real one.
[<RequireQualifiedAccess>]
type InternalTypeKind =
    /// Anything that does not require runtime-level special-casing under this scheme.
    | Ordinary
    /// `System.IntPtr`. ECMA-335 `ELEMENT_TYPE_I`. Some opcodes (e.g. `unbox.any`) push
    /// `EvalStackValue.NativeInt` rather than `UserDefinedValueType` for this exact type.
    | NativeInt
    /// `System.UIntPtr`. ECMA-335 `ELEMENT_TYPE_U`. Behaves like `NativeInt` for stack
    /// purposes but is preserved as a separate variant so individual call sites can
    /// distinguish them if needed (e.g. signature checks that mandate `IntPtr` specifically).
    | NativeUInt
    /// Any instantiation of `System.Nullable\`1`. Requires the spec-mandated box/unbox
    /// semantics (box of null-valued Nullable becomes null; unbox.any reconstructs the
    /// Nullable from a boxed underlying value, etc.).
    | Nullable

[<RequireQualifiedAccess>]
module InternalTypeKind =
    /// Classify a concrete type against the BCL canonical identities. Comparison is by
    /// `ResolvedTypeIdentity` (assembly + TypeDef handle), so a Nullable\`1 instantiation
    /// matches the open-generic definition regardless of its generic arguments, and a
    /// user-defined `System.IntPtr` in a non-corelib assembly does not collide with the
    /// real one.
    let kind (bct : BaseClassTypes<DumpedAssembly>) (ct : ConcreteType<'a>) : InternalTypeKind =
        let identity = ct.Identity

        if identity = bct.Nullable.Identity then
            InternalTypeKind.Nullable
        elif ct.Generics.IsEmpty && identity = bct.IntPtr.Identity then
            InternalTypeKind.NativeInt
        elif ct.Generics.IsEmpty && identity = bct.UIntPtr.Identity then
            InternalTypeKind.NativeUInt
        else
            InternalTypeKind.Ordinary

    /// Resolve a `ConcreteTypeHandle` through `AllConcreteTypes` and classify it.
    /// Structural wrappers (Byref, Pointer, arrays, function pointers) are `Ordinary`:
    /// the spec's special cases attach to nominal value types, not their structural
    /// composers, so this is the correct identity for them under this classification.
    let kindFromHandle
        (bct : BaseClassTypes<DumpedAssembly>)
        (allCt : AllConcreteTypes)
        (h : ConcreteTypeHandle)
        : InternalTypeKind
        =
        match AllConcreteTypes.lookup h allCt with
        | None -> InternalTypeKind.Ordinary
        | Some ct -> kind bct ct
