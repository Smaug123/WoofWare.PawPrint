namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Runtime.InteropServices

/// A field's marshalling descriptor (ECMA-335 II.23.4) as CoreCLR's `MarshalInfo` (mlinfo.cpp) sees
/// it when laying the field out for marshalling. Only the cases the interpreter consumes are
/// decoded structurally; every other native type is stashed in `Other` so callers can reject
/// explicitly rather than silently treating a field as having no descriptor.
type FieldMarshalDescriptor =
    /// `[MarshalAs(UnmanagedType.ByValTStr, SizeConst = N)]`. Inline fixed-size character array
    /// whose unmanaged byte size depends on the declaring type's CharSet.
    | ByValTStr of sizeConst : int
    /// `[MarshalAs(UnmanagedType.ByValArray, SizeConst = N, ArraySubType = elementType)]`.
    /// Inline fixed-size array. The element `UnmanagedType` is `None` where `MarshalInfo` is left
    /// with `NATIVE_TYPE_DEFAULT`, and so picks the element's native type from the managed element
    /// type: when the blob stops after the size, when what follows it is not a well-formed
    /// compressed integer, and when it spells out `NATIVE_TYPE_DEFAULT` (0x50) itself.
    | ByValArray of sizeConst : int * elementType : UnmanagedType option
    /// Any other `UnmanagedType`. Preserved verbatim so callers can decide case-by-case.
    | Other of UnmanagedType
    /// A non-empty blob, led by this `NATIVE_TYPE_*` byte, which CoreCLR's `ParseNativeTypeInfo`
    /// refuses: a `ByValTStr` or `ByValArray` without a well-formed size, or a custom marshaler
    /// without all four of its strings. `MarshalInfo` then marks the field illegal to marshal, so
    /// `Marshal.SizeOf` of any struct containing it throws `ArgumentException`.
    | Malformed of nativeType : byte
    /// A `FieldMarshal` row whose blob is empty. CoreCLR's two readers of the blob disagree about
    /// it: `IsFieldBlittable` (fieldmarshaler.cpp) reads it as no descriptor, while
    /// `ParseNativeTypeInfo` refuses it. So `Marshal.SizeOf` answers the managed size when every
    /// field of the struct is blittable, because it then never lays the struct out for marshalling,
    /// and throws `ArgumentException` otherwise.
    | Empty

/// <summary>
/// Represents detailed information about a field in a .NET assembly.
/// This is a strongly-typed representation of FieldDefinition from System.Reflection.Metadata.
/// </summary>
type FieldInfo<'typeGeneric, 'fieldGeneric> =
    {
        /// <summary>
        /// The metadata token handle that uniquely identifies this field in the assembly.
        /// </summary>
        Handle : FieldDefinitionHandle

        /// <summary>The name of the field.</summary>
        Name : string

        /// <summary>
        /// The type that declares this field.
        /// </summary>
        DeclaringType : ConcreteType<'typeGeneric>

        /// <summary>
        /// The type of the field.
        /// </summary>
        Signature : 'fieldGeneric

        /// <summary>
        /// The attributes applied to this field, including visibility, static/instance,
        /// literal, and other characteristics.
        /// </summary>
        Attributes : FieldAttributes

        /// Static fields don't have an offset at all; also, instance fields which don't have an explicit offset (but
        /// which of course do have one implicitly, which is most fields) are None here.
        Offset : int option

        /// The Relative Virtual Address for fields with the HasFieldRVA attribute.
        /// This points to the raw data in the PE image for fields used in array initialization, etc.
        RelativeVirtualAddress : int option

        /// Parsed `[MarshalAs(...)]` descriptor, or `None` if the field has no marshalling
        /// descriptor or one that spells out `NATIVE_TYPE_DEFAULT`, which CoreCLR treats
        /// identically. Drives unmanaged-size computation for `Marshal.SizeOf` and structure
        /// marshalling.
        MarshallingDescriptor : FieldMarshalDescriptor option

        /// True when this is a static field carrying `[System.ThreadStaticAttribute]`, i.e. one
        /// whose storage is per-thread rather than per-process. `[ThreadStatic]` is a custom
        /// attribute rather than a `FieldAttributes` flag, so it cannot be read off `Attributes`.
        ///
        /// The runtime ignores `[ThreadStatic]` on an instance field, and so do we: this is
        /// false for instance fields regardless of the attribute.
        IsThreadStatic : bool
    }

    member this.HasFieldRVA = this.Attributes.HasFlag FieldAttributes.HasFieldRVA
    member this.IsStatic = this.Attributes.HasFlag FieldAttributes.Static

    override this.ToString () : string =
        let assembly = AssemblyDefinitionName.simpleName this.DeclaringType.AssemblyFullName

        $"%s{assembly}.{this.DeclaringType.Name}.%s{this.Name}"

[<RequireQualifiedAccess>]
module FieldMarshalDescriptor =
    /// The descriptor `MarshalInfo` reads from a field's MarshalSpec blob (ECMA-335 II.23.4):
    /// CoreCLR's `ParseNativeTypeInfo`, as `NativeTypeParamInfo.parse` reproduces it, projected onto
    /// the struct `MarshalInfo` starts from. `None` where that struct is left at
    /// `NATIVE_TYPE_DEFAULT`, i.e. where the blob's leading byte spells it out.
    let ofBlob (blob : ImmutableArray<byte>) : FieldMarshalDescriptor option =
        if blob.IsEmpty then
            Some FieldMarshalDescriptor.Empty
        else

        match NativeTypeParamInfo.parse blob with
        | None -> Some (FieldMarshalDescriptor.Malformed blob.[0])
        | Some info ->

        // `parse` writes the size of both fixed shapes whenever it succeeds on them.
        let sizeConst () : int =
            match info.Additive with
            | Some size -> int size
            | None ->
                failwith
                    $"NativeTypeParamInfo.parse succeeded on NATIVE_TYPE 0x%02x{info.NativeType} without writing its size"

        match info.NativeType with
        | NativeTypeParamInfo.NativeTypeDefault -> None
        | NativeTypeParamInfo.NativeTypeFixedSysString -> Some (FieldMarshalDescriptor.ByValTStr (sizeConst ()))
        | NativeTypeParamInfo.NativeTypeFixedArray ->
            let elementType =
                info.ArrayElementType
                |> Option.filter (fun elementType -> elementType <> uint32 NativeTypeParamInfo.NativeTypeDefault)
                |> Option.map (fun elementType -> (LanguagePrimitives.EnumOfValue (int elementType) : UnmanagedType))

            Some (FieldMarshalDescriptor.ByValArray (sizeConst (), elementType))
        | other -> Some (FieldMarshalDescriptor.Other (LanguagePrimitives.EnumOfValue (int other)))

    /// `ofBlob` of the blob at `handle`.
    let parse (mr : MetadataReader) (handle : BlobHandle) : FieldMarshalDescriptor option =
        ofBlob (mr.GetBlobContent handle)

[<RequireQualifiedAccess>]
module FieldInfo =
    /// Does this field carry `[System.ThreadStaticAttribute]`?
    ///
    /// Accepted risk (consistent with the existing precedent in `MethodInfo.isIntrinsicAttribute`,
    /// and inherited from `CustomAttribute.constructorParentName`): the match is on namespace+name
    /// strings and does not verify that the type resolves to corelib's
    /// `System.ThreadStaticAttribute`.
    let private hasThreadStaticAttribute
        (mr : MetadataReader)
        (describeField : unit -> string)
        (def : FieldDefinition)
        : bool
        =
        let describeTarget () = $"field %s{describeField ()}"

        def.GetCustomAttributes ()
        |> Seq.exists (fun handle ->
            let attr = mr.GetCustomAttribute handle

            match CustomAttribute.constructorParentName mr describeTarget attr.Constructor with
            | Some (ns, name) -> ns = "System" && name = "ThreadStaticAttribute"
            | None -> false
        )

    let make
        (mr : MetadataReader)
        (assembly : AssemblyName)
        (handle : FieldDefinitionHandle)
        (def : FieldDefinition)
        : FieldInfo<GenericParamFromMetadata, TypeDefn>
        =
        let name = mr.GetString def.Name
        let fieldSig = def.DecodeSignature (TypeDefn.typeProvider assembly, ())
        let declaringType = def.GetDeclaringType ()

        let decType = mr.GetTypeDefinition declaringType

        let typeGenerics =
            decType.GetGenericParameters () |> GenericParameter.readAll assembly mr

        let declaringTypeNamespace = mr.GetString decType.Namespace
        let declaringTypeName = mr.GetString decType.Name

        let declaringType =
            ConcreteType.make assembly declaringType declaringTypeNamespace declaringTypeName typeGenerics

        let offset =
            match def.GetOffset () with
            | -1 -> None
            | s -> Some s

        let rva =
            let v = def.GetRelativeVirtualAddress ()
            if v = 0 then None else Some v

        // CoreCLR looks up the `FieldMarshal` row whatever the field's flags say. A nil handle is
        // either no row or a row naming the empty blob at heap offset 0, which System.Reflection.Metadata
        // does not tell apart; ECMA-335 II.22.15 requires the `HasFieldMarshal` flag exactly when
        // the row exists, so the flag decides between them.
        let marshallingDescriptor =
            let handle = def.GetMarshallingDescriptor ()

            if handle.IsNil && not (def.Attributes.HasFlag FieldAttributes.HasFieldMarshal) then
                None
            else
                FieldMarshalDescriptor.parse mr handle

        // `[ThreadStatic]` is a custom attribute rather than a `FieldAttributes` flag, so it is
        // computed once here at parse time rather than re-walking metadata at each access.
        // The runtime ignores `[ThreadStatic]` on an instance field, and so must we; checking
        // staticness first also short-circuits the metadata walk for the common case.
        let isThreadStatic =
            def.Attributes.HasFlag FieldAttributes.Static
            && hasThreadStaticAttribute
                mr
                (fun () -> $"%s{assembly.Name}!%s{declaringTypeNamespace}.%s{declaringTypeName}::%s{name}")
                def

        {
            Name = name
            Signature = fieldSig
            DeclaringType = declaringType
            Handle = handle
            Attributes = def.Attributes
            Offset = offset
            RelativeVirtualAddress = rva
            MarshallingDescriptor = marshallingDescriptor
            IsThreadStatic = isThreadStatic
        }

    let mapTypeGenerics<'a, 'b, 'field> (f : int -> 'a -> 'b) (input : FieldInfo<'a, 'field>) : FieldInfo<'b, 'field> =
        let declaringType = input.DeclaringType |> ConcreteType.mapGeneric f

        {
            Handle = input.Handle
            Name = input.Name
            DeclaringType = declaringType
            Signature = input.Signature
            Attributes = input.Attributes
            Offset = input.Offset
            RelativeVirtualAddress = input.RelativeVirtualAddress
            MarshallingDescriptor = input.MarshallingDescriptor
            IsThreadStatic = input.IsThreadStatic
        }
