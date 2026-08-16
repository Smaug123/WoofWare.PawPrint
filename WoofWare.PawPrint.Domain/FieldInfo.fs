namespace WoofWare.PawPrint

open System.Reflection
open System.Reflection.Metadata
open System.Runtime.InteropServices

/// Parsed form of a field's marshalling descriptor (ECMA-335 §II.23.4).
/// Only the cases the interpreter consumes are decoded structurally; everything
/// else is stashed in `Other` so callers can reject explicitly rather than silently treating a
/// field as having no descriptor.
type FieldMarshalDescriptor =
    /// `[MarshalAs(UnmanagedType.ByValTStr, SizeConst = N)]`. Inline fixed-size character array
    /// whose unmanaged byte size depends on the declaring type's CharSet.
    | ByValTStr of sizeConst : int
    /// `[MarshalAs(UnmanagedType.ByValArray, SizeConst = N, ArraySubType = elementType)]`.
    /// Inline fixed-size array; the element `UnmanagedType` is absent when the descriptor blob
    /// stops after the size constant.
    | ByValArray of sizeConst : int * elementType : UnmanagedType option
    /// Any other `UnmanagedType`. Preserved verbatim so callers can decide case-by-case.
    | Other of UnmanagedType

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

        /// Parsed `[MarshalAs(...)]` descriptor for fields with the HasFieldMarshal attribute, or
        /// `None` if the field has no marshalling descriptor. Drives unmanaged-size computation
        /// for `Marshal.SizeOf` and structure marshalling.
        MarshallingDescriptor : FieldMarshalDescriptor option

        /// True when this is a static field carrying `[System.ThreadStaticAttribute]`, i.e. one
        /// whose storage is per-thread rather than per-process. `[ThreadStatic]` is a custom
        /// attribute rather than a `FieldAttributes` flag, so this is computed once at parse
        /// time (see `FieldInfo.make`) rather than re-walking metadata at each access.
        ///
        /// The runtime ignores `[ThreadStatic]` on an instance field, and so do we: this is
        /// false for instance fields regardless of the attribute.
        IsThreadStatic : bool
    }

    member this.HasFieldRVA = this.Attributes.HasFlag FieldAttributes.HasFieldRVA
    member this.IsStatic = this.Attributes.HasFlag FieldAttributes.Static

    override this.ToString () : string =
        $"%s{this.DeclaringType.Assembly.Name}.{this.DeclaringType.Name}.%s{this.Name}"

[<RequireQualifiedAccess>]
module FieldMarshalDescriptor =
    /// Decode a field-marshal descriptor blob (ECMA-335 §II.23.4) into the structured form we
    /// need for sizing computations. Returns `None` if the blob is empty (which is invalid per
    /// the spec, but we tolerate it). Any unexpected trailing bytes are ignored — we only read
    /// the fields the standard says are present for the leading `NATIVE_TYPE`.
    let parse (mr : MetadataReader) (handle : BlobHandle) : FieldMarshalDescriptor option =
        let mutable reader = mr.GetBlobReader handle

        if reader.RemainingBytes = 0 then
            None
        else
            let nativeType : UnmanagedType =
                LanguagePrimitives.EnumOfValue (int32 (reader.ReadByte ()))

            match nativeType with
            | UnmanagedType.ByValTStr ->
                if reader.RemainingBytes = 0 then
                    Some (Other nativeType)
                else
                    let sizeConst = reader.ReadCompressedInteger ()
                    Some (ByValTStr sizeConst)
            | UnmanagedType.ByValArray ->
                let sizeConst =
                    if reader.RemainingBytes = 0 then
                        0
                    else
                        reader.ReadCompressedInteger ()

                let elementType =
                    if reader.RemainingBytes = 0 then
                        None
                    else
                        let raw = int32 (reader.ReadByte ())
                        Some (LanguagePrimitives.EnumOfValue raw : UnmanagedType)

                Some (ByValArray (sizeConst, elementType))
            | other -> Some (Other other)

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

        let marshallingDescriptor =
            if def.Attributes.HasFlag FieldAttributes.HasFieldMarshal then
                FieldMarshalDescriptor.parse mr (def.GetMarshallingDescriptor ())
            else
                None

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
