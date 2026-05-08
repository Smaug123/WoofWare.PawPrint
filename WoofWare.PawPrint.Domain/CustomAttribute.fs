namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata

/// <summary>
/// Represents a custom attribute applied to a type, method, field, or other metadata entity.
/// This is a strongly-typed representation of CustomAttribute from System.Reflection.Metadata.
/// </summary>
type CustomAttribute =
    {
        /// <summary>
        /// The metadata token handle that uniquely identifies this custom attribute in the assembly.
        /// </summary>
        Handle : CustomAttributeHandle

        /// <summary>
        /// The metadata entity (TypeDef, MethodDef, FieldDef, Assembly, etc.) to which this
        /// custom attribute is applied.
        /// </summary>
        Parent : MetadataToken

        /// <summary>
        /// The constructor method used to create this custom attribute instance.
        /// This token references the method that constructs the attribute.
        /// </summary>
        Constructor : MetadataToken

        /// <summary>
        /// The encoded constructor-argument blob for this custom attribute (the bytes of
        /// the <c>Value</c> blob in ECMA-335 II.23.3). The CoreCLR <c>MetadataImport</c>
        /// returns this as a <c>ConstArray</c> from <c>GetCustomAttributeProps</c>.
        /// </summary>
        Value : ImmutableArray<byte>
    }

[<RequireQualifiedAccess>]
module CustomAttribute =
    let make
        (metadataReader : MetadataReader)
        (handle : CustomAttributeHandle)
        (attr : System.Reflection.Metadata.CustomAttribute)
        : CustomAttribute
        =
        let parent = attr.Parent |> MetadataToken.ofEntityHandle
        let ctor = attr.Constructor |> MetadataToken.ofEntityHandle

        let value : ImmutableArray<byte> =
            if attr.Value.IsNil then
                ImmutableArray.Empty
            else
                ImmutableArray.Create<byte> (metadataReader.GetBlobBytes attr.Value)

        {
            Handle = handle
            Parent = parent
            Constructor = ctor
            Value = value
        }
