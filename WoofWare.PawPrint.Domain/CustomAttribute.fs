namespace WoofWare.PawPrint

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
        /// The constructor method used to create this custom attribute instance.
        /// This token references the method that constructs the attribute.
        /// </summary>
        Constructor : MetadataToken
    }

[<RequireQualifiedAccess>]
module CustomAttribute =
    let make (handle : CustomAttributeHandle) (attr : System.Reflection.Metadata.CustomAttribute) : CustomAttribute =
        let ctor = attr.Constructor |> MetadataToken.ofEntityHandle

        {
            Handle = handle
            Constructor = ctor
        }
