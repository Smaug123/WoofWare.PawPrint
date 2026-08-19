namespace WoofWare.PawPrint

open System.Reflection

/// <summary>
/// Represents a .NET assembly definition.
/// This is a strongly-typed representation of AssemblyDefinition from System.Reflection.Metadata.
/// </summary>
type AssemblyDefinition =
    {
        /// <summary>
        /// The fully specified name of the assembly, including name, version, culture, and public key token.
        /// </summary>
        Name : AssemblyName
        /// <summary>
        /// <c>Name</c> serialised to a display name: this assembly's <em>definition identity</em>, which
        /// <c>LoadedAssemblies</c> keys on.
        /// </summary>
        /// <remarks>
        /// Stored rather than recomputed from <c>Name</c>. A metadata-derived <c>AssemblyName</c> carries the
        /// assembly's public <em>key</em> rather than its token, and <c>AssemblyName.FullName</c> derives the
        /// token from the key on every call — a SHA-1 over the key, and 613 bytes, each time, over a value
        /// that cannot change.
        /// </remarks>
        FullName : string
    }

[<RequireQualifiedAccess>]
module AssemblyDefinition =
    let make (assy : System.Reflection.Metadata.AssemblyDefinition) : AssemblyDefinition =
        let name = assy.GetAssemblyName ()

        {
            Name = name
            FullName = name.FullName
        }
