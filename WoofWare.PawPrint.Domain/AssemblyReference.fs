namespace WoofWare.PawPrint

open System
open System.Reflection
open System.Reflection.Metadata

type AssemblyReference =
    {
        /// A handle relative to the specified assembly.
        Handle : AssemblyReferenceHandle * AssemblyName
        Culture : StringToken
        Flags : AssemblyFlags
        Name : AssemblyName
        /// <summary>
        /// <c>Name</c> serialised to a display name: this reference's <em>reference identity</em>, which
        /// <c>LoadedAssemblies</c> binds to a definition identity.
        /// </summary>
        /// <remarks>
        /// Stored rather than recomputed from <c>Name</c>, for the reason <c>AssemblyDefinition.FullName</c>
        /// is: a metadata-derived <c>AssemblyName</c> carries the public key rather than its token, and
        /// <c>AssemblyName.FullName</c> hashes the key to derive the token on every call.
        /// </remarks>
        FullName : string
        Version : Version
    }

[<RequireQualifiedAccess>]
module AssemblyReference =
    let make
        (handle : AssemblyReferenceHandle * AssemblyName)
        (ref : System.Reflection.Metadata.AssemblyReference)
        : AssemblyReference
        =
        let name = ref.GetAssemblyName ()

        {
            Handle = handle
            Culture = StringToken.String ref.Culture
            Flags = ref.Flags
            Name = name
            FullName = name.FullName
            Version = ref.Version
        }
