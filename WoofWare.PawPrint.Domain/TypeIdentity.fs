namespace WoofWare.PawPrint

open System.Reflection
open System.Reflection.Metadata

[<NoComparison>]
type MetadataTypeIdentifier =
    private
        {
            ReferencedInAssembly : AssemblyName
            Token : MetadataToken
        }

    member this.Assembly = this.ReferencedInAssembly
    member this.MetadataToken = this.Token

[<RequireQualifiedAccess>]
module MetadataTypeIdentifier =
    let private make (assemblyName : AssemblyName) (token : MetadataToken) : MetadataTypeIdentifier =
        match token with
        | MetadataToken.TypeDefinition _
        | MetadataToken.TypeReference _
        | MetadataToken.TypeSpecification _ ->
            {
                ReferencedInAssembly = assemblyName
                Token = token
            }
        | _ -> failwithf "MetadataTypeIdentifier can only be constructed from type metadata tokens, but got %A" token

    let ofTypeDef (assemblyName : AssemblyName) (handle : TypeDefinitionHandle) : MetadataTypeIdentifier =
        make assemblyName (MetadataToken.TypeDefinition handle)

    let ofTypeRef (assemblyName : AssemblyName) (handle : TypeReferenceHandle) : MetadataTypeIdentifier =
        make assemblyName (MetadataToken.TypeReference handle)

    let ofTypeSpec (assemblyName : AssemblyName) (handle : TypeSpecificationHandle) : MetadataTypeIdentifier =
        make assemblyName (MetadataToken.TypeSpecification handle)

[<CustomEquality>]
[<CustomComparison>]
type ResolvedTypeIdentity =
    private
        {
            DefiningAssembly : AssemblyName
            Definition : ComparableTypeDefinitionHandle
        }

    member this.Assembly = this.DefiningAssembly
    member this.TypeDefinition = this.Definition

    override this.Equals (other : obj) : bool =
        match other with
        | :? ResolvedTypeIdentity as other ->
            this.Definition = other.Definition
            && this.DefiningAssembly.FullName = other.DefiningAssembly.FullName
        | _ -> false

    override this.GetHashCode () : int =
        hash (this.DefiningAssembly.FullName, this.Definition)

    interface System.IComparable<ResolvedTypeIdentity> with
        member this.CompareTo (other : ResolvedTypeIdentity) : int =
            compare
                (this.DefiningAssembly.FullName, this.Definition)
                (other.DefiningAssembly.FullName, other.Definition)

    interface System.IComparable with
        member this.CompareTo (other : obj) : int =
            match other with
            | :? ResolvedTypeIdentity as other -> (this :> System.IComparable<ResolvedTypeIdentity>).CompareTo other
            | _ -> failwith "invalid comparison"

[<RequireQualifiedAccess>]
module ResolvedTypeIdentity =
    let internal make (assemblyName : AssemblyName) (handle : ComparableTypeDefinitionHandle) : ResolvedTypeIdentity =
        {
            DefiningAssembly = assemblyName
            Definition = handle
        }

    let ofTypeDefinition (assemblyName : AssemblyName) (handle : TypeDefinitionHandle) : ResolvedTypeIdentity =
        make assemblyName (ComparableTypeDefinitionHandle.Make handle)
