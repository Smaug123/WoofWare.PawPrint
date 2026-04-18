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
            DefiningAssemblyFullName : string
            Definition : ComparableTypeDefinitionHandle
        }

    member this.AssemblyFullName = this.DefiningAssemblyFullName
    member this.Assembly = AssemblyName this.DefiningAssemblyFullName
    member this.TypeDefinition = this.Definition

    override this.Equals (other : obj) : bool =
        match other with
        | :? ResolvedTypeIdentity as other ->
            this.Definition = other.Definition
            && this.DefiningAssemblyFullName = other.DefiningAssemblyFullName
        | _ -> false

    override this.GetHashCode () : int =
        hash (this.DefiningAssemblyFullName, this.Definition)

    interface System.IComparable<ResolvedTypeIdentity> with
        member this.CompareTo (other : ResolvedTypeIdentity) : int =
            compare (this.DefiningAssemblyFullName, this.Definition) (other.DefiningAssemblyFullName, other.Definition)

    interface System.IComparable with
        member this.CompareTo (other : obj) : int =
            match other with
            | :? ResolvedTypeIdentity as other -> (this :> System.IComparable<ResolvedTypeIdentity>).CompareTo other
            | _ -> failwith "invalid comparison"

[<RequireQualifiedAccess>]
module ResolvedTypeIdentity =
    let internal make (assemblyFullName : string) (handle : ComparableTypeDefinitionHandle) : ResolvedTypeIdentity =
        {
            DefiningAssemblyFullName = assemblyFullName
            Definition = handle
        }

    let ofTypeDefinition (assemblyName : AssemblyName) (handle : TypeDefinitionHandle) : ResolvedTypeIdentity =
        make assemblyName.FullName (ComparableTypeDefinitionHandle.Make handle)
