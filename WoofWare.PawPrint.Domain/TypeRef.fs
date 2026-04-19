namespace WoofWare.PawPrint

open System
open System.Reflection.Metadata
open Microsoft.FSharp.Core

[<CustomComparison>]
[<CustomEquality>]
type TypeRefResolutionScope =
    | Assembly of AssemblyReferenceHandle
    | ModuleDef of ModuleDefinitionHandle
    | ModuleRef of ModuleReferenceHandle
    | TypeRef of TypeReferenceHandle

    override this.Equals (other : obj) : bool =
        let other =
            match other with
            | :? TypeRefResolutionScope as other -> other
            | _ -> failwith "should never compare with non-TypeRefResolutionScope"

        match this, other with
        | TypeRefResolutionScope.Assembly a1, TypeRefResolutionScope.Assembly a2 -> a1 = a2
        | TypeRefResolutionScope.Assembly _, _ -> false
        | TypeRefResolutionScope.ModuleDef _, TypeRefResolutionScope.ModuleDef _ -> true
        | TypeRefResolutionScope.ModuleDef _, _ -> false
        | TypeRefResolutionScope.ModuleRef m1, TypeRefResolutionScope.ModuleRef m2 -> m1 = m2
        | TypeRefResolutionScope.ModuleRef _, _ -> false
        | TypeRefResolutionScope.TypeRef t1, TypeRefResolutionScope.TypeRef t2 -> t1 = t2
        | TypeRefResolutionScope.TypeRef _, _ -> false

    override this.GetHashCode () : int =
        match this with
        | TypeRefResolutionScope.Assembly h -> hash (1, h)
        | TypeRefResolutionScope.ModuleDef _ -> hash 2
        | TypeRefResolutionScope.ModuleRef h -> hash (3, h)
        | TypeRefResolutionScope.TypeRef h -> hash (4, h)

    interface IComparable<TypeRefResolutionScope> with
        member this.CompareTo other =
            match this, other with
            | TypeRefResolutionScope.Assembly h1, TypeRefResolutionScope.Assembly h2 ->
                // this happens to get the underlying int
                h1.GetHashCode().CompareTo (h2.GetHashCode ())
            | TypeRefResolutionScope.Assembly _, _ -> -1
            | _, TypeRefResolutionScope.Assembly _ -> 1
            | TypeRefResolutionScope.ModuleDef _, TypeRefResolutionScope.ModuleDef _ -> 0
            | TypeRefResolutionScope.ModuleDef _, _ -> -1
            | _, TypeRefResolutionScope.ModuleDef _ -> 1
            | TypeRefResolutionScope.ModuleRef m1, TypeRefResolutionScope.ModuleRef m2 ->
                m1.GetHashCode().CompareTo (m2.GetHashCode ())
            | TypeRefResolutionScope.ModuleRef _, _ -> -1
            | _, TypeRefResolutionScope.ModuleRef _ -> 1
            | TypeRefResolutionScope.TypeRef t1, TypeRefResolutionScope.TypeRef t2 ->
                t1.GetHashCode().CompareTo (t2.GetHashCode ())

    interface IComparable with
        member this.CompareTo (other : obj) : int =
            let other =
                match other with
                | :? TypeRefResolutionScope as other -> other
                | _ -> failwith "unexpectedly comparing TypeRefResolutionScope with something else"

            (this :> IComparable<TypeRefResolutionScope>).CompareTo other

/// <summary>
/// Represents a type reference in a .NET assembly metadata.
/// This corresponds to a TypeReferenceHandle in System.Reflection.Metadata.
/// </summary>
type TypeRef =
    {
        /// <summary>The simple name of the referenced type (without namespace).</summary>
        Name : string

        /// <summary>The namespace of the referenced type, or empty string for nested types.</summary>
        Namespace : string

        /// <summary>
        /// The scope of the type reference: where to find the type.
        /// </summary>
        ResolutionScope : TypeRefResolutionScope
    }

[<RequireQualifiedAccess>]
module TypeRef =
    let make (metadataReader : MetadataReader) (ty : TypeReferenceHandle) : TypeRef =
        let typeRef = metadataReader.GetTypeReference ty
        let prettyName = metadataReader.GetString typeRef.Name
        let prettyNamespace = metadataReader.GetString typeRef.Namespace

        let resolutionScope =
            match MetadataToken.ofEntityHandle typeRef.ResolutionScope with
            | MetadataToken.AssemblyReference ref -> TypeRefResolutionScope.Assembly ref
            | MetadataToken.ModuleDefinition ref -> TypeRefResolutionScope.ModuleDef ref
            | MetadataToken.ModuleReference ref -> TypeRefResolutionScope.ModuleRef ref
            | MetadataToken.TypeReference ref -> TypeRefResolutionScope.TypeRef ref
            | handle -> failwith $"Unexpected TypeRef resolution scope: {handle}"

        {
            Name = prettyName
            Namespace = prettyNamespace
            ResolutionScope = resolutionScope
        }
