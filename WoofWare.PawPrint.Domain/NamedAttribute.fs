namespace WoofWare.PawPrint

open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335

/// The attributes applied to an assembly or a method, found by the name of their type as CoreCLR
/// finds them.
[<RequireQualifiedAccess>]
module NamedAttribute =

    /// The raw token of the single-row AssemblyDefinition table.
    [<Literal>]
    let private AssemblyDefinitionToken = 0x20000001

    /// `CompareCustomAttribute` (`md/inc/metamodel.h`) joins a type's namespace and name with a dot, unless the namespace
    /// is empty, when the name alone must be the whole string.
    let private isNamed (fullName : string) (ns : string, name : string) : bool =
        if ns = "" then
            name = fullName
        else
            ns + "." + name = fullName

    /// The namespace and name of the type declaring an attribute's constructor, found as CoreCLR's
    /// `CommonGetNameOfCustomAttribute` (`md/inc/metamodel.h`) finds them: a MethodDef's declaring
    /// type, a MemberRef's parent, and for a TypeSpec parent the class at its core once pointers,
    /// byrefs, pinning and a generic instantiation are stripped. `None` for a TypeSpec with no
    /// class there, which names nothing and so matches nothing.
    let rec private constructorTypeName
        (assembly : DumpedAssembly)
        (constructor : MetadataToken)
        : (string * string) option
        =
        match constructor with
        | MetadataToken.MemberReference handle -> constructorTypeName assembly assembly.Members.[handle].Parent
        | MetadataToken.MethodDef handle ->
            let declaring = assembly.Methods.[handle].RequiredDeclaringType
            Some (declaring.Namespace, declaring.Name)
        | MetadataToken.TypeReference handle ->
            let typeRef = assembly.TypeRefs.[handle]
            Some (typeRef.Namespace, typeRef.Name)
        | MetadataToken.TypeDefinition handle ->
            let typeDef = assembly.TypeDefs.[handle]
            Some (typeDef.Namespace, typeDef.Name)
        | MetadataToken.TypeSpecification handle ->
            // `GetTypeDefRefTokenInTypeSpec` skips PTR, BYREF, the element types with the 0x40 bit
            // (PINNED among them) and GENERICINST, and stops at anything else; a custom modifier
            // is not skipped.
            let rec core (ty : TypeDefn) : (string * string) option =
                match ty with
                | TypeDefn.Pointer inner
                | TypeDefn.Byref inner
                | TypeDefn.Pinned inner -> core inner
                | TypeDefn.GenericInstantiation (generic, _) -> core generic
                | TypeDefn.FromReference (typeRef, _) -> Some (typeRef.Namespace, typeRef.Name)
                | TypeDefn.FromDefinition (identity, _) ->
                    let typeDef = assembly.TypeDefs.[identity.TypeDefinition.Get]
                    Some (typeDef.Namespace, typeDef.Name)
                | _ -> None

            core assembly.TypeSpecs.[handle].Signature
        | MetadataToken.ModuleReference _ ->
            // `CommonGetNameOfCustomAttribute` answers COR_E_BADIMAGEFORMAT, which ends the by-name
            // search with an error. CoreCLR searches an assembly's own attributes while loading
            // it, so on one of those it never loads; what each other search makes of the error is
            // not modelled.
            failwith
                $"A custom attribute in %s{assembly.DefinitionFullName} has its constructor on a module reference, which CoreCLR's search of attributes by name rejects"
        | other -> failwith $"A custom attribute in %s{assembly.DefinitionFullName} has constructor %O{other}"

    /// The attributes in `assembly` applied to the entity with token `parent` whose type is named
    /// `fullName`, in metadata order, as CoreCLR's `GetCustomAttributeByName` searches them.
    let private onParent
        (fullName : string)
        (assembly : DumpedAssembly)
        (parent : int)
        : WoofWare.PawPrint.CustomAttribute list
        =
        match assembly.CustomAttributesByParentToken.TryGetValue parent with
        | false, _ -> []
        | true, tokens ->
            [
                for token in tokens do
                    let attribute =
                        match MetadataToken.ofInt token with
                        | MetadataToken.CustomAttribute handle -> assembly.Attributes.[handle]
                        | other ->
                            failwith $"The custom attributes of %s{assembly.DefinitionFullName} include %O{other}"

                    if
                        constructorTypeName assembly attribute.Constructor
                        |> Option.exists (isNamed fullName)
                    then
                        yield attribute
            ]

    /// The attributes of `assembly` whose type is named `fullName` (a namespace and a name joined
    /// with a dot), in metadata order, as CoreCLR's `GetCustomAttributeByName` searches them:
    /// however that name is split between namespace and name, and whatever assembly the type is
    /// scoped to.
    let ofAssembly (fullName : string) (assembly : DumpedAssembly) : WoofWare.PawPrint.CustomAttribute list =
        onParent fullName assembly AssemblyDefinitionToken

    /// The attributes of `method`, a method of `assembly`, whose type is named `fullName`, searched
    /// as `ofAssembly` searches an assembly's.
    let ofMethod
        (fullName : string)
        (assembly : DumpedAssembly)
        (method : MethodDefinitionHandle)
        : WoofWare.PawPrint.CustomAttribute list
        =
        onParent fullName assembly (MetadataTokens.GetToken (MethodDefinitionHandle.op_Implicit method : EntityHandle))
