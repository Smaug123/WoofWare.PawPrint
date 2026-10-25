namespace WoofWare.PawPrint

open System.Reflection.Metadata

/// Where an interpreter caller that does not yet pass on a failure to bind a token meets one: the
/// run stops, with the failure described as resolving that token describes it. Each use marks a
/// caller still to be converted to return the failure.
[<RequireQualifiedAccess>]
module internal TokenBindingStop =

    /// What type reference <paramref name="target" />, made in <paramref name="assy" />, bound; a
    /// failure stops the run.
    let typeReference (target : TypeRef) (assy : DumpedAssembly) (result : Result<'a, TypeBindingFailure>) : 'a =
        match result with
        | Ok bound -> bound
        | Error failure -> failwith (TypeResolution.describeReferenceFailure target assy failure)

    /// What type token <paramref name="token" /> of <paramref name="assy" /> bound; a failure stops
    /// the run.
    let typeToken (assy : DumpedAssembly) (token : MetadataToken) (result : Result<'a, TypeBindingFailure>) : 'a =
        match token with
        | MetadataToken.TypeReference handle -> typeReference assy.TypeRefs.[handle] assy result
        | _ -> BindingStop.bound result

    /// What the base type a type definition of <paramref name="assy" /> names bound; a failure stops
    /// the run.
    let baseType (assy : DumpedAssembly) (baseType : BaseTypeInfo) (result : Result<'a, TypeBindingFailure>) : 'a =
        match baseType with
        | BaseTypeInfo.TypeRef handle -> typeReference assy.TypeRefs.[handle] assy result
        | BaseTypeInfo.TypeDef _
        | BaseTypeInfo.TypeSpec _ -> BindingStop.bound result

    /// What MemberRef row <paramref name="m" /> of <paramref name="assy" /> bound; a failure stops
    /// the run.
    let memberReference
        (assy : DumpedAssembly)
        (m : MemberReferenceHandle)
        (result : Result<'a, MemberBindingFailure>)
        : 'a
        =
        match result with
        | Ok bound -> bound
        | Error failure -> failwith (MemberReferenceInstantiation.describeFailure assy m failure)
