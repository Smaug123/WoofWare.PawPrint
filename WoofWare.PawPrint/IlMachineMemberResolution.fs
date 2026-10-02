namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Microsoft.Extensions.Logging

/// What an instruction does with the member a MemberRef names, which decides whether CoreCLR lets
/// the reference's parent name a generic definition without an instantiation.
[<RequireQualifiedAccess>]
type MemberReferenceUse =
    /// `ldtoken`, which `CEEInfo::resolveToken` lets name a member of a generic definition's typical
    /// instantiation (`PermitUninstDefOrRef`).
    | Ldtoken
    /// Any other instruction, named for diagnostics, for which `resolveToken` refuses such a parent
    /// (`FailIfUninstDefOrRef`).
    | Other of opName : string

[<RequireQualifiedAccess>]
module IlMachineMemberResolution =
    /// <summary>
    /// Refuse to execute a member that MemberRef row <paramref name="m" /> of
    /// <paramref name="assy" /> names through a generic definition with no instantiation: a
    /// TypeReference or TypeDefinition parent naming a generic type, whichever type declares the
    /// member. CoreCLR's <c>CEEInfo::resolveToken</c> loads such a parent with
    /// <c>FailIfUninstDefOrRef</c> for every instruction but <c>ldtoken</c>, so it throws
    /// <c>TypeLoadException</c>; that is not modelled.
    /// </summary>
    /// <remarks>
    /// The row must already have been resolved, so that the parent's assembly is loaded. Without
    /// this, the caller would instantiate the definition from the executing frame's own generic
    /// arguments, which is a type the reference does not name.
    /// </remarks>
    let private refuseUninstantiatedParent
        (opName : string)
        (assy : DumpedAssembly)
        (m : MemberReferenceHandle)
        (state : IlMachineState)
        : unit
        =
        let parent =
            match assy.Members.[m].Parent with
            | MetadataToken.TypeDefinition handle -> Some assy.TypeDefs.[handle]
            | MetadataToken.TypeReference handle ->
                match
                    LoadedTypeResolution.resolveTypeRef
                        state.TypeSystem._LoadedAssemblies
                        assy
                        ImmutableArray.Empty
                        assy.TypeRefs.[handle]
                with
                | TypeResolutionResult.Resolved (definedIn, _, resolved) ->
                    Some definedIn.TypeDefs.[resolved.TypeDefHandle]
                | other ->
                    failwith
                        $"BUG: %s{opName}: the TypeReference parent of a MemberRef in %s{assy.DefinitionFullName} resolved when the row was resolved, but not now: %O{other}"
            | _ -> None

        match parent with
        | Some definition when not definition.Generics.IsEmpty ->
            failwith
                $"TODO: raise TypeLoadException: %s{opName} names a member through generic type definition %s{definition.Namespace}.%s{definition.Name} with no instantiation, which CoreCLR refuses for every instruction but ldtoken; that is not modelled"
        | _ -> ()

    /// `MemberReferenceInstantiation.resolveMemberWithGenerics` against the machine's type system.
    let resolveMemberWithGenerics
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (assy : DumpedAssembly)
        (typeGenerics : ImmutableArray<TypeDefn>)
        (methodGenerics : ImmutableArray<TypeDefn>)
        (m : MemberReferenceHandle)
        (state : IlMachineState)
        : IlMachineState *
          AssemblyName *
          Choice<
              WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>,
              WoofWare.PawPrint.FieldInfo<TypeDefn, TypeDefn>
           > *
          TypeDefn ImmutableArray
        =
        // TODO: do we need to initialise the parent class here?
        let typeSystem, declaringAssembly, member', targetTypeGenerics =
            MemberReferenceInstantiation.resolveMemberWithGenerics
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                assy
                typeGenerics
                methodGenerics
                m
                state.TypeSystem

        state.WithTypeSystem typeSystem, declaringAssembly, member', targetTypeGenerics

    /// `MemberReferenceInstantiation.resolveMember` against the machine's type system, in the
    /// generic context of the method `currentThread` is executing. For any use but `ldtoken`,
    /// refuses a row whose parent names a generic definition without an instantiation, as CoreCLR
    /// refuses it.
    let resolveMember
        (use' : MemberReferenceUse)
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (currentThread : ThreadId)
        (assy : DumpedAssembly)
        (m : MemberReferenceHandle)
        (state : IlMachineState)
        : IlMachineState *
          AssemblyName *
          Choice<
              WoofWare.PawPrint.MethodInfo<TypeDefn, GenericParamFromMetadata, TypeDefn>,
              WoofWare.PawPrint.FieldInfo<TypeDefn, TypeDefn>
           > *
          TypeDefn ImmutableArray
        =
        let executing = state.ThreadState.[currentThread].MethodState.ExecutingMethod

        let typeSystem, declaringAssembly, member', targetTypeGenerics =
            MemberReferenceInstantiation.resolveMember
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                assy
                executing.DeclaringTypeGenerics
                executing.Generics
                m
                state.TypeSystem

        let state = state.WithTypeSystem typeSystem

        match use' with
        | MemberReferenceUse.Ldtoken -> ()
        | MemberReferenceUse.Other opName -> refuseUninstantiatedParent opName assy m state

        state, declaringAssembly, member', targetTypeGenerics
