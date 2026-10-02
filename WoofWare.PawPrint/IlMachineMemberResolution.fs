namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module IlMachineMemberResolution =
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
    /// generic context of the method `currentThread` is executing.
    let resolveMember
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

        state.WithTypeSystem typeSystem, declaringAssembly, member', targetTypeGenerics
