namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging

/// `ConcreteInterfaceDispatch` against the machine's type system.
[<RequireQualifiedAccess>]
module InterfaceDispatch =

    /// `ConcreteInterfaceDispatch.resolveImplementedInterface` against the machine's type system.
    let internal resolveImplementedInterface
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (ownerTy : ConcreteType<ConcreteTypeHandle>)
        (impl : WoofWare.PawPrint.InterfaceImplementation)
        (state : IlMachineState)
        : IlMachineState *
          ConcreteTypeHandle *
          ConcreteType<ConcreteTypeHandle> *
          TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        let typeSystem, implHandle, implTy, typeInfo =
            ConcreteInterfaceDispatch.resolveImplementedInterface
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                ownerTy
                impl
                state.TypeSystem

        state.WithTypeSystem typeSystem, implHandle, implTy, typeInfo

    /// `ConcreteInterfaceDispatch.interfaceMapOf` against the machine's type system.
    let internal interfaceMapOf
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (typeHandle : ConcreteTypeHandle)
        : IlMachineState * ConcreteInterfaceDispatch.InterfaceMapEntry list
        =
        let typeSystem, result =
            ConcreteInterfaceDispatch.interfaceMapOf
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                operation
                state.TypeSystem
                typeHandle

        state.WithTypeSystem typeSystem, result

    /// `ConcreteInterfaceDispatch.ownDispatchMapOf` against the machine's type system.
    let ownDispatchMapOf
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (typeHandle : ConcreteTypeHandle)
        : IlMachineState * InterfaceDispatchMap
        =
        let typeSystem, result =
            ConcreteInterfaceDispatch.ownDispatchMapOf
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                operation
                state.TypeSystem
                typeHandle

        state.WithTypeSystem typeSystem, result

    /// `ConcreteInterfaceDispatch.tryFindImplementationSlot` against the machine's type system.
    let tryFindImplementationSlot
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (receiver : ConcreteTypeHandle)
        (walkBaseTypes : bool)
        (target : ConcreteTypeHandle)
        (interfaceMethod : SlotIdentity)
        : IlMachineState * int option
        =
        let typeSystem, result =
            ConcreteInterfaceDispatch.tryFindImplementationSlot
                loggerFactory
                state.DotnetRuntimeDirs
                baseClassTypes
                operation
                state.TypeSystem
                receiver
                walkBaseTypes
                target
                interfaceMethod

        state.WithTypeSystem typeSystem, result
