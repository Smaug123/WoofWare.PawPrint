namespace WoofWare.PawPrint

open System.Collections.Immutable

type TypeHandleRegistry =
    private
        {
            TypeHandleToType : Map<ManagedHeapAddress, RuntimeTypeHandleTarget>
            TypeToHandle : Map<RuntimeTypeHandleTarget, ManagedHeapAddress>
        }

[<RequireQualifiedAccess>]
module TypeHandleRegistry =
    let empty () =
        {
            TypeHandleToType = Map.empty
            TypeToHandle = Map.empty
        }

    /// Returns an allocated System.RuntimeType as well.
    let getOrAllocate
        (allConcreteTypes : AllConcreteTypes)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (allocState : 'allocState)
        (allocate : CliValueType -> 'allocState -> ManagedHeapAddress * 'allocState)
        (def : RuntimeTypeHandleTarget)
        (reg : TypeHandleRegistry)
        : ManagedHeapAddress * TypeHandleRegistry * 'allocState
        =
        match Map.tryFind def reg.TypeToHandle with
        | Some v -> v, reg, allocState
        | None ->

        // Here follows the class System.RuntimeType, which is an internal class type with a constructor
        // whose only purpose is to throw.
        // https://github.com/dotnet/runtime/blob/2b21c73fa2c32fa0195e4a411a435dda185efd08/src/libraries/System.Private.CoreLib/src/System/RuntimeType.cs#L14
        // and https://github.com/dotnet/runtime/blob/f0168ee80ba9aca18a7e7140b2bb436defda623c/src/coreclr/System.Private.CoreLib/src/System/RuntimeType.CoreCLR.cs#L44
        let runtimeTypeHandle =
            AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes corelib.RuntimeType

        let runtimeTypeField (name : string) (contents : CliType) (fieldTypeHandle : ConcreteTypeHandle) : CliField =
            let field = FieldIdentity.requiredOwnInstanceField corelib.RuntimeType name

            FieldIdentity.cliField runtimeTypeHandle field contents fieldTypeHandle

        let fields =
            [
                // for the GC, I think?
                runtimeTypeField
                    "m_keepalive"
                    (CliType.ObjectRef None)
                    (AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes corelib.Object)
                runtimeTypeField
                    "m_cache"
                    (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)))
                    (AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes corelib.IntPtr)
                runtimeTypeField
                    "m_handle"
                    (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.TypeHandlePtr def)))
                    (AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes corelib.IntPtr)
            ]
            |> CliValueType.OfFields corelib allConcreteTypes runtimeTypeHandle Layout.Default

        let alloc, state = allocate fields allocState

        let reg =
            {
                TypeHandleToType = reg.TypeHandleToType |> Map.add alloc def
                TypeToHandle = reg.TypeToHandle |> Map.add def alloc
            }

        alloc, reg, state
