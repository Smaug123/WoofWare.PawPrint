namespace WoofWare.PawPrint

type TypeHandleRegistry =
    private
        {
            TypeHandleToType : Map<ManagedHeapAddress, ConcreteTypeHandle>
            TypeToHandle : Map<ConcreteTypeHandle, ManagedHeapAddress>
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
        (allocState : 'allocState)
        (allocate : (string * CliType) list -> 'allocState -> ManagedHeapAddress * 'allocState)
        (def : ConcreteTypeHandle)
        (reg : TypeHandleRegistry)
        : ManagedHeapAddress * TypeHandleRegistry * 'allocState
        =
        match Map.tryFind def reg.TypeToHandle with
        | Some v -> v, reg, allocState
        | None ->

        // Here follows the class System.RuntimeType, which is an internal class type with a constructor
        // whose only purpose is to throw.
        let fields =
            [
                // for the GC, I think?
                "m_keepalive", CliType.ObjectRef None
                // TODO: this is actually a System.IntPtr https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/coreclr/nativeaot/Runtime.Base/src/System/Primitives.cs#L339
                "m_cache", CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))
                "m_handle", CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.TypeHandlePtr def))
                // This is the const -1, apparently?!
                // https://github.com/dotnet/runtime/blob/f0168ee80ba9aca18a7e7140b2bb436defda623c/src/coreclr/System.Private.CoreLib/src/System/RuntimeType.CoreCLR.cs#L2496
                "GenericParameterCountAny", CliType.Numeric (CliNumericType.Int32 -1)
            ]

        let alloc, state = allocate fields allocState

        let reg =
            {
                TypeHandleToType = reg.TypeHandleToType |> Map.add alloc def
                TypeToHandle = reg.TypeToHandle |> Map.add def alloc
            }

        alloc, reg, state
