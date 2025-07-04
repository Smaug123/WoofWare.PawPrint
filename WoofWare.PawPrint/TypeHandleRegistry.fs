namespace WoofWare.PawPrint

open WoofWare.PawPrint.Domain

type TypeHandleRegistry =
    private
        {
            TypeHandleToType : Map<int, ConcreteTypeHandle>
            TypeToHandle : Map<ConcreteTypeHandle, int * ManagedHeapAddress>
            NextHandle : int
        }

[<RequireQualifiedAccess>]
module TypeHandleRegistry =
    let empty () =
        {
            TypeHandleToType = Map.empty
            TypeToHandle = Map.empty
            NextHandle = 1
        }

    /// Returns an allocated System.RuntimeType as well.
    let getOrAllocate
        (allocState : 'allocState)
        (allocate : (string * CliType) list -> 'allocState -> ManagedHeapAddress * 'allocState)
        (def : ConcreteTypeHandle)
        (reg : TypeHandleRegistry)
        : (int * ManagedHeapAddress) * TypeHandleRegistry * 'allocState
        =
        match Map.tryFind def reg.TypeToHandle with
        | Some v -> v, reg, allocState
        | None ->

        let handle = reg.NextHandle

        // Here follows the class System.RuntimeType, which is an internal class type with a constructor
        // whose only purpose is to throw.
        let fields =
            [
                // for the GC, I think?
                "m_keepalive", CliType.ObjectRef None
                // TODO: this is actually a System.IntPtr https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/coreclr/nativeaot/Runtime.Base/src/System/Primitives.cs#L339
                "m_cache", CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))
                "m_handle", CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.TypeHandlePtr handle))
                // This is the const -1, apparently?!
                // https://github.com/dotnet/runtime/blob/f0168ee80ba9aca18a7e7140b2bb436defda623c/src/coreclr/System.Private.CoreLib/src/System/RuntimeType.CoreCLR.cs#L2496
                "GenericParameterCountAny", CliType.Numeric (CliNumericType.Int32 -1)
            ]

        let alloc, state = allocate fields allocState

        let reg =
            {
                NextHandle = handle + 1
                TypeHandleToType = reg.TypeHandleToType |> Map.add handle def
                TypeToHandle = reg.TypeToHandle |> Map.add def (handle, alloc)
            }

        (handle, alloc), reg, state
