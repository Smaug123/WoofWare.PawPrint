namespace WoofWare.PawPrint

open System.Collections.Immutable

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
        (allConcreteTypes : AllConcreteTypes)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (allocState : 'allocState)
        (allocate : CliValueType -> 'allocState -> ManagedHeapAddress * 'allocState)
        (def : ConcreteTypeHandle)
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
        let fields =
            [
                // for the GC, I think?
                {
                    Name = "m_keepalive"
                    Contents = CliType.ObjectRef None
                    Offset = None
                    Type =
                        AllConcreteTypes.findExistingConcreteType
                            allConcreteTypes
                            (corelib.Object.Assembly,
                             corelib.Object.Namespace,
                             corelib.Object.Name,
                             ImmutableArray.Empty)
                        |> Option.get
                }
                {
                    Name = "m_cache"
                    Contents = CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))
                    Offset = None
                    Type =
                        AllConcreteTypes.findExistingConcreteType
                            allConcreteTypes
                            (corelib.IntPtr.Assembly,
                             corelib.IntPtr.Namespace,
                             corelib.IntPtr.Name,
                             ImmutableArray.Empty)
                        |> Option.get
                }
                {
                    Name = "m_handle"
                    Contents = CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.TypeHandlePtr def))
                    Offset = None
                    Type =
                        AllConcreteTypes.findExistingConcreteType
                            allConcreteTypes
                            (corelib.IntPtr.Assembly,
                             corelib.IntPtr.Namespace,
                             corelib.IntPtr.Name,
                             ImmutableArray.Empty)
                        |> Option.get
                }
                // This is the const -1, apparently?!
                // https://github.com/dotnet/runtime/blob/f0168ee80ba9aca18a7e7140b2bb436defda623c/src/coreclr/System.Private.CoreLib/src/System/RuntimeType.CoreCLR.cs#L2496
                {
                    Name = "GenericParameterCountAny"
                    Contents = CliType.Numeric (CliNumericType.Int32 -1)
                    Offset = None
                    Type =
                        AllConcreteTypes.findExistingConcreteType
                            allConcreteTypes
                            (corelib.Int32.Assembly, corelib.Int32.Namespace, corelib.Int32.Name, ImmutableArray.Empty)
                        |> Option.get
                }
            ]
            |> CliValueType.OfFields Layout.Default

        let alloc, state = allocate fields allocState

        let reg =
            {
                TypeHandleToType = reg.TypeHandleToType |> Map.add alloc def
                TypeToHandle = reg.TypeToHandle |> Map.add def alloc
            }

        alloc, reg, state
