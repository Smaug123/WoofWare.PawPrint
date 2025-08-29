namespace WoofWare.PawPrint

open System.Reflection
open System.Reflection.Metadata

type FieldHandle =
    private
        {
            AssemblyFullName : string
            DeclaringType : ConcreteTypeHandle
            FieldHandle : ComparableFieldDefinitionHandle
        }

type FieldHandleRegistry =
    private
        {
            FieldHandleToId : Map<FieldHandle, int64>
            FieldHandleToField : Map<ManagedHeapAddress, FieldHandle>
            FieldToHandle : Map<FieldHandle, ManagedHeapAddress>
            NextHandle : int64
        }

[<RequireQualifiedAccess>]
module FieldHandleRegistry =
    let empty () =
        {
            FieldHandleToField = Map.empty
            FieldToHandle = Map.empty
            FieldHandleToId = Map.empty
            NextHandle = 1L
        }

    /// Returns a (struct) System.RuntimeFieldHandle, with its contents (reference type) freshly allocated if necessary.
    let getOrAllocate
        (baseClassTypes : BaseClassTypes<'corelib>)
        (allocState : 'allocState)
        (allocate : CliValueType -> 'allocState -> ManagedHeapAddress * 'allocState)
        (declaringAssy : AssemblyName)
        (declaringType : ConcreteTypeHandle)
        (handle : FieldDefinitionHandle)
        (reg : FieldHandleRegistry)
        : CliType * FieldHandleRegistry * 'allocState
        =

        let runtimeFieldHandle (runtimeFieldInfoStub : ManagedHeapAddress) =
            // RuntimeFieldHandle is a struct; it contains one field, an IRuntimeFieldInfo
            // https://github.com/dotnet/runtime/blob/1d1bf92fcf43aa6981804dc53c5174445069c9e4/src/coreclr/System.Private.CoreLib/src/System/RuntimeHandles.cs#L1048
            // In practice we expect to use RuntimeFieldInfoStub for that IRuntimeFieldInfo:
            // https://github.com/dotnet/runtime/blob/1d1bf92fcf43aa6981804dc53c5174445069c9e4/src/coreclr/System.Private.CoreLib/src/System/RuntimeHandles.cs#L1157
            let runtimeFieldHandleType = baseClassTypes.RuntimeFieldHandle
            let field = runtimeFieldHandleType.Fields |> List.exactlyOne

            if field.Name <> "m_ptr" then
                failwith $"unexpected field name %s{field.Name} for BCL type RuntimeFieldHandle"

            {
                Name = "m_ptr"
                Contents = CliType.ofManagedObject runtimeFieldInfoStub
                Offset = None
            }
            |> List.singleton
            |> CliValueType.OfFields Layout.Default
            |> CliType.ValueType

        let handle =
            {
                AssemblyFullName = declaringAssy.FullName
                FieldHandle = ComparableFieldDefinitionHandle.Make handle
                DeclaringType = declaringType
            }

        match Map.tryFind handle reg.FieldToHandle with
        | Some v -> runtimeFieldHandle v, reg, allocState
        | None ->

        let newHandle = reg.NextHandle

        let runtimeFieldHandleInternal =
            let field = baseClassTypes.RuntimeFieldHandleInternal.Fields |> List.exactlyOne

            if field.Name <> "m_handle" then
                failwith $"unexpected field name %s{field.Name} for BCL type RuntimeFieldHandleInternal"

            match field.Signature with
            | TypeDefn.PrimitiveType PrimitiveType.IntPtr -> ()
            | s -> failwith $"bad sig: {s}"

            // https://github.com/dotnet/runtime/blob/2b21c73fa2c32fa0195e4a411a435dda185efd08/src/coreclr/System.Private.CoreLib/src/System/RuntimeHandles.cs#L1380
            {
                Name = "m_handle"
                Contents = CliType.RuntimePointer (CliRuntimePointer.FieldRegistryHandle newHandle)
                Offset = None // no struct layout was specified
            }
            |> List.singleton
            |> CliValueType.OfFields Layout.Default
            |> CliType.ValueType

        // https://github.com/dotnet/runtime/blob/1d1bf92fcf43aa6981804dc53c5174445069c9e4/src/coreclr/System.Private.CoreLib/src/System/RuntimeHandles.cs#L1074
        let runtimeFieldInfoStub =
            // LayoutKind.Sequential
            [
                // If we ever implement a GC, something should change here
                {
                    Name = "m_keepalive"
                    Contents = CliType.ObjectRef None
                    Offset = None
                }
                {
                    Name = "m_c"
                    Contents = CliType.ObjectRef None
                    Offset = None
                }
                {
                    Name = "m_d"
                    Contents = CliType.ObjectRef None
                    Offset = None
                }
                {
                    Name = "m_b"
                    Contents = CliType.Numeric (CliNumericType.Int32 0)
                    Offset = None
                }
                {
                    Name = "m_e"
                    Contents = CliType.ObjectRef None
                    Offset = None
                }
                // RuntimeFieldHandleInternal: https://github.com/dotnet/runtime/blob/1d1bf92fcf43aa6981804dc53c5174445069c9e4/src/coreclr/System.Private.CoreLib/src/System/RuntimeHandles.cs#L1048
                {
                    Name = "m_fieldHandle"
                    Contents = runtimeFieldHandleInternal
                    Offset = None
                }
            ]
            |> CliValueType.OfFields Layout.Default // explicitly sequential but no custom packing size

        let alloc, state = allocate runtimeFieldInfoStub allocState

        let reg =
            {
                FieldHandleToField = reg.FieldHandleToField |> Map.add alloc handle
                FieldToHandle = reg.FieldToHandle |> Map.add handle alloc
                FieldHandleToId = reg.FieldHandleToId |> Map.add handle newHandle
                NextHandle = reg.NextHandle + 1L
            }

        runtimeFieldHandle alloc, reg, state
