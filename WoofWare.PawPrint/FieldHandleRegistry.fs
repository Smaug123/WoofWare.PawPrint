namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata

type FieldHandle =
    private
        {
            AssemblyFullName : string
            DeclaringType : ConcreteTypeHandle
            FieldHandle : ComparableFieldDefinitionHandle
        }

    member this.GetAssemblyFullName () : string = this.AssemblyFullName
    member this.GetDeclaringTypeHandle () : ConcreteTypeHandle = this.DeclaringType
    member this.GetFieldDefinitionHandle () : ComparableFieldDefinitionHandle = this.FieldHandle

type FieldHandleRegistry =
    private
        {
            FieldHandleToId : Map<FieldHandle, int64>
            FieldHandleIdToField : Map<int64, FieldHandle>
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
            FieldHandleIdToField = Map.empty
            NextHandle = 1L
        }

    /// Returns a (struct) System.RuntimeFieldHandle, with its contents (reference type) freshly allocated if necessary.
    let getOrAllocate
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (allConcreteTypes : AllConcreteTypes)
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
                Id = FieldId.named "m_ptr"
                Name = "m_ptr"
                Contents = CliType.ofManagedObject runtimeFieldInfoStub
                Offset = None
                Type = AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.RuntimeFieldInfoStub
            }
            |> List.singleton
            |> CliValueType.OfFields
                baseClassTypes
                allConcreteTypes
                (AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.RuntimeFieldHandle)
                Layout.Default
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
                Id = FieldId.named "m_handle"
                Name = "m_handle"
                Contents = CliType.RuntimePointer (CliRuntimePointer.FieldRegistryHandle newHandle)
                Offset = None // no struct layout was specified
                Type = AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.IntPtr
            }
            |> List.singleton
            |> CliValueType.OfFields
                baseClassTypes
                allConcreteTypes
                (AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.RuntimeFieldHandleInternal)
                Layout.Default
            |> CliType.ValueType

        // https://github.com/dotnet/runtime/blob/1d1bf92fcf43aa6981804dc53c5174445069c9e4/src/coreclr/System.Private.CoreLib/src/System/RuntimeHandles.cs#L1074
        let runtimeFieldInfoStub =
            let objType =
                AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.Object

            let intType =
                AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.Int32

            // LayoutKind.Sequential
            [
                // If we ever implement a GC, something should change here
                {
                    Id = FieldId.named "m_keepalive"
                    Name = "m_keepalive"
                    Contents = CliType.ObjectRef None
                    Offset = None
                    Type = objType
                }
                {
                    Id = FieldId.named "m_c"
                    Name = "m_c"
                    Contents = CliType.ObjectRef None
                    Offset = None
                    Type = objType
                }
                {
                    Id = FieldId.named "m_d"
                    Name = "m_d"
                    Contents = CliType.ObjectRef None
                    Offset = None
                    Type = objType
                }
                {
                    Id = FieldId.named "m_b"
                    Name = "m_b"
                    Contents = CliType.Numeric (CliNumericType.Int32 0)
                    Offset = None
                    Type = intType
                }
                {
                    Id = FieldId.named "m_e"
                    Name = "m_e"
                    Contents = CliType.ObjectRef None
                    Offset = None
                    Type = objType
                }
                // RuntimeFieldHandleInternal: https://github.com/dotnet/runtime/blob/1d1bf92fcf43aa6981804dc53c5174445069c9e4/src/coreclr/System.Private.CoreLib/src/System/RuntimeHandles.cs#L1048
                {
                    Id = FieldId.named "m_fieldHandle"
                    Name = "m_fieldHandle"
                    Contents = runtimeFieldHandleInternal
                    Offset = None
                    Type =
                        AllConcreteTypes.getRequiredNonGenericHandle
                            allConcreteTypes
                            baseClassTypes.RuntimeFieldHandleInternal
                }
            ]
            |> CliValueType.OfFields
                baseClassTypes
                allConcreteTypes
                (AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.RuntimeFieldInfoStub)
                Layout.Default // explicitly sequential but no custom packing size

        let alloc, state = allocate runtimeFieldInfoStub allocState

        let reg =
            {
                FieldHandleToField = reg.FieldHandleToField |> Map.add alloc handle
                FieldToHandle = reg.FieldToHandle |> Map.add handle alloc
                FieldHandleToId = reg.FieldHandleToId |> Map.add handle newHandle
                FieldHandleIdToField = reg.FieldHandleIdToField |> Map.add newHandle handle
                NextHandle = reg.NextHandle + 1L
            }

        runtimeFieldHandle alloc, reg, state

    /// Given the ManagedHeapAddress of a RuntimeFieldInfoStub, resolve it to the FieldHandle.
    let resolveFieldFromAddress (addr : ManagedHeapAddress) (reg : FieldHandleRegistry) : FieldHandle option =
        Map.tryFind addr reg.FieldHandleToField

    /// Given the integer payload of a RuntimeFieldHandleInternal, resolve it to the FieldHandle.
    let resolveFieldFromId (id : int64) (reg : FieldHandleRegistry) : FieldHandle option =
        Map.tryFind id reg.FieldHandleIdToField
