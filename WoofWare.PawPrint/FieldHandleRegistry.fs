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
        (allocate : (string * CliType) list -> 'allocState -> ManagedHeapAddress * 'allocState)
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
                Fields = [ "m_ptr", CliType.ofManagedObject runtimeFieldInfoStub ]
            }
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

            {
                Fields = [ "m_handle", CliType.RuntimePointer (CliRuntimePointer.Unmanaged newHandle) ]
            }
            |> CliType.ValueType

        let runtimeFieldInfoStub =
            [
                // If we ever implement a GC, something should change here
                "m_keepalive", CliType.ObjectRef None
                "m_c", CliType.ObjectRef None
                "m_d", CliType.ObjectRef None
                "m_b", CliType.Numeric (CliNumericType.Int32 0)
                "m_e", CliType.ObjectRef None
                // RuntimeFieldHandleInternal: https://github.com/dotnet/runtime/blob/1d1bf92fcf43aa6981804dc53c5174445069c9e4/src/coreclr/System.Private.CoreLib/src/System/RuntimeHandles.cs#L1048
                "m_fieldHandle", runtimeFieldHandleInternal
            ]

        let alloc, state = allocate runtimeFieldInfoStub allocState

        let reg =
            {
                FieldHandleToField = reg.FieldHandleToField |> Map.add alloc handle
                FieldToHandle = reg.FieldToHandle |> Map.add handle alloc
                FieldHandleToId = reg.FieldHandleToId |> Map.add handle newHandle
                NextHandle = reg.NextHandle + 1L
            }

        runtimeFieldHandle alloc, reg, state
