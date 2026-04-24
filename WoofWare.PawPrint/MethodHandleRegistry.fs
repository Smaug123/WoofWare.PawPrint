namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection

type MethodHandle =
    private
        {
            AssemblyFullName : string
            DeclaringType : ConcreteTypeHandle
            MethodHandle : ComparableMethodDefinitionHandle
            MethodGenerics : ConcreteTypeHandle list
        }

    member this.GetAssemblyFullName () : string = this.AssemblyFullName
    member this.GetMethodDefinitionHandle () : ComparableMethodDefinitionHandle = this.MethodHandle
    member this.GetMethodGenerics () : ConcreteTypeHandle list = this.MethodGenerics

type MethodHandleRegistry =
    private
        {
            MethodHandleToId : Map<MethodHandle, int64>
            MethodHandleToMethod : Map<ManagedHeapAddress, MethodHandle>
            MethodToHandle : Map<MethodHandle, ManagedHeapAddress>
            NextHandle : int64
        }

[<RequireQualifiedAccess>]
module MethodHandleRegistry =
    let empty () =
        {
            MethodHandleToMethod = Map.empty
            MethodToHandle = Map.empty
            MethodHandleToId = Map.empty
            NextHandle = 1L
        }

    let rec private isReferenceShaped (typeDefn : TypeDefn) : bool =
        match typeDefn with
        | TypeDefn.PrimitiveType PrimitiveType.Object
        | TypeDefn.PrimitiveType PrimitiveType.String
        | TypeDefn.Array _
        | TypeDefn.OneDimensionalArrayLowerBoundZero _
        | TypeDefn.FromReference (_, System.Reflection.Metadata.SignatureTypeKind.Class)
        | TypeDefn.FromDefinition (_, System.Reflection.Metadata.SignatureTypeKind.Class) -> true
        | TypeDefn.GenericInstantiation (generic, _) -> isReferenceShaped generic
        | TypeDefn.Modified (original, _, _) -> isReferenceShaped original
        | TypeDefn.PrimitiveType _
        | TypeDefn.Pinned _
        | TypeDefn.Pointer _
        | TypeDefn.Byref _
        | TypeDefn.FromReference _
        | TypeDefn.FromDefinition _
        | TypeDefn.FunctionPointer _
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _
        | TypeDefn.Void -> false

    /// Returns a (struct) System.RuntimeMethodHandle, with its contents (reference type) freshly allocated if necessary.
    let getOrAllocate
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (allConcreteTypes : AllConcreteTypes)
        (allocState : 'allocState)
        (allocate : CliValueType -> 'allocState -> ManagedHeapAddress * 'allocState)
        (method : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (reg : MethodHandleRegistry)
        : CliType * MethodHandleRegistry * 'allocState
        =

        let runtimeMethodHandle (runtimeMethodInfoStub : ManagedHeapAddress) =
            // RuntimeMethodHandle is a struct; it contains one field, an IRuntimeMethodInfo.
            // In practice we expect to use RuntimeMethodInfoStub for that IRuntimeMethodInfo:
            // https://github.com/dotnet/runtime/blob/1d1bf92fcf43aa6981804dc53c5174445069c9e4/src/coreclr/System.Private.CoreLib/src/System/RuntimeHandles.cs#L921
            let runtimeMethodHandleType = baseClassTypes.RuntimeMethodHandle
            let field = runtimeMethodHandleType.Fields |> List.exactlyOne

            if field.Name <> "m_value" then
                failwith $"unexpected field name %s{field.Name} for BCL type RuntimeMethodHandle"

            {
                Name = "m_value"
                Contents = CliType.ofManagedObject runtimeMethodInfoStub
                Offset = None
                Type =
                    AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.RuntimeMethodInfoStub
            }
            |> List.singleton
            |> CliValueType.OfFields
                baseClassTypes
                allConcreteTypes
                (AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.RuntimeMethodHandle)
                Layout.Default
            |> CliType.ValueType

        let handle =
            {
                AssemblyFullName = method.DeclaringType.Assembly.FullName
                MethodHandle = ComparableMethodDefinitionHandle.Make method.Handle
                DeclaringType =
                    AllConcreteTypes.findExistingConcreteType
                        allConcreteTypes
                        method.DeclaringType.Identity
                        method.DeclaringType.Generics
                    |> Option.defaultWith (fun () ->
                        failwith $"declaring type for method %O{method} was not found in ConcreteTypes"
                    )
                MethodGenerics = method.Generics |> Seq.toList
            }

        match Map.tryFind handle reg.MethodToHandle with
        | Some v -> runtimeMethodHandle v, reg, allocState
        | None ->

        let newHandle = reg.NextHandle

        let runtimeMethodHandleInternal =
            let field = baseClassTypes.RuntimeMethodHandleInternal.Fields |> List.exactlyOne

            if field.Name <> "m_handle" then
                failwith $"unexpected field name %s{field.Name} for BCL type RuntimeMethodHandleInternal"

            match field.Signature with
            | TypeDefn.PrimitiveType PrimitiveType.IntPtr -> ()
            | s -> failwith $"bad RuntimeMethodHandleInternal.m_handle signature: {s}"

            {
                Name = "m_handle"
                Contents = CliType.RuntimePointer (CliRuntimePointer.MethodRegistryHandle newHandle)
                Offset = None
                Type = AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.IntPtr
            }
            |> List.singleton
            |> CliValueType.OfFields
                baseClassTypes
                allConcreteTypes
                (AllConcreteTypes.getRequiredNonGenericHandle
                    allConcreteTypes
                    baseClassTypes.RuntimeMethodHandleInternal)
                Layout.Default
            |> CliType.ValueType

        let runtimeMethodInfoStub =
            let objType =
                AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.Object

            let runtimeMethodHandleInternalType =
                AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.RuntimeMethodHandleInternal

            let fields =
                baseClassTypes.RuntimeMethodInfoStub.Fields
                |> List.filter (fun field -> not field.IsStatic)
                |> List.map (fun field ->
                    if field.Name = "m_value" then
                        {
                            Name = field.Name
                            Contents = runtimeMethodHandleInternal
                            Offset = field.Offset
                            Type = runtimeMethodHandleInternalType
                        }
                    else
                        if not (isReferenceShaped field.Signature) then
                            failwith
                                $"RuntimeMethodInfoStub field %s{field.Name} was expected to be reference-shaped, got %O{field.Signature}"

                        {
                            Name = field.Name
                            Contents = CliType.ObjectRef None
                            Offset = field.Offset
                            Type = objType
                        }
                )

            if fields |> List.exists (fun field -> field.Name = "m_value") |> not then
                failwith "RuntimeMethodInfoStub did not contain the expected m_value field"

            fields
            |> CliValueType.OfFields
                baseClassTypes
                allConcreteTypes
                (AllConcreteTypes.getRequiredNonGenericHandle allConcreteTypes baseClassTypes.RuntimeMethodInfoStub)
                Layout.Default

        let alloc, state = allocate runtimeMethodInfoStub allocState

        let reg =
            {
                MethodHandleToMethod = reg.MethodHandleToMethod |> Map.add alloc handle
                MethodToHandle = reg.MethodToHandle |> Map.add handle alloc
                MethodHandleToId = reg.MethodHandleToId |> Map.add handle newHandle
                NextHandle = reg.NextHandle + 1L
            }

        runtimeMethodHandle alloc, reg, state

    /// Given the ManagedHeapAddress of a RuntimeMethodInfoStub, resolve it to the MethodHandle.
    let resolveMethodFromAddress (addr : ManagedHeapAddress) (reg : MethodHandleRegistry) : MethodHandle option =
        Map.tryFind addr reg.MethodHandleToMethod
