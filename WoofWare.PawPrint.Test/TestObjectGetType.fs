namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PawPrint.Analysis

/// `ObjectGetType` stands in for `Object.GetType`, on the premise that the method reaches native
/// code only through the QCall that makes a type's `RuntimeType` on first use, and that the QCall
/// loads a type only where the type is not fully loaded. These tests hold that premise to each
/// CoreLib's IL and to the pinned runtime's C++.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestObjectGetType =

    type private Method = MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>

    let coreLibs : TestCaseData list = TestIntrinsicBody.coreLibs

    let private fullName (corelib : DumpedAssembly) (method : Method) : string =
        let declaringType = corelib.TypeDefs.[method.RequiredDeclaringType.Definition.Get]
        $"%s{declaringType.Namespace}.%s{declaringType.Name}::%s{method.Name}"

    /// The methods `method`'s IL calls, in order, each by its full name: every target in this
    /// CoreLib, which a `MethodDef` or a `MethodSpec` of one names.
    let private callees (corelib : DumpedAssembly) (method : Method) : string list =
        let named (token : MetadataToken) : string =
            match token with
            | MetadataToken.MethodDef handle -> fullName corelib corelib.Methods.[handle]
            | other -> failwith $"%s{fullName corelib method} calls %A{other}, which is not a method definition"

        match MethodInfo.tryIlBody method with
        | None -> failwith $"%s{fullName corelib method} has no IL: %A{method.Body}"
        | Some body ->
            body.Instructions
            |> List.choose (fun (op, _) ->
                match op with
                | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Call, operand)
                | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Callvirt, operand)
                | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Newobj, operand) ->
                    match operand with
                    | MetadataOperand.FromMetadata sourced ->
                        match sourced.Token with
                        | MetadataToken.MethodSpecification handle -> Some (named corelib.MethodSpecs.[handle].Method)
                        | token -> Some (named token)
                    | MetadataOperand.FromDynamicScope _ ->
                        failwith $"%s{fullName corelib method} has a dynamic-scope operand"
                | _ -> None
            )

    [<TestCaseSource(nameof coreLibs)>]
    let ``the contract stands in for one method of the CoreLib, Object.GetType`` (which : AuditedCoreLib) : unit =
        let corelib = TestIntrinsicBody.coreLib which

        let recognised =
            [
                for KeyValue (handle, method) in corelib.Methods do
                    if ObjectGetType.isGetType corelib handle then
                        yield method
            ]

        let method = recognised |> List.exactlyOne
        fullName corelib method |> shouldEqual "System.Object::GetType"
        method.IsStatic |> shouldEqual false
        method.IsVirtual |> shouldEqual false
        method.Signature.ParameterTypes |> shouldEqual []

    [<TestCaseSource(nameof coreLibs)>]
    let ``Object.GetType reaches native code only through the QCall that makes a RuntimeType``
        (which : AuditedCoreLib)
        : unit
        =
        let corelib = TestIntrinsicBody.coreLib which

        let onlyOne (typeName : string) (methodName : string) (accept : Method -> bool) : Method =
            corelib.Methods.Values
            |> Seq.filter (fun (method : Method) ->
                fullName corelib method = $"%s{typeName}::%s{methodName}" && accept method
            )
            |> Seq.exactlyOne

        let getType =
            corelib.Methods
            |> Seq.filter (fun (KeyValue (handle, _)) -> ObjectGetType.isGetType corelib handle)
            |> Seq.exactlyOne
            |> fun (KeyValue (_, method)) -> method

        // `GetMethodTable` and `KeepAlive` the JIT expands, reading the object's header, and
        // keeping it alive, which runs nothing.
        callees corelib getType
        |> shouldEqual
            [
                "System.Runtime.CompilerServices.RuntimeHelpers::GetMethodTable"
                "System.RuntimeTypeHandle::GetRuntimeType"
                "System.GC::KeepAlive"
            ]

        // The `RuntimeType` it has made already, read through the method table's auxiliary data,
        // or the slow path.
        let getRuntimeType =
            onlyOne "System.RuntimeTypeHandle" "GetRuntimeType" (fun (method : Method) -> method.IsStatic)

        callees corelib getRuntimeType
        |> shouldEqual
            [
                "System.Runtime.CompilerServices.MethodTableAuxiliaryData::get_ExposedClassObject"
                "System.RuntimeTypeHandle::GetRuntimeTypeFromHandleSlow"
            ]

        let slowPaths =
            corelib.Methods.Values
            |> Seq.filter (fun (method : Method) ->
                fullName corelib method = "System.RuntimeTypeHandle::GetRuntimeTypeFromHandleSlow"
            )
            |> List.ofSeq

        let wrapper =
            slowPaths
            |> List.filter (fun (method : Method) -> method.Signature.ParameterTypes.Length = 1)
            |> List.exactlyOne

        callees corelib wrapper
        |> shouldEqual
            [
                "System.Runtime.CompilerServices.ObjectHandleOnStack::Create"
                "System.RuntimeTypeHandle::GetRuntimeTypeFromHandleSlow"
            ]

        let qcall =
            slowPaths
            |> List.filter (fun (method : Method) -> method.Signature.ParameterTypes.Length = 2)
            |> List.exactlyOne

        match qcall.Body, qcall.TryNativeImport with
        | MethodBody.PInvoke, Some import ->
            import.ModuleName |> shouldEqual "QCall"

            import.EntryPointName
            |> shouldEqual "RuntimeTypeHandle_GetRuntimeTypeFromHandleSlow"
        | other -> failwith $"The slow path's native half is not a QCall: %A{other}"

    /// The C++ function `name` that `path`, under the pinned runtime source's `src/coreclr`,
    /// defines.
    let private definition (path : string) (name : string) : SourceRoutine =
        let coreclr =
            Path.Combine (FrameworkUnderTest.requireRuntimeSource (), "src", "coreclr")

        File.ReadAllText (Path.Combine (coreclr, path))
        |> CppSource.definitions
        |> List.filter (fun routine -> routine.Name = name)
        |> List.exactlyOne

    [<Test>]
    let ``the QCall loads a type only where the type is not fully loaded`` () : unit =
        let runtime = FrameworkUnderTest.runtime ()

        // The QCall makes the `RuntimeType` of the type handle it is given.
        (definition "vm/runtimehandles.cpp" "RuntimeTypeHandle_GetRuntimeTypeFromHandleSlow").Calls
        |> shouldEqual (
            set
                [
                    "GCX_COOP"
                    "Get"
                    "GetManagedClassObject"
                    "Set"
                    "TypeHandle::FromPtr"
                    "_ASSERTE"
                ]
        )

        // An object's type has a method table, not a type descriptor, which only a pointer, a
        // byref, a function pointer or a type variable has.
        (definition "vm/typehandle.cpp" "TypeHandle::GetManagedClassObject").Calls
        |> shouldEqual (
            set
                [
                    "AsMethodTable"
                    "AsTypeDesc"
                    "COMPlusThrowOM"
                    "GetManagedClassObject"
                    "INJECT_FAULT"
                    "IsTypeDesc"
                    "MaybeTrigger"
                ]
        )

        // The method table's makes the `RuntimeType` where it has none, after `CheckRestore`.
        let contractMacros =
            match runtime with
            | EmulatedRuntime.Net10 -> set [ "CONTRACT" ; "POSTCONDITION" ; "RETURN" ]
            | EmulatedRuntime.Net11 -> Set.empty

        (definition "vm/methodtable.cpp" "MethodTable::GetManagedClassObject").Calls
        |> shouldEqual (
            contractMacros
            + set
                [
                    "AllocateManagedClassObject"
                    "COMPlusThrowOM"
                    "CheckRestore"
                    "GetAuxiliaryData"
                    "GetAuxiliaryDataForWrite"
                    "GetManagedClassObjectIfExists"
                    "INJECT_FAULT"
                    "MaybeTrigger"
                    "TypeHandle"
                ]
        )

        // `CheckRestore` loads a type that is not fully loaded, and does nothing to one that is.
        let checkRestore = definition "vm/methodtable.cpp" "MethodTable::CheckRestore"

        checkRestore.Calls
        |> shouldEqual (
            set
                [
                    "CONTRACTL_END"
                    "ClassLoader::EnsureLoaded"
                    "FORBIDGC_LOADER_USE_ENABLED"
                    "IsFullyLoaded"
                    "_ASSERTE"
                ]
        )

        checkRestore.Text.Contains "if (!IsFullyLoaded())" |> shouldEqual true

        // Making the `RuntimeType` allocates it, loading nothing and running no managed code, but on
        // .NET 11 refuses a runtime-async continuation, as the contract says.
        let allocate =
            definition "vm/typehandle.cpp" "TypeHandle::AllocateManagedClassObject"

        let refusal =
            match runtime with
            | EmulatedRuntime.Net10 -> Set.empty
            | EmulatedRuntime.Net11 -> set [ "CONTRACTL_END" ; "COMPlusThrow" ; "IsContinuationWithoutMetadata" ; "W" ]

        allocate.Calls
        |> shouldEqual (
            refusal
            + set
                [
                    "AllocateObject"
                    "AppDomain::GetCurrentDomain"
                    "CanUnload"
                    "CrstHolder"
                    "GCPROTECT_BEGIN"
                    "GCPROTECT_END"
                    "GetBaseSize"
                    "GetExposedObject"
                    "GetLoaderAllocator"
                    "GetMethodTableExposedClassObjectLock"
                    "InsertObjectIntoFieldWithLifetimeOfCollectibleLoaderAllocator"
                    "ObjectToOBJECTREF"
                    "SetKeepAlive"
                    "SetType"
                    "SystemDomain::GetFrozenObjectHeapManager"
                    "TryAllocateObject"
                    "VolatileLoad"
                    "VolatileStore"
                    "_ASSERTE"
                ]
        )

        let refuses = allocate.Text.Contains "COMPlusThrow(kNotSupportedException"

        ObjectGetType.raises runtime
        |> List.exists (fun name -> name.Namespace = "System" && name.Name = "NotSupportedException")
        |> shouldEqual refuses
