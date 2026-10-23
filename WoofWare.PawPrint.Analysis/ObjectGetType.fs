namespace WoofWare.PawPrint.Analysis

open System.Reflection.Metadata
open WoofWare.PawPrint

/// CoreLib's `Object.GetType`, which the escape analysis always takes to raise only
/// `ObjectGetType.raises` in place of its body.
///
/// Its body reads the object's method table and returns the type's `RuntimeType`, made on first
/// use by the QCall `RuntimeTypeHandle_GetRuntimeTypeFromHandleSlow`. That QCall finishes loading a
/// type that is not fully loaded, which can run an `AssemblyLoadContext`'s managed resolution, so no
/// contract describes it for every type it is given. An object's type is fully loaded, though:
/// every way CoreCLR allocates an object either loads its type fully first, as the JIT does for the
/// type an instruction names and as reflection does for a type it instantiates, or needs the
/// type's `RuntimeType`, and making that loads the type fully or throws. So here the QCall only
/// allocates the `RuntimeType`, or on .NET 11 refuses to make one for a runtime-async continuation.
[<RequireQualifiedAccess>]
module internal ObjectGetType =

    let private exceptionName (fullName : string) : ExceptionName =
        match ExceptionName.parse fullName with
        | Some name -> name
        | None -> failwith $"ObjectGetType: %s{fullName} is not an exception type's full name"

    /// The exceptions CoreCLR makes by running their parameterless constructors when
    /// `Object.GetType` raises them on `runtime`.
    let constructs (runtime : EmulatedRuntime) : ExceptionName list =
        [
            // Called on null, it reads the object's method table.
            yield exceptionName "System.NullReferenceException"
            match runtime with
            | EmulatedRuntime.Net10 -> ()
            // A continuation of runtime-async code is of a type the runtime makes without metadata,
            // which has no `RuntimeType` (`TypeHandle::AllocateManagedClassObject`).
            | EmulatedRuntime.Net11 -> yield exceptionName "System.NotSupportedException"
        ]

    /// The exceptions `Object.GetType` can raise on `runtime`.
    let raises (runtime : EmulatedRuntime) : ExceptionName list =
        constructs runtime
        @ [
            // Allocating the `RuntimeType`; CoreCLR raises its preallocated one.
            exceptionName "System.OutOfMemoryException"
            // It makes calls, any of which can run out of stack.
            exceptionName "System.StackOverflowException"
        ]

    /// Whether `method` of `assembly` is `Object.GetType`: `assembly` is a CoreLib, and the method
    /// is `System.Object`'s instance, non-virtual `GetType() : System.Type`.
    let isGetType (assembly : DumpedAssembly) (method : MethodDefinitionHandle) : bool =
        let definition = assembly.Methods.[method]

        let declaringType =
            assembly.TypeDefs.[definition.RequiredDeclaringType.Definition.Get]

        let returnsType =
            match definition.Signature.ReturnType with
            | MethodReturnType.Returns (TypeDefn.FromDefinition (identity, _)) when
                identity.AssemblyFullName = assembly.DefinitionFullName
                ->
                let returned = assembly.TypeDefs.[identity.TypeDefinition.Get]

                not returned.IsNested && returned.Namespace = "System" && returned.Name = "Type"
            | _ -> false

        assembly.ThisAssemblyDefinition.Name.Name = "System.Private.CoreLib"
        && not declaringType.IsNested
        && declaringType.Namespace = "System"
        && declaringType.Name = "Object"
        && not definition.IsStatic
        && not definition.IsVirtual
        && definition.Signature.GenericParameterCount = 0
        && definition.Name = "GetType"
        && definition.Signature.ParameterTypes.IsEmpty
        && returnsType
