namespace WoofWare.PawPrint

open System.Reflection
open Microsoft.Extensions.Logging

/// What CoreCLR's class loader makes of a method's return type when it decides which methods take
/// part in runtime async (`MethodReturnKind`, method.hpp; `ClassifyMethodReturnKind`, method.cpp).
[<RequireQualifiedAccess>]
type MethodReturnKind =
    /// Returns anything but one of the four promise types below.
    | NormalMethod
    /// Returns CoreLib's `System.Threading.Tasks.Task` or `ValueTask`.
    | NonGenericTaskReturningMethod
    /// Returns CoreLib's `System.Threading.Tasks.Task<T>` or `ValueTask<T>`.
    | GenericTaskReturningMethod

/// Which methods CoreCLR's class loader makes callable with runtime async's calling convention.
[<RequireQualifiedAccess>]
module RuntimeAsyncMethod =

    /// `MethodImplAttributes.Async`, which the `System.Reflection` of this project's target
    /// framework does not name.
    let asyncImplAttribute : MethodImplAttributes = enum<MethodImplAttributes> 0x2000

    [<Literal>]
    let private PromiseNamespace = "System.Threading.Tasks"

    /// Reading an element type from a signature skips the custom modifiers before it
    /// (`SigPointer::GetElemType`).
    let rec private unmodified (ty : TypeDefn) : TypeDefn =
        match ty with
        | TypeDefn.Modified modified -> unmodified modified.Unmodified
        | ty -> ty

    /// Whether the type a signature names by `token` is defined in CoreLib
    /// (`IsTypeDefOrRefImplementedInSystemModule`, method.cpp): a TypeDef when `declaringAssembly`
    /// is CoreLib, and a TypeRef when it binds to a definition there.
    let private definedInCoreLib
        (loggerFactory : ILoggerFactory)
        (runtimeDirs : string seq)
        (coreLib : DumpedAssembly)
        (declaringAssembly : DumpedAssembly)
        (token : TypeDefn)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * Result<bool, TypeBindingFailure>
        =
        match token with
        | TypeDefn.FromDefinition (identity, _) ->
            assemblies, Ok (identity.AssemblyFullName = coreLib.DefinitionFullName)
        | TypeDefn.FromReference (typeRef, _) ->
            match
                TypeResolution.tryResolveTypeRefIdentity loggerFactory runtimeDirs declaringAssembly typeRef assemblies
            with
            | assemblies, TypeReferenceIdentity.Resolved identity ->
                assemblies, Ok (identity.AssemblyFullName = coreLib.DefinitionFullName)
            | assemblies, TypeReferenceIdentity.AssemblyUnavailable reference ->
                assemblies, Error (TypeBindingFailure.AssemblyUnavailable reference)
            | assemblies, TypeReferenceIdentity.TypeAbsent miss ->
                assemblies, Error (TypeBindingFailure.TypeAbsent miss)
            | assemblies, TypeReferenceIdentity.Unmodelled unmodelled ->
                assemblies, Error (TypeBindingFailure.Unmodelled unmodelled)
        | token -> failwith $"logic error: %O{token} is not a TypeDef or TypeRef token"

    /// CoreCLR's `ClassifyMethodReturnKind` (method.cpp) of `returnType`, the return type of a
    /// method `declaringAssembly` declares. It compares the names a TypeDef or TypeRef row gives,
    /// and binds a TypeRef only when its name matches, which may load the assembly it names.
    let classifyReturn
        (loggerFactory : ILoggerFactory)
        (runtimeDirs : string seq)
        (coreLib : DumpedAssembly)
        (declaringAssembly : DumpedAssembly)
        (returnType : MethodReturnType<TypeDefn>)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * Result<MethodReturnKind, TypeBindingFailure>
        =
        // The name and namespace of the row a signature's token names, and whether the signature
        // spells it `valuetype`.
        let rowName (token : TypeDefn) : (string * string * bool) option =
            match token with
            | TypeDefn.FromDefinition (identity, kind) ->
                let typeDef = declaringAssembly.TypeDefs.[identity.TypeDefinition.Get]
                Some (typeDef.Namespace, typeDef.Name, kind)
            | TypeDefn.FromReference (typeRef, kind) -> Some (typeRef.Namespace, typeRef.Name, kind)
            | _ -> None
            |> Option.map (fun (ns, name, kind) ->
                match kind with
                | System.Reflection.Metadata.SignatureTypeKind.ValueType -> ns, name, true
                | System.Reflection.Metadata.SignatureTypeKind.Class -> ns, name, false
                | kind ->
                    failwith
                        $"logic error: a signature spelled %s{ns}.%s{name} as neither class nor valuetype (%O{kind})"
            )

        let candidate =
            match returnType with
            | MethodReturnType.Void -> None
            | MethodReturnType.Returns ty ->
                match unmodified ty with
                | TypeDefn.GenericInstantiation (generic, typeArgs) when typeArgs.Length = 1 ->
                    let generic = unmodified generic

                    rowName generic
                    |> Option.bind (fun (ns, name, isValueTask) ->
                        let expected = if isValueTask then "ValueTask`1" else "Task`1"

                        if ns = PromiseNamespace && name = expected then
                            Some (generic, MethodReturnKind.GenericTaskReturningMethod)
                        else
                            None
                    )
                | token ->
                    rowName token
                    |> Option.bind (fun (ns, name, isValueTask) ->
                        let expected = if isValueTask then "ValueTask" else "Task"

                        if ns = PromiseNamespace && name = expected then
                            Some (token, MethodReturnKind.NonGenericTaskReturningMethod)
                        else
                            None
                    )

        match candidate with
        | None -> assemblies, Ok MethodReturnKind.NormalMethod
        | Some (token, kind) ->
            match definedInCoreLib loggerFactory runtimeDirs coreLib declaringAssembly token assemblies with
            | assemblies, Ok true -> assemblies, Ok kind
            | assemblies, Ok false -> assemblies, Ok MethodReturnKind.NormalMethod
            | assemblies, Error failure -> assemblies, Error failure

    /// Whether `declaringType`, of `declaringAssembly`, is a delegate type: CoreCLR's
    /// `MethodTable::IsDelegate`, whose immediate parent is `System.MulticastDelegate`.
    let private isDelegate
        (loggerFactory : ILoggerFactory)
        (runtimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (declaringAssembly : DumpedAssembly)
        (declaringType : TypeInfo<'generic, 'field>)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * Result<bool, TypeBindingFailure>
        =
        let multicastDelegate = baseClassTypes.MulticastDelegateType.Identity

        match declaringType.BaseType with
        | None
        | Some (BaseTypeInfo.TypeSpec _) -> assemblies, Ok false
        | Some (BaseTypeInfo.TypeDef handle) ->
            assemblies,
            Ok (
                declaringAssembly.DefinitionFullName = multicastDelegate.AssemblyFullName
                && handle = multicastDelegate.TypeDefinition.Get
            )
        | Some (BaseTypeInfo.TypeRef handle) ->
            let typeRef = declaringAssembly.TypeRefs.[handle]

            match
                TypeResolution.tryResolveTypeRefIdentity loggerFactory runtimeDirs declaringAssembly typeRef assemblies
            with
            | assemblies, TypeReferenceIdentity.Resolved identity -> assemblies, Ok (identity = multicastDelegate)
            | assemblies, TypeReferenceIdentity.AssemblyUnavailable reference ->
                assemblies, Error (TypeBindingFailure.AssemblyUnavailable reference)
            | assemblies, TypeReferenceIdentity.TypeAbsent miss ->
                assemblies, Error (TypeBindingFailure.TypeAbsent miss)
            | assemblies, TypeReferenceIdentity.Unmodelled unmodelled ->
                assemblies, Error (TypeBindingFailure.Unmodelled unmodelled)

    /// CoreCLR's `MethodDesc::IsAsyncMethod` for the method `method`, which `declaringType` of
    /// `declaringAssembly` declares: whether the class loader gives it the `AsyncCall` flag.
    /// This is the method a MethodDef row declares, never the async variant CoreCLR adds beside a
    /// method returning a promise type, which reflection does not see.
    let isAsyncCall
        (loggerFactory : ILoggerFactory)
        (runtimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (declaringAssembly : DumpedAssembly)
        (declaringType : TypeInfo<'generic, 'field>)
        (method : MetadataMethodFacts)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * Result<bool, TypeBindingFailure>
        =
        // `MethodTableBuilder::EnumerateClassMethods`: a method declaring `Async` is `AsyncCall`
        // unless it returns a promise type, when its async variant is instead. It classifies every
        // method of a delegate as returning normally. Without `Async`, a method is never
        // `AsyncCall`, so nothing else need be read.
        if not (method.ImplAttributes.HasFlag asyncImplAttribute) then
            assemblies, Ok false
        else

        match isDelegate loggerFactory runtimeDirs baseClassTypes declaringAssembly declaringType assemblies with
        | assemblies, Error failure -> assemblies, Error failure
        | assemblies, Ok true -> assemblies, Ok true
        | assemblies, Ok false ->
            match
                classifyReturn
                    loggerFactory
                    runtimeDirs
                    baseClassTypes.Corelib
                    declaringAssembly
                    method.RawSignature.ReturnType
                    assemblies
            with
            | assemblies, Ok MethodReturnKind.NormalMethod -> assemblies, Ok true
            | assemblies, Ok MethodReturnKind.NonGenericTaskReturningMethod
            | assemblies, Ok MethodReturnKind.GenericTaskReturningMethod -> assemblies, Ok false
            | assemblies, Error failure -> assemblies, Error failure
