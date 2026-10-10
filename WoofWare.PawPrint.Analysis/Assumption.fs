namespace WoofWare.PawPrint.Analysis

open System.Reflection.Metadata
open WoofWare.PawPrint

/// A claim about the program that the escape analysis may take as given rather than derive from the
/// code it reads. Each one lets the analysis replace a method's body with a contract
/// (`Assumption.raises`); a caller chooses which it allows (`EscapeAnalysis.create`), and each
/// answer lists those it relied on (`Escapes.Assumes`).
[<RequireQualifiedAccess>]
type Assumption =
    /// Every type that the metadata of a loaded type names loads: the assembly that defines it is
    /// found without running the program's `AssemblyResolve` or `Resolving` handlers, is intact,
    /// and defines the type as the name says. So CoreCLR's native code for listing a generic
    /// parameter's constraints, the QCall `RuntimeTypeHandle_GetConstraints`, which loads each
    /// constraint's type, raises only what `Assumption.raises` lists, besides what constructing the
    /// exceptions it makes raises (`Assumption.constructs`). Were an assembly missing, the runtime
    /// would run those handlers, which could throw anything.
    | NamedTypesLoad
    /// Reading an exception's `Source` and `StackTrace` raises only what `Assumption.raises` lists.
    /// CoreCLR reads both when it throws an exception that has been thrown before
    /// (`Exception.InternalPreserveStackTrace`), as it does a type initializer's own exception when
    /// it cannot wrap that in a `TypeInitializationException`. `Exception`'s own getters walk the
    /// exception's stack trace, reading each method's metadata through reflection and native code
    /// the analysis does not see into. This holds when every exception class that overrides either
    /// getter raises from it only what `Exception`'s own does, and when the metadata of each method
    /// on the stack trace, and of every type and assembly it names, is intact and loads without
    /// running the program's `AssemblyResolve` or `Resolving` handlers.
    | StackTracePreserved
    /// CoreLib's type initializers, each of them, raise only what `Assumption.raises` lists. Every
    /// CoreLib exception's constructor runs one, `SR`'s, which reads an `AppContext` switch through
    /// dictionaries and reflection the analysis does not see into, as many others do. This holds
    /// when the runtime is installed completely and intact; when the configuration the host gives
    /// the runtime (its `runtimeconfig.json` properties and the environment variables it reads) is
    /// well formed; when only CoreLib writes CoreLib's private static fields; and when the
    /// initializer of each of CoreLib's generic types does so whatever its type arguments.
    | CoreLibTypeInitializers

[<RequireQualifiedAccess>]
module Assumption =

    /// Every assumption.
    let all : Set<Assumption> =
        Set.ofList
            [
                Assumption.NamedTypesLoad
                Assumption.StackTracePreserved
                Assumption.CoreLibTypeInitializers
            ]

    let private corelib : string = "System.Private.CoreLib"

    let private exceptionName (fullName : string) : ExceptionName =
        match ExceptionName.parse fullName with
        | Some name -> name
        | None -> failwith $"Assumption: %s{fullName} is not an exception type's full name"

    /// The exceptions each method an assumption summarises can raise, when the assumption holds.
    let raises (assumption : Assumption) : ExceptionName list =
        match assumption with
        | Assumption.NamedTypesLoad ->
            [
                // The runtime raises it for a type that is not a generic parameter
                // (`Assumption.constructs`).
                exceptionName "System.ArgumentException"
                // It allocates the list of constraints, and each constraint's `RuntimeType` on
                // first use.
                exceptionName "System.OutOfMemoryException"
                // Loading a type loads those it names, which can run out of stack.
                exceptionName "System.StackOverflowException"
            ]
        | Assumption.StackTracePreserved ->
            [
                // It allocates: the frames, their methods' reflection objects, the strings.
                exceptionName "System.OutOfMemoryException"
                // It runs the type initializers of reflection's and the stack trace's types, which
                // fail only by running out of memory.
                exceptionName "System.TypeInitializationException"
                // It waits for locks, among them the resource lookup's, and an interrupted wait
                // raises this.
                exceptionName "System.Threading.ThreadInterruptedException"
                // It makes calls, any of which can run out of stack.
                exceptionName "System.StackOverflowException"
            ]
        | Assumption.CoreLibTypeInitializers ->
            [
                // It allocates.
                exceptionName "System.OutOfMemoryException"
                // It runs other types' initializers, which may fail.
                exceptionName "System.TypeInitializationException"
                // It may wait for a lock, and an interrupted wait raises this.
                exceptionName "System.Threading.ThreadInterruptedException"
                // It makes calls, any of which can run out of stack.
                exceptionName "System.StackOverflowException"
            ]

    /// The exceptions the method an assumption summarises makes in native code before raising them,
    /// by running each one's parameterless constructor, which is CoreLib's managed code: what that
    /// constructor raises, it raises too. CoreCLR's `EEException::CreateThrowable` makes an
    /// exception this way, then looks up its message through `SR.GetResourceString`, as CoreLib's
    /// exception constructors themselves do, so following the constructor accounts for both.
    let constructs (assumption : Assumption) : ExceptionName list =
        match assumption with
        | Assumption.NamedTypesLoad -> [ exceptionName "System.ArgumentException" ]
        | Assumption.StackTracePreserved
        | Assumption.CoreLibTypeInitializers -> []

    /// The assumption that summarises `method` of `assembly`, when `assembly` is a CoreLib and the
    /// method is one some assumption names: for a QCall, by class, name and entry point; for IL, by
    /// class, name and signature; and any type initializer.
    let summarises (assembly : DumpedAssembly) (method : MethodDefinitionHandle) : Assumption option =
        let definition = assembly.Methods.[method]

        let declaringType =
            assembly.TypeDefs.[definition.RequiredDeclaringType.Definition.Get]

        let inCoreLib (ns : string) (name : string) (isStatic : bool) : bool =
            assembly.ThisAssemblyDefinition.Name.Name = corelib
            && not declaringType.IsNested
            && declaringType.Generics.IsEmpty
            && declaringType.Namespace = ns
            && declaringType.Name = name
            && definition.IsStatic = isStatic
            && definition.Signature.GenericParameterCount = 0

        let qcall (entryPoint : string) : bool =
            match definition.Body, definition.TryNativeImport with
            | MethodBody.PInvoke, Some import -> import.ModuleName = "QCall" && import.EntryPointName = entryPoint
            | _ -> false

        let il : bool =
            match definition.Body with
            | MethodBody.Il _ -> true
            | _ -> false

        if
            inCoreLib "System" "RuntimeTypeHandle" true
            && definition.Name = "GetConstraints"
            && qcall "RuntimeTypeHandle_GetConstraints"
        then
            Some Assumption.NamedTypesLoad
        elif
            inCoreLib "System" "Exception" false
            && not definition.IsVirtual
            && definition.Name = "InternalPreserveStackTrace"
            && definition.Signature.ParameterTypes.IsEmpty
            && definition.Signature.ReturnType = MethodReturnType.Void
            && il
        then
            Some Assumption.StackTracePreserved
        elif
            assembly.ThisAssemblyDefinition.Name.Name = corelib
            && definition.IsStatic
            && definition.Name = ".cctor"
        then
            Some Assumption.CoreLibTypeInitializers
        else
            None
