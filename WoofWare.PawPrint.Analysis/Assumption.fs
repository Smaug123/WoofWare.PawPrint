namespace WoofWare.PawPrint.Analysis

open System.Reflection.Metadata
open WoofWare.PawPrint

/// A claim about the program that the escape analysis may take as given rather than derive from the
/// code it reads. Each one lets the analysis replace a method's body with a contract
/// (`Assumption.raises`), or, for `JitOptimises`, code with what the JIT compiles it to; a caller
/// chooses which it allows (`EscapeAnalysis.create`), and each answer lists those it relied on
/// (`Escapes.Assumes`).
[<RequireQualifiedAccess>]
type Assumption =
    /// Every type that the metadata of a loaded type names loads: the assembly that defines it is
    /// found without running the program's `AssemblyResolve` or `Resolving` handlers, is intact,
    /// and defines the type as the name says. So CoreCLR's native code for listing a generic
    /// parameter's constraints, the QCall `RuntimeTypeHandle_GetConstraints`, which loads each
    /// constraint's type, raises only what `Assumption.raises` lists, besides what constructing the
    /// exceptions it makes raises (`Assumption.constructs`); and a native whose contract holds
    /// under `NativeAssumption.NamedTypesLoad` keeps to it, as the cast helpers' fallback,
    /// `IsInstanceOf_NoCacheLookup`, does where reflection asks it about an open type. Were an
    /// assembly missing, the runtime would run those handlers, which could throw anything.
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
    /// CoreCLR's JIT compiles every method with at least the optimisations it makes at tier 0
    /// (the JIT's `Tier0OptimizationEnabled`), wherever neither the method nor its assembly says
    /// otherwise: no debugger or profiler disables optimisation, `DOTNET_JitMinOpts` and
    /// `DOTNET_JitDebuggable` are unset, and no interpreter runs the code. Then the JIT compiles
    /// `typeof(X).IsValueType` to a constant, compares `typeof(X) == typeof(Y)`, or `!=`, by the
    /// two types' handles, and compares any other `t == typeof(X)` by reference, calling neither
    /// `Type.GetTypeFromHandle` nor the getter or the operator. The method says otherwise when it
    /// is marked
    /// `MethodImplOptions.NoOptimization`, and the assembly when its `DebuggableAttribute`
    /// disables optimisation (`Debuggable.allowsJitOptimisation`). This assumption stands in for
    /// no method's body, so `Assumption.raises` and `Assumption.constructs` list nothing for it.
    | JitOptimises
    /// Every class implementing `IDynamicInterfaceCastable` raises from `IsInterfaceImplemented`
    /// only the `InvalidCastException` its documentation asks for, and that only where it is asked
    /// to throw rather than to answer false; and formatting the message of the
    /// `InvalidCastException` CoreLib throws in its place from the two types' names raises only
    /// what allocating and looking up a resource raises. CoreCLR asks an object whose class
    /// implements that interface, where a cast to an interface its class does not implement misses
    /// the cast cache, through `DynamicInterfaceCastableHelpers.IsInterfaceImplemented`, which
    /// raises only what `Assumption.raises` lists when this holds. Whether it does turns on code
    /// the program supplies, and on reflection over the two types the analysis does not see into.
    | DynamicCastsAsDocumented

[<RequireQualifiedAccess>]
module Assumption =

    /// Every assumption.
    let all : Set<Assumption> =
        Set.ofList
            [
                Assumption.NamedTypesLoad
                Assumption.StackTracePreserved
                Assumption.CoreLibTypeInitializers
                Assumption.JitOptimises
                Assumption.DynamicCastsAsDocumented
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
        | Assumption.JitOptimises -> []
        | Assumption.DynamicCastsAsDocumented ->
            [
                // What the documentation asks for, and what CoreLib throws in its place.
                exceptionName "System.InvalidCastException"
                // Calling the object's method on a null one: CoreCLR passes it only an object, but
                // another caller may pass null.
                exceptionName "System.NullReferenceException"
                // Making that allocates.
                exceptionName "System.OutOfMemoryException"
                // Its message is looked up through `SR`, whose initializer may fail, and whose
                // lookup waits for a lock, and an interrupted wait raises this.
                exceptionName "System.TypeInitializationException"
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
        | Assumption.CoreLibTypeInitializers
        | Assumption.JitOptimises
        | Assumption.DynamicCastsAsDocumented -> []

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
            inCoreLib "System.Runtime.InteropServices" "DynamicInterfaceCastableHelpers" true
            && definition.Name = "IsInterfaceImplemented"
            && definition.Signature.ParameterTypes.Length = 3
            && definition.Signature.ReturnType = MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.Boolean)
            && il
        then
            Some Assumption.DynamicCastsAsDocumented
        elif
            assembly.ThisAssemblyDefinition.Name.Name = corelib
            && definition.IsStatic
            && definition.Name = ".cctor"
        then
            Some Assumption.CoreLibTypeInitializers
        else
            None

    /// The assumption that is the claim a native's contract holds under.
    let ofNative (assumption : NativeAssumption) : Assumption =
        match assumption with
        | NativeAssumption.NamedTypesLoad -> Assumption.NamedTypesLoad
