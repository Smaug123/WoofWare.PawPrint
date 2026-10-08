namespace WoofWare.PawPrint.Analysis

open System.Reflection.Metadata
open WoofWare.PawPrint

/// A claim about the program that the escape analysis may take as given rather than derive from the
/// code it reads. Each one lets the analysis replace a method's body with a contract
/// (`Assumption.raises`); a caller chooses which it allows (`EscapeAnalysis.create`), and each
/// answer lists those it relied on (`Escapes.Assumes`).
[<RequireQualifiedAccess>]
type Assumption =
    /// CoreLib's lookup of its own resource strings, `System.SR.InternalGetResourceString`, which
    /// gives every CoreLib exception its message, raises only what `Assumption.raises` lists. That
    /// holds when:
    /// - only CoreLib writes CoreLib's private static fields. Reflection could otherwise replace the
    ///   lookup's resource manager with a subclass whose `GetString` throws anything;
    /// - the runtime is installed completely and intact. A damaged resources file would otherwise
    ///   throw from the reader;
    /// - every culture the lookup walks (the current UI culture and each one its `Parent` chain
    ///   reaches) that is of a subclass of `CultureInfo` the program defines behaves as CoreLib's own
    ///   `CultureInfo` would for that culture: its overrides raise nothing, its `Name` is a valid
    ///   culture name, its `Parent` chain ends at the invariant culture, and it changes no culture
    ///   state. The lookup reads each culture's `Name` and `Parent`; what an override throws there
    ///   escapes the exception's constructor, and a name the lookup rejects raises
    ///   `ArgumentException`.
    | CoreLibResourceLookup
    /// Every type that the metadata of a loaded type names loads: the assembly that defines it is
    /// found without running the program's `AssemblyResolve` or `Resolving` handlers, is intact,
    /// and defines the type as the name says. So CoreCLR's native code for listing a generic
    /// parameter's constraints, the QCall `RuntimeTypeHandle_GetConstraints`, which loads each
    /// constraint's type, raises only what `Assumption.raises` lists, besides what constructing the
    /// exceptions it makes raises (`Assumption.constructs`). Were an assembly missing, the runtime
    /// would run those handlers, which could throw anything.
    | NamedTypesLoad

[<RequireQualifiedAccess>]
module Assumption =

    /// Every assumption.
    let all : Set<Assumption> =
        Set.ofList [ Assumption.CoreLibResourceLookup ; Assumption.NamedTypesLoad ]

    let private corelib : string = "System.Private.CoreLib"

    let private exceptionName (fullName : string) : ExceptionName =
        match ExceptionName.parse fullName with
        | Some name -> name
        | None -> failwith $"Assumption: %s{fullName} is not an exception type's full name"

    /// The exceptions the method an assumption summarises can raise, when the assumption holds.
    let raises (assumption : Assumption) : ExceptionName list =
        match assumption with
        | Assumption.CoreLibResourceLookup ->
            [
                // It allocates: the key's list, the strings it reads.
                exceptionName "System.OutOfMemoryException"
                // It runs the type initializers of the resource reader's types, which fail only by
                // running out of memory.
                exceptionName "System.TypeInitializationException"
                // It reads the key's length before anything else.
                exceptionName "System.NullReferenceException"
                // It waits for a lock, and an interrupted wait raises this.
                exceptionName "System.Threading.ThreadInterruptedException"
                // It makes calls, any of which can run out of stack.
                exceptionName "System.StackOverflowException"
            ]
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

    /// The exceptions the method an assumption summarises makes in native code before raising them,
    /// by running each one's parameterless constructor, which is CoreLib's managed code: what that
    /// constructor raises, it raises too. CoreCLR's `EEException::CreateThrowable` makes an
    /// exception this way, then looks up its message through `SR.GetResourceString`, as CoreLib's
    /// exception constructors themselves do, so following the constructor accounts for both.
    let constructs (assumption : Assumption) : ExceptionName list =
        match assumption with
        | Assumption.CoreLibResourceLookup -> []
        | Assumption.NamedTypesLoad -> [ exceptionName "System.ArgumentException" ]

    /// The assumption that summarises `method` of `assembly`, when `assembly` is a CoreLib and the
    /// method is the one some assumption names: by class, name and signature, or, for a QCall, by
    /// class, name and entry point.
    let summarises (assembly : DumpedAssembly) (method : MethodDefinitionHandle) : Assumption option =
        let definition = assembly.Methods.[method]

        let declaringType =
            assembly.TypeDefs.[definition.RequiredDeclaringType.Definition.Get]

        let string = TypeDefn.PrimitiveType PrimitiveType.String

        let inCoreLib (ns : string) (name : string) : bool =
            assembly.ThisAssemblyDefinition.Name.Name = corelib
            && not declaringType.IsNested
            && declaringType.Generics.IsEmpty
            && declaringType.Namespace = ns
            && declaringType.Name = name
            && definition.IsStatic
            && definition.Signature.GenericParameterCount = 0

        let qcall (entryPoint : string) : bool =
            match definition.Body, definition.TryNativeImport with
            | MethodBody.PInvoke, Some import -> import.ModuleName = "QCall" && import.EntryPointName = entryPoint
            | _ -> false

        if
            inCoreLib "System" "SR"
            && definition.Name = "InternalGetResourceString"
            && definition.Signature.ParameterTypes = [ string ]
            && definition.Signature.ReturnType = MethodReturnType.Returns string
        then
            Some Assumption.CoreLibResourceLookup
        elif
            inCoreLib "System" "RuntimeTypeHandle"
            && definition.Name = "GetConstraints"
            && qcall "RuntimeTypeHandle_GetConstraints"
        then
            Some Assumption.NamedTypesLoad
        else
            None
