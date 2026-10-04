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

[<RequireQualifiedAccess>]
module Assumption =

    /// Every assumption.
    let all : Set<Assumption> = Set.ofList [ Assumption.CoreLibResourceLookup ]

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

    /// The assumption that summarises `method` of `assembly`, when `assembly` is a CoreLib and the
    /// method is the one some assumption names, by class, name and signature.
    let summarises (assembly : DumpedAssembly) (method : MethodDefinitionHandle) : Assumption option =
        let definition = assembly.Methods.[method]

        let declaringType =
            assembly.TypeDefs.[definition.RequiredDeclaringType.Definition.Get]

        let string = TypeDefn.PrimitiveType PrimitiveType.String

        if
            assembly.ThisAssemblyDefinition.Name.Name = corelib
            && not declaringType.IsNested
            && declaringType.Generics.IsEmpty
            && declaringType.Namespace = "System"
            && declaringType.Name = "SR"
            && definition.Name = "InternalGetResourceString"
            && definition.IsStatic
            && definition.Signature.GenericParameterCount = 0
            && definition.Signature.ParameterTypes = [ string ]
            && definition.Signature.ReturnType = MethodReturnType.Returns string
        then
            Some Assumption.CoreLibResourceLookup
        else
            None
