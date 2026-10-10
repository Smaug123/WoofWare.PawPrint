namespace WoofWare.PawPrint.Analysis

open System.Reflection.Metadata
open WoofWare.PawPrint

/// CoreLib's lookup of its own resource strings, `System.SR.InternalGetResourceString`, which gives
/// every CoreLib exception its message. The escape analysis always takes its contract
/// (`ResourceLookup.raises`) in place of its body, on the conditions `EscapeAnalysis`'s remarks
/// state.
[<RequireQualifiedAccess>]
module internal ResourceLookup =

    let private exceptionName (fullName : string) : ExceptionName =
        match ExceptionName.parse fullName with
        | Some name -> name
        | None -> failwith $"ResourceLookup: %s{fullName} is not an exception type's full name"

    /// The exceptions the lookup can raise.
    let raises : ExceptionName list =
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

    /// Whether `method` of `assembly` is the lookup: `assembly` is a CoreLib, and the method is
    /// `System.SR`'s static `InternalGetResourceString(string) : string`.
    let isLookup (assembly : DumpedAssembly) (method : MethodDefinitionHandle) : bool =
        let definition = assembly.Methods.[method]

        let declaringType =
            assembly.TypeDefs.[definition.RequiredDeclaringType.Definition.Get]

        let string = TypeDefn.PrimitiveType PrimitiveType.String

        assembly.ThisAssemblyDefinition.Name.Name = "System.Private.CoreLib"
        && not declaringType.IsNested
        && declaringType.Generics.IsEmpty
        && declaringType.Namespace = "System"
        && declaringType.Name = "SR"
        && definition.IsStatic
        && definition.Signature.GenericParameterCount = 0
        && definition.Name = "InternalGetResourceString"
        && definition.Signature.ParameterTypes = [ string ]
        && definition.Signature.ReturnType = MethodReturnType.Returns string
