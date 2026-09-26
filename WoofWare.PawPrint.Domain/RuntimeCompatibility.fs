namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata

/// <summary>
/// What an assembly's <c>[assembly: RuntimeCompatibility]</c> says about objects thrown that do
/// not derive from <c>System.Exception</c>.
/// </summary>
/// <remarks>
/// CoreCLR wraps every such object in a <c>RuntimeWrappedException</c> the moment it is thrown,
/// and before offering it to a <c>catch</c> or <c>filter</c> clause unwraps it again unless the
/// assembly declaring the clause's method wraps non-exception throws
/// (<c>PossiblyUnwrapThrowable</c>, <c>vm/excep.cpp</c>). So a clause in an assembly that wraps
/// sees a <c>RuntimeWrappedException</c>, and a clause in one that does not sees the object itself.
/// C# and Visual Basic always emit the attribute; F# does not.
/// </remarks>
[<RequireQualifiedAccess>]
module RuntimeCompatibility =

    /// The raw token of the single-row AssemblyDefinition table.
    [<Literal>]
    let private AssemblyDefinitionToken = 0x20000001

    /// `Module::UpdateCachedIsRuntimeWrapExceptions` (`vm/ceeload.cpp`): the attribute's void
    /// constructor, then `::ParseKnownCaNamedArgs` (`md/compiler/custattr_emit.cpp`) with a single
    /// descriptor, a boolean named `WrapNonExceptionThrows`. Any failure of that parse leaves the
    /// answer false, even when the flag had already been read as true.
    let private blobWraps (blob : ImmutableArray<byte>) : bool =
        match CustomAttribute.readFixedArgs [] blob with
        | Error _ -> false
        | Ok (_, afterFixed) ->

        // The count is read with `GetI2`, which yields 0 when fewer than two bytes remain, and is
        // signed, so a count with its high bit set reads no arguments.
        let count, afterCount =
            if blob.Length - afterFixed < 2 then
                0, afterFixed
            else
                int (int16 (uint16 blob.[afterFixed] ||| (uint16 blob.[afterFixed + 1] <<< 8))), afterFixed + 2

        let wanted =
            CustomAttribFieldOrPropType.Scalar (CustomAttribSerializationType.Primitive PrimitiveType.Boolean)

        // `value` is `None` until the argument is read: CoreCLR zero-initialises it, so an
        // attribute naming no argument does not wrap. An argument of another name or type, or the
        // same one twice, fails the parse.
        let rec loop (remaining : int) (cursor : int) (value : bool option) : bool =
            if remaining <= 0 then
                value = Some true
            else

            match CustomAttribute.readNamedArgHeader blob cursor with
            | Error _ -> false
            | Ok (header, afterName) ->
                if
                    header.ElemType <> wanted
                    || header.Name <> Some "WrapNonExceptionThrows"
                    || value.IsSome
                    || afterName >= blob.Length
                then
                    false
                else
                    loop (remaining - 1) (afterName + 1) (Some (blob.[afterName] <> 0uy))

        loop count afterCount None

    /// The namespace and name of the type declaring an attribute's constructor, as CoreCLR's
    /// `GetCustomAttributeByName` compares them. `None` for a constructor on an instantiated
    /// generic type, which cannot be the attribute sought.
    let private constructorTypeName
        (assembly : DumpedAssembly)
        (constructor : MetadataToken)
        : (string * string) option
        =
        match constructor with
        | MetadataToken.MemberReference handle ->
            match assembly.Members.[handle].Parent with
            | MetadataToken.TypeReference parent ->
                let typeRef = assembly.TypeRefs.[parent]
                Some (typeRef.Namespace, typeRef.Name)
            | MetadataToken.TypeDefinition parent ->
                let typeDef = assembly.TypeDefs.[parent]
                Some (typeDef.Namespace, typeDef.Name)
            | _ -> None
        | MetadataToken.MethodDef handle ->
            let declaring = assembly.Methods.[handle].RequiredDeclaringType
            Some (declaring.Namespace, declaring.Name)
        | other -> failwith $"A custom attribute in %s{assembly.DefinitionFullName} has constructor %O{other}"

    /// Does a `catch` or `filter` clause in a method of `assembly` see a thrown object that is
    /// not an exception wrapped in a `RuntimeWrappedException`, rather than as itself? Decided, as
    /// CoreCLR decides it, by the first `RuntimeCompatibilityAttribute` applied to the assembly.
    let wrapsNonExceptionThrows (assembly : DumpedAssembly) : bool =
        match assembly.CustomAttributesByParentToken.TryGetValue AssemblyDefinitionToken with
        | false, _ -> false
        | true, tokens ->
            tokens
            |> Seq.map (fun token ->
                match MetadataToken.ofInt token with
                | MetadataToken.CustomAttribute handle -> assembly.Attributes.[handle]
                | other -> failwith $"The custom attributes of %s{assembly.DefinitionFullName} include %O{other}"
            )
            |> Seq.tryFind (fun attribute ->
                constructorTypeName assembly attribute.Constructor = Some (
                    "System.Runtime.CompilerServices",
                    "RuntimeCompatibilityAttribute"
                )
            )
            |> Option.map (fun attribute -> blobWraps attribute.Value)
            |> Option.defaultValue false
