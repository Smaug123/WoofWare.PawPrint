namespace WoofWare.PosixKernel.Test

open System
open System.Reflection

/// The names a client could write for the library's types, modules, functions,
/// fields and union cases, read from the assembly by reflection, and whether a
/// client can reach each. Shared by the checks that hold documentation, the
/// docstrings and the README alike, to names that exist.
[<RequireQualifiedAccess>]
module LibraryNames =

    let kindOf (m : MemberInfo) : SourceConstructFlags option =
        m.GetCustomAttributes (typeof<CompilationMappingAttribute>, false)
        |> Seq.cast<CompilationMappingAttribute>
        |> Seq.tryHead
        |> Option.map (fun (a : CompilationMappingAttribute) ->
            a.SourceConstructFlags &&& SourceConstructFlags.KindMask
        )

    let withoutArity (name : string) : string =
        match name.IndexOf '`' with
        | -1 -> name
        | i -> name.Substring (0, i)

    /// The name a member has in F# source, where the compiler gave it another.
    let sourceName (m : MemberInfo) : string =
        m.GetCustomAttributes (typeof<CompilationSourceNameAttribute>, false)
        |> Seq.cast<CompilationSourceNameAttribute>
        |> Seq.tryHead
        |> Option.map (fun (a : CompilationSourceNameAttribute) -> a.SourceName)
        |> Option.defaultValue (withoutArity m.Name)

    /// A type's name as a docstring writes it: a module without the `Module`
    /// suffix the compiler adds when a type shares its name, and a nested type
    /// after its container's name.
    let rec dottedName (t : Type) : string =
        let own =
            let name = withoutArity t.Name

            if kindOf t = Some SourceConstructFlags.Module && name.EndsWith "Module" then
                name.Substring (0, name.Length - "Module".Length)
            else
                name

        if isNull t.DeclaringType then
            own
        else
            $"%s{dottedName t.DeclaringType}.%s{own}"

    let rec reachable (t : Type) : bool =
        if t.IsNested then
            t.IsNestedPublic && reachable t.DeclaringType
        else
            t.IsPublic

    let allMembers : BindingFlags =
        BindingFlags.Public
        ||| BindingFlags.NonPublic
        ||| BindingFlags.Static
        ||| BindingFlags.Instance
        ||| BindingFlags.DeclaredOnly

    /// Every name a docstring could give one of the library's types, modules,
    /// functions, fields or union cases, with whether a client can reach
    /// something of that name: `true` if any member so named is public.
    let namesInSource (assembly : Assembly) : Map<string, bool> =
        let types =
            assembly.GetTypes ()
            |> Array.filter (fun (t : Type) -> not (t.Name.Contains "@") && not (t.FullName.Contains "<"))

        seq {
            for t in types do
                let typeName = dottedName t
                yield typeName, reachable t

                for p in t.GetProperties allMembers do
                    let getter = p.GetGetMethod true

                    if not (isNull getter) then
                        yield $"%s{typeName}.%s{sourceName p}", reachable t && getter.IsPublic

                for m in t.GetMethods allMembers do
                    if not m.IsSpecialName && not (m.Name.Contains "@") then
                        // A union case with fields is made by a method `New<Case>`.
                        let name =
                            if kindOf m = Some SourceConstructFlags.UnionCase && m.Name.StartsWith "New" then
                                m.Name.Substring 3
                            else
                                sourceName m

                        yield $"%s{typeName}.%s{name}", reachable t && m.IsPublic

                for f in t.GetFields allMembers do
                    if f.IsLiteral || (f.IsStatic && not (f.Name.Contains "@")) then
                        yield $"%s{typeName}.%s{f.Name}", reachable t && f.IsPublic
        }
        |> Seq.groupBy fst
        |> Seq.map (fun (name, entries) -> name, Seq.exists snd entries)
        |> Map.ofSeq
