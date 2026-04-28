namespace WoofWare.PawPrint

open System
open System.Collections.Generic
open System.Reflection.Metadata

[<RequireQualifiedAccess>]
module IlFormatting =
    let qualifyTypeName
        (typeDefs : IReadOnlyDictionary<TypeDefinitionHandle, TypeInfo<GenericParamFromMetadata, TypeDefn>>)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : string
        =
        let rec buildNesting (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>) : string list =
            if ti.DeclaringType.IsNil then
                if String.IsNullOrEmpty ti.Namespace then
                    [ ti.Name ]
                else
                    [ $"%s{ti.Namespace}.%s{ti.Name}" ]
            else
                match typeDefs.TryGetValue ti.DeclaringType with
                | true, parent -> ti.Name :: buildNesting parent
                | false, _ -> [ ti.Name ]

        buildNesting typeInfo |> List.rev |> String.concat "/"

    let private formatMemberSignature (signature : MemberSignature) : string =
        match signature with
        | MemberSignature.Method m ->
            let paramTypes =
                m.ParameterTypes |> List.map (fun p -> $"%O{p}") |> String.concat ", "

            $"(%s{paramTypes}) : %O{m.ReturnType}"
        | MemberSignature.Field f -> $" : %O{f}"

    let rec formatMetadataToken (assembly : DumpedAssembly) (token : MetadataToken) : string =
        match token with
        | MetadataToken.MethodDef handle ->
            match assembly.Methods.TryGetValue handle with
            | true, m ->
                let typeHandle = m.DeclaringType.Definition.Get

                let typeName =
                    match assembly.TypeDefs.TryGetValue typeHandle with
                    | true, td -> qualifyTypeName assembly.TypeDefs td
                    | false, _ -> $"%O{m.DeclaringType}"

                $"%s{typeName}::%s{m.Name}"
            | false, _ -> $"MethodDef(%O{handle})"
        | MetadataToken.MemberReference handle ->
            match assembly.Members.TryGetValue handle with
            | true, m ->
                let parentStr = formatMetadataToken assembly m.Parent
                let sigStr = formatMemberSignature m.Signature
                $"%s{parentStr}::%s{m.PrettyName}%s{sigStr}"
            | false, _ -> $"MemberRef(%O{handle})"
        | MetadataToken.MethodSpecification handle ->
            match assembly.MethodSpecs.TryGetValue handle with
            | true, spec ->
                let args = spec.Signature |> Seq.map (fun t -> $"%O{t}") |> String.concat ", "

                match spec.Method with
                | MetadataToken.MemberReference memberHandle ->
                    match assembly.Members.TryGetValue memberHandle with
                    | true, m ->
                        let parentStr = formatMetadataToken assembly m.Parent
                        let sigStr = formatMemberSignature m.Signature
                        $"%s{parentStr}::%s{m.PrettyName}<%s{args}>%s{sigStr}"
                    | false, _ -> $"MemberRef(%O{memberHandle})<%s{args}>"
                | other ->
                    let methodName = formatMetadataToken assembly other
                    $"%s{methodName}<%s{args}>"
            | false, _ -> $"MethodSpec(%O{handle})"
        | MetadataToken.TypeReference handle ->
            match assembly.TypeRefs.TryGetValue handle with
            | true, tr ->
                let rec qualifyTypeRef (r : TypeRef) : string =
                    match r.ResolutionScope with
                    | TypeRefResolutionScope.TypeRef parentHandle ->
                        match assembly.TypeRefs.TryGetValue parentHandle with
                        | true, parent -> $"%s{qualifyTypeRef parent}/%s{r.Name}"
                        | false, _ -> r.Name
                    | _ ->
                        if String.IsNullOrEmpty r.Namespace then
                            r.Name
                        else
                            $"%s{r.Namespace}.%s{r.Name}"

                qualifyTypeRef tr
            | false, _ -> $"TypeRef(%O{handle})"
        | MetadataToken.TypeDefinition handle ->
            match assembly.TypeDefs.TryGetValue handle with
            | true, td -> qualifyTypeName assembly.TypeDefs td
            | false, _ -> $"TypeDef(%O{handle})"
        | MetadataToken.TypeSpecification handle ->
            match assembly.TypeSpecs.TryGetValue handle with
            | true, ts -> $"%O{ts.Signature}"
            | false, _ -> $"TypeSpec(%O{handle})"
        | MetadataToken.FieldDefinition handle ->
            match assembly.Fields.TryGetValue handle with
            | true, f ->
                let typeHandle = f.DeclaringType.Definition.Get

                let typeName =
                    match assembly.TypeDefs.TryGetValue typeHandle with
                    | true, td -> qualifyTypeName assembly.TypeDefs td
                    | false, _ -> $"%O{f.DeclaringType}"

                $"%s{typeName}::%s{f.Name}"
            | false, _ -> $"FieldDef(%O{handle})"
        | other -> $"%O{other}"

    let escapeStringLiteral (s : string) : string =
        s
            .Replace("\\", "\\\\")
            .Replace("\"", "\\\"")
            .Replace("\n", "\\n")
            .Replace("\r", "\\r")
            .Replace("\t", "\\t")
            .Replace ("\0", "\\0")

    let formatIlOp (assembly : DumpedAssembly) (ilOp : IlOp) (offset : int) : string =
        match ilOp with
        | IlOp.UnaryMetadataToken (op, token) ->
            let tokenStr = formatMetadataToken assembly token.Token
            $"    IL_%04X{offset}: %-20O{op} %s{tokenStr}"
        | IlOp.UnaryStringToken (op, token) ->
            let str = assembly.Strings token.Token |> escapeStringLiteral
            $"    IL_%04X{offset}: %-20O{op} \"%s{str}\""
        | _ -> IlOp.Format ilOp offset

    let formatMethodLines
        (assembly : DumpedAssembly)
        (qualifiedTypeName : string)
        (method : MethodInfo<'typeGenerics, 'methodGenerics, 'methodVars>)
        : string list
        =
        let staticStr = if method.IsStatic then "static " else ""

        let generics =
            if method.Generics.Length = 0 then
                ""
            else
                let gs = method.Generics |> Seq.map (fun g -> $"%O{g}") |> String.concat ", "
                $"<%s{gs}>"

        let paramTypes =
            method.RawSignature.ParameterTypes
            |> List.map (fun p -> $"%O{p}")
            |> String.concat ", "

        let header =
            $"// %s{qualifiedTypeName}::%s{staticStr}%s{method.Name}%s{generics}(%s{paramTypes}) : %O{method.RawSignature.ReturnType}"

        match method.Instructions with
        | None -> [ header ; "  // No IL body (native/internal method)" ]
        | Some instructions ->
            let localLines =
                match instructions.LocalVars with
                | None -> []
                | Some locals when locals.Length = 0 -> []
                | Some locals ->
                    let initStr = if instructions.LocalsInit then " init" else ""

                    [
                        yield $"  .locals%s{initStr}"

                        for i = 0 to locals.Length - 1 do
                            yield $"    [%d{i}] %O{locals.[i]}"
                    ]

            let instructionLines =
                instructions.Instructions
                |> List.map (fun (ilOp, offset) -> formatIlOp assembly ilOp offset)

            [ yield header ; yield! localLines ; yield! instructionLines ]
