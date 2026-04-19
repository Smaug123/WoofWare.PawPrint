namespace WoofWare.PawPrint.IlDump

open System
open System.Collections.Generic
open System.IO
open System.Reflection.Metadata
open Microsoft.Extensions.Logging
open WoofWare.PawPrint

module Program =

    let rec private formatMetadataToken (assembly : DumpedAssembly) (token : MetadataToken) : string =
        match token with
        | MetadataToken.MethodDef handle ->
            match assembly.Methods.TryGetValue handle with
            | true, m -> $"%O{m}"
            | false, _ -> $"MethodDef(%O{handle})"
        | MetadataToken.MemberReference handle ->
            match assembly.Members.TryGetValue handle with
            | true, m -> m.PrettyName
            | false, _ -> $"MemberRef(%O{handle})"
        | MetadataToken.MethodSpecification handle ->
            match assembly.MethodSpecs.TryGetValue handle with
            | true, spec ->
                let methodName = formatMetadataToken assembly spec.Method
                let args = spec.Signature |> Seq.map (fun t -> $"%O{t}") |> String.concat ", "
                $"%s{methodName}<%s{args}>"
            | false, _ -> $"MethodSpec(%O{handle})"
        | MetadataToken.TypeReference handle ->
            match assembly.TypeRefs.TryGetValue handle with
            | true, tr ->
                if String.IsNullOrEmpty tr.Namespace then
                    tr.Name
                else
                    $"%s{tr.Namespace}.%s{tr.Name}"
            | false, _ -> $"TypeRef(%O{handle})"
        | MetadataToken.TypeDefinition handle ->
            match assembly.TypeDefs.TryGetValue handle with
            | true, td ->
                if String.IsNullOrEmpty td.Namespace then
                    td.Name
                else
                    $"%s{td.Namespace}.%s{td.Name}"
            | false, _ -> $"TypeDef(%O{handle})"
        | MetadataToken.TypeSpecification handle ->
            match assembly.TypeSpecs.TryGetValue handle with
            | true, ts -> $"%O{ts}"
            | false, _ -> $"TypeSpec(%O{handle})"
        | MetadataToken.FieldDefinition handle ->
            match assembly.Fields.TryGetValue handle with
            | true, f -> $"%O{f.DeclaringType}::%s{f.Name}"
            | false, _ -> $"FieldDef(%O{handle})"
        | other -> $"%O{other}"

    let private formatIlOp (assembly : DumpedAssembly) (ilOp : IlOp) (offset : int) : string =
        match ilOp with
        | IlOp.UnaryMetadataToken (op, token) ->
            let tokenStr = formatMetadataToken assembly token
            $"    IL_%04X{offset}: %-20O{op} %s{tokenStr}"
        | IlOp.UnaryStringToken (op, token) ->
            let str = assembly.Strings token
            $"    IL_%04X{offset}: %-20O{op} \"%s{str}\""
        | _ -> IlOp.Format ilOp offset

    let private qualifyTypeName
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

    let private printMethod
        (assembly : DumpedAssembly)
        (qualifiedTypeName : string)
        (method : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        : unit
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

        printfn
            $"// %s{qualifiedTypeName}::%s{staticStr}%s{method.Name}%s{generics}(%s{paramTypes}) : %O{method.RawSignature.ReturnType}"

        match method.Instructions with
        | None -> printfn "  // No IL body (native/internal method)"
        | Some instructions ->
            match instructions.LocalVars with
            | None -> ()
            | Some locals when locals.Length = 0 -> ()
            | Some locals ->
                let initStr = if instructions.LocalsInit then " init" else ""
                printfn $"  .locals%s{initStr}"

                for i = 0 to locals.Length - 1 do
                    printfn $"    [%d{i}] %O{locals.[i]}"

            for (ilOp, offset) in instructions.Instructions do
                printfn $"%s{formatIlOp assembly ilOp offset}"

        printfn ""

    [<EntryPoint>]
    let main (argv : string[]) : int =
        let args = argv |> Array.toList

        match args with
        | [] ->
            eprintfn "Usage: dotnet run --project WoofWare.PawPrint.IlDump -- <dll-path> [TypeName] [MethodName]"
            1
        | dllPath :: rest ->
            let typeFilter, methodFilter =
                match rest with
                | [] -> None, None
                | [ t ] -> Some t, None
                | t :: m :: _ -> Some t, Some m

            use loggerFactory =
                LoggerFactory.Create (fun builder ->
                    builder.SetMinimumLevel(LogLevel.Warning).AddConsole ()
                    |> ignore<ILoggingBuilder>
                )

            use fileStream = new FileStream (dllPath, FileMode.Open, FileAccess.Read)
            let assembly = Assembly.read loggerFactory (Some dllPath) fileStream

            for kvp in assembly.TypeDefs do
                let typeInfo = kvp.Value
                let qualifiedName = qualifyTypeName assembly.TypeDefs typeInfo

                let typeMatches =
                    match typeFilter with
                    | None -> true
                    | Some filter -> qualifiedName.Contains (filter, StringComparison.OrdinalIgnoreCase)

                if typeMatches then
                    for method in typeInfo.Methods do
                        let methodMatches =
                            match methodFilter with
                            | None -> true
                            | Some filter -> method.Name.Contains (filter, StringComparison.OrdinalIgnoreCase)

                        if methodMatches then
                            printMethod assembly qualifiedName method

            0
