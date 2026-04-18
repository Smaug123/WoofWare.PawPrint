namespace WoofWare.PawPrint.IlDump

open System
open System.IO
open Microsoft.Extensions.Logging
open WoofWare.PawPrint

module Program =

    let private printMethod
        (typeNamespace : string)
        (typeName : string)
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
            $"// %s{typeNamespace}.%s{typeName}::%s{staticStr}%s{method.Name}%s{generics}(%s{paramTypes}) : %O{method.RawSignature.ReturnType}"

        match method.Instructions with
        | None -> printfn "  // No IL body (native/internal method)"
        | Some instructions ->
            match instructions.LocalVars with
            | None -> ()
            | Some locals when locals.Length = 0 -> ()
            | Some locals ->
                printfn $"  .locals init (%b{instructions.LocalsInit})"

                for i = 0 to locals.Length - 1 do
                    printfn $"    [%d{i}] %O{locals.[i]}"

            for (ilOp, offset) in instructions.Instructions do
                printfn $"%s{IlOp.Format ilOp offset}"

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

            let loggerFactory =
                LoggerFactory.Create (fun builder ->
                    builder.SetMinimumLevel(LogLevel.Warning).AddConsole ()
                    |> ignore<ILoggingBuilder>
                )

            use fileStream = new FileStream (dllPath, FileMode.Open, FileAccess.Read)
            let assembly = Assembly.read loggerFactory (Some dllPath) fileStream

            for kvp in assembly.TypeDefs do
                let typeInfo = kvp.Value

                let typeMatches =
                    match typeFilter with
                    | None -> true
                    | Some filter ->
                        typeInfo.Name.Contains (filter, StringComparison.OrdinalIgnoreCase)
                        || (typeInfo.Namespace + "." + typeInfo.Name)
                            .Contains (filter, StringComparison.OrdinalIgnoreCase)

                if typeMatches then
                    for method in typeInfo.Methods do
                        let methodMatches =
                            match methodFilter with
                            | None -> true
                            | Some filter -> method.Name.Contains (filter, StringComparison.OrdinalIgnoreCase)

                        if methodMatches then
                            printMethod typeInfo.Namespace typeInfo.Name method

            0
