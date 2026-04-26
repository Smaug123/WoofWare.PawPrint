namespace WoofWare.PawPrint.IlDump

open System
open System.IO
open Microsoft.Extensions.Logging
open WoofWare.PawPrint

module Program =

    let private printMethod
        (assembly : DumpedAssembly)
        (qualifiedTypeName : string)
        (method : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        : unit
        =
        for line in IlFormatting.formatMethodLines assembly qualifiedTypeName method do
            printfn $"%s{line}"

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
                let qualifiedName = IlFormatting.qualifyTypeName assembly.TypeDefs typeInfo

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
