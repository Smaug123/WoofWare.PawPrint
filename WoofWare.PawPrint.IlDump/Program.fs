namespace WoofWare.PawPrint.IlDump

open System
open System.IO
open System.Reflection.Metadata
open Microsoft.Extensions.Logging
open WoofWare.PawPrint

module Program =

    type private Mode =
        | Default
        | AttrsOnly

    let private printMethod
        (assembly : DumpedAssembly)
        (qualifiedTypeName : string)
        (method : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        : unit
        =
        for line in IlFormatting.formatMethodLines assembly qualifiedTypeName method do
            printfn $"%s{line}"

        printfn ""

    /// Emit attribute-only output for a single type: the type's own attribute
    /// lines (if any), followed by attribute lines for each filter-matching
    /// method, field, property, and event. Members with no attributes are
    /// silently skipped. The type header itself is only emitted if at least
    /// one of those produces output. Returns true iff any lines were emitted.
    let private printTypeAttrs
        (assembly : DumpedAssembly)
        (memberFilter : string option)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : bool
        =
        let qualified = IlFormatting.qualifyTypeName assembly.TypeDefs typeInfo

        let typeHeader = AttributeFormatting.typeHeader assembly typeInfo

        let typeLines =
            AttributeFormatting.renderOwnerLines
                assembly
                typeHeader
                (MetadataToken.TypeDefinition typeInfo.TypeDefHandle)

        let memberNameMatches (name : string) : bool =
            match memberFilter with
            | None -> true
            | Some filter -> name.Contains (filter, StringComparison.OrdinalIgnoreCase)

        let methodGroups =
            typeInfo.Methods
            |> List.filter (fun m -> memberNameMatches m.Name)
            |> List.map (fun m ->
                let header = AttributeFormatting.methodHeader qualified m
                AttributeFormatting.renderOwnerLines assembly header (MetadataToken.MethodDef m.Handle)
            )

        let fieldGroups =
            typeInfo.Fields
            |> List.filter (fun f -> memberNameMatches f.Name)
            |> List.map (fun f ->
                let header = AttributeFormatting.fieldHeader qualified f
                AttributeFormatting.renderOwnerLines assembly header (MetadataToken.FieldDefinition f.Handle)
            )

        // Properties live only in the metadata reader: there is no domain type.
        let propertyGroups =
            let mr = assembly.PeReader.GetMetadataReader ()
            let typeDef = mr.GetTypeDefinition typeInfo.TypeDefHandle

            [
                for propHandle in typeDef.GetProperties () do
                    let prop = mr.GetPropertyDefinition propHandle
                    let name = mr.GetString prop.Name

                    if memberNameMatches name then
                        let header = AttributeFormatting.propertyHeader qualified name
                        let token = MetadataToken.PropertyDefinition propHandle
                        yield AttributeFormatting.renderOwnerLines assembly header token
            ]

        let eventGroups =
            [
                for evt in typeInfo.Events do
                    if memberNameMatches evt.Name then
                        let header = AttributeFormatting.eventHeader qualified evt
                        let token = MetadataToken.EventDefinition evt.Handle
                        yield AttributeFormatting.renderOwnerLines assembly header token
            ]

        let nonEmptyGroups =
            [ typeLines ] @ methodGroups @ fieldGroups @ propertyGroups @ eventGroups
            |> List.filter (not << List.isEmpty)

        if List.isEmpty nonEmptyGroups then
            false
        else
            let mutable first = true

            for group in nonEmptyGroups do
                if not first then
                    printfn ""

                for line in group do
                    printfn $"%s{line}"

                first <- false

            true

    [<EntryPoint>]
    let main (argv : string[]) : int =
        let mode, args =
            match Array.toList argv with
            | "--attrs-only" :: rest -> Mode.AttrsOnly, rest
            | other -> Mode.Default, other

        match args with
        | [] ->
            eprintfn
                "Usage: dotnet run --project WoofWare.PawPrint.IlDump -- [--attrs-only] <dll-path> [TypeName] [MemberName]"

            1
        | dllPath :: rest ->
            let typeFilter, memberFilter =
                match rest with
                | [] -> None, None
                | [ t ] -> Some t, None
                | t :: m :: _ -> Some t, Some m

            use loggerFactory =
                LoggerFactory.Create (fun builder ->
                    builder.SetMinimumLevel(LogLevel.Warning).AddConsole ()
                    |> ignore<ILoggingBuilder>
                )

            let assembly = Assembly.readFile loggerFactory dllPath

            match mode with
            | Mode.Default ->
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
                                match memberFilter with
                                | None -> true
                                | Some filter -> method.Name.Contains (filter, StringComparison.OrdinalIgnoreCase)

                            if methodMatches then
                                printMethod assembly qualifiedName method

            | Mode.AttrsOnly ->
                let mutable anyEmitted = false

                for kvp in assembly.TypeDefs do
                    let typeInfo = kvp.Value
                    let qualifiedName = IlFormatting.qualifyTypeName assembly.TypeDefs typeInfo

                    let typeMatches =
                        match typeFilter with
                        | None -> true
                        | Some filter -> qualifiedName.Contains (filter, StringComparison.OrdinalIgnoreCase)

                    if typeMatches then
                        if anyEmitted then
                            printfn ""

                        let emitted = printTypeAttrs assembly memberFilter typeInfo

                        if emitted then
                            anyEmitted <- true

            0
