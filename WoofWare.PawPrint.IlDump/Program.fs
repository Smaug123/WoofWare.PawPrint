namespace WoofWare.PawPrint.IlDump

open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Microsoft.Extensions.Logging
open WoofWare.PawPrint

module Program =

    type private Mode =
        | Default
        | AttrsOnly

    /// Emit attribute-only output for a single type: the type's own attribute
    /// lines (if any), followed by attribute lines for each filter-matching
    /// method, field, property, and event. Members with no attributes are
    /// silently skipped. The type header itself is only emitted if at least
    /// one of those produces output. When this emits any lines and
    /// <paramref name="isFirstType"/> is false, a leading blank line is printed
    /// to separate from the previous type. Returns true iff any lines were
    /// emitted.
    let private printTypeAttrs
        (assembly : DumpedAssembly)
        (memberFilter : string option)
        (isFirstType : bool)
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

        let memberNameMatches : string -> bool = IlDumpRendering.matchesFilter memberFilter

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
            // Separate from the previous type only now that we know there's
            // something to emit — otherwise broad filters that skip multiple
            // types would leave a trail of blank separator lines.
            if not isFirstType then
                printfn ""

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
            let filter =
                match rest with
                | [] -> IlDumpFilter.make None None
                | [ t ] -> IlDumpFilter.make (Some t) None
                | t :: m :: _ -> IlDumpFilter.make (Some t) (Some m)

            use loggerFactory =
                LoggerFactory.Create (fun builder ->
                    builder.SetMinimumLevel(LogLevel.Warning).AddConsole ()
                    |> ignore<ILoggingBuilder>
                )

            let assembly = Assembly.readFile loggerFactory dllPath

            match mode with
            | Mode.Default ->
                let mutable anyEmitted = false

                for kvp in assembly.TypeDefs do
                    let typeInfo = kvp.Value

                    if IlDumpRendering.typeMatches assembly filter typeInfo then
                        let lines = IlDumpRendering.formatTypeLines assembly filter typeInfo

                        if not (List.isEmpty lines) then
                            // Separate from the previous type only now that we know
                            // there's something to emit.
                            if anyEmitted then
                                printfn ""

                            for line in lines do
                                printfn $"%s{line}"

                            anyEmitted <- true

            | Mode.AttrsOnly ->
                let mutable anyEmitted = false

                let printOwnerGroup (header : string) (parent : MetadataToken) : unit =
                    let lines = AttributeFormatting.renderOwnerLines assembly header parent

                    if not (List.isEmpty lines) then
                        if anyEmitted then
                            printfn ""

                        for line in lines do
                            printfn $"%s{line}"

                        anyEmitted <- true

                // Assembly- and module-scoped attributes (e.g. [assembly: InternalsVisibleTo],
                // [module: SkipLocalsInit]) live under the singleton AssemblyDefinition /
                // ModuleDefinition rows, not under any type. Emit them up front when the user
                // hasn't narrowed the scope with a type filter; a type filter scopes the
                // output to types and should suppress these.
                if Option.isNone filter.Type then
                    printOwnerGroup
                        (AttributeFormatting.assemblyHeader assembly)
                        (MetadataToken.AssemblyDefinition EntityHandle.AssemblyDefinition)

                    let moduleName =
                        let mr = assembly.PeReader.GetMetadataReader ()
                        let moduleDef = mr.GetModuleDefinition ()
                        mr.GetString moduleDef.Name

                    printOwnerGroup
                        (AttributeFormatting.moduleHeader moduleName)
                        (MetadataToken.ModuleDefinition EntityHandle.ModuleDefinition)

                for kvp in assembly.TypeDefs do
                    let typeInfo = kvp.Value

                    if IlDumpRendering.typeMatches assembly filter typeInfo then
                        let emitted = printTypeAttrs assembly filter.Member (not anyEmitted) typeInfo

                        if emitted then
                            anyEmitted <- true

            0
