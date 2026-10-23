namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Reflection.Metadata
open System.Runtime.Loader
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `MetadataAccess` checked against the host runtime, over every way of nesting a type up to two
/// deep in another, with every visibility at each level, and every visibility of a method and a
/// field of the innermost type. Each is accessed from four places: a type of its own assembly, a
/// type of another, the innermost type itself, and a type nested in it; by `newarr` of the type and
/// of a public generic over it, `call` of the method and `ldsfld` of the field. Where
/// `MetadataAccess` says an access is `Accessible`, the host must not refuse it.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMetadataAccess =

    let private nestedVisibilities : TypeAttributes list =
        [
            TypeAttributes.NestedPublic
            TypeAttributes.NestedPrivate
            TypeAttributes.NestedFamily
            TypeAttributes.NestedAssembly
            TypeAttributes.NestedFamANDAssem
            TypeAttributes.NestedFamORAssem
        ]

    let private memberVisibilities : (string * MethodAttributes * FieldAttributes) list =
        [
            "Public", MethodAttributes.Public, FieldAttributes.Public
            "Private", MethodAttributes.Private, FieldAttributes.Private
            "Family", MethodAttributes.Family, FieldAttributes.Family
            "Assembly", MethodAttributes.Assembly, FieldAttributes.Assembly
            "FamANDAssem", MethodAttributes.FamANDAssem, FieldAttributes.FamANDAssem
            "FamORAssem", MethodAttributes.FamORAssem, FieldAttributes.FamORAssem
        ]

    /// A chain of visibilities, outermost first: a top-level type's, then each nested type's.
    let private chains : TypeAttributes list list =
        [
            for outer in [ TypeAttributes.Public ; TypeAttributes.NotPublic ] do
                yield [ outer ]

                for nested in nestedVisibilities do
                    yield [ outer ; nested ]

                    for deeper in nestedVisibilities do
                        yield [ outer ; nested ; deeper ]
        ]

    let private innermostName (index : int) : string = $"Chain%d{index}"
    let private insideName (index : int) : string = $"Inside%d{index}"

    /// Where a probe is.
    [<RequireQualifiedAccess>]
    type private Site =
        /// A type of the assembly defining the targets, unrelated to them.
        | Own
        /// A type of another assembly.
        | Other
        /// The innermost type of the chain the probe accesses.
        | Self
        /// A public type nested in that.
        | Inside

    /// What a probe accesses.
    [<RequireQualifiedAccess>]
    type private Accessed =
        | Type
        | OverType
        | Method of visibility : string
        | Field of visibility : string

    type private Probe =
        {
            Chain : int
            Accessed : Accessed
            Site : Site
            Name : string
        }

    let private accesses : Accessed list =
        [
            yield Accessed.Type
            yield Accessed.OverType

            for name, _, _ in memberVisibilities do
                yield Accessed.Method name
                yield Accessed.Field name
        ]

    /// A static method `int32 ()` of `holder` for each access of `inner`'s chain.
    let private defineProbes
        (holder : TypeBuilder)
        (site : Site)
        (chain : int)
        (inner : Type)
        (methods : Map<string, MethodInfo>)
        (fields : Map<string, FieldInfo>)
        (over : Type)
        : Probe list
        =
        [
            for accessed in accesses do
                let name =
                    match accessed with
                    | Accessed.Type -> "Type"
                    | Accessed.OverType -> "OverType"
                    | Accessed.Method visibility -> $"Method%s{visibility}"
                    | Accessed.Field visibility -> $"Field%s{visibility}"
                    |> sprintf "Probe%d%s" chain

                let il =
                    holder
                        .DefineMethod(name, MethodAttributes.Public ||| MethodAttributes.Static, typeof<int32>, [||])
                        .GetILGenerator ()

                match accessed with
                | Accessed.Type ->
                    il.Emit OpCodes.Ldc_I4_1
                    il.Emit (OpCodes.Newarr, inner)
                | Accessed.OverType ->
                    il.Emit OpCodes.Ldc_I4_1
                    il.Emit (OpCodes.Newarr, over.MakeGenericType [| inner |])
                | Accessed.Method visibility -> il.Emit (OpCodes.Call, methods.[visibility])
                | Accessed.Field visibility -> il.Emit (OpCodes.Ldsfld, fields.[visibility])

                il.Emit OpCodes.Pop
                il.Emit OpCodes.Ldc_I4_0
                il.Emit OpCodes.Ret

                yield
                    {
                        Chain = chain
                        Accessed = accessed
                        Site = site
                        Name = name
                    }
        ]

    /// The innermost type of a chain, with a static method and a static field of each member
    /// visibility, by name.
    type private Innermost =
        {
            Type : Type
            Methods : Map<string, MethodInfo>
            Fields : Map<string, FieldInfo>
        }

    /// Each chain built in `modul`, with the probes that are in its innermost type and in a type
    /// nested in that; and a public generic class `Over<T>`.
    let private defineTargets (modul : ModuleBuilder) : Innermost list * Type * Probe list =
        let over = modul.DefineType ("Over", TypeAttributes.Public ||| TypeAttributes.Class)

        over.DefineGenericParameters [| "T" |] |> ignore<GenericTypeParameterBuilder[]>
        over.CreateType () |> ignore<Type>

        let built =
            chains
            |> List.mapi (fun index chain ->
                let builders =
                    (((None : TypeBuilder option), []), List.indexed chain)
                    ||> List.fold (fun (enclosing, built) (depth, visibility) ->
                        let name =
                            if depth = chain.Length - 1 then
                                innermostName index
                            else
                                $"Chain%d{index}Level%d{depth}"

                        let builder =
                            match enclosing with
                            | None -> modul.DefineType (name, visibility ||| TypeAttributes.Class)
                            | Some enclosing -> enclosing.DefineNestedType (name, visibility ||| TypeAttributes.Class)

                        Some builder, builder :: built
                    )
                    |> snd

                let inner = List.head builders

                let members =
                    [
                        for name, methodVisibility, fieldVisibility in memberVisibilities do
                            let method =
                                inner.DefineMethod (
                                    $"M%s{name}",
                                    methodVisibility ||| MethodAttributes.Static,
                                    typeof<int32>,
                                    [||]
                                )

                            let il = method.GetILGenerator ()
                            il.Emit OpCodes.Ldc_I4_0
                            il.Emit OpCodes.Ret

                            let field =
                                inner.DefineField (
                                    $"F%s{name}",
                                    typeof<int32>,
                                    fieldVisibility ||| FieldAttributes.Static
                                )

                            name, (method :> MethodInfo, field :> FieldInfo)
                    ]

                let methods =
                    members |> List.map (fun (name, (method, _)) -> name, method) |> Map.ofList

                let fields =
                    members |> List.map (fun (name, (_, field)) -> name, field) |> Map.ofList

                let inside =
                    inner.DefineNestedType (insideName index, TypeAttributes.NestedPublic ||| TypeAttributes.Class)

                let probes =
                    defineProbes inner Site.Self index inner methods fields over
                    @ defineProbes inside Site.Inside index inner methods fields over

                for builder in inside :: builders |> List.rev do
                    builder.CreateType () |> ignore<Type>

                {
                    Type = inner :> Type
                    Methods = methods
                    Fields = fields
                },
                probes
            )

        List.map fst built, over :> Type, List.collect snd built

    let private save (builder : PersistedAssemblyBuilder) : byte[] =
        use stream = new MemoryStream ()
        builder.Save stream
        stream.ToArray ()

    let private holderOf (site : Site) : string = $"%A{site}Probes"

    /// The images of the assembly defining the targets, and of the other assembly; and every probe.
    let private images : Lazy<byte[] * byte[] * Probe list> =
        lazy
            let targetBuilder =
                PersistedAssemblyBuilder (AssemblyName "AccessTargets", typeof<obj>.Assembly)

            let targetModule = targetBuilder.DefineDynamicModule "AccessTargets"
            let innermost, over, inChains = defineTargets targetModule

            let probesIn (modul : ModuleBuilder) (site : Site) : Probe list =
                let holder =
                    modul.DefineType (holderOf site, TypeAttributes.Public ||| TypeAttributes.Class)

                let probes =
                    innermost
                    |> List.indexed
                    |> List.collect (fun (index, inner) ->
                        defineProbes holder site index inner.Type inner.Methods inner.Fields over
                    )

                holder.CreateType () |> ignore<Type>
                probes

            let own = probesIn targetModule Site.Own

            let callerBuilder =
                PersistedAssemblyBuilder (AssemblyName "AccessCallers", typeof<obj>.Assembly)

            let other = probesIn (callerBuilder.DefineDynamicModule "AccessCallers") Site.Other

            save targetBuilder, save callerBuilder, inChains @ own @ other

    let private key (probe : Probe) : string = $"%A{probe.Site} %s{probe.Name}"

    /// The name of the type holding `probe`.
    let private holderName (probe : Probe) : string =
        match probe.Site with
        | Site.Own
        | Site.Other -> holderOf probe.Site
        | Site.Self -> innermostName probe.Chain
        | Site.Inside -> insideName probe.Chain

    /// Whether the host refuses each probe's access.
    let private refusedOnHost (targetImage : byte[]) (callerImage : byte[]) (probes : Probe list) : Map<string, bool> =
        let context = AssemblyLoadContext ("TestMetadataAccess", true)

        try
            let target = context.LoadFromStream (new MemoryStream (targetImage))
            let caller = context.LoadFromStream (new MemoryStream (callerImage))

            let byName =
                [ target ; caller ]
                |> Seq.collect (fun assembly -> assembly.GetTypes ())
                |> Seq.map (fun ty -> ty.Name, ty)
                |> Map.ofSeq

            probes
            |> List.map (fun probe ->
                let method =
                    byName.[holderName probe].GetMethod (probe.Name, BindingFlags.Public ||| BindingFlags.Static)

                let refused =
                    try
                        method.Invoke ((null : obj), [||]) |> ignore<obj>
                        false
                    with :? TargetInvocationException as e ->
                        match e.InnerException with
                        | :? MethodAccessException
                        | :? FieldAccessException
                        | :? TypeAccessException -> true
                        | other -> failwith $"%s{key probe} raised %O{other}"

                key probe, refused
            )
            |> Map.ofList
        finally
            context.Unload ()

    [<Test>]
    let ``what MetadataAccess calls accessible the host never refuses, and it decides some of each`` () : unit =
        let targetImage, callerImage, probes = images.Force ()
        let refused = refusedOnHost targetImage callerImage probes

        let _, loggerFactory = LoggerFactory.makeTest ()

        let target =
            Assembly.read loggerFactory (Some "AccessTargets.dll") (new MemoryStream (targetImage))

        let caller =
            Assembly.read loggerFactory (Some "AccessCallers.dll") (new MemoryStream (callerImage))

        let assemblies =
            LoadedAssemblies.empty.WithLoadedAssembly(target).WithLoadedAssembly caller

        let definitionIn (assembly : DumpedAssembly) (name : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
            assembly.TypeDefs.Values
            |> Seq.filter (fun ty -> ty.Name = name)
            |> Seq.exactlyOne

        let callerAssembly (probe : Probe) : DumpedAssembly =
            match probe.Site with
            | Site.Other -> caller
            | Site.Own
            | Site.Self
            | Site.Inside -> target

        // A type as the probe's assembly spells it: by definition in the target assembly, by
        // reference from the other.
        let spelled (probe : Probe) (name : string) : TypeDefn =
            match probe.Site with
            | Site.Other ->
                let reference =
                    caller.TypeRefs.Values
                    |> Seq.filter (fun ty -> ty.Name = name)
                    |> Seq.exactlyOne

                TypeDefn.FromReference (reference, SignatureTypeKind.Class)
            | Site.Own
            | Site.Self
            | Site.Inside -> TypeDefn.FromDefinition ((definitionIn target name).Identity, SignatureTypeKind.Class)

        let visibilityOf (name : string) : MemberVisibility =
            let _, methodVisibility, _ =
                memberVisibilities |> List.find (fun (candidate, _, _) -> candidate = name)

            MemberVisibility.ofMethodAttributes methodVisibility

        let decided =
            probes
            |> List.map (fun probe ->
                let innerName = innermostName probe.Chain
                let inner = spelled probe innerName

                let accessTarget =
                    match probe.Accessed with
                    | Accessed.Type -> AccessTarget.Type inner
                    | Accessed.OverType ->
                        AccessTarget.Type (
                            TypeDefn.GenericInstantiation (spelled probe "Over", ImmutableArray.Create inner)
                        )
                    | Accessed.Method visibility
                    | Accessed.Field visibility ->
                        AccessTarget.Member (
                            inner,
                            (definitionIn target innerName).Identity,
                            visibilityOf visibility,
                            []
                        )

                let assembly = callerAssembly probe
                let holder = (definitionIn assembly (holderName probe)).Identity
                probe, MetadataAccess.decide assemblies assembly holder accessTarget
            )

        decided
        |> List.filter (fun (probe, decision) -> decision = AccessDecision.Accessible && refused.[key probe])
        |> List.map (fst >> key)
        |> shouldEqual []

        // Not vacuous: each kind of access is decided from each site.
        for site in [ Site.Own ; Site.Other ; Site.Self ; Site.Inside ] do
            for kind in [ "Type" ; "OverType" ; "Method" ; "Field" ] do
                decided
                |> List.exists (fun (probe, decision) ->
                    probe.Site = site
                    && (sprintf "%A" probe.Accessed).StartsWith kind
                    && decision = AccessDecision.Accessible
                )
                |> shouldEqual true

        // From an unrelated type the host refuses some of what is left undecided, so the probes do
        // reach the check. From within the chain it refuses nothing, and this decides everything.
        for site in [ Site.Own ; Site.Other ] do
            decided
            |> List.exists (fun (probe, decision) ->
                probe.Site = site && decision = AccessDecision.Undecided && refused.[key probe]
            )
            |> shouldEqual true

        decided
        |> List.filter (fun (probe, decision) ->
            (probe.Site = Site.Self || probe.Site = Site.Inside)
            && (decision = AccessDecision.Undecided || refused.[key probe])
        )
        |> List.map (fst >> key)
        |> shouldEqual []
