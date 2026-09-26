namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `MethodReferenceResolution` against the real runtime's own answer. For every method-shaped
/// MemberRef row in a set of shared-framework assemblies, `Module.ResolveMethod` on the test host
/// (the same framework files; asserted below) says which MethodDef CoreCLR binds it to, and the
/// resolver must say the same thing.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMethodReferenceResolution =

    /// Chosen for shape rather than size: CoreLib's MemberRefs are almost all instantiations of its
    /// own types, System.Linq and System.Collections call across into CoreLib through generic
    /// instantiations and inherited members, and System.Text.Json and System.Private.Xml are large
    /// ordinary libraries.
    let assemblyNames : string list =
        [
            "System.Private.CoreLib"
            "System.Linq"
            "System.Collections"
            "System.Text.Json"
            "System.Private.Xml"
        ]

    /// Is `mb`, which reflection resolved a MemberRef to, the runtime-supplied array method
    /// `accessor` names?
    let arrayAccessorIs (accessor : ArrayAccessor) (mb : MethodBase) : bool =
        mb.DeclaringType.IsArray
        && match accessor with
           | ArrayAccessor.Get -> mb.Name = "Get"
           | ArrayAccessor.Set -> mb.Name = "Set"
           | ArrayAccessor.Address -> mb.Name = "Address"
           | ArrayAccessor.Constructor arity -> mb.Name = ".ctor" && mb.GetParameters().Length = arity

    /// What the real runtime says a MemberRef binds to.
    [<RequireQualifiedAccess>]
    type private Oracle =
        | Method of MethodBase
        /// `ResolveMethod` threw `MissingMethodException`: nothing of that name and signature is
        /// found where CoreCLR looks.
        | Missing
        /// The reference's parent mentions a type variable, no method's IL uses it to supply a
        /// context, and `object` violates the variable's constraints, so this row says nothing.
        | NoAnswer of exn

    /// A generic context a MemberRef is actually used in: the type and method type parameters of
    /// some method whose IL names it, directly or through a MethodSpec. Those parameters satisfy
    /// whatever constraints the reference's parent places on them, because the compiler checked
    /// exactly that where it emitted the use.
    let private contextsOfUse
        (analysed : DumpedAssembly)
        : System.Collections.Generic.IReadOnlyDictionary<MemberReferenceHandle, MethodDefinitionHandle>
        =
        let referenced (token : MetadataToken) : MemberReferenceHandle option =
            match token with
            | MetadataToken.MemberReference h -> Some h
            | MetadataToken.MethodSpecification h ->
                match analysed.MethodSpecs.[h].Method with
                | MetadataToken.MemberReference h -> Some h
                | _ -> None
            | _ -> None

        seq {
            for KeyValue (user, method) in analysed.Methods do
                match method.Body with
                | MethodBody.Il body ->
                    for op, _ in body.Instructions do
                        match op with
                        | IlOp.UnaryMetadataToken (_, MetadataOperand.FromMetadata t) ->
                            match referenced t.Token with
                            | Some h -> yield h, user
                            | None -> ()
                        | _ -> ()
                | _ -> ()
        }
        |> Seq.distinctBy fst
        |> readOnlyDict

    /// Failing a context of use, `object` for every variable, which loads wherever the parent does
    /// not constrain them.
    let private placeholderArguments : Type[] = Array.create 16 typeof<obj>

    let private askReflection (m : Module) (context : MethodDefinitionHandle option) (token : int) : Oracle =
        let resolve (typeArgs : Type[]) (methodArgs : Type[]) : Oracle =
            try
                Oracle.Method (m.ResolveMethod (token, typeArgs, methodArgs))
            with :? MissingMethodException ->
                Oracle.Missing

        try
            resolve null null
        with
        | :? ArgumentException
        | :? BadImageFormatException
        | :? TypeLoadException as e ->
            let inContext =
                context
                |> Option.bind (fun user ->
                    let user =
                        m.ResolveMethod (
                            MetadataTokens.GetToken (MethodDefinitionHandle.op_Implicit user : EntityHandle)
                        )

                    let methodArgs =
                        if user.IsGenericMethodDefinition then
                            user.GetGenericArguments ()
                        else
                            [||]

                    try
                        Some (resolve (user.DeclaringType.GetGenericArguments ()) methodArgs)
                    with _ ->
                        None
                )

            match inContext with
            | Some answer -> answer
            | None ->
                try
                    resolve placeholderArguments placeholderArguments
                with _ ->
                    Oracle.NoAnswer e

    [<TestCaseSource(nameof assemblyNames)>]
    let ``every method MemberRef resolves where the real runtime binds it`` (assemblyName : string) : unit =
        let frameworkDir = FrameworkUnderTest.sharedFrameworkDirectory ()
        let runtimeDirs = FrameworkUnderTest.runtimeDirs ()
        let _, loggerFactory = LoggerFactory.makeTest ()

        let reflected = Assembly.Load (AssemblyName assemblyName)

        // The oracle is only an oracle if it read the same bytes.
        Path.GetDirectoryName reflected.Location
        |> shouldEqual (Path.GetFullPath frameworkDir)

        let corelib =
            Assembly.readFile loggerFactory (Path.Combine (frameworkDir, "System.Private.CoreLib.dll"))

        let analysed = Assembly.readFile loggerFactory reflected.Location
        let baseClassTypes = Corelib.getBaseTypes corelib
        let loaded = LoadedAssemblies.ofAssemblies [ corelib ; analysed ]

        let mutable ctx : TypeConcretization.ConcretizationContext<DumpedAssembly> =
            {
                ConcreteTypes = Corelib.concretizeAll loaded baseClassTypes AllConcreteTypes.Empty
                LoadedAssemblies = loaded
                BaseTypes = baseClassTypes
            }

        let contexts = contextsOfUse analysed
        let failures = ResizeArray<string> ()
        let mutable agreed = 0
        let mutable noAnswer = 0

        for KeyValue (handle, reference) in analysed.Members do
            match reference.Signature with
            | MemberSignature.Field _ -> ()
            | MemberSignature.Method _ ->
                let token =
                    MetadataTokens.GetToken (MemberReferenceHandle.op_Implicit handle : EntityHandle)

                let describe = $"%s{reference.PrettyName} (0x%08x{token})"

                let ctx', ours =
                    MethodReferenceResolution.resolve
                        loggerFactory
                        runtimeDirs
                        ctx
                        (ctx.LoadedAssemblies.ByDefinitionName analysed.DefinitionFullName)
                        handle

                ctx <- ctx'

                match
                    askReflection
                        reflected.ManifestModule
                        (match contexts.TryGetValue handle with
                         | true, user -> Some user
                         | false, _ -> None)
                        token,
                    ours
                with
                | Oracle.NoAnswer _, _ -> noAnswer <- noAnswer + 1
                | Oracle.Method mb, MethodReferenceTarget.Defined (declaringAssembly, method) ->
                    let theirs = mb.Module.Assembly.FullName, mb.MetadataToken

                    let ours =
                        declaringAssembly.DefinitionFullName,
                        MetadataTokens.GetToken (MethodDefinitionHandle.op_Implicit method : EntityHandle)

                    if theirs = ours then
                        agreed <- agreed + 1
                    else
                        failures.Add
                            $"%s{describe}: runtime binds %s{mb.DeclaringType.FullName}::%s{mb.Name} (%A{theirs}), resolver %A{ours}"
                | Oracle.Method mb, MethodReferenceTarget.ArrayMethod (_, accessor) ->
                    if arrayAccessorIs accessor mb then
                        agreed <- agreed + 1
                    else
                        failures.Add
                            $"%s{describe}: runtime binds %s{mb.DeclaringType.FullName}::%s{mb.Name}, resolver an array's %A{accessor}"
                | Oracle.Missing, MethodReferenceTarget.Missing -> agreed <- agreed + 1
                | oracle, ours -> failures.Add $"%s{describe}: runtime %A{oracle}, resolver %A{ours}"

        TestContext.Progress.WriteLine
            $"%s{assemblyName}: %d{agreed} method MemberRefs agree; %d{noAnswer} the runtime could not be asked about"

        if failures.Count > 0 then
            failures |> Seq.truncate 30 |> String.concat Environment.NewLine |> failwith

        // Vacuity guards: a resolver that is never asked anything agrees with everything, and an
        // oracle that cannot be asked is no oracle.
        agreed |> shouldBeGreaterThan 100
        noAnswer * 100 |> shouldBeSmallerThan agreed
