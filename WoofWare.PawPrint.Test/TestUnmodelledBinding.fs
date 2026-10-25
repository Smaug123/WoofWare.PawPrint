namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The Loader reports what a guest's metadata names and it cannot bind as a result: a miss it can
/// name (a type absent from its assembly, an assembly no runtime directory supplies), or a shape
/// PawPrint does not implement binding at all, with the diagnostic that used to be a `failwith`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnmodelledBinding =

    let private unmodelledSaying (fragment : string) (unmodelled : UnmodelledBinding) : unit =
        Assert.That (unmodelled.Diagnostic, Does.Contain fragment)

    [<Test>]
    let ``a type reference scoped to a module reference is unmodelled`` () : unit =
        let f = UnmodelledImage.fixture ()
        let target = f.Analysed.TypeRefs.[f.Shapes.ModuleScoped]

        match LoadedTypeResolution.resolveTypeRef f.Assemblies f.Analysed ImmutableArray.Empty target with
        | TypeResolutionResult.Unmodelled unmodelled ->
            unmodelledSaying "ModuleRef type resolution is not yet supported for type N.Inner" unmodelled
        | other -> failwith $"expected Unmodelled, got %O{other}"

        match
            TypeResolution.resolveTypeFromRef
                f.LoggerFactory
                HostImage.runtimeDirs
                f.Analysed
                target
                ImmutableArray.Empty
                f.Assemblies
        with
        | _, Error (TypeBindingFailure.Unmodelled unmodelled) ->
            unmodelledSaying "ModuleRef type resolution is not yet supported for type N.Inner" unmodelled
        | _, other -> failwith $"expected Unmodelled, got %A{other}"

    [<Test>]
    let ``a type reference naming nothing is a named miss`` () : unit =
        let f = UnmodelledImage.fixture ()

        match
            TypeResolution.resolveTypeFromRef
                f.LoggerFactory
                HostImage.runtimeDirs
                f.Analysed
                f.Analysed.TypeRefs.[f.Shapes.Absent]
                ImmutableArray.Empty
                f.Assemblies
        with
        | _, Error (TypeBindingFailure.TypeAbsent (TypeResolutionMiss.TopLevelTypeAbsent (_, Some "System", "Gone"))) ->
            ()
        | _, other -> failwith $"expected TypeAbsent, got %A{other}"

        match
            TypeResolution.resolveTypeFromRef
                f.LoggerFactory
                HostImage.runtimeDirs
                f.Analysed
                f.Analysed.TypeRefs.[f.Shapes.Unbound]
                ImmutableArray.Empty
                f.Assemblies
        with
        | _, Error (TypeBindingFailure.AssemblyUnavailable reference) -> reference.Name.Name |> shouldEqual "Nowhere"
        | _, other -> failwith $"expected AssemblyUnavailable, got %A{other}"

    [<Test>]
    let ``a type exported from another file of the assembly is unmodelled`` () : unit =
        let f = UnmodelledImage.fixture ()

        match
            TypeResolution.tryResolveTypeFromExport
                f.LoggerFactory
                HostImage.runtimeDirs
                f.Analysed
                f.Analysed.ExportedTypes.[f.Shapes.FileExport]
                ImmutableArray.Empty
                f.Assemblies
        with
        | _, ExportedTypeResolution.Unmodelled unmodelled ->
            unmodelledSaying "AssemblyFile exported types are not yet supported" unmodelled
        | _, other -> failwith $"expected Unmodelled, got %A{other}"

    [<Test>]
    let ``a member reference whose parent is a module reference is unmodelled`` () : unit =
        let f = UnmodelledImage.fixture ()

        match
            MemberReferenceParent.resolve
                f.LoggerFactory
                HostImage.runtimeDirs
                f.BaseClassTypes
                f.Assemblies
                f.Analysed
                f.Shapes.ModuleParent
        with
        | _, MemberReferenceParent.Unmodelled unmodelled ->
            unmodelledSaying "names a global member of another module, which is not modelled" unmodelled
        | _, other -> failwith $"expected Unmodelled, got %A{other}"

    [<Test>]
    let ``a field reference ECMA-335 forbids is unmodelled`` () : unit =
        let f = UnmodelledImage.fixture ()

        let resolve (reference : MemberReferenceHandle) =
            FieldReferenceResolution.resolve
                f.LoggerFactory
                HostImage.runtimeDirs
                f.BaseClassTypes
                f.Assemblies
                f.Analysed
                reference
            |> snd

        match resolve f.Shapes.MethodParent with
        | FieldReferenceTarget.Unmodelled unmodelled ->
            unmodelledSaying "a field reference's parent is a MethodDef, which ECMA-335 does not permit" unmodelled
        | other -> failwith $"expected Unmodelled, got %A{other}"

        match resolve f.Shapes.MethodSignature with
        | FieldReferenceTarget.Unmodelled unmodelled -> unmodelledSaying "names a method, not a field" unmodelled
        | other -> failwith $"expected Unmodelled, got %A{other}"

    [<Test>]
    let ``an argument of an instantiation that names nothing is the instantiation's miss`` () : unit =
        let f = UnmodelledImage.fixture ()

        let spelled (handle : TypeReferenceHandle) =
            TypeDefn.FromReference (f.Analysed.TypeRefs.[handle], SignatureTypeKind.Class)

        // The arguments are resolved in order, stopping at the first that fails, so the second
        // argument's unavailable assembly is never looked for.
        for arguments in [ [ f.Shapes.Absent ] ; [ f.Shapes.Absent ; f.Shapes.Unbound ] ] do
            let instantiation =
                TypeDefn.GenericInstantiation (
                    spelled f.Shapes.List,
                    arguments |> List.map spelled |> ImmutableArray.CreateRange
                )

            match
                TypeResolution.resolveTypeFromDefn
                    f.LoggerFactory
                    HostImage.runtimeDirs
                    f.BaseClassTypes
                    instantiation
                    ImmutableArray.Empty
                    ImmutableArray.Empty
                    f.Analysed
                    f.Assemblies
            with
            | _,
              Error (TypeBindingFailure.TypeAbsent (TypeResolutionMiss.TopLevelTypeAbsent (_, Some "System", "Gone"))) ->
                ()
            | _, other -> failwith $"expected System.Gone to be absent, got %A{other}"

    [<Test>]
    let ``a type whose base type is absent is a named miss`` () : unit =
        let f = UnmodelledImage.fixture ()
        let derived = f.Analysed.TypeDefs.[f.Shapes.Derived]

        match
            TypeResolution.resolveTypeFromDefn
                f.LoggerFactory
                HostImage.runtimeDirs
                f.BaseClassTypes
                (TypeDefn.FromDefinition (derived.Identity, SignatureTypeKind.Class))
                ImmutableArray.Empty
                ImmutableArray.Empty
                f.Analysed
                f.Assemblies
        with
        | _,
          Error (TypeBindingFailure.BaseTypeAbsent (TypeResolutionMiss.TopLevelTypeAbsent (_, Some "System", "Gone"))) ->
            ()
        | _, other -> failwith $"expected the base type to be absent, got %A{other}"
