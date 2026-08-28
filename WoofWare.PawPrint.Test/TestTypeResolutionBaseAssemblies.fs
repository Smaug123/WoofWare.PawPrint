namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// <summary>
/// Regression coverage for the invariant that <c>TypeResolution</c>'s loading-capable resolvers
/// hand back only <c>TypeInfo</c>s whose entire base-type chain is already loaded.
/// </summary>
/// <remarks>
/// <para>
/// The pure walks over a type's base chain — <c>DumpedAssembly.isValueType</c>,
/// <c>signatureTypeKind</c>, <c>typeInfoToTypeDefn</c> — take a <c>LoadedAssemblies</c> and have
/// no way to load one; they fail hard if a base-type TypeRef along the chain names an assembly
/// nobody has loaded. <c>TypeResolution</c> is where the load capability lives, so it is where
/// that precondition has to be discharged.
/// </para>
/// <para>
/// The real-world trigger (issue: `sprintf "%d" 3` crashing with "seems pretty unlikely that we
/// could have constructed this object without loading its base type") is
/// <c>substituteGenericsInTypeDefn</c> calling <c>typeInfoToTypeDefn</c> on a freshly resolved
/// FSharp.Core type. FSharp.Core names <c>System.Object</c> through a TypeRef scoped to the
/// <c>netstandard</c> facade, and the `newobj` of <c>PrintfFormat`4</c> is the first thing in
/// that program to touch FSharp.Core at all.
/// </para>
/// </remarks>
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestTypeResolutionBaseAssemblies =

    let private readAssembly (path : string) : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory path

    let private topLevelTypeDef
        (assy : DumpedAssembly)
        (ns : string)
        (name : string)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        assy.TryGetTopLevelTypeDef ns name
        |> Option.defaultWith (fun () ->
            failwith $"Expected %s{ns}.%s{name} in netstandard2.1 FSharp.Core; nuget layout may have changed"
        )

    /// corelib + netstandard2.1 FSharp.Core, deliberately without the `netstandard` facade that
    /// FSharp.Core's base-type TypeRefs point at. Also asserts that the scenario is still live:
    /// the pure walk over an FSharp.Core type must fail in this state, or the tests below would
    /// pass vacuously.
    let private setUp () : DumpedAssembly * BaseClassTypes<DumpedAssembly> * LoadedAssemblies * string list =
        let corelib = readAssembly Netstandard21FSharpCore.corelibPath
        let fsharpCore = readAssembly Netstandard21FSharpCore.path.Value
        let baseTypes = Corelib.getBaseTypes corelib
        Netstandard21FSharpCore.assertNetstandardAvailable ()

        let loaded = LoadedAssemblies.ofAssemblies [ corelib ; fsharpCore ]

        Netstandard21FSharpCore.isLoaded loaded.DefinitionNames |> shouldEqual false

        let walkBeforeResolution =
            try
                DumpedAssembly.isValueType baseTypes loaded (topLevelTypeDef fsharpCore "Microsoft.FSharp.Core" "Unit")
                |> Ok
            with e ->
                Error e.Message

        match walkBeforeResolution with
        | Ok _ ->
            Assert.Fail
                "Expected the base-chain walk over Microsoft.FSharp.Core.Unit to fail before resolution primed it; this test no longer exercises the bug."
        | Error msg -> msg |> shouldContainText "seems pretty unlikely"

        fsharpCore, baseTypes, loaded, [ Netstandard21FSharpCore.runtimeDir ]

    /// `FSharpFunc<int, string>`, spelled as the metadata would.
    let private fsharpFuncIntString (fsharpCore : DumpedAssembly) : TypeDefn =
        let fsharpFunc = topLevelTypeDef fsharpCore "Microsoft.FSharp.Core" "FSharpFunc`2"

        TypeDefn.GenericInstantiation (
            TypeDefn.FromDefinition (fsharpFunc.Identity, SignatureTypeKind.Class),
            ImmutableArray.CreateRange
                [
                    TypeDefn.PrimitiveType PrimitiveType.Int32
                    TypeDefn.PrimitiveType PrimitiveType.String
                ]
        )

    [<Test>]
    let ``resolveTypeFromDefn primes the base chain of the TypeInfo it returns`` () : unit =
        let fsharpCore, baseTypes, loaded, runtimeDirs = setUp ()
        let _, loggerFactory = LoggerFactory.makeTest ()

        let unit = topLevelTypeDef fsharpCore "Microsoft.FSharp.Core" "Unit"

        let loadedAfter, _resolvedIn, resolved =
            TypeResolution.resolveTypeFromDefn
                loggerFactory
                runtimeDirs
                baseTypes
                (TypeDefn.FromDefinition (unit.Identity, SignatureTypeKind.Class))
                ImmutableArray.Empty
                ImmutableArray.Empty
                fsharpCore
                loaded

        Netstandard21FSharpCore.isLoaded loadedAfter.DefinitionNames |> shouldEqual true

        // The point of the invariant: a caller may now run the pure walk on what it was handed.
        resolved.Name |> shouldEqual "Unit"
        DumpedAssembly.isValueType baseTypes loadedAfter resolved |> shouldEqual false

    [<Test>]
    let ``resolveTypeFromDefn handles a generic instantiation whose argument needs a facade`` () : unit =
        // This is the `sprintf "%d" 3` shape: PrintfFormat<int -> string, unit, string, string>,
        // reached by `newobj` before anything else has touched FSharp.Core. Resolving it walks
        // each generic argument through substituteGenericsInTypeDefn, which converts the
        // resolved argument back to a TypeDefn — and that conversion is the base-chain walk.
        let fsharpCore, baseTypes, loaded, runtimeDirs = setUp ()
        let _, loggerFactory = LoggerFactory.makeTest ()

        let printfFormat =
            topLevelTypeDef fsharpCore "Microsoft.FSharp.Core" "PrintfFormat`4"

        let unit = topLevelTypeDef fsharpCore "Microsoft.FSharp.Core" "Unit"

        let printfFormatOfSprintf : TypeDefn =
            TypeDefn.GenericInstantiation (
                TypeDefn.FromDefinition (printfFormat.Identity, SignatureTypeKind.Class),
                ImmutableArray.CreateRange
                    [
                        fsharpFuncIntString fsharpCore
                        TypeDefn.FromDefinition (unit.Identity, SignatureTypeKind.Class)
                        TypeDefn.PrimitiveType PrimitiveType.String
                        TypeDefn.PrimitiveType PrimitiveType.String
                    ]
            )

        let loadedAfter, _resolvedIn, resolved =
            TypeResolution.resolveTypeFromDefn
                loggerFactory
                runtimeDirs
                baseTypes
                printfFormatOfSprintf
                ImmutableArray.Empty
                ImmutableArray.Empty
                fsharpCore
                loaded

        Netstandard21FSharpCore.isLoaded loadedAfter.DefinitionNames |> shouldEqual true

        resolved.Name |> shouldEqual "PrintfFormat`4"
        DumpedAssembly.isValueType baseTypes loadedAfter resolved |> shouldEqual false

    [<Test>]
    let ``resolveTypeFromRef primes the base chain of the TypeInfo it returns`` () : unit =
        // The same invariant reached through a TypeRef rather than a TypeDefinition. It has to be
        // a TypeRef from a *third* assembly into FSharp.Core: a TypeRef whose own scope is the
        // missing facade would drag netstandard in just by being resolved, which would make the
        // assertion below pass for a reason that has nothing to do with base chains.
        let fsharpCore, baseTypes, loaded, runtimeDirs = setUp ()
        let _, loggerFactory = LoggerFactory.makeTest ()

        let holderAssembly : DumpedAssembly =
            let fsharpCoreRef =
                MetadataReference.CreateFromFile Netstandard21FSharpCore.path.Value :> MetadataReference

            let source =
                """
public class Holder
{
    public Microsoft.FSharp.Core.FSharpFunc<int, string> Field;
}
"""

            let bytes =
                Roslyn.compileAssembly
                    "TypeRefBaseChainRegression"
                    OutputKind.DynamicallyLinkedLibrary
                    [ fsharpCoreRef ]
                    [ source ]

            let _, loggerFactory = LoggerFactory.makeTest ()
            use stream = new MemoryStream (bytes)
            AssemblyApi.read loggerFactory None stream

        let loaded = loaded.WithLoadedAssembly holderAssembly

        let fsharpFuncRef =
            holderAssembly.TypeRefs.Values
            |> Seq.filter (fun r -> r.Namespace = "Microsoft.FSharp.Core" && r.Name = "FSharpFunc`2")
            |> Seq.tryHead
            |> Option.defaultWith (fun () ->
                failwith "Expected a TypeRef to Microsoft.FSharp.Core.FSharpFunc`2 in the compiled Holder assembly"
            )

        let loadedAfter, _resolvedIn, resolved =
            TypeResolution.resolveTypeFromRef
                loggerFactory
                runtimeDirs
                holderAssembly
                fsharpFuncRef
                ImmutableArray.Empty
                loaded

        Netstandard21FSharpCore.isLoaded loadedAfter.DefinitionNames |> shouldEqual true

        resolved.Name |> shouldEqual "FSharpFunc`2"
        DumpedAssembly.isValueType baseTypes loadedAfter resolved |> shouldEqual false

    /// FSharp.Core loaded as the entry assembly beside corelib, with the `netstandard` facade its
    /// base-type TypeRefs point at still unloaded -- `setUp`'s state, as an `IlMachineState`.
    let private machineStateWithUnloadedFacade
        ()
        : Microsoft.Extensions.Logging.ILoggerFactory * DumpedAssembly * BaseClassTypes<DumpedAssembly> * IlMachineState
        =
        let fsharpCore, baseTypes, _loaded, runtimeDirs = setUp ()
        let _, loggerFactory = LoggerFactory.makeTest ()

        let corelib = readAssembly Netstandard21FSharpCore.corelibPath

        let state =
            IlMachineState.initial loggerFactory (ImmutableArray.CreateRange runtimeDirs) fsharpCore

        let state = state.WithLoadedAssembly corelib

        Netstandard21FSharpCore.isLoaded state._LoadedAssemblies.DefinitionNames
        |> shouldEqual false

        loggerFactory, fsharpCore, baseTypes, state

    [<Test>]
    let ``the typical declaring type of a MethodDef needs no facade to be loaded`` () : unit =
        // `ModuleHandle.ResolveMethod` reaches this for any MethodDef token, including one whose
        // declaring type's base chain runs through an assembly nothing has loaded yet. There is no
        // loading capability at hand, so the answer has to be derivable without a base-chain walk.
        let loggerFactory, fsharpCore, baseTypes, state = machineStateWithUnloadedFacade ()

        let unit = topLevelTypeDef fsharpCore "Microsoft.FSharp.Core" "Unit"

        let method =
            fsharpCore.Methods.Values
            |> Seq.tryFind (fun method ->
                match method.Owner.TryDeclaringType with
                | Some declaringType -> declaringType.Identity = unit.Identity
                | None -> false
            )
            |> Option.defaultWith (fun () -> failwith "expected Microsoft.FSharp.Core.Unit to declare a method")

        let state, target =
            NativeRuntimeTypeHelpers.typicalDeclaringTypeTarget loggerFactory baseTypes fsharpCore method state

        match target with
        | RuntimeTypeHandleTarget.Closed handle ->
            AllConcreteTypes.lookup handle state.ConcreteTypes
            |> Option.defaultWith (fun () -> failwith "declaring type was not registered in ConcreteTypes")
            |> fun concreteType -> concreteType.Identity |> shouldEqual unit.Identity
        | other -> failwithf "expected the closed non-generic type Unit, got %A" other

    [<Test>]
    let ``the typical declaring type of a method on a generic type needs no facade either`` () : unit =
        let loggerFactory, fsharpCore, baseTypes, state = machineStateWithUnloadedFacade ()

        let fsharpFunc = topLevelTypeDef fsharpCore "Microsoft.FSharp.Core" "FSharpFunc`2"

        let method =
            fsharpCore.Methods.Values
            |> Seq.tryFind (fun method ->
                match method.Owner.TryDeclaringType with
                | Some declaringType -> declaringType.Identity = fsharpFunc.Identity
                | None -> false
            )
            |> Option.defaultWith (fun () -> failwith "expected FSharpFunc`2 to declare a method")

        let _state, target =
            NativeRuntimeTypeHelpers.typicalDeclaringTypeTarget loggerFactory baseTypes fsharpCore method state

        match target with
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition definition -> definition |> shouldEqual fsharpFunc.Identity
        | other -> failwithf "expected the open generic definition of FSharpFunc`2, got %A" other
