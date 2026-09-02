namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// `Concretization.concretizeMethod` takes one argument per generic parameter of the declaring
/// type and one per generic parameter of the method. Anything else must be refused: a longer list
/// would otherwise be silently truncated to a prefix, and a caller that handed a derived type's
/// instantiation to a base type's method would get a plausible-looking wrong instantiation
/// instead of a diagnostic.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestConcretizeMethodArity =

    let private corelibPath : string = typeof<obj>.Assembly.Location

    let private runtimeDir : string = Path.GetDirectoryName corelibPath

    let private readAssembly (path : string) : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory path

    let private loadCompiledLibrary (assemblyName : string) (source : string) : DumpedAssembly =
        let bytes =
            Roslyn.compileAssembly assemblyName OutputKind.DynamicallyLinkedLibrary [] [ source ]

        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = new MemoryStream (bytes)
        AssemblyApi.read loggerFactory None stream

    /// Every arity from 0 to 2, top-level and nested (a nested type's arity includes the
    /// parameters it inherits from its declaring types), plus methods of arity 0 to 2.
    let private source : string =
        """
namespace N;
public class Plain
{
    public static void M() {}
}
public class One<T>
{
    public static void M() {}
    public class InnerZero
    {
        public static void M() {}
    }
    public class InnerOne<U>
    {
        public static void M() {}
    }
}
public class Two<T, U>
{
    public static void M() {}
}
public static class Methods
{
    public static void G0() {}
    public static void G1<T>() {}
    public static void G2<T, U>() {}
}
"""

    type private Fixture =
        {
            Assembly : DumpedAssembly
            BaseTypes : BaseClassTypes<DumpedAssembly>
            Loaded : LoadedAssemblies
            LoadAssembly : IAssemblyLoad
            ConcreteTypes : AllConcreteTypes
            /// Distinct handles, so that a substitution which permutes or repeats its inputs
            /// would be visible in the result.
            Handles : ConcreteTypeHandle list
        }

    let private makeFixture () : Fixture =
        let corelib = readAssembly corelibPath
        let baseTypes = Corelib.getBaseTypes corelib
        let asm = loadCompiledLibrary "ConcretizeMethodArity" source
        let loaded = LoadedAssemblies.ofAssemblies [ corelib ; asm ]

        let _, loggerFactory = LoggerFactory.makeTest ()
        let loadAssembly = TypeResolution.directoryLoader loggerFactory [ runtimeDir ]

        let ctx : TypeConcretization.ConcretizationContext<DumpedAssembly> =
            {
                ConcreteTypes = Corelib.concretizeAll loaded baseTypes AllConcreteTypes.Empty
                LoadedAssemblies = loaded
                BaseTypes = baseTypes
            }

        let primitives =
            [
                PrimitiveType.Int32
                PrimitiveType.String
                PrimitiveType.Boolean
                PrimitiveType.Char
                PrimitiveType.Int64
            ]

        let handles, ctx =
            ((([] : ConcreteTypeHandle list), ctx), primitives)
            ||> List.fold (fun (handles, ctx) prim ->
                let handle, ctx =
                    TypeConcretization.concretizeType
                        ctx
                        loadAssembly
                        corelib.DefinitionFullName
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        (TypeDefn.PrimitiveType prim)

                handle :: handles, ctx
            )

        let handles = List.rev handles
        handles |> List.distinct |> List.length |> shouldEqual handles.Length

        {
            Assembly = asm
            BaseTypes = baseTypes
            Loaded = ctx.LoadedAssemblies
            LoadAssembly = loadAssembly
            ConcreteTypes = ctx.ConcreteTypes
            Handles = handles
        }

    let private args (fixture : Fixture) (count : int) : ImmutableArray<ConcreteTypeHandle> =
        if count > fixture.Handles.Length then
            failwith $"at most %d{fixture.Handles.Length} distinct handles are available"

        fixture.Handles |> List.truncate count |> ImmutableArray.CreateRange

    let private concretize
        (fixture : Fixture)
        (method : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        (typeArgs : ImmutableArray<ConcreteTypeHandle>)
        (methodArgs : ImmutableArray<ConcreteTypeHandle>)
        : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let concretized, _, _ =
            Concretization.concretizeMethod
                fixture.ConcreteTypes
                fixture.LoadAssembly
                fixture.Loaded
                fixture.BaseTypes
                method
                typeArgs
                methodArgs

        concretized

    let private getMethod
        (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (name : string)
        : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        =
        ty.Methods |> List.filter (fun m -> m.Name = name) |> List.exactlyOne

    let private getTopLevel (asm : DumpedAssembly) (name : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        asm.TryGetTopLevelTypeDef "N" name
        |> Option.defaultWith (fun () -> failwith $"Missing type N.%s{name}")

    let private getNested
        (asm : DumpedAssembly)
        (parent : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (name : string)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        asm.TryGetNestedTypeDef parent.TypeDefHandle name
        |> Option.defaultWith (fun () -> failwith $"Missing nested type %s{name} in %s{parent.Name}")

    [<Test>]
    let ``declaring-type arguments must match the declaring type's arity exactly`` () : unit =
        let fixture = makeFixture ()
        let asm = fixture.Assembly
        let one = getTopLevel asm "One`1"

        let generic =
            [
                one, 1
                getNested asm one "InnerZero", 1
                getNested asm one "InnerOne`1", 2
                getTopLevel asm "Two`2", 2
            ]

        for ty, arity in generic do
            ty.Generics.Length |> shouldEqual arity
            let m = getMethod ty "M"

            for count in 0 .. arity + 2 do
                let typeArgs = args fixture count

                if count = arity then
                    let concretized = concretize fixture m typeArgs ImmutableArray.Empty

                    concretized.DeclaringTypeGenerics
                    |> Seq.toList
                    |> shouldEqual (Seq.toList typeArgs)
                else
                    let ex =
                        Assert.Throws<System.Exception> (fun () ->
                            concretize fixture m typeArgs ImmutableArray.Empty |> ignore
                        )

                    Assert.That (ex.Message, Does.Contain ty.Name, $"%s{ty.Name} with %d{count} type argument(s)")
                    Assert.That (ex.Message, Does.Contain $"%d{arity} generic parameter")
                    Assert.That (ex.Message, Does.Contain $"%d{count} type argument")

        // A non-generic declaring type takes no arguments at all.
        let plain = getTopLevel asm "Plain"
        plain.Generics.Length |> shouldEqual 0

        let concretized =
            concretize fixture (getMethod plain "M") ImmutableArray.Empty ImmutableArray.Empty

        concretized.DeclaringTypeGenerics.IsEmpty |> shouldEqual true

    [<Test>]
    let ``method arguments must match the method's arity exactly`` () : unit =
        let fixture = makeFixture ()
        let methods = getTopLevel fixture.Assembly "Methods"

        for name, arity in [ "G0", 0 ; "G1", 1 ; "G2", 2 ] do
            let m = getMethod methods name
            m.Generics.Length |> shouldEqual arity

            for count in 0 .. arity + 2 do
                let methodArgs = args fixture count

                if count = arity then
                    let concretized = concretize fixture m ImmutableArray.Empty methodArgs

                    concretized.Generics |> Seq.toList |> shouldEqual (Seq.toList methodArgs)
                else
                    let ex =
                        Assert.Throws<System.Exception> (fun () ->
                            concretize fixture m ImmutableArray.Empty methodArgs |> ignore
                        )

                    Assert.That (ex.Message, Does.Contain name, $"%s{name} with %d{count} method argument(s)")
                    Assert.That (ex.Message, Does.Contain $"%d{arity} generic parameter")
                    Assert.That (ex.Message, Does.Contain $"%d{count} method argument")
