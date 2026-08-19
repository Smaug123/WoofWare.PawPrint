namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Zero-initialising a struct requires laying out every one of its fields, and a field's type can
/// live in an assembly that nothing else in the guest has any reason to name. The real runtime
/// loads that assembly as part of building the struct's MethodTable; PawPrint must do the same.
[<TestFixture>]
module TestCrossAssemblyStructZero =

    /// `External` is the *only* thing `ArrCopy.Dep` contributes, and only `ArrCopy.Lib` names it.
    /// A guest that mentions neither still forces the load as soon as it lays out `ArrCopy.Lib.S`.
    let private depSource : string =
        """
namespace ArrCopy.Dep;

public struct External
{
    public int X;
}
"""

    let private libSource : string =
        """
namespace ArrCopy.Lib;

public struct S
{
    public ArrCopy.Dep.External E;
}
"""

    [<Test>]
    let ``newarr of a struct whose field type lives in an unnamed assembly`` () : unit =
        {
            Assemblies =
                [
                    CrossAssemblySpec.library "ArrCopy.Dep" [] [ depSource ]
                    CrossAssemblySpec.library "ArrCopy.Lib" [ "ArrCopy.Dep" ] [ libSource ]
                    CrossAssemblySpec.entryPoint
                        "ArrCopy.Entry"
                        [ "ArrCopy.Lib" ]
                        [
                            """
class Program
{
    static int Main(string[] argv)
    {
        // The entry assembly never names ArrCopy.Dep, so nothing forces it to load. Laying out
        // ArrCopy.Lib.S nevertheless requires it, because S's only field is typed there.
        ArrCopy.Lib.S[] a = new ArrCopy.Lib.S[2];
        return a.Length - 2;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "ArrCopy.Entry"
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTest

    [<Test>]
    let ``newarr of a struct whose field's base class lives in a further unnamed assembly`` () : unit =
        // Laying out `S` reaches `Holder`, and deciding that `Holder` is a reference type means
        // walking its base chain — which leaves `ArrCopy.Dep` for a third assembly that is even
        // further from anything the guest names. Loading a field's *own* assembly is not enough.
        {
            Assemblies =
                [
                    CrossAssemblySpec.library
                        "ArrCopy.Root"
                        []
                        [
                            """
namespace ArrCopy.Root;

public class Base
{
    public int Y;
}
"""
                        ]
                    CrossAssemblySpec.library
                        "ArrCopy.Dep"
                        [ "ArrCopy.Root" ]
                        [
                            """
namespace ArrCopy.Dep;

public class Holder : ArrCopy.Root.Base
{
}
"""
                        ]
                    CrossAssemblySpec.library
                        "ArrCopy.Lib"
                        [ "ArrCopy.Dep" ; "ArrCopy.Root" ]
                        [
                            """
namespace ArrCopy.Lib;

public struct WithRef
{
    public ArrCopy.Dep.Holder H;
}

public static class Check
{
    // Lives here rather than in the entry assembly: reading `H` from there would force the
    // entry assembly to reference ArrCopy.Dep, which is precisely what must not happen.
    public static int AllNull(WithRef[] a)
    {
        foreach (WithRef w in a)
        {
            if (w.H != null) return 1;
        }
        return 0;
    }
}
"""
                        ]
                    CrossAssemblySpec.entryPoint
                        "ArrCopy.Entry"
                        [ "ArrCopy.Lib" ]
                        [
                            """
class Program
{
    static int Main(string[] argv)
    {
        ArrCopy.Lib.WithRef[] a = new ArrCopy.Lib.WithRef[2];
        return ArrCopy.Lib.Check.AllNull(a) + a.Length - 2;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "ArrCopy.Entry"
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTest

    // ------------------------------------------------------------------------------------------
    // The MethodTable projection reaches the same layout walk without going through `newarr`, and
    // `MethodTable::Flags` is what `Type.IsValueType`'s managed body reads (#869). There is no
    // guest route to it for a cross-assembly struct today — `IsReferenceOrContainsReferences<T>`
    // is served by an intrinsic that walks fields itself — so drive the projection directly.

    /// A machine state holding corelib and `ArrCopy.Lib`, but deliberately *not* `ArrCopy.Dep`,
    /// with both test assemblies on disk in a runtime dir so that the production loader can find
    /// `ArrCopy.Dep` if (and only if) something asks it to.
    let private stateWithLibLoaded () : IlMachineState * BaseClassTypes<DumpedAssembly> * ConcreteTypeHandle * string =
        let compiled =
            CrossAssemblyHarness.compileAssemblies
                [
                    CrossAssemblySpec.library "ArrCopy.Dep" [] [ depSource ]
                    CrossAssemblySpec.library "ArrCopy.Lib" [ "ArrCopy.Dep" ] [ libSource ]
                ]

        let tempDir = Path.Combine (Path.GetTempPath (), Path.GetRandomFileName ())
        Directory.CreateDirectory tempDir |> ignore

        for KeyValue (name, bytes) in compiled do
            File.WriteAllBytes (Path.Combine (tempDir, name + ".dll"), bytes)

        // Factory intentionally undisposed: the DumpedAssembly loggers outlive this scope.
        let _, loggerFactory = LoggerFactory.makeTest ()

        let corelibPath = typeof<obj>.Assembly.Location
        let corelib = Assembly.readFile loggerFactory corelibPath
        let bct = Corelib.getBaseTypes corelib

        let lib =
            Assembly.readFile loggerFactory (Path.Combine (tempDir, "ArrCopy.Lib.dll"))

        let runtimeDirs =
            ImmutableArray.CreateRange [ tempDir ; Path.GetDirectoryName corelibPath ]

        let state = IlMachineState.initial loggerFactory runtimeDirs corelib

        let state =
            { state.WithLoadedAssembly lib with
                ConcreteTypes = Corelib.concretizeAll state._LoadedAssemblies bct AllConcreteTypes.Empty
            }

        let sTypeDef =
            lib.TryGetTopLevelTypeDef "ArrCopy.Lib" "S"
            |> Option.defaultWith (fun () -> failwith "ArrCopy.Lib.S not found in the compiled test assembly")

        let state, sHandle =
            IlMachineState.concretizeType
                loggerFactory
                bct
                state
                lib.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (sTypeDef.Identity, SignatureTypeKind.ValueType))

        // Concretizing `S` itself does not touch its fields, so `ArrCopy.Dep` must still be absent.
        // If a future change primes it here, every assertion below would hold for the wrong reason.
        state._LoadedAssemblies.DefinitionNames
        |> Seq.exists (fun (name : string) -> name.StartsWith ("ArrCopy.Dep,", System.StringComparison.Ordinal))
        |> shouldEqual false

        state, bct, sHandle, tempDir

    let private methodTableField (bct : BaseClassTypes<DumpedAssembly>) (name : string) =
        match bct.Corelib.TryGetTopLevelTypeDef "System.Runtime.CompilerServices" "MethodTable" with
        | None -> failwith "System.Runtime.CompilerServices.MethodTable not found in corelib"
        | Some methodTable ->
            methodTable.Fields
            |> List.tryFind (fun field -> field.Name = name)
            |> Option.defaultWith (fun () -> failwith $"MethodTable::%s{name} not found")

    /// `MethodTable::Flags` bit 0x01000000; see `MethodTableProjection`.
    let private containsGcPointersFlag : int32 = 0x01000000

    [<Test>]
    let ``MethodTable Flags for a struct whose field type lives in an unloaded assembly`` () : unit =
        let state, bct, sHandle, tempDir = stateWithLibLoaded ()

        try
            // Factory intentionally undisposed, as above.
            let _, loggerFactory = LoggerFactory.makeTest ()

            let flags =
                match
                    MethodTableProjection.tryProjectField loggerFactory bct (methodTableField bct "Flags") sHandle state
                with
                | Some (CliType.Numeric (CliNumericType.Int32 flags), _) -> flags
                | Some (other, _) -> failwith $"Expected MethodTable::Flags as Int32, got %O{other}"
                | None -> failwith "Expected MethodTable::Flags to project"

            // `S` holds one `int` behind one struct field: no GC pointers anywhere in it.
            flags &&& containsGcPointersFlag |> shouldEqual 0
        finally
            try
                Directory.Delete (tempDir, true)
            with :? IOException ->
                ()

    [<Test>]
    let ``GetNumInstanceFieldBytes for a struct whose field type lives in an unloaded assembly`` () : unit =
        let state, bct, sHandle, tempDir = stateWithLibLoaded ()

        try
            MethodTableProjection.numInstanceFieldBytes bct state sHandle
            |> fst
            |> shouldEqual 4u
        finally
            try
                Directory.Delete (tempDir, true)
            with :? IOException ->
                ()
