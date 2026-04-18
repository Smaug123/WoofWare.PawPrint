namespace WoofWare.PawPrint.Test

open System
open NUnit.Framework
open FsUnitTyped
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestExceptionHResults =

    /// Construct a real CLR exception of the given type and return its HResult.
    let private getActualHResult (typeName : string) : int =
        let ty = Type.GetType (typeName, throwOnError = true)

        // TypeInitializationException has no default constructor; it requires (string, Exception).
        let exn =
            if typeName = "System.TypeInitializationException" then
                Activator.CreateInstance (ty, [| box "Foo" ; box (null : Exception) |]) :?> Exception
            else
                Activator.CreateInstance ty :?> Exception

        exn.HResult

    let hresultCases : obj array list =
        ExceptionHResults.table |> List.map (fun (name, hr) -> [| box name ; box hr |])

    [<TestCaseSource(nameof hresultCases)>]
    let ``HResult matches real CLR`` (typeName : string) (expectedHResult : int) : unit =
        let actual = getActualHResult typeName
        actual |> shouldEqual expectedHResult

    [<Test>]
    let ``Fallback COR_E_EXCEPTION matches base Exception`` () : unit =
        let actual = (new Exception ()).HResult
        actual |> shouldEqual ExceptionHResults.corEException

    [<Test>]
    let ``Table names are all distinct`` () : unit =
        let names = ExceptionHResults.table |> List.map fst
        names |> List.distinct |> List.length |> shouldEqual (List.length names)

    [<Test>]
    let ``Table names all resolve to distinct CLR types`` () : unit =
        // throwOnError = true ensures every name resolves
        let types =
            ExceptionHResults.table
            |> List.map (fun (name, _) -> name, Type.GetType (name, throwOnError = true))

        // All resolved types are distinct (guards against name collision)
        let typeSet = types |> List.map snd |> List.distinct
        typeSet |> List.length |> shouldEqual (List.length types)
