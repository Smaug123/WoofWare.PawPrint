namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
module TestNativeMethodDetection =

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        use stream = File.OpenRead corelibPath
        Assembly.read loggerFactory (Some corelibPath) stream

    let private findMethod (ns : string) (typeName : string) (methodName : string) =
        match corelib.TryGetTopLevelTypeDef ns typeName with
        | None -> failwith $"Type {ns}.{typeName} not found in CoreLib"
        | Some typeInfo ->

        match typeInfo.Methods |> List.filter (fun m -> m.Name = methodName) with
        | [] -> failwith $"Method {ns}.{typeName}.{methodName} not found in CoreLib"
        | [ m ] -> m
        | many -> failwith $"Ambiguous: found {List.length many} overloads of {ns}.{typeName}.{methodName}"

    [<Test>]
    let ``Environment.GetProcessorCount is a native method`` () : unit =
        let m = findMethod "System" "Environment" "GetProcessorCount"
        m.IsNativeMethod |> shouldEqual true
        m.Instructions |> shouldEqual None

    [<Test>]
    let ``Monitor.ReliableEnter is a native method`` () : unit =
        let m = findMethod "System.Threading" "Monitor" "ReliableEnter"
        m.IsNativeMethod |> shouldEqual true
        m.IsCliInternal |> shouldEqual true
        m.Instructions |> shouldEqual None

    [<Test>]
    let ``Object.ToString is not a native method`` () : unit =
        let m = findMethod "System" "Object" "ToString"
        m.IsNativeMethod |> shouldEqual false
        m.Instructions |> Option.isSome |> shouldEqual true

    [<Test>]
    let ``every mocked method in AbstractMachine extern dispatch is native`` () : unit =
        // These are the methods currently intercepted by AbstractMachine.executeOneStep.
        let mockedMethods =
            [
                "System", "Environment", "GetProcessorCount"
                "System", "Environment", "_Exit"
                "System.Threading", "Monitor", "ReliableEnter"
                "System.Threading", "Monitor", "Exit"
            ]

        for (ns, typeName, methodName) in mockedMethods do
            let m = findMethod ns typeName methodName

            if not m.IsNativeMethod then
                failwith
                    $"{ns}.{typeName}.{methodName} is mocked in AbstractMachine but is NOT a native method (ImplAttributes=%O{m.ImplAttributes}, MethodAttributes=%O{m.MethodAttributes}). We should not be intercepting managed IL."
