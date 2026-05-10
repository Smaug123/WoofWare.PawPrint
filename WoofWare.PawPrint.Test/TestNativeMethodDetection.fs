namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
module TestNativeMethodDetection =

    // Factory intentionally undisposed: corelib.Logger outlives this scope.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private findMethod (ns : string) (typeName : string) (methodName : string) =
        match corelib.TryGetTopLevelTypeDef ns typeName with
        | None -> failwith $"Type {ns}.{typeName} not found in CoreLib"
        | Some typeInfo ->

        match typeInfo.Methods |> List.filter (fun m -> m.Name = methodName) with
        | [] -> failwith $"Method {ns}.{typeName}.{methodName} not found in CoreLib"
        | [ m ] -> m
        | many -> failwith $"Ambiguous: found {List.length many} overloads of {ns}.{typeName}.{methodName}"

    let private findPInvokeByEntryPoint (ns : string) (typeName : string) (entryPoint : string) =
        match corelib.TryGetTopLevelTypeDef ns typeName with
        | None -> failwith $"Type {ns}.{typeName} not found in CoreLib"
        | Some typeInfo ->

        match
            typeInfo.Methods
            |> List.filter (fun m ->
                match m.NativeImport with
                | Some import -> import.ModuleName = "QCall" && import.EntryPointName = entryPoint
                | None -> false
            )
        with
        | [] -> failwith $"QCall entry point {entryPoint} not found on {ns}.{typeName}"
        | [ m ] -> m
        | many -> failwith $"Ambiguous: found {List.length many} QCall entry points {entryPoint} on {ns}.{typeName}"

    let private isNativeBody (body : MethodBody<_>) : bool =
        match body with
        | MethodBody.InternalCall
        | MethodBody.PInvoke -> true
        | MethodBody.Il _
        | MethodBody.RuntimeProvided _
        | MethodBody.Abstract -> false

    [<Test>]
    let ``Environment.GetProcessorCount is a native method`` () : unit =
        let m = findMethod "System" "Environment" "GetProcessorCount"
        isNativeBody m.Body |> shouldEqual true

    [<Test>]
    let ``Monitor.TryEnter_FastPath is a native method`` () : unit =
        // .NET 10 split the old ReliableEnter primitive into TryEnter_FastPath / Enter_Slowpath /
        // TryEnter_Slowpath. TryEnter_FastPath remains an InternalCall and is the analogous primitive.
        let m =
            corelib.TryGetTopLevelTypeDef "System.Threading" "Monitor"
            |> Option.defaultWith (fun () -> failwith "Type System.Threading.Monitor not found in CoreLib")
            |> fun typeInfo ->
                typeInfo.Methods
                |> List.filter (fun m -> m.Name = "TryEnter_FastPath" && List.length m.Signature.ParameterTypes = 1)
                |> function
                    | [ m ] -> m
                    | [] -> failwith "Method System.Threading.Monitor.TryEnter_FastPath(obj) not found in CoreLib"
                    | many ->
                        failwith
                            $"Ambiguous: found {List.length many} overloads of System.Threading.Monitor.TryEnter_FastPath with 1 parameter"

        match m.Body with
        | MethodBody.InternalCall -> ()
        | other -> failwith $"Expected Monitor.TryEnter_FastPath to be InternalCall, got %O{other}"

    [<Test>]
    let ``generated QCall stubs expose native entry point metadata`` () : unit =
        let rva =
            findPInvokeByEntryPoint "System" "RuntimeFieldHandle" "RuntimeFieldHandle_GetRVAFieldInfo"

        match rva.Body with
        | MethodBody.PInvoke -> ()
        | other -> failwith $"Expected RuntimeFieldHandle_GetRVAFieldInfo to be PInvoke, got %O{other}"

        let sizeOf =
            findPInvokeByEntryPoint "System.Runtime.InteropServices" "Marshal" "MarshalNative_SizeOfHelper"

        match sizeOf.Body with
        | MethodBody.PInvoke -> ()
        | other -> failwith $"Expected MarshalNative_SizeOfHelper to be PInvoke, got %O{other}"

    [<Test>]
    let ``Object.ToString is not a native method`` () : unit =
        let m = findMethod "System" "Object" "ToString"
        isNativeBody m.Body |> shouldEqual false

        match m.Body with
        | MethodBody.Il _ -> ()
        | other -> failwith $"Expected Object.ToString to have an IL body, got %O{other}"

    [<Test>]
    let ``every extern-dispatched method in AbstractMachine is native`` () : unit =
        // These are the methods currently intercepted by AbstractMachine.executeOneStep.
        let externDispatchedMethods =
            [
                "System", "Environment", "GetProcessorCount"
                "System", "Environment", "get_CurrentManagedThreadId"
                "System", "Environment", "_Exit"
            ]

        for (ns, typeName, methodName) in externDispatchedMethods do
            let m = findMethod ns typeName methodName

            if not (isNativeBody m.Body) then
                failwith
                    $"{ns}.{typeName}.{methodName} is extern-dispatched in AbstractMachine but is NOT a native method (Body=%O{m.Body}). We should not be intercepting managed IL."
