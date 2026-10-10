namespace WoofWare.PawPrint.Test

open System.Reflection
open System.Reflection.Emit
open WoofWare.PawPrint

/// Running one instruction on the host runtime, for the fixtures that check `OpcodeFaults` against
/// it (`TestOpcodeFaultsOnHost*`).
[<RequireQualifiedAccess>]
module HostFaultProbe =

    /// The full name of what the host raises running `method` on `arguments`, if it raises.
    let raised (method : DynamicMethod) (arguments : obj list) : string option =
        try
            method.Invoke ((null : obj), Array.ofList arguments) |> ignore<obj>
            None
        with :? TargetInvocationException as e ->
            Some (e.InnerException.GetType().FullName)

    /// The full names of the faults a table entry lists.
    let listed (entry : OpcodeFaults) : Set<string> =
        match entry with
        | OpcodeFaults.Unmodelled -> failwith "the entry is unmodelled, so the host cannot contradict it"
        | OpcodeFaults.Raises faults -> faults |> List.map OpcodeFault.typeName |> Set.ofList

    /// Every list that takes one element from each of `lists`, in order.
    let rec cartesian (lists : 'a list list) : 'a list list =
        match lists with
        | [] -> [ [] ]
        | first :: rest ->
            let tails = cartesian rest

            [
                for x in first do
                    for tail in tails -> x :: tail
            ]

    /// Compares what the host raised for each instruction with its table entry, both ways: a fault
    /// the host raised and the entry omits, and a fault the entry lists that no input raised.
    /// `observed` maps each fault raised to a description of one input that raised it.
    let mismatches (instruction : string) (entry : OpcodeFaults) (observed : Map<string, string>) : string list =
        let expected = listed entry

        [
            for KeyValue (name, witness) in observed do
                if not (expected.Contains name) then
                    $"%s{instruction} raised %s{name}, which the table omits, on %s{witness}"

            for name in expected do
                if not (observed.ContainsKey name) then
                    $"%s{instruction} never raised %s{name}, which the table lists"
        ]
