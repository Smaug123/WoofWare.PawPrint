namespace WoofWare.PawPrint.Test

open System.Reflection
open System.Reflection.Emit
open WoofWare.PawPrint

/// One instruction's `DynamicMethod`, and the argument lists to run it on. Everything the method
/// does besides the instruction must be unable to raise, so that whatever the host raises is the
/// instruction's.
type HostProbe =
    {
        /// The instruction's name as `IlOp` spells it.
        Instruction : string
        /// The instruction's `OpcodeFaults` entry.
        Entry : OpcodeFaults
        /// How the host encodes the instruction.
        OpCode : OpCode
        /// What distinguishes this probe from the instruction's others, such as the type its token
        /// names, for reporting.
        Shape : string
        Method : DynamicMethod
        Arguments : obj list list
    }

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

    /// Compares what the host raised for an instruction with its table entry, both ways: a fault
    /// the host raised and the entry omits, and a fault the entry lists that no input raised.
    /// `observed` maps each fault raised to a description of one input that raised it.
    ///
    /// A fault of `FaultKind.ResourceExhaustion` need not have been raised, since no input raises
    /// one reliably, and nor need any fault in `unobservable`.
    let mismatches
        (instruction : string)
        (entry : OpcodeFaults)
        (unobservable : Set<OpcodeFault>)
        (observed : Map<string, string>)
        : string list
        =
        let expected = listed entry

        let mayGoUnseen : Set<string> =
            match entry with
            | OpcodeFaults.Unmodelled -> Set.empty
            | OpcodeFaults.Raises faults ->
                faults
                |> List.filter (fun fault ->
                    OpcodeFault.kind fault = FaultKind.ResourceExhaustion
                    || unobservable.Contains fault
                )
                |> List.map OpcodeFault.typeName
                |> Set.ofList

        [
            for KeyValue (name, witness) in observed do
                if not (expected.Contains name) then
                    $"%s{instruction} raised %s{name}, which the table omits, on %s{witness}"

            for name in expected do
                if not (observed.ContainsKey name) && not (mayGoUnseen.Contains name) then
                    $"%s{instruction} never raised %s{name}, which the table lists"
        ]

    /// Runs every probe on every one of its argument lists, and compares what each instruction
    /// raised across all its probes with its table entry, as `mismatches` does. `unobservable`
    /// names the faults of an instruction that no probe can raise, and why that is not a
    /// mismatch is for the caller to say.
    let check (unobservable : (string * OpcodeFault) list) (probes : HostProbe list) : string list =
        probes
        |> List.groupBy (fun probe -> probe.Instruction)
        |> List.collect (fun (instruction, probes) ->
            let observed =
                [
                    for probe in probes do
                        for arguments in probe.Arguments do
                            match raised probe.Method arguments with
                            | Some name -> name, $"%s{probe.Shape} %A{arguments}"
                            | None -> ()
                ]
                |> List.groupBy fst
                |> List.map (fun (name, witnesses) -> name, snd (List.head witnesses))
                |> Map.ofList

            let unobservable =
                unobservable
                |> List.filter (fun (name, _) -> name = instruction)
                |> List.map snd
                |> Set.ofList

            mismatches instruction (List.head probes).Entry unobservable observed
        )

    /// The probes whose `OpCode` is not the instruction they claim to be, which would check one
    /// instruction's entry against another's behaviour.
    let misencoded (probes : HostProbe list) : string list =
        let spelling (name : string) : string =
            name.Replace("_", "").Replace(".", "").ToLowerInvariant ()

        probes
        |> List.filter (fun probe -> spelling probe.OpCode.Name <> spelling probe.Instruction)
        |> List.map (fun probe -> $"%s{probe.Instruction} is encoded as %s{probe.OpCode.Name}")
        |> List.distinct
