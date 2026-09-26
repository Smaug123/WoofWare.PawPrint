namespace WoofWare.PosixKernel.Test

open System
open System.IO
open System.Reflection
open WoofWare.PosixKernel

/// What one `socket(2)` call answered on a real kernel.
[<RequireQualifiedAccess>]
type MeasuredSocketAnswer =
    /// A socket was created, and reported these back.
    | Created of soType : int * soProtocol : int option * nonBlocking : bool * closeOnExec : bool
    /// The call failed with this raw errno.
    | Failed of errno : int

/// The sweeps in `socketSyscall/`, taken by
/// `docs/plans/2026-08-23-posix-kernel-extraction/socket-arguments.c`, whose
/// header describes the format.
[<RequireQualifiedAccess>]
module SocketSweep =

    let private assy = Assembly.GetExecutingAssembly ()

    let private parseAnswer (text : string) : MeasuredSocketAnswer =
        if text.StartsWith ("E ", StringComparison.Ordinal) then
            // `E NAME(number)`.
            let opening = text.IndexOf '('
            let closing = text.IndexOf ')'
            MeasuredSocketAnswer.Failed (Int32.Parse (text.Substring (opening + 1, closing - opening - 1)))
        elif text.StartsWith ("OK ", StringComparison.Ordinal) then
            let fields =
                text.Substring(3).Split ' '
                |> Array.map (fun field ->
                    match field.Split '=' with
                    | [| key ; value |] -> key, value
                    | _ -> failwith $"malformed field %s{field} in %s{text}"
                )
                |> Map.ofArray

            let flag (key : string) : bool =
                match fields.[key] with
                | "0" -> false
                | "1" -> true
                | other -> failwith $"%s{key}=%s{other} in %s{text} is neither 0 nor 1"

            MeasuredSocketAnswer.Created (
                Int32.Parse fields.["type"],
                (match fields.["protocol"] with
                 | "-" -> None
                 | value -> Some (Int32.Parse value)),
                flag "nonblock",
                flag "cloexec"
            )
        else
            failwith $"unrecognised answer %s{text}"

    /// Every (domain, type word, protocol) the sweep in `file` asked about, and
    /// what the kernel answered, in the order the sweep asked.
    let load (file : string) : ((int * int * int) * MeasuredSocketAnswer) list =
        let resource = $"WoofWare.PosixKernel.Test.socketSyscall.%s{file}"

        use stream =
            match assy.GetManifestResourceStream resource with
            | null -> failwith $"embedded resource %s{resource} is missing"
            | stream -> stream

        use reader = new StreamReader (stream)

        let lines =
            reader.ReadToEnd().Split '\n'
            |> Array.filter (fun line ->
                not (String.IsNullOrWhiteSpace line)
                && not (line.StartsWith ("#", StringComparison.Ordinal))
            )

        let mutable types : int[] = [||]
        let mutable protocols : int[] = [||]
        let rows = ResizeArray ()

        for line in lines do
            match line.Split '\t' with
            | [| "@types" ; list |] -> types <- list.Split ',' |> Array.map (fun t -> Convert.ToInt32 (t, 16))
            | [| "@protocols" ; list |] -> protocols <- list.Split ',' |> Array.map Int32.Parse
            | [| domain ; firstType ; lastType ; runs |] ->
                let domain = Int32.Parse domain
                let firstIndex = Array.IndexOf (types, Convert.ToInt32 (firstType, 16))
                let lastIndex = Array.IndexOf (types, Convert.ToInt32 (lastType, 16), firstIndex)

                if firstIndex < 0 || lastIndex < 0 then
                    failwith $"%s{resource}: type run %s{firstType}..%s{lastType} is not in the @types list"

                let answers =
                    runs.Split ';'
                    |> Array.map (fun run ->
                        let equals = run.IndexOf '='
                        let bounds = run.Substring(0, equals).Split ".."
                        Int32.Parse bounds.[0], Int32.Parse bounds.[1], parseAnswer (run.Substring (equals + 1))
                    )

                for typeIndex in firstIndex..lastIndex do
                    let mutable position = 0

                    for first, last, answer in answers do
                        if protocols.[position] <> first then
                            failwith
                                $"%s{resource}: a run starts at %d{first} where the list has %d{protocols.[position]}"

                        let mutable stop = false

                        while not stop do
                            rows.Add ((domain, types.[typeIndex], protocols.[position]), answer)
                            stop <- protocols.[position] = last
                            position <- position + 1

                    if position <> protocols.Length then
                        failwith
                            $"%s{resource}: the runs for domain %d{domain} cover %d{position} of %d{protocols.Length} protocols"
            | _ -> failwith $"%s{resource}: malformed line %s{line}"

        List.ofSeq rows
