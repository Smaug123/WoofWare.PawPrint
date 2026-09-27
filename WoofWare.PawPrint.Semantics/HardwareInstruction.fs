namespace WoofWare.PawPrint

open System
open System.IO

/// The architecture CoreCLR's JIT compiles for, which decides which of its hardware-intrinsic
/// tables applies: `hwintrinsiclistarm64.h` with `hwintrinsiclistarm64sve.h`, or
/// `hwintrinsiclistxarch.h`.
[<RequireQualifiedAccess>]
type JitTarget =
    | Arm64
    | X64

/// One row of CoreCLR's JIT hardware-intrinsic tables, `HARDWARE_INTRINSIC(...)`: the columns that
/// bear on what an instruction can raise, spelled as the table spells them.
type HardwareIntrinsicRow =
    {
        Target : JitTarget
        /// The table's instruction-set column, such as `AdvSimd_Arm64`.
        InstructionSet : string
        /// The method name, which is how the JIT's `lookupId` finds a row within an instruction set.
        Name : string
        /// `HW_Category_...`.
        Category : string
        /// The `HW_Flag_...` names the row sets.
        Flags : Set<string>
    }

/// The JIT's hardware-intrinsic tables, as checked in beside this file (`HardwareIntrinsicTable.tsv`,
/// embedded). A test regenerates them from the pinned runtime source and fails on any difference.
[<RequireQualifiedAccess>]
module HardwareIntrinsicTable =

    let private targetName (target : JitTarget) : string =
        match target with
        | JitTarget.Arm64 -> "Arm64"
        | JitTarget.X64 -> "X64"

    /// One line of the checked-in table: target, instruction set, name, category and flags,
    /// separated by tabs, the flags by `|`.
    let format (row : HardwareIntrinsicRow) : string =
        String.Join (
            "\t",
            [|
                targetName row.Target
                row.InstructionSet
                row.Name
                row.Category
                String.Join ("|", Set.toArray row.Flags)
            |]
        )

    let private parse (line : string) : HardwareIntrinsicRow =
        match line.Split '\t' with
        | [| target ; instructionSet ; name ; category ; flags |] ->
            {
                Target =
                    match target with
                    | "Arm64" -> JitTarget.Arm64
                    | "X64" -> JitTarget.X64
                    | other -> failwith $"HardwareIntrinsicTable: unknown target %s{other} in %s{line}"
                InstructionSet = instructionSet
                Name = name
                Category = category
                Flags =
                    if flags = "" then
                        Set.empty
                    else
                        Set.ofArray (flags.Split '|')
            }
        | _ -> failwith $"HardwareIntrinsicTable: malformed line %s{line}"

    /// Every row, in the order the checked-in table lists them.
    let rows : Lazy<HardwareIntrinsicRow list> =
        lazy
            (let assembly = typeof<HardwareIntrinsicRow>.Assembly
             let name = "WoofWare.PawPrint.Semantics.HardwareIntrinsicTable.tsv"

             use stream =
                 match assembly.GetManifestResourceStream name with
                 | null -> failwith $"HardwareIntrinsicTable: %s{assembly.FullName} embeds no %s{name}"
                 | stream -> stream

             use reader = new StreamReader (stream)

             reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
             |> Seq.map parse
             |> List.ofSeq)

/// An exception a hardware instruction can raise on a CPU that has it. Unrecoverable failures, such
/// as an access violation at an address that is neither null nor valid, are out of scope, as for
/// `OpcodeFaults`.
[<RequireQualifiedAccess>]
type InstructionFault =
    /// The instruction reads or writes memory at an address taken from its operands (a pointer, or a
    /// vector of addresses), and a null address raises `NullReferenceException`.
    | NullAddress
    /// An operand the instruction encodes as an immediate is outside the range it encodes, which
    /// the JIT checks (`addRangeCheckIfNeeded`, hwintrinsic.cpp), raising
    /// `ArgumentOutOfRangeException`.
    | ImmediateOutOfRange

/// What a hardware instruction can do on a CPU that has it.
[<RequireQualifiedAccess>]
type InstructionContract =
    /// It raises at most these. The empty set is a positive claim that it cannot fault.
    | Raises of Set<InstructionFault>
    /// Not known: the JIT's table has no row for it, or classifies it as a helper or special
    /// intrinsic, whose import the JIT writes by hand.
    | Unknown

/// What a hardware-intrinsic placeholder's call to itself can raise when the JIT expands it into
/// the instruction, from the JIT's own tables.
[<RequireQualifiedAccess>]
module HardwareInstruction =

    /// The JIT's name for the instruction set of `intrinsicClass`, as its `lookupIsa`
    /// (hwintrinsicarm64.cpp) names it when compiling for `target`; `None` where it names none.
    /// Only Arm64's is transcribed: on X64 this is `None` for every class.
    let instructionSet (target : JitTarget) (intrinsicClass : IntrinsicClass) : string option =
        match target with
        | JitTarget.X64 -> None
        | JitTarget.Arm64 ->
            // `lookupInstructionSet`: the classes of `System.Runtime.Intrinsics.Arm`, by name. It also
            // names `Vector64` and `Vector128`, which are left out here, so a placeholder of theirs
            // has no contract.
            let topLevel (name : string) : string option =
                match intrinsicClass.Namespace, name with
                | "System.Runtime.Intrinsics.Arm",
                  ("AdvSimd" | "Aes" | "ArmBase" | "Crc32" | "Dp" | "Rdm" | "Sha1" | "Sha256" | "Sve" | "Sve2") ->
                    Some name
                | _ -> None

            match intrinsicClass.Path with
            | [ name ] -> topLevel name
            // `Arm64VersionOfIsa`: the class nested in an instruction set's, for its 64-bit-only
            // instructions.
            | [ enclosing ; "Arm64" ] -> topLevel enclosing |> Option.map (fun isa -> isa + "_Arm64")
            | _ -> None

    let private index : Lazy<Map<JitTarget * string * string, HardwareIntrinsicRow list>> =
        lazy
            (HardwareIntrinsicTable.rows.Force ()
             |> List.groupBy (fun row -> row.Target, row.InstructionSet, row.Name)
             |> Map.ofList)

    /// What one row says the instruction can raise, or `None` for a helper or special intrinsic.
    let private faultsOf (row : HardwareIntrinsicRow) : Set<InstructionFault> option =
        match row.Category with
        | "HW_Category_Helper"
        | "HW_Category_Special" -> None
        | category ->
            let memory =
                category = "HW_Category_MemoryLoad" || category = "HW_Category_MemoryStore"

            // `HWIntrinsicInfo::HasImmediateOperand`: on Arm64, the flag alone. Where the operand's
            // bounds admit every value its type holds, it cannot in fact be out of range, which this
            // does not distinguish.
            let immediate =
                match row.Target with
                | JitTarget.Arm64 -> row.Flags.Contains "HW_Flag_HasImmediateOperand"
                | JitTarget.X64 ->
                    failwith
                        $"HardwareInstruction: an X64 row (%s{row.InstructionSet}.%s{row.Name}) was consulted, but instructionSet names no X64 class"

            Some (
                Set.ofList
                    [
                        if memory then
                            InstructionFault.NullAddress
                        if immediate then
                            InstructionFault.ImmediateOutOfRange
                    ]
            )

    /// What the instruction `methodName` of `intrinsicClass` can raise on a CPU that has it, when
    /// the JIT compiles for `target`: whatever any row of the JIT's table for it says.
    let contract (target : JitTarget) (intrinsicClass : IntrinsicClass) (methodName : string) : InstructionContract =
        match instructionSet target intrinsicClass with
        | None -> InstructionContract.Unknown
        | Some isa ->

        match index.Force().TryFind (target, isa, methodName) with
        | None -> InstructionContract.Unknown
        | Some rows ->

        let faults = rows |> List.map faultsOf

        if faults |> List.exists Option.isNone then
            InstructionContract.Unknown
        else
            faults |> List.choose id |> Set.unionMany |> InstructionContract.Raises
