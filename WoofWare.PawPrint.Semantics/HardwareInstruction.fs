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

[<RequireQualifiedAccess>]
module JitTarget =
    /// Every target.
    let all : JitTarget list = [ JitTarget.Arm64 ; JitTarget.X64 ]

    /// The namespace of the instruction-set classes whose instructions the JIT emits when compiling
    /// for `target`. CoreLib declares the other targets' classes too, with bodies that throw.
    let instructionSetNamespace (target : JitTarget) : string =
        match target with
        | JitTarget.Arm64 -> "System.Runtime.Intrinsics.Arm"
        | JitTarget.X64 -> "System.Runtime.Intrinsics.X86"

/// One row of CoreCLR's JIT hardware-intrinsic tables, `HARDWARE_INTRINSIC(...)`: the columns that
/// bear on what an instruction can raise, spelled as the table spells them.
type HardwareIntrinsicRow =
    {
        Target : JitTarget
        /// The table's instruction-set column, such as `AdvSimd_Arm64`.
        InstructionSet : string
        /// The method name, which is how the JIT's `lookupId` finds a row within an instruction set.
        Name : string
        /// The `INS_...` instructions the row emits, for whichever element types it has one;
        /// `INS_invalid` is left out.
        Instructions : Set<string>
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

    /// One line of the checked-in table: target, instruction set, name, instructions, category and
    /// flags, separated by tabs, the instructions and the flags by `|`.
    let format (row : HardwareIntrinsicRow) : string =
        String.Join (
            "\t",
            [|
                targetName row.Target
                row.InstructionSet
                row.Name
                String.Join ("|", Set.toArray row.Instructions)
                row.Category
                String.Join ("|", Set.toArray row.Flags)
            |]
        )

    let private parse (line : string) : HardwareIntrinsicRow =
        let names (field : string) : Set<string> =
            if field = "" then
                Set.empty
            else
                Set.ofArray (field.Split '|')

        match line.Split '\t' with
        | [| target ; instructionSet ; name ; instructions ; category ; flags |] when
            [ target ; instructionSet ; name ; instructions ; category ; flags ]
            |> List.forall (fun field -> not (Seq.exists Char.IsWhiteSpace field))
            ->
            {
                Target =
                    match target with
                    | "Arm64" -> JitTarget.Arm64
                    | "X64" -> JitTarget.X64
                    | other -> failwith $"HardwareIntrinsicTable: unknown target %s{other} in %s{line}"
                InstructionSet = instructionSet
                Name = name
                Instructions = names instructions
                Category = category
                Flags = names flags
            }
        | _ -> failwith $"HardwareIntrinsicTable: malformed line %s{line}"

    /// The rows of a table in the checked-in format, whatever its line endings: a checkout may have
    /// converted them, and a `\r` left on a line would change its last flag's name.
    let ofText (text : string) : HardwareIntrinsicRow list =
        use reader = new StringReader (text)

        let rec lines (acc : string list) : string list =
            match reader.ReadLine () with
            | null -> List.rev acc
            | "" -> lines acc
            | line -> lines (line :: acc)

        lines [] |> List.map parse

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
             ofText (reader.ReadToEnd ()))


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
    /// An x86 integer divide (`div`, `idiv`) by zero, raising `DivideByZeroException`.
    | ZeroDivisor
    /// An x86 integer divide by a divisor other than zero whose quotient does not fit its register,
    /// raising `OverflowException`: the CPU reports it as the same fault as a zero divisor, and
    /// CoreCLR tells the two apart by the divisor (`IsDivByZeroAnIntegerOverflow`,
    /// exceptionhandling.cpp).
    | QuotientOverflow

/// What a hardware instruction can do on a CPU that has it.
[<RequireQualifiedAccess>]
type InstructionContract =
    /// It raises at most these. The empty set is a positive claim that it cannot fault.
    | Raises of Set<InstructionFault>
    /// Not known: the JIT names no instruction set for its class, its table has no row for it, or
    /// it classifies it as a helper or special intrinsic, whose import the JIT writes by hand.
    | Unknown

/// The instruction set CoreCLR's JIT names for an intrinsic class (`Compiler::lookupIsa`), spelled as
/// the first column of its tables spells it.
[<RequireQualifiedAccess>]
type JitInstructionSet =
    /// This one, whatever the CPU.
    | Fixed of string
    /// `ifSupported` on a CPU that supports that instruction set, and `otherwise` on any other.
    | ByCpuSupport of ifSupported : string * otherwise : string

/// What a hardware-intrinsic placeholder's call to itself can raise when the JIT expands it into
/// the instruction, from the JIT's own tables.
[<RequireQualifiedAccess>]
module HardwareInstruction =

    /// The class nested in an instruction set's class, named `nested`, as `lookupIsa` maps the
    /// enclosing class's instruction set to the nested one's; `None` where it maps it to none.
    let private nestedInstructionSet (target : JitTarget) (nested : string) (enclosing : string) : string option =
        match target, nested with
        // `Arm64VersionOfIsa` (hwintrinsicarm64.cpp).
        | JitTarget.Arm64, "Arm64" -> Some (enclosing + "_Arm64")
        // `X64VersionOfIsa` (hwintrinsicxarch.cpp).
        | JitTarget.X64, "X64" ->
            match enclosing with
            | "X86Base"
            | "SSE42"
            | "AVX"
            | "AVX2"
            | "AVX512"
            | "AVX512v2"
            | "AVX512v3"
            | "AVX10v1"
            | "AVX10v2"
            | "AES"
            | "AVX512VP2INTERSECT"
            | "AVXIFMA"
            | "AVXVNNI"
            | "GFNI"
            | "SHA"
            | "WAITPKG"
            | "X86Serialize" -> Some (enclosing + "_X64")
            | "AVXVNNIINT"
            | "AVXVNNIINT_V512" -> Some enclosing
            | _ -> None
        // `VLVersionOfIsa`.
        | JitTarget.X64, "VL" ->
            match enclosing with
            | "AVX512"
            | "AVX512v2"
            | "AVX512v3"
            | "AVX10v1" -> Some enclosing
            | _ -> None
        // `V256VersionOfIsa`.
        | JitTarget.X64, "V256" ->
            match enclosing with
            | "AES"
            | "GFNI" -> Some (enclosing + "_V256")
            | _ -> None
        // `V512VersionOfIsa`.
        | JitTarget.X64, "V512" ->
            match enclosing with
            | "AVX10v1"
            | "AVX10v1_X64"
            | "AVX10v2"
            | "AVX10v2_X64" -> Some enclosing
            | "AES"
            | "GFNI" -> Some (enclosing + "_V512")
            | "AVXVNNIINT"
            | "AVXVNNIINT_V512" -> Some "AVXVNNIINT_V512"
            | _ -> None
        | _ -> None

    /// `lookupInstructionSet`: the instruction set of a class that is not nested, by its name. It
    /// also names the cross-platform `Vector64` to `Vector512`, which are left out here, so a
    /// placeholder of theirs has no contract.
    let private topLevelInstructionSet (target : JitTarget) (ns : string) (name : string) : JitInstructionSet option =
        match target, ns with
        | JitTarget.Arm64, _ when ns = JitTarget.instructionSetNamespace JitTarget.Arm64 ->
            match name with
            | "AdvSimd"
            | "Aes"
            | "ArmBase"
            | "Crc32"
            | "Dp"
            | "Rdm"
            | "Sha1"
            | "Sha256"
            | "Sve"
            | "Sve2" -> Some (JitInstructionSet.Fixed name)
            | _ -> None
        | JitTarget.X64, _ when ns = JitTarget.instructionSetNamespace JitTarget.X64 ->
            match name with
            | "Aes"
            | "Pclmulqdq" -> Some (JitInstructionSet.Fixed "AES")
            | "Avx" -> Some (JitInstructionSet.Fixed "AVX")
            | "Avx10v1"
            | "Avx512Bf16"
            | "Avx512Fp16" -> Some (JitInstructionSet.Fixed "AVX10v1")
            | "Avx10v2" -> Some (JitInstructionSet.Fixed "AVX10v2")
            | "Avx2"
            | "Bmi1"
            | "Bmi2"
            | "F16c"
            | "Fma"
            | "Lzcnt" -> Some (JitInstructionSet.Fixed "AVX2")
            | "Avx512BW"
            | "Avx512CD"
            | "Avx512DQ"
            | "Avx512F" -> Some (JitInstructionSet.Fixed "AVX512")
            | "Avx512Vbmi" -> Some (JitInstructionSet.Fixed "AVX512v2")
            | "Avx512Bitalg"
            | "Avx512Vbmi2"
            | "Avx512Vpopcntdq" -> Some (JitInstructionSet.Fixed "AVX512v3")
            | "Avx512Vp2intersect" -> Some (JitInstructionSet.Fixed "AVX512VP2INTERSECT")
            | "AvxIfma" -> Some (JitInstructionSet.Fixed "AVXIFMA")
            | "AvxVnni" -> Some (JitInstructionSet.Fixed "AVXVNNI")
            | "AvxVnniInt8"
            | "AvxVnniInt16" -> Some (JitInstructionSet.ByCpuSupport ("AVXVNNIINT", "AVXVNNIINT_V512"))
            | "Gfni" -> Some (JitInstructionSet.Fixed "GFNI")
            | "Popcnt"
            | "Sse3"
            | "Sse41"
            | "Sse42"
            | "Ssse3" -> Some (JitInstructionSet.Fixed "SSE42")
            | "Sha" -> Some (JitInstructionSet.Fixed "SHA")
            | "Sse"
            | "Sse2"
            | "X86Base" -> Some (JitInstructionSet.Fixed "X86Base")
            | "WaitPkg" -> Some (JitInstructionSet.Fixed "WAITPKG")
            | "X86Serialize" -> Some (JitInstructionSet.Fixed "X86Serialize")
            | _ -> None
        | _ -> None

    /// The instruction set the JIT names for `intrinsicClass` when compiling for `target`, as its
    /// `lookupIsa` names it; `None` where it names none.
    let instructionSet (target : JitTarget) (intrinsicClass : IntrinsicClass) : JitInstructionSet option =
        let nested (name : string) (enclosing : JitInstructionSet) : JitInstructionSet option =
            match enclosing with
            | JitInstructionSet.Fixed isa -> nestedInstructionSet target name isa |> Option.map JitInstructionSet.Fixed
            | JitInstructionSet.ByCpuSupport (ifSupported, otherwise) ->
                match nestedInstructionSet target name ifSupported, nestedInstructionSet target name otherwise with
                | Some ifSupported, Some otherwise when ifSupported = otherwise ->
                    Some (JitInstructionSet.Fixed ifSupported)
                | Some ifSupported, Some otherwise -> Some (JitInstructionSet.ByCpuSupport (ifSupported, otherwise))
                // On some CPU the JIT names no instruction set.
                | _ -> None

        // The JIT sees a class's name and at most two enclosing ones, so it does not name an
        // instruction set for a class nested deeper.
        match intrinsicClass.Path with
        | [ name ] -> topLevelInstructionSet target intrinsicClass.Namespace name
        | [ enclosing ; name ] ->
            topLevelInstructionSet target intrinsicClass.Namespace enclosing
            |> Option.bind (nested name)
        | [ outer ; inner ; name ] ->
            topLevelInstructionSet target intrinsicClass.Namespace outer
            |> Option.bind (nested inner)
            |> Option.bind (nested name)
        | _ -> None

    /// `lookupId`: the instruction sets whose rows the JIT searches for a method of a class it names
    /// `isa`, in order, taking the first row it finds. AVX10v1 unifies the AVX-512 ones.
    let private searched (target : JitTarget) (isa : string) : string list =
        match target, isa with
        | JitTarget.X64, "AVX10v1" -> [ "AVX512" ; "AVX512v2" ; "AVX512v3" ]
        | JitTarget.X64, "AVX10v1_X64" -> [ "AVX512_X64" ]
        | _ -> [ isa ]

    let private index : Lazy<Map<JitTarget * string * string, HardwareIntrinsicRow>> =
        lazy
            (HardwareIntrinsicTable.rows.Force ()
             |> List.groupBy (fun row -> row.Target, row.InstructionSet, row.Name)
             |> List.map (fun (key, rows) ->
                 match rows with
                 | [ row ] -> key, row
                 // `binarySearchId` finds a method by its name alone within an instruction set.
                 | _ ->
                     failwith $"HardwareIntrinsicTable: %d{rows.Length} rows for %A{key}; the JIT's search assumes one"
             )
             |> Map.ofList)

    /// The rows of the JIT's table from which it may expand the instruction `methodName` of
    /// `intrinsicClass` when compiling for `target`: one for each instruction set it might name for
    /// the class, depending on the CPU. `None` where it names none, or where one it might name has
    /// no row for the method.
    let rows
        (target : JitTarget)
        (intrinsicClass : IntrinsicClass)
        (methodName : string)
        : HardwareIntrinsicRow list option
        =
        let rowOf (isa : string) : HardwareIntrinsicRow option =
            searched target isa
            |> List.tryPick (fun isa -> index.Force().TryFind (target, isa, methodName))

        match instructionSet target intrinsicClass with
        | None -> None
        | Some (JitInstructionSet.Fixed isa) -> rowOf isa |> Option.map List.singleton
        | Some (JitInstructionSet.ByCpuSupport (ifSupported, otherwise)) ->
            match rowOf ifSupported, rowOf otherwise with
            | Some ifSupported, Some otherwise -> Some [ ifSupported ; otherwise ]
            | _ -> None

    /// What one row says the instruction can raise, or `None` for a helper or special intrinsic.
    let private faultsOf (row : HardwareIntrinsicRow) : Set<InstructionFault> option =
        match row.Category with
        | "HW_Category_Helper"
        | "HW_Category_Special" -> None
        | category ->
            let memory =
                category = "HW_Category_MemoryLoad"
                || category = "HW_Category_MemoryStore"
                || (row.Target = JitTarget.X64
                    && (row.Flags.Contains "HW_Flag_MaybeMemoryLoad"
                        || row.Flags.Contains "HW_Flag_MaybeMemoryStore"))

            // `HWIntrinsicInfo::isImmOp` and `addRangeCheckIfNeeded`: on Arm64, the flag alone; on
            // X64, the category, where the immediate is not a full byte's range. Where the operand's
            // bounds admit every value its type holds, it cannot in fact be out of range, which this
            // does not distinguish.
            let immediate =
                match row.Target with
                | JitTarget.Arm64 -> row.Flags.Contains "HW_Flag_HasImmediateOperand"
                | JitTarget.X64 -> category = "HW_Category_IMM" && not (row.Flags.Contains "HW_Flag_FullRangeIMM")

            let divide =
                row.Target = JitTarget.X64
                && (row.Instructions.Contains "INS_div" || row.Instructions.Contains "INS_idiv")

            Some (
                Set.ofList
                    [
                        if memory then
                            InstructionFault.NullAddress
                        if immediate then
                            InstructionFault.ImmediateOutOfRange
                        if divide then
                            InstructionFault.ZeroDivisor
                            InstructionFault.QuotientOverflow
                    ]
            )

    /// What the instruction `methodName` of `intrinsicClass` can raise on a CPU that has it, when
    /// the JIT compiles for `target`: whatever any row it may expand it from says.
    let contract (target : JitTarget) (intrinsicClass : IntrinsicClass) (methodName : string) : InstructionContract =
        match rows target intrinsicClass methodName with
        | None -> InstructionContract.Unknown
        | Some rows ->

        let faults = rows |> List.map faultsOf

        if faults |> List.exists Option.isNone then
            InstructionContract.Unknown
        else
            faults |> List.choose id |> Set.unionMany |> InstructionContract.Raises
