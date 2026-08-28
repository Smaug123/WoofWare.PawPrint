namespace WoofWare.PawPrint.Test

open FsUnitTyped
open Microsoft.FSharp.Reflection
open NUnit.Framework
open WoofWare.PawPrint

/// Tests for the `OpcodeFaults` table.
///
/// Totality of each per-opcode match is enforced by the compiler, so these tests are aimed at the
/// two things it cannot see: that the escape hatch stays closed, and that entries mean what their
/// names say.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestOpcodeFaults =

    /// Enumerate a discriminated union's nullary cases. Every case of `NullaryIlOp` is nullary, so
    /// this is exhaustive for it; `UnaryMetadataTokenIlOp` and `UnaryStringTokenIlOp` are the same.
    let private allCasesOf<'a> () : 'a list =
        FSharpType.GetUnionCases typeof<'a>
        |> Array.toList
        |> List.map (fun case -> FSharpValue.MakeUnion (case, [||]) :?> 'a)

    let private nameOf (x : 'a) : string =
        let case, _ = FSharpValue.GetUnionFields (x, typeof<'a>)
        case.Name

    // ---------- The escape hatch stays closed ----------

    /// `Unmodelled` means "may raise anything", so every entry that carries it costs an analysis
    /// all precision at that instruction, and an entry added to it silently is a table that
    /// quietly stopped saying anything. There is exactly one, and it is `rethrow`, whose answer
    /// genuinely is not a fact about the opcode.
    ///
    /// Written as an equality against the whole set rather than as a per-case assertion, so that
    /// *adding* an unmodelled entry fails here as loudly as removing one — which is the direction
    /// that matters, since the way to make an awkward opcode compile is to reach for `Unmodelled`.
    [<Test>]
    let ``exactly one nullary opcode is unmodelled`` () : unit =
        allCasesOf<NullaryIlOp> ()
        |> List.filter (fun op -> OpcodeFaults.ofNullary op = OpcodeFaults.Unmodelled)
        |> List.map nameOf
        |> shouldEqual [ "Rethrow" ]

    [<Test>]
    let ``no unary-metadata opcode is unmodelled`` () : unit =
        allCasesOf<UnaryMetadataTokenIlOp> ()
        |> List.filter (fun op -> OpcodeFaults.ofUnaryMetadata op = OpcodeFaults.Unmodelled)
        |> List.map nameOf
        |> shouldEqual []

    [<Test>]
    let ``no string-token opcode is unmodelled`` () : unit =
        allCasesOf<UnaryStringTokenIlOp> ()
        |> List.filter (fun op -> OpcodeFaults.ofUnaryStringToken op = OpcodeFaults.Unmodelled)
        |> List.map nameOf
        |> shouldEqual []

    /// `ldstr` looks free, and is on every execution but the first: interning means the literal has
    /// no object until one is made for it, and making it allocates. ECMA-335 III.4.15 says
    /// "Exceptions: None", which describes the steady state rather than first materialisation, so
    /// this entry is one the specification alone would get wrong.
    [<Test>]
    let ``ldstr can fail to allocate its literal`` () : unit =
        OpcodeFaults.ofUnaryStringToken UnaryStringTokenIlOp.Ldstr
        |> shouldEqual (OpcodeFaults.Raises [ OpcodeFault.OutOfMemory ])

    /// `refanyval` checks the requested type against the one the `TypedRef` carries and can fail
    /// that check; `mkrefany` only packages an address and a handle, and performs no check at all.
    /// The two are easy to treat as mirror images, and ECMA-335 III.4.28 and III.4.16 are explicit
    /// that they are not.
    [<Test>]
    let ``refanyval checks its type and mkrefany does not`` () : unit =
        OpcodeFaults.ofUnaryMetadata UnaryMetadataTokenIlOp.Refanyval
        |> shouldEqual (OpcodeFaults.Raises [ OpcodeFault.InvalidCast ])

        OpcodeFaults.ofUnaryMetadata UnaryMetadataTokenIlOp.Mkrefany
        |> shouldEqual (OpcodeFaults.Raises [])

    /// A duplicate entry would make `Raises` a bag where the type says it is a set, and would make
    /// two tables that mean the same thing compare unequal.
    [<Test>]
    let ``no nullary entry lists a fault twice`` () : unit =
        for op in allCasesOf<NullaryIlOp> () do
            match OpcodeFaults.ofNullary op with
            | OpcodeFaults.Unmodelled -> ()
            | OpcodeFaults.Raises xs -> xs |> List.distinct |> shouldEqual xs

    [<Test>]
    let ``no unary-metadata entry lists a fault twice`` () : unit =
        for op in allCasesOf<UnaryMetadataTokenIlOp> () do
            match OpcodeFaults.ofUnaryMetadata op with
            | OpcodeFaults.Unmodelled -> ()
            | OpcodeFaults.Raises xs -> xs |> List.distinct |> shouldEqual xs

    // ---------- `mayRaise` ----------

    /// The safety property the whole table rests on: an unclassified instruction must never be
    /// readable as harmless, whichever fault is asked about.
    [<Test>]
    let ``Unmodelled may raise every fault`` () : unit =
        for fault in allCasesOf<OpcodeFault> () do
            OpcodeFaults.mayRaise fault OpcodeFaults.Unmodelled |> shouldEqual true

    [<Test>]
    let ``an empty Raises may raise nothing`` () : unit =
        for fault in allCasesOf<OpcodeFault> () do
            OpcodeFaults.mayRaise fault (OpcodeFaults.Raises []) |> shouldEqual false

    [<Test>]
    let ``mayRaise finds a listed fault and not an unlisted one`` () : unit =
        let faults = OpcodeFaults.ofNullary NullaryIlOp.LdLen
        OpcodeFaults.mayRaise OpcodeFault.NullReference faults |> shouldEqual true
        OpcodeFaults.mayRaise OpcodeFault.IndexOutOfRange faults |> shouldEqual false

    // ---------- Entries mean what their names say ----------

    /// The signed forms have an unrepresentable quotient at MinValue / -1; the unsigned forms do
    /// not. Asserted as a pair, because the two differing in exactly one fault is the whole claim
    /// and testing either alone would not show it.
    [<Test>]
    let ``signed division overflows where unsigned division does not`` () : unit =
        OpcodeFaults.ofNullary NullaryIlOp.Div
        |> shouldEqual (OpcodeFaults.Raises [ OpcodeFault.DivideByZero ; OpcodeFault.Overflow ])

        OpcodeFaults.ofNullary NullaryIlOp.Div_un
        |> shouldEqual (OpcodeFaults.Raises [ OpcodeFault.DivideByZero ])

        OpcodeFaults.ofNullary NullaryIlOp.Rem
        |> shouldEqual (OpcodeFaults.Raises [ OpcodeFault.DivideByZero ; OpcodeFault.Overflow ])

        OpcodeFaults.ofNullary NullaryIlOp.Rem_un
        |> shouldEqual (OpcodeFaults.Raises [ OpcodeFault.DivideByZero ])

    /// Every checked conversion can overflow, and no unchecked one can. The two families are named
    /// alike and are easy to move between by accident.
    [<Test>]
    let ``checked conversions overflow and unchecked ones do not`` () : unit =
        let overflows, rest =
            allCasesOf<NullaryIlOp> ()
            |> List.filter (fun op -> (nameOf op).StartsWith ("Conv_", System.StringComparison.Ordinal))
            |> List.partition (fun op -> (nameOf op).StartsWith ("Conv_ovf", System.StringComparison.Ordinal))

        overflows |> List.isEmpty |> shouldEqual false
        rest |> List.isEmpty |> shouldEqual false

        for op in overflows do
            OpcodeFaults.ofNullary op
            |> shouldEqual (OpcodeFaults.Raises [ OpcodeFault.Overflow ])

        for op in rest do
            OpcodeFaults.ofNullary op |> shouldEqual (OpcodeFaults.Raises [])

    /// A store into an array takes the covariance check that a load does not, and `ldelema` takes
    /// it too: it hands out a writable address, so letting it through would defeat the check
    /// `stelem` makes.
    [<Test>]
    let ``array stores take the covariance check and loads do not`` () : unit =
        OpcodeFaults.ofNullary NullaryIlOp.Ldelem_ref
        |> OpcodeFaults.mayRaise OpcodeFault.ArrayTypeMismatch
        |> shouldEqual false

        OpcodeFaults.ofNullary NullaryIlOp.Stelem_ref
        |> OpcodeFaults.mayRaise OpcodeFault.ArrayTypeMismatch
        |> shouldEqual true

        OpcodeFaults.ofUnaryMetadata UnaryMetadataTokenIlOp.Ldelem
        |> OpcodeFaults.mayRaise OpcodeFault.ArrayTypeMismatch
        |> shouldEqual false

        OpcodeFaults.ofUnaryMetadata UnaryMetadataTokenIlOp.Ldelema
        |> OpcodeFaults.mayRaise OpcodeFault.ArrayTypeMismatch
        |> shouldEqual true

    /// A static-field access can surface a failed `.cctor`; an instance-field access cannot,
    /// having no `.cctor` to run.
    /// Every field-accessing instruction carries the `.cctor`, including the three that take a
    /// receiver: ECMA-335 III.4.10, III.4.11 and III.4.28 each permit their token to name a
    /// *static* field, and each says so twice over — once in the description ("can be either an
    /// instance field ... or a static field") and once by conditioning the null check on it
    /// ("thrown if obj is null *and the field is not static*"). A static-field access triggers the
    /// initializer whichever instruction performs it (I.8.9.5), so the receiver is what
    /// distinguishes these six, and the `.cctor` is not.
    ///
    /// PawPrint's own interpreter refuses a static target on the receiver-taking three
    /// (`UnaryMetadataFieldOps.checkFieldStaticness`), so no guest can reach that fault and no
    /// guest test can pin this. That is precisely why it is asserted here: the table's consumers
    /// include analyses over assemblies PawPrint has never run.
    [<Test>]
    let ``every field access carries the cctor, and only the receiver-taking ones dereference`` () : unit =
        for op in
            [
                UnaryMetadataTokenIlOp.Ldsfld
                UnaryMetadataTokenIlOp.Ldsflda
                UnaryMetadataTokenIlOp.Stsfld
            ] do
            OpcodeFaults.ofUnaryMetadata op
            |> shouldEqual (OpcodeFaults.Raises [ OpcodeFault.StackOverflow ; OpcodeFault.TypeInitialization ])

        for op in
            [
                UnaryMetadataTokenIlOp.Ldfld
                UnaryMetadataTokenIlOp.Ldflda
                UnaryMetadataTokenIlOp.Stfld
            ] do
            OpcodeFaults.ofUnaryMetadata op
            |> shouldEqual (
                OpcodeFaults.Raises
                    [
                        OpcodeFault.NullReference
                        OpcodeFault.StackOverflow
                        OpcodeFault.TypeInitialization
                    ]
            )

        // `ldtoken` can name a field without accessing it, so nothing is initialized. This is the
        // arm that stops the rule above being read as "anything naming a field".
        OpcodeFaults.ofUnaryMetadata UnaryMetadataTokenIlOp.Ldtoken
        |> shouldEqual (OpcodeFaults.Raises [])

    /// What a *target* raises is the call graph's business, not this table's — but two things an
    /// invoking instruction does are its own, and neither travels by the call edge.
    ///
    /// Every invoking instruction can trigger the declaring type's `.cctor` (ECMA-335 I.8.9.5),
    /// which is a different method from the named callee: an analyser following only the target
    /// would miss it. And `callvirt` and `calli` dereference something first — a receiver and a
    /// function pointer — where `call` and `jmp` name their target by token and have nothing to
    /// fault on.
    ///
    /// `calli`'s null entry is the one to be careful with: ECMA-335 III.3.20 lists only
    /// `SecurityException` against it, so reading the specification alone gives the wrong answer
    /// here. What pins it is `TestPureCases`' "calli through a null function pointer throws
    /// NullReferenceException", which is PawPrint's own deliberate divergence from CoreCLR.
    [<Test>]
    let ``invoking instructions carry the cctor, and a dereference where they have one`` () : unit =
        OpcodeFaults.ofUnaryMetadata UnaryMetadataTokenIlOp.Call
        |> shouldEqual (OpcodeFaults.Raises [ OpcodeFault.StackOverflow ; OpcodeFault.TypeInitialization ])

        // `callvirt` and `calli` both dereference, but only `callvirt` allocates: under
        // `constrained.`, a value type that inherits the target from `Object`/`ValueType`/`Enum`
        // has its receiver boxed (ECMA-335 III.2.1). `calli` is the arm that keeps that from being
        // read as "every indirect invocation allocates".
        OpcodeFaults.ofUnaryMetadata UnaryMetadataTokenIlOp.Callvirt
        |> shouldEqual (
            OpcodeFaults.Raises
                [
                    OpcodeFault.NullReference
                    OpcodeFault.OutOfMemory
                    OpcodeFault.StackOverflow
                    OpcodeFault.TypeInitialization
                ]
        )

        OpcodeFaults.ofUnaryMetadata UnaryMetadataTokenIlOp.Calli
        |> shouldEqual (
            OpcodeFaults.Raises
                [
                    OpcodeFault.NullReference
                    OpcodeFault.StackOverflow
                    OpcodeFault.TypeInitialization
                ]
        )

        OpcodeFaults.ofUnaryMetadata UnaryMetadataTokenIlOp.Newobj
        |> shouldEqual (
            OpcodeFaults.Raises
                [
                    OpcodeFault.OutOfMemory
                    OpcodeFault.StackOverflow
                    OpcodeFault.TypeInitialization
                ]
        )

        // `jmp` stacks no frame of its own — it replaces the current activation rather than
        // pushing one (ECMA-335 III.3.37) — but the `.cctor` it can trigger is a frame, which is
        // why it carries `StackOverflow` all the same.
        OpcodeFaults.ofUnaryMetadata UnaryMetadataTokenIlOp.Jmp
        |> shouldEqual (OpcodeFaults.Raises [ OpcodeFault.StackOverflow ; OpcodeFault.TypeInitialization ])

        // `ldftn` names a method without invoking it, so no `.cctor` runs. This is the arm that
        // stops the rule above being read as "anything naming a method".
        OpcodeFaults.ofUnaryMetadata UnaryMetadataTokenIlOp.Ldftn
        |> shouldEqual (OpcodeFaults.Raises [])

    /// Running an initializer means entering a frame, so anything that can raise
    /// `TypeInitialization` can also exhaust the stack. Checked over the whole table rather than
    /// per entry: this is the property that keeps a future entry from carrying one without the
    /// other, which is how the two got out of step in the first place.
    [<Test>]
    let ``anything that can run a cctor can also overflow the stack`` () : unit =
        let entries : (string * OpcodeFaults) list =
            [
                yield!
                    allCasesOf<NullaryIlOp> ()
                    |> List.map (fun op -> $"%O{op}", OpcodeFaults.ofNullary op)
                yield!
                    allCasesOf<UnaryMetadataTokenIlOp> ()
                    |> List.map (fun op -> $"%O{op}", OpcodeFaults.ofUnaryMetadata op)
                // `UnaryConstIlOp` is absent because `allCasesOf` cannot build its cases: every one
                // carries a payload. It could not offend regardless — `ofUnaryConst` is a single
                // arm returning `none` for the whole DU, so no case of it raises anything at all.
                yield!
                    allCasesOf<UnaryStringTokenIlOp> ()
                    |> List.map (fun op -> $"%O{op}", OpcodeFaults.ofUnaryStringToken op)
            ]

        let offenders =
            entries
            |> List.filter (fun (_, faults) ->
                match faults with
                | OpcodeFaults.Unmodelled -> false
                | OpcodeFaults.Raises fs ->
                    List.contains OpcodeFault.TypeInitialization fs
                    && not (List.contains OpcodeFault.StackOverflow fs)
            )
            |> List.map fst

        offenders |> shouldEqual []

    /// `ofIlOp` must agree with the per-shape functions rather than being a second opinion.
    [<Test>]
    let ``ofIlOp agrees with the per-shape tables`` () : unit =
        for op in allCasesOf<NullaryIlOp> () do
            OpcodeFaults.ofIlOp (IlOp.Nullary op) |> shouldEqual (OpcodeFaults.ofNullary op)

        let dummy =
            SourcedMetadataToken.ofInt (System.Reflection.AssemblyName "dummy-for-opcode-fault-tests") 0x02000001

        for op in allCasesOf<UnaryMetadataTokenIlOp> () do
            OpcodeFaults.ofIlOp (IlOp.UnaryMetadataToken (op, MetadataOperand.FromMetadata dummy))
            |> shouldEqual (OpcodeFaults.ofUnaryMetadata op)

    // ---------- Fault kinds, and the filtering they enable ----------

    /// The resource-exhaustion class is exactly `OutOfMemory` and `StackOverflow`. Asserted as a
    /// whole-set equality so that moving a fault *into* it fails here as loudly as moving one out:
    /// the class exists so a reader can hide it, and quietly widening what gets hidden is the way
    /// this stops being honest.
    [<Test>]
    let ``exactly two faults are resource exhaustion`` () : unit =
        allCasesOf<OpcodeFault> ()
        |> List.filter (fun f -> OpcodeFault.kind f = FaultKind.ResourceExhaustion)
        |> List.map nameOf
        |> List.sort
        |> shouldEqual [ "OutOfMemory" ; "StackOverflow" ]

    /// `excludingKind` is a reporting policy, so it must not turn an admission of ignorance into a
    /// claim of safety: an unclassified instruction might raise a fault of any kind, including one
    /// the caller did not ask to drop.
    [<Test>]
    let ``excludingKind leaves Unmodelled alone`` () : unit =
        OpcodeFaults.excludingKind FaultKind.ResourceExhaustion OpcodeFaults.Unmodelled
        |> shouldEqual OpcodeFaults.Unmodelled

        OpcodeFaults.excludingKind FaultKind.Logic OpcodeFaults.Unmodelled
        |> shouldEqual OpcodeFaults.Unmodelled

    /// Drops the named kind and nothing else. `newarr` is the useful case to assert on: it carries
    /// one fault of each kind, so an implementation that dropped everything, or nothing, or the
    /// wrong one, all read differently here.
    [<Test>]
    let ``excludingKind drops one kind and keeps the other`` () : unit =
        let newarr = OpcodeFaults.ofUnaryMetadata UnaryMetadataTokenIlOp.Newarr

        newarr
        |> shouldEqual (OpcodeFaults.Raises [ OpcodeFault.Overflow ; OpcodeFault.OutOfMemory ])

        newarr
        |> OpcodeFaults.excludingKind FaultKind.ResourceExhaustion
        |> shouldEqual (OpcodeFaults.Raises [ OpcodeFault.Overflow ])

        newarr
        |> OpcodeFaults.excludingKind FaultKind.Logic
        |> shouldEqual (OpcodeFaults.Raises [ OpcodeFault.OutOfMemory ])

    /// Filtering must never *add* a fault, whatever it is asked to drop. Checked over the whole
    /// table rather than at a chosen opcode, since the interesting failure would be at whichever
    /// entry the author did not think of.
    [<Test>]
    let ``excludingKind only ever removes`` () : unit =
        let check (before : OpcodeFaults) =
            for kind in allCasesOf<FaultKind> () do
                match before, OpcodeFaults.excludingKind kind before with
                | OpcodeFaults.Unmodelled, after -> after |> shouldEqual OpcodeFaults.Unmodelled
                | OpcodeFaults.Raises xs, OpcodeFaults.Raises ys ->
                    for y in ys do
                        List.contains y xs |> shouldEqual true
                | OpcodeFaults.Raises _, OpcodeFaults.Unmodelled ->
                    failwith "excludingKind turned a classified entry into an unclassified one"

        for op in allCasesOf<NullaryIlOp> () do
            check (OpcodeFaults.ofNullary op)

        for op in allCasesOf<UnaryMetadataTokenIlOp> () do
            check (OpcodeFaults.ofUnaryMetadata op)

    // ---------- The two spellings of "which type is this fault?" agree ----------

    /// `OpcodeFault.typeName` and `OpcodeFault.resolve` answer the same question for two different
    /// consumers — one holding no assemblies, one holding a corelib — and nothing but this ties
    /// them together. A fault whose two answers disagreed would have an analyser reporting one
    /// type where the interpreter raised another, which is precisely the drift the table exists to
    /// prevent.
    ///
    /// Resolved against the *host's* corelib rather than a fabricated one: the point is that these
    /// ten names exist and are the types they claim to be, which a stub could not establish.
    [<Test>]
    let ``typeName and resolve agree for every fault`` () : unit =
        // Factory intentionally undisposed: corelib.Logger outlives this scope.
        let corelib =
            let _, loggerFactory = LoggerFactory.makeTest ()
            Assembly.readFile loggerFactory typeof<obj>.Assembly.Location

        let bct = Corelib.getBaseTypes corelib

        for fault in allCasesOf<OpcodeFault> () do
            let resolved = OpcodeFault.resolve bct fault

            $"{resolved.Namespace}.{resolved.Name}"
            |> shouldEqual (OpcodeFault.typeName fault)

    /// `switch` falls through when the index is out of range rather than faulting, which is the
    /// one thing about it a reader is likely to get wrong.
    [<Test>]
    let ``switch does not fault on an out-of-range index`` () : unit =
        OpcodeFaults.ofIlOp (IlOp.Switch (System.Collections.Immutable.ImmutableArray.Create<int32> 0))
        |> shouldEqual (OpcodeFaults.Raises [])
