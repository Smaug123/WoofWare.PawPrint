namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The per-thread last-error slots: errno (`LastSystemError`) and the value
/// `Marshal.GetLastPInvokeError` reads (`LastPInvokeError`).
///
/// `sourcesPure/LastErrorIsPerThread.cs` pins the guest-visible half — that one
/// thread cannot observe another's write — against the real runtime. What it
/// cannot see is the *representation*: both slots store zero by removing the
/// thread's entry, so that a state which zeroed a slot is structurally equal to
/// one that never wrote it. `EmulatedKernel` is compared for equality to decide
/// whether a step changed anything, so a stored zero would be a state that looks
/// different while behaving identically — invisible to any guest, and corrosive
/// to the determinism the interpreter exists to provide.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestLastError =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private t0 : ThreadId = ThreadId 0
    let private t1 : ThreadId = ThreadId 1
    let private t2 : ThreadId = ThreadId 2

    let private allThreads : ThreadId list = [ t0 ; t1 ; t2 ]

    /// Which of the two slots an operation addresses. They are independent, so
    /// the property drives both and checks neither disturbs the other.
    type private Slot =
        | System
        | PInvoke

    let private read (slot : Slot) (thread : ThreadId) (kernel : EmulatedKernel) : int =
        match slot with
        | Slot.System -> EmulatedKernel.lastSystemErrorFor thread kernel
        | Slot.PInvoke -> EmulatedKernel.lastPInvokeErrorFor thread kernel

    let private write (slot : Slot) (thread : ThreadId) (value : int) (kernel : EmulatedKernel) : EmulatedKernel =
        match slot with
        | Slot.System -> EmulatedKernel.withLastSystemError thread value kernel
        | Slot.PInvoke -> EmulatedKernel.withLastPInvokeError thread value kernel

    [<Test>]
    let ``a fresh kernel reports zero for every thread and both slots`` () =
        for thread in allThreads do
            read Slot.System thread EmulatedKernel.initial |> shouldEqual 0
            read Slot.PInvoke thread EmulatedKernel.initial |> shouldEqual 0

    [<Test>]
    let ``each thread's slot is its own`` () =
        let kernel =
            EmulatedKernel.initial
            |> EmulatedKernel.withLastSystemError t0 9
            |> EmulatedKernel.withLastSystemError t1 22

        EmulatedKernel.lastSystemErrorFor t0 kernel |> shouldEqual 9
        EmulatedKernel.lastSystemErrorFor t1 kernel |> shouldEqual 22
        // A thread that was never written still reads the default.
        EmulatedKernel.lastSystemErrorFor t2 kernel |> shouldEqual 0

    [<Test>]
    let ``the two slots are independent`` () =
        let kernel =
            EmulatedKernel.initial
            |> EmulatedKernel.withLastSystemError t0 9
            |> EmulatedKernel.withLastPInvokeError t0 22

        EmulatedKernel.lastSystemErrorFor t0 kernel |> shouldEqual 9
        EmulatedKernel.lastPInvokeErrorFor t0 kernel |> shouldEqual 22

    [<Test>]
    let ``zeroing a slot restores structural equality with a kernel that never wrote it`` () =
        // Not merely "reads back as 0": the whole kernel must be indistinguishable,
        // because that is what the interpreter compares.
        for slot in [ Slot.System ; Slot.PInvoke ] do
            EmulatedKernel.initial
            |> write slot t1 34
            |> write slot t1 0
            |> shouldEqual EmulatedKernel.initial

    [<Test>]
    let ``writing zero to a fresh slot changes nothing`` () =
        for slot in [ Slot.System ; Slot.PInvoke ] do
            EmulatedKernel.initial |> write slot t1 0 |> shouldEqual EmulatedKernel.initial

    /// Drives an arbitrary sequence of writes through the module and through a
    /// structurally different oracle: a plain association list that stores every
    /// write verbatim, zeroes included, and defaults on lookup. The two agree on
    /// every read iff the module's remove-on-zero is a pure representation choice
    /// rather than a behavioural one.
    [<Test>]
    let ``reads agree with a store-everything oracle, and no zero is ever stored`` () =
        let property (ops : (int * int * bool) list) : bool =
            let ops =
                ops
                |> List.map (fun (threadIdx, value, isSystem) ->
                    let thread = allThreads.[((threadIdx % 3) + 3) % 3]
                    let slot = if isSystem then Slot.System else Slot.PInvoke
                    thread, value, slot
                )

            let kernel =
                ops
                |> List.fold (fun kernel (thread, value, slot) -> write slot thread value kernel) EmulatedKernel.initial

            // Oracle: last write wins, zeroes stored like any other value.
            let oracle =
                ops
                |> List.fold (fun m (thread, value, slot) -> Map.add (thread, slot) value m) Map.empty

            let readsAgree =
                allThreads
                |> List.forall (fun thread ->
                    [ Slot.System ; Slot.PInvoke ]
                    |> List.forall (fun slot ->
                        let expected =
                            match Map.tryFind (thread, slot) oracle with
                            | None -> 0
                            | Some v -> v

                        read slot thread kernel = expected
                    )
                )

            // Canonical representation: a zero is never stored, in either map.
            let noStoredZero =
                (kernel.LastSystemError |> Map.forall (fun _ v -> v <> 0))
                && (kernel.LastPInvokeError |> Map.forall (fun _ v -> v <> 0))

            readsAgree && noStoredZero

        Check.One (propertyConfig, Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary) property)
