namespace WoofWare.Pawprint.Test

open System
open System.Reflection.Metadata
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `CallSiteConvention` is what decides whether an entry into a `[UnmanagedCallersOnly]` method is
/// the legal native transition or the fatal managed one, so which side of it each calling
/// convention falls on is a contract rather than an implementation detail.
///
/// ECMA-335 II.15.3: `DEFAULT` and `VARARG` are the managed conventions; the rest name a platform
/// ABI and so describe a call native code would make.
[<TestFixture>]
module TestCallSiteConvention =

    /// The whole of ECMA-335 II.23.2.3's calling-convention space, written out rather than
    /// computed, so that this says what the mapping *should* be instead of restating what it is.
    let private expected : (SignatureCallingConvention * IlMachineStateExecution.CallSiteConvention) list =
        [
            SignatureCallingConvention.Default, IlMachineStateExecution.CallSiteConvention.Managed
            SignatureCallingConvention.VarArgs, IlMachineStateExecution.CallSiteConvention.Managed
            SignatureCallingConvention.CDecl, IlMachineStateExecution.CallSiteConvention.Unmanaged
            SignatureCallingConvention.StdCall, IlMachineStateExecution.CallSiteConvention.Unmanaged
            SignatureCallingConvention.ThisCall, IlMachineStateExecution.CallSiteConvention.Unmanaged
            SignatureCallingConvention.FastCall, IlMachineStateExecution.CallSiteConvention.Unmanaged
            SignatureCallingConvention.Unmanaged, IlMachineStateExecution.CallSiteConvention.Unmanaged
        ]

    [<Test>]
    let ``each calling convention lands on the documented side`` () : unit =
        for convention, side in expected do
            IlMachineStateExecution.CallSiteConvention.ofSignatureCallingConvention convention
            |> shouldEqual side

    /// Tied to the enum rather than to the list above: a runtime that grew a new calling
    /// convention would otherwise leave it silently unclassified, and the classifier's `failwith`
    /// would first be discovered by a guest.
    [<Test>]
    let ``the table covers every calling convention the metadata reader can produce`` () : unit =
        let declared =
            Enum.GetValues typeof<SignatureCallingConvention>
            |> Seq.cast<SignatureCallingConvention>
            |> Set.ofSeq

        expected |> List.map fst |> Set.ofList |> shouldEqual declared

    /// The one that matters for the gate, stated on its own so that a change collapsing the two
    /// sides together cannot pass by rewriting the table above in the same edit.
    [<Test>]
    let ``exactly the managed conventions are Managed`` () : unit =
        let managed =
            Enum.GetValues typeof<SignatureCallingConvention>
            |> Seq.cast<SignatureCallingConvention>
            |> Seq.filter (fun c ->
                IlMachineStateExecution.CallSiteConvention.ofSignatureCallingConvention c = IlMachineStateExecution.CallSiteConvention.Managed
            )
            |> Set.ofSeq

        managed
        |> shouldEqual (Set.ofList [ SignatureCallingConvention.Default ; SignatureCallingConvention.VarArgs ])
