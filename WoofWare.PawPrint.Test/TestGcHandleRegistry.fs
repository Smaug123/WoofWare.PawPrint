namespace WoofWare.Pawprint.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
module TestGcHandleRegistry =
    [<Test>]
    let ``compare-exchange updates only when the target matches`` () : unit =
        let originalTarget = ManagedHeapAddress.ManagedHeapAddress 10
        let replacementTarget = ManagedHeapAddress.ManagedHeapAddress 11

        let handle, registry =
            GcHandleRegistry.empty ()
            |> GcHandleRegistry.allocate
                GcHandleKind.WeakTrackResurrection
                (GcHandleOwner.TypeAssociated (ConcreteTypeHandle.Concrete 12))
                None

        GcHandleRegistry.target handle registry |> shouldEqual None

        let oldTarget, registry =
            registry
            |> GcHandleRegistry.compareExchangeTarget handle (Some originalTarget) None

        oldTarget |> shouldEqual None
        GcHandleRegistry.target handle registry |> shouldEqual (Some originalTarget)

        let oldTarget, registry =
            registry
            |> GcHandleRegistry.compareExchangeTarget handle (Some replacementTarget) None

        oldTarget |> shouldEqual (Some originalTarget)
        GcHandleRegistry.target handle registry |> shouldEqual (Some originalTarget)
