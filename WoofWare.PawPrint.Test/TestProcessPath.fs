namespace WoofWare.PawPrint.Test

open System
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `KernelConfig.ProcessPath` is the executable path a guest reads back through
/// `SystemNative_GetProcessPath`. These pin the configuration-to-kernel wiring,
/// which no end-to-end guest can distinguish from a handler that read the field
/// but discarded the write: `applyTo` threading a value it then drops would leave
/// every case in the suite green.
///
/// The guest-visible half — that the value reaches `Environment.ProcessPath`
/// byte for byte, and that the `None` default surfaces as NULL with errno
/// ENOENT — lives in `sourcesImpure/ProcessPathConfigured.cs` and
/// `sourcesImpure/ProcessPathAbsent.cs`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestProcessPath =

    [<Test>]
    let ``The default is no process path at all`` () : unit =
        // Asserted on both the config default and a freshly-minted kernel, since
        // `EmulatedKernel.initial` and `KernelConfig.Default` are separate
        // literals that could drift apart.
        KernelConfig.Default.ProcessPath |> shouldEqual None
        EmulatedKernel.initial.ProcessPath |> shouldEqual None

    [<Test>]
    let ``applyTo carries a configured process path onto the kernel`` () : unit =
        let path = AbsoluteUnixPath.parseOrFail "test" "/home/pawprint/work/Guest"

        let kernel =
            EmulatedKernel.initial
            |> KernelConfig.applyTo
                { KernelConfig.Default with
                    ProcessPath = Some path
                }

        kernel.ProcessPath |> shouldEqual (Some path)

    [<Test>]
    let ``applyTo preserves None rather than substituting a default`` () : unit =
        // `KernelConfig` holds two `option` fields whose `None`s mean different
        // things: `FileSystemType`'s asks `applyTo` to derive a value from the
        // flavour, while `ProcessPath`'s *is* the answer. This is the test that
        // stops the second from being "fixed" into the first.
        let kernel = EmulatedKernel.initial |> KernelConfig.applyTo KernelConfig.Default

        kernel.ProcessPath |> shouldEqual None

    [<Test>]
    let ``withProcessPath rejects a forged path`` () : unit =
        // `AbsoluteUnixPath`'s case is private, so the only invalid value a host
        // can produce is a defaulted one. The setter is where that stops, and it
        // must name the knob rather than failing as a null reference inside the
        // first `SystemNative_GetProcessPath` — the same boundary
        // `withFileSystemAndCurrentDirectory` draws for the current directory.
        let exn =
            Assert.Throws<Exception> (fun () ->
                EmulatedKernel.initial
                |> EmulatedKernel.withProcessPath (Some Unchecked.defaultof<AbsoluteUnixPath>)
                |> ignore<EmulatedKernel>
            )

        exn.Message |> shouldContainText "EmulatedKernel.ProcessPath"

    [<Test>]
    let ``withProcessPath accepts None`` () : unit =
        // The validation must not fire on the absent case: `None` carries no
        // path to validate.
        EmulatedKernel.initial
        |> EmulatedKernel.withProcessPath None
        |> fun k -> k.ProcessPath
        |> shouldEqual None
