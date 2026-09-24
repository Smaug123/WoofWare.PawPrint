namespace WoofWare.PawPrint.Test

open System
open System.Net
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel.Test

/// `SocketShimPal.socketAddressSizes` against the sizes this machine's own
/// runtime builds its socket addresses with.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSocketShimPal =

    /// `IPEndPoint.Serialize()` hands back a platform `sockaddr` whose length
    /// is the shim's `sizeof`, so it is evidence about this machine.
    [<Test>]
    let ``the internet address sizes are this machine's`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            if not BitConverter.IsLittleEndian then
                Assert.Ignore "no preset describes a big-endian host"

            let sizes = SocketShimPal.socketAddressSizes (HostPlatform.platformOf flavour)

            IPEndPoint(IPAddress.Parse "1.2.3.4", 0x1234).Serialize().Size
            |> shouldEqual sizes.InterNetwork

            IPEndPoint(IPAddress.Parse "::1%7", 0x1234).Serialize().Size
            |> shouldEqual sizes.InterNetworkV6
        )
