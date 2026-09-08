namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.Security.Cryptography
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The `EVP_MD_CTX` table behind the OpenSSL digest natives. Digests are checked against the
/// host's `SHA256.HashData`; handle lifetimes against what OpenSSL's contract permits.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEvpDigestRegistry =

    let private bytes (s : string) : ImmutableArray<byte> =
        Text.Encoding.ASCII.GetBytes s |> ImmutableArray.CreateRange

    let private hex (digest : ImmutableArray<byte>) : string =
        Convert.ToHexString(digest.AsSpan ()).ToLowerInvariant ()

    let private oracle (s : string) : string =
        Convert.ToHexString(SHA256.HashData (Text.Encoding.ASCII.GetBytes s)).ToLowerInvariant ()

    let private update (op : string) (handle : EvpMdCtxHandle) (data : string) (registry : EvpDigestRegistry) =
        EvpDigestRegistry.set
            op
            handle
            (EvpDigestContext.update op handle (bytes data) (EvpDigestRegistry.get op handle registry))
            registry

    let private finalise
        (op : string)
        (handle : EvpMdCtxHandle)
        (registry : EvpDigestRegistry)
        : string * EvpDigestRegistry
        =
        let digest, context =
            EvpDigestContext.finalise op handle (EvpDigestRegistry.get op handle registry)

        hex digest, EvpDigestRegistry.set op handle context registry

    /// The guest holds handles as `IntPtr`s and may compare or hash them, so two runs must
    /// mint the same sequence. The first is 1 rather than 0, which is what keeps
    /// `SafeEvpMdCtxHandle.IsInvalid` false.
    [<Test>]
    let ``handles are minted from a counter starting at one`` () : unit =
        let mintThree () : EvpMdCtxHandle list =
            let registry = EvpDigestRegistry.empty ()
            let first, registry = EvpDigestRegistry.create EvpDigestAlgorithm.Sha256 registry
            let second, registry = EvpDigestRegistry.create EvpDigestAlgorithm.Sha256 registry
            let registry = EvpDigestRegistry.destroy "test" first registry
            let third, _ = EvpDigestRegistry.copy "test" second registry
            [ first ; second ; third ]

        mintThree ()
        |> shouldEqual [ EvpMdCtxHandle 1L ; EvpMdCtxHandle 2L ; EvpMdCtxHandle 3L ]
        // A destroyed handle's number is never reused: a stale IntPtr must keep failing rather
        // than silently naming a newer context.
        mintThree () |> shouldEqual (mintThree ())

    [<Test>]
    let ``a copy continues independently of the original`` () : unit =
        let registry = EvpDigestRegistry.empty ()
        let original, registry = EvpDigestRegistry.create EvpDigestAlgorithm.Sha256 registry
        let registry = update "update" original "abc" registry
        let copied, registry = EvpDigestRegistry.copy "copy" original registry
        let registry = update "update" original "def" registry
        let registry = update "update" copied "xyz" registry

        let originalDigest, registry = finalise "final" original registry
        let copiedDigest, _ = finalise "final" copied registry

        originalDigest |> shouldEqual (oracle "abcdef")
        copiedDigest |> shouldEqual (oracle "abcxyz")

    [<Test>]
    let ``current reports the digest so far without finalising`` () : unit =
        let registry = EvpDigestRegistry.empty ()
        let handle, registry = EvpDigestRegistry.create EvpDigestAlgorithm.Sha256 registry
        let registry = update "update" handle "hello " registry

        hex (EvpDigestContext.current "current" handle (EvpDigestRegistry.get "current" handle registry))
        |> shouldEqual (oracle "hello ")

        let registry = update "update" handle "world" registry
        let digest, _ = finalise "final" handle registry
        digest |> shouldEqual (oracle "hello world")

    [<Test>]
    let ``reset after finalising starts a fresh computation`` () : unit =
        let registry = EvpDigestRegistry.empty ()
        let handle, registry = EvpDigestRegistry.create EvpDigestAlgorithm.Sha256 registry
        let registry = update "update" handle "first" registry
        let first, registry = finalise "final" handle registry

        let registry =
            EvpDigestRegistry.set "reset" handle (EvpDigestContext.reset EvpDigestAlgorithm.Sha256) registry

        let registry = update "update" handle "second" registry
        let second, _ = finalise "final" handle registry

        first |> shouldEqual (oracle "first")
        second |> shouldEqual (oracle "second")

    let private shouldFailNaming (operation : string) (handle : EvpMdCtxHandle) (f : unit -> unit) : unit =
        let exn = Assert.Throws<Exception> (fun () -> f ())
        exn.Message |> shouldContainText operation
        exn.Message |> shouldContainText (string<EvpMdCtxHandle> handle)

    [<Test>]
    let ``a handle that was never minted is refused by name`` () : unit =
        let registry = EvpDigestRegistry.empty ()
        let invented = EvpMdCtxHandle 42L

        shouldFailNaming
            "CryptoNative_EvpDigestUpdate"
            invented
            (fun () ->
                EvpDigestRegistry.get "CryptoNative_EvpDigestUpdate" invented registry
                |> ignore<EvpDigestContext>
            )

        shouldFailNaming
            "CryptoNative_EvpMdCtxDestroy"
            invented
            (fun () ->
                EvpDigestRegistry.destroy "CryptoNative_EvpMdCtxDestroy" invented registry
                |> ignore<EvpDigestRegistry>
            )

    [<Test>]
    let ``a destroyed handle is refused by name`` () : unit =
        let registry = EvpDigestRegistry.empty ()
        let handle, registry = EvpDigestRegistry.create EvpDigestAlgorithm.Sha256 registry
        let registry = EvpDigestRegistry.destroy "destroy" handle registry

        EvpDigestRegistry.isLive handle registry |> shouldEqual false

        shouldFailNaming
            "CryptoNative_EvpDigestFinalEx"
            handle
            (fun () ->
                EvpDigestRegistry.get "CryptoNative_EvpDigestFinalEx" handle registry
                |> ignore<EvpDigestContext>
            )

        shouldFailNaming
            "CryptoNative_EvpMdCtxCopyEx"
            handle
            (fun () ->
                EvpDigestRegistry.copy "CryptoNative_EvpMdCtxCopyEx" handle registry
                |> ignore<EvpMdCtxHandle * EvpDigestRegistry>
            )

        // A store must not resurrect it either.
        shouldFailNaming
            "CryptoNative_EvpDigestReset"
            handle
            (fun () ->
                EvpDigestRegistry.set
                    "CryptoNative_EvpDigestReset"
                    handle
                    (EvpDigestContext.reset EvpDigestAlgorithm.Sha256)
                    registry
                |> ignore<EvpDigestRegistry>
            )

        // Double free.
        shouldFailNaming
            "CryptoNative_EvpMdCtxDestroy"
            handle
            (fun () ->
                EvpDigestRegistry.destroy "CryptoNative_EvpMdCtxDestroy" handle registry
                |> ignore<EvpDigestRegistry>
            )

    /// OpenSSL permits no `EVP_DigestUpdate` after `EVP_DigestFinal_ex` without a
    /// re-initialisation, and does not say what a second final or a current would produce.
    [<Test>]
    let ``a finalised context refuses everything but reset and destroy`` () : unit =
        let registry = EvpDigestRegistry.empty ()
        let handle, registry = EvpDigestRegistry.create EvpDigestAlgorithm.Sha256 registry
        let _, registry = finalise "final" handle registry
        let context = EvpDigestRegistry.get "get" handle registry

        shouldFailNaming
            "CryptoNative_EvpDigestUpdate"
            handle
            (fun () ->
                EvpDigestContext.update "CryptoNative_EvpDigestUpdate" handle (bytes "more") context
                |> ignore<EvpDigestContext>
            )

        shouldFailNaming
            "CryptoNative_EvpDigestFinalEx"
            handle
            (fun () ->
                EvpDigestContext.finalise "CryptoNative_EvpDigestFinalEx" handle context
                |> ignore<ImmutableArray<byte> * EvpDigestContext>
            )

        shouldFailNaming
            "CryptoNative_EvpDigestCurrent"
            handle
            (fun () ->
                EvpDigestContext.current "CryptoNative_EvpDigestCurrent" handle context
                |> ignore<ImmutableArray<byte>>
            )

        EvpDigestContext.algorithm context |> shouldEqual EvpDigestAlgorithm.Sha256
        EvpDigestRegistry.destroy "destroy" handle registry |> ignore<EvpDigestRegistry>
