namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Collections.Immutable
open System.Security.Cryptography
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint.Test.LinuxCoreLibFlavour
open WoofWare.PosixKernel

/// The OpenSSL EVP digest natives (`libSystem.Security.Cryptography.Native.OpenSsl`), reached
/// the way `System.Security.Cryptography` reaches them on Linux: `SHA256.Create().ComputeHash`
/// through `LiteHash` (`EvpMdCtxCreate`, one `EvpDigestUpdate` per stream read,
/// `EvpDigestFinalEx`, `EvpDigestReset`, `EvpMdCtxDestroy`), `SHA256.HashData` through
/// `EvpDigestOneShot`, and `IncrementalHash` through `EvpDigestCurrent` and `EvpMdCtxCopyEx`.
///
/// Linux-flavour only: the macOS build of that assembly goes to CryptoKit instead, so on a
/// macOS host these paths exist only in the pinned linux-x64 pack.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEvpDigestNatives =

    /// The guest's message, regenerated here rather than shared: 10000 bytes, which is 156
    /// whole SHA-256 blocks and 16 bytes over, so the padding runs on a part-filled block.
    /// `ComputeHash(Stream)` reads in whole multiples of the 64-byte block, so it is the
    /// `IncrementalHash` section below — whose first append is 100 bytes — that makes an
    /// `EvpDigestUpdate` start 36 bytes into a block and so carries pending bytes between
    /// calls.
    let private guestData () : byte[] =
        Array.init 10000 (fun i -> byte (((i * 31 + (i >>> 5)) ^^^ 0x5A) &&& 0xFF))

    let private hex (bytes : byte[]) : string =
        Convert.ToHexString(bytes).ToLowerInvariant ()

    // Pinned from the host: `SHA256.HashData` over `guestData ()`, its first 100 bytes, and
    // nothing. Pinned as literals as well as recomputed below, so that a change to the
    // generator on one side cannot silently move the expectation with it.
    let private expectedWhole =
        "a6e27cfd5a637ced279a6e2bdf85c44eb7b653085e70e3fad583628345243f0a"

    let private expectedPrefix =
        "598d642f4df8b16ceeb30a60da07a5927d544465279ef9a9e86f05669f530653"

    let private expectedEmpty =
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

    [<Test>]
    let ``the pinned expectations are the host's digests of the guest's message`` () : unit =
        let data = guestData ()
        hex (SHA256.HashData data) |> shouldEqual expectedWhole
        hex (SHA256.HashData data.[0..99]) |> shouldEqual expectedPrefix
        hex (SHA256.HashData Array.empty<byte>) |> shouldEqual expectedEmpty

    let private guestSource =
        """
using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Security.Cryptography;

public class Program
{
    // Raw write(2) rather than Console, so that the only machinery between the digest and
    // the assertion is the one under test.
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write")]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    static byte[] Data()
    {
        byte[] data = new byte[10000];
        for (int i = 0; i < data.Length; i++)
        {
            data[i] = (byte)((i * 31 + (i >> 5)) ^ 0x5A);
        }
        return data;
    }

    static bool Same(byte[] a, byte[] b)
    {
        if (a.Length != b.Length) return false;
        for (int i = 0; i < a.Length; i++)
        {
            if (a[i] != b[i]) return false;
        }
        return true;
    }

    static unsafe bool Emit(byte[] digest)
    {
        fixed (byte* p = digest)
        {
            return Write((IntPtr)1, p, digest.Length) == digest.Length;
        }
    }

    public static int Main(string[] args)
    {
        byte[] data = Data();

        // ComputeHash(Stream) reads a bufferful at a time: one EvpDigestUpdate per read, then
        // EvpDigestFinalEx and EvpDigestReset; Dispose reaches EvpMdCtxDestroy.
        byte[] streamed;
        using (SHA256 sha = SHA256.Create())
        using (MemoryStream stream = new MemoryStream(data))
        {
            streamed = sha.ComputeHash(stream);
        }

        // EvpDigestOneShot.
        byte[] oneShot = SHA256.HashData(data);

        // EvpDigestFinalEx with no update at all.
        byte[] empty;
        using (SHA256 sha = SHA256.Create())
        using (MemoryStream stream = new MemoryStream(Array.Empty<byte>()))
        {
            empty = sha.ComputeHash(stream);
        }

        // EvpDigestCurrent leaves the context running; EvpMdCtxCopyEx forks it; a context
        // reset after EvpDigestFinalEx starts over.
        byte[] prefixCurrent;
        byte[] cloneFinal;
        byte[] whole;
        byte[] afterReset;
        using (IncrementalHash running = IncrementalHash.CreateHash(HashAlgorithmName.SHA256))
        {
            running.AppendData(data, 0, 100);
            prefixCurrent = running.GetCurrentHash();
            using (IncrementalHash clone = running.Clone())
            {
                running.AppendData(data, 100, data.Length - 100);
                cloneFinal = clone.GetHashAndReset();
            }
            whole = running.GetHashAndReset();
            running.AppendData(data, 0, 100);
            afterReset = running.GetHashAndReset();
        }

        if (streamed.Length != 32) return 1;
        if (!Same(whole, streamed)) return 2;
        if (!Same(cloneFinal, prefixCurrent)) return 3;
        if (!Same(afterReset, prefixCurrent)) return 4;

        if (!Emit(streamed)) return 5;
        if (!Emit(oneShot)) return 6;
        if (!Emit(empty)) return 7;
        if (!Emit(prefixCurrent)) return 8;

        // The first byte of the streamed digest, so that the exit code alone already
        // distinguishes a wrong digest from a right one 255 times out of 256.
        return streamed[0];
    }
}
"""

    [<Test>]
    let ``SHA256 through the EVP digest natives agrees with the host on the linux-x64 CoreLib`` () : unit =
        let frameworkDir = requireLinuxFramework ()

        let terminalState =
            runOnLinuxFramework "EvpDigestNatives.cs" frameworkDir guestSource

        loadedCorelibPath terminalState |> shouldEqual (corelibPath frameworkDir)

        let stdout =
            OutputLogEntry.bytesFor FileDescriptorRole.StandardOutput terminalState.Kernel.OutputLog
            |> Seq.toArray

        // Every sentinel in the guest is below 32, and no expected first byte is: 0xa6 = 166.
        terminalState.LatchedExitCode |> shouldEqual 166

        stdout.Length |> shouldEqual 128
        hex stdout.[0..31] |> shouldEqual expectedWhole
        hex stdout.[32..63] |> shouldEqual expectedWhole
        hex stdout.[64..95] |> shouldEqual expectedEmpty
        hex stdout.[96..127] |> shouldEqual expectedPrefix

    /// Only SHA-256 is modelled. Asking for another algorithm must stop the run naming that
    /// algorithm, rather than answer with SHA-256's `EVP_MD` or with a null one — which for MD5
    /// CoreLib does not screen at all, so it would surface as a failure much further in.
    [<Test>]
    let ``an unmodelled algorithm is refused by name on the linux-x64 CoreLib`` () : unit =
        let frameworkDir = requireLinuxFramework ()

        let source =
            """
using System.Security.Cryptography;

public class Program
{
    public static int Main(string[] args)
    {
        byte[] digest = MD5.HashData(new byte[] { 1, 2, 3 });
        return digest[0];
    }
}
"""

        let exn =
            Assert.Catch (fun () ->
                runOnLinuxFramework "EvpDigestUnmodelled.cs" frameworkDir source
                |> ignore<IlMachineState>
            )

        exn.Message |> shouldContainText "CryptoNative_EvpMd5"
        exn.Message |> shouldContainText "MD5"

    /// The host's own framework: the guest below declares every import itself, so it needs no
    /// particular flavour of `System.Security.Cryptography`.
    let private runOnHostFramework (name : string) (source : string) : IlMachineState =
        let image = Roslyn.compile [ source ]
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        use peImage = new MemoryStream (image)

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        match Program.run loggerFactory (Some name) peImage (HostConfig.Default dotnetRuntimes) with
        | RunOutcome.NormalExit (terminalState, _) -> terminalState
        | other -> failwith $"Expected the guest to exit normally, got %O{other}"

    /// What CoreLib's callers cannot see. `LiteHash.Finalize` and `OneShotHashProvider.HashData`
    /// both discard the length the shim writes through `s`/`mdSize` and report the size they
    /// already knew, so only a hand-rolled P/Invoke observes that write, the bytes beyond the
    /// digest being left alone, `GetMaxMdSize`, a zero-length update through a null pointer,
    /// and `EvpMdCtxDestroy(NULL)` being a no-op. Every check has its own exit code.
    [<Test>]
    let ``the entry points answer a hand-rolled P/Invoke as OpenSSL's contract says`` () : unit =
        let source =
            """
using System;
using System.Runtime.InteropServices;

public unsafe class Program
{
    const string Lib = "libSystem.Security.Cryptography.Native.OpenSsl";

    [DllImport(Lib, EntryPoint = "CryptoNative_GetMaxMdSize")] static extern int GetMaxMdSize();
    [DllImport(Lib, EntryPoint = "CryptoNative_EvpSha256")] static extern IntPtr EvpSha256();
    [DllImport(Lib, EntryPoint = "CryptoNative_EvpMdSize")] static extern int EvpMdSize(IntPtr md);
    [DllImport(Lib, EntryPoint = "CryptoNative_EvpMdCtxCreate")] static extern IntPtr EvpMdCtxCreate(IntPtr type);
    [DllImport(Lib, EntryPoint = "CryptoNative_EvpMdCtxCopyEx")] static extern IntPtr EvpMdCtxCopyEx(IntPtr ctx);
    [DllImport(Lib, EntryPoint = "CryptoNative_EvpMdCtxDestroy")] static extern void EvpMdCtxDestroy(IntPtr ctx);
    [DllImport(Lib, EntryPoint = "CryptoNative_EvpDigestReset")] static extern int EvpDigestReset(IntPtr ctx, IntPtr type);
    [DllImport(Lib, EntryPoint = "CryptoNative_EvpDigestUpdate")] static extern int EvpDigestUpdate(IntPtr ctx, byte* d, int cnt);
    [DllImport(Lib, EntryPoint = "CryptoNative_EvpDigestFinalEx")] static extern int EvpDigestFinalEx(IntPtr ctx, byte* md, uint* s);
    [DllImport(Lib, EntryPoint = "CryptoNative_EvpDigestCurrent")] static extern int EvpDigestCurrent(IntPtr ctx, byte* md, uint* s);
    [DllImport(Lib, EntryPoint = "CryptoNative_EvpDigestOneShot")] static extern int EvpDigestOneShot(IntPtr type, byte* source, int sourceSize, byte* md, uint* mdSize);

    // "abc" is FIPS 180-4's own worked example; "abcd" is pinned from the host's
    // SHA256.HashData.
    static readonly byte[] AbcDigest = new byte[]
    {
        0xba, 0x78, 0x16, 0xbf, 0x8f, 0x01, 0xcf, 0xea, 0x41, 0x41, 0x40, 0xde, 0x5d, 0xae, 0x22, 0x23,
        0xb0, 0x03, 0x61, 0xa3, 0x96, 0x17, 0x7a, 0x9c, 0xb4, 0x10, 0xff, 0x61, 0xf2, 0x00, 0x15, 0xad,
    };
    static readonly byte[] AbcdDigest = new byte[]
    {
        0x88, 0xd4, 0x26, 0x6f, 0xd4, 0xe6, 0x33, 0x8d, 0x13, 0xb8, 0x45, 0xfc, 0xf2, 0x89, 0x57, 0x9d,
        0x20, 0x9c, 0x89, 0x78, 0x23, 0xb9, 0x21, 0x7d, 0xa3, 0xe1, 0x61, 0x93, 0x6f, 0x03, 0x15, 0x89,
    };

    static bool IsDigest(byte[] buffer, byte[] expected)
    {
        for (int i = 0; i < 32; i++)
        {
            if (buffer[i] != expected[i]) return false;
        }
        // The shim writes exactly the digest: the rest of a 64-byte buffer is untouched.
        for (int i = 32; i < buffer.Length; i++)
        {
            if (buffer[i] != 0xEE) return false;
        }
        return true;
    }

    static byte[] Scratch()
    {
        byte[] buffer = new byte[64];
        for (int i = 0; i < buffer.Length; i++) buffer[i] = 0xEE;
        return buffer;
    }

    static int Update(IntPtr ctx, string text)
    {
        byte[] bytes = System.Text.Encoding.ASCII.GetBytes(text);
        fixed (byte* p = bytes)
        {
            return EvpDigestUpdate(ctx, p, bytes.Length);
        }
    }

    public static int Main(string[] args)
    {
        if (GetMaxMdSize() != 64) return 1;

        IntPtr sha256 = EvpSha256();
        if (sha256 == IntPtr.Zero) return 2;
        if (EvpMdSize(sha256) != 32) return 3;

        IntPtr ctx = EvpMdCtxCreate(sha256);
        if (ctx == IntPtr.Zero) return 4;

        if (Update(ctx, "abc") != 1) return 5;

        // A zero-length update through a null pointer is what an empty span pins to.
        if (EvpDigestUpdate(ctx, null, 0) != 1) return 6;

        // Current: the digest so far, and the context keeps running.
        byte[] current = Scratch();
        uint currentLength = 0xFFFFFFFF;
        fixed (byte* p = current)
        {
            if (EvpDigestCurrent(ctx, p, &currentLength) != 1) return 7;
        }
        if (currentLength != 32) return 8;
        if (!IsDigest(current, AbcDigest)) return 9;

        // A copy forks the computation.
        IntPtr copy = EvpMdCtxCopyEx(ctx);
        if (copy == IntPtr.Zero || copy == ctx) return 10;
        if (Update(ctx, "d") != 1) return 11;

        byte[] copied = Scratch();
        uint copiedLength = 0xFFFFFFFF;
        fixed (byte* p = copied)
        {
            if (EvpDigestFinalEx(copy, p, &copiedLength) != 1) return 12;
        }
        if (copiedLength != 32) return 13;
        if (!IsDigest(copied, AbcDigest)) return 14;

        byte[] final = Scratch();
        uint finalLength = 0xFFFFFFFF;
        fixed (byte* p = final)
        {
            if (EvpDigestFinalEx(ctx, p, &finalLength) != 1) return 15;
        }
        if (finalLength != 32) return 16;
        if (!IsDigest(final, AbcdDigest)) return 17;

        // Reset starts over on the same handle.
        if (EvpDigestReset(ctx, sha256) != 1) return 18;
        if (Update(ctx, "abc") != 1) return 19;
        byte[] again = Scratch();
        uint againLength = 0xFFFFFFFF;
        fixed (byte* p = again)
        {
            if (EvpDigestFinalEx(ctx, p, &againLength) != 1) return 20;
        }
        if (againLength != 32) return 21;
        if (!IsDigest(again, AbcDigest)) return 22;

        // One shot.
        byte[] source = System.Text.Encoding.ASCII.GetBytes("abcd");
        byte[] oneShot = Scratch();
        uint oneShotLength = 0xFFFFFFFF;
        fixed (byte* s = source)
        fixed (byte* p = oneShot)
        {
            if (EvpDigestOneShot(sha256, s, source.Length, p, &oneShotLength) != 1) return 23;
        }
        if (oneShotLength != 32) return 24;
        if (!IsDigest(oneShot, AbcdDigest)) return 25;

        EvpMdCtxDestroy(copy);
        EvpMdCtxDestroy(ctx);
        // EVP_MD_CTX_free(NULL) is a documented no-op.
        EvpMdCtxDestroy(IntPtr.Zero);

        return 0;
    }
}
"""

        let terminalState = runOnHostFramework "EvpDigestHandRolled.cs" source
        terminalState.LatchedExitCode |> shouldEqual 0

        EvpDigestRegistry.isLive (EvpMdCtxHandle 1L) terminalState.EvpDigests
        |> shouldEqual false

        EvpDigestRegistry.isLive (EvpMdCtxHandle 2L) terminalState.EvpDigests
        |> shouldEqual false
