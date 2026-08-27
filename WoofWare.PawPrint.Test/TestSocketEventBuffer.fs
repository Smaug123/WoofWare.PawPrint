namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// The width of the block `SystemNative_CreateSocketEventBuffer` allocates, which is the one
/// thing about that entry point no guest can report *directly*: the stride is 16 bytes an
/// element under epoll and 32 under kqueue, reading past the end of the block aborts the run
/// rather than returning a code, and 16 is a prefix of 32 — so a guest writing into the block
/// can establish a lower bound and nothing more.
///
/// `sourcesPure/SocketEventBufferScreening.cs` covers the rows that *are* guest-visible, and
/// `sourcesImpure/SocketEventBuffer{Linux,Darwin}.cs` cover the count at which the request
/// stops being representable, which is `Int32.MaxValue / stride` and so sees the stride
/// indirectly. These tests read the width itself, out of the machine state.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSocketEventBuffer =

    let private assy = typeof<RunResult>.Assembly

    /// A guest that allocates one buffer of `count` elements and parks the pointer in a static
    /// field, so a test can find the block it names. `release` says whether it then frees it:
    /// the block's width is only readable while the block is live, and the pair of runs is what
    /// makes the free observable at all.
    let private source (count : int) (release : bool) : string =
        let free =
            if release then
                "if (FreeSocketEventBuffer(buffer) != 0) return 3;"
            else
                ""

        $"""
using System;
using System.Runtime.InteropServices;

static unsafe class Probe
{{
    public static byte* Buffer;
}}

class Program
{{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CreateSocketEventBuffer")]
    static extern unsafe int CreateSocketEventBuffer(int count, byte** buffer);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FreeSocketEventBuffer")]
    static extern unsafe int FreeSocketEventBuffer(byte* buffer);

    static unsafe int Main()
    {{
        byte* buffer = null;
        if (CreateSocketEventBuffer({count}, &buffer) != 0) return 1;
        if (buffer == null) return 2;
        Probe.Buffer = buffer;
        {free}
        return 0;
    }}
}}
"""

    let private run (platform : SimulatedUnixPlatform) (count : int) (release : bool) : IlMachineState =
        let image = Roslyn.compile [ source count release ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", "SocketEventBufferProbe.cs" ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        let hostConfig = HostConfig.Default dotnetRuntimes

        let hostConfig =
            { hostConfig with
                Guest =
                    { hostConfig.Guest with
                        Kernel =
                            { hostConfig.Guest.Kernel with
                                UnixPlatform = platform
                            }
                    }
            }

        match Program.run loggerFactory (Some "SocketEventBufferProbe.cs") peImage hostConfig with
        | RunOutcome.NormalExit (state, thread)
        | RunOutcome.ProcessExit (state, thread) ->
            match state.ThreadState.[thread].MethodState.EvaluationStack.Values with
            | EvalStackValue.Int32 (Int32Source.Verbatim 0) :: _ -> state
            | EvalStackValue.Int32 (Int32Source.Verbatim other) :: _ ->
                failwith $"guest failed its own check %d{other}, so it never reached the state these tests read"
            | other -> failwith $"guest did not return an int exit code: %O{other}"
        | other -> failwith $"guest did not exit normally: %O{other}"

    /// The native-heap block `Probe.Buffer` points at.
    ///
    /// Fails rather than answering if the static holds anything other than a pointer to byte 0 of
    /// such a block. That is the assertion doing the work in the tests below as much as the width
    /// is: the guest reached this static by loading the out-parameter the handler wrote and then
    /// `stsfld`-ing it, so a handler that had stored a synthesised bit pattern instead of a
    /// pointer would arrive here as a raw address naming no block at all.
    let private bufferBlock (state : IlMachineState) : NativeMemoryBlockId =
        let probeType =
            state._LoadedAssemblies.DefinitionNames
            |> Seq.collect (fun name -> state._LoadedAssemblies.ByDefinitionName(name).TypeDefs.Values)
            |> Seq.filter (fun ty -> ty.Name = "Probe" && ty.Namespace = "")
            |> Seq.toList
            |> function
                | [ ty ] -> ty
                | other -> failwith $"expected exactly one `Probe` type across loaded assemblies, got %d{other.Length}"

        let probeHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes probeType

        let field = FieldIdentity.requiredOwnStaticField probeType "Buffer"

        match
            IlMachineState.getStatic
                StaticOwner.Shared
                probeHandle
                (ComparableFieldDefinitionHandle.Make field.Handle)
                state
        with
        | Some (CliType.RuntimePointer (CliRuntimePointer.Managed (ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (blockId,
                                                                                                                           0),
                                                                                               [])))) -> blockId
        | other -> failwith $"expected `Probe.Buffer` to hold a pointer to byte 0 of a native-heap block, got %O{other}"

    let private blockWidth (platform : SimulatedUnixPlatform) (count : int) : int =
        let state = run platform count false

        NativeMemoryPool.blockSize (bufferBlock state) (IlMachineState.getNativeMemoryPool state)

    /// `max(sizeof(struct epoll_event), sizeof(SocketEvent))`. Three elements rather than one so
    /// that a handler which allocated a single element's worth, or one byte per element, is
    /// distinguishable from one that got the stride right.
    [<Test>]
    let ``the epoll stride is sixteen bytes an element`` () : unit =
        blockWidth SimulatedUnixPlatform.linuxX64 3 |> shouldEqual 48

    /// `sizeof(struct kevent)`, with no `max` to flatten the architecture out of it.
    [<Test>]
    let ``the kqueue stride is thirty-two bytes an element`` () : unit =
        blockWidth SimulatedUnixPlatform.macOsArm64 3 |> shouldEqual 96

    /// Both strides multiply a count, so a fixed-size allocation that happened to be right for
    /// one count would pass the tests above. This is the same claim at a different count.
    [<Test>]
    let ``the width scales with the count`` () : unit =
        blockWidth SimulatedUnixPlatform.linuxX64 1024 |> shouldEqual 16384

    /// `count == 0` is a real allocation of nothing: the pointer is non-null — which is what the
    /// static holding a block id at all establishes — and the block it names is zero bytes wide,
    /// so every offset in it is out of bounds.
    [<Test>]
    let ``a zero-element request allocates an empty block`` () : unit =
        blockWidth SimulatedUnixPlatform.linuxX64 0 |> shouldEqual 0

    /// The free half, via the only observer of a native-heap release there is. Two runs of the
    /// same guest differing *only* in whether it frees, so every other allocation either run
    /// makes is made by both, and the difference between the counts is exactly the buffer.
    [<Test>]
    let ``freeing the buffer releases its block`` () : unit =
        let leaked =
            run SimulatedUnixPlatform.linuxX64 3 false
            |> IlMachineState.getNativeMemoryPool
            |> NativeMemoryPool.liveBlockCount

        let released =
            run SimulatedUnixPlatform.linuxX64 3 true
            |> IlMachineState.getNativeMemoryPool
            |> NativeMemoryPool.liveBlockCount

        released |> shouldEqual (leaked - 1)

    /// A guest whose body is `body`, run for its refusal rather than its exit code. The two rows
    /// below are ones the handler answers with a `failwith`, so there is no code to return.
    let private refusalFrom (body : string) : string =
        $"""
using System;
using System.Runtime.InteropServices;

class Program
{{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CreateSocketEventBuffer")]
    static extern unsafe int CreateSocketEventBuffer(int count, byte** buffer);

    static unsafe int Main()
    {{
{body}
    }}
}}
"""

    let private refusalMessage (body : string) : string =
        let image = Roslyn.compile [ refusalFrom body ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", "SocketEventBufferRefusal.cs" ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        let exn =
            Assert.Catch (fun () ->
                Program.run
                    loggerFactory
                    (Some "SocketEventBufferRefusal.cs")
                    peImage
                    (HostConfig.Default dotnetRuntimes)
                |> ignore<RunOutcome>
            )

        exn.Message

    /// A non-null `buffer` naming no storage is refused rather than answered EFAULT. The C
    /// screens only `buffer == NULL`, so the real thing allocates and then faults on its
    /// unconditional store — a SIGSEGV, which is not something the guest could have caught, so
    /// answering EFAULT would let a run continue past a point the real one dies at.
    [<Test>]
    let ``a non-null buffer naming no storage is refused, not faulted`` () : unit =
        let message =
            refusalMessage
                """
        byte** buffer = (byte**)123;
        return CreateSocketEventBuffer(1, buffer);
"""

        message |> shouldContainText "SystemNative_CreateSocketEventBuffer"
        message |> shouldContainText "names no storage"

    /// A `buffer` that does name storage, but less than the eight bytes the C stores through it.
    /// Refused for the reason the handler's other buffer-room checks refuse: PawPrint's address
    /// space is a graph of typed cells, so there is nothing following the destination to
    /// overwrite and so no answer to give.
    [<Test>]
    let ``a destination narrower than a pointer is refused`` () : unit =
        let message =
            refusalMessage
                """
        byte oneByte = 0;
        return CreateSocketEventBuffer(1, (byte**)&oneByte);
"""

        message |> shouldContainText "SystemNative_CreateSocketEventBuffer"
        message |> shouldContainText "leaves the storage the buffer names"
