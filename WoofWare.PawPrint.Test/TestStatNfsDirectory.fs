namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `SystemNative_Stat`, `LStat` and `FStat` on a directory of an NFS-configured kernel. The kernel
/// refuses to state such a directory's size, and PawPrint turns that refusal into an interpreter
/// abort, which no exit code can assert.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestStatNfsDirectory =

    /// One five-byte file and one directory, on an NFS mount.
    let private nfs : KernelConfig =
        { KernelConfig.Default with
            Mount = Some EmulatedMount.Nfs
            FileSystem =
                let name (s : string) =
                    DirectoryEntryName.parseOrFail "test seed" s

                Map.ofList
                    [
                        name "f",
                        SeedEntry.file (System.Text.Encoding.UTF8.GetBytes "hello" |> ImmutableArray.CreateRange)
                        name "d", SeedEntry.directory Map.empty
                    ]
        }

    /// Raw `SystemNative_Stat`, `LStat`, `FStat` and `Open`, with a struct laid out as
    /// `Interop.Sys.FileStatus`. `{body}` is spliced into `Main`.
    let private guest (body : string) : string =
        $"""
using System;
using System.Runtime.InteropServices;

class Program
{{
    [StructLayout(LayoutKind.Sequential)]
    struct FileStatus
    {{
        public int Flags; public int Mode; public uint Uid; public uint Gid; public long Size;
        public long ATime; public long ATimeNsec; public long MTime; public long MTimeNsec;
        public long CTime; public long CTimeNsec; public long BirthTime; public long BirthTimeNsec;
        public long Dev; public long RDev; public long Ino; public uint UserFlags;
    }}

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Stat", SetLastError = true)]
    static extern unsafe int Stat(byte* path, FileStatus* output);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LStat", SetLastError = true)]
    static extern unsafe int LStat(byte* path, FileStatus* output);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FStat", SetLastError = true)]
    static extern unsafe int FStat(IntPtr fd, FileStatus* output);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    static unsafe int Main(string[] args)
    {{
        byte* d = stackalloc byte[2]; d[0] = (byte)'d'; d[1] = 0;
        byte* f = stackalloc byte[2]; f[0] = (byte)'f'; f[1] = 0;
        FileStatus st;
{body}
    }}
}}
"""

    let private exitCodeOf (outcome : RunOutcome) : int =
        match outcome with
        | RunOutcome.NormalExit (state, _)
        | RunOutcome.ProcessExit (state, _) -> state.LatchedExitCode
        | other -> failwith $"expected the guest to terminate cleanly, got %O{other}"

    let private run (name : string) (source : string) : RunOutcome =
        let image = Roslyn.compile [ source ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes = FrameworkUnderTest.runtimeDirs ()

        use peImage = new MemoryStream (image)

        BoundedRun.run
            loggerFactory
            name
            (Some name)
            peImage
            { HostConfig.Default dotnetRuntimes with
                Guest =
                    { GuestConfig.Default dotnetRuntimes with
                        Kernel = nfs
                    }
            }

    [<TestCase("SystemNative_Stat", "Stat(d, &st);")>]
    [<TestCase("SystemNative_LStat", "LStat(d, &st);")>]
    [<TestCase("SystemNative_FStat", "IntPtr fd = Open(d, 0, 0); if (fd == new IntPtr(-1)) return 1; FStat(fd, &st);")>]
    let ``statting an NFS directory aborts, naming the NFS server`` (entryPoint : string) (call : string) : unit =
        let source = guest $"        %s{call}\n        return 0;"

        let exn =
            Assert.Catch (fun () -> run $"StatNfsDirectory{entryPoint}.cs" source |> ignore<RunOutcome>)

        exn.Message |> shouldContainText entryPoint
        exn.Message |> shouldContainText "NFS server"

    /// The refusal is about directories alone.
    [<Test>]
    let ``statting an NFS file is answered`` () : unit =
        let source =
            guest
                """
        if (Stat(f, &st) != 0 || st.Size != 5) return 1;
        if (LStat(f, &st) != 0 || st.Size != 5) return 2;
        IntPtr fd = Open(f, 0, 0);
        if (fd == new IntPtr(-1)) return 3;
        if (FStat(fd, &st) != 0 || st.Size != 5) return 4;
        return 0;
"""

        run "StatNfsFile.cs" source |> exitCodeOf |> shouldEqual 0
