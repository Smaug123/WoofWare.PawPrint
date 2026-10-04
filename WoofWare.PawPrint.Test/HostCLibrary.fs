namespace WoofWare.PawPrint.Test

open System.Runtime.InteropServices
open WoofWare.PawPrint
open WoofWare.PosixKernel
open WoofWare.PosixKernel.Test

/// The C library *this test process's* System.Native shim runs against, in
/// `CLibrary`'s terms: what a guest compared against this host's real runtime
/// gets from `strerror_r`.
[<RequireQualifiedAccess>]
module HostCLibrary =

    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_StrErrorR")>]
    extern nativeint private hostStrErrorR(int platformErrno, nativeint buffer, int bufferSize)

    /// Whether this host's shim answers `number` as `answer` says, for a
    /// 1024-byte buffer.
    let private hostAnswers (number : int) (answer : StrErrorRAnswer) : bool =
        let size = 1024
        let buffer = Marshal.AllocHGlobal size

        try
            Marshal.Copy (Array.create size 0xAAuy, 0, buffer, size)
            let ret = hostStrErrorR (number, buffer, size)

            match answer with
            | StrErrorRAnswer.LibraryText text -> ret <> 0n && ret <> buffer && Marshal.PtrToStringUTF8 ret = text
            | StrErrorRAnswer.Buffer written ->
                let after = Array.zeroCreate<byte> written.Length
                Marshal.Copy (buffer, after, 0, written.Length)
                ret = buffer && after = Seq.toArray written
            | StrErrorRAnswer.NullAfterWriting _
            | StrErrorRAnswer.RefusedSize -> false
        finally
            Marshal.FreeHGlobal buffer

    /// The glibc builds `CLibrary` models.
    let glibcBuilds : CLibrary list =
        [
            CLibrary.Glibc GlibcErrnoSet.ThroughEHWPOISON
            CLibrary.Glibc GlibcErrnoSet.ThroughEFTYPE
        ]

    /// This host's C library, or `None` on a host of no modelled flavour.
    ///
    /// Which glibc build a Linux host runs is asked of the host itself. The
    /// builds differ only in whether they name errno 134, so the host's answer
    /// for 134 picks the one build whose answer it is. A host whose answer is
    /// neither build's fails here, rather than being compared against a model
    /// it does not run.
    let detect () : CLibrary option =
        HostPlatform.flavour ()
        |> Option.map (fun flavour ->
            match CLibrary.ofPlatform (HostPlatform.platformOf flavour) with
            | CLibrary.DarwinLibc -> CLibrary.DarwinLibc
            | CLibrary.Glibc _ ->
                match
                    glibcBuilds
                    |> List.filter (fun library -> hostAnswers 134 (StrErrorR.answer library 134 1024))
                with
                | [ library ] -> library
                | matches ->
                    failwith
                        $"HostCLibrary: this host's glibc answers errno 134 as %d{matches.Length} of the modelled glibc builds (%A{glibcBuilds}) do, rather than as exactly one; model the build this host runs."
        )

    /// `detect ()`, asked once: the host does not change under a running test.
    let private detected : Lazy<CLibrary option> = lazy (detect ())

    /// Whether this host's C library is `library`.
    let runs (library : CLibrary) : bool = detected.Force () = Some library
