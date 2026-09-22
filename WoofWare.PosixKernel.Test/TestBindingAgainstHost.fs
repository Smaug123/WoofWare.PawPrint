namespace WoofWare.PosixKernel.Test

open System
open System.Diagnostics
open System.IO
open System.Runtime.InteropServices
open NUnit.Framework
open WoofWare.PosixKernel

/// Differential test of which names a binding refuses, and where in the
/// verdict: every row of `BindingProbes` is put to this host's kernel and to
/// the model of its flavour, each in a fresh copy of the probe tree.
///
/// The P/Invokes take `byte[]` rather than `string`, because the names under
/// test are not UTF-8 and a `string` could not carry them.
[<TestFixture>]
module TestBindingAgainstHost =

    [<DllImport("libc", SetLastError = true)>]
    extern int private mkdir(byte[] path, uint32 mode)

    /// Variadic in C; see `TestVirtualFileSystemAgainstHost`'s `open` for why
    /// only the outcome, never the created mode, is asserted on.
    [<DllImport("libc", SetLastError = true)>]
    extern int private ``open``(byte[] path, int flags, int mode)

    [<DllImport("libc", SetLastError = true)>]
    extern int private close(int fd)

    [<DllImport("libc", SetLastError = true)>]
    extern int private access(byte[] path, int mode)

    [<DllImport("libc", SetLastError = true)>]
    extern nativeint private readlink(byte[] path, byte[] buf, nativeint bufsiz)

    [<DllImport("libc", SetLastError = true)>]
    extern int private unlink(byte[] path)

    [<DllImport("libc", SetLastError = true)>]
    extern int private rmdir(byte[] path)

    [<DllImport("libc", SetLastError = true)>]
    extern int private rename(byte[] oldPath, byte[] newPath)

    [<DllImport("libc")>]
    extern uint32 private geteuid()

    /// `O_CREAT`, which differs between the two kernels; `O_WRONLY` is 1 and
    /// `O_RDONLY` 0 on both.
    let private oCreat (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Darwin -> 0x0200
        | SimulatedUnixFlavour.Linux -> 0o100

    /// `root/path` as the NUL-terminated C string a `byte[]` P/Invoke hands over.
    let private cPath (root : string) (path : byte list) : byte[] =
        Array.concat
            [
                PathText.bytes root
                [| UnixPathText.separatorByte |]
                Array.ofList path
                [| 0uy |]
            ]

    let private outcome (result : int64) : int option =
        if result < 0L then
            Some (Marshal.GetLastPInvokeError ())
        else
            None

    let private runHost (flavour : SimulatedUnixFlavour) (root : string) (call : BindingProbeCall) : int option =
        let path = cPath root

        match call with
        | BindingProbeCall.Mkdir p -> outcome (int64 (mkdir (path p, 0o777u)))
        | BindingProbeCall.OpenCreate p
        | BindingProbeCall.OpenRead p ->
            let flags =
                match call with
                | BindingProbeCall.OpenCreate _ -> oCreat flavour ||| 1
                | _ -> 0

            let fd = ``open`` (path p, flags, 0o666)
            let result = outcome (int64 fd)

            if fd >= 0 then
                close fd |> ignore<int>

            result
        | BindingProbeCall.Exists p -> outcome (int64 (access (path p, 0)))
        | BindingProbeCall.ReadLink p -> outcome (int64 (readlink (path p, Array.zeroCreate 4096, 4096n)))
        | BindingProbeCall.Unlink p -> outcome (int64 (unlink (path p)))
        | BindingProbeCall.RmDir p -> outcome (int64 (rmdir (path p)))
        | BindingProbeCall.Rename (source, destination) -> outcome (int64 (rename (path source, path destination)))

    /// Every name in the probe tree is ASCII, so `System.IO` can build it.
    let rec private materialise (directory : string) (entries : Map<DirectoryEntryName, SeedEntry>) : unit =
        for KeyValue (name, entry) in entries do
            let path = Path.Combine (directory, PathText.ofName name)

            match entry with
            | SeedEntry.File (contents, permissions) ->
                File.WriteAllBytes (path, Seq.toArray contents)
                File.SetUnixFileMode (path, enum<UnixFileMode> (PermissionBits.toInt permissions))
            | SeedEntry.Directory (children, permissions) ->
                Directory.CreateDirectory path |> ignore<DirectoryInfo>
                // Children first: an unwritable directory could not be filled afterwards.
                materialise path children
                File.SetUnixFileMode (path, enum<UnixFileMode> (PermissionBits.toInt permissions))
            | SeedEntry.Symlink _ -> failwith "the probe tree holds no symlinks"

    /// Remove a probe tree through the host's own tools rather than `System.IO`:
    /// a row may have bound a name that is not UTF-8, which a `string` cannot
    /// name, and the tree holds directories their owner cannot write.
    let private deleteTree (root : string) : unit =
        let run (program : string) (args : string list) : unit =
            let info = ProcessStartInfo program

            for arg in args do
                info.ArgumentList.Add arg

            use proc = Process.Start info
            proc.WaitForExit ()

            if proc.ExitCode <> 0 then
                failwith $"%s{program} %s{String.Join (' ', args)} exited %d{proc.ExitCode}"

        run "chmod" [ "-R" ; "u+rwX" ; root ]
        run "rm" [ "-rf" ; root ]

    /// Run `call` in a fresh copy of the probe tree on this host.
    let private onHost (flavour : SimulatedUnixFlavour) (call : BindingProbeCall) : int option =
        let root =
            Path.Combine (Path.GetTempPath (), "pawprint-binding-" + Path.GetRandomFileName ())

        Directory.CreateDirectory root |> ignore<DirectoryInfo>

        try
            materialise root BindingProbes.tree
            runHost flavour root call
        finally
            deleteTree root

    /// The model of this host, as the caller this test process is, holding the probe tree.
    let private model (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        let initial = UnixSystem.initial platform

        let system =
            { initial with
                Process =
                    { initial.Process with
                        UserId = geteuid ()
                    }
            }

        match
            UnixSystem.withFileSystemAndCurrentDirectory
                (UnixTimestamp.ofMillisecondsSinceEpoch 0L)
                BindingProbes.tree
                AbsoluteUnixPath.root
                system
        with
        | Ok system -> system
        | Error fault -> failwith $"seeding failed: %A{fault}"

    let private onModel (platform : SimulatedUnixPlatform) (call : BindingProbeCall) : int option =
        BindingProbes.runModel call (model platform)
        |> Option.map (UnixError.toRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering platform))

    [<Test>]
    let ``every probe row agrees with this host's kernel`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            let platform = HostPlatform.platformOf flavour

            let disagreements =
                BindingProbes.rows
                |> List.choose (fun (description, call, _) ->
                    let host = onHost flavour call
                    let modelled = onModel platform call

                    if host = modelled then
                        None
                    else
                        Some $"%s{description}: host %A{host}, model %A{modelled}"
                )

            if not disagreements.IsEmpty then
                failwith (String.Join ("\n", disagreements))
        )

    [<Test>]
    let ``the model's Darwin binds what APFS refuses, and only that`` () : unit =
        // Plan §1.1.1's deliberate over-admission, checked against a real
        // kernel: on APFS each row is EILSEQ where the model succeeds, and on
        // Linux both succeed. A row that starts agreeing on APFS means either
        // the model became faithful or macOS changed; either wants a look.
        HostPlatform.onUnixHost (fun flavour ->
            let platform = HostPlatform.platformOf flavour

            let eilseq =
                UnixError.toRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering platform) UnixError.EILSEQ

            for description, name in BindingProbes.overAdmitted do
                let call = BindingProbeCall.Mkdir (BindingProbes.text "d/" @ name)

                let expectedHost =
                    match flavour with
                    | SimulatedUnixFlavour.Darwin -> Some eilseq
                    | SimulatedUnixFlavour.Linux -> None

                let host = onHost flavour call
                let modelled = onModel platform call

                if host <> expectedHost || modelled.IsSome then
                    failwith $"mkdir(d/%s{description}): host %A{host}, model %A{modelled}"
        )
