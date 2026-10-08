namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `SocketFlagsPal` transcribes `pal_networking.h`'s `SocketFlags` and the
/// shim's `ConvertSocketFlagsPalToPlatform`, so its oracles are upstream's
/// source, pinned, and the host's own shim, whose `SystemNative_Receive`
/// answers `Error_ENOTSUP` for exactly the words it cannot convert.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSocketFlagsPal =

    /// The pinned runtime source only exists inside the Nix devshell, so a plain
    /// `dotnet test` in a non-Nix checkout skips rather than fails.
    let private palSource (leaf : string) : string =
        match Environment.GetEnvironmentVariable "DOTNET_RUNTIME_SRC" with
        | null
        | "" ->
            Assert.Ignore
                "DOTNET_RUNTIME_SRC is unset; run under `nix develop` to check against pinned upstream sources."

            failwith "unreachable: Assert.Ignore did not throw"
        | dir ->
            let path = Path.Combine (dir, "src", "native", "libs", "System.Native", leaf)

            if not (File.Exists path) then
                failwith
                    $"TestSocketFlagsPal: expected the pinned PAL networking source at %s{path}. If the sparse checkout in flake.nix no longer includes src/native/libs/System.Native, this transcription has lost its oracle."

            File.ReadAllText path

    /// The `MSG_` name of `flag`, which upstream's PAL names `SocketFlags_MSG_*`
    /// after.
    let private nameOf (flag : MessageFlag) : string = MessageFlag.describe flag

    [<Test>]
    let ``the PAL's SocketFlags are upstream's`` () : unit =
        let pinned =
            Regex.Matches (
                palSource "pal_networking.h",
                @"^\s+SocketFlags_(?<name>MSG_[A-Z]+)\s*=\s*0x(?<value>[0-9A-Fa-f]+),",
                RegexOptions.Multiline
            )
            |> Seq.map (fun m -> m.Groups.["name"].Value, Convert.ToInt32 (m.Groups.["value"].Value, 16))
            |> Map.ofSeq

        SocketFlagsPal.table
        |> List.map (fun (pal, flag) -> nameOf flag, pal)
        |> Map.ofList
        |> shouldEqual pinned

    /// Each `#ifdef MSG_X ... (palFlags & SocketFlags_MSG_X) == 0 ? 0 : MSG_X`
    /// row of the conversion, by name: the table converts exactly those, each
    /// to the flag of its own name.
    [<Test>]
    let ``the shim converts each PAL flag to the flag of its own name`` () : unit =
        let source = palSource "pal_networking.c"
        let signature = "static int8_t ConvertSocketFlagsPalToPlatform"

        let body =
            match source.IndexOf (signature, StringComparison.Ordinal) with
            | -1 -> failwith $"TestSocketFlagsPal: the pinned pal_networking.c no longer declares `%s{signature}`."
            | start ->
                let rest = source.Substring start
                rest.Substring (0, rest.IndexOf ("\n}", StringComparison.Ordinal))

        let rows =
            Regex.Matches (
                body,
                @"\(palFlags\s*&\s*SocketFlags_(?<pal>MSG_[A-Z]+)\)\s*==\s*0\s*\?\s*0\s*:\s*(?<to>MSG_[A-Z]+)"
            )
            |> Seq.map (fun m -> m.Groups.["pal"].Value, m.Groups.["to"].Value)
            |> List.ofSeq

        rows |> List.iter (fun (pal, converted) -> converted |> shouldEqual pal)

        rows
        |> List.map fst
        |> Set.ofList
        |> shouldEqual (SocketFlagsPal.table |> List.map (snd >> nameOf) |> Set.ofList)

    [<Test>]
    let ``a PAL word converts to the flavour's word for its flags, or to none for a bit the flavour cannot take``
        ()
        : unit
        =
        let property (flavour : SimulatedUnixFlavour, word : int) : unit =
            let converted =
                SocketFlagsPal.table
                |> List.filter (fun (pal, _) -> word &&& pal <> 0)
                |> List.map snd

            let known =
                SocketFlagsPal.table
                |> List.filter (fun (_, flag) -> (MessageFlag.number flavour flag).IsSome)
                |> List.fold (fun mask (pal, _) -> mask ||| pal) 0

            let expected =
                if word &&& ~~~known <> 0 then
                    None
                else
                    MessageFlag.encode flavour converted

            SocketFlagsPal.toPlatform flavour word |> shouldEqual expected

        let gen =
            gen {
                let! flavour = Gen.elements [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ]
                let! random = ArbMap.defaults |> ArbMap.generate<int>
                let! table = Gen.subListOf SocketFlagsPal.table
                let ofTable = table |> List.fold (fun word (pal, _) -> word ||| pal) 0
                let! word = Gen.elements [ random ; ofTable ; ofTable ||| (1 <<< (abs random % 32)) ]
                return flavour, word
            }

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 2000, Prop.forAll (Arb.fromGen gen) property)

        // Darwin's header defines no MSG_ERRQUEUE, so its shim leaves the PAL's
        // flag out of the mask.
        SocketFlagsPal.toPlatform SimulatedUnixFlavour.Darwin 0x2000 |> shouldEqual None

        SocketFlagsPal.toPlatform SimulatedUnixFlavour.Linux 0x2000
        |> shouldEqual (Some 0x2000)

        SocketFlagsPal.toPlatform SimulatedUnixFlavour.Linux 0x1002
        |> shouldEqual (Some 0x42)

        SocketFlagsPal.toPlatform SimulatedUnixFlavour.Darwin 0x1002
        |> shouldEqual (Some 0x82)

    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_Receive")>]
    extern int private hostReceive(nativeint socket, byte[] buffer, int bufferLen, int flags, int& received)

    /// `Error_ENOTSUP`, as `pal_error_common.h` numbers it.
    let private palENOTSUP : int = 0x1003D

    /// The host's own shim, given each single bit and a descriptor nothing
    /// holds: `Error_ENOTSUP` for a bit it cannot convert, before it reaches
    /// the descriptor, and the kernel's answer for one it can.
    [<Test>]
    let ``the host's shim refuses exactly the bits SocketFlagsPal cannot convert for its flavour`` () : unit =
        let flavour =
            if OperatingSystem.IsLinux () then
                SimulatedUnixFlavour.Linux
            elif OperatingSystem.IsMacOS () then
                SimulatedUnixFlavour.Darwin
            else
                Assert.Ignore "not a Unix host"
                failwith "unreachable: Assert.Ignore did not throw"

        let buffer = Array.zeroCreate<byte> 4
        let closed = nativeint 1_000_000

        for bit in 0..31 do
            let word = 1 <<< bit
            let mutable received = 0
            let answer = hostReceive (closed, buffer, buffer.Length, word, &received)
            let refused = answer = palENOTSUP

            if refused <> (SocketFlagsPal.toPlatform flavour word).IsNone then
                failwith
                    $"the host's SystemNative_Receive answered 0x%x{answer} for the PAL flag 0x%x{word}, where SocketFlagsPal.toPlatform %O{flavour} says %A{SocketFlagsPal.toPlatform flavour word}"
