namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// A `safeIntrinsics` row admits an `[Intrinsic]` method's IL only when that IL is a body the row's
/// review covered: each row lists the fingerprints of the bodies it was reviewed against, and the
/// gate refuses a body with any other fingerprint.
///
/// The fixtures below check the table against the CoreLib the suite runs on (and the pinned
/// linux-x64 CoreLib, when present), that the fingerprint responds to an instruction's opcode and to
/// its operand, and that the gate refuses a listed method whose body was altered.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSafeIntrinsicFingerprints =

    let private assy = typeof<RunResult>.Assembly

    let private coreLibFileName : string = "System.Private.CoreLib.dll"

    let private readCoreLib (path : string) : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory path

    let private readImage (bytes : byte[]) : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = new MemoryStream (bytes)
        Assembly.read loggerFactory (Some coreLibFileName) stream

    /// Every audited entry must be reviewed; the failure names each one that is not, in the form a
    /// row's `reviewed` list takes.
    let private assertTableCoversImage (corelib : DumpedAssembly) : unit =
        let audit = IntrinsicMethodKeys.audit corelib

        // A table that named nothing in the image would pass the check below vacuously.
        audit |> List.isEmpty |> shouldEqual false

        let unreviewed = audit |> List.filter (fun entry -> not entry.Reviewed)

        if not unreviewed.IsEmpty then
            let lines =
                unreviewed
                |> List.map (fun entry ->
                    $"  row %d{entry.RowIndex} (%s{entry.Row}): %s{entry.Method} has body %s{entry.Fingerprint.Hex}"
                )
                |> String.concat "\n"

            failwith
                $"safeIntrinsics names bodies in %s{corelib.DefinitionFullName} (%A{corelib.OriginalPath}) that it has not reviewed. Review each body's IL, then add its fingerprint to the row:\n%s{lines}"

    [<Test>]
    let ``every body the table names in the suite's CoreLib is one its row reviewed`` () =
        assertTableCoversImage (readCoreLib typeof<obj>.Assembly.Location)

    [<Test>]
    let ``every body the table names in the pinned linux-x64 CoreLib is one its row reviewed`` () =
        let frameworkDir = LinuxCoreLibFlavour.requireLinuxFramework ()
        assertTableCoversImage (readCoreLib (LinuxCoreLibFlavour.corelibPath frameworkDir))

    [<Test>]
    let ``a listed method is reviewed exactly when its body is among its row's`` () =
        let fingerprints =
            Gen.elements [ 0..7 ]
            |> Gen.map (fun i -> IlBodyFingerprint.OfHex (String.replicate 15 "0" + string<int> i))

        let property (listed : IlBodyFingerprint list) (found : IlBodyFingerprint) : bool =
            let expected =
                if List.contains found listed then
                    IntrinsicMethodKeys.SafeIntrinsicVerdict.Reviewed
                else
                    IntrinsicMethodKeys.SafeIntrinsicVerdict.UnreviewedBody (found, listed)

            IntrinsicMethodKeys.verdict (Some listed) (Some found) = expected

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 500,
            Prop.forAll
                (Arb.fromGen (Gen.listOf fingerprints))
                (fun listed -> Prop.forAll (Arb.fromGen fingerprints) (property listed))
        )

    [<Test>]
    let ``an unlisted method is not listed, and a listed one with no IL body says so`` () =
        let fingerprint = IlBodyFingerprint.OfHex "0123456789abcdef"

        IntrinsicMethodKeys.verdict None (Some fingerprint)
        |> shouldEqual IntrinsicMethodKeys.SafeIntrinsicVerdict.NotListed

        IntrinsicMethodKeys.verdict None None
        |> shouldEqual IntrinsicMethodKeys.SafeIntrinsicVerdict.NotListed

        IntrinsicMethodKeys.verdict (Some [ fingerprint ]) None
        |> shouldEqual IntrinsicMethodKeys.SafeIntrinsicVerdict.ListedWithoutIlBody

    /// An image whose static methods' bodies differ from one another only in one operand each,
    /// in the ways a display rendering of IL loses: two same-named fields of different types, and
    /// string literals that UTF-8 cannot tell apart. `calli` names a signature as a standalone
    /// row, and a one-method image puts it in the same row whatever it says, so that case is one
    /// image per signature.
    let private fabricate (defineMethods : Reflection.Emit.ModuleBuilder -> unit) : DumpedAssembly =
        let assemblyBuilder =
            Reflection.Emit.PersistedAssemblyBuilder (
                Reflection.AssemblyName "PawPrintFingerprintOperands",
                typeof<obj>.Assembly
            )

        let moduleBuilder =
            assemblyBuilder.DefineDynamicModule "PawPrintFingerprintOperands.dll"

        defineMethods moduleBuilder
        use stream = new MemoryStream ()
        assemblyBuilder.Save stream
        readImage (stream.ToArray ())

    let private defineBody
        (typeBuilder : Reflection.Emit.TypeBuilder)
        (name : string)
        (emit : Reflection.Emit.ILGenerator -> unit)
        : unit
        =
        let methodBuilder =
            typeBuilder.DefineMethod (
                name,
                Reflection.MethodAttributes.Public ||| Reflection.MethodAttributes.Static,
                typeof<Void>,
                Type.EmptyTypes
            )

        let il = methodBuilder.GetILGenerator ()
        emit il
        il.Emit Reflection.Emit.OpCodes.Pop
        il.Emit Reflection.Emit.OpCodes.Ret

    let private fingerprintOf (image : DumpedAssembly) (name : string) : IlBodyFingerprint =
        image.Methods.Values
        |> Seq.filter (fun m -> m.Name = name)
        |> Seq.exactlyOne
        |> IlBodyFingerprint.ofMethod image
        |> Option.get

    [<Test>]
    let ``bodies differing only in which same-named field they load have different fingerprints`` () =
        let image =
            fabricate (fun moduleBuilder ->
                let holder =
                    moduleBuilder.DefineType (
                        "Holder",
                        Reflection.TypeAttributes.Public ||| Reflection.TypeAttributes.Class
                    )

                let staticField =
                    Reflection.FieldAttributes.Public ||| Reflection.FieldAttributes.Static

                let asInt = holder.DefineField ("f", typeof<int>, staticField)
                let asLong = holder.DefineField ("f", typeof<int64>, staticField)
                defineBody holder "LoadInt" (fun il -> il.Emit (Reflection.Emit.OpCodes.Ldsfld, asInt))
                defineBody holder "LoadLong" (fun il -> il.Emit (Reflection.Emit.OpCodes.Ldsfld, asLong))
                holder.CreateType () |> ignore<Type>
            )

        fingerprintOf image "LoadInt" |> shouldNotEqual (fingerprintOf image "LoadLong")

    [<Test>]
    let ``bodies differing only in a string literal's unpaired surrogate have different fingerprints`` () =
        // Built from code units: a lone surrogate in an F# literal does not reach the image intact.
        let literals =
            [
                "LoadD800", System.String (char 0xD800, 1)
                "LoadD801", System.String (char 0xD801, 1)
                "LoadFFFD", System.String (char 0xFFFD, 1)
            ]

        let image =
            fabricate (fun moduleBuilder ->
                let holder =
                    moduleBuilder.DefineType (
                        "Holder",
                        Reflection.TypeAttributes.Public ||| Reflection.TypeAttributes.Class
                    )

                for name, literal in literals do
                    defineBody holder name (fun il -> il.Emit (Reflection.Emit.OpCodes.Ldstr, literal))

                holder.CreateType () |> ignore<Type>
            )

        literals
        |> List.map (fst >> fingerprintOf image)
        |> List.distinct
        |> List.length
        |> shouldEqual literals.Length

    [<Test>]
    let ``bodies differing only in their calli signature have different fingerprints`` () =
        let calling (parameters : Type[]) : DumpedAssembly =
            fabricate (fun moduleBuilder ->
                let holder =
                    moduleBuilder.DefineType (
                        "Holder",
                        Reflection.TypeAttributes.Public ||| Reflection.TypeAttributes.Class
                    )

                defineBody
                    holder
                    "CallThrough"
                    (fun il ->
                        il.Emit Reflection.Emit.OpCodes.Ldc_I4_0
                        il.Emit Reflection.Emit.OpCodes.Conv_I

                        il.EmitCalli (
                            Reflection.Emit.OpCodes.Calli,
                            Reflection.CallingConventions.Standard,
                            typeof<int>,
                            parameters,
                            null
                        )
                    )

                holder.CreateType () |> ignore<Type>
            )

        fingerprintOf (calling Type.EmptyTypes) "CallThrough"
        |> shouldNotEqual (fingerprintOf (calling [| typeof<int> |]) "CallThrough")

    /// Where `String.get_Length`'s IL begins in the host CoreLib's bytes, and the field token its
    /// `ldfld` names. The body is `ldarg.0; ldfld String::_stringLength; ret` under a tiny header.
    let private getLengthBody (bytes : byte[]) : int * int * int =
        use peReader = new PEReader (ImmutableArray.Create<byte> bytes)
        let metadata = peReader.GetMetadataReader ()

        let stringType =
            metadata.TypeDefinitions
            |> Seq.map (fun h -> h, metadata.GetTypeDefinition h)
            |> Seq.find (fun (_, t) ->
                metadata.GetString t.Namespace = "System"
                && metadata.GetString t.Name = "String"
            )
            |> snd

        let fieldToken (name : string) : int =
            stringType.GetFields ()
            |> Seq.find (fun h -> metadata.GetString (metadata.GetFieldDefinition h).Name = name)
            |> fun h -> MetadataTokens.GetToken (FieldDefinitionHandle.op_Implicit h : EntityHandle)

        let getLength =
            stringType.GetMethods ()
            |> Seq.map metadata.GetMethodDefinition
            |> Seq.find (fun m -> metadata.GetString m.Name = "get_Length")

        let rva = getLength.RelativeVirtualAddress

        let section =
            peReader.PEHeaders.SectionHeaders
            |> Seq.find (fun s -> rva >= s.VirtualAddress && rva < s.VirtualAddress + s.VirtualSize)

        let headerOffset = rva - section.VirtualAddress + section.PointerToRawData
        // A tiny header is one byte, its low two bits 0b10, its upper six the code size.
        bytes.[headerOffset] &&& 0x3uy |> shouldEqual 0x2uy
        int (bytes.[headerOffset] >>> 2) |> shouldEqual 7
        let code = headerOffset + 1
        bytes.[code] |> shouldEqual 0x02uy // ldarg.0
        bytes.[code + 1] |> shouldEqual 0x7Buy // ldfld
        bytes.[code + 6] |> shouldEqual 0x2Auy // ret

        BitConverter.ToInt32 (bytes, code + 2)
        |> shouldEqual (fieldToken "_stringLength")

        code, fieldToken "_stringLength", fieldToken "_firstChar"

    /// The host CoreLib with `String.get_Length`'s body altered by `patch`, which is given the
    /// offset of the body's first instruction and the `_firstChar` field token.
    let private patchedCoreLib (patch : byte[] -> int -> int -> unit) : byte[] =
        let bytes = File.ReadAllBytes typeof<obj>.Assembly.Location
        let code, _, firstChar = getLengthBody bytes
        patch bytes code firstChar
        bytes

    /// `ldfld String::_firstChar` in place of `ldfld String::_stringLength`: an operand change only.
    let private operandPatch (bytes : byte[]) (code : int) (firstChar : int) : unit =
        BitConverter.GetBytes(firstChar).CopyTo (bytes, code + 2)

    /// `ldarg.1` in place of `ldarg.0`: an opcode change only.
    let private opcodePatch (bytes : byte[]) (code : int) (_ : int) : unit = bytes.[code] <- 0x03uy

    let private getLengthFingerprint (corelib : DumpedAssembly) : IlBodyFingerprint =
        corelib.Methods.Values
        |> Seq.filter (fun m ->
            m.Name = "get_Length"
            && m.RequiredDeclaringType.Namespace = "System"
            && m.RequiredDeclaringType.Name = "String"
        )
        |> Seq.exactlyOne
        |> IlBodyFingerprint.ofMethod corelib
        |> Option.get

    [<Test>]
    let ``the fingerprint of an unaltered body does not depend on which copy of the image it is read from`` () =
        let original = File.ReadAllBytes typeof<obj>.Assembly.Location

        getLengthFingerprint (readImage original)
        |> shouldEqual (getLengthFingerprint (readImage (Array.copy original)))

    [<Test>]
    let ``changing only an instruction's operand changes the fingerprint`` () =
        let original =
            getLengthFingerprint (readImage (File.ReadAllBytes typeof<obj>.Assembly.Location))

        let patched = getLengthFingerprint (readImage (patchedCoreLib operandPatch))
        patched |> shouldNotEqual original

    [<Test>]
    let ``changing only an instruction's opcode changes the fingerprint`` () =
        let original =
            getLengthFingerprint (readImage (File.ReadAllBytes typeof<obj>.Assembly.Location))

        let patched = getLengthFingerprint (readImage (patchedCoreLib opcodePatch))
        patched |> shouldNotEqual original

    let private trivialGuest : string =
        """
public static class Program
{
    public static int Main() { return "abc".Length; }
}
"""

    /// Run the trivial guest on a copy of the host CoreLib altered by `patch`, placed at the head
    /// of the runtime directories so that it is the CoreLib the guest binds.
    let private runOnPatchedCoreLib (patch : byte[] -> int -> int -> unit) : Result<RunOutcome, exn> =
        let dir =
            Path.Combine (Path.GetTempPath (), "PawPrintPatchedCoreLib", Guid.NewGuid().ToString ("N"))

        Directory.CreateDirectory dir |> ignore<DirectoryInfo>

        try
            File.WriteAllBytes (Path.Combine (dir, coreLibFileName), patchedCoreLib patch)

            let runtimeDirs =
                seq {
                    yield dir
                    yield! DotnetRuntime.SelectForDll assy.Location
                }
                |> ImmutableArray.CreateRange

            let image = Roslyn.compile [ trivialGuest ]
            let _, loggerFactory = LoggerFactory.makeTest ()
            use _loggerFactoryResource = loggerFactory
            use peImage = new MemoryStream (image)

            try
                Program.run loggerFactory (Some "TrivialGuest.cs") peImage (HostConfig.Default runtimeDirs)
                |> Ok
            with e ->
                Error e
        finally
            Directory.Delete (dir, true)

    let private assertRefusedAsUnreviewed (outcome : Result<RunOutcome, exn>) : unit =
        match outcome with
        | Ok _ -> failwith "the run interpreted a listed intrinsic whose body its row did not review"
        | Error e ->
            let rec messages (e : exn) : string list =
                if isNull e then
                    []
                else
                    e.Message :: messages e.InnerException

            let message = messages e |> String.concat "\n"

            message |> shouldContainText "System.String.get_Length"

            message
            |> shouldContainText "is listed in safeIntrinsics, but its IL body has fingerprint"

    /// The control for the two refusals below: the same copying, with nothing altered, runs.
    [<Test>]
    let ``the gate admits a listed intrinsic whose body is its row's, from a copied CoreLib`` () =
        match runOnPatchedCoreLib (fun _ _ _ -> ()) with
        | Ok (RunOutcome.NormalExit (state, _)) -> state.LatchedExitCode |> shouldEqual 3
        | Ok other -> failwith $"expected the guest to exit normally, got %O{other}"
        | Error e -> raise e

    [<Test>]
    let ``the gate refuses a listed intrinsic whose body differs from its row's in an operand`` () =
        runOnPatchedCoreLib operandPatch |> assertRefusedAsUnreviewed

    [<Test>]
    let ``the gate refuses a listed intrinsic whose body differs from its row's in an opcode`` () =
        runOnPatchedCoreLib opcodePatch |> assertRefusedAsUnreviewed
