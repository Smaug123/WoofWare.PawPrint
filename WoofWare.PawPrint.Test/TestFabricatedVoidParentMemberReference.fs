namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open NUnit.Framework

/// A method MemberRef whose parent is `System.Void`, against the real runtime.
///
/// CoreCLR's `CEEInfo::resolveToken` refuses a token whose type is `ELEMENT_TYPE_VOID` for every
/// kind of token but `ldtoken`'s, and for a member that type is the reference's parent. Looking a
/// method up through a value type finds only the virtual methods it inherits (`MemberLoader::FindMethod`
/// over its method table), so the shape that reaches the refusal is a reference to `ToString`,
/// `GetHashCode`, `Equals` or `Finalize` through `System.Void`. Measured: `call`, `callvirt`, `ldftn`
/// and `ldvirtftn` of one raise `InvalidProgramException`, with that type's default message, where
/// the same reference through `System.Int32` runs; `ldtoken` of one runs.
///
/// No compiler names a member through a type that does not declare it, let alone through
/// `System.Void`, so the image is emitted naming the declaring type and its MemberRef parents are
/// re-pointed afterwards. CoreCLR refuses the method when it compiles it, PawPrint when it reaches
/// the instruction (see docs/divergences.md). Each fabricated method reaches its instruction on
/// every run, with nothing observable before it and no handler of its own, so that difference
/// cannot show.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedVoidParentMemberReference =

    /// What the real runtime does with a fabricated method: return 1, or refuse it.
    type private Expected =
        | Returns
        | Refused

    /// The type a scenario's MemberRef is re-pointed through.
    type private Parent =
        | Void
        | Int32

    let private instanceMethod (declaring : Type) (name : string) (parameters : Type list) : MethodInfo =
        declaring.GetMethod (
            name,
            BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic,
            null,
            Array.ofList parameters,
            null
        )

    let private boxedInt (il : ILGenerator) : unit =
        il.Emit OpCodes.Ldc_I4_7
        il.Emit (OpCodes.Box, typeof<int>)

    /// Each scenario's method name, the method its MemberRef names as emitted, the parent that
    /// MemberRef is re-pointed through, what the real runtime does, and its body, which leaves the
    /// stack empty before the method returns 1. A MemberRef row is shared by every reference to the
    /// same member through the same parent, so no two scenarios name the same member.
    let private scenarios : (string * MethodInfo * Parent * Expected * (ILGenerator -> MethodInfo -> unit)) list =
        [
            "LdftnVoid",
            instanceMethod typeof<ValueType> "ToString" [],
            Parent.Void,
            Expected.Refused,
            fun il target ->
                il.Emit (OpCodes.Ldftn, target)
                il.Emit OpCodes.Pop
            "CallVoid",
            instanceMethod typeof<ValueType> "GetHashCode" [],
            Parent.Void,
            Expected.Refused,
            fun il target ->
                il.Emit (OpCodes.Ldloca, il.DeclareLocal typeof<int>)
                il.Emit (OpCodes.Call, target)
                il.Emit OpCodes.Pop
            "CallvirtVoid",
            instanceMethod typeof<ValueType> "Equals" [ typeof<obj> ],
            Parent.Void,
            Expected.Refused,
            fun il target ->
                boxedInt il
                il.Emit OpCodes.Ldnull
                il.Emit (OpCodes.Callvirt, target)
                il.Emit OpCodes.Pop
            "LdvirtftnVoid",
            instanceMethod typeof<obj> "ToString" [],
            Parent.Void,
            Expected.Refused,
            fun il target ->
                boxedInt il
                il.Emit (OpCodes.Ldvirtftn, target)
                il.Emit OpCodes.Pop
            "LdtokenVoid",
            instanceMethod typeof<obj> "Finalize" [],
            Parent.Void,
            Expected.Returns,
            fun il target ->
                il.Emit (OpCodes.Ldtoken, target)
                il.Emit OpCodes.Pop
            "LdftnInt32",
            instanceMethod typeof<obj> "GetHashCode" [],
            Parent.Int32,
            Expected.Returns,
            fun il target ->
                il.Emit (OpCodes.Ldftn, target)
                il.Emit OpCodes.Pop
            "CallvirtInt32",
            instanceMethod typeof<obj> "Equals" [ typeof<obj> ],
            Parent.Int32,
            Expected.Returns,
            fun il target ->
                boxedInt il
                il.Emit OpCodes.Ldnull
                il.Emit (OpCodes.Callvirt, target)
                il.Emit OpCodes.Pop
        ]

    /// Re-point the parent of each scenario's MemberRef at the TypeRef its scenario names.
    let private repoint (image : byte[]) : byte[] =
        use pe = new PEReader (new MemoryStream (image))
        let reader = pe.GetMetadataReader ()

        let typeRef (name : string) : TypeReferenceHandle =
            reader.TypeReferences
            |> Seq.filter (fun handle ->
                let reference = reader.GetTypeReference handle

                reader.GetString reference.Namespace = "System"
                && reader.GetString reference.Name = name
            )
            |> Seq.exactlyOne

        // MemberRefParent is a coded index over five tables, so its tag is three bits wide
        // (ECMA-335 II.24.2.6), and TypeRef's tag is 1.
        let coded (parent : Parent) : int =
            let handle =
                match parent with
                | Parent.Void -> typeRef "Void"
                | Parent.Int32 -> typeRef "Int32"

            (MetadataTokens.GetRowNumber (TypeReferenceHandle.op_Implicit handle : EntityHandle)
             <<< 3)
            ||| 1

        if reader.GetTableRowCount TableIndex.TypeRef >= (1 <<< 13) then
            failwith "the fabricated image is too large for a two-byte MemberRefParent column"

        let tableStart =
            pe.PEHeaders.MetadataStartOffset
            + reader.GetTableMetadataOffset TableIndex.MemberRef

        let rowSize = reader.GetTableRowSize TableIndex.MemberRef
        let patched = Array.copy image

        for _, target, parent, _, _ in scenarios do
            let rows =
                reader.MemberReferences
                |> Seq.filter (fun handle ->
                    let reference = reader.GetMemberReference handle

                    let declaring =
                        match reference.Parent.Kind with
                        | HandleKind.TypeReference ->
                            let parent =
                                reader.GetTypeReference (TypeReferenceHandle.op_Explicit reference.Parent)

                            reader.GetString parent.Name
                        | _ -> ""

                    reader.GetString reference.Name = target.Name
                    && declaring = target.DeclaringType.Name
                )
                |> List.ofSeq

            match rows with
            | [ handle ] ->
                let row =
                    MetadataTokens.GetRowNumber (MemberReferenceHandle.op_Implicit handle : EntityHandle)

                let offset = tableStart + (row - 1) * rowSize
                let value = coded parent
                patched.[offset] <- byte (value &&& 0xFF)
                patched.[offset + 1] <- byte (value >>> 8)
            | rows ->
                failwith
                    $"expected one MemberRef to %s{target.DeclaringType.Name}::%s{target.Name}, found %d{rows.Length}"

        patched

    /// `static class Probes`, with one `static int` method of no arguments per scenario, and one
    /// more naming `System.Void` and `System.Int32` so that the image has a TypeRef to each.
    let private fabricate () : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "VoidParentMemberReference", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "VoidParentMemberReference"

        let probes =
            modul.DefineType ("Probes", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let define (name : string) (emit : ILGenerator -> unit) : unit =
            let il =
                probes
                    .DefineMethod(name, MethodAttributes.Public ||| MethodAttributes.Static, typeof<int>, [||])
                    .GetILGenerator ()

            emit il
            il.Emit OpCodes.Ldc_I4_1
            il.Emit OpCodes.Ret

        define
            "NamesParents"
            (fun il ->
                for parent in [ typeof<Void> ; typeof<int> ] do
                    il.Emit (OpCodes.Ldtoken, parent)
                    il.Emit OpCodes.Pop
            )

        for name, target, _, _, emit in scenarios do
            define name (fun il -> emit il target)

        probes.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        repoint (image.ToArray ())

    /// Scenario `i` (from 0) answers `2i + 1` where it went the wrong way, and `2i + 2` where it
    /// threw something else, or a refusal whose message is not CoreCLR's.
    let private driverSource : string =
        let checks =
            scenarios
            |> List.mapi (fun i (name, _, _, expected, _) ->
                let helper =
                    match expected with
                    | Expected.Returns -> "Expect"
                    | Expected.Refused -> "ExpectRefused"

                $"        if ((r = %s{helper}(Probes.%s{name}, %d{2 * i + 1})) != 0) return r;"
            )
            |> String.concat "\n"

        $$"""
using System;

public static class Driver
{
    private static int Expect(Func<int> call, int code)
    {
        try
        {
            return call() == 1 ? 0 : code;
        }
        catch (Exception)
        {
            return code + 1;
        }
    }

    // CoreCLR's message is InvalidProgramException's default.
    private static int ExpectRefused(Func<int> call, int code)
    {
        try
        {
            call();
            return code;
        }
        catch (InvalidProgramException e)
        {
            return e.Message == new InvalidProgramException().Message ? 0 : code + 1;
        }
        catch (Exception)
        {
            return code + 1;
        }
    }

    public static int Main(string[] args)
    {
        int r;
{{checks}}
        return 0;
    }
}
"""

    /// The scenario a nonzero exit code of the driver names.
    let private describe (code : int) : string =
        match List.tryItem ((code - 1) / 2) scenarios with
        | Some (name, _, _, expected, _) ->
            let how =
                if code % 2 = 1 then
                    "went the wrong way"
                else
                    "threw something else"

            $"%s{name} (expected %A{expected}) %s{how}"
        | None -> $"exit code %d{code} names no scenario"

    [<Test>]
    let ``a method reference through System.Void is refused as on the real runtime`` () : unit =
        let onHost, onPawPrint =
            FabricatedGuest.runOnBoth
                "VoidParentMemberReference"
                (fabricate ())
                "VoidParentMemberReferenceDriver"
                driverSource

        match onHost with
        | RealRuntimeResult.NormalExit 0 -> ()
        | RealRuntimeResult.NormalExit code -> failwith $"on the real runtime, %s{describe code}"
        | other -> failwith $"real runtime did not exit normally: %O{other}"

        match onPawPrint with
        | FabricatedOutcome.Exited 0 -> ()
        | FabricatedOutcome.Exited code -> failwith $"on PawPrint, %s{describe code}"
        | FabricatedOutcome.Failed e -> raise (Exception ("PawPrint did not run the guest to completion", e))
