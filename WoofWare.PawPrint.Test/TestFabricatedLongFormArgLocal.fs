namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The long forms of the argument and local accessors (`ldarg`, `ldarga`, `starg`, `ldloc`,
/// `ldloca`, `stloc`; ECMA-335 III.3.38-39, III.3.43-44, III.3.63, III.3.62), whose index is an
/// unsigned two-byte operand rather than the short forms' one byte.
///
/// Roslyn emits a long form only for a slot of 255 or more, so the fabricated methods here take 300
/// arguments or declare 300 locals, and address slots on both sides of 256. Each also uses a
/// long form at slot 0, which Roslyn never does. Every method folds the slots it touches into one
/// number chosen so that an index truncated to its low byte (256 read as 0, 299 as 43) produces a
/// different answer.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedLongFormArgLocal =

    let private slotCount : int = 300

    /// `Wide::Poke(ref int)` stores 777 through its argument. The other methods of `Wide` take
    /// `slotCount` int32 arguments, or take one int32 and declare `slotCount` int32 locals; the
    /// comment above each gives its body.
    let private fabricate () : byte[] =
        let builder = PersistedAssemblyBuilder (AssemblyName "Wide", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Wide"

        let wide =
            modul.DefineType ("Wide", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let attributes = MethodAttributes.Public ||| MethodAttributes.Static

        let poke =
            wide.DefineMethod ("Poke", attributes, typeof<Void>, [| typeof<int>.MakeByRefType () |])

        do
            let il = poke.GetILGenerator ()
            il.Emit OpCodes.Ldarg_0
            il.Emit (OpCodes.Ldc_I4, 777)
            il.Emit OpCodes.Stind_I4
            il.Emit OpCodes.Ret

        let manyArguments (name : string) : ILGenerator =
            wide.DefineMethod(name, attributes, typeof<int>, Array.create slotCount typeof<int>).GetILGenerator ()

        let manyLocals (name : string) : ILGenerator =
            let il =
                wide.DefineMethod(name, attributes, typeof<int>, [| typeof<int> |]).GetILGenerator ()

            for _ in 1..slotCount do
                il.DeclareLocal typeof<int> |> ignore<LocalBuilder>

            il

        /// Multiply the top of the stack by `factor` and add the next value `push` leaves.
        let shiftThenAdd (il : ILGenerator) (factor : int) (push : unit -> unit) : unit =
            il.Emit (OpCodes.Ldc_I4, factor)
            il.Emit OpCodes.Mul
            push ()
            il.Emit OpCodes.Add

        // (ldarg 0 * 1000 + ldarg 256) * 1000 + ldarg 299
        do
            let il = manyArguments "Ldarg"
            il.Emit (OpCodes.Ldarg, 0s)
            shiftThenAdd il 1000 (fun () -> il.Emit (OpCodes.Ldarg, 256s))
            shiftThenAdd il 1000 (fun () -> il.Emit (OpCodes.Ldarg, 299s))
            il.Emit OpCodes.Ret

        // starg 299 <- -5; starg 0 <- 9;
        // (ldarg.0 * 1000 + ldarg.s 43) * 10000 + ldarg 299
        do
            let il = manyArguments "Starg"
            il.Emit (OpCodes.Ldc_I4, -5)
            il.Emit (OpCodes.Starg, 299s)
            il.Emit (OpCodes.Ldc_I4, 9)
            il.Emit (OpCodes.Starg, 0s)
            il.Emit OpCodes.Ldarg_0
            shiftThenAdd il 1000 (fun () -> il.Emit (OpCodes.Ldarg_S, 43uy))
            shiftThenAdd il 10000 (fun () -> il.Emit (OpCodes.Ldarg, 299s))
            il.Emit OpCodes.Ret

        // *(ldarga 0) <- 3; Poke(ref ldarga 298);
        // (ldarg.0 * 1000 + ldarg 298) * 1000 + ldarg.s 42
        do
            let il = manyArguments "Ldarga"
            il.Emit (OpCodes.Ldarga, 0s)
            il.Emit (OpCodes.Ldc_I4, 3)
            il.Emit OpCodes.Stind_I4
            il.Emit (OpCodes.Ldarga, 298s)
            il.Emit (OpCodes.Call, poke)
            il.Emit OpCodes.Ldarg_0
            shiftThenAdd il 1000 (fun () -> il.Emit (OpCodes.Ldarg, 298s))
            shiftThenAdd il 1000 (fun () -> il.Emit (OpCodes.Ldarg_S, 42uy))
            il.Emit OpCodes.Ret

        // stloc 0 <- 11; stloc.s 255 <- 13; stloc 256 <- x; stloc 299 <- 17;
        // ((ldloc 0 * 100 + ldloc.s 255) * 1000 + ldloc 256) * 100 + ldloc 299
        do
            let il = manyLocals "Locals"
            il.Emit (OpCodes.Ldc_I4, 11)
            il.Emit (OpCodes.Stloc, 0s)
            il.Emit (OpCodes.Ldc_I4, 13)
            il.Emit (OpCodes.Stloc_S, 255uy)
            il.Emit OpCodes.Ldarg_0
            il.Emit (OpCodes.Stloc, 256s)
            il.Emit (OpCodes.Ldc_I4, 17)
            il.Emit (OpCodes.Stloc, 299s)
            il.Emit (OpCodes.Ldloc, 0s)
            shiftThenAdd il 100 (fun () -> il.Emit (OpCodes.Ldloc_S, 255uy))
            shiftThenAdd il 1000 (fun () -> il.Emit (OpCodes.Ldloc, 256s))
            shiftThenAdd il 100 (fun () -> il.Emit (OpCodes.Ldloc, 299s))
            il.Emit OpCodes.Ret

        // stloc.s 43 <- 4; stloc 298 <- 3; *(ldloca 0) <- 5; *(ldloca 299) <- x;
        // Poke(ref ldloca 297);
        // (((ldloc.0 * 1000 + ldloc 299) * 1000 + ldloc 297) * 10 + ldloc 298) * 10 + ldloc.s 43
        do
            let il = manyLocals "Ldloca"
            il.Emit (OpCodes.Ldc_I4, 4)
            il.Emit (OpCodes.Stloc_S, 43uy)
            il.Emit (OpCodes.Ldc_I4, 3)
            il.Emit (OpCodes.Stloc, 298s)
            il.Emit (OpCodes.Ldloca, 0s)
            il.Emit (OpCodes.Ldc_I4, 5)
            il.Emit OpCodes.Stind_I4
            il.Emit (OpCodes.Ldloca, 299s)
            il.Emit OpCodes.Ldarg_0
            il.Emit OpCodes.Stind_I4
            il.Emit (OpCodes.Ldloca, 297s)
            il.Emit (OpCodes.Call, poke)
            il.Emit OpCodes.Ldloc_0
            shiftThenAdd il 1000 (fun () -> il.Emit (OpCodes.Ldloc, 299s))
            shiftThenAdd il 1000 (fun () -> il.Emit (OpCodes.Ldloc, 297s))
            shiftThenAdd il 10 (fun () -> il.Emit (OpCodes.Ldloc, 298s))
            shiftThenAdd il 10 (fun () -> il.Emit (OpCodes.Ldloc_S, 43uy))
            il.Emit OpCodes.Ret

        wide.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// The long-form instructions in every method body of `image`, in the order they appear.
    let private longFormsIn (image : byte[]) : UnaryConstIlOp list =
        use peReader = new PEReader (new MemoryStream (image))
        let metadataReader = peReader.GetMetadataReader ()

        [
            for handle in metadataReader.MethodDefinitions do
                let methodDef = metadataReader.GetMethodDefinition handle
                let body = peReader.GetMethodBody methodDef.RelativeVirtualAddress

                let instructions =
                    IlDecoding.decodeInstructions (IlTokenUniverse.Metadata (AssemblyName "Wide")) (body.GetILBytes ())

                for instruction, _ in instructions do
                    match instruction with
                    | IlOp.UnaryConst (UnaryConstIlOp.Ldarg _ as op)
                    | IlOp.UnaryConst (UnaryConstIlOp.Ldarga _ as op)
                    | IlOp.UnaryConst (UnaryConstIlOp.Starg _ as op)
                    | IlOp.UnaryConst (UnaryConstIlOp.Ldloc _ as op)
                    | IlOp.UnaryConst (UnaryConstIlOp.Ldloca _ as op)
                    | IlOp.UnaryConst (UnaryConstIlOp.Stloc _ as op) -> yield op
                    | _ -> ()
        ]

    /// Argument i is i + 1, so that no argument is zero and each differs from its neighbours.
    let private arguments : string =
        Seq.init slotCount (fun i -> string<int> (i + 1)) |> String.concat ", "

    /// Returns the index of the first check that fails, and 0 when every check passes.
    let private driverSource : string =
        $$"""
public static class Driver
{
    public static int Main(string[] args)
    {
        if (Wide.Ldarg({{arguments}}) != (1 * 1000 + 257) * 1000 + 300) return 1;
        if (Wide.Starg({{arguments}}) != (9 * 1000 + 44) * 10000 + -5) return 2;
        if (Wide.Ldarga({{arguments}}) != (3 * 1000 + 777) * 1000 + 43) return 3;
        if (Wide.Locals(7) != ((11 * 100 + 13) * 1000 + 7) * 100 + 17) return 4;
        if (Wide.Ldloca(7) != (((5 * 1000 + 7) * 1000 + 777) * 10 + 3) * 10 + 4) return 5;
        return 0;
    }
}
"""

    [<Test>]
    let ``the fabricated bodies carry the long forms`` () : unit =
        // `ILGenerator` writes the opcode it is given, but if it ever chose the short encoding
        // for a small index the other test here would stop covering the long forms without
        // failing.
        longFormsIn (fabricate ())
        |> shouldEqual
            [
                UnaryConstIlOp.Ldarg 0us
                UnaryConstIlOp.Ldarg 256us
                UnaryConstIlOp.Ldarg 299us

                UnaryConstIlOp.Starg 299us
                UnaryConstIlOp.Starg 0us
                UnaryConstIlOp.Ldarg 299us

                UnaryConstIlOp.Ldarga 0us
                UnaryConstIlOp.Ldarga 298us
                UnaryConstIlOp.Ldarg 298us

                UnaryConstIlOp.Stloc 0us
                UnaryConstIlOp.Stloc 256us
                UnaryConstIlOp.Stloc 299us
                UnaryConstIlOp.Ldloc 0us
                UnaryConstIlOp.Ldloc 256us
                UnaryConstIlOp.Ldloc 299us

                UnaryConstIlOp.Stloc 298us
                UnaryConstIlOp.Ldloca 0us
                UnaryConstIlOp.Ldloca 299us
                UnaryConstIlOp.Ldloca 297us
                UnaryConstIlOp.Ldloc 299us
                UnaryConstIlOp.Ldloc 297us
                UnaryConstIlOp.Ldloc 298us
            ]

    [<Test>]
    let ``long-form argument and local accessors address slots above 255`` () : unit =
        FabricatedGuest.run "Wide" (fabricate ()) "WideDriver" driverSource 0
