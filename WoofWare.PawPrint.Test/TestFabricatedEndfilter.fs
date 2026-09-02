namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open NUnit.Framework

/// `endfilter` (ECMA-335 III.3.34) against the real runtime, over filter results other than 0
/// and 1.
///
/// The spec defines only those two values, and Roslyn only ever emits a `when` clause, whose
/// result is a `bool` normalised to 0 or 1 before the `endfilter`. CoreCLR nonetheless has to
/// answer for every int32 a hand-written filter can leave on the stack, and it does so by
/// comparing the funclet's result with `EXCEPTION_EXECUTE_HANDLER` (which is 1) exactly, in
/// `CallFilterFunclet` in `vm/exceptionhandling.cpp`: a filter that ends `ldc.i4.2; endfilter`
/// declines the exception just as `ldc.i4.0` would. So a filter result is not a truth value, and
/// this fixture pins that down for the values a "non-zero means accept" reading would get wrong.
///
/// The fabricated method takes its filter result as an argument, so one body serves every row,
/// and the driver's expectation is asserted against the host runtime as well as against PawPrint.
[<TestFixture>]
module TestFabricatedEndfilter =

    /// `Filt::Run(int filterResult)`: throws an `InvalidOperationException` inside a `try` whose
    /// filter block pops the exception and then leaves `filterResult` for `endfilter` to judge.
    /// Returns 1 if the handler ran; otherwise the exception escapes to the caller.
    let private fabricate () : byte[] =
        let builder = PersistedAssemblyBuilder (AssemblyName "Filt", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Filt"

        let filt =
            modul.DefineType ("Filt", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let attributes = MethodAttributes.Public ||| MethodAttributes.Static

        let run = filt.DefineMethod ("Run", attributes, typeof<int>, [| typeof<int> |])

        let il = run.GetILGenerator ()

        let handled = il.DeclareLocal typeof<int>

        let exnCtor =
            typeof<InvalidOperationException>.GetConstructor (Array.empty<Type>)
            |> Option.ofObj
            |> Option.defaultWith (fun () -> failwith "InvalidOperationException has no parameterless constructor")

        il.Emit OpCodes.Ldc_I4_0
        il.Emit (OpCodes.Stloc, handled)

        il.BeginExceptionBlock () |> ignore<Label>
        il.Emit (OpCodes.Newobj, exnCtor)
        il.Emit OpCodes.Throw

        // The filter block: discard the exception object, then leave the caller's chosen result
        // on the stack for the `endfilter` that `BeginCatchBlock null` emits.
        il.BeginExceptFilterBlock ()
        il.Emit OpCodes.Pop
        il.Emit OpCodes.Ldarg_0

        // The handler block: discard the exception object and record that the handler ran.
        il.BeginCatchBlock null
        il.Emit OpCodes.Pop
        il.Emit OpCodes.Ldc_I4_1
        il.Emit (OpCodes.Stloc, handled)
        il.EndExceptionBlock ()

        il.Emit (OpCodes.Ldloc, handled)
        il.Emit OpCodes.Ret

        filt.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// Rows are the filter results whose treatment differs between "exactly 1" and "non-zero";
    /// 0 and 1 are there as the two values the spec defines, so a fabrication that stopped
    /// reaching the filter at all fails on them. On the first row whose outcome is not the
    /// CoreCLR one, the driver returns that row's index plus ten, so a disagreement names the
    /// row; a throw of anything other than the fabricated method's own exception returns 1.
    let private driverSource : string =
        """
using System;

public static class Driver
{
    private const int HandlerRan = 1;
    private const int Escaped = 0;
    private const int ReturnedWithoutHandling = 2;

    private static int Outcome(int filterResult)
    {
        try
        {
            return Filt.Run(filterResult) == 1 ? HandlerRan : ReturnedWithoutHandling;
        }
        catch (InvalidOperationException)
        {
            return Escaped;
        }
    }

    public static int Main(string[] args)
    {
        int[] rows = { 0, 1, 2, -1, 256, 0x10000, int.MinValue, int.MaxValue };
        for (int i = 0; i < rows.Length; i++)
        {
            int expected = rows[i] == 1 ? HandlerRan : Escaped;
            int actual;
            try
            {
                actual = Outcome(rows[i]);
            }
            catch (Exception)
            {
                return 1;
            }
            if (actual != expected) return 10 + i;
        }
        return 0;
    }
}
"""

    [<Test>]
    let ``endfilter accepts exactly the result 1, as the real runtime does`` () : unit =
        FabricatedGuest.run "Filt" (fabricate ()) "EndfilterDriver" driverSource 0
