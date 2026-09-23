namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Which constructor entry points `RuntimeTypeHandle_GetActivationInfo` hands to
/// `RuntimeType.ActivatorCache`, read back off the cache object the guest leaves on the heap.
///
/// A value type's parameterless constructor comes back twice: its boxed entry point (an unboxing
/// stub) in `_pfnRefCtor`, and its unboxed entry point in `_pfnValueCtor`. Measured on CoreCLR,
/// the two addresses differ and only the latter is `RuntimeMethodHandle.GetFunctionPointer`. The
/// guest cannot observe which one PawPrint wrote, because calling either on a box constructs into
/// it, and reflection over a function-pointer field is not yet modelled; so this reads the cache
/// from the host instead.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestActivatorCacheEntryPoints =

    let private guestSource : string =
        """
using System;

public struct ValueWithCtor
{
    public int Value;
    public ValueWithCtor() { Value = 5; }
}

public class ReferenceWithCtor
{
    public int Value;
    public ReferenceWithCtor() { Value = 7; }
}

public static class Program
{
    public static int Main(string[] args)
    {
        if (((ValueWithCtor)Activator.CreateInstance(typeof(ValueWithCtor))).Value != 5) { return 1; }
        if (((ReferenceWithCtor)Activator.CreateInstance(typeof(ReferenceWithCtor))).Value != 7) { return 2; }
        return 0;
    }
}
"""

    let private runGuest () : IlMachineState =
        let sourceName = "ActivatorCacheEntryPoints.cs"
        let image = Roslyn.compile [ guestSource ]

        let messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes = FrameworkUnderTest.runtimeDirs ()

        use peImage = new MemoryStream (image)

        try
            match Program.run loggerFactory (Some sourceName) peImage (HostConfig.Default dotnetRuntimes) with
            | RunOutcome.NormalExit (state, _)
            | RunOutcome.ProcessExit (state, _) ->
                state.LatchedExitCode |> shouldEqual 0
                state
            | other -> failwith $"guest did not exit normally: %O{other}"
        with _ ->
            for message in messages () do
                System.Console.Error.WriteLine $"{message}"

            reraise ()

    let private typeName (state : IlMachineState) (handle : ConcreteTypeHandle) : string =
        match AllConcreteTypes.lookup handle state.ConcreteTypes with
        | None -> failwith $"%O{handle} is not registered"
        | Some ty -> state._LoadedAssemblies.ByDefinitionName(ty.AssemblyFullName).TypeDefs.[ty.Definition.Get].Name

    /// The `(_pfnRefCtor, _pfnValueCtor)` pair of the one `ActivatorCache` whose ref ctor is a
    /// constructor declared on the named guest type.
    let private cacheEntriesFor (state : IlMachineState) (declaringTypeName : string) : CliType * CliType =
        let ctorOf (target : FunctionPointerTarget) =
            match target with
            | FunctionPointerTarget.Managed m
            | FunctionPointerTarget.UnboxingStub m -> Some m
            | _ -> None

        HeapObserver.nonArrayObjects state.ManagedHeap
        |> Seq.map snd
        |> Seq.filter (fun (obj : AllocatedNonArrayObject) -> typeName state obj.ConcreteType = "ActivatorCache")
        |> Seq.map (fun obj ->
            AllocatedNonArrayObject.DereferenceField "_pfnRefCtor" obj,
            AllocatedNonArrayObject.DereferenceField "_pfnValueCtor" obj
        )
        |> Seq.filter (fun (refCtor, _) ->
            match refCtor with
            | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FunctionPointer target)) ->
                match ctorOf target with
                | Some m -> m.RequiredDeclaringType.Name = declaringTypeName
                | None -> false
            | _ -> false
        )
        |> Seq.toList
        |> function
            | [ entries ] -> entries
            | other -> failwith $"expected one ActivatorCache for %s{declaringTypeName}, found %d{other.Length}"

    let private state = lazy (runGuest ())

    [<Test>]
    let ``a value type's ref ctor is its unboxing stub and its value ctor its unboxed entry point`` () : unit =
        let refCtor, valueCtor = cacheEntriesFor state.Value "ValueWithCtor"

        match refCtor, valueCtor with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FunctionPointer (FunctionPointerTarget.UnboxingStub boxed))),
          CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FunctionPointer (FunctionPointerTarget.Managed unboxed))) ->
            boxed.Name |> shouldEqual ".ctor"
            MethodInfo.NominallyEqual boxed unboxed |> shouldEqual true
        | other -> failwith $"expected (UnboxingStub ctor, Managed ctor), got %O{other}"

    [<Test>]
    let ``a reference type's ref ctor is the ctor itself and it has no value ctor`` () : unit =
        let refCtor, valueCtor = cacheEntriesFor state.Value "ReferenceWithCtor"

        match refCtor with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FunctionPointer (FunctionPointerTarget.Managed ctor))) ->
            ctor.Name |> shouldEqual ".ctor"
        | other -> failwith $"expected a Managed ctor, got %O{other}"

        match valueCtor with
        | CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null) -> ()
        | CliType.Numeric (CliNumericType.NativeInt source) when NativeIntSource.isZero source -> ()
        | other -> failwith $"expected a null value ctor, got %O{other}"
