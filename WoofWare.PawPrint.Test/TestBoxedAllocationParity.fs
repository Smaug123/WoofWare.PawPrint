namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// The runtime's `newobj` allocation helper — the function pointer
/// `RuntimeTypeHandle_GetActivationInfo` hands to `RuntimeType.ActivatorCache`, invoked through
/// `calli` — must produce, for a value type, exactly the boxed representation that the `box`
/// opcode produces for `default(T)`.
///
/// That claim is load-bearing and is not enforced by the type system: the two writes are made
/// by different code (`IlMachineState.allocateUninitialisedInstance` for the allocator,
/// `UnaryMetadataObjectOps.executeBox` for `box`), and the unbox reader
/// (`UnaryMetadataObjectOps`, "three shapes come back out") depends on the shape being one it
/// recognises. A mismatch would not be a crash but a wrong read — a boxed primitive whose
/// payload the unboxer looks for in the wrong place.
///
/// So this asserts it structurally rather than behaviourally: the guest activates and boxes
/// each type in a corpus, and the host compares the resulting heap objects cell for cell. The
/// corpus spans the shapes `box` treats differently — a bare primitive (stored inside a
/// synthetic single-field struct), an enum, a multi-field struct, a nested struct, a struct
/// with a reference field, an `[InlineArray]`, and a generic struct instantiation.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestBoxedAllocationParity =

    let private assy = typeof<RunResult>.Assembly

    /// The value types to check, as C# type expressions. Chosen to span the storage shapes
    /// `box` distinguishes, since it is exactly a disagreement about shape that this pins.
    let private corpus : string list =
        [
            // Bare primitives: `box` stores these inside a synthetic single-field struct.
            "int"
            "long"
            "bool"
            // An enum is primitive-like but nominally its own type.
            "Colour"
            // Genuine multi-field structs, including nesting and a reference field (which
            // makes the storage non-byte-renderable).
            "Plain"
            "Nested"
            "WithReference"
            // `[InlineArray]` storage is N repeats of one declared field.
            "Buffer4"
            // Generic instantiations, whose fields are concretized per instantiation.
            "Generic<int>"
            "Generic<Plain>"
        ]

    let private guestSource : string =
        """
using System;
using System.Runtime.CompilerServices;

public enum Colour { Zero = 0, One = 1 }
public struct Plain { public int X; public long Y; public byte Z; }
public struct Nested { public Plain Inner; public short Tag; }
public struct WithReference { public string S; public int N; }
[InlineArray(4)] public struct Buffer4 { private int _element0; }
public struct Generic<T> { public T Item; public int Count; }

public static class Probe
{
    // Held in statics so both objects stay reachable and the host can find them on the heap
    // after the guest has exited.
    public static object Activated;
    public static object Boxed;
}

public static class Program
{
    public static int Main(string[] args)
    {
        Probe.Activated = Activator.CreateInstance(typeof(TYPE));
        Probe.Boxed = (object)default(TYPE);

        // Neither may be null: a null here would make the host's comparison vacuous.
        if (Probe.Activated == null || Probe.Boxed == null) { return 1; }

        // ... and both must claim the same type, which is the cheap half of the check the
        // host then makes structurally.
        if (Probe.Activated.GetType() != Probe.Boxed.GetType()) { return 2; }

        return 0;
    }
}
"""

    /// Run the guest to completion and hand back its final state and exit code.
    let private runGuest (sourceName : string) (source : string) : IlMachineState * ThreadId * int =
        let image = Roslyn.compile [ source ]

        let messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        try
            match Program.run loggerFactory (Some sourceName) peImage (HostConfig.Default dotnetRuntimes) with
            | RunOutcome.NormalExit (state, thread)
            | RunOutcome.ProcessExit (state, thread) ->
                let exitCode =
                    match state.ThreadState.[thread].MethodState.EvaluationStack.Values with
                    | EvalStackValue.Int32 (Int32Source.Verbatim i) :: _ -> i
                    | other -> failwith $"guest did not return an int exit code: %O{other}"

                state, thread, exitCode
            | other -> failwith $"guest did not exit normally: %O{other}"
        with _ ->
            for message in messages () do
                System.Console.Error.WriteLine $"{message}"

            reraise ()

    /// Read the two probe statics back out of the final state. The `Probe` type is declared by
    /// the guest, so it is found by scanning the loaded assemblies rather than through
    /// `BaseClassTypes`.
    let private probeAddresses (state : IlMachineState) : ManagedHeapAddress * ManagedHeapAddress =
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

        let readStatic (name : string) : ManagedHeapAddress =
            let field = FieldIdentity.requiredOwnStaticField probeType name

            match
                IlMachineState.getStatic
                    StaticOwner.Shared
                    probeHandle
                    (ComparableFieldDefinitionHandle.Make field.Handle)
                    state
            with
            | Some (CliType.ObjectRef (Some addr)) -> addr
            | other -> failwith $"expected `Probe.%s{name}` to hold an object reference, got %O{other}"

        readStatic "Activated", readStatic "Boxed"

    [<TestCaseSource(nameof corpus)>]
    let ``the allocator's boxed value type matches box of default`` (typeExpr : string) =
        let source = guestSource.Replace ("TYPE", typeExpr)
        let sourceName = $"BoxedAllocationParity_%s{typeExpr}.cs"

        let state, _thread, exitCode = runGuest sourceName source

        // The guest's own checks come first: without them the comparison below could be
        // comparing two nulls, or two objects of unrelated types.
        exitCode |> shouldEqual 0

        let activatedAddr, boxedAddr = probeAddresses state

        // Distinct objects — otherwise the comparison is a tautology.
        activatedAddr |> shouldNotEqual boxedAddr

        let activated = ManagedHeap.get activatedAddr state.ManagedHeap
        let boxed = ManagedHeap.get boxedAddr state.ManagedHeap

        activated.ConcreteType |> shouldEqual boxed.ConcreteType

        // The whole point: the *storage*, not just the type, must agree. `CliValueType` has
        // structural equality, so this compares the field cells and their provenance.
        activated.Contents |> shouldEqual boxed.Contents
