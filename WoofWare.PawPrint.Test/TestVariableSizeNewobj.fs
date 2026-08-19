namespace WoofWare.Pawprint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PawPrint.Test

/// `newobj` on `System.String` is the CLR's variable-size-object case
/// (`CORINFO_FLG_VAROBJSIZE`, set because String's MethodTable
/// `HasComponentSize`): the runtime allocates nothing up front and passes no
/// `this`; the constructor allocates and *returns* the object. See
/// `src/coreclr/jit/importer.cpp` ("At present this can only be String",
/// `newObjThisPtr = nullptr`) and `src/coreclr/interpreter/compiler.cpp`
/// (`doCallInsteadOfNew = true`).
///
/// PawPrint realises it the way CoreCLR does rather than by special-casing the
/// allocation: `ecall.cpp`'s `PopulateManagedStringConstructors` points each of
/// the nine `String` constructors at the managed static `String.Ctor` of matching
/// signature, which allocates and returns the string, so `executeNewobj` redirects
/// to that static and pushes its return value.
///
/// These tests pin the observable consequence: `newobj String::.ctor(...)` must
/// not leave a half-built placeholder String on the managed heap.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestVariableSizeNewobj =
    let private assy = typeof<RunResult>.Assembly

    /// Is this the concrete handle for `System.String`?
    let private isSystemString (state : IlMachineState) (handle : ConcreteTypeHandle) : bool =
        match AllConcreteTypes.lookup handle state.ConcreteTypes with
        | None -> false
        | Some ct ->
            AssemblyDefinitionName.isNamed "System.Private.CoreLib" ct.AssemblyFullName
            && ct.Namespace = "System"
            && ct.Name = "String"
            && ct.Generics.IsEmpty

    /// PawPrint's String representation is split: the managed object carries only
    /// `_stringLength`, while the characters (including the metadata-level
    /// `_firstChar`) live in `ManagedHeap.StringArrayData`, reached through the
    /// `StringContents` and `StringDataOffsets` side-tables. Every String-typed
    /// heap object is therefore required to appear in both side-tables --
    /// `ManagedHeap.getStringChar`, `setStringChar`, `stringsEqual` and
    /// `RuntimeFieldProjection`'s `_firstChar` handling all fail loudly otherwise.
    ///
    /// Returns the addresses that violate that invariant, with which side-table
    /// each is missing from.
    let private unregisteredStringObjects (state : IlMachineState) : (ManagedHeapAddress * string) list =
        HeapObserver.nonArrayObjects state.ManagedHeap
        |> List.choose (fun (addr, object) ->
            if not (isSystemString state object.ConcreteType) then
                None
            else

            let hasContents = (ManagedHeap.getStringContents addr state.ManagedHeap).IsSome

            let hasDataOffset =
                (ManagedHeap.tryGetStringDataOffset addr state.ManagedHeap).IsSome

            if hasContents && hasDataOffset then
                None
            else
                Some (addr, $"StringContents=%b{hasContents}, StringDataOffsets=%b{hasDataOffset}")
        )

    let private countStringObjects (state : IlMachineState) : int =
        HeapObserver.nonArrayObjects state.ManagedHeap
        |> List.filter (fun (_, object) -> isSystemString state object.ConcreteType)
        |> List.length

    /// Compile `source` and run it through PawPrint, returning the final machine
    /// state. Fails the test if the guest does not exit cleanly with code 0.
    let private runToNormalExit (sourceName : string) (source : string) : IlMachineState =
        let image = Roslyn.compile [ source ]

        let messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        try
            match Program.run loggerFactory (Some sourceName) peImage (HostConfig.Default dotnetRuntimes) with
            | RunOutcome.NormalExit (state, terminatingThread) ->
                match state.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                | EvalStackValue.Int32 (Int32Source.Verbatim 0) :: _ -> ()
                | other -> failwith $"%s{sourceName}: expected guest exit code 0, got %O{other}"

                state
            | other -> failwith $"%s{sourceName}: expected a normal exit, got %O{other}"
        with _ ->
            for message in messages () do
                System.Console.Error.WriteLine $"{message}"

            reraise ()

    let private charPointerCtorSource =
        """
using System;

unsafe class VarObjSizeNewobjCharPointer
{
    static int Main(string[] args)
    {
        char[] chars = { 'h', 'e', 'l', 'l', 'o', '\0' };
        fixed (char* p = chars)
        {
            string s = new string(p);
            if (s != "hello")
                return 1;
        }

        // The null and empty cases both return the canonical interned empty
        // string, so they must not allocate a String object at all.
        string fromNull = new string((char*)null);
        if (fromNull.Length != 0)
            return 2;
        if (!ReferenceEquals(fromNull, ""))
            return 3;

        return 0;
    }
}
"""

    let private spanCtorSource =
        """
using System;

unsafe class VarObjSizeNewobjSpan
{
    static int Main(string[] args)
    {
        char* buf = stackalloc char[3];
        buf[0] = 'a';
        buf[1] = 'b';
        buf[2] = 'c';

        string s = new string(new ReadOnlySpan<char>(buf, 3));
        if (s != "abc")
            return 1;

        // Zero length collapses onto the interned empty string, allocating nothing.
        string empty = new string(new ReadOnlySpan<char>(buf, 0));
        if (!ReferenceEquals(empty, ""))
            return 2;

        return 0;
    }
}
"""

    [<Test>]
    let ``newobj String..ctor(char*) leaves no unregistered String on the heap`` () : unit =
        let state = runToNormalExit "VarObjSizeNewobjCharPointer.cs" charPointerCtorSource

        unregisteredStringObjects state |> shouldEqual []

    [<Test>]
    let ``newobj String..ctor(ReadOnlySpan<char>) leaves no unregistered String on the heap`` () : unit =
        let state = runToNormalExit "VarObjSizeNewobjSpan.cs" spanCtorSource

        unregisteredStringObjects state |> shouldEqual []

    /// The BCL allocates strings of its own during startup, so an absolute count is
    /// not assertable. A *differential* count is: these two programs are identical
    /// except that the second performs two extra `newobj String::.ctor(char*)`
    /// instructions, so the heap must end up with exactly two more String objects.
    /// A `newobj` that allocated a placeholder as well would cost *two* String
    /// objects per extra `newobj`, giving a delta of 4.
    let private oneNewobjSource =
        """
using System;

unsafe class VarObjSizeNewobjOne
{
    static int Main(string[] args)
    {
        char[] chars = { 'h', 'e', 'l', 'l', 'o', '\0' };
        fixed (char* p = chars)
        {
            string s = new string(p);
            if (s != "hello")
                return 1;
            if (s.Length != 5)
                return 2;
            if (s.Length != 5)
                return 3;
        }

        return 0;
    }
}
"""

    let private threeNewobjSource =
        """
using System;

unsafe class VarObjSizeNewobjThree
{
    static int Main(string[] args)
    {
        char[] chars = { 'h', 'e', 'l', 'l', 'o', '\0' };
        fixed (char* p = chars)
        {
            string s = new string(p);
            string t = new string(p);
            string u = new string(p);
            if (s != "hello")
                return 1;
            if (t.Length != 5)
                return 2;
            if (u.Length != 5)
                return 3;
        }

        return 0;
    }
}
"""

    [<Test>]
    let ``each extra String newobj allocates exactly one String`` () : unit =
        let one =
            runToNormalExit "VarObjSizeNewobjOne.cs" oneNewobjSource |> countStringObjects

        let three =
            runToNormalExit "VarObjSizeNewobjThree.cs" threeNewobjSource
            |> countStringObjects

        three - one |> shouldEqual 2
