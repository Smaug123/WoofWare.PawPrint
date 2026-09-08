namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Emit
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// A MethodImpl on a class replaces the *slot's* implementation, and a slot is shared by every
/// declaration in its override chain. So a MethodImpl spelled `.override A::M` also decides what a
/// call spelled `B::M` reaches, when `B.M` overrides `A.M`.
///
/// No C# compiler emits that: Roslyn names the *immediate* parent in the MethodImpls it writes, which
/// are the covariant-return overrides that `sourcesPure/CovariantReturnDispatch*.cs` cover. Reaching
/// the aliasing case therefore needs fabricated IL, which is why this fixture exists rather than
/// another `sourcesPure` guest.
///
/// The shape:
///
///     class A            { public virtual int M() => 1; }        // newslot: introduces the slot
///     class B : A        { public override int M() => 2; }       // same slot as A.M
///     class C : B        { newslot int M() => 3; .override A::M } // its own slot, and writes A's
///     class D : C        { public override int M() => 4; }       // overrides C.M, so writes C's slot
///
/// `MethodTableBuilder` unifies those: D's write to C's slot propagates to the slot A introduced, so
/// every one of `A::M`, `B::M` and `C::M` reaches `D.M` on a `D` receiver. The interesting spelling is
/// `B::M`, which names neither the MethodImpl's declaration nor anything D's own declaration was
/// placed onto -- a dispatch rule keyed on a single declaration answers `B.M` and is wrong.
[<TestFixture>]
module TestFabricatedSlotAliasing =

    /// `A`/`B`/`C`/`D` as above, as a PE image. `PersistedAssemblyBuilder` writes exactly the method
    /// attributes it is given, so `newslot` and reuse-slot are both directly expressible; and being
    /// built against `typeof<obj>.Assembly` the image references System.Private.CoreLib directly,
    /// which is what PawPrint resolves.
    let private fabricate () : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "SlotAliasing", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "SlotAliasing"

        let define
            (name : string)
            (parent : Type option)
            (attrs : MethodAttributes)
            (returns : int)
            : TypeBuilder * MethodBuilder
            =
            let typeBuilder =
                match parent with
                | None -> modul.DefineType (name, TypeAttributes.Public)
                | Some parent -> modul.DefineType (name, TypeAttributes.Public, parent)

            let method = typeBuilder.DefineMethod ("M", attrs, typeof<int>, Type.EmptyTypes)

            let il = method.GetILGenerator ()
            il.Emit (OpCodes.Ldc_I4, returns)
            il.Emit OpCodes.Ret
            typeBuilder, method

        let virt = MethodAttributes.Public ||| MethodAttributes.Virtual
        let newSlot = virt ||| MethodAttributes.NewSlot

        let aBuilder, _ = define "A" None newSlot 1
        let aType = aBuilder.CreateType ()

        let bBuilder, _ = define "B" (Some aType) virt 2
        let bType = bBuilder.CreateType ()

        // C introduces a slot of its own *and* claims A's, which is the aliasing this fixture is for.
        let cBuilder, cMethod = define "C" (Some bType) newSlot 3

        cBuilder.DefineMethodOverride (cMethod, aType.GetMethod "M")
        let cType = cBuilder.CreateType ()

        let dBuilder, _ = define "D" (Some cType) virt 4
        dBuilder.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// A guest that dispatches through each of the three spellings and reports which of them reached
    /// `D.M` as a bitmask, so a failure says *which* spelling was wrong rather than only that one was.
    /// `B::M` is bit 1.
    let private driverSource : string =
        """
public static class Driver
{
    public static int Main()
    {
        D d = new D();
        A viaA = d;
        B viaB = d;
        C viaC = d;

        return (viaA.M() == 4 ? 1 : 0) + (viaB.M() == 4 ? 2 : 0) + (viaC.M() == 4 ? 4 : 0);
    }
}
"""

    /// Lay the fabricated assembly and a C# driver compiled against it side by side, run the driver on
    /// both runtimes, and require they agree.
    ///
    /// The real runtime is the oracle rather than a remembered number, because what is being pinned is
    /// CoreCLR's own slot behaviour. `expectedOnHost` is asserted too, so that a fabrication which
    /// stopped exercising the shape -- every spelling accidentally reaching the same body, say -- fails
    /// here rather than passing vacuously.
    let private runFabricated
        (assemblyName : string)
        (fabricated : byte[])
        (driverName : string)
        (driverSource : string)
        (expectedOnHost : int)
        : unit
        =
        let driver =
            Roslyn.compileAssembly
                driverName
                OutputKind.ConsoleApplication
                [ MetadataReference.CreateFromImage (ImmutableArray.CreateRange fabricated) ]
                [ driverSource ]

        let tempDir = Path.Combine (Path.GetTempPath (), Path.GetRandomFileName ())

        Directory.CreateDirectory tempDir |> ignore<DirectoryInfo>

        try
            File.WriteAllBytes (Path.Combine (tempDir, assemblyName + ".dll"), fabricated)
            let driverPath = Path.Combine (tempDir, driverName + ".dll")
            File.WriteAllBytes (driverPath, driver)

            let expected =
                match RealRuntime.executeAssemblyInPlace [||] driverPath with
                | RealRuntimeResult.NormalExit code -> code
                | other -> failwith $"real runtime did not exit normally: %O{other}"

            expected |> shouldEqual expectedOnHost

            let messages, loggerFactory =
                LoggerFactory.makeTestWithProperties [ "entry_assembly", driverPath ]

            use _loggerFactoryResource = loggerFactory

            let dotnetRuntimeDirs =
                seq {
                    yield tempDir
                    yield! DotnetRuntime.SelectForDll typeof<RunResult>.Assembly.Location
                }
                |> ImmutableArray.CreateRange

            use peImage = new MemoryStream (driver)

            let actual =
                try
                    match
                        Program.run loggerFactory (Some driverPath) peImage (HostConfig.Default dotnetRuntimeDirs)
                    with
                    | RunOutcome.NormalExit (state, _)
                    | RunOutcome.ProcessExit (state, _) -> state.LatchedExitCode
                    | RunOutcome.GuestUnhandledException (_, _, exn) -> failwith $"guest threw: %O{exn.ExceptionObject}"
                    | RunOutcome.Aborted (_, _, fatal) ->
                        let message = fatal.Message |> Option.defaultValue "<none>"
                        failwith $"guest aborted (%O{fatal.Code}): %s{message}"
                    | RunOutcome.SignalTerminated (_, signal) -> failwith $"guest was signalled: %O{signal}"
                with _ ->
                    for message in messages () do
                        Console.Error.WriteLine $"{message}"

                    reraise ()

            actual |> shouldEqual expected
        finally
            try
                if Directory.Exists tempDir then
                    Directory.Delete (tempDir, true)
            with
            | :? IOException
            | :? UnauthorizedAccessException -> ()

    [<Test>]
    let ``a MethodImpl aliasing a base slot decides what a derived spelling reaches`` () : unit =
        runFabricated "SlotAliasing" (fabricate ()) "SlotAliasingDriver" driverSource 7

    /// Two further shapes no C# compiler emits, both of which a declaration-keyed replay gets wrong
    /// if it is careless about *ordering*.
    ///
    ///  - `A1.M`; `B1` both overrides it ordinarily *and* carries `.override A1::M` on a
    ///    differently-named method. `PlaceVirtualMethods` runs before `PlaceMethodImpls`, so the
    ///    MethodImpl body is the one that survives -- measured, `A1::M` on a `B1` answers `B1.X`.
    ///  - `A2.M`/`A2.N`; `B2.M` overrides `A2::M` by placement *and* is aliased into `A2::N`'s slot by
    ///    a MethodImpl; then `C2.M` and `C2.N` override. The two slots must stay separate: `A2::M`
    ///    answers `C2.M` and `A2::N` answers `C2.N`. Distinguishing them needs the slot *indices*
    ///    CoreCLR has, so the slot-replay rule declines this shape rather than picking by metadata
    ///    order, and the pre-existing walk answers it.
    let private fabricateOrdering () : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "SlotOrdering", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "SlotOrdering"

        let method (typeBuilder : TypeBuilder) (name : string) (attrs : MethodAttributes) (returns : int) =
            let method = typeBuilder.DefineMethod (name, attrs, typeof<int>, Type.EmptyTypes)
            let il = method.GetILGenerator ()
            il.Emit (OpCodes.Ldc_I4, returns)
            il.Emit OpCodes.Ret
            method

        let virt = MethodAttributes.Public ||| MethodAttributes.Virtual
        let newSlot = virt ||| MethodAttributes.NewSlot

        let a1 = modul.DefineType ("A1", TypeAttributes.Public)
        method a1 "M" newSlot 1 |> ignore<MethodBuilder>
        let a1Type = a1.CreateType ()

        let b1 = modul.DefineType ("B1", TypeAttributes.Public, a1Type)
        method b1 "M" virt 2 |> ignore<MethodBuilder>
        let b1X = method b1 "X" newSlot 3
        b1.DefineMethodOverride (b1X, a1Type.GetMethod "M")
        b1.CreateType () |> ignore<Type>

        let a2 = modul.DefineType ("A2", TypeAttributes.Public)
        method a2 "M" newSlot 10 |> ignore<MethodBuilder>
        method a2 "N" newSlot 20 |> ignore<MethodBuilder>
        let a2Type = a2.CreateType ()

        let b2 = modul.DefineType ("B2", TypeAttributes.Public, a2Type)
        let b2M = method b2 "M" virt 11
        b2.DefineMethodOverride (b2M, a2Type.GetMethod "N")
        let b2Type = b2.CreateType ()

        let c2 = modul.DefineType ("C2", TypeAttributes.Public, b2Type)
        method c2 "M" virt 12 |> ignore<MethodBuilder>
        method c2 "N" virt 22 |> ignore<MethodBuilder>
        c2.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    let private orderingDriverSource : string =
        """
public static class Driver
{
    public static int Main()
    {
        B1 b1 = new B1();
        A1 viaA1 = b1;
        A2 viaA2 = new C2();

        // bit 0: the MethodImpl body wins over the ordinary override placed in the same slot.
        // bit 1: A2::M reaches C2.M.  bit 2: A2::N reaches C2.N, not C2.M.
        return (viaA1.M() == 3 ? 1 : 0) + (viaA2.M() == 12 ? 2 : 0) + (viaA2.N() == 22 ? 4 : 0);
    }
}
"""

    [<Test>]
    let ``writes to one slot are ordered as the method-table builder orders them`` () : unit =
        runFabricated "SlotOrdering" (fabricateOrdering ()) "SlotOrderingDriver" orderingDriverSource 7

    /// A slot introduced by a *generic* base and aliased by a MethodImpl below it:
    ///
    ///     class AG<T>        { public virtual int M() => 1; }
    ///     class BG : AG<int> { public override int M() => 2; }
    ///     class CG : BG      { newslot int M() => 3; .override AG<int>::M }
    ///     class DG : CG      { public override int M() => 4; }
    ///
    /// `BG::M` on a `DG` reaches `DG.M`, because `CG`'s MethodImpl writes the slot `AG<int>.M`
    /// introduced and `DG.M` then overrides `CG.M`, which unifies back into it.
    ///
    /// The slot replay cannot follow that: seeding upward from `BG.M` reaches `AG<int>`, whose
    /// signatures mention its own type variable, so it cannot say which of that type's declarations
    /// the slot extends into. It therefore declines and the pre-existing walk answers -- correctly.
    /// This case exists because *stopping* there instead of declining is the tempting mistake: the
    /// slot would then be missing `AG<int>.M`, `CG`'s MethodImpl would go unnoticed, and the answer
    /// would be `BG.M`.
    let private fabricateGenericBase () : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "GenericBaseSlot", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "GenericBaseSlot"

        let method (typeBuilder : TypeBuilder) (name : string) (attrs : MethodAttributes) (returns : int) =
            let method = typeBuilder.DefineMethod (name, attrs, typeof<int>, Type.EmptyTypes)
            let il = method.GetILGenerator ()
            il.Emit (OpCodes.Ldc_I4, returns)
            il.Emit OpCodes.Ret
            method

        let virt = MethodAttributes.Public ||| MethodAttributes.Virtual
        let newSlot = virt ||| MethodAttributes.NewSlot

        let openBase = modul.DefineType ("AG", TypeAttributes.Public)

        openBase.DefineGenericParameters [| "T" |]
        |> ignore<GenericTypeParameterBuilder[]>

        method openBase "M" newSlot 1 |> ignore<MethodBuilder>
        let openType = openBase.CreateType ()
        let closedType = openType.MakeGenericType [| typeof<int> |]

        let b = modul.DefineType ("BG", TypeAttributes.Public, closedType)
        method b "M" virt 2 |> ignore<MethodBuilder>
        let bType = b.CreateType ()

        let c = modul.DefineType ("CG", TypeAttributes.Public, bType)
        let cM = method c "M" newSlot 3
        c.DefineMethodOverride (cM, TypeBuilder.GetMethod (closedType, openType.GetMethod "M"))
        let cType = c.CreateType ()

        let d = modul.DefineType ("DG", TypeAttributes.Public, cType)
        method d "M" virt 4 |> ignore<MethodBuilder>
        d.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    let private genericBaseDriverSource : string =
        """
public static class Driver
{
    public static int Main()
    {
        DG d = new DG();
        BG viaB = d;
        return viaB.M();
    }
}
"""

    [<Test>]
    let ``a slot introduced by a generic base is left to the signature walk`` () : unit =
        runFabricated "GenericBaseSlot" (fabricateGenericBase ()) "GenericBaseSlotDriver" genericBaseDriverSource 4

    /// An alias retired by a later direct write:
    ///
    ///     class BR      { virtual int M() => 1; newslot int X() => 2; .override BR::M with X }
    ///     class CR : BR { public override int M() => 3; }
    ///     class DR : CR { public override int X() => 4; }
    ///
    /// `BR::M` on a `DR` reaches `CR.M`. `DR.X` wrote `X`'s own slot, and `CR.M`'s write to `M`'s slot
    /// retired the alias that put `BR.X`'s body there.
    ///
    /// A rule that tracks a slot as a growing set of declarations cannot express retirement -- the set
    /// only ever gains members -- so this shape is declined: the MethodImpl *renames* (Body `X`,
    /// Declaration `M`), and only a shape-preserving alias, which is what a covariant-return override
    /// is, is served. Carrying the alias forward instead answers `DR.X`.
    let private fabricateRetiredAlias () : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "RetiredAlias", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "RetiredAlias"

        let method (typeBuilder : TypeBuilder) (name : string) (attrs : MethodAttributes) (returns : int) =
            let method = typeBuilder.DefineMethod (name, attrs, typeof<int>, Type.EmptyTypes)
            let il = method.GetILGenerator ()
            il.Emit (OpCodes.Ldc_I4, returns)
            il.Emit OpCodes.Ret
            method

        let virt = MethodAttributes.Public ||| MethodAttributes.Virtual
        let newSlot = virt ||| MethodAttributes.NewSlot

        let b = modul.DefineType ("BR", TypeAttributes.Public)
        let bM = method b "M" newSlot 1
        let bX = method b "X" newSlot 2
        b.DefineMethodOverride (bX, bM)
        let bType = b.CreateType ()

        let c = modul.DefineType ("CR", TypeAttributes.Public, bType)
        method c "M" virt 3 |> ignore<MethodBuilder>
        let cType = c.CreateType ()

        let d = modul.DefineType ("DR", TypeAttributes.Public, cType)
        method d "X" virt 4 |> ignore<MethodBuilder>
        d.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    let private retiredAliasDriverSource : string =
        """
public static class Driver
{
    public static int Main()
    {
        DR d = new DR();
        BR viaB = d;
        return viaB.M();
    }
}
"""

    [<Test>]
    let ``an alias retired by a later write is left to the signature walk`` () : unit =
        runFabricated "RetiredAlias" (fabricateRetiredAlias ()) "RetiredAliasDriver" retiredAliasDriverSource 3
