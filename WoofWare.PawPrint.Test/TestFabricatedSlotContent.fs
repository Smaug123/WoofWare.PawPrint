namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Emit
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// The differential oracle for vtable slot *content*: which body a `callvirt` through a given slot
/// actually runs.
///
/// It has to be dispatch on fabricated types, because nothing else can see content. No managed API
/// reports the occupant of a slot -- `RuntimeMethodHandle.GetSlot` answers identity and
/// `MethodInfo.GetBaseDefinition` answers layout -- and corelib cannot stand in for a corpus either:
/// of its 4120 MethodImpl rows, every one either declares an interface method (which writes the
/// dispatch map, not the vtable) or is a non-NewSlot `.override System.Object::Finalize` whose body
/// placement had already put in that very slot. So slot content equals slot identity throughout
/// corelib, and a content walk that resolved no declaration at all would pass a corelib corpus.
///
/// What *is* observable is the thing content decides: for a declaration `D` and a receiver of type
/// `C`, `callvirt D` on a `C` runs exactly `content[slotOf(D)]` of `C`. So each body here returns a
/// tag naming itself, a driver spells the call through one ancestor chosen by argv, and the real
/// runtime says which tag came back. PawPrint is then asked for `slotOf(D)` and for the occupant of
/// that slot, and the two answers must name the same method.
///
/// Reading PawPrint's table directly rather than running the guest is deliberate: this fixture is
/// about the table, and virtual dispatch does not consult it yet.
[<TestFixture>]
module TestFabricatedSlotContent =

    /// One tag per body, so a wrong answer names the body that ran. Kept below 128: a guest's exit
    /// code is 8 bits and `Process.ExitCode` reports a signalled child as `128 + signo`, so a tag at
    /// or above 128 is indistinguishable from a crash.
    let private tags : (string * int) list =
        [
            "A", 11
            "B", 12
            "C", 13
            "D", 14
            "AG", 21
            "BG", 22
            "CG", 23
            "DG", 24
        ]

    let private tagOf (typeName : string) : int =
        tags |> List.find (fun (name, _) -> name = typeName) |> snd

    /// Two hierarchies, each ending in a type reached through every ancestor spelling.
    ///
    /// `A`/`B`/`C`/`D`: `C` introduces a slot of its own *and* claims `A`'s with a MethodImpl, so `A`
    /// and `C` name one slot while `B` names another. That is the aliasing a declaration-keyed rule
    /// cannot see, and it exercises the MethodDef declaration path.
    ///
    /// `AG&lt;T&gt;`/`BG : AG&lt;int&gt;`/`CG`/`DG`: the same shape with a generic base, so `CG`'s MethodImpl
    /// declaration is a MemberRef whose parent is a **TypeSpec**. That is the riskiest path in the
    /// walk -- it has to match `AG&lt;int32&gt;` against the spelling of `BG`'s extends clause -- and the
    /// only one corelib cannot reach at all.
    let private fabricate () : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "SlotContent", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "SlotContent"

        let virt = MethodAttributes.Public ||| MethodAttributes.Virtual
        let newSlot = virt ||| MethodAttributes.NewSlot

        let body (method : MethodBuilder) (tag : int) : unit =
            let il = method.GetILGenerator ()
            il.Emit (OpCodes.Ldc_I4, tag)
            il.Emit OpCodes.Ret

        // --- the non-generic chain ---
        let aBuilder = modul.DefineType ("A", TypeAttributes.Public)
        let aMethod = aBuilder.DefineMethod ("M", newSlot, typeof<int>, Type.EmptyTypes)
        body aMethod (tagOf "A")
        let aType = aBuilder.CreateType ()

        let bBuilder = modul.DefineType ("B", TypeAttributes.Public, aType)
        body (bBuilder.DefineMethod ("M", virt, typeof<int>, Type.EmptyTypes)) (tagOf "B")
        let bType = bBuilder.CreateType ()

        let cBuilder = modul.DefineType ("C", TypeAttributes.Public, bType)
        let cMethod = cBuilder.DefineMethod ("M", newSlot, typeof<int>, Type.EmptyTypes)
        body cMethod (tagOf "C")
        cBuilder.DefineMethodOverride (cMethod, aType.GetMethod "M")
        let cType = cBuilder.CreateType ()

        let dBuilder = modul.DefineType ("D", TypeAttributes.Public, cType)
        body (dBuilder.DefineMethod ("M", virt, typeof<int>, Type.EmptyTypes)) (tagOf "D")
        dBuilder.CreateType () |> ignore<Type>

        // --- the same shape over a generic base ---
        //
        // `M` takes a `T`, which is the whole point: with a parameterless `M` the declaration's
        // signature would mention `T` nowhere, so `AG<!0>::M()` and `AG<int32>::M()` would compare
        // equal whatever substitution the walk applied -- and a walk that dropped the TypeSpec's
        // arguments entirely would pass. Measured: with `M()` the mutant that discards those arguments
        // survives; with `M(T)` it is killed.
        let agBuilder = modul.DefineType ("AG`1", TypeAttributes.Public)
        let agParameters = agBuilder.DefineGenericParameters [| "T" |]
        let tParameter = agParameters.[0] :> Type

        let agMethod = agBuilder.DefineMethod ("M", newSlot, typeof<int>, [| tParameter |])

        body agMethod (tagOf "AG")
        let agType = agBuilder.CreateType ()

        let agClosed = agType.MakeGenericType [| typeof<int> |]

        let bgBuilder = modul.DefineType ("BG", TypeAttributes.Public, agClosed)
        body (bgBuilder.DefineMethod ("M", virt, typeof<int>, [| typeof<int> |])) (tagOf "BG")
        let bgType = bgBuilder.CreateType ()

        let cgBuilder = modul.DefineType ("CG", TypeAttributes.Public, bgType)

        let cgMethod = cgBuilder.DefineMethod ("M", newSlot, typeof<int>, [| typeof<int> |])

        body cgMethod (tagOf "CG")
        // The declaration is `AG<int32>::M(int32)`, so the MethodImpl row's Declaration is a MemberRef
        // whose parent is a TypeSpec rather than a TypeDef, and matching it against `AG<!0>::M(!0)`
        // needs the TypeSpec's arguments.
        cgBuilder.DefineMethodOverride (cgMethod, TypeBuilder.GetMethod (agClosed, agMethod))
        let cgType = cgBuilder.CreateType ()

        let dgBuilder = modul.DefineType ("DG", TypeAttributes.Public, cgType)
        body (dgBuilder.DefineMethod ("M", virt, typeof<int>, [| typeof<int> |])) (tagOf "DG")
        dgBuilder.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    let private image : byte[] = fabricate ()

    /// One process per spelling, with the spelling chosen by argv and the observed tag returned as the
    /// exit code. Folding every spelling into one exit code would not fit: eight tags do not encode in
    /// the seven usable bits, and a hash of them would make a disagreement invisible.
    let private driverSource : string =
        """
public static class Driver
{
    public static int Main(string[] args)
    {
        switch (args[0])
        {
            case "A": return ((A)new D()).M();
            case "B": return ((B)new D()).M();
            case "C": return ((C)new D()).M();
            case "D": return new D().M();
            case "AG": return ((AG<int>)new DG()).M(0);
            case "BG": return ((BG)new DG()).M(0);
            case "CG": return ((CG)new DG()).M(0);
            case "DG": return new DG().M(0);
        }
        return 99;
    }
}
"""

    // Undisposed on purpose, as in the sibling layout fixtures: the DumpedAssembly's logger closes
    // over its sinks, and disposing while the assembly is live would drop events.
    let private corelib : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory (typeof<obj>.Assembly.Location)

    let private fabricated : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.read loggerFactory None (new MemoryStream (image))

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : LoadedAssemblies =
        LoadedAssemblies.ofAssemblies [ corelib ; fabricated ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loaded bct AllConcreteTypes.Empty

    let private loggerFactory = snd (LoggerFactory.makeTest ())

    let private state () : IlMachineState =
        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
            _LoadedAssemblies = loaded
        }

    /// A generic type's metadata name carries its arity, so the spelling `AG` that the driver uses is
    /// `AG`1` in the image.
    let private metadataName (spelling : string) : string =
        if spelling = "AG" then "AG`1" else spelling

    let private identityOfFabricated (spelling : string) : ResolvedTypeIdentity =
        let name = metadataName spelling

        match fabricated.TryGetTopLevelTypeDef "" name with
        | None -> failwith $"fabricated assembly has no type %s{name}"
        | Some typeInfo -> ResolvedTypeIdentity.ofDefinitionInAssembly typeInfo.AssemblyFullName typeInfo.TypeDefHandle

    /// The tag of the body a slot holds, from the description of the type that declared it. A
    /// definition's description carries its arity and an instantiation's carries its arguments, so the
    /// leading identifier is what names the type.
    let private tagOfDeclaringType (description : string) : int =
        let head = (description.Split [| '`' ; '<' ; ' ' ; '[' |]).[0]

        match tags |> List.tryFind (fun (name, _) -> name = head) with
        | Some (_, tag) -> tag
        | None -> failwith $"content occupant was declared by %s{description}, which is not one of the fabricated types"

    /// The real runtime's answer for one spelling: which body `callvirt <spelling>::M` on the
    /// most-derived receiver actually ran.
    let private hostTags : Map<string, int> =
        let driver =
            Roslyn.compileAssembly
                "ContentDriver"
                OutputKind.ConsoleApplication
                [ MetadataReference.CreateFromImage (ImmutableArray.CreateRange image) ]
                [ driverSource ]

        let tempDir = Path.Combine (Path.GetTempPath (), Path.GetRandomFileName ())
        Directory.CreateDirectory tempDir |> ignore<DirectoryInfo>

        try
            File.WriteAllBytes (Path.Combine (tempDir, "SlotContent.dll"), image)
            let driverPath = Path.Combine (tempDir, "ContentDriver.dll")
            File.WriteAllBytes (driverPath, driver)

            tags
            |> List.map (fun (spelling, _) ->
                match RealRuntime.executeAssemblyInPlace [| spelling |] driverPath with
                | RealRuntimeResult.NormalExit code -> spelling, code
                | other -> failwith $"real runtime did not exit normally for spelling %s{spelling}: %O{other}"
            )
            |> Map.ofList
        finally
            try
                if Directory.Exists tempDir then
                    Directory.Delete (tempDir, true)
            with
            | :? IOException
            | :? UnauthorizedAccessException -> ()

    /// `(declaration spelling, most-derived receiver)` pairs, as the driver spells them.
    let private spellings : string list =
        [ "A" ; "B" ; "C" ; "D" ; "AG" ; "BG" ; "CG" ; "DG" ]

    [<TestCaseSource(nameof spellings)>]
    let ``the slot a declaration owns holds the body the real runtime dispatches to`` (spelling : string) : unit =
        let receiver =
            if spelling.EndsWith ("G", StringComparison.Ordinal) then
                "DG"
            else
                "D"

        let hostTag = hostTags.[spelling]

        // Not vacuous, and not self-fulfilling: the host has to have reached *a* declared body, and
        // the interesting spellings are the ones where it is not the receiver's own.
        tags |> List.map snd |> shouldContain hostTag

        let receiverIdentity = identityOfFabricated receiver
        let declarationIdentity = identityOfFabricated spelling

        let state = state ()

        let state, placed =
            VirtualSlotLayout.placedSlotsOfDefinition loggerFactory bct "test" state receiverIdentity

        let _, content =
            VirtualSlotLayout.contentVtableOfDefinition loggerFactory bct "test" state receiverIdentity

        // Which slot does the *declaration* own? That is what a `callvirt` naming it selects.
        let slot =
            placed
            |> List.filter (fun (candidate, _) ->
                candidate.DeclaredBy.Identity = declarationIdentity
                && candidate.Method.Name = "M"
            )
            |> function
                | [ (_, index) ] -> index
                | [] -> failwith $"PawPrint placed no `M` for the declaration on %s{spelling}"
                | several ->
                    let indices = several |> List.map snd |> List.map string |> String.concat ", "
                    failwith $"PawPrint placed %s{spelling}'s `M` at more than one slot: %s{indices}"

        let occupant =
            match List.tryItem slot content with
            | Some occupant -> occupant
            | None ->
                failwith
                    $"PawPrint says %s{spelling}::M owns slot %i{slot}, but %s{receiver}'s content table has only %i{List.length content} slots"

        let pawPrintTag = tagOfDeclaringType occupant.DeclaredBy.Description

        pawPrintTag |> shouldEqual hostTag

    [<Test>]
    let ``the fabrication really does alias two slots`` () : unit =
        // Otherwise every spelling would reach the receiver's own body and the fixture would agree with
        // any content rule whatever. Measured on the host: `B` reaches `D` (one slot, overridden all
        // the way down) while `A` and `C` reach `D` too but *through C's slot*, and the shape is only
        // interesting because `A` and `B` name different slots -- which shows up as the receiver's own
        // body not being the answer for every spelling of the *generic* chain.
        let distinct = spellings |> List.map (fun s -> hostTags.[s]) |> List.distinct

        // At least two different bodies are reached across the eight spellings.
        List.length distinct |> shouldBeGreaterThan 1
