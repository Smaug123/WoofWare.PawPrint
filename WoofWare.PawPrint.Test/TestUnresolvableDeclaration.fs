namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Runtime.Loader
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// A MethodImpl declaration that does not resolve at the ancestor it names is refused loudly, at
/// table-build time, rather than answered approximately.
///
/// Upstream would resolve such a row by searching the named ancestor's own bases
/// (`MethodTableBuilder::FindDeclMethodOnClassInHierarchy`) and by retrying under COM type
/// equivalence; PawPrint implements neither. Refusal is exact about which claim it can make: names
/// are untouched by substitution and equivalence alike, so when nothing at or above the named
/// ancestor even bears the name, the image is one CoreCLR rejects at load, and the message says so;
/// a name match whose signatures do not resolve may be an image CoreCLR loads, and the message says
/// that instead.
///
/// The refusal costs no program that runs. The shape that would make it matter on a *valid* image --
/// a MemberRef to an ancestor that merely inherits the named method -- cannot be fabricated here
/// (`DefineMethodOverride` takes a `MethodInfo`, whose `DeclaringType` is where the method is
/// declared, so the row always names the declaring type; measured, not assumed), and a census over
/// every assembly of the host shared framework and the pinned linux-x64 pack found no MethodImpl row
/// of that shape at all. What *is* fabricable reaches the same code path from the invalid side: a
/// MemberRef on a TypeSpec naming a method that holds no slot, which CoreCLR rejects with
/// `IDS_CLASSLOAD_MI_MUSTBEVIRTUAL` -- checked against the real runtime below rather than assumed.
[<TestFixture>]
module TestUnresolvableDeclaration =

    /// `AG<T>` declares `M` *non-virtually*, taking its own type parameter `T`; `BG : AG<int>`
    /// declares a `newslot virtual M` carrying `.override AG<int>::M`, which therefore names a method
    /// holding no slot. `CG : BG` exists so the chain has a type below the one carrying the row.
    /// `Plain` carries no MethodImpl at all, as a control.
    ///
    /// With `withOverloads`, `AG<T>` additionally declares a virtual `M (Other)`, which does take a
    /// slot: the row still resolves to nothing -- its `!0` parameter matches no overload -- but the
    /// name it declares is now borne by a slot at the named ancestor, which is what selects between
    /// the two refusal messages.
    let private image (withOverloads : bool) : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "Unresolvable", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Unresolvable"

        let body (method : MethodBuilder) (value : int) : unit =
            let il = method.GetILGenerator ()
            il.Emit (OpCodes.Ldc_I4, value)
            il.Emit OpCodes.Ret

        let otherType = (modul.DefineType ("Other", TypeAttributes.Public)).CreateType ()

        let ag = modul.DefineType ("AG`1", TypeAttributes.Public)
        let tParameter = (ag.DefineGenericParameters [| "T" |]).[0] :> Type

        let agMethod =
            ag.DefineMethod ("M", MethodAttributes.Public, typeof<int>, [| tParameter |])

        body agMethod 1

        if withOverloads then
            body
                (ag.DefineMethod (
                    "M",
                    MethodAttributes.Public
                    ||| MethodAttributes.Virtual
                    ||| MethodAttributes.NewSlot,
                    typeof<int>,
                    [| otherType |]
                ))
                5

        let agType = ag.CreateType ()
        let agClosed = agType.MakeGenericType [| typeof<int> |]

        let bg = modul.DefineType ("BG", TypeAttributes.Public, agClosed)

        let bgMethod =
            bg.DefineMethod (
                "M",
                MethodAttributes.Public
                ||| MethodAttributes.Virtual
                ||| MethodAttributes.NewSlot,
                typeof<int>,
                [| typeof<int> |]
            )

        body bgMethod 2
        bg.DefineMethodOverride (bgMethod, TypeBuilder.GetMethod (agClosed, agMethod))
        let bgType = bg.CreateType ()

        let cg = modul.DefineType ("CG", TypeAttributes.Public, bgType)

        body
            (cg.DefineMethod ("M", MethodAttributes.Public ||| MethodAttributes.Virtual, typeof<int>, [| typeof<int> |]))
            3

        cg.CreateType () |> ignore<Type>

        let plain = modul.DefineType ("Plain", TypeAttributes.Public)

        body
            (plain.DefineMethod (
                "M",
                MethodAttributes.Public
                ||| MethodAttributes.Virtual
                ||| MethodAttributes.NewSlot,
                typeof<int>,
                Type.EmptyTypes
            ))
            4

        plain.CreateType () |> ignore<Type>

        use stream = new MemoryStream ()
        builder.Save stream
        stream.ToArray ()

    // Undisposed on purpose, as in the sibling layout fixtures: the DumpedAssembly's logger closes
    // over its sinks, and disposing while the assembly is live would drop events.
    let private corelib : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory (typeof<obj>.Assembly.Location)

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loggerFactory = snd (LoggerFactory.makeTest ())

    /// Everything a slot question about one fabrication needs.
    type private Fixture =
        {
            Description : string
            Image : byte[]
            Fabricated : DumpedAssembly
            /// The dispatch table of the named fabricated type, as a closed runtime type.
            Table : string -> DispatchTable option
        }

    let private fixtureFor (description : string) (withOverloads : bool) : Fixture =
        let image = image withOverloads

        let fabricated =
            let _, loggerFactory = LoggerFactory.makeTest ()
            Assembly.read loggerFactory None (new MemoryStream (image))

        let loaded = LoadedAssemblies.ofAssemblies [ corelib ; fabricated ]
        let concreteTypes = Corelib.concretizeAll loaded bct AllConcreteTypes.Empty

        let state () : IlMachineState =
            { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
                ConcreteTypes = concreteTypes
                _LoadedAssemblies = loaded
            }

        {
            Description = description
            Image = image
            Fabricated = fabricated
            Table =
                fun name ->
                    let typeInfo =
                        match fabricated.TryGetTopLevelTypeDef "" name with
                        | None -> failwith $"fabricated assembly has no type %s{name}"
                        | Some typeInfo -> typeInfo

                    let state, handle =
                        DumpedAssembly.typeInfoToTypeDefn' bct loaded typeInfo
                        |> IlMachineState.concretizeType
                            loggerFactory
                            bct
                            (state ())
                            fabricated.DefinitionFullName
                            ImmutableArray.Empty
                            ImmutableArray.Empty

                    VirtualSlotLayout.dispatchTableOfClosed loggerFactory bct "test" state handle
                    |> snd
        }

    /// No same-name virtual anywhere at or above the named ancestor.
    let private noOverloads : Fixture = fixtureFor "no overloads" false

    /// A same-name virtual at the named ancestor, which the row's signature nonetheless matches no
    /// slot of.
    let private withOverloads : Fixture = fixtureFor "with overloads" true

    let private fixtures : Fixture list = [ noOverloads ; withOverloads ]

    /// What the real runtime does when asked to load one of these types.
    let private realRuntimeLoad (fixture : Fixture) (typeName : string) : exn option =
        let context = AssemblyLoadContext ("probe", true)

        try
            let assembly = context.LoadFromStream (new MemoryStream (fixture.Image))

            try
                let ty = assembly.GetType (typeName, true)
                Activator.CreateInstance ty |> ignore<obj>
                None
            with e ->
                Some e
        finally
            context.Unload ()

    /// The real runtime is the oracle for what these images are, and it rejects all of them: the
    /// declaration is a non-virtual method, which is `IDS_CLASSLOAD_MI_MUSTBEVIRTUAL`. So PawPrint
    /// refusing them diverges from nothing that runs.
    [<Test>]
    let ``the real runtime rejects every image, and loads the control`` () : unit =
        for fixture in fixtures do
            for typeName in [ "BG" ; "CG" ] do
                match realRuntimeLoad fixture typeName with
                | None ->
                    failwith
                        $"expected the real runtime to reject %s{typeName} of the %s{fixture.Description} image; it loaded"
                | Some e ->
                    e.GetType () |> shouldEqual typeof<TypeLoadException>
                    e.Message |> shouldContainText "must be virtual"

            // Non-vacuity: the rejections above are about the row, not about the fabrication being
            // unloadable in general.
            match realRuntimeLoad fixture "Plain" with
            | None -> ()
            | Some e -> failwith $"expected the real runtime to load Plain of the %s{fixture.Description} image: %O{e}"

    /// The fabrication really does emit the shape, checked directly. If a future builder resolved
    /// `TypeBuilder.GetMethod` differently -- to a MethodDef, say -- the outcomes below would still hold
    /// but for another reason, and this fixture would be testing something else.
    [<Test>]
    let ``the fabricated declaration is a MemberRef on a TypeSpec`` () : unit =
        for fixture in fixtures do
            let bg =
                match fixture.Fabricated.TryGetTopLevelTypeDef "" "BG" with
                | None -> failwith "fabricated assembly has no BG"
                | Some typeInfo -> typeInfo

            match bg.MethodImpls.Values |> Seq.map (fun impl -> impl.Declaration) |> List.ofSeq with
            | [ MetadataToken.MemberReference handle ] ->
                match fixture.Fabricated.Members.[handle].Parent with
                | MetadataToken.TypeSpecification _ -> ()
                | other -> failwith $"BG's MethodImpl declaration names its parent with %O{other}, not a TypeSpec"
            | other -> failwith $"expected BG to carry exactly one MemberRef MethodImpl; got %i{List.length other}"

    /// `CG` inherits the row from `BG`, so building its table resolves that row too.
    let private forBothCarriers (fixture : Fixture) (check : string -> unit) : unit =
        for typeName in [ "BG" ; "CG" ] do
            check typeName

    [<Test>]
    let ``a declaration whose name no slot in reach bears is refused as an invalid image`` () : unit =
        // Nothing at or above `AG<int>` places a virtual method called `M` (BG's own `M` is below the
        // named ancestor), so upstream's search matches no name at any level it visits and no method
        // table exists for `BG` at all; the refusal claims as much.
        forBothCarriers
            noOverloads
            (fun typeName ->
                let thrown = Assert.Throws (fun () -> noOverloads.Table typeName |> ignore<_>)

                thrown.Message |> shouldContainText "places a virtual method of that name"
                thrown.Message |> shouldContainText "MI_DECLARATIONNOTFOUND"
            )

    [<Test>]
    let ``a declaration that does not resolve at the named ancestor is refused as unimplemented`` () : unit =
        // `AG<T>`'s virtual `M (Other)` bears the name, so upstream's search would compare signatures
        // and climb -- the search PawPrint does not implement -- and the refusal must not claim the
        // image is invalid, because on this evidence a valid image is possible.
        forBothCarriers
            withOverloads
            (fun typeName ->
                let thrown = Assert.Throws (fun () -> withOverloads.Table typeName |> ignore<_>)

                thrown.Message |> shouldContainText "FindDeclMethodOnClassInHierarchy"
            )

    [<Test>]
    let ``a type carrying no such row is unaffected`` () : unit =
        // Non-vacuity: the refusals above are about the row, not about these images being unreadable in
        // general. `Plain` carries no MethodImpl at all, and its chain does not include the carrier.
        for fixture in fixtures do
            match fixture.Table "Plain" with
            | None -> failwith $"expected Plain's dispatch table to be built in the %s{fixture.Description} image"
            | Some table -> table.Occupants.Length |> shouldBeGreaterThan 0

    /// Source compatibility for the two types that moved to the namespace. Both were public API of a
    /// shipped package and nested in `VirtualSlotLayout`; these are the qualified names an existing
    /// consumer wrote, and this file failing to compile is the regression.
    ///
    /// It cannot check *binary* compatibility: an abbreviation is not a distinct CLR type, so a
    /// consumer compiled against the previous package must be recompiled.
    let private _sourceCompatibility (slot : VirtualSlotLayout.VtableSlot) (owner : VirtualSlotLayout.SlotOwner) =
        slot.Method.Name, owner.Identity
