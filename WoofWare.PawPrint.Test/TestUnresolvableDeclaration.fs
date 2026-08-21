namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Reflection.Metadata
open System.Runtime.Loader
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// A MethodImpl declaration this rule cannot resolve costs only the slots it could have written.
///
/// The distinction became load-bearing when dispatch started reading the table. Building content
/// resolves every MethodImpl row on every type in the receiver's chain, so treating one unresolvable
/// row as a fact about the whole table would decide what every virtual call on that type does --
/// `ToString` included -- from a row that has nothing to do with any of them. The row's reach is what
/// upstream's search compares: the name exactly, and then a signature that some substitution or type
/// equivalence could still have made equal.
///
/// The shape that makes this matter on a *valid* image -- a MemberRef to an ancestor that merely
/// inherits the named method, which CoreCLR loads by searching that ancestor's bases -- cannot be
/// fabricated here: `DefineMethodOverride` takes a `MethodInfo`, and a `MethodInfo`'s `DeclaringType`
/// is where the method is declared, so the row always names the declaring type. Measured, not assumed.
///
/// What is fabricable reaches the same code path from the other side: a MemberRef on a TypeSpec naming
/// a method that holds no slot. CoreCLR rejects every image here, which the fixture checks rather than
/// assumes; PawPrint does not distinguish "no ancestor declares this" from "an ancestor this rule
/// cannot search declares it", because telling them apart is the search it does not implement.
///
/// The overloads below cover one representative of each kind of claim the bound makes: a shape
/// substitution cannot introduce (byref), a count it cannot change (a parameter, a method type
/// variable), a rigid variable on the declaration's side, two primitives no equivalence relates, and
/// the return column. `couldBeSameType`'s pointer, `Void` and array-rank arms are the same claim as the
/// byref one and are not separately fabricated; every arm it does *not* make is an unconditional
/// `true`, so an unfabricated shape is over-approximated rather than wrongly excluded.
///
/// The calling-convention column has no killing test here and cannot get one: the only fabricable way
/// for two same-named methods of one type to differ in that byte is a vararg, and a vararg member of a
/// generic type is rejected outright -- "Generic code may not be varargs", measured -- while the
/// declaration must live on the generic `AG<T>` for its MemberRef to name a TypeSpec at all. The
/// header's generic bit is already covered by the generic-count comparison beside it.
[<TestFixture>]
module TestUnresolvableDeclaration =

    /// What the MethodImpl row's declaration takes as its one argument. `Nominal` is the fabricated
    /// class `Arg`, and an overload taking the *other* fabricated class cannot then be ruled out: COM
    /// type equivalence relates distinct definitions, and a definition also reaches this walk both as a
    /// TypeRef and as a TypeDef. `Variable` is `AG<T>`'s own type parameter, which a MemberRef does not
    /// have substituted, so only another variable could have become it.
    type private Declaration =
        | Nominal
        | Variable
        /// `int`. Against this, the `M(long)` overload *is* ruled out -- no equivalence relates two
        /// distinct primitives -- while `M(Other)` is not, the same definition reaching this walk as a
        /// primitive from a short encoding and as a token from a long one.
        | Primitive

    /// The overloads `AG<T>` carries besides the one the row names. Against a `Nominal` declaration
    /// each is ruled out of the row's reach by a different invariant except the first, and each of the
    /// others must therefore keep answering.
    type private Overload =
        /// `M(Other)`: a named type against a named type, which nothing here rules out.
        | Named
        /// `M(Arg, int)`: substitution cannot add a parameter.
        | Pair
        /// `M<U>(Other)`: nor a method type variable.
        | Generic
        /// `M(ref Other)`: nor a byref.
        | Ref
        /// `M(int64)`: and no equivalence relates two distinct primitives.
        | OtherPrimitive
        /// `string M(Other)`: the search selects a declaration comparing the return column too.
        | OtherReturn

    let private overloads : Overload list =
        [
            Overload.Named
            Overload.Pair
            Overload.Generic
            Overload.Ref
            Overload.OtherPrimitive
            Overload.OtherReturn
        ]

    /// Does this signature belong to the named overload? Read off the shape rather than the order the
    /// builder emitted them in.
    let private isOverload (overload : Overload) (signature : TypeMethodSignature<TypeDefn>) : bool =
        let returnsString =
            match signature.ReturnType with
            | MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.String) -> true
            | _ -> false

        match overload, signature.GenericParameterCount, signature.ParameterTypes with
        | Overload.OtherReturn, _, _ -> returnsString
        | _, _, _ when returnsString -> false
        | Overload.OtherPrimitive, 0, [ TypeDefn.PrimitiveType PrimitiveType.Int64 ] -> true
        | Overload.Named, 0, [ TypeDefn.Byref _ ]
        | Overload.Named, 0, [ TypeDefn.PrimitiveType _ ] -> false
        | Overload.Named, 0, [ _ ] -> true
        | Overload.Pair, 0, [ _ ; _ ] -> true
        | Overload.Generic, 1, [ _ ] -> true
        | Overload.Ref, 0, [ TypeDefn.Byref _ ] -> true
        | _ -> false

    /// `AG<T>` declares `M` *non-virtually*, taking either `string` or `T`; `BG : AG<int>` declares a
    /// `newslot virtual M` of the same shape carrying `.override AG<int>::M`, which therefore names a
    /// method holding no slot. `CG : BG` exists so the chain has a type below the one carrying the row.
    /// `Plain` carries no MethodImpl at all, as a control.
    ///
    /// With `withOverloads`, `AG<T>` additionally declares the four *virtual* overloads above, which do
    /// take slots. The row resolves to none of them -- its signature matches only the non-virtual `M` --
    /// so which of them it is nonetheless in reach of is exactly what the bound decides.
    let private image (declaration : Declaration) (withOverloads : bool) : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "Unresolvable", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Unresolvable"

        let body (method : MethodBuilder) (value : int) : unit =
            let il = method.GetILGenerator ()
            il.Emit (OpCodes.Ldc_I4, value)
            il.Emit OpCodes.Ret

        // Two argument classes rather than two primitives: the rule under test deliberately declines to
        // rule out one *named* type against another, and two primitives it does rule out.
        let argType = (modul.DefineType ("Arg", TypeAttributes.Public)).CreateType ()
        let otherType = (modul.DefineType ("Other", TypeAttributes.Public)).CreateType ()

        let ag = modul.DefineType ("AG`1", TypeAttributes.Public)
        let tParameter = (ag.DefineGenericParameters [| "T" |]).[0] :> Type

        let declaredType =
            match declaration with
            | Declaration.Nominal -> argType
            | Declaration.Variable -> tParameter
            | Declaration.Primitive -> typeof<int>

        let agMethod =
            ag.DefineMethod ("M", MethodAttributes.Public, typeof<int>, [| declaredType |])

        body agMethod 1

        if withOverloads then
            let virtualAttributes =
                MethodAttributes.Public
                ||| MethodAttributes.Virtual
                ||| MethodAttributes.NewSlot

            body (ag.DefineMethod ("M", virtualAttributes, typeof<int>, [| otherType |])) 5
            body (ag.DefineMethod ("M", virtualAttributes, typeof<int>, [| argType ; typeof<int> |])) 6
            body (ag.DefineMethod ("M", virtualAttributes, typeof<int>, [| otherType.MakeByRefType () |])) 7
            body (ag.DefineMethod ("M", virtualAttributes, typeof<int>, [| typeof<int64> |])) 9

            // Differing only in the return column, which no C# overload set can express and the search
            // nonetheless compares.
            body (ag.DefineMethod ("M", virtualAttributes, typeof<string>, [| otherType |])) 10


            // The generic overload's signature is set after `DefineGenericParameters`, which is the
            // only order the builder accepts.
            let generic = ag.DefineMethod ("M", virtualAttributes)

            generic.DefineGenericParameters [| "U" |]
            |> ignore<GenericTypeParameterBuilder[]>

            generic.SetSignature (typeof<int>, null, null, [| otherType |], null, null)
            body generic 8

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
                [|
                    (match declaration with
                     | Declaration.Nominal -> argType
                     | Declaration.Variable
                     | Declaration.Primitive -> typeof<int>)
                |]
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
            Table : string -> ((VirtualSlotLayout.VtableSlot * int) list * VirtualSlotLayout.SlotOccupant list) option
        }

    let private fixtureFor (description : string) (declaration : Declaration) (withOverloads : bool) : Fixture =
        let image = image declaration withOverloads

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
    let private noOverloads : Fixture =
        fixtureFor "no overloads" Declaration.Variable false

    /// A declaration naming a *named* type, which the `M(Other)` overload cannot be ruled out against.
    let private nominalDeclaration : Fixture =
        fixtureFor "nominal declaration" Declaration.Nominal true

    /// A declaration naming `AG<T>`'s own type parameter, which every overload *can* be ruled out
    /// against -- a MemberRef's `!0` is not substituted, so only another variable could have become it.
    let private variableDeclaration : Fixture =
        fixtureFor "variable declaration" Declaration.Variable true

    /// A declaration naming a primitive, which the `M(int64)` overload *is* ruled out against while
    /// `M(Other)` is not.
    let private primitiveDeclaration : Fixture =
        fixtureFor "primitive declaration" Declaration.Primitive true

    let private fixtures : Fixture list =
        [
            noOverloads
            nominalDeclaration
            variableDeclaration
            primitiveDeclaration
        ]

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
    /// refusing two of them below diverges from nothing that runs, and PawPrint merely *marking* a slot
    /// of the third is the permissive direction on an image no runtime loads. Distinguishing them --
    /// an ancestor that declares the name non-virtually from one whose own bases declare it virtually --
    /// is the hierarchy search this rule does not implement.
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

    /// `CG` inherits the row from `BG`, so building its table resolves that row too -- which is what
    /// made refusing a whole table so expensive, dispatch asking on every `callvirt`.
    let private forBothCarriers (fixture : Fixture) (check : string -> unit) : unit =
        for typeName in [ "BG" ; "CG" ] do
            check typeName

    [<Test>]
    let ``a declaration in reach of no slot at all is refused`` () : unit =
        // Nothing at or above `AG<int>` places a method called `M`, so upstream's search matches no name
        // at any level it visits and no method table exists for `BG` at all.
        forBothCarriers
            noOverloads
            (fun typeName ->
                let thrown = Assert.Throws (fun () -> noOverloads.Table typeName |> ignore<_>)

                thrown.Message |> shouldContainText "could match it under any substitution"
            )

    [<Test>]
    let ``a declaration every overload is ruled out against is refused`` () : unit =
        // The four overloads all bear the name, and the row still cannot have written any of their
        // slots: its `!0` is not substituted, so no named type, no extra parameter, no method type
        // variable and no byref could have become it.
        forBothCarriers
            variableDeclaration
            (fun typeName ->
                let thrown =
                    Assert.Throws (fun () -> variableDeclaration.Table typeName |> ignore<_>)

                thrown.Message |> shouldContainText "could match it under any substitution"
            )

    [<Test>]
    let ``a declaration in reach of one slot costs only that slot`` () : unit =
        // Which overloads each declaration shape leaves in reach, and so which slots it costs. A named
        // type is in reach of a primitive and vice versa -- the same definition reaches this walk as
        // `PrimitiveType` from a short encoding and as a token from a long one -- while two distinct
        // primitives are not.
        let expected =
            [
                nominalDeclaration, [ Overload.Named ; Overload.OtherPrimitive ]
                primitiveDeclaration, [ Overload.Named ]
            ]

        for fixture, inReach in expected do
            forBothCarriers
                fixture
                (fun typeName ->
                    match fixture.Table typeName with
                    | None -> failwith $"expected %s{typeName}'s dispatch table to be built; it declined"
                    | Some (placed, content) ->
                        // The slot each of `AG<T>`'s virtual overloads owns, found through the fabricated
                        // assembly's own metadata so that the expectation is not a second copy of the
                        // computation under test.
                        let slotOf (overload : Overload) : int =
                            let ag =
                                match fixture.Fabricated.TryGetTopLevelTypeDef "" "AG`1" with
                                | None -> failwith "fabricated assembly has no AG`1"
                                | Some typeInfo -> typeInfo

                            let declaration =
                                match
                                    ag.Methods
                                    |> List.filter (fun method ->
                                        method.Name = "M" && method.IsVirtual && isOverload overload method.Signature
                                    )
                                with
                                | [ declaration ] -> declaration
                                | other ->
                                    failwith
                                        $"expected AG`1 to declare one virtual %O{overload}; got %i{List.length other}"

                            let target = fixture.Fabricated.Name.FullName, declaration.IdentityKey

                            match
                                placed
                                |> List.filter (fun (slot, _) ->
                                    (slot.DeclaredBy.AssemblyFullName, slot.Method.IdentityKey) = target
                                )
                            with
                            | [ (_, slot) ] -> slot
                            | other ->
                                failwith $"expected AG`1's %O{overload} to hold one slot; got %i{List.length other}"

                        let unresolved =
                            content
                            |> List.indexed
                            |> List.choose (fun (i, occupant) ->
                                match occupant with
                                | VirtualSlotLayout.SlotOccupant.Unresolved _ -> Some i
                                | VirtualSlotLayout.SlotOccupant.Occupied _ -> None
                            )

                        unresolved |> shouldEqual (inReach |> List.map slotOf |> List.sort)

                        // Named one by one so that a failure says which invariant stopped holding.
                        for overload in overloads |> List.filter (fun overload -> not (List.contains overload inReach)) do
                            match List.tryItem (slotOf overload) content with
                            | Some (VirtualSlotLayout.SlotOccupant.Occupied _) -> ()
                            | other -> failwith $"expected AG's %O{overload} to keep its occupant; got %O{other}"
                )

    [<Test>]
    let ``a type carrying no such row is unaffected`` () : unit =
        // Non-vacuity: the outcomes above are about the row, not about these images being unreadable in
        // general. `Plain` carries no MethodImpl at all.
        for fixture in fixtures do
            match fixture.Table "Plain" with
            | None -> failwith $"expected Plain's dispatch table to be built in the %s{fixture.Description} image"
            | Some (_, content) ->
                content |> List.length |> shouldBeGreaterThan 0

                content
                |> List.iter (fun occupant ->
                    match occupant with
                    | VirtualSlotLayout.SlotOccupant.Occupied _ -> ()
                    | VirtualSlotLayout.SlotOccupant.Unresolved reason ->
                        failwith $"Plain carries no MethodImpl, but a slot of it came back unresolved: %s{reason}"
                )
