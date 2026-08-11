namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Reflection.Metadata.Ecma335
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `MethodTableBuilder::PlaceVirtualMethods` gives an instance virtual that is *not* marked NewSlot
/// and matches nothing in the parent vtable a fresh slot of its own
/// (methodtablebuilder.cpp:5466-5482). No C# compiler emits that shape -- measured, 0 of corelib's
/// 1470 non-generic classes trigger it -- but F# emits it constantly, because that is how it writes
/// the structural equality and comparison members of every union and record.
///
/// Testing the rule needs types no compiler will produce, so this fixture *fabricates* them. Two
/// things make that work where loading a real F# assembly does not:
///
///  - `MethodAttributes.NewSlot` is an opt-in bit, and `PersistedAssemblyBuilder` writes exactly what
///    it is given: `Public, Virtual` stays reuse-slot rather than being silently promoted. So the
///    interesting shape is directly expressible.
///  - Built against `typeof<obj>.Assembly`, the image references System.Private.CoreLib *directly*.
///    That is what the unit harness can resolve. FSharp.Core, whose TypeRefs go through the
///    System.Runtime facade, fails in `getTypeRef` for want of the rest of the framework closure --
///    which is why the natural corpus is unavailable here and fabrication is the way in.
///
/// The oracle is the host CLR: it loads the very same bytes and lays them out in C++ with the real
/// MethodTable builder, and its `RuntimeMethodHandle.GetSlot` is the same function the BCL's
/// `PopulateMethods` calls. Nothing derived from PawPrint's own walk feeds the expected values.
[<TestFixture>]
module TestFabricatedVtableLayout =

    /// The fabricated image, as bytes, so that the host CLR and PawPrint read *the same* assembly
    /// rather than two separately-built ones that might differ.
    ///
    /// Method declaration order is load-bearing throughout: it is the order CoreCLR places slots in,
    /// so it is what the layout assertions are about. `DefineMethod` emits MethodDef rows in call
    /// order.
    let private image : byte array =
        let assemblyBuilder =
            PersistedAssemblyBuilder (AssemblyName "PawPrintFabricatedVtable", typeof<obj>.Assembly)

        let moduleBuilder =
            assemblyBuilder.DefineDynamicModule "PawPrintFabricatedVtable.dll"

        let defineVirtual (typeBuilder : TypeBuilder) (name : string) (isNewSlot : bool) : unit =
            let attributes =
                if isNewSlot then
                    MethodAttributes.Public
                    ||| MethodAttributes.Virtual
                    ||| MethodAttributes.NewSlot
                else
                    MethodAttributes.Public ||| MethodAttributes.Virtual

            let method =
                typeBuilder.DefineMethod (name, attributes, typeof<int>, Type.EmptyTypes)

            let il = method.GetILGenerator ()
            il.Emit (OpCodes.Ldc_I4, 0)
            il.Emit OpCodes.Ret

        // The discriminator. A NewSlot virtual matching nothing, declared *before* a non-NewSlot
        // virtual matching nothing. CoreCLR's single declaration-order pass gives `NewSlotFirst` the
        // lower slot; any scheme that places the fallbacks in a separate pass from the NewSlot
        // methods -- in either order -- disagrees. This is the only shape that tells those apart, and
        // there are none in corelib or FSharp.Core, which is why it is built here.
        let discriminator =
            moduleBuilder.DefineType ("Discriminator", TypeAttributes.Public ||| TypeAttributes.Class, typeof<obj>)

        defineVirtual discriminator "NewSlotFirst" true
        defineVirtual discriminator "FallbackSecond" false
        discriminator.CreateType () |> ignore

        // The mirror image, so that neither ordering can pass by being hardcoded: here the fallback
        // is declared first and must take the lower slot.
        let reversed =
            moduleBuilder.DefineType ("Reversed", TypeAttributes.Public ||| TypeAttributes.Class, typeof<obj>)

        defineVirtual reversed "FallbackFirst" false
        defineVirtual reversed "NewSlotSecond" true
        reversed.CreateType () |> ignore

        // Several fallbacks in a row, pinning their order relative to each other rather than only
        // relative to the NewSlot ones.
        let multiple =
            moduleBuilder.DefineType ("MultipleFallbacks", TypeAttributes.Public ||| TypeAttributes.Class, typeof<obj>)

        defineVirtual multiple "FallbackAlpha" false
        defineVirtual multiple "FallbackBeta" false
        defineVirtual multiple "FallbackGamma" false
        multiple.CreateType () |> ignore

        use stream = new MemoryStream ()
        assemblyBuilder.Save stream
        stream.ToArray ()

    /// The host CLR's copy. `Assembly.Load` of a byte array lands in the default load context, which
    /// is what makes `RuntimeMethodHandle.GetSlot` answer about a fully built MethodTable.
    let private hostAssembly : Assembly = Assembly.Load image

    // Undisposed on purpose, as in TestVirtualMethodSlots: the DumpedAssembly's logger closes over
    // its sinks, and disposing while the assembly is live would drop events.
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

    let private vtableOf (name : string) : NativeRuntimeTypeHelpers.VtableSlot list =
        let state = state ()

        let typeInfo =
            match fabricated.TryGetTopLevelTypeDef "" name with
            | None -> failwith $"fabricated assembly has no type %s{name}"
            | Some typeInfo -> typeInfo

        let state, handle =
            DumpedAssembly.typeInfoToTypeDefn' bct state._LoadedAssemblies typeInfo
            |> IlMachineState.concretizeType
                loggerFactory
                bct
                state
                fabricated.Name
                ImmutableArray.Empty
                ImmutableArray.Empty

        NativeRuntimeTypeHelpers.vtableOfClosed loggerFactory bct "test" state handle
        |> snd

    let private hostSlotOf : MethodInfo -> int =
        let impl =
            typeof<RuntimeMethodHandle>.GetMethods (BindingFlags.NonPublic ||| BindingFlags.Static)
            |> Array.filter (fun candidate ->
                candidate.Name = "GetSlot"
                && candidate.GetParameters().[0].ParameterType.Name = "IRuntimeMethodInfo"
            )
            |> Array.exactlyOne

        fun (method : MethodInfo) -> impl.Invoke ((null : obj), [| box method |]) :?> int

    /// The host's vtable for a fabricated type, slot 0 upwards, named by MethodDef token.
    let private hostLayout (name : string) : int list =
        match hostAssembly.GetType (name, false) with
        | null -> failwith $"host CLR could not load fabricated type %s{name}"
        | t ->
            t.GetMethods (BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
            |> Array.filter _.IsVirtual
            |> Array.map (fun method -> hostSlotOf method, method.MetadataToken)
            |> Array.sortBy fst
            |> Array.map snd
            |> List.ofArray

    let private pawPrintLayout (slots : NativeRuntimeTypeHelpers.VtableSlot list) : int list =
        slots
        |> List.map (fun slot ->
            match fst slot.Method.IdentityKey with
            | Some handle ->
                MetadataTokens.GetToken (
                    System.Reflection.Metadata.MethodDefinitionHandle.op_Implicit handle
                    : System.Reflection.Metadata.EntityHandle
                )
            | None -> -1
        )

    /// The names in slot order, for assertions that read better as names than as tokens.
    let private hostNames (name : string) : string list =
        match hostAssembly.GetType (name, false) with
        | null -> failwith $"host CLR could not load fabricated type %s{name}"
        | t ->
            t.GetMethods (BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
            |> Array.filter _.IsVirtual
            |> Array.map (fun method -> hostSlotOf method, method.Name)
            |> Array.sortBy fst
            |> Array.map snd
            |> List.ofArray

    /// Guards the whole fixture against going vacuous. Every assertion below is about a non-NewSlot
    /// virtual that matches nothing; if `PersistedAssemblyBuilder` ever started setting NewSlot for
    /// us, the fabricated types would be ordinary and every test here would pass while testing
    /// nothing at all.
    [<Test>]
    let ``the fabricated methods really do lack NewSlot`` () : unit =
        let attributesOf (typeName : string) : (string * bool) list =
            match hostAssembly.GetType (typeName, false) with
            | null -> failwith $"host CLR could not load fabricated type %s{typeName}"
            | t ->
                t.GetMethods (BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.DeclaredOnly)
                |> Array.filter _.IsVirtual
                |> Array.map (fun method -> method.Name, method.Attributes.HasFlag MethodAttributes.NewSlot)
                |> List.ofArray

        attributesOf "Discriminator"
        |> shouldEqual [ "NewSlotFirst", true ; "FallbackSecond", false ]

        attributesOf "Reversed"
        |> shouldEqual [ "FallbackFirst", false ; "NewSlotSecond", true ]

        attributesOf "MultipleFallbacks"
        |> shouldEqual [ "FallbackAlpha", false ; "FallbackBeta", false ; "FallbackGamma", false ]

    /// The rule itself: an unmatched non-NewSlot virtual gets a slot rather than being rejected.
    /// Before this was implemented, reaching any of these types was a `failwith`.
    [<Test>]
    let ``an unmatched non-newslot virtual is given a fresh slot`` () : unit =
        for typeName in [ "Discriminator" ; "Reversed" ; "MultipleFallbacks" ] do
            let actual = pawPrintLayout (vtableOf typeName)
            let expected = hostLayout typeName

            if actual <> expected then
                failwith $"%s{typeName}: PawPrint layout %A{actual} disagrees with the host CLR's %A{expected}"

    /// The ordering claim, stated in names so that a failure says which rule broke. CoreCLR runs one
    /// declaration-order pass, so the *declaration* order of the two appended methods decides their
    /// slots -- not which of them is NewSlot. `Discriminator` and `Reversed` declare the same pair in
    /// opposite orders, so a scheme that grouped the fallbacks separately from the NewSlot methods
    /// gets exactly one of these two wrong whichever group it puts first.
    [<Test>]
    let ``appended slots follow declaration order, not NewSlot grouping`` () : unit =
        let objectSlots = 4

        hostNames "Discriminator"
        |> List.skip objectSlots
        |> shouldEqual [ "NewSlotFirst" ; "FallbackSecond" ]

        hostNames "Reversed"
        |> List.skip objectSlots
        |> shouldEqual [ "FallbackFirst" ; "NewSlotSecond" ]

        hostNames "MultipleFallbacks"
        |> List.skip objectSlots
        |> shouldEqual [ "FallbackAlpha" ; "FallbackBeta" ; "FallbackGamma" ]

        // And PawPrint agrees with all three. Asserted through the host's own answer rather than
        // against the literal lists above, so that this stays a differential check.
        for typeName in [ "Discriminator" ; "Reversed" ; "MultipleFallbacks" ] do
            pawPrintLayout (vtableOf typeName) |> shouldEqual (hostLayout typeName)

    /// A fallback slot is an ordinary slot: it sits after every inherited one, and the inherited
    /// prefix is untouched. `Object`'s four slots must still be `Object`'s.
    [<Test>]
    let ``fresh slots are appended after the inherited ones`` () : unit =
        let objectLayout =
            typeof<obj>.GetMethods (BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
            |> Array.filter _.IsVirtual
            |> Array.map (fun method -> hostSlotOf method, method.MetadataToken)
            |> Array.sortBy fst
            |> Array.map snd
            |> List.ofArray

        for typeName in [ "Discriminator" ; "Reversed" ; "MultipleFallbacks" ] do
            let slots = vtableOf typeName
            let layout = pawPrintLayout slots

            List.truncate (List.length objectLayout) layout |> shouldEqual objectLayout

            // Everything past the inherited prefix is declared by the fabricated type itself.
            for slot in List.skip (List.length objectLayout) slots do
                slot.DeclaredBy.Assembly.FullName |> shouldEqual fabricated.Name.FullName
