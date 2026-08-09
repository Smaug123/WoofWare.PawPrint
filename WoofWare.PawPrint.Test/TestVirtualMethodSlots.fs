namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `NativeRuntimeTypeHelpers.vtableOfClosed` is the single definition of "which vtable slot" in
/// PawPrint: `RuntimeMethodHandle.GetSlot` is an index into it and `RuntimeTypeHandle.GetNumVirtuals`
/// is its length. The end-to-end coverage is `sourcesPure/ReflectionVirtualMethodSlots.cs`, which
/// pins the shapes a hand-written matcher gets wrong (overloads where only one is overridden,
/// covariant returns, `new virtual` shadows, reabstraction, generic substitution). What that file
/// cannot do is range over a large corpus, because its expectations have to be written out by hand.
///
/// So these tests take the *host CLR* as the oracle instead. The host loads the very same
/// System.Private.CoreLib image and computes its vtables for real, in C++, from the real
/// MethodTable builder; `Type.GetMethods` then exposes one method per occupied slot, because that
/// is exactly what `PopulateMethods`' `overrides[slot]` dedupe achieves. Comparing against that is
/// a genuinely outside check: nothing about PawPrint's own walk feeds into the expected value.
[<TestFixture>]
module TestVirtualMethodSlots =

    // Undisposed on purpose: the returned DumpedAssembly.Logger closes over its sinks, and
    // disposing while the assembly is still live would silently drop events.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : LoadedAssemblies = LoadedAssemblies.ofAssemblies [ corelib ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loaded bct AllConcreteTypes.Empty

    let private loggerFactory = snd (LoggerFactory.makeTest ())

    let private state () : IlMachineState =
        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    let private concretize
        (state : IlMachineState)
        (``namespace`` : string)
        (name : string)
        : IlMachineState * ConcreteTypeHandle
        =
        let typeInfo =
            match corelib.TryGetTopLevelTypeDef ``namespace`` name with
            | None -> failwith $"%s{``namespace``}.%s{name} not found in corelib"
            | Some typeInfo -> typeInfo

        DumpedAssembly.typeInfoToTypeDefn' bct state._LoadedAssemblies typeInfo
        |> IlMachineState.concretizeType loggerFactory bct state corelib.Name ImmutableArray.Empty ImmutableArray.Empty

    let private vtable
        (state : IlMachineState)
        (handle : ConcreteTypeHandle)
        : IlMachineState * NativeRuntimeTypeHelpers.VtableSlot list
        =
        NativeRuntimeTypeHelpers.vtableOfClosed loggerFactory bct "test" state handle

    /// A spread of corelib shapes: the root, a value type and its own root, deep exception
    /// hierarchies, types that override some but not all of Object's virtuals, types with
    /// explicit interface implementations (private/final/virtual/newslot, which are in the
    /// vtable and which the "inherited private" filter keeps precisely because they are virtual),
    /// sealed types, abstract types, and delegates.
    let private corpus : (string * string) list =
        [
            "System", "Object"
            "System", "ValueType"
            "System", "String"
            "System", "Int32"
            "System", "Int64"
            "System", "Byte"
            "System", "Double"
            "System", "Boolean"
            "System", "Char"
            "System", "Guid"
            "System", "DateTime"
            "System", "TimeSpan"
            "System", "Decimal"
            "System", "Enum"
            "System", "DayOfWeek"
            "System", "Array"
            "System", "Delegate"
            "System", "MulticastDelegate"
            "System", "Attribute"
            "System", "Version"
            "System", "Exception"
            "System", "ArgumentException"
            "System", "ArgumentNullException"
            "System", "InvalidOperationException"
            "System", "Type"
            "System.Text", "StringBuilder"
            "System.IO", "Stream"
            "System.IO", "MemoryStream"
        ]

    let private hostType (``namespace`` : string) (name : string) : Type =
        let full =
            if ``namespace`` = "" then
                name
            else
                ``namespace`` + "." + name

        match typeof<obj>.Assembly.GetType (full, false) with
        | null -> failwith $"host CLR could not find %s{full} in its own corelib"
        | t -> t

    /// Every method `Type.GetMethods` reports that carries `Virtual` corresponds to exactly one
    /// occupied vtable slot, and every occupied slot yields exactly one such method -- that is what
    /// the `overrides[slot]` dedupe in `PopulateMethods` is for. So this count *is* the host's
    /// `MethodTable::GetNumVirtuals`, obtained without any private API.
    let private hostNumVirtuals (t : Type) : int =
        t.GetMethods (BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        |> Array.filter _.IsVirtual
        |> Array.length

    [<Test>]
    let ``vtable length agrees with the host CLR's vtable size`` () : unit =
        let mutable exercised = 0
        let mutable failures = []

        for ns, name in corpus do
            let state, handle = concretize (state ()) ns name
            let _, slots = vtable state handle

            let expected = hostNumVirtuals (hostType ns name)

            if List.length slots <> expected then
                failures <-
                    $"%s{ns}.%s{name}: PawPrint %i{List.length slots}, host %i{expected}"
                    :: failures

            exercised <- exercised + 1

        // Report every divergence at once: one wrong rule usually breaks a family of types, and
        // seeing the family is what identifies the rule.
        if not (List.isEmpty failures) then
            failwith (
                "vtable size disagrees with the host CLR:\n"
                + String.Join ("\n", List.rev failures)
            )

        exercised |> shouldEqual (List.length corpus)

    [<Test>]
    let ``GetNumVirtuals is exactly the vtable length`` () : unit =
        // Not a tautology worth skipping: it is the one thing an implementation could regress by
        // reintroducing an independent count, which is the failure mode the whole design exists to
        // rule out (the BCL *compares* the two).
        for ns, name in corpus do
            let state, handle = concretize (state ()) ns name
            let state, slots = vtable state handle

            let _, count =
                NativeRuntimeTypeHelpers.numVirtualsOfClosed loggerFactory bct "test" state handle

            count |> shouldEqual (List.length slots)

    [<Test>]
    let ``every vtable entry is a non-static virtual method`` () : unit =
        for ns, name in corpus do
            let state, handle = concretize (state ()) ns name
            let _, slots = vtable state handle

            for slot in slots do
                if slot.Method.IsStatic then
                    failwith $"%s{ns}.%s{name}: static method %s{slot.Method.Name} occupies a vtable slot"

                if not slot.Method.IsVirtual then
                    failwith $"%s{ns}.%s{name}: non-virtual method %s{slot.Method.Name} occupies a vtable slot"

    [<Test>]
    let ``no method occupies two slots`` () : unit =
        // An override replaces exactly one base entry. If the matcher ever bound one method to
        // several slots -- or if MethodImpl consultation crept back in and stamped a body into a
        // slot it does not declare -- the same method would appear twice, and `GetSlot`'s
        // `tryFindIndex` would silently answer the first.
        for ns, name in corpus do
            let state, handle = concretize (state ()) ns name
            let _, slots = vtable state handle

            let keys =
                slots
                |> List.map (fun slot -> slot.DeclaredBy.Identity, slot.Method.IdentityKey)

            List.length (List.distinct keys) |> shouldEqual (List.length keys)

    [<Test>]
    let ``a type's vtable extends its base's, replacing entries only with its own methods`` () : unit =
        // The layout discipline: inherit the base's slots in order, replace the ones this type
        // overrides, append new slots after. This is what makes a slot number mean the same thing
        // at every level of the chain, which is the property `PopulateMethods` relies on when it
        // indexes one `bool[]` while walking from derived to base.
        for ns, name in corpus do
            let state, handle = concretize (state ()) ns name
            let state, slots = vtable state handle

            let state, baseHandle =
                IlMachineState.resolveBaseConcreteType loggerFactory bct state handle

            match baseHandle with
            | None -> ()
            | Some bh ->
                let state, baseSlots = vtable state bh

                if List.length slots < List.length baseSlots then
                    failwith
                        $"%s{ns}.%s{name}: vtable is shorter (%i{List.length slots}) than its base's (%i{List.length baseSlots})"

                let ownIdentity =
                    match IlMachineState.tryGetConcreteTypeInfo state handle with
                    | Some (ct, _) -> ct.Identity
                    | None -> failwith $"%s{ns}.%s{name}: handle was not registered"

                List.zip (List.truncate (List.length baseSlots) slots) baseSlots
                |> List.iteri (fun i (slot, baseSlot) ->
                    let inherited =
                        slot.DeclaredBy.Identity = baseSlot.DeclaredBy.Identity
                        && slot.Method.IdentityKey = baseSlot.Method.IdentityKey

                    let overriddenHere = slot.DeclaredBy.Identity = ownIdentity

                    if not inherited && not overriddenHere then
                        failwith
                            $"%s{ns}.%s{name}: slot %i{i} holds %s{slot.Method.Name} declared by %O{slot.DeclaredBy}, which is neither the inherited entry nor a method of this type"
                )
