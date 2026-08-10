namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata.Ecma335
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

    /// Closes a generic corelib type definition at the given corelib type arguments.
    let private concretizeClosed
        (state : IlMachineState)
        (``namespace`` : string)
        (name : string)
        (args : (string * string) list)
        : IlMachineState * ConcreteTypeHandle
        =
        let typeInfo =
            match corelib.TryGetTopLevelTypeDef ``namespace`` name with
            | None -> failwith $"%s{``namespace``}.%s{name} not found in corelib"
            | Some typeInfo -> typeInfo

        let openDefn =
            DumpedAssembly.typeInfoToTypeDefn' bct state._LoadedAssemblies typeInfo

        let state, argHandles =
            ((state, []), args)
            ||> List.fold (fun (state, acc) (argNamespace, argName) ->
                let state, handle = concretize state argNamespace argName
                state, handle :: acc
            )

        // As in the interface test: `typeInfoToTypeDefn'` already yields the instantiation shape
        // `T`n<!0, ..>`, so close it by supplying the arguments as the type-generic context rather
        // than by wrapping it again.
        openDefn
        |> IlMachineState.concretizeType
            loggerFactory
            bct
            state
            corelib.Name
            (ImmutableArray.CreateRange (List.rev argHandles))
            ImmutableArray.Empty

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

    /// Closed generic types, which put the *substitution* half of the matcher under the same
    /// outside oracle. The non-generic corpus above cannot: every signature it compares is already
    /// closed in the metadata, so a matcher that ignored generic arguments entirely would still
    /// agree with the host on all of it. These types override methods whose signatures are written
    /// `!0` on the base and spelled concretely (or substituted) on the derived side -- exactly the
    /// shape `G1`/`G2` covers in the differential guest, but here over real corelib signatures
    /// (spans, nullables, interface reimplementation) that no hand-written case would think to
    /// produce.
    let private genericCorpus : (string * string * (string * string) list * Type) list =
        [
            "System", "Nullable`1", [ "System", "Int32" ], typeof<System.Nullable<int>>
            "System.Collections.Generic",
            "EqualityComparer`1",
            [ "System", "Int32" ],
            typeof<System.Collections.Generic.EqualityComparer<int>>
            "System.Collections.Generic",
            "Comparer`1",
            [ "System", "Int32" ],
            typeof<System.Collections.Generic.Comparer<int>>
            "System.Collections.Generic", "List`1", [ "System", "Int32" ], typeof<System.Collections.Generic.List<int>>
            "System.Collections.Generic",
            "Dictionary`2",
            [ "System", "Int32" ; "System", "Int32" ],
            typeof<System.Collections.Generic.Dictionary<int, int>>
        ]

    /// The two corpora as one list of (label, concretiser, host type), so every check below ranges
    /// over both.
    let private allCorpus : (string * (IlMachineState -> IlMachineState * ConcreteTypeHandle) * Type) list =
        (corpus
         |> List.map (fun (ns, name) ->
             $"%s{ns}.%s{name}", (fun (state : IlMachineState) -> concretize state ns name), hostType ns name
         ))
        @ (genericCorpus
           |> List.map (fun (ns, name, args, host) ->
               let arguments = args |> List.map snd |> String.concat ","

               $"%s{ns}.%s{name}[%s{arguments}]",
               (fun (state : IlMachineState) -> concretizeClosed state ns name args),
               host
           ))

    /// Every method `Type.GetMethods` reports that carries `Virtual` corresponds to exactly one
    /// occupied vtable slot, and every occupied slot yields exactly one such method -- that is what
    /// the `overrides[slot]` dedupe in `PopulateMethods` is for. So this count *is* the host's
    /// `MethodTable::GetNumVirtuals`, obtained without any private API.
    let private hostNumVirtuals (t : Type) : int =
        t.GetMethods (BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        |> Array.filter _.IsVirtual
        |> Array.length

    /// `GetSlot` finds its answer by locating the method in its declaring type's vtable, and a
    /// vtable spans assemblies: a guest type's own slots sit on top of corelib's. `IdentityKey` is
    /// a MethodDef *row number*, unique only within its own module, so row 6 of the guest and row 6
    /// of corelib compare equal on it alone. No corpus type happens to collide, which is exactly
    /// why this is a constructed test rather than a corpus one -- the failure it guards against is
    /// a silently wrong slot, and `PopulateMethods` would then suppress the wrong declaration.
    [<Test>]
    let ``slot lookup is scoped to the declaring assembly`` () : unit =
        let row (n : int) : System.Reflection.Metadata.MethodDefinitionHandle option * SynthesisedMethod option =
            Some (MetadataTokens.MethodDefinitionHandle n), None

        // A base method from corelib and a derived method from the guest, sharing row 6.
        let slots = [ "CoreLib", row 6 ; "Guest", row 6 ]

        NativeRuntimeTypeHelpers.slotIndexOfIdentity ("Guest", row 6) slots
        |> shouldEqual (Some 1)

        NativeRuntimeTypeHelpers.slotIndexOfIdentity ("CoreLib", row 6) slots
        |> shouldEqual (Some 0)

        // A row present in one assembly must not be found via another.
        NativeRuntimeTypeHelpers.slotIndexOfIdentity ("Other", row 6) slots
        |> shouldEqual None

    /// An interface has no base class, so `MethodTableBuilder::PlaceVirtualMethods` adds every
    /// instance virtual it declares without consulting NewSlot. Corelib contains exactly one method
    /// that makes the difference visible: `INumberBase<T>` declares
    /// `System.IUtf8SpanFormattable.TryFormat` as `Private, Final, Virtual, HideBySig` with no
    /// NewSlot. Treating it as an override would look for a base vtable that does not exist.
    [<Test>]
    let ``every instance virtual an interface declares gets its own slot`` () : unit =
        let state = state ()

        let typeInfo =
            match corelib.TryGetTopLevelTypeDef "System.Numerics" "INumberBase`1" with
            | None -> failwith "System.Numerics.INumberBase`1 not found in corelib"
            | Some typeInfo -> typeInfo

        let openDefn =
            DumpedAssembly.typeInfoToTypeDefn' bct state._LoadedAssemblies typeInfo

        // `typeInfoToTypeDefn'` already yields the instantiation shape `INumberBase`1<!0>`, so close
        // it by supplying Int32 as the type-generic context rather than by wrapping it again.
        let state, int32Handle = concretize state "System" "Int32"

        let state, handle =
            openDefn
            |> IlMachineState.concretizeType
                loggerFactory
                bct
                state
                corelib.Name
                (ImmutableArray.Create int32Handle)
                ImmutableArray.Empty

        let state, slots = vtable state handle

        // No base class, so the vtable is exactly the instance virtuals the interface declares --
        // including the reuse-slot one, which is what the NewSlot partition would have dropped.
        let declared =
            typeInfo.Methods
            |> List.filter (fun method -> not method.IsStatic && method.IsVirtual)

        List.length slots |> shouldEqual (List.length declared)

        let reuseSlot =
            declared |> List.filter (fun method -> not method.IsNewSlot) |> List.map _.Name

        // Guard against the test going vacuous if corelib ever stops carrying such a method: the
        // whole point is that at least one is not NewSlot.
        reuseSlot |> shouldEqual [ "System.IUtf8SpanFormattable.TryFormat" ]

    [<Test>]
    let ``vtable length agrees with the host CLR's vtable size`` () : unit =
        let mutable exercised = 0
        let mutable failures = []

        for label, concretiseType, host in allCorpus do
            let state, handle = concretiseType (state ())
            let _, slots = vtable state handle

            let expected = hostNumVirtuals host

            if List.length slots <> expected then
                failures <- $"%s{label}: PawPrint %i{List.length slots}, host %i{expected}" :: failures

            exercised <- exercised + 1

        // Report every divergence at once: one wrong rule usually breaks a family of types, and
        // seeing the family is what identifies the rule.
        if not (List.isEmpty failures) then
            failwith (
                "vtable size disagrees with the host CLR:\n"
                + String.Join ("\n", List.rev failures)
            )

        exercised |> shouldEqual (List.length allCorpus)

    [<Test>]
    let ``GetNumVirtuals is exactly the vtable length`` () : unit =
        // Not a tautology worth skipping: it is the one thing an implementation could regress by
        // reintroducing an independent count, which is the failure mode the whole design exists to
        // rule out (the BCL *compares* the two).
        for _, concretiseType, _ in allCorpus do
            let state, handle = concretiseType (state ())
            let state, slots = vtable state handle

            let _, count =
                NativeRuntimeTypeHelpers.numVirtualsOfClosed loggerFactory bct "test" state handle

            count |> shouldEqual (List.length slots)

    [<Test>]
    let ``every vtable entry is a non-static virtual method`` () : unit =
        for label, concretiseType, _ in allCorpus do
            let state, handle = concretiseType (state ())
            let _, slots = vtable state handle

            for slot in slots do
                if slot.Method.IsStatic then
                    failwith $"%s{label}: static method %s{slot.Method.Name} occupies a vtable slot"

                if not slot.Method.IsVirtual then
                    failwith $"%s{label}: non-virtual method %s{slot.Method.Name} occupies a vtable slot"

    [<Test>]
    let ``no method occupies two slots`` () : unit =
        // An override replaces exactly one base entry. If the matcher ever bound one method to
        // several slots -- or if MethodImpl consultation crept back in and stamped a body into a
        // slot it does not declare -- the same method would appear twice, and `GetSlot`'s
        // `tryFindIndex` would silently answer the first.
        for _, concretiseType, _ in allCorpus do
            let state, handle = concretiseType (state ())
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
        for label, concretiseType, _ in allCorpus do
            let state, handle = concretiseType (state ())
            let state, slots = vtable state handle

            let state, baseHandle =
                IlMachineState.resolveBaseConcreteType loggerFactory bct state handle

            match baseHandle with
            | None -> ()
            | Some bh ->
                let state, baseSlots = vtable state bh

                if List.length slots < List.length baseSlots then
                    failwith
                        $"%s{label}: vtable is shorter (%i{List.length slots}) than its base's (%i{List.length baseSlots})"

                let ownIdentity =
                    match IlMachineState.tryGetConcreteTypeInfo state handle with
                    | Some (ct, _) -> ct.Identity
                    | None -> failwith $"%s{label}: handle was not registered"

                List.zip (List.truncate (List.length baseSlots) slots) baseSlots
                |> List.iteri (fun i (slot, baseSlot) ->
                    let inherited =
                        slot.DeclaredBy.Identity = baseSlot.DeclaredBy.Identity
                        && slot.Method.IdentityKey = baseSlot.Method.IdentityKey

                    let overriddenHere = slot.DeclaredBy.Identity = ownIdentity

                    if not inherited && not overriddenHere then
                        failwith
                            $"%s{label}: slot %i{i} holds %s{slot.Method.Name} declared by %O{slot.DeclaredBy}, which is neither the inherited entry nor a method of this type"
                )
