namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata.Ecma335
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `VirtualSlotLayout.slotTableOfClosed` is the single definition of "which slot" in
/// PawPrint: `RuntimeMethodHandle.GetSlot` is an index into it, and `RuntimeTypeHandle.GetNumVirtuals`
/// is the length of its `Vtable` half alone. The end-to-end coverage is
/// `sourcesPure/ReflectionVirtualMethodSlots.cs`, which
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
        |> IlMachineState.concretizeType
            loggerFactory
            bct
            state
            corelib.DefinitionFullName
            ImmutableArray.Empty
            ImmutableArray.Empty

    let private vtable
        (state : IlMachineState)
        (handle : ConcreteTypeHandle)
        : IlMachineState * VirtualSlotLayout.VtableSlot list
        =
        VirtualSlotLayout.vtableOfClosed loggerFactory bct "test" state handle

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
            corelib.DefinitionFullName
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
            // Carry a MethodImpl whose *declaration* is a class method rather than an interface one, so
            // that the content walk's declaration resolution runs against real metadata. Measured over
            // corelib: 36 of its 4120 MethodImpl rows are class-declaration ones, every one of them
            // `.override System.Object::Finalize`, and every such body is a non-NewSlot override that
            // placement had already put in that very slot -- which is why no corelib type can
            // distinguish slot content from slot identity.
            "System", "WeakReference"
            "System.Threading", "Thread"
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
            // A generic type declaring *both* constructors, with lower-numbered rows before them:
            // `Lazy`1` has its `.cctor` at a higher row than its default ctor, and ordinary methods
            // below both. On a generic type every other method is placed in the first of the two
            // passes past the vtable, so without this entry a walk that gave the constructors their
            // priority only on non-generic types would agree with the host everywhere in the corpus.
            "System", "Lazy`1", [ "System", "Int32" ], typeof<System.Lazy<int>>
            // An interface, and specifically one carrying `static abstract` members. Those are
            // `virtual` *and* static, so CoreCLR places them outside the vtable. Measured here:
            // slot 0 is an ordinary *instance* virtual (the default implementation of
            // `IUtf8SpanFormattable.TryFormat`), slots 1-41 are the static virtuals, and slots 42-43
            // are static but *not* virtual (`IUtf8SpanParsable<TSelf>.Parse`/`TryParse`). Nothing
            // else in either corpus is an interface at all, so a placement filter written as "not
            // virtual" rather than "not an instance virtual" would drop those 41 and no other entry
            // would notice.
            // Named through reflection rather than `typeof<INumberBase<int>>`: F# rejects spelling an
            // interface with static abstract members outside a constraint position (FS3536), and
            // suppressing that warning to write a type name would be the wrong trade.
            "System.Numerics",
            "INumberBase`1",
            [ "System", "Int32" ],
            (hostType "System.Numerics" "INumberBase`1").MakeGenericType [| typeof<int> |]
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

    /// The host CLR's own `RuntimeMethodHandle.GetSlot`, which is what `PopulateMethods` calls. It is
    /// internal, so this is the one place the oracle reaches past the public surface -- and it is
    /// worth it: without it the oracle can only compare vtable *lengths*, and a walk that appended a
    /// spurious slot while dropping a real one has the right length and the wrong layout.
    let private hostSlotOf : MethodBase -> int =
        let impl =
            typeof<RuntimeMethodHandle>.GetMethods (BindingFlags.NonPublic ||| BindingFlags.Static)
            |> Array.filter (fun candidate ->
                candidate.Name = "GetSlot"
                && candidate.GetParameters().[0].ParameterType.Name = "IRuntimeMethodInfo"
            )
            |> Array.exactlyOne

        fun (method : MethodBase) -> impl.Invoke ((null : obj), [| box method |]) :?> int

    /// Every MethodDef the type declares, which is what `DeclaredMethodIterator` ranges over.
    /// `GetMethods` alone is not that: it never returns constructors, so the instance constructors
    /// and the class constructor have to be fetched separately -- and they are exactly the methods
    /// the placement rule gives priority to, so omitting them would leave the interesting case
    /// untested.
    let private hostDeclaredMethods (t : Type) : MethodBase list =
        let flags =
            BindingFlags.DeclaredOnly
            ||| BindingFlags.Instance
            ||| BindingFlags.Static
            ||| BindingFlags.Public
            ||| BindingFlags.NonPublic

        [
            yield! (t.GetMethods flags |> Seq.cast<MethodBase>)
            yield! (t.GetConstructors flags |> Seq.cast<MethodBase>)
            match t.TypeInitializer with
            | null -> ()
            | cctor -> yield (cctor :> MethodBase)
        ]
        // `GetConstructors` with `Static` in the flags also yields the class constructor, so the
        // explicit `TypeInitializer` above can duplicate it.
        |> List.distinctBy _.MetadataToken

    /// The identity `slotIndexInTable` looks a method up by. Every corpus type is corelib's, so the
    /// declaring assembly is corelib's; the row number comes straight from the host's metadata
    /// token, which is the same number PawPrint read out of the same image.
    let private identityOf
        (method : MethodBase)
        : string * (System.Reflection.Metadata.MethodDefinitionHandle option * SynthesisedMethod option)
        =
        let row = method.MetadataToken &&& 0xFFFFFF

        corelib.Name.FullName, (Some (MetadataTokens.MethodDefinitionHandle row), None)

    /// The host's vtable as a list of MethodDef tokens, slot 0 upwards: `GetSlot` gives the index and
    /// `MetadataToken` names the occupant. This is the *layout*, not merely its size.
    let private hostSlotLayout (t : Type) : int list =
        t.GetMethods (BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
        |> Array.filter _.IsVirtual
        |> Array.map (fun method -> hostSlotOf method, method.MetadataToken)
        |> Array.sortBy fst
        |> Array.map snd
        |> List.ofArray

    /// The same list for PawPrint: each slot's occupant named by its MethodDef token.
    let private pawPrintSlotLayout (slots : VirtualSlotLayout.VtableSlot list) : int list =
        slots
        |> List.map (fun slot ->
            match fst slot.Method.IdentityKey with
            | Some handle ->
                MetadataTokens.GetToken (
                    System.Reflection.Metadata.MethodDefinitionHandle.op_Implicit handle
                    : System.Reflection.Metadata.EntityHandle
                )
            // A synthesised method has no MethodDef row. None occupies a slot in any corpus type
            // here, and -1 makes it a loud mismatch rather than a silent match if one ever does.
            | None -> -1
        )

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

        VirtualSlotLayout.slotIndexOfIdentity ("Guest", row 6) slots
        |> shouldEqual (Some 1)

        VirtualSlotLayout.slotIndexOfIdentity ("CoreLib", row 6) slots
        |> shouldEqual (Some 0)

        // A row present in one assembly must not be found via another.
        VirtualSlotLayout.slotIndexOfIdentity ("Other", row 6) slots |> shouldEqual None

    /// An interface has no base class, so `MethodTableBuilder::PlaceVirtualMethods` adds every
    /// instance virtual it declares without consulting NewSlot. Corelib contains exactly one method
    /// that makes the difference visible: `INumberBase<T>` declares
    /// `System.IUtf8SpanFormattable.TryFormat` as `Private, Final, Virtual, HideBySig` with no
    /// NewSlot. Treating it as an override would look for a base vtable that does not exist.
    ///
    /// `vtableOfClosed` has no interface-specific branch: with no base slots to search, the general
    /// rule finds no match and gives the method a fresh slot, which is the same answer. This test is
    /// what holds that equivalence down.
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
                corelib.DefinitionFullName
                (ImmutableArray.Create int32Handle)
                ImmutableArray.Empty

        let state, slots = vtable state handle

        // No base class, so the vtable is exactly the instance virtuals the interface declares --
        // including the reuse-slot one, which an implementation that refused to place an unmatched
        // non-NewSlot virtual would have rejected outright.
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

    /// Strictly stronger than the length check above, and the reason it is worth reaching for an
    /// internal API: placing a method in the wrong slot, or appending a spurious slot while failing
    /// to place a real one, leaves the length right and the layout wrong. An unmatched non-newslot
    /// virtual *appends* rather than failing loudly, so a gap in `candidateFillsSlot` does not
    /// announce itself; this is what catches it instead.
    [<Test>]
    let ``vtable layout agrees with the host CLR slot for slot`` () : unit =
        let mutable exercised = 0
        let mutable failures = []

        for label, concretiseType, host in allCorpus do
            let state, handle = concretiseType (state ())
            let _, slots = vtable state handle

            let actual = pawPrintSlotLayout slots
            let expected = hostSlotLayout host

            if actual <> expected then
                // Truncate *both* to the common prefix: a walk that drops a slot makes `actual`
                // the shorter list, and zipping a short list against a long one throws, which
                // would replace the diagnostic with an ArgumentException and lose the rest of the
                // corpus's results along with it.
                let common = min (List.length actual) (List.length expected)

                let firstDivergence =
                    List.zip (List.truncate common actual) (List.truncate common expected)
                    |> List.tryFindIndex (fun ((a : int), (b : int)) -> a <> b)

                let divergence =
                    match firstDivergence with
                    | Some i -> $"first differing slot %i{i}"
                    | None -> "identical up to the shorter length; only the lengths differ"

                failures <-
                    $"%s{label}: PawPrint %i{List.length actual} slots, host %i{List.length expected}, %s{divergence}"
                    :: failures

            exercised <- exercised + 1

        if not (List.isEmpty failures) then
            failwith (
                "vtable layout disagrees with the host CLR:\n"
                + String.Join ("\n", List.rev failures)
            )

        exercised |> shouldEqual (List.length allCorpus)

    /// The strongest form of the oracle, and the only one that pins what `RuntimeMethodHandle.GetSlot`
    /// actually computes: for **every method a corpus type declares**, virtual or not, PawPrint's
    /// slot number must equal the host CLR's.
    ///
    /// Deliberately compares numbers rather than two lists laid side by side. A list comparison of
    /// the region past the vtable checks its *order* but never its *origin*, so an implementation
    /// that forgot to add `numVirtuals` -- the one piece of arithmetic in the whole change -- would
    /// pass it. Asking for the number instead puts `slotIndexInTable` itself under the oracle.
    ///
    /// It has to be a unit test rather than an end-to-end one. Of the five `GetSlot` call sites in
    /// the pinned corelib, four guard on `Virtual` first and the fifth (`PopulateProperties`) only
    /// ever compares the answer with `numVirtuals`. So no guest can observe the numbering past the
    /// vtable at all: stubbing it to `numVirtuals + 999` gets every end-to-end case in the suite
    /// through, measured. The host CLR is the only oracle available, and it is a thorough one.
    [<Test>]
    let ``every declared method's slot agrees with the host CLR`` () : unit =
        let mutable failures = []
        let mutable virtualsChecked = 0
        let mutable beyondChecked = 0

        for label, concretiseType, host in allCorpus do
            let state, handle = concretiseType (state ())

            let _, table =
                VirtualSlotLayout.slotTableOfClosed loggerFactory bct "test" state handle

            let numVirtuals = List.length table.Vtable

            for method in hostDeclaredMethods host do
                let expected = hostSlotOf method
                let actual = VirtualSlotLayout.slotIndexInTable (identityOf method) table

                match actual with
                | None ->
                    failures <-
                        $"%s{label}: %s{method.Name} (row %i{method.MetadataToken &&& 0xFFFFFF}) has no slot in PawPrint's table, host says %i{expected}"
                        :: failures
                | Some actual ->
                    if actual <> expected then
                        failures <-
                            $"%s{label}: %s{method.Name} (row %i{method.MetadataToken &&& 0xFFFFFF}) PawPrint slot %i{actual}, host %i{expected}"
                            :: failures

                if expected < numVirtuals then
                    virtualsChecked <- virtualsChecked + 1
                else
                    beyondChecked <- beyondChecked + 1

        if not (List.isEmpty failures) then
            // Report every divergence at once: one wrong rule usually breaks a family of methods,
            // and seeing the family is what identifies the rule. Truncated, because a rule that
            // misplaces the whole region past the vtable produces thousands.
            let shown = failures |> List.rev |> List.truncate 40

            failwith (
                $"%i{List.length failures} method slots disagree with the host CLR (first %i{List.length shown}):\n"
                + String.Join ("\n", shown)
            )

        // Guard against the check going vacuous in either half. A corpus that stopped yielding
        // methods past the vtable would leave the new rule untested while every assertion above
        // still passed; the floors are well below the ~1500 and ~200 the corpus actually produces.
        beyondChecked |> shouldBeGreaterThan 500
        virtualsChecked |> shouldBeGreaterThan 100

    [<Test>]
    let ``GetNumVirtuals is exactly the vtable length`` () : unit =
        // Not a tautology worth skipping: it is the one thing an implementation could regress by
        // reintroducing an independent count, which is the failure mode the whole design exists to
        // rule out (the BCL *compares* the two).
        for _, concretiseType, _ in allCorpus do
            let state, handle = concretiseType (state ())
            let state, slots = vtable state handle

            let _, count =
                VirtualSlotLayout.numVirtualsOfClosed loggerFactory bct "test" state handle

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

    /// Corelib's open generic type *definitions*, which is the shape `RuntimeTypeHandle.GetNumVirtuals`
    /// and `RuntimeMethodHandle.GetSlot` are asked about when a guest reflects over `typeof(G<>)`, and
    /// the layout every instantiation of one shares.
    ///
    /// Chosen for their extends clauses rather than for their popularity, because that is the half a
    /// closed instantiation cannot exercise: `NullableComparer<T> : Comparer<Nullable<T>>` applies its
    /// base to a type *built from* its own variable, and `Dictionary<K,V>.Enumerator` is a nested type
    /// that redeclares both of its enclosing type's parameters. `List`1` and `Nullable`1` are here as
    /// the ordinary cases, and `Nullable`1` is a value type, whose layout CoreCLR builds afresh for
    /// each instantiation rather than cloning from a canonical one.
    let private definitionCorpus : (string * string) list =
        [
            "System", "Nullable`1"
            "System", "Lazy`1"
            "System.Collections.Generic", "List`1"
            "System.Collections.Generic", "Dictionary`2"
            "System.Collections.Generic", "Comparer`1"
            "System.Collections.Generic", "EqualityComparer`1"
            // The composite extends clause: `Comparer<Nullable<T>>`.
            "System.Collections.Generic", "NullableComparer`1"
            "System.Collections.Generic", "NullableEqualityComparer`1"
            "System.Threading.Tasks", "Task`1"
        ]

    let private definitionCorpusNames : string list =
        definitionCorpus |> List.map (fun (ns, name) -> $"%s{ns}.%s{name}")

    /// The host CLR lays a method table out on the generic definition and shares it with every
    /// instantiation, so a definition has a layout of its own to compare against -- and
    /// `typeof(G<>).GetMethods()` plus the internal `GetSlot` report it without any private API beyond
    /// the one this fixture already uses.
    [<TestCaseSource(nameof definitionCorpusNames)>]
    let ``a definition's vtable layout matches the host's`` (fullName : string) : unit =
        let ``namespace``, name =
            let index = fullName.LastIndexOf '.'
            fullName.Substring (0, index), fullName.Substring (index + 1)

        let typeInfo =
            match corelib.TryGetTopLevelTypeDef ``namespace`` name with
            | None -> failwith $"%s{fullName} not found in corelib"
            | Some typeInfo -> typeInfo

        let identity =
            ResolvedTypeIdentity.ofDefinitionInAssembly typeInfo.AssemblyFullName typeInfo.TypeDefHandle

        let _, slots =
            VirtualSlotLayout.vtableOfDefinition loggerFactory bct "test" (state ()) identity

        let expected = hostSlotLayout (hostType ``namespace`` name)

        // Not vacuous: a definition that reported no virtuals at all would agree with a walk that
        // returned nothing.
        expected |> shouldNotEqual []

        pawPrintSlotLayout slots |> shouldEqual expected

    /// The same comparison for the whole method table rather than its vtable prefix, which is what
    /// `RuntimeMethodHandle.GetSlot` answers from when a guest reflects over `typeof(G<>)`: a
    /// non-virtual method of a definition holds a slot too, past the end of the vtable.
    ///
    /// The outside oracle is the only check on that region for a definition. `PopulateMethods` asks
    /// `GetSlot` only about methods carrying `MethodAttributes.Virtual`, and `PopulateProperties`,
    /// which asks without that guard, cannot reach an open definition yet (a property of one stops in
    /// `ModuleHandle.ResolveMethod`) -- so no guest test can currently distinguish a definition's
    /// beyond-vtable numbering from an empty one.
    [<TestCaseSource(nameof definitionCorpusNames)>]
    let ``a definition's declared method slots agree with the host's`` (fullName : string) : unit =
        let ``namespace``, name =
            let index = fullName.LastIndexOf '.'
            fullName.Substring (0, index), fullName.Substring (index + 1)

        let typeInfo =
            match corelib.TryGetTopLevelTypeDef ``namespace`` name with
            | None -> failwith $"%s{fullName} not found in corelib"
            | Some typeInfo -> typeInfo

        let identity =
            ResolvedTypeIdentity.ofDefinitionInAssembly typeInfo.AssemblyFullName typeInfo.TypeDefHandle

        let _, table =
            VirtualSlotLayout.slotTableOfDefinition loggerFactory bct "test" (state ()) identity

        let numVirtuals = List.length table.Vtable
        let host = hostType ``namespace`` name

        let mutable failures = []
        let mutable virtualsChecked = 0
        let mutable beyondChecked = 0

        for method in hostDeclaredMethods host do
            let expected = hostSlotOf method

            match VirtualSlotLayout.slotIndexInTable (identityOf method) table with
            | None ->
                failures <-
                    $"%s{method.Name} (row %i{method.MetadataToken &&& 0xFFFFFF}) has no slot in PawPrint's table, host says %i{expected}"
                    :: failures
            | Some actual ->
                if actual <> expected then
                    failures <-
                        $"%s{method.Name} (row %i{method.MetadataToken &&& 0xFFFFFF}) PawPrint slot %i{actual}, host %i{expected}"
                        :: failures

            if expected < numVirtuals then
                virtualsChecked <- virtualsChecked + 1
            else
                beyondChecked <- beyondChecked + 1

        if not (List.isEmpty failures) then
            let shown = failures |> List.rev |> List.truncate 40

            failwith (
                $"%s{fullName}: %i{List.length failures} method slots disagree with the host CLR (first %i{List.length shown}):\n"
                + String.Join ("\n", shown)
            )

        // Neither half of the check goes vacuous for any single case: measured over this corpus, the
        // scarcest are 1 method in the vtable (`Lazy`1`, which overrides only `ToString`, and
        // `Task`1`, which overrides only `InnerInvoke`) and 2 beyond it (`NullableComparer`1`, whose
        // only non-virtual declarations are its two constructors).
        virtualsChecked |> shouldBeGreaterThan 0
        beyondChecked |> shouldBeGreaterThan 1

    /// Every corpus type by full name, generic definitions included, for the placement oracle below.
    let private allCorpusNames : string list =
        (corpus @ definitionCorpus)
        |> List.map (fun (ns, name) -> if ns = "" then name else $"%s{ns}.%s{name}")

    /// `placedSlotsOfDefinition` answers `MethodDesc::GetSlot()` for every declaration in a chain,
    /// including the ones the vtable can no longer see, and the host CLR answers the same question
    /// for any `MethodBase` whatever type declared it. So this compares them directly.
    ///
    /// The interesting rows are exactly the ones the two tests above cannot reach. Those ask only
    /// about methods the corpus type *declares*, and compare against `slotIndexInTable`, which scans
    /// each slot's current occupant -- so a declaration that a derived type overrode by placement is
    /// absent from what they check, because `A.M` and `B.M` share a slot and only `B.M` occupies it.
    /// That vanished declaration is the whole reason this list exists: a MethodImpl spelled
    /// `.override A::M` writes the slot `A.M` owns, and nothing else can say which slot that is.
    ///
    /// The corpus reaches the shape on purpose: `System.ArgumentNullException` carries three
    /// generations of `Message` and `ToString` above it, so two of each are overridden declarations
    /// that only this list can still name.
    [<TestCaseSource(nameof allCorpusNames)>]
    let ``every declaration's slot agrees with the host's, overridden ones included`` (fullName : string) : unit =
        let ``namespace``, name =
            match fullName.LastIndexOf '.' with
            | -1 -> "", fullName
            | index -> fullName.Substring (0, index), fullName.Substring (index + 1)

        let typeInfo =
            match corelib.TryGetTopLevelTypeDef ``namespace`` name with
            | None -> failwith $"%s{fullName} not found in corelib"
            | Some typeInfo -> typeInfo

        let identity =
            ResolvedTypeIdentity.ofDefinitionInAssembly typeInfo.AssemblyFullName typeInfo.TypeDefHandle

        let _, placed =
            VirtualSlotLayout.placedSlotsOfDefinition loggerFactory bct "test" (state ()) identity

        // Keyed on the declaring assembly and the MethodDef *row*, which is what makes the key
        // injective. `MethodDefinitionHandle` is a struct whose `ToString` does not name the row, so
        // keying on a rendering of the identity tuple silently collapses every method of a type onto
        // one entry -- which reads as "the walk gave every declaration the same slot" and cost a
        // debugging round to spot.
        let keyOf
            (assembly : string)
            (handle : System.Reflection.Metadata.MethodDefinitionHandle option)
            : string * int
            =
            match handle with
            | Some handle ->
                assembly,
                MetadataTokens.GetRowNumber (
                    System.Reflection.Metadata.MethodDefinitionHandle.op_Implicit handle
                    : System.Reflection.Metadata.EntityHandle
                )
            // A synthesised method has no row. None is placed in any corpus type here, and a
            // negative row makes it a loud mismatch rather than a silent match if one ever is.
            | None -> assembly, -1

        // Two entries for one declaration would mean the walk placed a method twice.
        let byIdentity =
            placed
            |> List.map (fun (slot, index) ->
                keyOf slot.DeclaredBy.AssemblyFullName (fst slot.Method.IdentityKey), index
            )

        byIdentity
        |> List.countBy fst
        |> List.filter (fun (_, count) -> count > 1)
        |> shouldEqual []

        let byIdentity = Map.ofList byIdentity

        // The host's chain, most-derived first. An instance virtual declared anywhere on it owns a
        // vtable slot, and every one of them should be in `placed`.
        let hostChain =
            let rec chain (t : Type) =
                match t with
                | null -> []
                | t -> t :: chain t.BaseType

            chain (hostType ``namespace`` name)

        let mutable failures = []
        let mutable checked' = 0

        for ancestor in hostChain do
            for method in hostDeclaredMethods ancestor do
                if method.IsVirtual && not method.IsStatic then
                    checked' <- checked' + 1
                    let expected = hostSlotOf method

                    let key =
                        keyOf
                            corelib.Name.FullName
                            (Some (MetadataTokens.MethodDefinitionHandle (method.MetadataToken &&& 0xFFFFFF)))

                    match Map.tryFind key byIdentity with
                    | None ->
                        failures <-
                            $"%s{ancestor.Name}::%s{method.Name} (row %i{method.MetadataToken &&& 0xFFFFFF}) is absent from the placement list, host slot %i{expected}"
                            :: failures
                    | Some actual ->
                        if actual <> expected then
                            failures <-
                                $"%s{ancestor.Name}::%s{method.Name} (row %i{method.MetadataToken &&& 0xFFFFFF}) PawPrint slot %i{actual}, host %i{expected}"
                                :: failures

        if not (List.isEmpty failures) then
            let shown = failures |> List.rev |> List.truncate 40

            failwith (
                $"%s{fullName}: %i{List.length failures} of %i{checked'} declaration slots disagree with the host CLR (first %i{List.length shown}):\n"
                + String.Join ("\n", shown)
            )

        // Not vacuous: the scarcest case in this corpus is `System.Object` itself, whose chain is one
        // type declaring four instance virtuals -- `Finalize`, `ToString`, `Equals` and `GetHashCode`.
        checked' |> shouldBeGreaterThan 3

    /// Slot *content* -- what a `callvirt` through slot `i` runs -- against slot *identity*, which is
    /// what declaration owns slot `i`. The two are the same list except where a MethodImpl whose
    /// declaration names a class method has moved a slot away from its owner, so this pins both the
    /// agreement and the disagreement over real metadata.
    ///
    /// It is not the differential oracle for content, and it cannot become one: **corelib has no type
    /// whose content differs from its identity.** Measured -- of its 4120 MethodImpl rows, 4084 declare
    /// an interface method, which writes the dispatch map and not the vtable, and all 36 that declare a
    /// class method are `.override System.Object::Finalize` whose body is `Family, Virtual, HideBySig`
    /// with *no* NewSlot. Placement therefore already gave that body Object's `Finalize` slot, and the
    /// MethodImpl writes the same body to the same slot. So the two tables agree throughout corelib,
    /// and this asserts that agreement rather than pretending to check the content rule.
    ///
    /// What it does rule out is the failure a fabricated differential cannot see: the content walk
    /// crashing, or losing a slot, on the 2000-odd corelib types nobody fabricates. `System.WeakReference`
    /// and `System.Threading.Thread` are in the corpus so that `declarationSlot`'s MethodDef arm runs
    /// against real metadata and has to find Object's slot -- but because the answer coincides with
    /// placement, only the fabricated differential can tell whether it found the *right* one.
    [<TestCaseSource(nameof allCorpusNames)>]
    let ``slot content agrees with slot identity except where a class MethodImpl moved it`` (fullName : string) : unit =
        let ``namespace``, name =
            match fullName.LastIndexOf '.' with
            | -1 -> "", fullName
            | index -> fullName.Substring (0, index), fullName.Substring (index + 1)

        let typeInfo =
            match corelib.TryGetTopLevelTypeDef ``namespace`` name with
            | None -> failwith $"%s{fullName} not found in corelib"
            | Some typeInfo -> typeInfo

        let identity =
            ResolvedTypeIdentity.ofDefinitionInAssembly typeInfo.AssemblyFullName typeInfo.TypeDefHandle

        let _, identityTable =
            VirtualSlotLayout.vtableOfDefinition loggerFactory bct "test" (state ()) identity

        let _, contentTable =
            VirtualSlotLayout.contentVtableOfDefinition loggerFactory bct "test" (state ()) identity

        // Same shape: content never adds or removes a slot, it only changes occupants.
        List.length contentTable |> shouldEqual (List.length identityTable)

        let differing =
            List.zip identityTable contentTable
            |> List.indexed
            |> List.filter (fun (_, (a, b)) ->
                (a.DeclaredBy.AssemblyFullName, a.Method.IdentityKey)
                <> (b.DeclaredBy.AssemblyFullName, b.Method.IdentityKey)
            )

        // A difference means the content came from somewhere else in the chain, so the two occupants
        // must be declared by *different types*. Two methods of one type swapping slots would be the
        // walk scrambling placement rather than a MethodImpl moving a slot.
        //
        // Deliberately not "the occupant is the body of some MethodImpl in the chain": stating that
        // here would re-derive what the implementation computes, and the corelib shape would make it
        // circular rather than independent. The fabricated differential is what checks *which* body a
        // slot ends up with; this checks only that the walk has not lost one.
        for index, (owner, occupant) in differing do
            if owner.DeclaredBy.Identity = occupant.DeclaredBy.Identity then
                failwith
                    $"%s{fullName}: slot %i{index} is owned by %s{owner.Method.Name} and holds %s{occupant.Method.Name}, both declared by %s{owner.DeclaredBy.Description}; a MethodImpl moves a slot to another type's body, so this is placement being scrambled"

    [<Test>]
    let ``numVirtualsOfDefinition is exactly the definition's vtable length`` () : unit =
        // As for the closed case: the BCL *compares* the two, so an independently-computed count is
        // the regression this rules out.
        for ``namespace``, name in definitionCorpus do
            let typeInfo =
                match corelib.TryGetTopLevelTypeDef ``namespace`` name with
                | None -> failwith $"%s{``namespace``}.%s{name} not found in corelib"
                | Some typeInfo -> typeInfo

            let identity =
                ResolvedTypeIdentity.ofDefinitionInAssembly typeInfo.AssemblyFullName typeInfo.TypeDefHandle

            let state, slots =
                VirtualSlotLayout.vtableOfDefinition loggerFactory bct "test" (state ()) identity

            let _, count =
                VirtualSlotLayout.numVirtualsOfDefinition loggerFactory bct "test" state identity

            count |> shouldEqual (List.length slots)
