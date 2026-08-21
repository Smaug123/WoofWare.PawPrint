namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// An SZ array implicitly implements five generic interfaces whose bodies the runtime supplies
/// from `System.SZArrayHelper` (CoreCLR `MethodTable::FindDispatchImpl` /
/// `GetActualImplementationForArrayGenericIListOrIReadOnlyListMethod`). The end-to-end cases in
/// `sourcesPure/ArrayInterface*.cs` exercise the slots C# syntax can reach; these tests
/// quantify over *every* method of *every* one of the five interfaces, crossed with a
/// deliberately awkward pool of element/argument type pairs, and assert what a behavioural test
/// can only observe indirectly: that dispatch always lands on a callable body, and that the shim
/// is instantiated per CoreCLR's rule — the interface's type argument, canonicalised to
/// `System.Object` for reference types on every interface but `IEnumerable<T>`.
[<TestFixture>]
module TestSzArrayInterfaceDispatch =

    // The factory is intentionally undisposed: the returned DumpedAssembly.Logger closes over
    // its sinks, and disposing while the assembly is still live would silently drop events.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : LoadedAssemblies = LoadedAssemblies.ofAssemblies [ corelib ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loaded bct AllConcreteTypes.Empty

    let private loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory =
        // Factory intentionally undisposed: the states built from it outlive this scope.
        let _, loggerFactory = LoggerFactory.makeTest ()
        loggerFactory

    let private state () : IlMachineState =
        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    /// Virtual resolution takes a thread only to resolve MemberRef tokens while scanning
    /// MethodImpls. The SZ-array carve-out returns before any of that, so no thread state is
    /// needed; every case here stays on that path.
    let private unusedThread = ThreadId.ThreadId 0

    let private topLevelType (``namespace`` : string) (name : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        match corelib.TryGetTopLevelTypeDef ``namespace`` name with
        | None -> failwith $"%s{``namespace``}.%s{name} not found in corelib"
        | Some typeInfo -> typeInfo

    let private concretizeNonGeneric
        (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (state : IlMachineState)
        : IlMachineState * ConcreteTypeHandle
        =
        DumpedAssembly.typeInfoToTypeDefn' bct state._LoadedAssemblies ti
        |> IlMachineState.concretizeType
            loggerFactory
            bct
            state
            bct.Corelib.DefinitionFullName
            ImmutableArray.Empty
            ImmutableArray.Empty

    /// A deliberately awkward pool: two same-signedness-but-different-width integers, an
    /// enum over Int32 (whose normalised identity is Int32's), a value type that is not
    /// integer-like at all, and a three-level reference hierarchy so covariance has somewhere
    /// to go in both directions.
    let private elementTypes : (string * TypeInfo<GenericParamFromMetadata, TypeDefn>) list =
        [
            "Int32", bct.Int32
            "UInt32", bct.UInt32
            "Byte", bct.Byte
            "DayOfWeek", topLevelType "System" "DayOfWeek"
            "Guid", topLevelType "System" "Guid"
            "Object", bct.Object
            "Exception", bct.Exception
            "ArgumentException", bct.ArgumentException
            "String", bct.String
        ]

    let private theFiveInterfaces : (string * TypeInfo<GenericParamFromMetadata, TypeDefn>) list =
        [
            "IList`1", bct.IListGeneric
            "ICollection`1", bct.ICollectionGeneric
            "IEnumerable`1", bct.IEnumerableGeneric
            "IReadOnlyList`1", bct.IReadOnlyListGeneric
            "IReadOnlyCollection`1", bct.IReadOnlyCollectionGeneric
        ]

    type private DispatchCase =
        {
            ElementTypeName : string
            ElementType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            /// The interface's type argument, which under covariance need not be the element type.
            ArgumentTypeName : string
            ArgumentType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            InterfaceName : string
            Interface : TypeInfo<GenericParamFromMetadata, TypeDefn>
            /// A virtual slot declared directly on that interface.
            Method : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        }

        override this.ToString () =
            $"%s{this.ElementTypeName}[] as %s{this.InterfaceName}<%s{this.ArgumentTypeName}>::%s{this.Method.Name}"

    /// Every element/argument/interface/slot combination the pool describes. The space is small
    /// enough to walk in full, so these tests enumerate it rather than sampling it: how often
    /// each awkward corner gets reached is then a fixed property of the pool rather than a random
    /// variable, which is what the coverage assertions below rely on.
    let private allDispatchCases : DispatchCase list =
        [
            for elementTypeName, elementType in elementTypes do
                for argumentTypeName, argumentType in elementTypes do
                    for interfaceName, interfaceType in theFiveInterfaces do
                        // Every method these interfaces declare is a virtual slot, so the whole
                        // `Methods` list is fair game.
                        for method in interfaceType.Methods do
                            {
                                ElementTypeName = elementTypeName
                                ElementType = elementType
                                ArgumentTypeName = argumentTypeName
                                ArgumentType = argumentType
                                InterfaceName = interfaceName
                                Interface = interfaceType
                                Method = method
                            }
        ]

    /// Set up `elem[]` and the concretized `I<arg>::meth` the interpreter would be dispatching.
    let private prepare
        (case : DispatchCase)
        : IlMachineState *
          ConcreteTypeHandle *
          ConcreteTypeHandle *
          ConcreteTypeHandle *
          WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let state = state ()
        let state, elementHandle = concretizeNonGeneric case.ElementType state
        let state, argumentHandle = concretizeNonGeneric case.ArgumentType state

        let arrayHandle = ConcreteTypeHandle.OneDimArrayZero elementHandle

        let state, concretizedMethod, interfaceHandle =
            ExecutionConcretization.concretizeMethodWithAllGenerics
                loggerFactory
                bct
                (ImmutableArray.Create argumentHandle)
                case.Method
                ImmutableArray.Empty
                state

        state, arrayHandle, interfaceHandle, argumentHandle, concretizedMethod

    let private resolve
        (state : IlMachineState)
        (concretizedMethod : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (dispatchTypeHandle : ConcreteTypeHandle)
        : IlMachineState *
          WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> option
        =
        IlMachineStateExecution.tryResolveVirtualImplementation
            loggerFactory
            bct
            unusedThread
            concretizedMethod.Generics
            concretizedMethod
            dispatchTypeHandle
            true
            state

    let private objectHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes bct.Object

    /// CoreCLR's rule for which type the shim is instantiated over (`array.cpp`): the
    /// interface's type argument, except that a reference-type argument is canonicalised to
    /// `System.Object` on every interface but `IEnumerable<T>`. Recomputed here from the
    /// interface and the argument's own metadata, independently of how the interpreter derives
    /// it.
    let private expectedInstantiation (case : DispatchCase) (argumentHandle : ConcreteTypeHandle) =
        let argumentIsReferenceType =
            DumpedAssembly.isReferenceType bct loaded case.ArgumentType

        if case.InterfaceName = "IEnumerable`1" || not argumentIsReferenceType then
            argumentHandle
        else
            objectHandle

    let private checkCoverage (what : string) (expected : int) (actual : int) : unit =
        if actual <> expected then
            failwith
                $"coverage of %s{what} changed: expected %i{expected} cases but the walk found %i{actual}. This count is fixed by the element pool above and by the methods corelib declares on the five interfaces; work out which of those changed before updating the number."

    /// The central claim: whenever a well-typed program can hold an `elem[]` in an
    /// `I<arg>` local — i.e. whenever the cast the interpreter itself performs would succeed —
    /// dispatching any slot of `I<arg>` must land on a concrete, callable `SZArrayHelper`
    /// method, instantiated per CoreCLR's rule. Returning `None` is what produced the original
    /// bug: `callMethod` falls back to the abstract interface method and the interpreter trips
    /// its abstract-body guard.
    [<Test>]
    let ``assignable SZ-array receivers always dispatch to a callable SZArrayHelper body`` () =
        // The claim is conditional on assignability, so it would hold vacuously if the pool
        // produced no assignable pair. Count both halves of the instantiation rule as well, so a
        // pool that stopped reaching one of them fails loudly instead of quietly weakening.
        let mutable exercised = 0
        // Assignable cases where the interface argument differs from the element type *and* the
        // rule preserves it — the only ones that discriminate "T from the interface" from
        // "T from the array's element type".
        let mutable exercisedPreservedCovariant = 0
        // Assignable cases where the rule canonicalises to System.Object.
        let mutable exercisedCanonicalised = 0

        for case in allDispatchCases do
            try
                let state, arrayHandle, interfaceHandle, argumentHandle, concretizedMethod =
                    prepare case

                let state, assignable =
                    IlMachineState.isConcreteTypeAssignableTo loggerFactory bct state arrayHandle interfaceHandle

                // A non-assignable pair is unreachable from a well-typed program: the cast would
                // have thrown first, so dispatch makes no promise. Covered by the negative test
                // below.
                if assignable then
                    let expected = expectedInstantiation case argumentHandle

                    exercised <- exercised + 1

                    if expected = objectHandle && argumentHandle <> objectHandle then
                        exercisedCanonicalised <- exercisedCanonicalised + 1
                    elif case.ElementTypeName <> case.ArgumentTypeName then
                        exercisedPreservedCovariant <- exercisedPreservedCovariant + 1

                    let _, resolved = resolve state concretizedMethod arrayHandle

                    match resolved with
                    | None -> failwith "dispatch did not resolve, so the interpreter would crash on the abstract body"
                    | Some resolved ->

                    resolved.RequiredDeclaringType.Identity
                    |> shouldEqual bct.SZArrayHelper.Identity

                    // The shim is instantiated from the *interface's* type argument (not the
                    // array's element type), modulo CoreCLR's reference-type canonicalisation.
                    // Getting this wrong is observable: see
                    // `sourcesPure/ArrayInterfaceEqualityComparer.cs`.
                    Seq.toList resolved.Generics |> shouldEqual [ expected ]

                    // The whole point: an interpretable body, not another abstract slot.
                    match resolved.Body with
                    | MethodBody.Il _ -> ()
                    | other -> failwith $"resolved to a non-IL body %O{other}"

                    resolved.IsStatic |> shouldEqual false

                    resolved.Signature.RequiredParameterCount
                    |> shouldEqual concretizedMethod.Signature.RequiredParameterCount
            with e ->
                // An enumerated walk has no counterexample reporting of its own, and the
                // assertions above name only the values that disagreed.
                raise (Exception ($"%O{case}: %s{e.Message}", e))

        checkCoverage "assignable receivers" 285 exercised

        checkCoverage "instantiations preserved at a type other than the element type" 136 exercisedPreservedCovariant

        checkCoverage "instantiations canonicalised to System.Object" 56 exercisedCanonicalised

    /// Multi-dimensional arrays are excluded from the carve-out (CoreCLR reaches
    /// `IsImplicitInterfaceOfSZArray` only for SZ arrays), so they must never be handed an
    /// SZArrayHelper body — an `int[,]` receiver reinterpreted as `int[]` by
    /// `Unsafe.As<T[]>(this)` would read the wrong shape.
    [<Test>]
    let ``multi-dimensional arrays never dispatch to SZArrayHelper`` () =
        for case in allDispatchCases do
            try
                let state = state ()
                let state, elementHandle = concretizeNonGeneric case.ElementType state
                let state, argumentHandle = concretizeNonGeneric case.ArgumentType state

                let mdArrayHandle = ConcreteTypeHandle.Array (elementHandle, 2)

                let state, concretizedMethod, mdInterfaceHandle =
                    ExecutionConcretization.concretizeMethodWithAllGenerics
                        loggerFactory
                        bct
                        (ImmutableArray.Create argumentHandle)
                        case.Method
                        ImmutableArray.Empty
                        state

                // Precondition of the whole exercise: a multi-dim array is never assignable to
                // one of the five, so this dispatch is unreachable and the carve-out must stay
                // out of it.
                let state, assignable =
                    IlMachineState.isConcreteTypeAssignableTo loggerFactory bct state mdArrayHandle mdInterfaceHandle

                assignable |> shouldEqual false

                let _, resolved = resolve state concretizedMethod mdArrayHandle

                match resolved with
                | None -> ()
                | Some resolved ->

                resolved.RequiredDeclaringType.Identity
                |> shouldNotEqual bct.SZArrayHelper.Identity
            with e ->
                raise (Exception ($"%O{case}: %s{e.Message}", e))

    /// The dispatch carve-out and the assignability carve-out are two halves of one rule, and
    /// they must agree on which interfaces are in the set. Both read
    /// `BaseClassTypes.IsImplicitInterfaceOfSzArray`; this pins that the set is exactly the five
    /// CoreCLR names, so a corelib bump that renamed or moved one of them fails here rather than
    /// silently shrinking the carve-out.
    [<Test>]
    let ``the implicit interface set is exactly CoreCLR's five`` () =
        for name, ti in theFiveInterfaces do
            if not (bct.IsImplicitInterfaceOfSzArray ti.Identity) then
                failwith $"%s{name} should be in the SZ-array implicit interface set"

        // A near miss that must stay out: ISet<T> is a generic collection interface in the same
        // namespace, but arrays do not implement it.
        let iSet = topLevelType "System.Collections.Generic" "ISet`1"

        bct.IsImplicitInterfaceOfSzArray iSet.Identity |> shouldEqual false

        // As must the non-generic interfaces System.Array really does declare.
        let iCollectionNonGeneric = topLevelType "System.Collections" "ICollection"

        bct.IsImplicitInterfaceOfSzArray iCollectionNonGeneric.Identity
        |> shouldEqual false
