namespace WoofWare.PawPrint.Test

open System.Collections.Generic
open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// An SZ array implicitly implements five generic interfaces whose bodies the runtime supplies
/// from `System.SZArrayHelper` (CoreCLR `MethodTable::FindDispatchImpl` /
/// `GetActualImplementationForArrayGenericIListOrIReadOnlyListMethod`). The end-to-end cases in
/// `sourcesPure/ArrayInterface*.cs` exercise the slots C# syntax can reach; these properties
/// quantify over *every* method of *every* one of the five interfaces, crossed with a
/// deliberately awkward pool of element/argument type pairs, and assert the two things a
/// behavioural test cannot observe: that dispatch always lands on a callable body, and that the
/// shim is instantiated over the *interface's* type argument rather than the array's element
/// type.
[<TestFixture>]
module TestSzArrayInterfaceDispatch =

    // The factory is intentionally undisposed: the returned DumpedAssembly.Logger closes over
    // its sinks, and disposing while the assembly is still live would silently drop events.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : ImmutableDictionary<string, DumpedAssembly> =
        ImmutableDictionary.CreateRange [ KeyValuePair (corelib.Name.FullName, corelib) ]

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
    /// needed; every property here stays on that path.
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
            bct.Corelib.Name
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

    let private genDispatchCase : Gen<DispatchCase> =
        gen {
            let! elementTypeName, elementType = Gen.elements elementTypes
            let! argumentTypeName, argumentType = Gen.elements elementTypes
            let! interfaceName, interfaceType = Gen.elements theFiveInterfaces
            // Skip the property accessors' backing `.ctor`-less oddities by taking only what the
            // interface actually declares as a slot; every method on these interfaces is one.
            let! method = Gen.elements interfaceType.Methods

            return
                {
                    ElementTypeName = elementTypeName
                    ElementType = elementType
                    ArgumentTypeName = argumentTypeName
                    ArgumentType = argumentType
                    InterfaceName = interfaceName
                    Interface = interfaceType
                    Method = method
                }
        }

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

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 400

    /// The load-bearing property. Whenever a well-typed program can hold an `elem[]` in an
    /// `I<arg>` local — i.e. whenever the cast the interpreter itself performs would succeed —
    /// dispatching any slot of `I<arg>` must land on a concrete, callable `SZArrayHelper`
    /// method. Returning `None` here is what produced the original bug: `callMethod` falls back
    /// to the abstract interface method and the interpreter trips its abstract-body guard.
    [<Test>]
    let ``assignable SZ-array receivers always dispatch to a callable SZArrayHelper body`` () =
        // The property is conditional on assignability, so it would pass vacuously if the pool
        // never produced an assignable pair. Count the ones that reach the assertions, and
        // separately the ones where the interface argument differs from the element type, since
        // those are the only cases that discriminate "T from the interface" from "T from the
        // element type".
        let mutable exercised = 0
        let mutable exercisedCovariant = 0

        let property (case : DispatchCase) : bool =
            let state, arrayHandle, interfaceHandle, argumentHandle, concretizedMethod =
                prepare case

            let state, assignable =
                IlMachineState.isConcreteTypeAssignableTo loggerFactory bct state arrayHandle interfaceHandle

            if not assignable then
                // Unreachable from a well-typed program: the cast would have thrown first, so
                // dispatch makes no promise. Covered by the negative property below.
                true
            else

            exercised <- exercised + 1

            if case.ElementTypeName <> case.ArgumentTypeName then
                exercisedCovariant <- exercisedCovariant + 1

            let _, resolved = resolve state concretizedMethod arrayHandle

            match resolved with
            | None ->
                failwith $"%O{case}: dispatch did not resolve, so the interpreter would crash on the abstract body"
            | Some resolved ->

            resolved.DeclaringType.Identity |> shouldEqual bct.SZArrayHelper.Identity

            // Sub-decision (b): the shim is instantiated over the *interface's* type argument,
            // not the array's element type. For the covariant cases in the pool those differ,
            // so this genuinely discriminates.
            Seq.toList resolved.Generics |> shouldEqual [ argumentHandle ]

            // The whole point: an interpretable body, not another abstract slot.
            match resolved.Body with
            | MethodBody.Il _ -> ()
            | other -> failwith $"%O{case}: resolved to a non-IL body %O{other}"

            resolved.IsStatic |> shouldEqual false

            resolved.Signature.RequiredParameterCount
            |> shouldEqual concretizedMethod.Signature.RequiredParameterCount

            true

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genDispatchCase) property)

        if exercised < 50 then
            failwith
                $"property was near-vacuous: only %i{exercised} of the generated cases were assignable, so the assertions barely ran"

        if exercisedCovariant < 10 then
            failwith
                $"property never meaningfully exercised covariance: only %i{exercisedCovariant} assignable cases had an interface argument differing from the element type"

    /// Multi-dimensional arrays are excluded from the carve-out (CoreCLR reaches
    /// `IsImplicitInterfaceOfSZArray` only for SZ arrays), so they must never be handed an
    /// SZArrayHelper body — an `int[,]` receiver reinterpreted as `int[]` by
    /// `Unsafe.As<T[]>(this)` would read the wrong shape.
    [<Test>]
    let ``multi-dimensional arrays never dispatch to SZArrayHelper`` () =
        let property (case : DispatchCase) : bool =
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

            // Precondition of the whole exercise: a multi-dim array is never assignable to one
            // of the five, so this dispatch is unreachable and the carve-out must stay out of it.
            let state, assignable =
                IlMachineState.isConcreteTypeAssignableTo loggerFactory bct state mdArrayHandle mdInterfaceHandle

            assignable |> shouldEqual false

            let _, resolved = resolve state concretizedMethod mdArrayHandle

            match resolved with
            | None -> true
            | Some resolved ->
                resolved.DeclaringType.Identity |> shouldNotEqual bct.SZArrayHelper.Identity
                true

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genDispatchCase) property)

    /// The dispatch carve-out and the assignability carve-out are two halves of one rule, and
    /// they must agree on which interfaces are in the set. Both now read
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
