namespace WoofWare.PawPrint.Test

open System
open System.Collections.Generic
open System.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

type FunctionPointerOutcomeClass<'T> () =
    member _.Inst () : string = typeof<'T>.Name
    abstract Virtual : unit -> string
    default _.Virtual () : string = typeof<'T>.Name
    static member Static () : string = typeof<'T>.Name
    member _.Generic<'U> () : string = typeof<'T>.Name + typeof<'U>.Name
    static member StaticGeneric<'U> () : string = typeof<'T>.Name + typeof<'U>.Name

[<AbstractClass>]
type FunctionPointerOutcomeAbstract<'T> () =
    abstract Abstract : unit -> string
    member _.Concrete () : string = typeof<'T>.Name

type IFunctionPointerOutcomeInterface<'T> =
    abstract Abstract : unit -> string
    abstract Generic<'U> : unit -> string

[<Struct>]
type FunctionPointerOutcomeStruct<'T> =
    val Count : int
    member this.Inst () : string = typeof<'T>.Name + string this.Count
    static member Static () : string = typeof<'T>.Name

/// `NativeRuntimeMethodHandle.functionPointerOutcome` against the host CLR, which is the thing it
/// models. For a corpus of generic types, each instantiated over pairs of type arguments, the host's
/// own `RuntimeMethodHandle.GetFunctionPointer` says whether two instantiations of one method share
/// an address; the classifier, fed the facts the QCall would read, must say `SharedCode` for exactly
/// the pairs that do, and `ContainsGenericVariables` for exactly the handles on which the host throws.
///
/// The one fact the classifier is handed rather than computing, whether the declaring type is shared,
/// comes from `hostIsSharedTypeArgument` here, a restatement of `IlMachineRuntimeMetadata.isSharedTypeArgument`
/// over `System.Type`; the guest cases in `sourcesPure/MethodHandleGetFunctionPointer*.cs` exercise
/// the interpreter's own.
[<TestFixture>]
module TestFunctionPointerOutcome =

    let rec private hostIsSharedTypeArgument (t : Type) : bool =
        not t.IsValueType
        || (t.IsGenericType
            && t.GenericTypeArguments |> Array.exists hostIsSharedTypeArgument)

    let private definitions : Type list =
        [
            typedefof<FunctionPointerOutcomeClass<obj>>
            typedefof<FunctionPointerOutcomeAbstract<obj>>
            typedefof<IFunctionPointerOutcomeInterface<obj>>
            typedefof<FunctionPointerOutcomeStruct<obj>>
            typedefof<List<obj>>
            typedefof<Dictionary<obj, obj>>
            typedefof<Queue<obj>>
            typedefof<HashSet<obj>>
            typedefof<LinkedList<obj>>
            typedefof<KeyValuePair<obj, obj>>
            typedefof<ValueTuple<obj, obj>>
            typedefof<Tuple<obj, obj>>
            typedefof<Comparer<obj>>
            typedefof<EqualityComparer<obj>>
            typedefof<IComparer<obj>>
            typedefof<IEnumerable<obj>>
            typedefof<IList<obj>>
            typedefof<IDictionary<obj, obj>>
            typedefof<IEquatable<obj>>
            typedefof<Lazy<obj>>
            typedefof<ArraySegment<obj>>
            typedefof<Nullable<int>>
            typedefof<Func<obj, obj>>
        ]

    /// Two type arguments CoreCLR shares code between, as each of a pair of instantiations would use.
    let private sharedPairs : (Type * Type) list =
        [
            typeof<string>, typeof<obj>
            typeof<KeyValuePair<string, int>>, typeof<KeyValuePair<obj, int>>
        ]

    let private unsharedPairs : (Type * Type) list = [ typeof<int>, typeof<int64> ]

    let private allFlags : BindingFlags =
        BindingFlags.Public
        ||| BindingFlags.NonPublic
        ||| BindingFlags.Static
        ||| BindingFlags.Instance
        ||| BindingFlags.DeclaredOnly

    let private tryInstantiate (definition : Type) (argument : Type) : Type option =
        try
            definition.MakeGenericType (Array.create (definition.GetGenericArguments().Length) argument)
            |> Some
        with :? ArgumentException ->
            // A constraint the argument does not meet, e.g. `Nullable<string>`.
            None

    let private methodsOf (ty : Type) : MethodBase list =
        [
            yield! ty.GetMethods allFlags |> Seq.cast<MethodBase>
            yield! ty.GetConstructors allFlags |> Seq.cast<MethodBase>
        ]
        |> List.sortBy (fun m -> m.MetadataToken)

    /// The method instantiated over `int` for each of its own type parameters, or the method itself
    /// if it has none; `None` if `int` does not satisfy a constraint.
    let private closeMethod (m : MethodBase) : MethodBase option =
        match m with
        | :? System.Reflection.MethodInfo as mi when mi.IsGenericMethodDefinition ->
            try
                mi.MakeGenericMethod (Array.create (mi.GetGenericArguments().Length) typeof<int>) :> MethodBase
                |> Some
            with :? ArgumentException ->
                None
        | _ -> Some m

    let private outcomeOf (m : MethodBase) : FunctionPointerOutcome =
        let declaring = m.DeclaringType

        let declaringType =
            if declaring.ContainsGenericParameters then
                FunctionPointerDeclaringType.ContainsGenericVariables
            else
                FunctionPointerDeclaringType.Closed
                    {
                        IsValueType = declaring.IsValueType
                        IsInterface = declaring.IsInterface
                        IsSharedByGenericInstantiations =
                            declaring.IsGenericType
                            && declaring.GenericTypeArguments |> Array.exists hostIsSharedTypeArgument
                    }

        let genericParamCount, handleInstantiationCount =
            if m.IsGenericMethod then
                let count = m.GetGenericArguments().Length
                count, (if m.IsGenericMethodDefinition then 0 else count)
            else
                0, 0

        NativeRuntimeMethodHandle.functionPointerOutcome
            declaringType
            {
                IsStatic = m.IsStatic
                IsVirtual = m.IsVirtual
                GenericParamCount = genericParamCount
                HandleInstantiationCount = handleInstantiationCount
            }

    /// The host's answer, or `None` where it throws `ContainsGenericVariables`' exception.
    let private hostFunctionPointer (m : MethodBase) : nativeint option =
        try
            Some (m.MethodHandle.GetFunctionPointer ())
        with :? InvalidOperationException as e ->
            e.Message
            |> shouldEqual NativeRuntimeMethodHandle.containsGenericVariablesMessage

            None

    [<Test>]
    let ``ContainsGenericVariables exactly where the host throws`` () : unit =
        let mutable refused = 0
        let mutable answered = 0

        for definition in definitions do
            let closed =
                sharedPairs @ unsharedPairs
                |> List.collect (fun (a, b) -> [ a ; b ])
                |> List.choose (tryInstantiate definition)

            for ty in definition :: closed do
                for m in methodsOf ty do
                    let expected = hostFunctionPointer m |> Option.isNone

                    if expected then
                        refused <- refused + 1
                    else
                        answered <- answered + 1

                    (outcomeOf m = FunctionPointerOutcome.ContainsGenericVariables, m)
                    |> shouldEqual (expected, m)

        // Both arms are populated: every method of an open definition refuses, as does every
        // generic method definition, and every other method answers.
        refused |> shouldBeGreaterThan 100
        answered |> shouldBeGreaterThan 100

    [<Test>]
    let ``SharedCode exactly where two instantiations sharing code share an address`` () : unit =
        let mutable shared = 0
        let mutable exact = 0

        for definition in definitions do
            for left, right in sharedPairs do
                match tryInstantiate definition left, tryInstantiate definition right with
                | Some left, Some right ->
                    List.zip (methodsOf left) (methodsOf right)
                    |> List.iter (fun (l, r) ->
                        l.MetadataToken |> shouldEqual r.MetadataToken

                        match closeMethod l, closeMethod r with
                        | Some l, Some r ->
                            let outcome = outcomeOf l
                            outcomeOf r |> shouldEqual outcome

                            let sameAddress =
                                match hostFunctionPointer l, hostFunctionPointer r with
                                | Some lp, Some rp -> lp = rp
                                | lp, rp -> failwith $"host refused %O{l} (%O{lp}) or %O{r} (%O{rp}), both closed"

                            (outcome = FunctionPointerOutcome.SharedCode, l) |> shouldEqual (sameAddress, l)

                            if sameAddress then
                                shared <- shared + 1
                            else
                                exact <- exact + 1
                        | None, None -> ()
                        | _ -> failwith $"%O{l} and %O{r} disagree on whether int satisfies their constraints"
                    )
                | None, None -> ()
                | _ -> failwith $"%O{definition} admits one of %O{left} and %O{right} but not the other"

        shared |> shouldBeGreaterThan 20
        exact |> shouldBeGreaterThan 20

    [<Test>]
    let ``never SharedCode between instantiations that share no code`` () : unit =
        let mutable compared = 0

        for definition in definitions do
            for left, right in unsharedPairs do
                match tryInstantiate definition left, tryInstantiate definition right with
                | Some left, Some right ->
                    List.zip (methodsOf left) (methodsOf right)
                    |> List.iter (fun (l, r) ->
                        match closeMethod l, closeMethod r with
                        | Some l, Some r ->
                            match outcomeOf l with
                            | FunctionPointerOutcome.ExactInstantiation _ -> ()
                            | other -> failwith $"%O{l}: expected an exact instantiation, got %O{other}"

                            hostFunctionPointer l |> shouldNotEqual (hostFunctionPointer r)
                            compared <- compared + 1
                        | _ -> ()
                    )
                | _ -> ()

        compared |> shouldBeGreaterThan 20
