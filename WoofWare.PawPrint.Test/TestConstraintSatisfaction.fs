namespace WoofWare.PawPrint.Test

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open System.Text.RegularExpressions
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// `ConstraintSatisfaction.satisfiesClosed` against the host's CoreCLR. One corpus of generic types
/// and methods, each with a set of constraints, is loaded into the host and into a `TypeSystemState`;
/// for each argument, `MakeGenericType` (or `MakeGenericMethod`) on the host, whose rejection carries
/// the type loader's own verdict, says which instantiations are admitted, and which parameter is
/// rejected first.
///
/// The arguments are every leaf type of the corpus and of CoreLib's that a test names, and then
/// instantiations generated over them: arrays, `List<T>`, `IEnumerable<T>`, `Nullable<T>`,
/// `KeyValuePair<K, V>` and the corpus's covariant `IOut<T>`, each built only where the host admits
/// it, so that the type itself is legal and only the constraint is in question.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestConstraintSatisfaction =

    let private corpusSource : string =
        """
using System;
using System.Collections.Generic;

public interface IMarker { }
public interface IOut<out T> { }
public interface ISvm { static abstract void Make(); }
public interface IInheritsSvm : ISvm { }
public class BaseType { }
public class DerivedType : BaseType, IMarker { }
public class Unrelated { }
public abstract class AbstractDerived : BaseType, IMarker { }
public sealed class SealedMarker : IMarker { }
public class NoPublicCtor { private NoPublicCtor() { } }
public class InternalCtor { internal InternalCtor() { } }
public class OutOfDerived : IOut<DerivedType> { }
public abstract class AbstractSvm : ISvm { public static void Make() { } }
public class ConcreteSvm : ISvm { public static void Make() { } }
public struct MarkedStruct : IMarker { public int X; }
public struct PlainStruct { public int X; }
public enum MyEnum { A }
public ref struct MarkedRefStruct : IMarker { public int X; }
public ref struct PlainRefStruct { public int X; }

public class H_None<T> { }
public class H_Struct<T> where T : struct { }
public class H_Class<T> where T : class { }
public class H_New<T> where T : new() { }
public class H_Marker<T> where T : IMarker { }
public class H_Base<T> where T : BaseType { }
public class H_BaseAndMarker<T> where T : BaseType, IMarker { }
public class H_Comparable<T> where T : IComparable { }
public class H_ComparableOfSelf<T> where T : IComparable<T> { }
public class H_EnumerableOfInt<T> where T : IEnumerable<int> { }
public class H_EnumerableOfObject<T> where T : IEnumerable<object> { }
public class H_ListOfObject<T> where T : IList<object> { }
public class H_OutOfBase<T> where T : IOut<BaseType> { }
public class H_Enum<T> where T : Enum { }
public class H_Delegate<T> where T : Delegate { }
public class H_AllowsRef<T> where T : allows ref struct { }
public class H_AllowsRefMarker<T> where T : IMarker, allows ref struct { }
public class H_StructMarker<T> where T : struct, IMarker { }
public class H_ClassMarker<T> where T : class, IMarker { }
public class H_NewBase<T> where T : BaseType, new() { }
public class H_Unmanaged<T> where T : unmanaged { }
public class H_Svm<T> where T : ISvm { }
public class H_InheritsSvm<T> where T : IInheritsSvm { }
public class H2_MarkerThenBase<A, B> where A : IMarker where B : BaseType { }
public class H2_FirstBoundsSecond<A, B> where B : A { }

public static class Outer<U>
{
    public static void Bounded<T>() where T : U { }
    public static void ComparableOfOuter<T>() where T : IComparable<U> { }
    public static void Class<T>() where T : class { }
}
"""

    let private singleHolders : string list =
        [
            "H_None`1"
            "H_Struct`1"
            "H_Class`1"
            "H_New`1"
            "H_Marker`1"
            "H_Base`1"
            "H_BaseAndMarker`1"
            "H_Comparable`1"
            "H_ComparableOfSelf`1"
            "H_EnumerableOfInt`1"
            "H_EnumerableOfObject`1"
            "H_ListOfObject`1"
            "H_OutOfBase`1"
            "H_Enum`1"
            "H_Delegate`1"
            "H_AllowsRef`1"
            "H_AllowsRefMarker`1"
            "H_StructMarker`1"
            "H_ClassMarker`1"
            "H_NewBase`1"
            "H_Unmanaged`1"
            "H_Svm`1"
            "H_InheritsSvm`1"
        ]

    let private pairHolders : string list =
        [ "H2_MarkerThenBase`2" ; "H2_FirstBoundsSecond`2" ]

    /// The generic methods of `Outer<U>`, each instantiated over one argument after `Outer` is
    /// instantiated over another.
    let private outerMethods : string list =
        [ "Bounded" ; "ComparableOfOuter" ; "Class" ]

    let private image : byte[] =
        Roslyn.compileAssembly "ConstraintSatisfactionCorpus" OutputKind.DynamicallyLinkedLibrary [] [ corpusSource ]

    let private hostAssembly : Reflection.Assembly = Reflection.Assembly.Load image

    let private loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory =
        let _, loggerFactory = LoggerFactory.makeTest ()
        loggerFactory

    let private corelib : DumpedAssembly =
        Assembly.readFile loggerFactory HostImage.coreLibPath

    let private corpus : DumpedAssembly =
        use stream = new MemoryStream (image)
        AssemblyApi.read loggerFactory None stream

    let private bct : BaseClassTypes<DumpedAssembly> = BaseClassTypes.ofCorelib corelib

    let private initialState : TypeSystemState =
        let loaded = LoadedAssemblies.ofAssemblies [ corelib ; corpus ]

        { TypeSystemState.Empty with
            _LoadedAssemblies = loaded
            ConcreteTypes = Corelib.concretizeAll loaded bct AllConcreteTypes.Empty
        }

    let private corpusType (name : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        corpus.TypeDefs.Values
        |> Seq.tryFind (fun ty -> ty.Name = name)
        |> Option.defaultWith (fun () -> failwith $"corpus type %s{name} not found")

    let private hostType (name : string) : Type =
        match hostAssembly.GetType name with
        | null -> failwith $"corpus type %s{name} not found on the host"
        | ty -> ty

    let private corelibType (ns : string) (name : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        corelib.TryGetTopLevelTypeDef ns name
        |> Option.defaultWith (fun () -> failwith $"%s{ns}.%s{name} not found in CoreLib")

    let private nominal (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>) : TypeDefn =
        TypeDefn.FromDefinition (
            typeInfo.Identity,
            LoadedTypeInfo.signatureTypeKind bct initialState._LoadedAssemblies typeInfo
        )

    /// A type argument, as a tree the generator builds.
    type private Argument =
        | Leaf of name : string
        | ArrayOf of Argument
        | ListOf of Argument
        | EnumerableOf of Argument
        | NullableOf of Argument
        | PairOf of Argument * Argument
        | OutOf of Argument

    /// The leaves: each name, the type on the host and as PawPrint spells it, whether it is a value
    /// type other than `Nullable`, and whether it is byref-like.
    let private leaves : (string * Type * TypeDefn * bool * bool) list =
        let ofCorpus (name : string) (isValue : bool) (isByRefLike : bool) =
            name, hostType name, nominal (corpusType name), isValue, isByRefLike

        [
            "int", typeof<int>, TypeDefn.PrimitiveType PrimitiveType.Int32, true, false
            "string", typeof<string>, TypeDefn.PrimitiveType PrimitiveType.String, false, false
            "object", typeof<obj>, TypeDefn.PrimitiveType PrimitiveType.Object, false, false
            "DayOfWeek", typeof<DayOfWeek>, nominal (corelibType "System" "DayOfWeek"), true, false
            "Guid", typeof<Guid>, nominal (corelibType "System" "Guid"), true, false
            "Action", typeof<Action>, nominal (corelibType "System" "Action"), false, false
            "IComparable", typeof<IComparable>, nominal (corelibType "System" "IComparable"), false, false
            ofCorpus "IMarker" false false
            ofCorpus "ISvm" false false
            ofCorpus "BaseType" false false
            ofCorpus "DerivedType" false false
            ofCorpus "Unrelated" false false
            ofCorpus "AbstractDerived" false false
            ofCorpus "SealedMarker" false false
            ofCorpus "NoPublicCtor" false false
            ofCorpus "InternalCtor" false false
            ofCorpus "OutOfDerived" false false
            ofCorpus "AbstractSvm" false false
            ofCorpus "ConcreteSvm" false false
            ofCorpus "MarkedStruct" true false
            ofCorpus "PlainStruct" true false
            ofCorpus "MyEnum" true false
            ofCorpus "MarkedRefStruct" true true
            ofCorpus "PlainRefStruct" true true
        ]

    let private leaf (name : string) =
        leaves |> List.find (fun (leafName, _, _, _, _) -> leafName = name)

    let rec private isByRefLike (argument : Argument) : bool =
        match argument with
        | Leaf name ->
            let _, _, _, _, byRefLike = leaf name
            byRefLike
        | _ -> false

    /// Whether the argument is a value type other than an instantiation of `Nullable`.
    let rec private isPlainValueType (argument : Argument) : bool =
        match argument with
        | Leaf name ->
            let _, _, _, isValue, _ = leaf name
            isValue
        | PairOf _ -> true
        | ArrayOf _
        | ListOf _
        | EnumerableOf _
        | NullableOf _
        | OutOf _ -> false

    let rec private toHost (argument : Argument) : Type =
        match argument with
        | Leaf name ->
            let _, host, _, _, _ = leaf name
            host
        | ArrayOf element -> (toHost element).MakeArrayType ()
        | ListOf element -> typedefof<List<obj>>.MakeGenericType (toHost element)
        | EnumerableOf element -> typedefof<IEnumerable<obj>>.MakeGenericType (toHost element)
        | NullableOf element -> typedefof<Nullable<int>>.MakeGenericType (toHost element)
        | PairOf (key, value) -> typedefof<KeyValuePair<obj, obj>>.MakeGenericType (toHost key, toHost value)
        | OutOf element -> (hostType "IOut`1").MakeGenericType (toHost element)

    let rec private toDefn (argument : Argument) : TypeDefn =
        let instantiation (generic : TypeInfo<GenericParamFromMetadata, TypeDefn>) (arguments : Argument list) =
            TypeDefn.GenericInstantiation (nominal generic, arguments |> List.map toDefn |> ImmutableArray.CreateRange)

        match argument with
        | Leaf name ->
            let _, _, defn, _, _ = leaf name
            defn
        | ArrayOf element -> TypeDefn.OneDimensionalArrayLowerBoundZero (toDefn element)
        | ListOf element -> instantiation (corelibType "System.Collections.Generic" "List`1") [ element ]
        | EnumerableOf element -> instantiation (corelibType "System.Collections.Generic" "IEnumerable`1") [ element ]
        | NullableOf element -> instantiation bct.Nullable [ element ]
        | PairOf (key, value) ->
            instantiation (corelibType "System.Collections.Generic" "KeyValuePair`2") [ key ; value ]
        | OutOf element -> instantiation (corpusType "IOut`1") [ element ]

    let rec private describe (argument : Argument) : string =
        match argument with
        | Leaf name -> name
        | ArrayOf element -> describe element + "[]"
        | ListOf element -> $"List<%s{describe element}>"
        | EnumerableOf element -> $"IEnumerable<%s{describe element}>"
        | NullableOf element -> $"Nullable<%s{describe element}>"
        | PairOf (key, value) -> $"KeyValuePair<%s{describe key}, %s{describe value}>"
        | OutOf element -> $"IOut<%s{describe element}>"

    /// Arguments that are legal types: a byref-like type only at the top, and `Nullable` only over
    /// a value type that is not itself one.
    let private genArgument : Gen<Argument> =
        let leafNames = leaves |> List.map (fun (name, _, _, _, _) -> name)

        let heapLeaves =
            leaves
            |> List.filter (fun (_, _, _, _, byRefLike) -> not byRefLike)
            |> List.map (fun (name, _, _, _, _) -> Leaf name)

        let rec nested (size : int) : Gen<Argument> =
            if size <= 0 then
                Gen.elements heapLeaves
            else
                let smaller = nested (size / 2)

                Gen.frequency
                    [
                        3, Gen.elements heapLeaves
                        1, Gen.map ArrayOf smaller
                        1, Gen.map ListOf smaller
                        1, Gen.map EnumerableOf smaller
                        1, Gen.map OutOf smaller
                        1, Gen.map2 (fun key value -> PairOf (key, value)) smaller smaller
                        1,
                        smaller
                        |> Gen.map (fun element ->
                            if isPlainValueType element then
                                NullableOf element
                            else
                                NullableOf (Leaf "int")
                        )
                    ]

        Gen.frequency [ 1, Gen.elements (leafNames |> List.map Leaf) ; 3, Gen.sized nested ]

    /// `GenericArguments[N]` opens the host's message, naming the parameter it rejected.
    let private violatedIndex (message : string) : int =
        let m = Regex.Match (message, @"^GenericArguments\[(\d+)\]")

        if m.Success then
            int m.Groups.[1].Value
        else
            failwith $"the host's rejection has an unparseable message: %s{message}"

    /// The host's verdict on instantiating `make`'s generic: `None` if admitted, or the index of the
    /// parameter it rejected.
    let private hostVerdict (make : unit -> obj) : int option =
        try
            make () |> ignore<obj>
            None
        with :? ArgumentException as e ->
            Some (violatedIndex e.Message)

    let private concretize (state : TypeSystemState) (argument : Argument) : TypeSystemState * ConcreteTypeHandle =
        match
            TypeSystemState.concretizeType
                loggerFactory
                HostImage.runtimeDirs
                bct
                state
                corpus.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                (toDefn argument)
        with
        | state, Ok handle -> state, handle
        | _, Error failure -> failwith $"%s{describe argument} does not bind: %s{TypeBindingFailure.describe failure}"

    /// The verdict on a type holder's instantiation over `arguments`.
    let private typeVerdict
        (state : TypeSystemState)
        (holder : string)
        (arguments : Argument list)
        : TypeSystemState * ConstraintVerdict
        =
        let state, handles =
            ((state, []), arguments)
            ||> List.fold (fun (state, handles) argument ->
                let state, handle = concretize state argument
                state, handle :: handles
            )

        let handles = List.rev handles
        let typeInfo = corpusType holder

        match
            ConstraintSatisfaction.satisfiesClosed
                loggerFactory
                HostImage.runtimeDirs
                bct
                state
                corpus.DefinitionFullName
                (ImmutableArray.CreateRange handles)
                ImmutableArray.Empty
                typeInfo.Generics
                handles
        with
        | state, Ok verdict -> state, verdict
        | _, Error failure -> failwith $"%s{holder}'s constraints do not bind: %s{TypeBindingFailure.describe failure}"

    /// The verdict on `Outer<outer>.method<argument>`.
    let private methodVerdict
        (state : TypeSystemState)
        (method : string)
        (outer : Argument)
        (argument : Argument)
        : TypeSystemState * ConstraintVerdict
        =
        let state, outerHandle = concretize state outer
        let state, handle = concretize state argument

        let definition =
            (corpusType "Outer`1").Methods
            |> List.find (fun candidate -> candidate.Name = method)

        match
            ConstraintSatisfaction.satisfiesClosed
                loggerFactory
                HostImage.runtimeDirs
                bct
                state
                corpus.DefinitionFullName
                (ImmutableArray.Create outerHandle)
                (ImmutableArray.Create handle)
                definition.Generics
                [ handle ]
        with
        | state, Ok verdict -> state, verdict
        | _, Error failure ->
            failwith $"Outer.%s{method}'s constraints do not bind: %s{TypeBindingFailure.describe failure}"

    /// How a verdict disagrees with the host's, if it does. `Undecided` stands for either, at its
    /// parameter or later.
    let private disagreement (host : int option) (verdict : ConstraintVerdict) : string option =
        match host, verdict with
        | None, ConstraintVerdict.Satisfied -> None
        | Some index, ConstraintVerdict.Violated violation when violation.Parameter = index -> None
        | None, ConstraintVerdict.Undecided _ -> None
        | Some index, ConstraintVerdict.Undecided (parameter, _) when parameter <= index -> None
        | _ -> Some $"the host's verdict is %A{host}, TypeSystem's %A{verdict}"

    /// Counts of what the host decided, so that a run that only ever admits, or only ever rejects,
    /// fails instead of passing vacuously.
    type private Tally =
        {
            mutable Admitted : int
            mutable Rejected : int
            mutable Undecided : int
        }

    let private check
        (tally : Tally)
        (state : TypeSystemState ref)
        (name : string)
        (host : int option)
        (verdict : TypeSystemState -> TypeSystemState * ConstraintVerdict)
        : unit
        =
        let next, verdict = verdict state.Value
        state.Value <- next

        match verdict with
        | ConstraintVerdict.Undecided _ -> tally.Undecided <- tally.Undecided + 1
        | _ ->
            match host with
            | None -> tally.Admitted <- tally.Admitted + 1
            | Some _ -> tally.Rejected <- tally.Rejected + 1

        match disagreement host verdict with
        | None -> ()
        | Some problem -> failwith $"%s{name}: %s{problem}"

    /// Every holder, over one argument (each of two arguments, for the two-parameter holders), and
    /// every method of `Outer<U>` over a pair: the host and TypeSystem agree on each.
    let private property (tally : Tally) (state : TypeSystemState ref) (argument : Argument, other : Argument) =
        for holder in singleHolders do
            check
                tally
                state
                $"%s{holder}<%s{describe argument}>"
                (hostVerdict (fun () -> box ((hostType holder).MakeGenericType (toHost argument))))
                (fun state -> typeVerdict state holder [ argument ])

        // A byref-like argument is legal in no position of the others.
        if not (isByRefLike argument || isByRefLike other) then
            for holder in pairHolders do
                check
                    tally
                    state
                    $"%s{holder}<%s{describe argument}, %s{describe other}>"
                    (hostVerdict (fun () -> box ((hostType holder).MakeGenericType (toHost argument, toHost other))))
                    (fun state -> typeVerdict state holder [ argument ; other ])

            for method in outerMethods do
                check
                    tally
                    state
                    $"Outer<%s{describe other}>.%s{method}<%s{describe argument}>"
                    (hostVerdict (fun () ->
                        box (
                            (hostType("Outer`1").MakeGenericType(toHost other).GetMethod method)
                                .MakeGenericMethod (toHost argument)
                        )
                    ))
                    (fun state -> methodVerdict state method other argument)

    [<Test>]
    let ``every leaf argument, under every holder, agrees with the host`` () : unit =
        let tally =
            {
                Admitted = 0
                Rejected = 0
                Undecided = 0
            }

        let state = ref initialState
        let leafArguments = leaves |> List.map (fun (name, _, _, _, _) -> Leaf name)

        for argument in leafArguments do
            for other in leafArguments do
                property tally state (argument, other)

        // The abstract arguments against the interface with a static abstract method are the
        // undecided ones.
        Assert.That (tally.Admitted, Is.GreaterThan 100)
        Assert.That (tally.Rejected, Is.GreaterThan 100)
        Assert.That (tally.Undecided, Is.GreaterThan 0)

    [<Test>]
    let ``generated instantiations, under every holder, agree with the host`` () : unit =
        let tally =
            {
                Admitted = 0
                Rejected = 0
                Undecided = 0
            }

        let state = ref initialState

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 200,
            Prop.forAll (Arb.fromGen (Gen.zip genArgument genArgument)) (property tally state)
        )

        let decided = tally.Admitted + tally.Rejected
        Assert.That (tally.Admitted, Is.GreaterThan (decided / 10))
        Assert.That (tally.Rejected, Is.GreaterThan (decided / 10))

    /// A type parameter carrying both the `class` and the `struct` flag, which metadata can spell
    /// and C# cannot: CoreCLR rejects every argument, which the host's `MakeGenericType` confirms.
    [<Test>]
    let ``a parameter constrained both class and struct admits nothing, as on the host`` () : unit =
        let fabricated =
            let builder =
                Reflection.Emit.PersistedAssemblyBuilder (
                    Reflection.AssemblyName "ClassAndStruct",
                    typeof<obj>.Assembly
                )

            let modul = builder.DefineDynamicModule "ClassAndStruct"

            let holder =
                modul.DefineType (
                    "ClassAndStruct`1",
                    Reflection.TypeAttributes.Public ||| Reflection.TypeAttributes.Class
                )

            (holder.DefineGenericParameters [| "T" |])
                .[0].SetGenericParameterAttributes (
                    Reflection.GenericParameterAttributes.ReferenceTypeConstraint
                    ||| Reflection.GenericParameterAttributes.NotNullableValueTypeConstraint
                )

            holder.CreateType () |> ignore<Type>
            use stream = new MemoryStream ()
            builder.Save stream
            stream.ToArray ()

        let hostHolder = (Reflection.Assembly.Load fabricated).GetType "ClassAndStruct`1"

        let assembly =
            use stream = new MemoryStream (fabricated)
            AssemblyApi.read loggerFactory None stream

        let holder =
            assembly.TypeDefs.Values |> Seq.find (fun ty -> ty.Name = "ClassAndStruct`1")

        let mutable state = initialState.WithLoadedAssembly assembly

        let failures =
            [
                for name, host, _, _, _ in leaves do
                    match hostVerdict (fun () -> box (hostHolder.MakeGenericType host)) with
                    | None -> yield $"ClassAndStruct<%s{name}>: the host admits it"
                    | Some _ -> ()

                    let next, handle = concretize state (Leaf name)

                    match
                        ConstraintSatisfaction.satisfiesClosed
                            loggerFactory
                            HostImage.runtimeDirs
                            bct
                            next
                            assembly.DefinitionFullName
                            (ImmutableArray.Create handle)
                            ImmutableArray.Empty
                            holder.Generics
                            [ handle ]
                    with
                    | next, Ok (ConstraintVerdict.Violated _) -> state <- next
                    | _, other -> yield $"ClassAndStruct<%s{name}>: TypeSystem's verdict is %A{other}"
            ]

        if not failures.IsEmpty then
            failwith (String.concat "\n" failures)

    /// `Generics::CheckInstantiation` refuses a byref, a pointer, a function pointer,
    /// `TypedReference` and `void` as an argument, and nothing else.
    [<Test>]
    let ``a byref, pointer, function pointer, TypedReference or void argument is malformed`` () : unit =
        let state, int32 = concretize initialState (Leaf "int")

        let nominalOf (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>) (state : TypeSystemState) =
            match
                TypeSystemState.concretizeType
                    loggerFactory
                    HostImage.runtimeDirs
                    bct
                    state
                    corelib.DefinitionFullName
                    ImmutableArray.Empty
                    ImmutableArray.Empty
                    (nominal typeInfo)
            with
            | state, Ok handle -> state, handle
            | _, Error failure -> failwith (TypeBindingFailure.describe failure)

        let state, typedReference = nominalOf bct.TypedReference state
        let state, voidType = nominalOf bct.Void state

        let functionPointer =
            ConcreteTypeHandle.FunctionPointer
                {
                    Header =
                        ComparableSignatureHeader.Make (
                            SignatureHeader (
                                SignatureKind.Method,
                                SignatureCallingConvention.Default,
                                SignatureAttributes.None
                            )
                        )
                    ParameterTypes = []
                    GenericParameterCount = 0
                    RequiredParameterCount = 0
                    ReturnType = MethodReturnType.Returns int32
                }

        let malformed (arguments : ConcreteTypeHandle list) =
            ConstraintSatisfaction.malformedArgument bct state arguments

        malformed [ int32 ; ConcreteTypeHandle.Byref int32 ]
        |> shouldEqual (Some (1, MalformedArgument.Byref))

        malformed [ ConcreteTypeHandle.Pointer int32 ]
        |> shouldEqual (Some (0, MalformedArgument.Pointer))

        malformed [ functionPointer ]
        |> shouldEqual (Some (0, MalformedArgument.FunctionPointer))

        malformed [ typedReference ]
        |> shouldEqual (Some (0, MalformedArgument.TypedReference))

        malformed [ voidType ] |> shouldEqual (Some (0, MalformedArgument.Void))

        malformed
            [
                int32
                ConcreteTypeHandle.OneDimArrayZero int32
                ConcreteTypeHandle.Array (int32, 2)
            ]
        |> shouldEqual None
