namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// `typeof(D<>).GetMethod("M").MakeGenericMethod(arg)` binds a generic method of an open generic
/// type *definition*, and CoreCLR validates the method's constraints against the definition's
/// unbound formals (`MethodDesc::SatisfiesMethodConstraints`, genmeth.cpp:1594). Whether a closed
/// argument satisfies `where U : IIn<T>` then turns on variance and on what `T`'s own constraints
/// let it be cast to. `NativeRuntimeTypeHelpers.validateConstraintsOn`, given the declaring type's
/// variables as `Open`, is PawPrint's answer; here one corpus of declaring definitions, constraint
/// shapes and arguments is loaded into both PawPrint and the host CLR, and the two must agree on
/// every (definition, method, argument) triple.
///
/// The host is an outside oracle: nothing about the expected verdict is read out of the structure
/// under test. The corpus honours the same exclusions as `TestGenericConstraintSatisfaction`: no
/// pointer, byref or `void` argument, which `SanityCheckGenericArguments` screens before the
/// QCall, and no interface declaring a static abstract member.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestOpenDeclaringConstraintSatisfaction =

    /// (method name, constraint clause) for the methods each declaring definition declares, over
    /// its own first parameter `T`.
    let private methods : (string * string) list =
        [
            "M_T", "where U : T"
            "M_InT", "where U : IIn<T>"
            "M_OutT", "where U : IOut<T>"
            "M_InvT", "where U : IInv<T>"
            "M_InInT", "where U : IIn<IIn<T>>"
            "M_OutInT", "where U : IOut<IIn<T>>"
            "M_InOutT", "where U : IIn<IOut<T>>"
            "M_InListT", "where U : IIn<List<T>>"
            "M_ListT", "where U : List<T>"
            "M_EnumerableT", "where U : IEnumerable<T>"
            "M_EnumerableInT", "where U : IEnumerable<IIn<T>>"
            "M_IListInT", "where U : IList<IIn<T>>"
            "M_InPairT", "where U : IInPair<T, int>"
            "M_ClassInT", "where U : class, IIn<T>"
            "M_InTAndMarker", "where U : IIn<T>, IMarker"
            "M_InU", "where U : IIn<U>"
        ]

    /// (C# header, metadata name, methods C# refuses to declare there) for each declaring
    /// definition. The spread is over what `T` may be cast to, and over whether it counts as an
    /// object reference (`ConstrainedAsObjRef`).
    let private declaringDefinitions : (string * string * Set<string>) list =
        [
            "public class D_None<T>", "D_None`1", Set.empty
            "public class D_Class<T> where T : class", "D_Class`1", Set.empty
            // CS0456: a `struct`-constrained parameter cannot constrain another.
            "public class D_Struct<T> where T : struct", "D_Struct`1", Set.singleton "M_T"
            "public class D_Base<T> where T : Base", "D_Base`1", Set.empty
            "public class D_Marker<T> where T : IMarker", "D_Marker`1", Set.empty
            "public class D_ClassMarker<T> where T : class, IMarker", "D_ClassMarker`1", Set.empty
            "public class D_Enum<T> where T : Enum", "D_Enum`1", Set.empty
            "public class D_ViaBaseVar<T, S> where T : S where S : Base", "D_ViaBaseVar`2", Set.empty
            "public class D_ViaClassVar<T, S> where T : S where S : class", "D_ViaClassVar`2", Set.empty
            "public class D_InOfSelf<T> where T : class, IIn<T>", "D_InOfSelf`1", Set.empty
            // Revisits (T, IIn<RecIn>) through variance, which must end rather than be mistaken
            // for a constraint cycle.
            "public class D_InInOfSelf<T> where T : class, IIn<IIn<T>>", "D_InInOfSelf`1", Set.empty
        ]

    let private corpusSource : string =
        let definitions =
            declaringDefinitions
            |> List.map (fun (header, _, excluded) ->
                let body =
                    methods
                    |> List.filter (fun (name, _) -> not (Set.contains name excluded))
                    |> List.map (fun (name, clause) -> $"    public void %s{name}<U>() %s{clause} {{ }}")
                    |> String.concat "\n"

                $"%s{header}\n{{\n%s{body}\n}}\n"
            )
            |> String.concat "\n"

        """
using System;
using System.Collections.Generic;

public interface IIn<in T> { }
public interface IOut<out T> { }
public interface IInv<T> { }
public interface IInPair<in A, B> { }
public interface IMarker { }

public class Base { }
public class Derived : Base, IMarker { }
public class Unrelated { }
public struct Val { }

public class InOfObject : IIn<object> { }
public class InOfBase : IIn<Base> { }
public class DerivedInOfObject : InOfObject { }
public interface IInOfObjectSub : IIn<object> { }
public class ImplementsInOfObjectSub : IInOfObjectSub { }
public struct InOfObjectStruct : IIn<object> { }
public class OutOfDerived : IOut<Derived> { }
public class InOfObjectMarker : IIn<object>, IMarker { }
public class ListOfObject : List<object> { }
public class SelfIn : IIn<SelfIn> { }
public class RecIn : IIn<IIn<RecIn>> { }

// A constraint naming its own declaring definition over that definition's own formal is the
// typical instantiation, which is the definition itself. C# refuses a variant interface's own
// parameter in a method constraint in either direction (CS1961), so only invariant ones appear.
public interface ISelf<T> where T : class { void M_Self<U>() where U : ISelf<T> { } }
public class SelfOfObject : ISelf<object> { }
public class D_Self<T> where T : class { public void M_Self<U>() where U : D_Self<T> { } }

"""
        + definitions

    let private image : byte[] =
        Roslyn.compileAssembly
            "OpenDeclaringConstraintOracleCorpus"
            OutputKind.DynamicallyLinkedLibrary
            []
            [ corpusSource ]

    /// The same image the PawPrint side reads, loaded by the real CLR. Non-collectible, which is
    /// fine: nothing here is a guest, so the collectibility that would disqualify a `RealRuntime`
    /// oracle does not apply.
    let private hostAssembly : Reflection.Assembly = Reflection.Assembly.Load image

    // The factory is intentionally undisposed: the DumpedAssembly loggers close over its sinks and
    // outlive this scope.
    let private loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory =
        let _, loggerFactory = LoggerFactory.makeTest ()
        loggerFactory

    let private corelib : DumpedAssembly =
        Assembly.readFile loggerFactory typeof<obj>.Assembly.Location

    let private guest : DumpedAssembly =
        use stream = new MemoryStream (image)
        AssemblyApi.read loggerFactory None stream

    let private bct : BaseClassTypes<DumpedAssembly> = BaseClassTypes.ofCorelib corelib

    let private initialState : IlMachineState =
        let withAssemblies =
            (IlMachineState.initial loggerFactory ImmutableArray.Empty guest).WithLoadedAssembly corelib

        { withAssemblies with
            ConcreteTypes = Corelib.concretizeAll withAssemblies._LoadedAssemblies bct AllConcreteTypes.Empty
        }

    let private guestType (name : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        guest.TypeDefs.Values
        |> Seq.tryFind (fun ty -> ty.Name = name)
        |> Option.defaultWith (fun () -> failwith $"corpus type %s{name} not found in the PawPrint-read image")

    let private hostType (name : string) : Type =
        match hostAssembly.GetType name with
        | null -> failwith $"corpus type %s{name} not found in the host-loaded image"
        | ty -> ty

    /// A candidate argument, spelled once for each side: as a PawPrint `TypeDefn` and as a host
    /// `Type`.
    type private Argument =
        {
            Name : string
            Defn : TypeDefn
            Host : Type
        }

    let private ofGuest (name : string) : Argument =
        let typeInfo = guestType name

        {
            Name = name
            Defn =
                TypeDefn.FromDefinition (
                    typeInfo.Identity,
                    LoadedTypeInfo.signatureTypeKind bct initialState._LoadedAssemblies typeInfo
                )
            Host = hostType name
        }

    let private ofCorelib (defn : TypeDefn) (host : Type) : Argument =
        {
            Name = string<Type> host
            Defn = defn
            Host = host
        }

    /// A guest or corelib generic definition applied to arguments, on both sides at once.
    let private instantiate
        (definition : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (hostDefinition : Type)
        (args : Argument list)
        : Argument
        =
        {
            Name =
                let rendered = args |> List.map (fun a -> a.Name) |> String.concat ", "
                $"%s{definition.Name}<%s{rendered}>"
            Defn =
                TypeDefn.GenericInstantiation (
                    TypeDefn.FromDefinition (
                        definition.Identity,
                        LoadedTypeInfo.signatureTypeKind bct initialState._LoadedAssemblies definition
                    ),
                    args |> List.map (fun a -> a.Defn) |> ImmutableArray.CreateRange
                )
            Host = hostDefinition.MakeGenericType (args |> List.map (fun a -> a.Host) |> List.toArray)
        }

    let private argumentPool : Argument list =
        let object' = ofCorelib (TypeDefn.PrimitiveType PrimitiveType.Object) typeof<obj>
        let string' = ofCorelib (TypeDefn.PrimitiveType PrimitiveType.String) typeof<string>
        let int' = ofCorelib (TypeDefn.PrimitiveType PrimitiveType.Int32) typeof<int>
        let base' = ofGuest "Base"
        let derived = ofGuest "Derived"

        let array (element : Argument) : Argument =
            {
                Name = $"%s{element.Name}[]"
                Defn = TypeDefn.OneDimensionalArrayLowerBoundZero element.Defn
                Host = element.Host.MakeArrayType ()
            }

        let generic (name : string) (args : Argument list) : Argument =
            instantiate (guestType name) (hostType name) args

        let list (arg : Argument) : Argument =
            let listInfo =
                corelib.TryGetTopLevelTypeDef "System.Collections.Generic" "List`1"
                |> Option.defaultWith (fun () -> failwith "List`1 not found in corelib")

            instantiate listInfo typedefof<System.Collections.Generic.List<obj>> [ arg ]

        [
            object'
            string'
            int'
            base'
            derived
            ofGuest "Unrelated"
            ofGuest "Val"
            ofGuest "InOfObject"
            ofGuest "InOfBase"
            ofGuest "DerivedInOfObject"
            ofGuest "IInOfObjectSub"
            ofGuest "ImplementsInOfObjectSub"
            ofGuest "InOfObjectStruct"
            ofGuest "OutOfDerived"
            ofGuest "InOfObjectMarker"
            ofGuest "ListOfObject"
            ofGuest "SelfIn"
            ofGuest "RecIn"
            generic "IIn`1" [ generic "IIn`1" [ ofGuest "RecIn" ] ]
            // Through `M_InPairT`, this asks (T, IIn<RecIn>) a second time under D_InInOfSelf.
            generic "IInPair`2" [ generic "IIn`1" [ ofGuest "RecIn" ] ; int' ]
            generic "IIn`1" [ object' ]
            generic "IIn`1" [ string' ]
            generic "IIn`1" [ base' ]
            generic "IIn`1" [ int' ]
            generic "IOut`1" [ object' ]
            generic "IOut`1" [ derived ]
            generic "IInv`1" [ object' ]
            generic "IIn`1" [ generic "IIn`1" [ derived ] ]
            generic "IIn`1" [ generic "IIn`1" [ object' ] ]
            generic "IOut`1" [ generic "IIn`1" [ object' ] ]
            generic "IIn`1" [ generic "IOut`1" [ object' ] ]
            generic "IIn`1" [ generic "IOut`1" [ base' ] ]
            generic "IIn`1" [ list object' ]
            generic "IIn`1" [ generic "IInv`1" [ object' ] ]
            generic "IInPair`2" [ object' ; int' ]
            generic "IInPair`2" [ object' ; object' ]
            generic "IInPair`2" [ string' ; int' ]
            ofGuest "SelfOfObject"
            generic "ISelf`1" [ object' ]
            generic "D_Self`1" [ object' ]
            list object'
            list string'
            array object'
            array string'
            array (generic "IIn`1" [ object' ])
            array (generic "IIn`1" [ string' ])
        ]

    /// The host's verdict: whether `MakeGenericMethod` binds.
    let private hostAccepts (definition : string) (methodName : string) (argument : Type) : bool =
        let method = (hostType definition).GetMethod methodName

        try
            method.MakeGenericMethod [| argument |] |> ignore
            true
        with :? ArgumentException ->
            false

    /// PawPrint's verdict, from the validator `RuntimeMethodHandle_GetStubIfNeededSlow` runs, with
    /// the declaring type's variables left as the definition's own formals.
    let private pawPrintAccepts
        (state : IlMachineState)
        (definition : string)
        (methodName : string)
        (argument : TypeDefn)
        : IlMachineState * bool
        =
        let state, argHandle =
            IlMachineState.concretizeType
                loggerFactory
                bct
                state
                guest.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                argument

        let typeInfo = guestType definition

        let method =
            typeInfo.Methods
            |> List.filter (fun m -> m.Name = methodName)
            |> List.exactlyOne

        let formals =
            Seq.init
                typeInfo.Generics.Length
                (fun index -> RuntimeTypeHandleTarget.GenericParameter (typeInfo.Identity, index))
            |> ImmutableArray.CreateRange

        let state, violation =
            NativeRuntimeTypeHelpers.validateConstraintsOn
                loggerFactory
                bct
                state
                $"%s{definition}.%s{methodName}"
                typeInfo.AssemblyFullName
                (ReflectedTypeTarget.ReflectionVariableBinding.Open formals)
                (ImmutableArray.Create argHandle)
                method.Generics
                [ argHandle ]

        state, violation.IsNone

    let private cases : (string * string) list =
        [
            for _, definition, excluded in declaringDefinitions do
                for methodName, _ in methods do
                    if not (Set.contains methodName excluded) then
                        yield definition, methodName
            yield "ISelf`1", "M_Self"
            yield "D_Self`1", "M_Self"
        ]

    [<Test>]
    let ``every (open definition, method constraint, argument) triple agrees with the host CLR`` () : unit =
        let mutable state = initialState
        let mutable disagreements = []
        // Per method, how many arguments the host accepted, so that a method which accepts
        // everything or nothing is visible.
        let mutable acceptedByMethod = Map.empty

        for definition, methodName in cases do
            for argument in argumentPool do
                let expected = hostAccepts definition methodName argument.Host
                let state', actual = pawPrintAccepts state definition methodName argument.Defn
                state <- state'

                if expected then
                    acceptedByMethod <-
                        acceptedByMethod
                        |> Map.change (definition, methodName) (Option.defaultValue 0 >> (+) 1 >> Some)

                if expected <> actual then
                    disagreements <-
                        $"%s{definition}.%s{methodName}<%s{argument.Name}>: host accepts %b{expected}, PawPrint %b{actual}"
                        :: disagreements

        if not (List.isEmpty disagreements) then
            failwith (
                $"%d{List.length disagreements} triples disagree with the host CLR:\n"
                + String.concat "\n" (List.rev disagreements)
            )

        let accepted (definition : string) (methodName : string) : int =
            acceptedByMethod
            |> Map.tryFind (definition, methodName)
            |> Option.defaultValue 0

        // The shapes the corpus exists for must actually discriminate, or the agreement above says
        // nothing about them. `U : IIn<T>` under `T : class` is the pair the parked guest pinned:
        // some closed argument is accepted and some rejected, so neither "a constraint mentioning
        // a formal admits nothing" nor "... admits everything" survives.
        let poolSize = List.length argumentPool

        for definition, methodName in
            [
                "D_Class`1", "M_InT"
                "D_Base`1", "M_InT"
                "D_ViaBaseVar`2", "M_InT"
                "D_Class`1", "M_InOutT"
                "D_Class`1", "M_InPairT"
                "D_Class`1", "M_EnumerableInT"
                "D_Class`1", "M_IListInT"
            ] do
            let count = accepted definition methodName
            Assert.That (count, Is.GreaterThan 0, $"%s{definition}.%s{methodName} accepts no argument")
            Assert.That (count, Is.LessThan poolSize, $"%s{definition}.%s{methodName} accepts every argument")

        // `ConstrainedAsObjRef` must matter somewhere: a formal that may be a value type blocks
        // the contravariant match that a reference-constrained one allows.
        Assert.That (accepted "D_None`1" "M_InT", Is.LessThan (accepted "D_Class`1" "M_InT"))
        // Nothing is assignable to an unbound formal.
        Assert.That (accepted "D_Class`1" "M_T", Is.EqualTo 0)
