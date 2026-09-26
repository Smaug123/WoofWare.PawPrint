namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata.Ecma335
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Shapes of `RuntimeTypeHandle_GetMethodAt`'s receiver that corelib's corpus cannot supply: open
/// generic definitions, the open constructions on their chains, and arrays over a type variable. A
/// Roslyn-compiled corpus is read by PawPrint and loaded into the host CLR, whose own
/// `GetMethodAt` and `GetMethodBase` say what the answer is.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMethodAtSlotShapes =

    let private corpusSource : string =
        """
namespace PawPrint.MethodAtSlot
{
    public class Base<A>
    {
        public virtual A Value => default(A);
        public virtual A Echo(A value) => value;
        public override string ToString() => "base";
    }

    public class Mid<A, B> : Base<A>
    {
        public override A Echo(A value) => value;
        public virtual B Other(B value) => value;
    }

    // `Base<A>` is reached through `Mid<int, T>`: its one argument is spelled `int32` under a
    // context whose second entry is the definition's own formal, which nothing mentions.
    public class TwoStep<T> : Mid<int, T>
    {
        public override int Echo(int value) => value + 1;
    }

    public class HasArray<T>
    {
        public void Takes(T[] values) { }
    }

    // `Wrap<T of OverWrap>` is an open construction whose own ancestor, `Base<T of OverWrap>`, is
    // an open construction too.
    public class Wrap<A> : Base<A> { }

    public class OverWrap<T> : Wrap<T>
    {
        public override T Echo(T value) => value;
    }
}
"""

    let private loggerFactory = snd (LoggerFactory.makeTest ())
    let private corelibPath : string = typeof<obj>.Assembly.Location
    let private runtimeDir : string = Path.GetDirectoryName corelibPath
    let private corelib : DumpedAssembly = Assembly.readFile loggerFactory corelibPath
    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private corpusBytes : byte array =
        Roslyn.compileAssembly
            "PawPrint.MethodAtSlot"
            Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
            []
            [ corpusSource ]

    let private corpusAssembly : DumpedAssembly =
        use stream = new MemoryStream (corpusBytes)
        AssemblyApi.read loggerFactory (Some "PawPrint.MethodAtSlot.dll") stream

    let private hostAssembly : Assembly = Assembly.Load corpusBytes

    let private baseState : IlMachineState =
        let dirs = ImmutableArray.CreateRange [ runtimeDir ]
        let state = IlMachineState.initial loggerFactory dirs corelib

        let state =
            { state with
                ConcreteTypes = Corelib.concretizeAll state._LoadedAssemblies bct AllConcreteTypes.Empty
            }

        state.WithLoadedAssembly corpusAssembly

    let private typeInfoNamed (name : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        match corpusAssembly.TryGetTopLevelTypeDef "PawPrint.MethodAtSlot" name with
        | Some typeInfo -> typeInfo
        | None -> failwith $"corpus has no type named %s{name}"

    let private hostType (name : string) : Type =
        match hostAssembly.GetType ("PawPrint.MethodAtSlot." + name) with
        | null -> failwith $"host copy of the corpus has no type %s{name}"
        | t -> t

    /// The host's internal `GetMethodAt`, `GetNumVirtuals` and `GetMethodBase`, as
    /// `TestVirtualMethodSlots` reaches them.
    let private hostMethodAt : Type -> int -> MethodBase =
        let runtimeType = typeof<obj>.Assembly.GetType "System.RuntimeType"

        let internalHandle =
            typeof<obj>.Assembly.GetType "System.RuntimeMethodHandleInternal"

        let getMethodAt =
            typeof<RuntimeTypeHandle>
                .GetMethod (
                    "GetMethodAt",
                    BindingFlags.NonPublic ||| BindingFlags.Static,
                    null,
                    [| runtimeType ; typeof<int> |],
                    null
                )

        let getMethodBase =
            runtimeType.GetMethod (
                "GetMethodBase",
                BindingFlags.NonPublic ||| BindingFlags.Static,
                null,
                [| runtimeType ; internalHandle |],
                null
            )

        fun (t : Type) (slot : int) ->
            let handle = getMethodAt.Invoke ((null : obj), [| box t ; box slot |])
            getMethodBase.Invoke ((null : obj), [| box t ; handle |]) :?> MethodBase

    let private hostNumVirtuals : Type -> int =
        let runtimeType = typeof<obj>.Assembly.GetType "System.RuntimeType"

        let impl =
            typeof<RuntimeTypeHandle>
                .GetMethod (
                    "GetNumVirtuals",
                    BindingFlags.NonPublic ||| BindingFlags.Static,
                    null,
                    [| runtimeType |],
                    null
                )

        fun (t : Type) -> impl.Invoke ((null : obj), [| box t |]) :?> int

    /// A method's declaring type as both sides can spell it: the TypeDef token of its definition,
    /// and for each generic argument either the TypeDef token of a non-generic type (every closed
    /// argument in this corpus is one) or a type variable, named by its position and the TypeDef
    /// token of the definition declaring it. A definition's own arguments are its variables.
    let private hostDeclaringShape (method : MethodBase) : int * string list =
        let declaring = method.DeclaringType

        let definition =
            if declaring.IsGenericType then
                declaring.GetGenericTypeDefinition ()
            else
                declaring

        let argument (t : Type) : string =
            if t.IsGenericParameter then
                $"!%i{t.GenericParameterPosition} of %i{t.DeclaringType.MetadataToken}"
            else
                string t.MetadataToken

        definition.MetadataToken, (declaring.GetGenericArguments () |> Array.map argument |> List.ofArray)

    let private tokenOfTypeDefinition (identity : ResolvedTypeIdentity) : int =
        MetadataTokens.GetToken (
            System.Reflection.Metadata.TypeDefinitionHandle.op_Implicit identity.TypeDefinition.Get
            : System.Reflection.Metadata.EntityHandle
        )

    let private pawPrintDeclaringShape (state : IlMachineState) (target : RuntimeTypeHandleTarget) : int * string list =
        let closedArgument (argument : ConcreteTypeHandle) : string =
            match AllConcreteTypes.lookup argument state.ConcreteTypes with
            | Some argumentType -> string (tokenOfTypeDefinition argumentType.Identity)
            | None -> failwith $"generic argument %O{argument} is not a nominal type"

        let variable (owner : ResolvedTypeIdentity) (position : int) : string =
            $"!%i{position} of %i{tokenOfTypeDefinition owner}"

        match target with
        | RuntimeTypeHandleTarget.Closed handle ->
            let concreteType =
                AllConcreteTypes.lookup handle state.ConcreteTypes
                |> Option.defaultWith (fun () -> failwith $"declaring handle %O{handle} is not registered")

            tokenOfTypeDefinition concreteType.Identity, (concreteType.Generics |> Seq.map closedArgument |> List.ofSeq)
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            let arity =
                state._LoadedAssemblies
                    .ByDefinitionName(identity.AssemblyFullName)
                    .TypeDefs.[identity.TypeDefinition.Get].Generics.Length

            tokenOfTypeDefinition identity, List.init arity (variable identity)
        | RuntimeTypeHandleTarget.OpenConstructed (identity, arguments) ->
            let argument (argument : RuntimeTypeHandleTarget) : string =
                match argument with
                | RuntimeTypeHandleTarget.Closed handle -> closedArgument handle
                | RuntimeTypeHandleTarget.GenericParameter (owner, position) -> variable owner position
                | other -> failwith $"unexpected open-construction argument %O{other}"

            tokenOfTypeDefinition identity, List.map argument arguments
        | other -> failwith $"unexpected declaring target %O{other}"

    let private methodToken (slot : VtableSlot) : int =
        match fst slot.Method.IdentityKey with
        | Some handle ->
            MetadataTokens.GetToken (
                System.Reflection.Metadata.MethodDefinitionHandle.op_Implicit handle
                : System.Reflection.Metadata.EntityHandle
            )
        | None -> -1

    /// Every slot of `receiver` names the method the host names, declared by the type the host
    /// says declares it, with that type's generic arguments.
    let private checkEverySlot
        (label : string)
        (host : Type)
        (state : IlMachineState)
        (receiver : RuntimeTypeHandleTarget)
        : unit
        =
        let count = hostNumVirtuals host
        count |> shouldBeGreaterThan 0
        let mutable failures = []

        for slot in 0 .. count - 1 do
            let expected = hostMethodAt host slot

            let state, answer =
                VirtualSlotLayout.methodAt loggerFactory bct "test" state receiver slot

            match answer with
            | VirtualSlotLayout.MethodAtSlot.OutOfRange ->
                failures <-
                    $"%s{label} slot %i{slot}: PawPrint out of range, host %s{expected.DeclaringType.Name}.%s{expected.Name}"
                    :: failures
            | VirtualSlotLayout.MethodAtSlot.Method occupant ->
                if methodToken occupant <> expected.MetadataToken then
                    failures <-
                        $"%s{label} slot %i{slot}: PawPrint %s{occupant.Method.Name}, host %s{expected.Name}"
                        :: failures
                else
                    let state, declaring =
                        VirtualSlotLayout.declaringTypeAt loggerFactory bct "test" state receiver slot occupant

                    let actual = pawPrintDeclaringShape state declaring
                    let expectedShape = hostDeclaringShape expected

                    if actual <> expectedShape then
                        failures <-
                            $"%s{label} slot %i{slot} (%s{expected.Name}): PawPrint declares %A{actual}, host %A{expectedShape}"
                            :: failures

        if not failures.IsEmpty then
            failwith (String.Join ("\n", List.rev failures))

    /// Where each slot of `host` is declared, as the host reports it: by `host` itself, by a
    /// generic ancestor all of whose arguments are closed, or by one some of whose arguments are
    /// type variables. A law checked over a receiver is vacuous for any outcome that never occurs.
    let private declaringOutcomes (host : Type) : Set<string> =
        seq {
            for slot in 0 .. hostNumVirtuals host - 1 do
                let declaring = (hostMethodAt host slot).DeclaringType

                if declaring = host then
                    yield "own"
                elif declaring.IsGenericType then
                    if declaring.GetGenericArguments () |> Array.exists _.IsGenericParameter then
                        yield "open ancestor"
                    else
                        yield "closed ancestor"
        }
        |> Set.ofSeq

    /// The parent of `receiver`, as `Type.BaseType` reports it.
    let private parentOf (state : IlMachineState) (receiver : RuntimeTypeHandleTarget) : RuntimeTypeHandleTarget =
        match IlMachineState.resolveBaseRuntimeTypeHandleTarget loggerFactory bct state receiver with
        | _, Some parent -> parent
        | _, None -> failwith $"%O{receiver} has no parent"

    /// `TwoStep<T> : Mid<int, T> : Base<int>`, asked of the definition `TwoStep<>`: `Base`'s slots
    /// are declared by the closed `Base<int>` (its argument is spelled under a context whose other
    /// entry is `T`, which the spelling never mentions), and `Mid`'s `Other` by the open
    /// construction `Mid<int, T>`.
    [<Test>]
    let ``every slot of a definition is declared by its chain as the host instantiates it`` () : unit =
        let receiver =
            RuntimeTypeHandleTarget.OpenGenericTypeDefinition (typeInfoNamed "TwoStep`1").Identity

        let host = hostType "TwoStep`1"

        declaringOutcomes host
        |> shouldEqual (Set.ofList [ "own" ; "closed ancestor" ; "open ancestor" ])

        let _, count =
            VirtualSlotLayout.numVirtuals loggerFactory bct "test" baseState receiver

        count |> shouldEqual (hostNumVirtuals host)
        checkEverySlot "TwoStep<>" host baseState receiver

    /// The parent of `TwoStep<>`, the open construction `Mid<int, T>`, has its definition's slots,
    /// and names itself as the declarer of the ones `Mid` owns.
    [<Test>]
    let ``every slot of an open construction mixing closed and open arguments`` () : unit =
        let receiver =
            parentOf baseState (RuntimeTypeHandleTarget.OpenGenericTypeDefinition (typeInfoNamed "TwoStep`1").Identity)

        match receiver with
        | RuntimeTypeHandleTarget.OpenConstructed _ -> ()
        | other -> failwith $"expected the open construction Mid<int, T>, got %O{other}"

        let host = (hostType "TwoStep`1").BaseType

        declaringOutcomes host |> shouldEqual (Set.ofList [ "own" ; "closed ancestor" ])

        let _, count =
            VirtualSlotLayout.numVirtuals loggerFactory bct "test" baseState receiver

        count |> shouldEqual (hostNumVirtuals host)
        checkEverySlot "Mid<int, T>" host baseState receiver

    /// `OverWrap<T> : Wrap<T> : Base<T>`: asked of `OverWrap<>` and of its parent `Wrap<T>`, an
    /// inherited slot is declared by `Base<T>`, an open construction reached through another one.
    [<Test>]
    let ``an open construction's inherited slots are declared by an open ancestor`` () : unit =
        let definition =
            RuntimeTypeHandleTarget.OpenGenericTypeDefinition (typeInfoNamed "OverWrap`1").Identity

        let parent = parentOf baseState definition

        match parent with
        | RuntimeTypeHandleTarget.OpenConstructed _ -> ()
        | other -> failwith $"expected the open construction Wrap<T>, got %O{other}"

        let host = hostType "OverWrap`1"

        declaringOutcomes host |> shouldEqual (Set.ofList [ "own" ; "open ancestor" ])

        declaringOutcomes host.BaseType
        |> Set.contains "open ancestor"
        |> shouldEqual true

        for receiver, host in [ definition, host ; parent, host.BaseType ] do
            let _, count =
                VirtualSlotLayout.numVirtuals loggerFactory bct "test" baseState receiver

            count |> shouldEqual (hostNumVirtuals host)
            checkEverySlot (string receiver) host baseState receiver

    /// `T[]` is an array MethodTable whose slots are `System.Array`'s.
    [<Test>]
    let ``an array over a type variable has System.Array's slots`` () : unit =
        let hasArray = typeInfoNamed "HasArray`1"
        let formal = RuntimeTypeHandleTarget.GenericParameter (hasArray.Identity, 0)

        let receiver =
            RuntimeTypeHandleTarget.composite CompositeShape.OneDimArrayZero formal
        // The host spells the same type through reflection on the corpus.
        let host =
            (hostType "HasArray`1").GetMethod("Takes").GetParameters().[0].ParameterType

        host.IsArray |> shouldEqual true
        host.GetElementType().IsGenericParameter |> shouldEqual true

        let _, count =
            VirtualSlotLayout.numVirtuals loggerFactory bct "test" baseState receiver

        count |> shouldEqual (hostNumVirtuals host)
        checkEverySlot "T[]" host baseState receiver

    /// The closed instantiation is the control: `TwoStep<string>` closes everything, so every slot
    /// answers, and the base's declaring type is still `Base<int>`.
    [<Test>]
    let ``the closed instantiation answers every slot`` () : unit =
        let twoStep = typeInfoNamed "TwoStep`1"
        let state = baseState

        let state, stringHandle =
            LoadedTypeInfo.typeInfoToTypeDefn' bct state._LoadedAssemblies bct.String
            |> IlMachineState.concretizeType
                loggerFactory
                bct
                state
                corelib.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty

        let state, closed =
            LoadedTypeInfo.typeInfoToTypeDefn' bct state._LoadedAssemblies twoStep
            |> IlMachineState.concretizeType
                loggerFactory
                bct
                state
                corpusAssembly.DefinitionFullName
                (ImmutableArray.Create stringHandle)
                ImmutableArray.Empty

        let host = (hostType "TwoStep`1").MakeGenericType [| typeof<string> |]
        checkEverySlot "TwoStep<string>" host state (RuntimeTypeHandleTarget.Closed closed)
