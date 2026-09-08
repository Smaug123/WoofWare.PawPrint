namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata.Ecma335
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Shapes of `RuntimeTypeHandle_GetMethodAt`'s receiver that corelib's corpus cannot supply and a
/// C# guest cannot reach under PawPrint today, because an earlier query on the same path
/// (`BaseType` of a definition whose base is an open construction; `GetNumVirtuals` before
/// `GetMethodAt`) stops first. A Roslyn-compiled corpus is read by PawPrint and loaded into the
/// host CLR, whose own `GetMethodAt` and `GetMethodBase` say what the answer is.
[<TestFixture>]
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

    /// A method's declaring type as both sides can spell it: the TypeDef token of its definition
    /// and the TypeDef tokens of its generic arguments (each a non-generic corelib or corpus type
    /// here, so a token names it).
    let private hostDeclaringShape (method : MethodBase) : int * int list =
        let declaring = method.DeclaringType

        let definition =
            if declaring.IsGenericType then
                declaring.GetGenericTypeDefinition ()
            else
                declaring

        definition.MetadataToken, (declaring.GetGenericArguments () |> Array.map _.MetadataToken |> List.ofArray)

    let private tokenOfTypeDefinition (identity : ResolvedTypeIdentity) : int =
        MetadataTokens.GetToken (
            System.Reflection.Metadata.TypeDefinitionHandle.op_Implicit identity.TypeDefinition.Get
            : System.Reflection.Metadata.EntityHandle
        )

    let private pawPrintDeclaringShape (state : IlMachineState) (target : RuntimeTypeHandleTarget) : int * int list =
        match target with
        | RuntimeTypeHandleTarget.Closed handle ->
            let concreteType =
                AllConcreteTypes.lookup handle state.ConcreteTypes
                |> Option.defaultWith (fun () -> failwith $"declaring handle %O{handle} is not registered")

            let arguments =
                concreteType.Generics
                |> Seq.map (fun argument ->
                    match AllConcreteTypes.lookup argument state.ConcreteTypes with
                    | Some argumentType -> tokenOfTypeDefinition argumentType.Identity
                    | None -> failwith $"generic argument %O{argument} is not a nominal type"
                )
                |> List.ofSeq

            tokenOfTypeDefinition concreteType.Identity, arguments
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity -> tokenOfTypeDefinition identity, []
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

    /// `TwoStep<T> : Mid<int, T> : Base<int>`, asked of the definition `TwoStep<>`: `Base`'s slots
    /// are declared by the closed `Base<int>` (its argument is spelled under a context whose other
    /// entry is `T`, which the spelling never mentions), `Mid`'s `Echo` by `Mid<int, T>`, which is
    /// an open construction and refused, and `Mid`'s `Other` likewise.
    [<Test>]
    let ``an ancestor closed through an intermediate that also carries the formal`` () : unit =
        let twoStep = typeInfoNamed "TwoStep`1"
        let receiver = RuntimeTypeHandleTarget.OpenGenericTypeDefinition twoStep.Identity
        let host = hostType "TwoStep`1"
        let count = hostNumVirtuals host
        let mutable own = 0
        let mutable closedAncestors = 0
        let mutable refusedOpen = 0

        for slot in 0 .. count - 1 do
            let expected = hostMethodAt host slot

            let state, answer =
                VirtualSlotLayout.methodAt loggerFactory bct "test" baseState receiver slot

            match answer with
            | VirtualSlotLayout.MethodAtSlot.OutOfRange -> failwith $"slot %i{slot}: PawPrint out of range"
            | VirtualSlotLayout.MethodAtSlot.Method occupant ->
                methodToken occupant |> shouldEqual expected.MetadataToken
                let expectedShape = hostDeclaringShape expected

                // The host's answer says which of three outcomes this slot has: declared by the
                // definition itself (the host spells that `TwoStep<T>`; PawPrint, the definition);
                // by an ancestor whose arguments are all closed; or by an ancestor whose arguments
                // mention `T`, an open construction.
                let hostDeclaringDefinition =
                    if expected.DeclaringType.IsGenericType then
                        expected.DeclaringType.GetGenericTypeDefinition ()
                    else
                        expected.DeclaringType

                let hostMentionsFormal =
                    expected.DeclaringType.IsGenericType
                    && expected.DeclaringType.GetGenericArguments ()
                       |> Array.exists _.IsGenericParameter

                if hostDeclaringDefinition = host then
                    let _, declaring =
                        VirtualSlotLayout.declaringTypeAt loggerFactory bct "test" state receiver slot occupant

                    declaring |> shouldEqual receiver
                    own <- own + 1
                elif hostMentionsFormal then
                    let message =
                        try
                            VirtualSlotLayout.declaringTypeAt loggerFactory bct "test" state receiver slot occupant
                            |> ignore

                            failwith "expected refusal"
                        with e ->
                            e.Message

                    message |> shouldContainText "open construction"
                    refusedOpen <- refusedOpen + 1
                else
                    let state, declaring =
                        VirtualSlotLayout.declaringTypeAt loggerFactory bct "test" state receiver slot occupant

                    pawPrintDeclaringShape state declaring |> shouldEqual expectedShape

                    if expected.DeclaringType.IsGenericType then
                        closedAncestors <- closedAncestors + 1

        // All three outcomes must have been exercised, or the law above is vacuous for one of them.
        own |> shouldBeGreaterThan 0
        closedAncestors |> shouldBeGreaterThan 0
        refusedOpen |> shouldBeGreaterThan 0

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
            DumpedAssembly.typeInfoToTypeDefn' bct state._LoadedAssemblies bct.String
            |> IlMachineState.concretizeType
                loggerFactory
                bct
                state
                corelib.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty

        let state, closed =
            DumpedAssembly.typeInfoToTypeDefn' bct state._LoadedAssemblies twoStep
            |> IlMachineState.concretizeType
                loggerFactory
                bct
                state
                corpusAssembly.DefinitionFullName
                (ImmutableArray.Create stringHandle)
                ImmutableArray.Empty

        let host = (hostType "TwoStep`1").MakeGenericType [| typeof<string> |]
        checkEverySlot "TwoStep<string>" host state (RuntimeTypeHandleTarget.Closed closed)
