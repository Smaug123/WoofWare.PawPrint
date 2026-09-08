namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// `NativeRuntimeTypeHelpers.szArrayElementRefusal` decides whether CoreCLR's type loader would
/// construct an szarray over an element type, and `szArrayRefusalTypeName` renders the type string
/// its `TypeLoadException` carries when it would not. `sourcesPure/TypeMakeArrayType.cs` pins the
/// shapes a guest can spell; here we load one image into both PawPrint and the host CLR and assert
/// the two agree, on the verdict and on the type string, for every element in a corpus that also
/// reaches the targets no C# guest can name directly (a variable of an `allows ref struct`
/// parameter, a pointer over a ref struct, an open construction over a ref struct).
///
/// The host is a genuine outside oracle: the expected verdict is the real `Type.MakeArrayType`,
/// so a classification that is self-consistent but wrong about which targets are TypeDescs cannot
/// pass.
[<TestFixture>]
module TestMakeSZArrayRefusal =

    let private corpusSource : string =
        """
public class Plain { }

public class Generic<T> { }

public struct PlainStruct { public int Field; }

public ref struct RefStruct { public int Field; }

public ref struct GenericRefStruct<T> { public int Field; }

public class AllowsRef<T> where T : allows ref struct { }
"""

    let private image : byte[] =
        Roslyn.compileAssembly "SzArrayRefusalCorpus" OutputKind.DynamicallyLinkedLibrary [] [ corpusSource ]

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

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

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

    let private corelibType (ns : string) (name : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        corelib.TryGetTopLevelTypeDef ns name
        |> Option.defaultWith (fun () -> failwith $"%s{ns}.%s{name} not found in corelib")

    let private hostCorelibType (name : string) : Type =
        match typeof<obj>.Assembly.GetType name with
        | null -> failwith $"%s{name} not found in the host corelib"
        | ty -> ty

    let private definitionDefn (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>) : TypeDefn =
        TypeDefn.FromDefinition (
            typeInfo.Identity,
            DumpedAssembly.signatureTypeKind bct initialState._LoadedAssemblies typeInfo
        )

    let private closedGeneric (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>) (arg : TypeDefn) : TypeDefn =
        TypeDefn.GenericInstantiation (definitionDefn typeInfo, ImmutableArray.Create arg)

    /// A closed element: concretised into the state, so the target is `Closed`.
    let private closed (defn : TypeDefn) (state : IlMachineState) : IlMachineState * RuntimeTypeHandleTarget =
        let state, handle =
            IlMachineState.concretizeType
                loggerFactory
                bct
                state
                guest.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                defn

        state, RuntimeTypeHandleTarget.Closed handle

    /// An element that is not closed: the target is given directly.
    let private openTarget
        (target : RuntimeTypeHandleTarget)
        (state : IlMachineState)
        : IlMachineState * RuntimeTypeHandleTarget
        =
        state, target

    let private int32Defn : TypeDefn = TypeDefn.PrimitiveType PrimitiveType.Int32

    /// The element pool, as (display name, PawPrint target, host Type). The two sides are built by
    /// different routes but must denote the same types; the display name is only for assertion
    /// messages.
    let private elementPool : (string * (IlMachineState -> IlMachineState * RuntimeTypeHandleTarget) * Type) list =
        let generic = guestType "Generic`1"
        let genericRefStruct = guestType "GenericRefStruct`1"
        let allowsRef = guestType "AllowsRef`1"
        let span = corelibType "System" "Span`1"
        let hostGeneric = hostType "Generic`1"
        let hostGenericRefStruct = hostType "GenericRefStruct`1"
        let hostAllowsRef = hostType "AllowsRef`1"
        let hostSpan = hostCorelibType "System.Span`1"
        let hostVariable = hostGeneric.GetGenericArguments().[0]
        let hostAllowsRefVariable = hostAllowsRef.GetGenericArguments().[0]
        let variable = RuntimeTypeHandleTarget.GenericParameter (generic.Identity, 0)

        let allowsRefVariable =
            RuntimeTypeHandleTarget.GenericParameter (allowsRef.Identity, 0)

        let spanOfInt = closedGeneric span int32Defn
        let hostSpanOfInt = hostSpan.MakeGenericType typeof<int>
        let refStruct = definitionDefn (guestType "RefStruct")
        let hostRefStruct = hostType "RefStruct"

        [
            "System.Int32", closed int32Defn, typeof<int>
            "System.String", closed (TypeDefn.PrimitiveType PrimitiveType.String), typeof<string>
            "System.Object", closed (TypeDefn.PrimitiveType PrimitiveType.Object), typeof<obj>
            "System.Int32[]", closed (TypeDefn.OneDimensionalArrayLowerBoundZero int32Defn), typeof<int[]>
            "System.Int32[,]", closed (TypeDefn.Array (int32Defn, 2)), typeof<int[,]>
            "System.Int32*", closed (TypeDefn.Pointer int32Defn), typeof<int>.MakePointerType ()
            "System.Void*", closed (TypeDefn.Pointer TypeDefn.Void), typeof<Void>.MakePointerType ()
            "System.Int32&", closed (TypeDefn.Byref int32Defn), typeof<int>.MakeByRefType ()
            "System.Int32[]&",
            closed (TypeDefn.Byref (TypeDefn.OneDimensionalArrayLowerBoundZero int32Defn)),
            typeof<int[]>.MakeByRefType ()
            "System.Void", closed TypeDefn.Void, typeof<Void>
            "System.Void&", closed (TypeDefn.Byref TypeDefn.Void), typeof<Void>.MakeByRefType ()
            // Reflected: F# refuses `typeof<TypedReference>` for the same reason as `Span` below.
            "System.TypedReference",
            closed (TypeDefn.PrimitiveType PrimitiveType.TypedReference),
            hostCorelibType "System.TypedReference"
            // A *corelib* ref struct: its IsByRefLikeAttribute constructor is a MethodDef in the
            // assembly under inspection, where the guest ref structs' is a MemberReference into
            // corelib, so both encodings of the attribute are covered.
            // (Reflected rather than written as `typeof<Span<int>>`, which F# refuses: a byref-like
            // type may not instantiate a generic, and `typeof<_>` is one.)
            "System.Span<System.Int32>", closed spanOfInt, hostSpanOfInt
            "System.Span<System.Int32>&", closed (TypeDefn.Byref spanOfInt), hostSpanOfInt.MakeByRefType ()
            "System.Span<System.Int32>*", closed (TypeDefn.Pointer spanOfInt), hostSpanOfInt.MakePointerType ()
            "System.Span<>", openTarget (RuntimeTypeHandleTarget.OpenGenericTypeDefinition span.Identity), hostSpan
            "Plain", closed (definitionDefn (guestType "Plain")), hostType "Plain"
            "Plain[]",
            closed (TypeDefn.OneDimensionalArrayLowerBoundZero (definitionDefn (guestType "Plain"))),
            (hostType "Plain").MakeArrayType ()
            "PlainStruct", closed (definitionDefn (guestType "PlainStruct")), hostType "PlainStruct"
            "RefStruct", closed refStruct, hostRefStruct
            "RefStruct&", closed (TypeDefn.Byref refStruct), hostRefStruct.MakeByRefType ()
            "RefStruct*", closed (TypeDefn.Pointer refStruct), hostRefStruct.MakePointerType ()
            "Generic<System.Int32>", closed (closedGeneric generic int32Defn), hostGeneric.MakeGenericType typeof<int>
            "GenericRefStruct<System.Int32>",
            closed (closedGeneric genericRefStruct int32Defn),
            hostGenericRefStruct.MakeGenericType typeof<int>
            "Generic<>", openTarget (RuntimeTypeHandleTarget.OpenGenericTypeDefinition generic.Identity), hostGeneric
            "GenericRefStruct<>",
            openTarget (RuntimeTypeHandleTarget.OpenGenericTypeDefinition genericRefStruct.Identity),
            hostGenericRefStruct
            "T of Generic<>", openTarget variable, hostVariable
            "T of AllowsRef<>", openTarget allowsRefVariable, hostAllowsRefVariable
            "T&",
            openTarget (RuntimeTypeHandleTarget.composite CompositeShape.Byref variable),
            hostVariable.MakeByRefType ()
            "T*",
            openTarget (RuntimeTypeHandleTarget.composite CompositeShape.Pointer variable),
            hostVariable.MakePointerType ()
            "T[]",
            openTarget (RuntimeTypeHandleTarget.composite CompositeShape.OneDimArrayZero variable),
            hostVariable.MakeArrayType ()
            "GenericRefStruct<T of Generic<>>",
            openTarget (RuntimeTypeHandleTarget.openConstructed genericRefStruct.Identity [ variable ]),
            hostGenericRefStruct.MakeGenericType hostVariable
            "Generic<T of AllowsRef<>>",
            openTarget (RuntimeTypeHandleTarget.openConstructed generic.Identity [ allowsRefVariable ]),
            hostGeneric.MakeGenericType hostAllowsRefVariable
        ]

    /// The host's verdict: `None` if the array type loads, else the refusal and the type string
    /// its `TypeLoadException` carries.
    let private hostVerdict (element : Type) : (SzArrayElementRefusal * string) option =
        try
            element.MakeArrayType () |> ignore
            None
        with :? TypeLoadException as e ->
            let refusal =
                if e.Message.EndsWith "because the element type is ByRef." then
                    SzArrayElementRefusal.ByRef
                elif e.Message.EndsWith "because the element type is ByRef-like." then
                    SzArrayElementRefusal.ByRefLike
                elif e.Message.EndsWith "because the element type is System.Void." then
                    SzArrayElementRefusal.Void
                else
                    failwith $"host refused an szarray over %O{element} with an unrecognised message: %s{e.Message}"

            Some (refusal, e.TypeName)

    /// PawPrint's verdict, in the same shape.
    let private pawPrintVerdict
        (state : IlMachineState)
        (element : RuntimeTypeHandleTarget)
        : (SzArrayElementRefusal * string) option
        =
        NativeRuntimeTypeHelpers.szArrayElementRefusal bct state element
        |> Option.map (fun refusal ->
            refusal, NativeRuntimeTypeHelpers.szArrayRefusalTypeName "test" state element refusal
        )

    [<Test>]
    let ``every element's szarray refusal and type string agree with the host CLR`` () : unit =
        let mutable state = initialState
        let mutable disagreements = []
        let mutable verdictsSeen = Set.empty

        for name, target, hostElement in elementPool do
            let expected = hostVerdict hostElement
            let state', element = target state
            state <- state'
            let actual = pawPrintVerdict state element

            verdictsSeen <- verdictsSeen.Add (expected |> Option.map fst)

            if expected <> actual then
                disagreements <- $"%s{name}: host %O{expected}, PawPrint %O{actual}" :: disagreements

        if not (List.isEmpty disagreements) then
            failwith (
                $"%d{List.length disagreements} elements disagree with the host CLR:\n"
                + String.concat "\n" (List.rev disagreements)
            )

        // Guard against the corpus never exercising one of the four verdicts, which would leave
        // that arm of the classification untested.
        let allVerdicts =
            Set.ofList
                [
                    None
                    Some SzArrayElementRefusal.ByRef
                    Some SzArrayElementRefusal.ByRefLike
                    Some SzArrayElementRefusal.Void
                ]

        if verdictsSeen <> allVerdicts then
            failwith $"the corpus does not reach every verdict: saw only %A{Set.toList verdictsSeen}"
