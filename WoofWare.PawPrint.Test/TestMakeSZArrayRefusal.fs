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

public struct GenericStruct<T> { public T Field; }

public ref struct RefStruct { public int Field; }

public ref struct GenericRefStruct<T> { public int Field; }

public class AllowsRef<T> where T : allows ref struct { }

[System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Sequential, Size = 65535)]
public struct AtLimit { }

[System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Sequential, Size = 65536)]
public struct OverLimit { }

[System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Explicit, Size = 70000)]
public struct WayOverLimit { }

public struct NaturallyOversized { public AtLimit Pad; public int Tail; }

public class Outer
{
    [System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Sequential, Size = 65536)]
    public struct NestedOver { }

    [System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Sequential, Size = 65536)]
    public struct NestedGenericOver<T> { public int Field; }
}

namespace Namespaced
{
    [System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Sequential, Size = 65536)]
    public struct TopLevelOver { }

    public class NsOuter
    {
        [System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Sequential, Size = 65536)]
        public struct NestedOver { }
    }
}

public class OversizedHolder { public OverLimit Field; }

public unsafe class FnPtrHolder<T>
{
    public static void Open(delegate*<T, void> p) { }

    public static void Closed(delegate*<int, string> p) { }
}
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

    /// Corpus types are looked up by simple name, except that the two `NestedOver` structs share
    /// one -- being nested in different types is the whole point of that pair -- so the nested one
    /// under `NsOuter` is spelled `NsOuter+NestedOver` and matched through its declaring type.
    let private guestType (name : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        let matches (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>) : bool =
            let declaringName () = guest.TypeDefs.[ty.DeclaringType].Name

            match name.Split '+' with
            | [| simple |] -> ty.Name = simple && (not ty.IsNested || declaringName () <> "NsOuter")
            | [| outer ; simple |] -> ty.Name = simple && ty.IsNested && declaringName () = outer
            | _ -> failwith $"corpus type name %s{name} names more than one level of nesting"

        guest.TypeDefs.Values
        |> Seq.filter matches
        |> List.ofSeq
        |> function
            | [ ty ] -> ty
            | [] -> failwith $"corpus type %s{name} not found in the PawPrint-read image"
            | several ->
                let rendered = several |> List.map (fun ty -> ty.Name) |> String.concat ", "
                failwith $"corpus type %s{name} is ambiguous in the PawPrint-read image: %s{rendered}"

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

    /// The `delegate*<T, void>` in `FnPtrHolder<T>.Open`'s signature, as PawPrint spells it: a
    /// function pointer one of whose types is a type variable, so `functionPointer` keeps it open
    /// rather than collapsing it to a closed handle.
    let private openFunctionPointer (parameter : RuntimeTypeHandleTarget) : RuntimeTypeHandleTarget =
        RuntimeTypeHandleTarget.functionPointer
            {
                Header =
                    ComparableSignatureHeader.Make (
                        Reflection.Metadata.SignatureHeader (
                            Reflection.Metadata.SignatureKind.Method,
                            Reflection.Metadata.SignatureCallingConvention.Default,
                            Reflection.Metadata.SignatureAttributes.None
                        )
                    )
                ParameterTypes = [ parameter ]
                GenericParameterCount = 0
                RequiredParameterCount = 1
                ReturnType = MethodReturnType.Void
            }

    /// The same function pointer as the host sees it: the declared parameter type of
    /// `FnPtrHolder<T>.<name>`.
    let private hostFunctionPointer (name : string) : Type =
        (hostType "FnPtrHolder`1").GetMethod(name).GetParameters().[0].ParameterType

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

        let genericStruct = guestType "GenericStruct`1"
        let hostGenericStruct = hostType "GenericStruct`1"
        let fnPtrHolder = guestType "FnPtrHolder`1"

        let fnPtrVariable =
            RuntimeTypeHandleTarget.GenericParameter (fnPtrHolder.Identity, 0)

        let hostOpenFunctionPointer = hostFunctionPointer "Open"

        let closedFunctionPointerSignature : TypeMethodSignature<TypeDefn> =
            {
                Header =
                    ComparableSignatureHeader.Make (
                        Reflection.Metadata.SignatureHeader (
                            Reflection.Metadata.SignatureKind.Method,
                            Reflection.Metadata.SignatureCallingConvention.Default,
                            Reflection.Metadata.SignatureAttributes.None
                        )
                    )
                ParameterTypes = [ int32Defn ]
                GenericParameterCount = 0
                RequiredParameterCount = 1
                ReturnType = MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.String)
            }

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
            // A function pointer over a type variable is a TypeDesc that is not `Closed`, so it is
            // the one element whose *rendering* has to recurse through open targets: the byref row
            // below is what asks for a name at all.
            "System.Void(T)", openTarget (openFunctionPointer fnPtrVariable), hostOpenFunctionPointer
            "System.Void(T)&",
            openTarget (RuntimeTypeHandleTarget.composite CompositeShape.Byref (openFunctionPointer fnPtrVariable)),
            hostOpenFunctionPointer.MakeByRefType ()
            "System.String(System.Int32)&",
            closed (TypeDefn.Byref (TypeDefn.FunctionPointer closedFunctionPointerSignature)),
            (hostFunctionPointer "Closed").MakeByRefType ()
            // The size limit, from both sides of it: `ComponentSize` is a UInt16, so 65535 is the
            // largest element an array may have. Only value types can reach it -- a class element
            // is one pointer -- so the class row below is the control that keeps the rule from
            // being "anything whose declared Size is large".
            "AtLimit (65535 bytes)", closed (definitionDefn (guestType "AtLimit")), hostType "AtLimit"
            "OverLimit (65536 bytes)", closed (definitionDefn (guestType "OverLimit")), hostType "OverLimit"
            "WayOverLimit (70000 bytes)", closed (definitionDefn (guestType "WayOverLimit")), hostType "WayOverLimit"
            // Oversized by its fields rather than by a declared Size.
            "NaturallyOversized",
            closed (definitionDefn (guestType "NaturallyOversized")),
            hostType "NaturallyOversized"
            // A *generic* value type is sized after substitution, so the same argument makes
            // Generic<OverLimit> too large while Generic<AtLimit> is not.
            "GenericStruct<OverLimit>",
            closed (closedGeneric genericStruct (definitionDefn (guestType "OverLimit"))),
            hostGenericStruct.MakeGenericType (hostType "OverLimit")
            "GenericStruct<AtLimit>",
            closed (closedGeneric genericStruct (definitionDefn (guestType "AtLimit"))),
            hostGenericStruct.MakeGenericType (hostType "AtLimit")
            // The oversized refusal names the element through `TypeHandle::GetName`, which reads
            // the TypeDef row's own namespace and name: a nested row has neither an outer name nor
            // a namespace, so `Outer.NestedOver` is bare `NestedOver` where the three type-key
            // refusals would say `Outer+NestedOver`. These four rows are what pin that apart.
            "Outer.NestedOver (nested)", closed (definitionDefn (guestType "NestedOver")), hostType "Outer+NestedOver"
            "Outer.NestedGenericOver<int>",
            closed (closedGeneric (guestType "NestedGenericOver`1") int32Defn),
            (hostType "Outer+NestedGenericOver`1").MakeGenericType typeof<int>
            "Namespaced.TopLevelOver",
            closed (definitionDefn (guestType "TopLevelOver")),
            hostType "Namespaced.TopLevelOver"
            "Namespaced.NsOuter.NestedOver",
            closed (definitionDefn (guestType "NsOuter+NestedOver")),
            hostType "Namespaced.NsOuter+NestedOver"
            "OversizedHolder (a class)",
            closed (definitionDefn (guestType "OversizedHolder")),
            hostType "OversizedHolder"
            "OverLimit*",
            closed (TypeDefn.Pointer (definitionDefn (guestType "OverLimit"))),
            (hostType "OverLimit").MakePointerType ()
            "OverLimit&",
            closed (TypeDefn.Byref (definitionDefn (guestType "OverLimit"))),
            (hostType "OverLimit").MakeByRefType ()
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
                elif e.Message.EndsWith "cannot be created because base value type is too large." then
                    SzArrayElementRefusal.ValueClassTooLarge
                else
                    failwith $"host refused an szarray over %O{element} with an unrecognised message: %s{e.Message}"

            Some (refusal, e.TypeName)

    /// PawPrint's verdict, in the same shape.
    let private pawPrintVerdict
        (state : IlMachineState)
        (element : RuntimeTypeHandleTarget)
        : IlMachineState * (SzArrayElementRefusal * string) option
        =
        let state, refusal =
            NativeRuntimeTypeHelpers.szArrayElementRefusal "test" bct state element

        let rendered =
            refusal
            |> Option.map (fun refusal ->
                refusal, NativeRuntimeTypeHelpers.szArrayRefusalTypeName "test" state element refusal
            )

        state, rendered

    [<Test>]
    let ``every element's szarray refusal and type string agree with the host CLR`` () : unit =
        let mutable state = initialState
        let mutable disagreements = []
        let mutable verdictsSeen = Set.empty

        for name, target, hostElement in elementPool do
            let expected = hostVerdict hostElement
            let state', element = target state
            let state', actual = pawPrintVerdict state' element
            state <- state'

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
                    Some SzArrayElementRefusal.ValueClassTooLarge
                ]

        if verdictsSeen <> allVerdicts then
            failwith $"the corpus does not reach every verdict: saw only %A{Set.toList verdictsSeen}"

    /// The one element the classifier declines to answer for. It cannot be a row in the agreement
    /// corpus above: the host has a verdict for these (`BigWrapper<>[]` throws, `SmallWrapper<>[]`
    /// loads) and PawPrint has none, which is the whole point — it cannot size an open generic, so
    /// answering either way would be a guess. What must hold is that it *refuses* rather than
    /// silently reporting the array legal, which is what a guest would otherwise be handed.
    [<Test>]
    let ``an open value-type element is refused rather than answered`` () : unit =
        let openStruct =
            RuntimeTypeHandleTarget.OpenGenericTypeDefinition (guestType "GenericStruct`1").Identity

        let exn =
            Assert.Throws (fun () ->
                NativeRuntimeTypeHelpers.szArrayElementRefusal "test" bct initialState openStruct
                |> ignore
            )

        Assert.That (exn.Message, Does.Contain "cannot size an open generic")

        // The control: an open *reference* type needs no size, and is answered normally.
        let openClass =
            RuntimeTypeHandleTarget.OpenGenericTypeDefinition (guestType "Generic`1").Identity

        let _, refusal =
            NativeRuntimeTypeHelpers.szArrayElementRefusal "test" bct initialState openClass

        Assert.That (refusal, Is.Null)
