namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Text.RegularExpressions
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// `NativeRuntimeTypeHelpers.validateConstraintsOn` decides whether a generic instantiation is
/// legal (CoreCLR `TypeVarTypeDesc::SatisfiesConstraints`, typedesc.cpp:1491). The end-to-end cases
/// in `sourcesPure/MakeGenericType*.cs` pin the handful of shapes a hand-written guest program
/// covers; here we compile one corpus of constraint declarations and candidate arguments, load the
/// *same image* into both PawPrint and the host CLR, and assert the two agree on every pair.
///
/// The host is a genuine outside oracle: nothing about the expected verdict is read out of the
/// structure under test, so a self-consistent-but-wrong assignability walk cannot pass.
///
/// Two exclusions keep the two sides comparable, and the corpus must honour them:
///
///   * The validator is the *QCall-level* check. `RuntimeType.MakeGenericType` also runs a managed
///     screen first (`SanityCheckGenericArguments` -> `ThrowIfTypeNeverValidGenericArgument`),
///     which rejects pointers, byrefs and `void` outright. Those are not in the argument pool: the
///     host would throw where the validator, correctly, has no opinion. (Byref-like types are *not*
///     screened there — they are refused by the type loader, i.e. by the check under test — so ref
///     structs are in the pool.)
///   * CoreCLR refines the general check for an *abstract* argument against an interface constraint
///     that declares virtual static methods (typedesc.cpp:1686), which PawPrint does not implement
///     for want of `ResolveVirtualStaticMethod`. No interface in the corpus declares a static
///     abstract member, so the refinement never fires on either side.
[<TestFixture>]
module TestGenericConstraintSatisfaction =

    /// Every holder is single-parameter except the `H2_` pair, which exists so the reported
    /// parameter *index* has something to be wrong about.
    let private corpusSource : string =
        """
using System;
using System.Collections.Generic;

public interface IMarker { }

public interface IOther { }

public interface IOut<out T> { }

public class BaseType { }

public class DerivedType : BaseType, IMarker { }

public class Unrelated { }

public abstract class AbstractDerived : BaseType, IMarker { }

public sealed class SealedMarker : IMarker { }

public class NoPublicCtor { private NoPublicCtor() { } }

public class OutOfDerived : IOut<DerivedType> { }

public struct MarkedStruct : IMarker { public int X; }

public struct PlainStruct { public int X; }

public enum MyEnum { A }

public ref struct MarkedRefStruct : IMarker { public int X; }

public ref struct PlainRefStruct { public int X; }

public ref struct GenericRefStruct<T> { public int X; }

public class H_None<T> { }
public class H_Struct<T> where T : struct { }
public class H_Class<T> where T : class { }
public class H_New<T> where T : new() { }
public class H_Marker<T> where T : IMarker { }
public class H_Other<T> where T : IOther { }
public class H_Base<T> where T : BaseType { }
public class H_BaseAndMarker<T> where T : BaseType, IMarker { }
public class H_Comparable<T> where T : IComparable { }
public class H_ComparableOfSelf<T> where T : IComparable<T> { }
public class H_EnumerableOfInt<T> where T : IEnumerable<int> { }
public class H_EnumerableOfObject<T> where T : IEnumerable<object> { }
public class H_ListOfUInt<T> where T : IList<uint> { }
public class H_OutOfBase<T> where T : IOut<BaseType> { }
public class H_Enum<T> where T : Enum { }
public class H_AllowsRef<T> where T : allows ref struct { }
public class H_AllowsRefMarker<T> where T : IMarker, allows ref struct { }
public class H_StructMarker<T> where T : struct, IMarker { }
public class H_ClassMarker<T> where T : class, IMarker { }
public class H_NewBase<T> where T : BaseType, new() { }

public class H2_MarkerThenBase<A, B> where A : IMarker where B : BaseType { }
public class H2_BaseThenMarker<A, B> where A : BaseType where B : IMarker { }
"""

    let private holderNames : string list =
        [
            "H_None`1"
            "H_Struct`1"
            "H_Class`1"
            "H_New`1"
            "H_Marker`1"
            "H_Other`1"
            "H_Base`1"
            "H_BaseAndMarker`1"
            "H_Comparable`1"
            "H_ComparableOfSelf`1"
            "H_EnumerableOfInt`1"
            "H_EnumerableOfObject`1"
            "H_ListOfUInt`1"
            "H_OutOfBase`1"
            "H_Enum`1"
            "H_AllowsRef`1"
            "H_AllowsRefMarker`1"
            "H_StructMarker`1"
            "H_ClassMarker`1"
            "H_NewBase`1"
        ]

    let private twoParamHolderNames : string list =
        [ "H2_MarkerThenBase`2" ; "H2_BaseThenMarker`2" ]

    /// Guest-declared argument types, by metadata name.
    let private guestArgumentNames : string list =
        [
            "BaseType"
            "DerivedType"
            "Unrelated"
            "AbstractDerived"
            "SealedMarker"
            "NoPublicCtor"
            "MarkedStruct"
            "PlainStruct"
            "MyEnum"
            "MarkedRefStruct"
            "PlainRefStruct"
            "OutOfDerived"
        ]

    let private image : byte[] =
        Roslyn.compileAssembly "GenericConstraintOracleCorpus" OutputKind.DynamicallyLinkedLibrary [] [ corpusSource ]

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

    /// The argument pool, as (display name, PawPrint TypeDefn, host Type). The guest entries and
    /// the corelib entries are built by different routes but must denote the same types; the
    /// display name is only for assertion messages.
    let private argumentPool : (string * TypeDefn * Type) list =
        let ofGuest (name : string) =
            let typeInfo = guestType name

            let defn =
                TypeDefn.FromDefinition (
                    typeInfo.Identity,
                    DumpedAssembly.signatureTypeKind bct initialState._LoadedAssemblies typeInfo
                )

            name, defn, hostType name

        // Deliberately *not* `typeInfoToTypeDefn'`, which would hand back the open instantiation
        // `Nullable<!0>` rather than the bare definition these need as their generic head.
        let closedCorelibGeneric (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>) (arg : TypeDefn) =
            TypeDefn.GenericInstantiation (
                TypeDefn.FromDefinition (typeInfo.Identity, Reflection.Metadata.SignatureTypeKind.ValueType),
                ImmutableArray.Create arg
            )

        let corelibType (ns : string) (name : string) =
            corelib.TryGetTopLevelTypeDef ns name
            |> Option.defaultWith (fun () -> failwith $"%s{ns}.%s{name} not found in corelib")

        let guestGenericRefStruct =
            let typeInfo = guestType "GenericRefStruct`1"

            TypeDefn.GenericInstantiation (
                TypeDefn.FromDefinition (typeInfo.Identity, Reflection.Metadata.SignatureTypeKind.ValueType),
                ImmutableArray.Create (TypeDefn.PrimitiveType PrimitiveType.Int32)
            )

        (guestArgumentNames |> List.map ofGuest)
        @ [
            "System.Int32", TypeDefn.PrimitiveType PrimitiveType.Int32, typeof<int>
            "System.String", TypeDefn.PrimitiveType PrimitiveType.String, typeof<string>
            "System.Object", TypeDefn.PrimitiveType PrimitiveType.Object, typeof<obj>
            "System.Nullable<System.Int32>",
            closedCorelibGeneric bct.Nullable (TypeDefn.PrimitiveType PrimitiveType.Int32),
            typeof<Nullable<int>>
            // A *corelib* ref struct: its IsByRefLikeAttribute constructor is a MethodDef in the
            // assembly under inspection, where the guest ref structs' is a MemberReference into
            // corelib. Without this the two encodings the classification has to understand are not
            // both covered.
            // (Reflected rather than written as `typeof<Span<int>>`, which F# refuses: a byref-like
            // type may not instantiate a generic, and `typeof<_>` is one.)
            "System.Span<System.Int32>",
            closedCorelibGeneric (corelibType "System" "Span`1") (TypeDefn.PrimitiveType PrimitiveType.Int32),
            typeof<obj>.Assembly.GetType("System.Span`1").MakeGenericType typeof<int>
            "GenericRefStruct<System.Int32>",
            guestGenericRefStruct,
            (hostType "GenericRefStruct`1").MakeGenericType typeof<int>
            "System.Int32[]",
            TypeDefn.OneDimensionalArrayLowerBoundZero (TypeDefn.PrimitiveType PrimitiveType.Int32),
            typeof<int[]>
            "System.String[]",
            TypeDefn.OneDimensionalArrayLowerBoundZero (TypeDefn.PrimitiveType PrimitiveType.String),
            typeof<string[]>
        ]

    /// `GenericArguments[N]` opens both the CLR's message and PawPrint's, which is the only part of
    /// the two formats that agrees. (PawPrint's diagnostic deliberately says *which* constraint was
    /// violated where the CLR names only the parameter, so nothing past the index is comparable.)
    let private violatedIndex (message : string) : int option =
        let m = Regex.Match (message, @"^GenericArguments\[(\d+)\]")

        if m.Success then Some (int m.Groups.[1].Value) else None

    /// The host's verdict: `None` if the instantiation binds, `Some index` naming the first
    /// parameter it rejected.
    let private hostVerdict (holder : string) (args : Type list) : int option =
        try
            (hostType holder).MakeGenericType (List.toArray args) |> ignore
            None
        with :? ArgumentException as e ->
            match violatedIndex e.Message with
            | Some i -> Some i
            | None ->
                let rendered = args |> Seq.map string<Type> |> String.concat "; "

                failwith $"host rejected %s{holder}<%s{rendered}> with an unparseable message: %s{e.Message}"

    /// PawPrint's verdict, in the same shape.
    let private pawPrintVerdict
        (state : IlMachineState)
        (holder : string)
        (args : TypeDefn list)
        : IlMachineState * int option
        =
        let state, argHandles =
            ((state, []), args)
            ||> List.fold (fun (state, acc) arg ->
                let state, handle =
                    IlMachineState.concretizeType
                        loggerFactory
                        bct
                        state
                        guest.Name
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        arg

                state, handle :: acc
            )

        let argHandles = List.rev argHandles

        let state, violation =
            NativeRuntimeTypeHelpers.validateConstraints loggerFactory bct state (guestType holder) argHandles

        match violation with
        | None -> state, None
        | Some message ->
            match violatedIndex message with
            | Some i -> state, Some i
            | None -> failwith $"PawPrint's violation message for %s{holder} is unparseable: %s{message}"

    [<Test>]
    let ``every single-parameter (constraint, argument) pair agrees with the host CLR`` () : unit =
        let mutable state = initialState
        let mutable disagreements = []
        let mutable rejectedByHost = 0

        for holder in holderNames do
            for name, defn, hostArg in argumentPool do
                let expected = hostVerdict holder [ hostArg ]
                let state', actual = pawPrintVerdict state holder [ defn ]
                state <- state'

                if expected.IsSome then
                    rejectedByHost <- rejectedByHost + 1

                if expected <> actual then
                    disagreements <- $"%s{holder}<%s{name}>: host %O{expected}, PawPrint %O{actual}" :: disagreements

        if not (List.isEmpty disagreements) then
            failwith (
                $"%d{List.length disagreements} (constraint, argument) pairs disagree with the host CLR:\n"
                + String.concat "\n" (List.rev disagreements)
            )

        // Guard against the corpus degenerating into all-accept or all-reject, which would make the
        // agreement above vacuous.
        let total = List.length holderNames * List.length argumentPool
        Assert.That (rejectedByHost, Is.GreaterThan 0, "corpus rejects nothing")
        Assert.That (rejectedByHost, Is.LessThan total, "corpus accepts nothing")

    [<Test>]
    let ``for two-parameter holders, the reported parameter index agrees with the host CLR`` () : unit =
        let mutable state = initialState
        let mutable disagreements = []
        let mutable sawSecondParameterViolation = false

        for holder in twoParamHolderNames do
            for nameA, defnA, hostA in argumentPool do
                for nameB, defnB, hostB in argumentPool do
                    let expected = hostVerdict holder [ hostA ; hostB ]
                    let state', actual = pawPrintVerdict state holder [ defnA ; defnB ]
                    state <- state'

                    if expected = Some 1 then
                        sawSecondParameterViolation <- true

                    if expected <> actual then
                        disagreements <-
                            $"%s{holder}<%s{nameA}, %s{nameB}>: host %O{expected}, PawPrint %O{actual}"
                            :: disagreements

        if not (List.isEmpty disagreements) then
            failwith (
                $"%d{List.length disagreements} (constraint, argument pair) triples disagree with the host CLR:\n"
                + String.concat "\n" (List.rev disagreements)
            )

        // Without this the index assertion could pass while only ever reporting index 0.
        Assert.That (
            sawSecondParameterViolation,
            Is.True,
            "no corpus pair violates only the *second* parameter, so the index is untested"
        )
