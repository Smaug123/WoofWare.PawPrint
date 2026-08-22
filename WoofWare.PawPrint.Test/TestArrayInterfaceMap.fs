namespace WoofWare.PawPrint.Test

open System.Collections.Generic
open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `RuntimeTypeHandle_GetInterfaces` gives an array handle no interfaces of its own and lets
/// the ordinary base-type walk collect `System.Array`'s closure instead — mirroring CoreCLR,
/// which copies the parent's interface map row for row into every array MethodTable
/// (`src/coreclr/vm/array.cpp:410-424`).
///
/// That fall-through is only correct because an array's base type really does resolve to
/// `System.Array`, for every element type and every rank. These tests pin that invariant: if
/// `resolveBaseConcreteType` ever stopped answering `System.Array` for an array handle, the
/// QCall would silently report the *wrong* interface set rather than fail, and the only
/// symptom would be a differential test's exit code.
[<TestFixture>]
module TestArrayInterfaceMap =

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

    let private loggerFactory = snd (LoggerFactory.makeTest ())

    let private state () : IlMachineState =
        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    /// A spread of corelib element types: reference and value, primitive and struct, enum,
    /// and a generic instantiation. Nothing about an array's interface map should vary with
    /// any of this.
    let private elementTypes : (string * string) list =
        [
            "System", "Int32"
            "System", "UInt64"
            "System", "Byte"
            "System", "Double"
            "System", "Boolean"
            "System", "Char"
            "System", "DayOfWeek"
            "System", "Guid"
            "System", "Object"
            "System", "String"
            "System", "Exception"
            "System", "ArgumentException"
            "System", "DateTime"
        ]

    let private concretize
        (state : IlMachineState)
        (``namespace`` : string)
        (name : string)
        : IlMachineState * ConcreteTypeHandle
        =
        let typeInfo =
            match corelib.TryGetTopLevelTypeDef ``namespace`` name with
            | None -> failwith $"%s{``namespace``}.%s{name} not found in corelib"
            | Some typeInfo -> typeInfo

        DumpedAssembly.typeInfoToTypeDefn' bct state._LoadedAssemblies typeInfo
        |> IlMachineState.concretizeType
            loggerFactory
            bct
            state
            corelib.DefinitionFullName
            ImmutableArray.Empty
            ImmutableArray.Empty

    let private arrayBaseType (state : IlMachineState) (handle : ConcreteTypeHandle) : ResolvedTypeIdentity =
        let state, baseHandle =
            IlMachineState.resolveBaseConcreteType loggerFactory bct state handle

        let baseHandle =
            match baseHandle with
            | Some b -> b
            | None -> failwith $"array handle %O{handle} unexpectedly had no base type"

        match IlMachineState.tryGetConcreteTypeInfo state baseHandle with
        | Some (ct, _) -> ct.Identity
        | None -> failwith $"base type handle %O{baseHandle} of %O{handle} was not registered"

    [<Test>]
    let ``every SZ array's base type is System.Array`` () : unit =
        let mutable exercised = 0

        for ns, name in elementTypes do
            let state, elementHandle = concretize (state ()) ns name
            let handle = ConcreteTypeHandle.OneDimArrayZero elementHandle

            arrayBaseType state handle |> shouldEqual bct.Array.Identity
            exercised <- exercised + 1

        // Guard against the loop silently doing nothing.
        exercised |> shouldEqual (List.length elementTypes)

    [<Test>]
    let ``every multi-dimensional array's base type is System.Array, at any rank`` () : unit =
        let property (elementIndex : int) (rawRank : int) : bool =
            let ns, name = elementTypes.[abs elementIndex % List.length elementTypes]
            // Rank 1 is representable as `Array (elem, 1)` — a rank-1 array with a non-zero
            // lower bound, distinct from `OneDimArrayZero` — so include it.
            let rank = (abs rawRank % 32) + 1

            let state, elementHandle = concretize (state ()) ns name
            let handle = ConcreteTypeHandle.Array (elementHandle, rank)

            arrayBaseType state handle = bct.Array.Identity

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 300, property)

    [<Test>]
    let ``nested array element types do not change the base type`` () : unit =
        // int[][][]... to a few levels: each level is still an array whose base is
        // System.Array, so the fall-through terminates at the same place however deep the
        // element nesting goes.
        let state, elementHandle = concretize (state ()) "System" "Int32"

        let mutable handle = ConcreteTypeHandle.OneDimArrayZero elementHandle

        for _ in 1..5 do
            arrayBaseType state handle |> shouldEqual bct.Array.Identity
            handle <- ConcreteTypeHandle.OneDimArrayZero handle

    [<Test>]
    let ``System.Array declares exactly the interfaces an array inherits`` () : unit =
        // The fall-through reports whatever System.Array's closure contains, so pin that
        // closure by name. The differential tests assert the resulting totals (6 for a
        // multi-dimensional array, and 11 for an SZ array once PopulateInterfaces has added
        // the five implicit generic ones); if corelib ever changes System.Array's interface
        // list, those numbers move, and this test names what moved.
        let state, arrayHandle = concretize (state ()) "System" "Array"

        let _, typeInfo =
            IlMachineState.tryGetConcreteTypeInfo state arrayHandle
            |> Option.defaultWith (fun () -> failwith "System.Array was not registered")

        // State must be threaded through: each concretization can register new handles that
        // the subsequent lookup depends on.
        let _, declared =
            ((state, Set.empty), typeInfo.ImplementedInterfaces)
            ||> Seq.fold (fun (state, acc) impl ->
                let state, implTypeDefn, implAssy =
                    IlMachineState.resolveTypeMetadataToken loggerFactory bct state corelib impl.InterfaceHandle

                let state, implHandle =
                    IlMachineState.concretizeType
                        loggerFactory
                        bct
                        state
                        implAssy.DefinitionFullName
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        implTypeDefn

                match IlMachineState.tryGetConcreteTypeInfo state implHandle with
                | Some (_, info) -> state, Set.add $"%s{info.Namespace}.%s{info.Name}" acc
                | None -> failwith $"interface handle %O{implHandle} of System.Array was not registered"
            )

        // All six are declared directly on System.Array — ICollection and IEnumerable are
        // listed explicitly rather than being left to arrive transitively through IList — so
        // the declared set and the transitive closure coincide, and the reported total is 6.
        declared
        |> shouldEqual (
            Set.ofList
                [
                    "System.ICloneable"
                    "System.Collections.ICollection"
                    "System.Collections.IEnumerable"
                    "System.Collections.IList"
                    "System.Collections.IStructuralComparable"
                    "System.Collections.IStructuralEquatable"
                ]
        )
