namespace WoofWare.PawPrint.Test

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `GcReferences.containsReferences` checked against the host's own
/// `RuntimeHelpers.IsReferenceOrContainsReferences<T>()`, over the types of the host's CoreLib, the
/// image the host runs.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestGcReferences =

    let private isReferenceOrContainsReferences =
        typeof<System.Runtime.CompilerServices.RuntimeHelpers>
            .GetMethod ("IsReferenceOrContainsReferences", BindingFlags.Public ||| BindingFlags.Static)

    /// What the host answers for `ty`, or `None` where it refuses `ty` as the type argument.
    let private hostAnswer (ty : Type) : bool option =
        try
            let answer : obj =
                isReferenceOrContainsReferences.MakeGenericMethod(ty).Invoke ((null : obj), Array.empty<obj>)

            Some (unbox<bool> answer)
        with
        | :? ArgumentException
        | :? BadImageFormatException -> None

    [<Test>]
    let ``every CoreLib type, and each generic one over int and over string, holds references as the host says``
        ()
        : unit
        =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let corelib = Assembly.readFile loggerFactory HostImage.coreLibPath
        let hostCoreLib = typeof<obj>.Assembly
        let bct = BaseClassTypes.ofCorelib corelib
        let loaded = LoadedAssemblies.ofAssemblies [ corelib ]

        let mutable state =
            { TypeSystemState.Empty with
                _LoadedAssemblies = loaded
                ConcreteTypes = Corelib.concretizeAll loaded bct AllConcreteTypes.Empty
            }

        // The value types compared, by what the host answers for them.
        let valueTypes = Dictionary<bool, int> ()
        let failures = ResizeArray<string> ()

        let check (host : Type) (spelled : TypeDefn) : unit =
            match hostAnswer host with
            | None -> ()
            | Some expected ->
                let next, handle =
                    TypeSystemState.concretizeType
                        loggerFactory
                        HostImage.runtimeDirs
                        bct
                        state
                        corelib.DefinitionFullName
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        spelled

                let next, actual =
                    GcReferences.containsReferences loggerFactory HostImage.runtimeDirs bct next handle

                state <- next

                if host.IsValueType then
                    valueTypes.[expected] <-
                        (match valueTypes.TryGetValue expected with
                         | true, n -> n + 1
                         | false, _ -> 1)

                if actual <> expected then
                    failures.Add $"%s{host.FullName}: host %b{expected}, PawPrint %b{actual}"

        let kind (host : Type) : SignatureTypeKind =
            if host.IsValueType then
                SignatureTypeKind.ValueType
            else
                SignatureTypeKind.Class

        let int = typeof<int>, TypeDefn.PrimitiveType PrimitiveType.Int32
        let string = typeof<string>, TypeDefn.PrimitiveType PrimitiveType.String

        // The instantiations of a generic type of this arity to compare: every argument `int`,
        // every argument `string`, and `int` and `string` alternating.
        let instantiations (arity : int) : (Type * TypeDefn) list list =
            [
                List.replicate arity int
                List.replicate arity string
                List.init arity (fun i -> if i % 2 = 0 then int else string)
            ]

        // The first row is `<Module>`, which reflection does not resolve as a type.
        for KeyValue (handle, ty) in corelib.TypeDefs do
            if MetadataTokens.GetRowNumber (TypeDefinitionHandle.op_Implicit handle : EntityHandle) > 1 then
                let host =
                    hostCoreLib.ManifestModule.ResolveType (
                        MetadataTokens.GetToken (TypeDefinitionHandle.op_Implicit handle : EntityHandle)
                    )

                let definition = TypeDefn.FromDefinition (ty.Identity, kind host)

                if not host.IsGenericTypeDefinition then
                    check host definition
                else
                    for arguments in instantiations (host.GetGenericArguments().Length) |> List.distinct do
                        let instantiated =
                            try
                                Some (host.MakeGenericType (arguments |> List.map fst |> Array.ofList))
                            with :? ArgumentException ->
                                None

                        match instantiated with
                        | None -> ()
                        | Some instantiated ->
                            TypeDefn.GenericInstantiation (
                                definition,
                                ImmutableArray.CreateRange (arguments |> List.map snd)
                            )
                            |> check instantiated

        if failures.Count > 0 then
            failures |> String.concat Environment.NewLine |> failwith

        // Enough value types were compared, each way, for the property to say something.
        valueTypes.GetValueOrDefault true |> shouldBeGreaterThan 200
        valueTypes.GetValueOrDefault false |> shouldBeGreaterThan 500
