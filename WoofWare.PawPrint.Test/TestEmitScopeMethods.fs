namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.Reflection
open System.Text
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The methods CoreCLR's emitter defines in a dynamic module for `TypeBuilder_DefineMethod`, and the
/// implementation flags `TypeBuilder_SetMethodImpl` fills in afterwards.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEmitScopeMethods =

    /// Names the emitter marks special and names it does not: it compares the UTF-16 name, ordinally,
    /// with `.ctor` and `.cctor`, and its first eight characters with `_VtblGap`. Two differ only in a
    /// lone surrogate, which the emitter stores as the same UTF-8.
    let private methodNames : string list =
        [
            "M"
            ".ctor"
            ".cctor"
            ".CTOR"
            ".ctor\uD800"
            "_VtblGap"
            "_VtblGap1_2"
            "_VtblGap\uD800"
            "_VtblGa"
            "_vtblGap"
            "M\uD800"
            "M\uDBFF"
        ]

    /// Signatures, some of them alike, so that two methods of one type share a name and signature.
    let private signatures : ImmutableArray<byte> list =
        [
            ImmutableArray.Create<byte> [| 0x20uy ; 0x00uy ; 0x01uy |]
            ImmutableArray.Create<byte> [| 0x00uy ; 0x00uy ; 0x01uy |]
            ImmutableArray.Create<byte> [| 0x20uy ; 0x01uy ; 0x08uy ; 0x0Euy |]
        ]

    /// `mdReservedMask`, which `DefineMethod` clears from the flags it is given.
    let private reserved : int = 0xd000

    /// The string a `#Strings` entry holds for `value`: its UTF-8, with each lone surrogate replaced.
    let private stored (value : string) : string =
        Encoding.UTF8.GetString (Encoding.UTF8.GetBytes value)

    /// A scope with `<Module>` and two more types, for methods to belong to.
    let private withTypes () : EmitScope =
        let define (fullName : string) (scope : EmitScope) : EmitScope =
            match EmitScopeDefinitions.defineTopLevelTypeDef fullName TypeAttributes.Public None [] scope with
            | TypeDefDefinition.Defined (_, scope) -> scope
            | TypeDefDefinition.Duplicate _ -> failwith $"%s{fullName} is already in the scope"

        EmitScope.create "Dyn" Guid.Empty |> define "A" |> define "N.B"

    /// A method as the model holds it.
    type private ModelMethod =
        {
            Owner : TypeDefRid
            Name : string
            Signature : ImmutableArray<byte>
            Flags : MethodAttributes
            ImplFlags : MethodImplAttributes
        }

    type private Op =
        | Define of owner : int * name : int * signature : int * flags : int
        | SetImpl of method : int * flags : int

    let private genFlags : Gen<int> =
        Gen.zip (Gen.choose (0, 0xFFFF)) (Gen.choose (0, 0xFFFF))
        |> Gen.map (fun (high, low) -> (high <<< 16) ||| low)

    let private genOp : Gen<Op> =
        Gen.frequency
            [
                3,
                gen {
                    let! owner = Gen.choose (0, 1000)
                    let! name = Gen.choose (0, methodNames.Length - 1)
                    let! signature = Gen.choose (0, signatures.Length - 1)
                    let! flags = genFlags
                    return Op.Define (owner, name, signature, flags)
                }
                1, Gen.map2 (fun m f -> Op.SetImpl (m, f)) (Gen.choose (0, 1000)) genFlags
            ]

    [<Test>]
    let ``every method is a new row with the flags the emitter keeps, and its implementation flags are the last set``
        ()
        : unit
        =
        let mutable special = 0

        let property (ops : Op list) : unit =
            let start = withTypes ()
            let typeCount = (EmitScope.typeDefs start).Count
            let mutable model : ModelMethod list = []
            let mutable scope = start

            for op in ops do
                match op with
                | Op.Define (owner, nameIndex, signatureIndex, flags) ->
                    let owner = TypeDefRid (owner % typeCount + 1)
                    let name = methodNames.[nameIndex]
                    let signature = signatures.[signatureIndex]

                    let isSpecial =
                        name = ".ctor"
                        || name = ".cctor"
                        || name.StartsWith ("_VtblGap", StringComparison.Ordinal)

                    if isSpecial then
                        special <- special + 1

                    let expectedFlags =
                        (flags &&& ~~~reserved &&& 0xFFFF)
                        ||| (if isSpecial then
                                 int (MethodAttributes.RTSpecialName ||| MethodAttributes.SpecialName)
                             else
                                 0)

                    let rid, next =
                        EmitScopeDefinitions.defineMethod owner name signature (enum<MethodAttributes> flags) scope

                    model <-
                        model
                        @ [
                            {
                                Owner = owner
                                Name = name
                                Signature = signature
                                Flags = enum<MethodAttributes> expectedFlags
                                ImplFlags = MethodImplAttributes.IL
                            }
                        ]

                    rid |> shouldEqual (MethodDefRid model.Length)
                    scope <- next
                | Op.SetImpl (methodChoice, flags) ->
                    if not model.IsEmpty then
                        let index = methodChoice % model.Length

                        scope <-
                            EmitScopeDefinitions.setMethodImplFlags
                                (MethodDefRid (index + 1))
                                (enum<MethodImplAttributes> flags)
                                scope

                        model <-
                            model
                            |> List.updateAt
                                index
                                { model.[index] with
                                    ImplFlags = enum<MethodImplAttributes> (flags &&& 0xFFFF)
                                }

            EmitScope.methodDefs scope
            |> Seq.map (fun row ->
                row.Body |> shouldEqual None

                {
                    Owner = row.Owner
                    Name = EmitScope.stringAt row.Name scope
                    Signature = EmitScope.blobAt row.Signature scope
                    Flags = row.Flags
                    ImplFlags = row.ImplFlags
                }
            )
            |> List.ofSeq
            |> List.map (fun m -> m.Owner, m.Name, List.ofSeq m.Signature, m.Flags, m.ImplFlags)
            |> shouldEqual (
                model
                |> List.map (fun m -> m.Owner, stored m.Name, List.ofSeq m.Signature, m.Flags, m.ImplFlags)
            )

            // Nothing but the MethodDef table and the heaps changes.
            EmitScope.typeDefs scope |> shouldEqual (EmitScope.typeDefs start)
            EmitScope.typeRefs scope |> shouldEqual (EmitScope.typeRefs start)
            EmitScope.parameters scope |> Seq.isEmpty |> shouldEqual true

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 300,
            Prop.forAll (Arb.fromGen (Gen.listOf genOp |> Gen.map List.ofSeq)) property
        )

        special |> shouldBeGreaterThan 100

    let private thrown (f : unit -> 'a) : string =
        match
            (try
                f () |> ignore
                None
             with :? ArgumentException as e ->
                 Some e.Message)
        with
        | None -> failwith "expected an ArgumentException"
        | Some message -> message

    [<Test>]
    let ``a method row's flags must fit their two-byte columns`` () : unit =
        let scope = withTypes ()
        let name, scope = EmitScope.internString "M" scope
        let signature, scope = EmitScope.internBlob signatures.[0] scope

        let row : MethodDefRow =
            {
                Owner = TypeDefRid 2
                Body = None
                ImplFlags = MethodImplAttributes.IL
                Flags = MethodAttributes.Public
                Name = name
                Signature = signature
            }

        thrown (fun () ->
            EmitScope.addMethodDef
                { row with
                    Flags = enum<MethodAttributes> 0x10006
                }
                scope
        )
        |> shouldContainText "0x10006"

        thrown (fun () ->
            EmitScope.addMethodDef
                { row with
                    ImplFlags = enum<MethodImplAttributes> 0x10000
                }
                scope
        )
        |> shouldContainText "0x10000"

        let method, scope = EmitScope.addMethodDef row scope

        thrown (fun () -> EmitScope.setMethodImplFlags method (enum<MethodImplAttributes> 0x10008) scope)
        |> shouldContainText "0x10008"

        thrown (fun () -> EmitScope.setMethodImplFlags (MethodDefRid 2) MethodImplAttributes.NoInlining scope)
        |> shouldContainText "MethodDef RID 2"

        let filled =
            EmitScope.setMethodImplFlags method MethodImplAttributes.NoInlining scope

        (EmitScope.methodDefs filled).[0]
        |> shouldEqual
            { row with
                ImplFlags = MethodImplAttributes.NoInlining
            }

        // The scope it was given is unchanged.
        (EmitScope.methodDefs scope).[0] |> shouldEqual row
