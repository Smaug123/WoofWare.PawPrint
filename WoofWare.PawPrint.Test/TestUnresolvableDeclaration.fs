namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Emit
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// A MethodImpl declaration this rule cannot resolve must make the *table* decline, not abort the run.
///
/// The distinction only became load-bearing when dispatch started reading the table. Building content
/// resolves every MethodImpl row on every type in the receiver's chain, so a single unresolvable row
/// would otherwise abort every virtual call on that type -- `ToString` included -- where the
/// signature-matching walk it replaced never looked at unrelated rows at all. Declining instead hands
/// the call back to that walk, which is what answered it before the table existed.
///
/// The shape Codex named for this -- a MemberRef to an ancestor that merely *inherits* the named
/// method, which is valid IL CoreCLR loads by searching the ancestor's bases -- cannot be fabricated:
/// `DefineMethodOverride` takes a `MethodInfo`, and a `MethodInfo`'s `DeclaringType` is where the
/// method is declared, so the row always names the declaring type. Measured, not assumed.
///
/// What *is* fabricable reaches the same code path: a MemberRef on a TypeSpec naming a method that
/// holds no vtable slot. `AG&lt;T&gt;` declares `M` non-virtually, so it is absent from the placement list
/// the declaration is matched against, and the candidate set comes out empty exactly as it would for
/// the inherited case.
[<TestFixture>]
module TestUnresolvableDeclaration =

    /// `AG<T>` declares `M(T)` *non-virtually*; `BG : AG<int>` declares a `newslot virtual M(int)`
    /// carrying `.override AG<int>::M`. `CG : BG` exists so that the chain has a type below the one
    /// carrying the row.
    let private image : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "Unresolvable", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Unresolvable"

        let body (method : MethodBuilder) (value : int) : unit =
            let il = method.GetILGenerator ()
            il.Emit (OpCodes.Ldc_I4, value)
            il.Emit OpCodes.Ret

        let ag = modul.DefineType ("AG`1", TypeAttributes.Public)
        let tParameter = (ag.DefineGenericParameters [| "T" |]).[0] :> Type

        let agMethod =
            ag.DefineMethod ("M", MethodAttributes.Public, typeof<int>, [| tParameter |])

        body agMethod 1
        let agType = ag.CreateType ()
        let agClosed = agType.MakeGenericType [| typeof<int> |]

        let bg = modul.DefineType ("BG", TypeAttributes.Public, agClosed)

        let bgMethod =
            bg.DefineMethod (
                "M",
                MethodAttributes.Public
                ||| MethodAttributes.Virtual
                ||| MethodAttributes.NewSlot,
                typeof<int>,
                [| typeof<int> |]
            )

        body bgMethod 2
        bg.DefineMethodOverride (bgMethod, TypeBuilder.GetMethod (agClosed, agMethod))
        let bgType = bg.CreateType ()

        let cg = modul.DefineType ("CG", TypeAttributes.Public, bgType)

        body
            (cg.DefineMethod ("M", MethodAttributes.Public ||| MethodAttributes.Virtual, typeof<int>, [| typeof<int> |]))
            3

        cg.CreateType () |> ignore<Type>

        // A plain class carrying no MethodImpl at all, as the control for the decline below.
        let plain = modul.DefineType ("Plain", TypeAttributes.Public)

        body
            (plain.DefineMethod (
                "M",
                MethodAttributes.Public
                ||| MethodAttributes.Virtual
                ||| MethodAttributes.NewSlot,
                typeof<int>,
                Type.EmptyTypes
            ))
            4

        plain.CreateType () |> ignore<Type>

        use stream = new MemoryStream ()
        builder.Save stream
        stream.ToArray ()

    // Undisposed on purpose, as in the sibling layout fixtures: the DumpedAssembly's logger closes
    // over its sinks, and disposing while the assembly is live would drop events.
    let private corelib : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory (typeof<obj>.Assembly.Location)

    let private fabricated : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.read loggerFactory None (new MemoryStream (image))

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : LoadedAssemblies =
        LoadedAssemblies.ofAssemblies [ corelib ; fabricated ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loaded bct AllConcreteTypes.Empty

    let private loggerFactory = snd (LoggerFactory.makeTest ())

    let private state () : IlMachineState =
        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
            _LoadedAssemblies = loaded
        }

    let private closedHandle (name : string) : IlMachineState * ConcreteTypeHandle =
        let typeInfo =
            match fabricated.TryGetTopLevelTypeDef "" name with
            | None -> failwith $"fabricated assembly has no type %s{name}"
            | Some typeInfo -> typeInfo

        DumpedAssembly.typeInfoToTypeDefn' bct loaded typeInfo
        |> IlMachineState.concretizeType
            loggerFactory
            bct
            (state ())
            fabricated.DefinitionFullName
            ImmutableArray.Empty
            ImmutableArray.Empty

    /// The fabrication really does emit the shape, checked directly. If a future builder resolved
    /// `TypeBuilder.GetMethod` differently -- to a MethodDef, say -- the decline below would still hold
    /// but for another reason, and this fixture would be testing something else.
    [<Test>]
    let ``the fabricated declaration is a MemberRef on a TypeSpec`` () : unit =
        let bg =
            match fabricated.TryGetTopLevelTypeDef "" "BG" with
            | None -> failwith "fabricated assembly has no BG"
            | Some typeInfo -> typeInfo

        match bg.MethodImpls.Values |> Seq.map (fun impl -> impl.Declaration) |> List.ofSeq with
        | [ MetadataToken.MemberReference handle ] ->
            match fabricated.Members.[handle].Parent with
            | MetadataToken.TypeSpecification _ -> ()
            | other -> failwith $"BG's MethodImpl declaration names its parent with %O{other}, not a TypeSpec"
        | other -> failwith $"expected BG to carry exactly one MemberRef MethodImpl; got %i{List.length other}"

    [<Test>]
    let ``a declaration this rule cannot resolve declines the table rather than aborting`` () : unit =
        // `CG` inherits the row from `BG`, so building its table resolves that row too -- which is the
        // blast radius: before this declined, asking for `CG`'s table at all would throw, and dispatch
        // asks on every `callvirt`.
        for typeName in [ "BG" ; "CG" ] do
            let state, handle = closedHandle typeName

            let _, table =
                VirtualSlotLayout.dispatchTableOfClosed loggerFactory bct "test" state handle

            match table with
            | None -> ()
            | Some (_, content) ->
                failwith
                    $"expected %s{typeName}'s dispatch table to decline, but it produced %i{List.length content} slots"

    [<Test>]
    let ``a type without such a row still gets its table`` () : unit =
        // Non-vacuity: the decline above has to be about the unresolvable row, not about this
        // fabrication being unreadable in general. `Plain` carries no MethodImpl at all.
        let state, handle = closedHandle "Plain"

        let _, table =
            VirtualSlotLayout.dispatchTableOfClosed loggerFactory bct "test" state handle

        match table with
        | Some (_, content) -> List.length content |> shouldBeGreaterThan 0
        | None -> failwith "expected Plain's dispatch table to be built; it declined"
