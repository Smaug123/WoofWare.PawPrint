namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `ldtoken` of a *member* token that is not a definition in the executing assembly: a
/// `MemberReference` (method or field, `TypeReference` or `TypeSpecification` parent) or a
/// `MethodSpecification`.
///
/// These are the token kinds CoreCLR resolves *with* a `SigTypeContext` taken from the enclosing
/// method — `CEEInfo::resolveToken` calls `GetTypeContext(pResolvedToken->tokenContext, ...)` for
/// `mdtMemberRef`, `mdtMethodSpec` and `mdtTypeSpec`, and for `mdtMethodDef`/`mdtFieldDef` it does
/// not (jitinterface.cpp). So the executing frame's instantiation is what these tokens resolve
/// against, and getting that threading wrong yields a *wrong handle*, not a failure. Most of what
/// is asserted below exists to make that visible.
[<TestFixture>]
module TestLdtokenMemberTokens =

    /// Every member-reference shape this fixture needs, in one assembly.
    ///
    /// `Get`/`Item` are referenced only at `Gen<int>` and `GetOpen`/`OpenItem` only at
    /// `Gen<List<T>>`, so a lookup by name picks out exactly one `MemberReference` row and the
    /// `Seq.exactlyOne` below is the disambiguation rather than a hopeful guess.
    let private source =
        """
using System;
using System.Collections.Generic;
using System.Threading.Tasks;

public class Gen<T>
{
    public T Item;
    public T OpenItem;
    // Bodies deliberately touch no field: inside `Gen<T>`, `Item` would be reached through a
    // `Gen<!0>` TypeSpec parent, giving a second MemberReference row of the same name and making
    // the by-name lookup below ambiguous.
    public T Get() => default(T);
    public T GetOpen() => default(T);
    public T Other() => default(T);
    public static U Stat<U>(U x) => x;
    public static U StatOpen<U>(U x) => x;

    // The frame this fixture installs. Generic method on a generic type, so that every
    // `!0` and `!!0` appearing in any member reference below has a binding. Parameterless
    // because the frame is installed with no arguments; the body is never executed, only
    // compiled, so the null locals below are fine.
    public static int Sweep<V>()
    {
        Gen<List<T>> open_ = null;
        Gen<int> closed = null;
        V v = default(V);
        // MemberRef, TypeSpec parent whose argument list is `[List<!0>]` -- it mentions the
        // frame's type parameter but is deliberately NOT equal to the frame's own
        // instantiation `[!0]`, so a resolution that fell back to the frame's generics
        // produces `Gen<T>` where the token says `Gen<List<T>>`.
        int a = open_.GetOpen().Count + open_.OpenItem.Count;
        // MemberRef, TypeSpec parent, closed argument list.
        int b = closed.Get() + closed.Item + closed.Other();
        // MethodSpec over MemberRef, with a closed method argument.
        int c = Gen<string>.Stat<int>(1);
        // MethodSpec over MemberRef, method argument is the *frame's* method parameter, so a
        // resolution that ignored the frame's method generics yields a different instantiation
        // rather than an error.
        V d = Gen<string>.StatOpen<V>(v);
        // MemberRef, TypeRef parent, method (the rung-H shape).
        Task t = Task.CompletedTask;
        // MemberRef, TypeRef parent, field.
        string s = string.Empty;
        // MethodSpec over MethodDef.
        int e = Caller.Ident<int>(2);
        return a + b + c + s.Length + e + (t == null ? 0 : 1) + (d == null ? 0 : 1);
    }
}

public static class Caller
{
    public static T Ident<T>(T x) => x;

    public static int Current() => 0;
}
"""

    let private loadFixture () =
        let image =
            Roslyn.compileAssembly
                "LdtokenMemberTokensAssembly"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ source ]

        let _, loggerFactory = LoggerFactory.makeTest ()

        let corelib =
            global.WoofWare.PawPrint.AssemblyApi.readFile loggerFactory typeof<obj>.Assembly.Location

        let baseClassTypes = Corelib.getBaseTypes corelib

        use assemblyStream = new MemoryStream (image)

        let assembly =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None assemblyStream

        let state : IlMachineState =
            let initialState =
                IlMachineState.initial loggerFactory ImmutableArray.Empty assembly

            let state = initialState.WithLoadedAssembly corelib

            { state with
                ConcreteTypes = Corelib.concretizeAll state._LoadedAssemblies baseClassTypes state.ConcreteTypes
            }

        loggerFactory, baseClassTypes, corelib, assembly, state

    let private findMethod
        (declaringTypeName : string)
        (methodName : string)
        (assembly : DumpedAssembly)
        : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        =
        assembly.Methods.Values
        |> Seq.find (fun method ->
            method.RequiredDeclaringType.Name = declaringTypeName
            && method.Name = methodName
        )

    /// Which table a `MemberReference`'s parent indexes. The pair (name, parent kind) is unique
    /// across this fixture's source; `memberRef` asserts as much rather than taking the first hit.
    [<RequireQualifiedAccess>]
    type private ParentKind =
        | TypeRef
        | TypeSpec

    let private memberRef
        (assembly : DumpedAssembly)
        (parentKind : ParentKind)
        (name : string)
        : MemberReferenceHandle
        =
        assembly.Members
        |> Seq.filter (fun (KeyValue (_, mem)) ->
            assembly.Strings mem.Name = name
            && match mem.Parent, parentKind with
               | MetadataToken.TypeReference _, ParentKind.TypeRef -> true
               | MetadataToken.TypeSpecification _, ParentKind.TypeSpec -> true
               | _, _ -> false
        )
        |> Seq.map (fun (KeyValue (handle, _)) -> handle)
        |> Seq.toList
        |> function
            | [ single ] -> single
            | [] -> failwith $"no MemberReference named %s{name} with a %O{parentKind} parent"
            | several ->
                failwith
                    $"%d{several.Length} MemberReferences named %s{name} with a %O{parentKind} parent; the fixture's source has become ambiguous"

    /// The `MethodSpecification` row whose underlying method reference is `name`.
    let private methodSpec (assembly : DumpedAssembly) (name : string) : MethodSpecificationHandle =
        assembly.MethodSpecs
        |> Seq.filter (fun (KeyValue (_, spec)) ->
            match spec.Method with
            | MetadataToken.MemberReference h -> assembly.Strings assembly.Members.[h].Name = name
            | MetadataToken.MethodDef h -> assembly.Methods.[h].Name = name
            | _ -> false
        )
        |> Seq.map (fun (KeyValue (handle, _)) -> handle)
        |> Seq.toList
        |> function
            | [ single ] -> single
            | [] -> failwith $"no MethodSpecification over a method named %s{name}"
            | several -> failwith $"%d{several.Length} MethodSpecifications over a method named %s{name}"

    /// Install `Gen<typeGenerics>.Sweep<methodGenerics>` as the executing frame. Every member
    /// reference in the fixture source appears in that method's body, so this is the context all
    /// of them resolve against.
    let private installSweepFrame
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assembly : DumpedAssembly)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (state : IlMachineState)
        : IlMachineState * ThreadId
        =
        let method =
            assembly
            |> findMethod "Gen`1" "Sweep"
            |> MethodInfo.mapTypeGenerics (fun (param, _) -> TypeDefn.GenericTypeParameter param.SequenceNumber)

        let state, concretizedMethod, _declaringType =
            ExecutionConcretization.concretizeMethodWithAllGenerics
                loggerFactory
                baseClassTypes
                typeGenerics
                method
                methodGenerics
                state

        let methodState =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    baseClassTypes
                    state._LoadedAssemblies
                    assembly
                    concretizedMethod
                    ImmutableArray.Empty
                    ImmutableArray.Empty
                    None
            with
            | Ok methodState -> methodState
            | Error missing -> failwith $"Unexpected missing assembly references creating ldtoken frame: %O{missing}"

        let thread = ThreadId.ThreadId 0

        let state =
            { state with
                ThreadState =
                    Map.empty
                    |> Map.add thread (ThreadState.New (CpuId 0) (OsThreadId 1u) methodState)
            }

        state, thread

    let private executeLdtoken
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        (thread : ThreadId)
        (state : IlMachineState)
        : IlMachineState
        =
        let operand =
            token |> SourcedMetadataToken.make assembly.Name |> MetadataOperand.FromMetadata

        let state, whatWeDid =
            UnaryMetadataIlOp.execute loggerFactory baseClassTypes UnaryMetadataTokenIlOp.Ldtoken operand state thread

        whatWeDid |> shouldEqual WhatWeDid.Executed
        state

    /// The stub address inside the `RuntimeMethodHandle` / `RuntimeFieldHandle` that `ldtoken` just
    /// pushed. Both handle structs hold a single reference field, so both flatten to an object
    /// reference on the eval stack. That address *is* the identity the guest compares, so every
    /// assertion below about "the same handle" is an assertion about this.
    let private pushedStub (thread : ThreadId) (state : IlMachineState) : ManagedHeapAddress =
        match IlMachineState.peekEvalStack thread state with
        | Some (EvalStackValue.ObjectRef addr) -> addr
        | Some other -> failwith $"Expected ldtoken to push a handle struct, got %O{other}"
        | None -> failwith "Expected ldtoken to push a handle, but the eval stack was empty"

    let private stringHandle (baseClassTypes : BaseClassTypes<DumpedAssembly>) (state : IlMachineState) =
        AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.String

    let private int32Handle (baseClassTypes : BaseClassTypes<DumpedAssembly>) (state : IlMachineState) =
        AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Int32

    /// The metadata identity the registry recorded for the handle `ldtoken` just pushed.
    ///
    /// A metadata token can only ever name a metadata-backed method, so the `FromDynamic` arm
    /// failing here is itself an assertion: an `ldtoken` that produced one would mean a registry id
    /// had been crossed with a `DynamicMethod`'s.
    let private methodHandleOf (thread : ThreadId) (state : IlMachineState) : MetadataMethodIdentity =
        let stub = pushedStub thread state

        let id =
            match
                ManagedHeap.get stub state.ManagedHeap
                |> AllocatedNonArrayObject.DereferenceField "m_value"
            with
            | CliType.ValueType vt ->
                match CliValueType.DereferenceField "m_handle" vt with
                | CliType.RuntimePointer (CliRuntimePointer.MethodRegistryHandle id) -> id
                | other ->
                    failwith $"Expected RuntimeMethodHandleInternal.m_handle to be a registry handle, got %O{other}"
            | other -> failwith $"Expected RuntimeMethodInfoStub.m_value to be a value type, got %O{other}"

        match MethodHandleRegistry.resolveMethodFromId id state.MethodHandles with
        | Some (MethodHandle.FromMetadata identity) -> identity
        | Some (MethodHandle.FromDynamic handle) ->
            failwith $"ldtoken of a metadata token produced the dynamic-method handle %O{handle}"
        | None -> failwith $"method registry id %d{id} was not registered"

    /// The `FieldHandle` the registry recorded for the handle `ldtoken` just pushed.
    let private fieldHandleOf (thread : ThreadId) (state : IlMachineState) : FieldHandle =
        let stub = pushedStub thread state

        FieldHandleRegistry.resolveFieldFromAddress stub state.FieldHandles
        |> Option.defaultWith (fun () -> failwith $"RuntimeFieldInfoStub %O{stub} was not registered")

    /// The declaring type of a handle, as a `ConcreteType` — i.e. the identity *and* the
    /// instantiation, which is the whole question for a `TypeSpecification` parent.
    let private declaringConcreteType
        (state : IlMachineState)
        (target : RuntimeTypeHandleTarget)
        : ConcreteType<ConcreteTypeHandle>
        =
        match target with
        | RuntimeTypeHandleTarget.Closed handle ->
            AllConcreteTypes.lookup handle state.ConcreteTypes
            |> Option.defaultWith (fun () -> failwith $"declaring type %O{handle} was not registered")
        | other -> failwith $"Expected a closed declaring type, got %O{other}"

    /// Render a `ConcreteTypeHandle` as `Namespace.Name<args>`, so an assertion about an
    /// instantiation reads as one. Structural handles (arrays and the like) do not occur here.
    let rec private describe (state : IlMachineState) (handle : ConcreteTypeHandle) : string =
        match AllConcreteTypes.lookup handle state.ConcreteTypes with
        | None -> failwith $"type handle %O{handle} was not registered"
        | Some concrete ->
            let name =
                if System.String.IsNullOrEmpty concrete.Namespace then
                    concrete.Name
                else
                    concrete.Namespace + "." + concrete.Name

            if concrete.Generics.IsEmpty then
                name
            else
                let args = concrete.Generics |> Seq.map (describe state) |> String.concat ", "
                name + "<" + args + ">"

    let private describeTarget (state : IlMachineState) (target : RuntimeTypeHandleTarget) : string =
        match target with
        | RuntimeTypeHandleTarget.Closed handle -> describe state handle
        | other -> failwith $"Expected a closed declaring type, got %O{other}"

    let private sweepFrame (loggerFactory, baseClassTypes, _corelib, assembly, state) =
        let typeGenerics = ImmutableArray.Create (stringHandle baseClassTypes state)
        let methodGenerics = ImmutableArray.Create (int32Handle baseClassTypes state)

        let state, thread =
            installSweepFrame loggerFactory baseClassTypes assembly typeGenerics methodGenerics state

        loggerFactory, baseClassTypes, assembly, state, thread

    // ------------------------------------------------------------------------------------------
    // `MemberReference`, `TypeReference` parent. The rung-H shape: the member is defined in
    // another assembly entirely, so "which assembly's tables does this handle index" has two
    // candidate answers and only one right one.
    // ------------------------------------------------------------------------------------------

    [<Test>]
    let ``MemberRef method with TypeReference parent names the defining assembly's MethodDef`` () : unit =
        let fixture = loadFixture ()
        let _, _, corelib, _, _ = fixture
        let loggerFactory, baseClassTypes, assembly, state, thread = sweepFrame fixture

        let token =
            MetadataToken.MemberReference (memberRef assembly ParentKind.TypeRef "get_CompletedTask")

        let state = executeLdtoken loggerFactory baseClassTypes assembly token thread state
        let handle = methodHandleOf thread state

        // The reference is written in the guest assembly; the definition lives in CoreLib, and it
        // is CoreLib's tables the `MethodDefinitionHandle` indexes.
        handle.GetAssemblyFullName () |> shouldEqual corelib.DefinitionFullName
        handle.GetAssemblyFullName () |> shouldNotEqual assembly.DefinitionFullName

        // Not merely "an assembly name": the row that name plus this handle selects is the method
        // the reference named. Recording the *referencing* assembly would index a different table.
        let defining =
            state.LoadedAssembly (handle.GetAssemblyFullName ())
            |> Option.defaultWith (fun () -> failwith "defining assembly was not loaded")

        defining.Methods.[handle.GetMethodDefinitionHandle().Get].Name
        |> shouldEqual "get_CompletedTask"

        describeTarget state (handle.GetDeclaringType ())
        |> shouldEqual "System.Threading.Tasks.Task"

        // A non-generic method: no instantiation to carry.
        handle.GetMethodGenerics () |> shouldEqual []

    [<Test>]
    let ``MemberRef field with TypeReference parent records the defining assembly`` () : unit =
        let fixture = loadFixture ()
        let _, _, corelib, _, _ = fixture
        let loggerFactory, baseClassTypes, assembly, state, thread = sweepFrame fixture

        let token =
            MetadataToken.MemberReference (memberRef assembly ParentKind.TypeRef "Empty")

        let state = executeLdtoken loggerFactory baseClassTypes assembly token thread state
        let handle = fieldHandleOf thread state

        // `FieldDefinitionHandle` is an index into the *defining* assembly's tables. Recording the
        // referencing assembly here would mint a second registry id for a field the guest already
        // holds a handle to, and `ReferenceEquals(FieldInfo.GetFieldFromHandle(h), fi)` would go
        // false. The registry derives this from the declaring type rather than taking it on trust.
        handle.GetAssemblyFullName () |> shouldEqual corelib.DefinitionFullName
        handle.GetAssemblyFullName () |> shouldNotEqual assembly.DefinitionFullName

        let defining =
            state.LoadedAssembly (handle.GetAssemblyFullName ())
            |> Option.defaultWith (fun () -> failwith "defining assembly was not loaded")

        defining.Fields.[handle.GetFieldDefinitionHandle().Get].Name
        |> shouldEqual "Empty"

        describeTarget state (handle.GetDeclaringTypeHandle ())
        |> shouldEqual "System.String"

    // ------------------------------------------------------------------------------------------
    // `MemberReference`, `TypeSpecification` parent. CoreCLR resolves these against the enclosing
    // frame's `SigTypeContext`, so the parent's *own* argument list is what decides the
    // instantiation — not the frame's. The fixture's frame is `Gen<string>.Sweep<int>` and the
    // token below says `Gen<List<T>>`, so the two answers differ and the test can tell them apart.
    // ------------------------------------------------------------------------------------------

    [<Test>]
    let ``MemberRef method with TypeSpecification parent resolves at the parent's instantiation`` () : unit =
        let fixture = loadFixture ()
        let loggerFactory, baseClassTypes, assembly, state, thread = sweepFrame fixture

        let token =
            MetadataToken.MemberReference (memberRef assembly ParentKind.TypeSpec "GetOpen")

        let state = executeLdtoken loggerFactory baseClassTypes assembly token thread state
        let handle = methodHandleOf thread state

        // `Gen<List<string>>`, from the token's `Gen<List<!0>>` with `!0` bound by the frame --
        // *not* `Gen<string>`, which is what falling back to the frame's own declaring-type
        // generics would produce.
        describeTarget state (handle.GetDeclaringType ())
        |> shouldEqual "Gen`1<System.Collections.Generic.List`1<System.String>>"

        handle.GetAssemblyFullName () |> shouldEqual assembly.DefinitionFullName

    [<Test>]
    let ``MemberRef field with TypeSpecification parent resolves at the parent's instantiation`` () : unit =
        let fixture = loadFixture ()
        let loggerFactory, baseClassTypes, assembly, state, thread = sweepFrame fixture

        let token =
            MetadataToken.MemberReference (memberRef assembly ParentKind.TypeSpec "OpenItem")

        let state = executeLdtoken loggerFactory baseClassTypes assembly token thread state
        let handle = fieldHandleOf thread state

        describeTarget state (handle.GetDeclaringTypeHandle ())
        |> shouldEqual "Gen`1<System.Collections.Generic.List`1<System.String>>"

    [<Test>]
    let ``MemberRef with a closed TypeSpecification parent`` () : unit =
        let fixture = loadFixture ()
        let loggerFactory, baseClassTypes, assembly, state, thread = sweepFrame fixture

        let token =
            MetadataToken.MemberReference (memberRef assembly ParentKind.TypeSpec "Get")

        let state = executeLdtoken loggerFactory baseClassTypes assembly token thread state
        let handle = methodHandleOf thread state

        // The parent mentions no variable, so the frame cannot affect it either way. This is the
        // control for the two tests above: it passes whether or not the parent's arguments are
        // threaded through, which is exactly why it is not sufficient on its own.
        describeTarget state (handle.GetDeclaringType ())
        |> shouldEqual "Gen`1<System.Int32>"

    // ------------------------------------------------------------------------------------------
    // `MethodSpecification`, over both parents a spec may have.
    // ------------------------------------------------------------------------------------------

    [<Test>]
    let ``MethodSpec over a MemberReference binds both axes`` () : unit =
        let fixture = loadFixture ()
        let loggerFactory, baseClassTypes, assembly, state, thread = sweepFrame fixture

        let token = MetadataToken.MethodSpecification (methodSpec assembly "Stat")

        let state = executeLdtoken loggerFactory baseClassTypes assembly token thread state
        let handle = methodHandleOf thread state

        // Declaring type from the MemberRef's TypeSpec parent, method instantiation from the spec.
        // The two axes are independent and this token exercises both at once.
        describeTarget state (handle.GetDeclaringType ())
        |> shouldEqual "Gen`1<System.String>"

        handle.GetMethodGenerics ()
        |> List.map (describe state)
        |> shouldEqual [ "System.Int32" ]

    [<Test>]
    let ``MethodSpec argument taken from the frame's method generics`` () : unit =
        let fixture = loadFixture ()
        let loggerFactory, baseClassTypes, assembly, state, thread = sweepFrame fixture

        let token = MetadataToken.MethodSpecification (methodSpec assembly "StatOpen")

        let state = executeLdtoken loggerFactory baseClassTypes assembly token thread state
        let handle = methodHandleOf thread state

        // The spec's argument is `!!0`, so the answer comes from the frame's *method* generics
        // (`Sweep<int>`). A spec whose argument were a literal would exercise no substitution at
        // all and would pass with the method-generic context dropped entirely.
        handle.GetMethodGenerics ()
        |> List.map (describe state)
        |> shouldEqual [ "System.Int32" ]

        describeTarget state (handle.GetDeclaringType ())
        |> shouldEqual "Gen`1<System.String>"

    [<Test>]
    let ``MethodSpec over a MethodDef binds the spec's arguments`` () : unit =
        let fixture = loadFixture ()
        let loggerFactory, baseClassTypes, assembly, state, thread = sweepFrame fixture

        let token = MetadataToken.MethodSpecification (methodSpec assembly "Ident")

        let state = executeLdtoken loggerFactory baseClassTypes assembly token thread state
        let handle = methodHandleOf thread state

        handle.GetMethodGenerics ()
        |> List.map (describe state)
        |> shouldEqual [ "System.Int32" ]

        describeTarget state (handle.GetDeclaringType ()) |> shouldEqual "Caller"

    // ------------------------------------------------------------------------------------------
    // Identity.
    // ------------------------------------------------------------------------------------------

    [<Test>]
    let ``ldtoken of one member token twice yields one handle`` () : unit =
        let fixture = loadFixture ()
        let loggerFactory, baseClassTypes, assembly, state, thread = sweepFrame fixture

        let token =
            MetadataToken.MemberReference (memberRef assembly ParentKind.TypeRef "get_CompletedTask")

        let state = executeLdtoken loggerFactory baseClassTypes assembly token thread state
        let first = pushedStub thread state
        let _, state = IlMachineState.popEvalStack thread state

        let state = executeLdtoken loggerFactory baseClassTypes assembly token thread state
        pushedStub thread state |> shouldEqual first

    [<Test>]
    let ``ldtoken of two members of one type yields two handles`` () : unit =
        // Without this, the dedup assertion above would also be satisfied by an implementation
        // that returned one handle for everything.
        let fixture = loadFixture ()
        let loggerFactory, baseClassTypes, assembly, state, thread = sweepFrame fixture

        let get =
            MetadataToken.MemberReference (memberRef assembly ParentKind.TypeSpec "Get")

        let other =
            MetadataToken.MemberReference (memberRef assembly ParentKind.TypeSpec "Other")

        let state = executeLdtoken loggerFactory baseClassTypes assembly get thread state
        let getStub = pushedStub thread state
        let _, state = IlMachineState.popEvalStack thread state

        let state = executeLdtoken loggerFactory baseClassTypes assembly other thread state
        pushedStub thread state |> shouldNotEqual getStub

    [<Test>]
    let ``ldtoken and getOrAllocateMethod agree on identity`` () : unit =
        // The guest reaches the same method both ways -- `ldtoken` here, and
        // `GetMethod(...).MethodHandle` through the reflection stack, which allocates through
        // `getOrAllocateMethod`. CoreCLR hands back one `MethodDesc` for both, so these must be
        // one stub. Asserted against the direct route rather than against another token spelling:
        // two spellings that share a resolver would agree even if the resolver were wrong.
        let fixture = loadFixture ()
        let loggerFactory, baseClassTypes, assembly, state, thread = sweepFrame fixture

        let token =
            MetadataToken.MemberReference (memberRef assembly ParentKind.TypeSpec "Get")

        let state = executeLdtoken loggerFactory baseClassTypes assembly token thread state
        let viaLdtoken = pushedStub thread state
        let _, state = IlMachineState.popEvalStack thread state

        let method =
            assembly
            |> findMethod "Gen`1" "Get"
            |> MethodInfo.mapTypeGenerics (fun (param, _) -> TypeDefn.GenericTypeParameter param.SequenceNumber)

        let state, concretized, _ =
            ExecutionConcretization.concretizeMethodWithAllGenerics
                loggerFactory
                baseClassTypes
                (ImmutableArray.Create (int32Handle baseClassTypes state))
                method
                ImmutableArray.Empty
                state

        let handleValue, state =
            IlMachineState.getOrAllocateMethod loggerFactory baseClassTypes concretized state

        match handleValue with
        | CliType.ValueType vt ->
            match CliValueType.DereferenceField "m_value" vt with
            | CliType.ObjectRef (Some addr) -> addr |> shouldEqual viaLdtoken
            | other -> failwith $"Expected RuntimeMethodHandle.m_value to be an object ref, got %O{other}"
        | other -> failwith $"Expected a RuntimeMethodHandle value type, got %O{other}"

        ignore<IlMachineState> state

    // ------------------------------------------------------------------------------------------
    // The whole space, not a hand-picked sample.
    // ------------------------------------------------------------------------------------------

    [<Test>]
    let ``every member token in the assembly resolves to a row in its defining assembly`` () : unit =
        // The old catch-all refused *every* MemberReference and MethodSpecification, so a handful
        // of chosen tokens proves only that those are covered. This runs the classifier over every
        // such row the compiler emitted -- including the ones Roslyn adds for attributes, which no
        // hand-written case would think of -- and asserts the property that a wrong assembly or a
        // wrong instantiation breaks: the recorded (assembly, row) pair must name a member of that
        // assembly whose name is the one the token spelled.
        //
        // A handle that recorded the *referencing* assembly would index a foreign table: the row
        // id would either be absent or name an unrelated member, so this is not a tautology over
        // whatever the implementation happened to produce.
        let fixture = loadFixture ()
        let loggerFactory, baseClassTypes, assembly, state, thread = sweepFrame fixture

        // A bare MemberReference to a *generic method* names the typical instantiation, which is
        // out of scope here and refused loudly; it is separated out and its refusal asserted
        // below, rather than dropped, so that "not covered" cannot quietly become "crashes".
        let memberRefTokens, typicalMethodTokens =
            assembly.Members
            |> Seq.map (fun (KeyValue (handle, mem)) ->
                let isTypicalGenericMethod =
                    match mem.Signature with
                    | MemberSignature.Method signature -> signature.GenericParameterCount > 0
                    | MemberSignature.Field _ -> false

                (MetadataToken.MemberReference handle, assembly.Strings mem.Name), isTypicalGenericMethod
            )
            |> Seq.toList
            |> List.partition (snd >> not)
            |> fun (ordinary, typical) -> ordinary |> List.map fst, typical |> List.map fst

        let methodSpecTokens =
            assembly.MethodSpecs
            |> Seq.map (fun (KeyValue (handle, spec)) ->
                let name =
                    match spec.Method with
                    | MetadataToken.MemberReference h -> assembly.Strings assembly.Members.[h].Name
                    | MetadataToken.MethodDef h -> assembly.Methods.[h].Name
                    | other -> failwith $"unexpected MethodSpec parent %O{other}"

                MetadataToken.MethodSpecification handle, name
            )
            |> Seq.toList

        let tokens = memberRefTokens @ methodSpecTokens

        // The fixture's source is what makes this worth running; if it ever stops producing a
        // spread of rows, the sweep silently becomes weak.
        tokens.Length |> shouldBeGreaterThan 10

        // ... and the out-of-scope shape must actually occur, or the assertion below is vacuous.
        typicalMethodTokens |> List.isEmpty |> shouldEqual false

        for token, name in typicalMethodTokens do
            let exn =
                Assert.Throws<System.Exception> (fun () ->
                    executeLdtoken loggerFactory baseClassTypes assembly token thread state
                    |> ignore
                )

            exn.Message |> shouldContainText "names the typical instantiation"

            exn.Message |> shouldContainText name

        let mutable state = state

        for token, name in tokens do
            let after =
                try
                    executeLdtoken loggerFactory baseClassTypes assembly token thread state
                with e ->
                    raise (System.Exception ($"ldtoken of %O{token} (%s{name}) failed", e))

            let describeFailure (recorded : string) =
                $"%O{token} (%s{name}) recorded assembly %s{recorded}"

            match IlMachineState.peekEvalStack thread after with
            | Some (EvalStackValue.ObjectRef stub) ->
                match FieldHandleRegistry.resolveFieldFromAddress stub after.FieldHandles with
                | Some fieldHandle ->
                    let assemblyFullName = fieldHandle.GetAssemblyFullName ()

                    let defining =
                        after.LoadedAssembly assemblyFullName
                        |> Option.defaultWith (fun () ->
                            failwith $"%s{describeFailure assemblyFullName}, which is not loaded"
                        )

                    match defining.Fields.TryGetValue (fieldHandle.GetFieldDefinitionHandle().Get) with
                    | true, field -> field.Name |> shouldEqual name
                    | false, _ -> failwith $"%s{describeFailure assemblyFullName}, which has no such FieldDef row"
                | None ->
                    let methodHandle = methodHandleOf thread after
                    let assemblyFullName = methodHandle.GetAssemblyFullName ()

                    let defining =
                        after.LoadedAssembly assemblyFullName
                        |> Option.defaultWith (fun () ->
                            failwith $"%s{describeFailure assemblyFullName}, which is not loaded"
                        )

                    match defining.Methods.TryGetValue (methodHandle.GetMethodDefinitionHandle().Get) with
                    | true, method -> method.Name |> shouldEqual name
                    | false, _ -> failwith $"%s{describeFailure assemblyFullName}, which has no such MethodDef row"
            | other -> failwith $"%O{token} (%s{name}) pushed %O{other} rather than a handle"

            // Re-running the same token must land on the same stub: dedup, asserted across the
            // whole space rather than for one chosen token.
            let repeat = executeLdtoken loggerFactory baseClassTypes assembly token thread after
            pushedStub thread repeat |> shouldEqual (pushedStub thread after)

            state <- snd (IlMachineState.popEvalStack thread after)
