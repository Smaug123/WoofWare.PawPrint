namespace WoofWare.PawPrint

#nowarn "9"

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.PortableExecutable

/// <summary>
/// Represents information about a method parameter.
/// Corresponds to Parameter in System.Reflection.Metadata.
/// </summary>
type Parameter =
    {
        /// <summary>The name of the parameter.</summary>
        Name : string

        /// <summary>
        /// The default value of the parameter, if one is specified.
        /// This is used for optional parameters.
        /// </summary>
        DefaultValue : Constant

        /// <summary>
        /// The position of the parameter in the parameter list.
        /// For instance methods, index 0 is the 'this' parameter.
        /// </summary>
        SequenceNumber : int
    }

[<RequireQualifiedAccess>]
module Parameter =
    let readAll (metadata : MetadataReader) (param : ParameterHandleCollection) : Parameter ImmutableArray =
        let result = ImmutableArray.CreateBuilder ()

        for param in param do
            let param = metadata.GetParameter param

            // The spec doesn't seem to mention this behaviour, but a sequence number of 0 (and an unnamed parameter)
            // seems to correspond with a ref return.
            if param.SequenceNumber <> 0 then
                {
                    Name = metadata.GetString param.Name
                    DefaultValue = metadata.GetConstant (param.GetDefaultValue ())
                    SequenceNumber = param.SequenceNumber
                }
                |> result.Add

        result.ToImmutable ()

type NativeMethodImport =
    {
        ModuleName : string
        EntryPointName : string
        Attributes : MethodImportAttributes
    }

type ExceptionOffset =
    {
        TryLength : int
        TryOffset : int
        HandlerLength : int
        HandlerOffset : int
    }

/// <summary>
/// Which universe a <c>catch</c> clause's type is drawn from — the same distinction
/// <see cref="MetadataOperand"/> draws for an instruction's operand, and drawn for the same reason.
/// </summary>
/// <remarks>
/// A body decoded from a PE image names its clause type by a metadata token. A body minted by
/// <c>Reflection.Emit</c> names it by an index into its method's <c>DynamicScope</c>:
/// <c>BeginCatchBlock</c> writes <c>GetTokenFor(rtType)</c> into the clause's slot itself
/// (<c>DynamicILGenerator.cs:371</c>), reusing the field that holds a filter's IL offset for every
/// other clause kind. Nothing about the resulting bits distinguishes it from a token naming a real
/// <c>TypeDef</c> row, so a clause decoded as-is would select an unrelated real type — silently,
/// and only when an exception happened to be dispatched through the frame.
/// </remarks>
[<RequireQualifiedAccess>]
type ExceptionCatchType =
    /// A TypeRef, TypeDef or TypeSpec token, resolved against the declaring assembly of the method
    /// whose body the clause belongs to.
    | FromMetadata of MetadataToken
    /// An index into the <c>DynamicScope</c> of the dynamic method whose body the clause belongs
    /// to. Resolved when that method is first prepared for execution, which is where CoreCLR
    /// resolves it too — see <c>DynamicMethodExecution.concretize</c>.
    | FromDynamicScope of index : int

type ExceptionRegion =
    | Filter of filterOffset : int * ExceptionOffset
    | Catch of ExceptionCatchType * ExceptionOffset
    | Finally of ExceptionOffset
    | Fault of ExceptionOffset

    static member OfExceptionRegion (r : System.Reflection.Metadata.ExceptionRegion) : ExceptionRegion =
        let offset =
            {
                HandlerLength = r.HandlerLength
                HandlerOffset = r.HandlerOffset
                TryLength = r.TryLength
                TryOffset = r.TryOffset
            }

        match r.Kind with
        | ExceptionRegionKind.Catch ->
            ExceptionRegion.Catch (ExceptionCatchType.FromMetadata (MetadataToken.ofEntityHandle r.CatchType), offset)
        | ExceptionRegionKind.Filter -> ExceptionRegion.Filter (r.FilterOffset, offset)
        | ExceptionRegionKind.Finally -> ExceptionRegion.Finally offset
        | ExceptionRegionKind.Fault -> ExceptionRegion.Fault offset
        | _ -> raise (ArgumentOutOfRangeException ())

type MethodInstructions<'methodVars> =
    {
        /// <summary>
        /// The IL instructions that compose the method body, along with their offset positions.
        /// Each tuple contains the instruction and its offset in the method body.
        /// </summary>
        Instructions : (IlOp * int) list

        /// <summary>
        /// A map from instruction offset (program counter) to the corresponding IL operation.
        /// This is the inverse of Instructions for efficient lookup.
        /// </summary>
        Locations : Map<int, IlOp>

        /// <summary>
        /// Whether local variables in this method should be initialized to their default values.
        /// This corresponds to the localsinit flag in the method header.
        /// </summary>
        LocalsInit : bool

        LocalVars : ImmutableArray<'methodVars> option

        ExceptionRegions : ImmutableArray<ExceptionRegion>
    }

[<RequireQualifiedAccess>]
module MethodInstructions =
    let onlyRet () : MethodInstructions<'methodVars> =
        let op = IlOp.Nullary NullaryIlOp.Ret

        {
            Instructions = [ op, 0 ]
            Locations = Map.empty |> Map.add 0 op
            LocalsInit = false
            LocalVars = None
            ExceptionRegions = ImmutableArray.Empty
        }

    let setLocalVars<'a, 'b> (v : ImmutableArray<'b> option) (s : MethodInstructions<'a>) : MethodInstructions<'b> =
        {
            Instructions = s.Instructions
            Locations = s.Locations
            LocalsInit = s.LocalsInit
            LocalVars = v
            ExceptionRegions = s.ExceptionRegions
        }

/// <summary>
/// The kind of target an <c>[UnsafeAccessor]</c> method accesses, mirroring
/// <see cref="System.Runtime.CompilerServices.UnsafeAccessorKind"/> from the BCL.
/// </summary>
type UnsafeAccessorKind =
    | Constructor
    | Method
    | StaticMethod
    | Field
    | StaticField

/// Classifies the runtime-synthesised behaviour of a method whose implementation is
/// supplied by the runtime. Most variants correspond to
/// <c>MethodImplAttributes.Runtime</c> (the CLR uses it for delegates and multi-dim
/// array <c>Get</c>/<c>Set</c>/<c>Address</c>/<c>.ctor</c>). The
/// <see cref="UnsafeAccessor"/> variant is different: those methods carry
/// <c>ImplAttributes=IL</c> with <c>RVA=0</c>, and the runtime synthesises the body
/// from the <c>[UnsafeAccessor]</c> attribute rather than from <c>MethodImpl.Runtime</c>.
type RuntimeBehaviour =
    /// A delegate constructor, dispatched by writing the target object and method
    /// pointer into the new delegate instance.
    | DelegateCtor

    /// A delegate <c>Invoke</c> call, dispatched by reading the target/method-pointer
    /// fields off the delegate instance and calling through.
    | DelegateInvoke

    /// The struct-marshalling stub for the declaring type: what CoreCLR builds as synthesised IL
    /// in <c>PInvoke::CreateStructMarshalILStub</c> (dllimport.cpp:5289) and hands to CoreLib as a
    /// code address. PawPrint interprets it directly instead.
    ///
    /// Unlike every other case here, a method carrying this is not declared anywhere — it is a
    /// <see cref="MethodInfo.Synthesised"/>, so it has no MethodDef row.
    | StructMarshalStub

    /// <summary>
    /// A C# 12+ <c>[UnsafeAccessor]</c> <c>extern static</c> method. The runtime
    /// synthesises the body to forward to a (possibly inaccessible) member of the
    /// type given by the attributed method's first parameter (or, for
    /// <see cref="UnsafeAccessorKind.StaticField"/>/<see cref="UnsafeAccessorKind.StaticMethod"/>,
    /// the parameter's static type). <c>TargetName</c> is the value of the
    /// <c>Name</c> property on the attribute; <c>None</c> means the property was not supplied at
    /// all, which is what selects the attribute's documented default of "use the attributed
    /// method's name". An explicitly supplied <c>null</c> is <em>not</em> that: it arrives here as
    /// <c>Some ""</c>, because CoreLib's parse copies the supplied value verbatim and a null copies
    /// as empty.
    /// </summary>
    /// <remarks>
    /// <c>HasTypeNameOverrides</c> says that at least one of the declaration's parameters, or its
    /// return, carries <c>[UnsafeAccessorType("...")]</c> — the .NET 10 attribute that names the
    /// type by assembly-qualified string rather than by signature, so that a member of a type the
    /// declaring assembly cannot reference can still be reached. Every one of CoreLib's own
    /// <c>[UnsafeAccessor]</c> declarations is of that shape. Such a declaration's *signature*
    /// therefore does not name the types dispatch must use, so a dispatcher reading the signature
    /// would resolve against the wrong type (usually <c>System.Object</c>) and silently miss the
    /// target; the flag exists so that it refuses instead.
    /// </remarks>
    | UnsafeAccessor of kind : UnsafeAccessorKind * targetName : string option * hasTypeNameOverrides : bool

    /// <summary>
    /// The Runtime-impl flag is set but PawPrint has no specific handler. This currently
    /// covers <c>BeginInvoke</c>/<c>EndInvoke</c> on delegates and any other
    /// Runtime-impl method we have not classified. Reaching this at dispatch time is a
    /// bug in PawPrint's coverage; the dispatcher fails with a clear message.
    /// </summary>
    | Unrecognised of name : string

/// <summary>
/// The implementation a method carries. The CLR distinguishes several kinds of
/// "method body" beyond plain IL — InternalCalls, P/Invokes, runtime-synthesised
/// methods (delegates, multi-dim arrays), and abstract methods with no
/// body at all. This DU names them all so dispatch sites can match exhaustively
/// rather than treating "no IL" as an undifferentiated <c>None</c>.
/// </summary>
type MethodBody<'methodVars> =
    /// Normal IL body parsed from the assembly's PE stream.
    | Il of MethodInstructions<'methodVars>

    /// <summary>
    /// Marked <c>[MethodImpl(MethodImplOptions.InternalCall)]</c>. The implementation is
    /// supplied by the runtime; PawPrint dispatches via <c>NativeDispatch</c>.
    /// </summary>
    | InternalCall

    /// <summary>
    /// Marked <c>[MethodAttributes.PinvokeImpl]</c>. The import data lives on the parent
    /// <see cref="MetadataMethodFacts.NativeImport"/> field — a P/Invoke is always declared, so
    /// it is always a <see cref="MethodInfo.Metadata"/>.
    /// </summary>
    | PInvoke

    /// <summary>
    /// The runtime synthesises the body. Most cases are flagged by
    /// <c>[MethodImpl(MethodImplOptions.Runtime)]</c> (delegates, multi-dim array helpers
    /// — keyed off the declaring type and method name); the
    /// <see cref="RuntimeBehaviour.UnsafeAccessor"/> variant is the C# 12+
    /// <c>[UnsafeAccessor]</c> <c>extern static</c> case which carries
    /// <c>ImplAttributes=IL</c> instead. See <see cref="RuntimeBehaviour"/>.
    /// </summary>
    | RuntimeProvided of RuntimeBehaviour

    /// <summary>
    /// Marked <c>[MethodAttributes.Abstract]</c> — virtual without a body. Direct dispatch is
    /// illegal, and a <c>callvirt</c> reaching one means dispatch mis-resolved; but a *delegate*
    /// can legitimately name one, since <c>Delegate.CreateDelegate</c> closed over a null receiver
    /// has nothing to virtualise on. Real .NET binds that and raises
    /// <c>BadImageFormatException</c> at invocation, which delegate dispatch reproduces.
    /// </summary>
    | Abstract

[<RequireQualifiedAccess>]
module MethodBody =
    let tryIl<'methodVars> (body : MethodBody<'methodVars>) : MethodInstructions<'methodVars> option =
        match body with
        | MethodBody.Il instr -> Some instr
        | MethodBody.InternalCall
        | MethodBody.PInvoke
        | MethodBody.RuntimeProvided _
        | MethodBody.Abstract -> None

    let mapMethodVars<'a, 'b>
        (f : MethodInstructions<'a> -> MethodInstructions<'b>)
        (body : MethodBody<'a>)
        : MethodBody<'b>
        =
        match body with
        | MethodBody.Il instr -> MethodBody.Il (f instr)
        | MethodBody.InternalCall -> MethodBody.InternalCall
        | MethodBody.PInvoke -> MethodBody.PInvoke
        | MethodBody.RuntimeProvided rb -> MethodBody.RuntimeProvided rb
        | MethodBody.Abstract -> MethodBody.Abstract

/// <summary>
/// A method the runtime supplies rather than metadata declaring: it has no row in the MethodDef
/// table, so none of <see cref="MetadataMethodFacts"/> exists for it.
/// </summary>
/// <remarks>
/// <para>
/// CoreCLR builds these as real <c>MethodDesc</c>s over synthesised IL — see
/// <c>PInvoke::CreateStructMarshalILStub</c> (dllimport.cpp:5289). PawPrint has no IL synthesis,
/// so the case identifies *which* runtime behaviour this is and the interpreter supplies it
/// directly.
/// </para>
/// <para>
/// A synthesised method's identity is its owner plus its kind, and for the two cases with a
/// declaring type that is all the payload needed: a struct-marshal stub is owned by the type being
/// marshalled, so two stubs for the same type are the same method, which is exactly the
/// per-MethodTable identity CoreCLR's stub cache has.
/// </para>
/// <para>
/// <see cref="DynamicMethod"/> is the exception, and has to carry one. Every method minted by
/// <c>Reflection.Emit</c> into a given module shares one owner — the synthetic per-module class of
/// <see cref="MethodOwner.DynamicMethodsClass"/> — so owner-plus-kind would make every dynamic
/// method in an assembly the same method. Its <see cref="DynamicMethodHandle"/> is what separates
/// them, which is why it sits in the position a metadata method's MethodDef token occupies.
/// </para>
/// </remarks>
[<RequireQualifiedAccess>]
type SynthesisedMethod =
    /// The struct-marshalling stub for this method's declaring type, as returned by
    /// <c>MarshalNative_TryGetStructMarshalStub</c>'s has-layout-non-blittable arm.
    | StructMarshalStub

    /// <summary>
    /// The placeholder frame the entry thread carries before <c>Main</c> is installed.
    /// </summary>
    /// <remarks>
    /// <para>
    /// <c>Program.buildStartupFrame</c> needs a frame on the entry thread while startup runs
    /// class initialisers, and shapes it like the entry point — same name, declaring type and
    /// signature — so that everything reading a frame sees something sensible. Its *body*,
    /// though, is a bare <c>ret</c>: `Main` has not run and must not.
    /// </para>
    /// <para>
    /// Synthesised rather than metadata-flavoured precisely because of that substitution. Its
    /// body is not the body the entry point's MethodDef row describes, so nothing keyed by that
    /// row describes this frame either — and a consumer that resolved its IL offsets against the
    /// real <c>Main</c>'s debug information would report source lines for code that has not
    /// executed. Carrying no metadata handle makes that unrepresentable rather than merely
    /// discouraged.
    /// </para>
    /// </remarks>
    | EntryPointPlaceholder

    /// <summary>
    /// A method minted at runtime by <c>Reflection.Emit</c>: CoreCLR's <c>DynamicMethodDesc</c>,
    /// the thing <c>MethodDesc::IsNoMetadata()</c> answers <c>true</c> for.
    /// </summary>
    /// <remarks>
    /// Unlike the other cases, this one is not a *behaviour* the interpreter supplies — the method
    /// has a real IL body, read off its <c>DynamicResolver</c> when it was minted. What makes it
    /// synthesised is the absence of a MethodDef row, which is precisely what this DU distinguishes.
    /// The handle is both its identity and the key to that body in
    /// <c>MethodHandleRegistry.DynamicMethods</c>.
    /// </remarks>
    | DynamicMethod of DynamicMethodHandle

[<RequireQualifiedAccess>]
module SynthesisedMethod =
    /// Whether calling this method obliges the runtime to initialise its declaring type first, as
    /// calling a declared method does.
    ///
    /// For a synthesised method the declaring type is the *subject* rather than the *owner* — the
    /// type it acts on, chosen so the method's identity is one-per-subject — so the ordinary
    /// "calling a member initialises its type" rule does not follow. Whatever initialisation a
    /// synthesised method's semantics require is part of those semantics and is
    /// discharged by its interpreter: the struct-marshal stub, for instance, runs `loadClass` on
    /// `StubHelpers.DateMarshaler` itself before calling into it.
    let initialisesDeclaringType (kind : SynthesisedMethod) : bool =
        // A total match on purpose. Answering `false` for every kind would be a fine
        // approximation today and a silent trap tomorrow: a future kind whose semantics *do*
        // require its subject initialised — a JIT-style allocation helper for a precise-init
        // type, say, where CoreCLR emits the check at the call site — would inherit the skip
        // without anyone being asked. Adding a case here breaks the build, which is where the
        // question should be put.
        match kind with
        | SynthesisedMethod.StructMarshalStub -> false
        // Never reached: the placeholder is pushed onto the entry thread directly rather than
        // called, so no call site asks. Answering `false` states the semantics anyway — startup
        // drives the entry type's initialiser as part of its own class-init sweep, and having
        // this frame demand it as a side effect of being entered would be a second, unrelated
        // reason for the same work to happen.
        | SynthesisedMethod.EntryPointPlaceholder -> false
        // There is nothing to initialise. A dynamic method's owner is the synthetic per-module
        // class, which has no metadata, no static fields and no `.cctor` — CoreCLR's
        // `CreateMinimalMethodTable` builds it precisely so that a `DynamicMethodDesc` has
        // *somewhere* to hang, not so that it can behave as a type. Invoking an LCG method runs no
        // class initialiser in CoreCLR either.
        | SynthesisedMethod.DynamicMethod _ -> false

/// The facts that exist only because a method was read from a MethodDef row.
///
/// These live apart from <see cref="MethodCore"/> so that a synthesised method cannot be asked for
/// them: reaching any of these requires matching on <see cref="MethodInfo.Metadata"/>, so a
/// consumer must say what it does when they are absent. A method the runtime supplies has no
/// honest value for any of them — a metadata token would index nothing, and attribute flags of
/// zero are a claim about a declaration that does not exist — and other code keys on all of them.
type MetadataMethodFacts =
    {
        /// <summary>
        /// The metadata token handle that uniquely identifies this method in the assembly.
        /// </summary>
        Handle : MethodDefinitionHandle

        /// <summary>
        /// The parameters of this method, as read from the metadata Param table.
        /// </summary>
        /// <remarks>
        /// <para>
        /// The Param table only carries a row per parameter that has metadata worth recording
        /// (a name, a default value, marshalling info) — <see cref="Parameter.readAll"/> also
        /// drops any row with <c>SequenceNumber = 0</c> (an unnamed "ref return" row some
        /// compilers emit). Consequently <c>Parameters.Length</c> is <b>not</b> a reliable
        /// arity: it can be short, or entirely empty, for a method whose parameters carry no
        /// metadata at all — the F# compiler emits abstract member declarations this way, with
        /// zero Param rows regardless of declared arity.
        /// </para>
        /// <para>
        /// Do not use this field, or its <c>Length</c>/<c>IsEmpty</c>, to answer "how many
        /// parameters does this method take?" or "is this method parameterless?". Use
        /// <see cref="MethodInfo.arity"/> instead, which is derived from
        /// <see cref="MethodInfo.Signature"/> and is therefore always accurate. Reserve this
        /// field for call sites that genuinely want per-parameter metadata (names, default
        /// values).
        /// </para>
        /// </remarks>
        Parameters : Parameter ImmutableArray

        /// <summary>
        /// The signature as it was read from assembly metadata.
        /// </summary>
        RawSignature : TypeMethodSignature<TypeDefn>

        /// <summary>
        /// Custom attributes defined on the method.
        /// </summary>
        CustomAttributes : WoofWare.PawPrint.CustomAttribute ImmutableArray

        MethodAttributes : MethodAttributes

        ImplAttributes : MethodImplAttributes

        NativeImport : NativeMethodImport option

        /// <summary>
        /// Does this method carry <c>[System.Runtime.InteropServices.UnmanagedCallersOnly]</c>?
        /// </summary>
        /// <remarks>
        /// Such a method may be entered only from native code: CoreCLR compiles it with
        /// <c>CORJIT_FLAG_REVERSE_PINVOKE</c>, whose prologue asserts preemptive GC mode, so any
        /// managed entry is a fatal error rather than a catchable one. Derived from
        /// <see cref="CustomAttributes"/>, and computed here so that the call path can ask without
        /// re-walking metadata; the two are built together and so cannot drift apart.
        /// </remarks>
        IsUnmanagedCallersOnly : bool
    }

/// <summary>
/// What a method belongs to.
/// </summary>
/// <remarks>
/// <para>
/// Almost always a type: a MethodDef row lives in exactly one TypeDef's method list, and a
/// synthesised method is given a declaring type too (its <em>subject</em> rather than its owner —
/// see <see cref="SynthesisedMethod"/>) precisely so that it has one.
/// </para>
/// <para>
/// The exception is a method minted by <c>Reflection.Emit</c>. CoreCLR allocates its
/// <c>DynamicMethodDesc</c> from a per-module <c>DynamicMethodTable</c> whose
/// <c>CreateMinimalMethodTable</c> class has no metadata behind it at all, so there is no TypeDef
/// row for a <see cref="ConcreteType"/> to name — which is the same absence
/// <c>RuntimeTypeHandleTarget.DynamicMethodsClass</c> models one layer up, at the reflection
/// surface. Representing it as a case here rather than fabricating an owner is what stops a
/// dynamic method's frame claiming to be declared by a type that never declared it; measured
/// against real .NET, such a frame renders as a bare <c>at Thrower(Int32)</c> with no type at all.
/// </para>
/// </remarks>
/// <remarks>
/// Custom equality, for two reasons. A declared owner compares on identity plus instantiation and
/// deliberately <em>not</em> on the whole <see cref="ConcreteType"/> record, matching what
/// <c>MethodInfo.NominallyEqual</c> compares. And <c>AssemblyName</c> is a BCL class
/// with reference equality, so the dynamic case has to compare <c>FullName</c> or two reads of the
/// same assembly's name would be different owners.
/// </remarks>
[<CustomEquality>]
[<NoComparison>]
type MethodOwner<'typeGenerics> =
    /// Declared by a type, which is every method read from metadata and every synthesised method
    /// that has a subject to be keyed on.
    | DeclaredOn of ConcreteType<'typeGenerics>

    /// Owned by the synthetic per-module class that <c>Reflection.Emit</c> methods are minted into.
    /// Carries the assembly the method is scoped to, which is the one fact that class does have —
    /// and which consumers that resolve an assembly from a method alone (
    /// <c>ThreadState.ActiveAssembly</c>, <c>ExceptionDispatching.assemblyOfMethod</c>) still need
    /// a truthful answer for.
    | DynamicMethodsClass of scopeAssemblyFullName : string

    /// The definition identity of the assembly this method belongs to. Total, and honest for both
    /// cases: a dynamic method's scope assembly is a real loaded assembly, it simply declares no type
    /// that owns the method.
    member this.AssemblyFullName : string =
        match this with
        | MethodOwner.DeclaredOn declaringType -> declaringType.AssemblyFullName
        | MethodOwner.DynamicMethodsClass scopeAssemblyFullName -> scopeAssemblyFullName

    /// The declaring type's generic arguments — empty for a dynamic method, whose owning class is
    /// non-generic (`CreateMinimalMethodTable` builds no instantiation). Total because that
    /// emptiness is a fact rather than a stand-in: there is nothing for a signature to substitute.
    member this.Generics : ImmutableArray<'typeGenerics> =
        match this with
        | MethodOwner.DeclaredOn declaringType -> declaringType.Generics
        | MethodOwner.DynamicMethodsClass _ -> ImmutableArray.Empty

    /// The declaring type, when there is one. Deliberately an option rather than a member that
    /// throws: a caller needing the type's identity, name or TypeDef handle has to say what it
    /// does without one.
    member this.TryDeclaringType : ConcreteType<'typeGenerics> option =
        match this with
        | MethodOwner.DeclaredOn declaringType -> Some declaringType
        | MethodOwner.DynamicMethodsClass _ -> None

    override this.Equals (other : obj) : bool =
        match other with
        | :? MethodOwner<'typeGenerics> as other ->
            match this, other with
            | MethodOwner.DeclaredOn left, MethodOwner.DeclaredOn right ->
                left.Identity = right.Identity && left.Generics = right.Generics
            | MethodOwner.DynamicMethodsClass left, MethodOwner.DynamicMethodsClass right ->
                // One synthetic class per module, so the assembly is the whole identity. Which
                // *method* within it is what `MethodInfo.IdentityKey` carries.
                left = right
            | MethodOwner.DeclaredOn _, MethodOwner.DynamicMethodsClass _
            | MethodOwner.DynamicMethodsClass _, MethodOwner.DeclaredOn _ -> false
        | _ -> false

    override this.GetHashCode () : int =
        // F#'s structural `hash` throughout, and *not* `HashCode.Combine`, which is the trap
        // here. The generic arguments are an `ImmutableArray`, whose own `GetHashCode` reflects
        // the identity of the backing array while F#'s `=` — which `Equals` above uses —
        // compares it elementwise. Combining them the .NET way therefore gives two equal owners
        // different hashes whenever their instantiations were built from separate arrays, which
        // is most of the time. `ConcreteType.GetHashCode` is spelled the same way for the same
        // reason.
        match this with
        | MethodOwner.DeclaredOn declaringType -> hash (0, declaringType.Identity, declaringType.Generics)
        | MethodOwner.DynamicMethodsClass scopeAssemblyFullName -> hash (1, scopeAssemblyFullName)

[<RequireQualifiedAccess>]
module MethodOwner =
    /// A human-readable name for the owner, for diagnostics. Not a metadata name: the dynamic case
    /// deliberately renders as something no type could be called, so that a message quoting it
    /// cannot be mistaken for one naming a real type.
    let describe (owner : MethodOwner<'typeGenerics>) : string =
        match owner with
        | MethodOwner.DeclaredOn declaringType -> $"%s{declaringType.Namespace}.%s{declaringType.Name}"
        | MethodOwner.DynamicMethodsClass scopeAssemblyFullName ->
            $"<dynamic methods of %s{AssemblyDefinitionName.simpleName scopeAssemblyFullName}>"

    /// The declaring type, or a failure naming the operation that needed one. For call sites which
    /// genuinely cannot proceed without a TypeDef row — metadata lookups, vtable walks, attribute
    /// reads — and which a dynamic method therefore has no business reaching.
    let requireDeclaringType (operation : string) (owner : MethodOwner<'typeGenerics>) : ConcreteType<'typeGenerics> =
        match owner with
        | MethodOwner.DeclaredOn declaringType -> declaringType
        | MethodOwner.DynamicMethodsClass scopeAssemblyFullName ->
            failwith
                $"%s{operation}: this method is owned by the dynamic-methods class of %s{AssemblyDefinitionName.simpleName scopeAssemblyFullName}, which has no TypeDef row; the operation needs a declaring type"

    let map<'a, 'b> (f : ConcreteType<'a> -> ConcreteType<'b>) (owner : MethodOwner<'a>) : MethodOwner<'b> =
        match owner with
        | MethodOwner.DeclaredOn declaringType -> MethodOwner.DeclaredOn (f declaringType)
        | MethodOwner.DynamicMethodsClass scopeAssemblyFullName -> MethodOwner.DynamicMethodsClass scopeAssemblyFullName

/// The facts every method has, however it came to exist.
type MethodCore<'typeGenerics, 'methodGenerics, 'methodVars> =
    {
        /// <summary>
        /// What declares this method: nearly always a type, but see <see cref="MethodOwner"/>.
        /// </summary>
        Owner : MethodOwner<'typeGenerics>

        /// <summary>The name of the method.</summary>
        Name : string

        /// <summary>
        /// The implementation this method carries. The CLR distinguishes IL bodies,
        /// InternalCalls, P/Invokes, runtime-synthesised methods (delegates etc.), and
        /// abstract methods; see <see cref="MethodBody"/>.
        /// </summary>
        Body : MethodBody<'methodVars>

        /// <summary>
        /// The generic type parameters defined by this method, if any.
        /// </summary>
        Generics : 'methodGenerics ImmutableArray

        /// <summary>
        /// The signature of the method, including return type and parameter types.
        /// </summary>
        Signature : TypeMethodSignature<'methodVars>

        /// <summary>
        /// Whether this method is static (true) or an instance method (false).
        /// </summary>
        IsStatic : bool
    }

/// <summary>
/// Represents detailed information about a method: either one declared by a MethodDef row, or one
/// the runtime synthesises.
/// </summary>
/// <remarks>
/// The universal facts are reachable as members (<c>Name</c>, <c>Signature</c>, …) so the great
/// majority of consumers neither know nor care which kind they hold. The metadata-only facts are
/// deliberately *not* projected: reaching them requires matching, which is what stops a
/// synthesised method being asked for a token it does not have.
/// </remarks>
type MethodInfo<'typeGenerics, 'methodGenerics, 'methodVars> =
    /// Read from a MethodDef row.
    | Metadata of core : MethodCore<'typeGenerics, 'methodGenerics, 'methodVars> * facts : MetadataMethodFacts
    /// Supplied by the runtime; see <see cref="SynthesisedMethod"/>.
    | Synthesised of core : MethodCore<'typeGenerics, 'methodGenerics, 'methodVars> * kind : SynthesisedMethod

    member this.Core : MethodCore<'typeGenerics, 'methodGenerics, 'methodVars> =
        match this with
        | MethodInfo.Metadata (core, _) -> core
        | MethodInfo.Synthesised (core, _) -> core

    member this.Owner : MethodOwner<'typeGenerics> = this.Core.Owner

    /// The definition identity of the assembly this method belongs to; see
    /// <see cref="MethodOwner.AssemblyFullName"/> for why this is total where the declaring type is not.
    member this.DeclaringAssemblyFullName : string = this.Core.Owner.AssemblyFullName

    /// The declaring type's generic arguments; see <see cref="MethodOwner.Generics"/>.
    member this.DeclaringTypeGenerics : ImmutableArray<'typeGenerics> =
        this.Core.Owner.Generics

    /// The declaring type, when this method has one.
    member this.TryDeclaringType : ConcreteType<'typeGenerics> option =
        this.Core.Owner.TryDeclaringType

    /// <summary>
    /// The declaring type, failing if this method has none.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Every call site of this is a place that cannot proceed without a TypeDef row — a metadata
    /// lookup, a vtable walk, an intrinsic classifier keyed on a type name — and which therefore
    /// has to decide what a <c>Reflection.Emit</c> method means to it. Today none of them can be
    /// reached with one, because nothing constructs
    /// <see cref="MethodOwner.DynamicMethodsClass"/> yet.
    /// </para>
    /// <para>
    /// It exists rather than making each site write <c>MethodOwner.requireDeclaringType</c> so
    /// that those sites stay a greppable set: <c>RequiredDeclaringType</c> enumerates exactly the
    /// places that must be revisited when dynamic methods become executable, and several of them
    /// will want to answer "not an intrinsic" or "no vtable slot" rather than to fail. Prefer
    /// <see cref="TryDeclaringType"/> in new code, which forces that answer to be written down.
    /// </para>
    /// </remarks>
    member this.RequiredDeclaringType : ConcreteType<'typeGenerics> =
        match this.Core.Owner with
        | MethodOwner.DeclaredOn declaringType -> declaringType
        | MethodOwner.DynamicMethodsClass scopeAssemblyFullName ->
            failwith
                $"%O{this}: this method is owned by the dynamic-methods class of %s{AssemblyDefinitionName.simpleName scopeAssemblyFullName}, which has no TypeDef row, but the operation being performed needs a declaring type"

    member this.Name : string = this.Core.Name
    member this.Body : MethodBody<'methodVars> = this.Core.Body
    member this.Generics : 'methodGenerics ImmutableArray = this.Core.Generics
    member this.Signature : TypeMethodSignature<'methodVars> = this.Core.Signature
    member this.IsStatic : bool = this.Core.IsStatic

    /// Which kind of runtime-supplied method this is, or `None` if it was declared in metadata.
    member this.SynthesisedKind : SynthesisedMethod option =
        match this with
        | MethodInfo.Metadata _ -> None
        | MethodInfo.Synthesised (_, kind) -> Some kind

    /// A hashable stand-in for "which method within its declaring type is this". Pairs the
    /// metadata token with the synthesised kind, exactly the disjunction `NominallyEqual`
    /// compares, so anything hashing a method identity stays consistent with it.
    member this.IdentityKey : MethodDefinitionHandle option * SynthesisedMethod option =
        (this.TryMetadata |> Option.map _.Handle), this.SynthesisedKind

    /// The metadata facts, when this method has any. `None` for a synthesised method.
    member this.TryMetadata : MetadataMethodFacts option =
        match this with
        | MethodInfo.Metadata (_, facts) -> Some facts
        | MethodInfo.Synthesised _ -> None

    // The four predicates below are projected deliberately, where the raw `MethodAttributes` is
    // not. The distinction is that each has a genuine answer for a synthesised method rather than
    // a fabricated one: the runtime supplies such a method directly, so it occupies no vtable
    // slot, overrides nothing, and is reachable only from the interpreter that synthesised it.
    // Handing out an attribute flags value of zero, by contrast, would be inventing metadata —
    // which is why reflection paths that report the flags to the guest still have to match.

    /// True iff this method is `virtual`. A synthesised method never is.
    member this.IsVirtual : bool =
        match this with
        | MethodInfo.Metadata (_, facts) -> facts.MethodAttributes.HasFlag MethodAttributes.Virtual
        | MethodInfo.Synthesised _ -> false

    /// True iff this method introduces a new vtable slot rather than overriding one. A synthesised
    /// method occupies no slot at all.
    member this.IsNewSlot : bool =
        match this with
        | MethodInfo.Metadata (_, facts) -> facts.MethodAttributes.HasFlag MethodAttributes.NewSlot
        | MethodInfo.Synthesised _ -> false

    /// True iff this method is sealed against further overriding. Vacuously true of a synthesised
    /// method, which is not virtual to begin with.
    member this.IsFinal : bool =
        match this with
        | MethodInfo.Metadata (_, facts) -> facts.MethodAttributes.HasFlag MethodAttributes.Final
        | MethodInfo.Synthesised _ -> true

    /// True iff naming this method at a call site leaves the body still to be chosen by the
    /// receiver's runtime type, rather than binding to this declaration outright. `callvirt` and
    /// `ldvirtftn` must agree about this, so they share this one spelling.
    ///
    /// CoreCLR's JIT importer asks the same question as
    /// `mflags & (CORINFO_FLG_FINAL | CORINFO_FLG_STATIC) || !(mflags & CORINFO_FLG_VIRTUAL)`
    /// (`importer.cpp`, `case CEE_LDVIRTFTN`), and this is that condition negated — with one
    /// deliberate omission. `CORINFO_FLG_FINAL` is set by `IsMdFinal(attribs) || pMT->IsSealed()`
    /// (`jitinterface.cpp`, `getMethodAttribsInternal`), so CoreCLR also treats a `virtual`
    /// non-`final` method *declared on a sealed type* as final; this member reads only the method's
    /// own `MethodAttributes.Final`.
    ///
    /// That omission is unobservable at a `callvirt`, which null-checks unconditionally: nothing
    /// derives from a sealed type, so the receiver's runtime type is the declaring type itself and
    /// resolving from it yields the very method the call site named. It *is* observable at an
    /// `ldvirtftn` with a null receiver, where CoreCLR takes the non-dispatching path and does not
    /// throw — so `executeLdvirtftn` detects that shape and refuses it outright rather than
    /// silently raising a NullReferenceException the real runtime would not have raised. No C#
    /// compiler can produce it (a new `virtual` member on a sealed type is CS0549, and Roslyn marks
    /// overrides in sealed types `final`), so the refusal is unreachable from the test corpus.
    member this.DispatchesVirtually : bool =
        not this.IsStatic && this.IsVirtual && not this.IsFinal

    /// The P/Invoke import this method carries, if any. `None` for a synthesised method: the
    /// runtime supplies its body directly, so there is nothing to import. Already an option for
    /// metadata-backed methods, so no information is lost by projecting it.
    member this.TryNativeImport : NativeMethodImport option =
        match this with
        | MethodInfo.Metadata (_, facts) -> facts.NativeImport
        | MethodInfo.Synthesised _ -> None

    /// True iff this method is `public`. A synthesised method is not: it has no declaration for
    /// anything to name, and nothing outside the interpreter can reach it.
    member this.IsPublic : bool =
        match this with
        | MethodInfo.Metadata (_, facts) ->
            (facts.MethodAttributes &&& MethodAttributes.MemberAccessMask) = MethodAttributes.Public
        | MethodInfo.Synthesised _ -> false

    override this.ToString () =
        match this.Owner with
        | MethodOwner.DeclaredOn declaringType ->
            let assembly = AssemblyDefinitionName.simpleName declaringType.AssemblyFullName

            $"{assembly}.{declaringType.Name}.{this.Name}"
        // No declaring type to name, and nothing to invent: real .NET renders a dynamic method's
        // frame as a bare `at Thrower(Int32)`. The assembly is still worth carrying here, because
        // this string is what `GuestLocation.describeFrame` and `GuestFailureException` show and
        // "which module was this emitted into" is the only locating fact there is.
        | MethodOwner.DynamicMethodsClass scopeAssemblyFullName ->
            $"{AssemblyDefinitionName.simpleName scopeAssemblyFullName}.<dynamic>.{this.Name}"

[<RequireQualifiedAccess>]
module MethodInfo =
    /// Rebuild a method with a different core, carrying whichever tail it already had. The
    /// generic-mapping functions below are all this: they rewrite the universal facts and leave
    /// the metadata (or the synthesised kind) exactly as it was.
    let mapCore<'a, 'b, 'c, 'd, 'e, 'f>
        (f : MethodCore<'a, 'b, 'c> -> MethodCore<'d, 'e, 'f>)
        (m : MethodInfo<'a, 'b, 'c>)
        : MethodInfo<'d, 'e, 'f>
        =
        match m with
        | MethodInfo.Metadata (core, facts) -> MethodInfo.Metadata (f core, facts)
        | MethodInfo.Synthesised (core, kind) -> MethodInfo.Synthesised (f core, kind)

    /// The metadata facts, for a caller that can only meaningfully act on a declared method —
    /// reflection, IL rendering, overload comparison. Fails for a synthesised method rather than
    /// inventing metadata for it.
    let requireMetadata
        (operation : string)
        (m : MethodInfo<'typeGenerics, 'methodGenerics, 'methodVars>)
        : MetadataMethodFacts
        =
        match m.TryMetadata with
        | Some facts -> facts
        | None -> failwith $"%s{operation}: %O{m} is synthesised by the runtime and has no metadata to read"

    /// The signature as metadata declared it, for a caller that genuinely needs the `TypeDefn`
    /// form (comparing overloads, rendering a declaration).
    let requireRawSignature
        (operation : string)
        (m : MethodInfo<'typeGenerics, 'methodGenerics, 'methodVars>)
        : TypeMethodSignature<TypeDefn>
        =
        (requireMetadata operation m).RawSignature

    /// True iff both methods are metadata-backed and share a MethodDef token — that is, they are
    /// the same *declared* method, ignoring generic instantiation and declaring-type identity.
    ///
    /// False whenever either is synthesised. A synthesised method has no declaration, so it is
    /// not the same declared method as anything, including another synthesised one; callers that
    /// want to know whether two synthesised methods are the same should use `NominallyEqual`,
    /// which compares the declaring type and the kind.
    let sameDeclaredMethod
        (a : MethodInfo<'typeGenerics, 'methodGenerics, 'methodVars>)
        (b : MethodInfo<'typeGenericsB, 'methodGenericsB, 'methodVarsB>)
        : bool
        =
        match a.TryMetadata, b.TryMetadata with
        | Some a, Some b -> a.Handle = b.Handle
        | None, _
        | _, None -> false

    let NominallyEqual
        (a : MethodInfo<'typeGenerics, 'methodGenerics, 'methodVars>)
        (b : MethodInfo<'typeGenerics, 'methodGenerics, 'methodVars>)
        : bool
        =
        // Owner equality, which for two declared methods is an identity-plus-instantiation
        // comparison — see `MethodOwner`'s custom equality, which hashing agrees with.
        // Two dynamic methods are *not*
        // distinguished here: their owning class is per-module, so both sides of a same-module
        // comparison agree, and what separates them is the `DynamicMethodHandle` in their
        // synthesised kind, compared below where a metadata method's token is.
        a.Owner = b.Owner
        && a.Generics = b.Generics
        && // Within a declaring type, a metadata method is identified by its token and a
        // synthesised one by its kind. The two are never equal: a synthesised method has no
        // MethodDef row, so nothing it could be confused with.
        (
            match a, b with
            | MethodInfo.Metadata (_, a), MethodInfo.Metadata (_, b) -> a.Handle = b.Handle
            | MethodInfo.Synthesised (_, a), MethodInfo.Synthesised (_, b) -> a = b
            | MethodInfo.Metadata _, MethodInfo.Synthesised _
            | MethodInfo.Synthesised _, MethodInfo.Metadata _ -> false
        )

    /// The true number of declared parameters (excluding `this`), independent of how many
    /// Param-table rows the metadata happened to carry. See the doc comment on
    /// <see cref="MetadataMethodFacts.Parameters"/> for why `Parameters.Length`/`IsEmpty` must not be
    /// used for this: the Param table is metadata-only and can under-count, or be entirely
    /// empty, relative to the method's real declared arity.
    let arity (m : MethodInfo<'typeGenerics, 'methodGenerics, 'methodVars>) : int = m.Signature.ParameterTypes.Length

    let private isIntrinsicAttributeType (namespaceName : string) (typeName : string) : bool =
        namespaceName = "System.Runtime.CompilerServices"
        && typeName = "IntrinsicAttribute"

    let isIntrinsicAttribute
        (getMemberRefParentType : MemberReferenceHandle -> TypeRef)
        (methodDefs : IReadOnlyDictionary<MethodDefinitionHandle, MethodInfo<'a, 'b, 'c>>)
        (attr : WoofWare.PawPrint.CustomAttribute)
        : bool
        =
        match attr.Constructor with
        | MetadataToken.MethodDef handle ->
            let constructor = methodDefs.[handle]

            // A `[Intrinsic]` application is read from a MethodDef row, so its constructor is
            // always declared by a type; a dynamic method can carry no attributes at all.
            let declaringType =
                MethodOwner.requireDeclaringType "reading an [Intrinsic] attribute application" constructor.Owner

            isIntrinsicAttributeType declaringType.Namespace declaringType.Name
            && AssemblyDefinitionName.isNamed "System.Private.CoreLib" declaringType.AssemblyFullName
        | MetadataToken.MemberReference handle ->
            let ty = getMemberRefParentType handle
            isIntrinsicAttributeType ty.Namespace ty.Name
        | con -> failwith $"TODO: {con}"

    let hasIntrinsicAttribute
        (getMemberRefParentType : MemberReferenceHandle -> TypeRef)
        (methodDefs : IReadOnlyDictionary<MethodDefinitionHandle, MethodInfo<'a, 'b, 'c>>)
        (attrs : WoofWare.PawPrint.CustomAttribute seq)
        : bool
        =
        attrs |> Seq.exists (isIntrinsicAttribute getMemberRefParentType methodDefs)

    let isJITIntrinsic
        (getMemberRefParentType : MemberReferenceHandle -> TypeRef)
        (methodDefs : IReadOnlyDictionary<MethodDefinitionHandle, MethodInfo<'a, 'b, 'c>>)
        (this : MethodInfo<'d, 'e, 'f>)
        : bool
        =
        // A synthesised method carries no custom attributes: there is no MethodDef row for one
        // to hang off. `[Intrinsic]` in particular is a metadata annotation on BCL methods, so
        // the answer for a runtime-supplied method is simply "no".
        match this with
        | MethodInfo.Synthesised _ -> false
        | MethodInfo.Metadata (_, facts) ->
            hasIntrinsicAttribute getMemberRefParentType methodDefs facts.CustomAttributes

    /// May this method be entered only from native code — that is, does it carry
    /// `[System.Runtime.InteropServices.UnmanagedCallersOnly]`?
    ///
    /// False for a synthesised method, on the same reasoning as `isJITIntrinsic`: there is no
    /// MethodDef row for an attribute to hang off, so the answer for a method the runtime supplies
    /// is simply "no".
    let isUnmanagedCallersOnly (this : MethodInfo<'a, 'b, 'c>) : bool =
        match this with
        | MethodInfo.Synthesised _ -> false
        | MethodInfo.Metadata (_, facts) -> facts.IsUnmanagedCallersOnly

    let mapTypeGenerics<'a, 'b, 'methodGen, 'vars>
        (f : 'a -> 'b)
        (m : MethodInfo<'a, 'methodGen, 'vars>)
        : MethodInfo<'b, 'methodGen, 'vars>
        =
        m
        |> mapCore (fun core ->
            {
                Owner = core.Owner |> MethodOwner.map (ConcreteType.mapGeneric (fun _ -> f))
                Name = core.Name
                Body = core.Body
                Generics = core.Generics
                Signature = core.Signature
                IsStatic = core.IsStatic
            }
        )

    let mapMethodGenerics<'a, 'b, 'vars, 'typeGen>
        (f : int -> 'a -> 'b)
        (m : MethodInfo<'typeGen, 'a, 'vars>)
        : MethodInfo<'typeGen, 'b, 'vars>
        =
        m
        |> mapCore (fun core ->
            {
                Owner = core.Owner
                Name = core.Name
                Body = core.Body
                Generics = core.Generics |> Seq.mapi f |> ImmutableArray.CreateRange
                Signature = core.Signature
                IsStatic = core.IsStatic
            }
        )

    let setMethodVars
        (body : MethodBody<'vars2>)
        (signature : TypeMethodSignature<'vars2>)
        (m : MethodInfo<'typeGen, 'methodGen, 'vars1>)
        : MethodInfo<'typeGen, 'methodGen, 'vars2>
        =
        m
        |> mapCore (fun core ->
            {
                Owner = core.Owner
                Name = core.Name
                Body = body
                Generics = core.Generics
                Signature = signature
                IsStatic = core.IsStatic
            }
        )

    /// View helper for sites that genuinely just want "the IL body if there is one,"
    /// e.g. formatters, the debugger, and the abstract-method filter. Prefer matching
    /// on <see cref="MethodInfo.Body"/> directly when the dispatch site cares which
    /// non-IL variant is present.
    let tryIlBody (m : MethodInfo<'typeGen, 'methodGen, 'methodVars>) : MethodInstructions<'methodVars> option =
        MethodBody.tryIl m.Body

    type private RawMethodBody =
        {
            Instructions : (IlOp * int) list
            LocalInit : bool
            LocalSig : ImmutableArray<TypeDefn> option
            MaxStackSize : int
            ExceptionRegions : ImmutableArray<ExceptionRegion>
        }

    let private readMethodBody
        (peReader : PEReader)
        (metadataReader : MetadataReader)
        (assembly : AssemblyName)
        (methodDef : MethodDefinition)
        : RawMethodBody option
        =
        if methodDef.RelativeVirtualAddress = 0 then
            None
        else
            let methodBody = peReader.GetMethodBody methodDef.RelativeVirtualAddress

            let localSig =
                if methodBody.LocalSignature.IsNil then
                    None
                else

                let s = methodBody.LocalSignature |> metadataReader.GetStandaloneSignature
                s.DecodeLocalSignature (TypeDefn.typeProvider assembly, ()) |> Some

            let instructions =
                IlDecoding.decodeInstructions (IlTokenUniverse.Metadata assembly) (methodBody.GetILBytes ())

            let er =
                methodBody.ExceptionRegions
                |> Seq.map ExceptionRegion.OfExceptionRegion
                |> ImmutableArray.CreateRange

            {
                Instructions = instructions
                LocalInit = methodBody.LocalVariablesInitialized
                LocalSig = localSig
                MaxStackSize = methodBody.MaxStack
                ExceptionRegions = er
            }
            |> Some

    /// <summary>
    /// Decide whether the declaring type's direct base type is <c>System.MulticastDelegate</c>,
    /// purely from metadata (no assembly resolution). Every C#-emitted delegate inherits
    /// <c>MulticastDelegate</c> directly, so this is sufficient to recognise a delegate type
    /// at <see cref="read"/> time, before <see cref="BaseClassTypes"/> is available.
    ///
    /// <c>MulticastDelegate</c> itself extends <c>Delegate</c>, and <c>Delegate</c> extends
    /// <c>Object</c>; both are correctly excluded by this check, and their <c>.ctor</c>/
    /// <c>Invoke</c> are never directly dispatched anyway.
    /// </summary>
    let private declaringTypeIsDelegate (metadataReader : MetadataReader) (declaringDefn : TypeDefinition) : bool =
        if declaringDefn.BaseType.IsNil then
            false
        else
            match MetadataToken.ofEntityHandle declaringDefn.BaseType with
            | MetadataToken.TypeReference handle ->
                let tr = metadataReader.GetTypeReference handle

                metadataReader.GetString tr.Namespace = "System"
                && metadataReader.GetString tr.Name = "MulticastDelegate"
            | MetadataToken.TypeDefinition handle ->
                let td = metadataReader.GetTypeDefinition handle

                metadataReader.GetString td.Namespace = "System"
                && metadataReader.GetString td.Name = "MulticastDelegate"
            | _ -> false

    /// <summary>
    /// Inspect the constructor token of a custom attribute and return the namespace and
    /// type name of the attribute class, if available from metadata alone (i.e. without
    /// loading another assembly). The two shapes that occur in practice are:
    /// <list type="bullet">
    /// <item><c>MemberReference</c> whose Parent is a <c>TypeReference</c> — the common
    /// case when the attribute is defined in another assembly.</item>
    /// <item><c>MethodDefinition</c> whose declaring type is a <c>TypeDefinition</c> —
    /// occurs only when the attribute is applied within the same assembly that defines
    /// it (e.g. inside <c>System.Private.CoreLib</c> for built-in attributes).</item>
    /// </list>
    /// </summary>
    let private tryReadAttributeTypeName
        (metadataReader : MetadataReader)
        (ctorToken : EntityHandle)
        : (string * string) option
        =
        if ctorToken.IsNil then
            None
        else
            match MetadataToken.ofEntityHandle ctorToken with
            | MetadataToken.MemberReference handle ->
                let memberRef = metadataReader.GetMemberReference handle

                if memberRef.Parent.IsNil then
                    None
                else
                    match MetadataToken.ofEntityHandle memberRef.Parent with
                    | MetadataToken.TypeReference parentTypeRef ->
                        let tr = metadataReader.GetTypeReference parentTypeRef

                        Some (metadataReader.GetString tr.Namespace, metadataReader.GetString tr.Name)
                    | MetadataToken.TypeDefinition parentTypeDef ->
                        let td = metadataReader.GetTypeDefinition parentTypeDef

                        Some (metadataReader.GetString td.Namespace, metadataReader.GetString td.Name)
                    | _ -> None
            | MetadataToken.MethodDef handle ->
                let methodDef = metadataReader.GetMethodDefinition handle
                let declaringType = methodDef.GetDeclaringType ()

                if declaringType.IsNil then
                    None
                else
                    let td = metadataReader.GetTypeDefinition declaringType

                    Some (metadataReader.GetString td.Namespace, metadataReader.GetString td.Name)
            | _ -> None

    /// <summary>
    /// Parse the value blob of an <c>[UnsafeAccessor]</c> custom attribute. The attribute's
    /// only constructor takes an <c>UnsafeAccessorKind</c> enum (serialised as int32), and
    /// it has one optional named property <c>Name</c> of type <c>string</c>. ECMA-335
    /// II.23.3 specifies the encoding:
    /// <list type="bullet">
    /// <item>2-byte prolog <c>0x0001</c></item>
    /// <item>4-byte int32 for the enum-typed fixed argument</item>
    /// <item>2-byte uint16 named-argument count</item>
    /// <item>For each named arg: kind byte (<c>0x53</c> field / <c>0x54</c> property),
    /// type byte (<c>0x0E</c> for string), serialised name string, serialised value</item>
    /// </list>
    /// We only recognise the <c>Name</c> property; any unexpected named arg makes us
    /// abandon parsing and treat the attribute as malformed.
    /// </summary>
    let private tryParseUnsafeAccessorBlob (reader : byref<BlobReader>) : (UnsafeAccessorKind * string option) option =
        let prolog = reader.ReadUInt16 ()

        if prolog <> 0x0001us then
            None
        else
            let kindRaw = reader.ReadInt32 ()

            let kind =
                match kindRaw with
                | 0 -> Some UnsafeAccessorKind.Constructor
                | 1 -> Some UnsafeAccessorKind.Method
                | 2 -> Some UnsafeAccessorKind.StaticMethod
                | 3 -> Some UnsafeAccessorKind.Field
                | 4 -> Some UnsafeAccessorKind.StaticField
                | _ -> None

            match kind with
            | None -> None
            | Some kind ->
                let namedCount = int (reader.ReadUInt16 ())

                let mutable parsedName : string option = None
                let mutable malformed = false
                let mutable i = 0

                while not malformed && i < namedCount do
                    let argKind = reader.ReadByte ()
                    let argType = reader.ReadByte ()

                    // 0x54 = PROPERTY, 0x0E = ELEMENT_TYPE_STRING. We only recognise
                    // a string-typed property; anything else we don't expect from
                    // [UnsafeAccessor] and refuse to guess at.
                    if argKind <> 0x54uy || argType <> 0x0Euy then
                        malformed <- true
                    else
                        let argName = reader.ReadSerializedString ()
                        let argValue = reader.ReadSerializedString ()

                        if argName = "Name" then
                            // `ReadSerializedString` returns null for the explicit-null encoding
                            // (0xFF), which is *not* the same as the property being absent.
                            // CoreCLR's `TryParseUnsafeAccessorAttribute` keys the default off the
                            // named argument's presence -- an undefined argument takes the
                            // attributed method's own name, while a supplied one is copied
                            // verbatim, and copying a null yields the empty string. So an explicit
                            // `Name = null` asks for a member called "", which no type declares.
                            // Measured on real .NET 10: `[UnsafeAccessor(Method, Name = null)]`
                            // raises `MissingMethodException`, not a call to the same-named member.
                            parsedName <- Some (if isNull argValue then "" else argValue)
                        else
                            malformed <- true

                    i <- i + 1

                if malformed then None else Some (kind, parsedName)

    /// Does any of the method's Param rows carry <c>[UnsafeAccessorType("...")]</c>?
    ///
    /// Every Param row is scanned, sequence number 0 included: that row is where a
    /// <c>[return: UnsafeAccessorType]</c> lands, and it is the row
    /// <see cref="UnsafeAccessorKind.Constructor"/> names the target type through.
    let private hasUnsafeAccessorTypeAttribute (metadataReader : MetadataReader) (methodDef : MethodDefinition) : bool =
        methodDef.GetParameters ()
        |> Seq.exists (fun paramHandle ->
            metadataReader.GetParameter(paramHandle).GetCustomAttributes ()
            |> Seq.exists (fun attrHandle ->
                let attr = metadataReader.GetCustomAttribute attrHandle

                match tryReadAttributeTypeName metadataReader attr.Constructor with
                | Some ("System.Runtime.CompilerServices", "UnsafeAccessorTypeAttribute") -> true
                | _ -> false
            )
        )

    /// <summary>
    /// Scan a method's custom attributes for <c>[UnsafeAccessor]</c> and parse the
    /// kind and (optional) target name, together with whether any parameter or the return
    /// carries <c>[UnsafeAccessorType]</c>. Returns <c>None</c> when the attribute is
    /// absent or the blob fails to match the expected shape.
    /// </summary>
    let private tryReadUnsafeAccessor
        (metadataReader : MetadataReader)
        (methodDef : MethodDefinition)
        : (UnsafeAccessorKind * string option * bool) option
        =
        let mutable result = None

        for handle in methodDef.GetCustomAttributes () do
            if result.IsNone then
                let attr = metadataReader.GetCustomAttribute handle

                match tryReadAttributeTypeName metadataReader attr.Constructor with
                | Some ("System.Runtime.CompilerServices", "UnsafeAccessorAttribute") ->
                    if not attr.Value.IsNil then
                        let mutable reader = metadataReader.GetBlobReader attr.Value
                        result <- tryParseUnsafeAccessorBlob &reader
                | _ -> ()

        result
        |> Option.map (fun (kind, targetName) ->
            kind, targetName, hasUnsafeAccessorTypeAttribute metadataReader methodDef
        )

    /// Does this method carry `[System.Runtime.InteropServices.UnmanagedCallersOnly]`?
    ///
    /// Accepted risk, inherited from `CustomAttribute.constructorParentName` and consistent with
    /// `FieldInfo`'s `[ThreadStatic]` check: the match is on namespace+name strings and does not
    /// verify that the type resolves to corelib's `UnmanagedCallersOnlyAttribute`, so a guest
    /// declaring its own attribute of that full name would be classified as carrying this one.
    let private hasUnmanagedCallersOnlyAttribute
        (metadataReader : MetadataReader)
        (describeMethod : unit -> string)
        (methodDef : MethodDefinition)
        : bool
        =
        let describeTarget () = $"method %s{describeMethod ()}"

        methodDef.GetCustomAttributes ()
        |> Seq.exists (fun handle ->
            let attr = metadataReader.GetCustomAttribute handle

            match CustomAttribute.constructorParentName metadataReader describeTarget attr.Constructor with
            | Some (ns, name) -> ns = "System.Runtime.InteropServices" && name = "UnmanagedCallersOnlyAttribute"
            | None -> false
        )

    let read
        (peReader : PEReader)
        (metadataReader : MetadataReader)
        (methodHandle : MethodDefinitionHandle)
        : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        =
        let assemblyName = metadataReader.GetAssemblyDefinition().GetAssemblyName ()
        let methodDef = metadataReader.GetMethodDefinition methodHandle
        let methodName = metadataReader.GetString methodDef.Name
        let methodSig = methodDef.DecodeSignature (TypeDefn.typeProvider assemblyName, ())
        let implAttrs = methodDef.ImplAttributes
        let methodAttrs = methodDef.Attributes

        let declaringType = methodDef.GetDeclaringType ()

        let declaringDefn = metadataReader.GetTypeDefinition (declaringType)

        let declaringTypeNamespace = metadataReader.GetString declaringDefn.Namespace

        let declaringTypeName = metadataReader.GetString declaringDefn.Name

        let body : MethodBody<TypeDefn> =
            if implAttrs.HasFlag MethodImplAttributes.InternalCall then
                MethodBody.InternalCall
            elif methodAttrs.HasFlag MethodAttributes.PinvokeImpl then
                MethodBody.PInvoke
            elif implAttrs.HasFlag MethodImplAttributes.Runtime then
                let behaviour =
                    if declaringTypeIsDelegate metadataReader declaringDefn then
                        match methodName with
                        | ".ctor" -> RuntimeBehaviour.DelegateCtor
                        | "Invoke" -> RuntimeBehaviour.DelegateInvoke
                        | _ -> RuntimeBehaviour.Unrecognised methodName
                    else
                        RuntimeBehaviour.Unrecognised methodName

                MethodBody.RuntimeProvided behaviour
            elif methodAttrs.HasFlag MethodAttributes.Abstract then
                MethodBody.Abstract
            else
                match readMethodBody peReader metadataReader assemblyName methodDef with
                | Some raw ->
                    {
                        MethodInstructions.Instructions = raw.Instructions
                        Locations = raw.Instructions |> List.map (fun (a, b) -> b, a) |> Map.ofList
                        LocalsInit = raw.LocalInit
                        LocalVars = raw.LocalSig
                        ExceptionRegions = raw.ExceptionRegions
                    }
                    |> MethodBody.Il
                | None ->
                    // ECMA-335 II.22.26 nominally requires one of PinvokeImpl / Abstract /
                    // Runtime / InternalCall when RVA = 0, but C# 12+ [UnsafeAccessor] extern
                    // static methods land here too: ImplAttributes is IL, MethodAttributes
                    // doesn't include PinvokeImpl/Abstract, and the body is synthesised by the
                    // runtime from the attribute. Recognise that case explicitly; anything else
                    // is genuinely unexpected and we fail loudly so we surface the gap rather
                    // than silently synthesising an Abstract method.
                    match tryReadUnsafeAccessor metadataReader methodDef with
                    | Some (kind, targetName, hasTypeNameOverrides) ->
                        MethodBody.RuntimeProvided (
                            RuntimeBehaviour.UnsafeAccessor (kind, targetName, hasTypeNameOverrides)
                        )
                    | None ->
                        failwith
                            $"%s{assemblyName.Name}::%s{declaringTypeNamespace}.%s{declaringTypeName}::%s{methodName}: RVA=0 but no InternalCall/PInvoke/Runtime/Abstract flag and no [UnsafeAccessor] attribute (ImplAttributes=%O{implAttrs}, MethodAttributes=%O{methodAttrs}); malformed metadata or unhandled body classification"

        let declaringTypeGenericParams =
            metadataReader.GetTypeDefinition(declaringType).GetGenericParameters ()
            |> GenericParameter.readAll assemblyName metadataReader

        let attrs =
            let result = ImmutableArray.CreateBuilder ()
            let attrs = methodDef.GetCustomAttributes ()

            for attr in attrs do
                metadataReader.GetCustomAttribute attr
                |> CustomAttribute.make metadataReader attr
                |> result.Add

            result.ToImmutable ()

        let typeSig = TypeMethodSignature.make methodSig

        let methodParams = Parameter.readAll metadataReader (methodDef.GetParameters ())

        let methodGenericParams =
            GenericParameter.readAll assemblyName metadataReader (methodDef.GetGenericParameters ())

        let nativeImport =
            if methodAttrs.HasFlag MethodAttributes.PinvokeImpl then
                let import = methodDef.GetImport ()
                let moduleRef = metadataReader.GetModuleReference import.Module

                Some
                    {
                        ModuleName = metadataReader.GetString moduleRef.Name
                        EntryPointName = metadataReader.GetString import.Name
                        Attributes = import.Attributes
                    }
            else
                None

        let isUnmanagedCallersOnly =
            hasUnmanagedCallersOnlyAttribute
                metadataReader
                (fun () -> $"%s{assemblyName.Name}!%s{declaringTypeNamespace}.%s{declaringTypeName}::%s{methodName}")
                methodDef

        let declaringType =
            ConcreteType.make
                assemblyName
                declaringType
                declaringTypeNamespace
                declaringTypeName
                declaringTypeGenericParams

        // `IsStatic` lives in the core because a synthesised method needs an answer, but for a
        // metadata method it is also derivable from `MethodAttributes.Static`. Splitting the two
        // apart made disagreement representable, so check it here at the one place both are in
        // hand rather than leaving a silently-inconsistent method to be discovered downstream.
        do
            let staticByAttribute = methodAttrs.HasFlag MethodAttributes.Static
            let staticBySignature = not methodSig.Header.IsInstance

            if staticByAttribute <> staticBySignature then
                failwith
                    $"%s{declaringTypeName}::%s{methodName} disagrees with itself about being static: MethodAttributes says %b{staticByAttribute}, the signature header says %b{staticBySignature}"

        MethodInfo.Metadata (
            {
                Owner = MethodOwner.DeclaredOn declaringType
                Name = methodName
                Body = body
                Generics = methodGenericParams
                Signature = typeSig
                IsStatic = not methodSig.Header.IsInstance
            },
            {
                Handle = methodHandle
                Parameters = methodParams
                RawSignature = typeSig
                MethodAttributes = methodAttrs
                CustomAttributes = attrs
                ImplAttributes = implAttrs
                NativeImport = nativeImport
                IsUnmanagedCallersOnly = isUnmanagedCallersOnly
            }
        )

    let rec resolveBaseType
        (methodGenerics : TypeDefn ImmutableArray option)
        (executingMethod : MethodInfo<TypeDefn, 'methodGen, 'vars>)
        (td : TypeDefn)
        : ResolvedBaseType
        =
        match td with
        | TypeDefn.Void -> failwith "Void isn't a type that appears at runtime and has no base type"
        | TypeDefn.PrimitiveType ty ->
            match ty with
            | PrimitiveType.SByte
            | PrimitiveType.Byte
            | PrimitiveType.Int16
            | PrimitiveType.UInt16
            | PrimitiveType.Int32
            | PrimitiveType.UInt32
            | PrimitiveType.Int64
            | PrimitiveType.UInt64
            | PrimitiveType.Single
            | PrimitiveType.Double
            | PrimitiveType.Char
            | PrimitiveType.Boolean -> ResolvedBaseType.ValueType
            | PrimitiveType.String -> ResolvedBaseType.Object
            | PrimitiveType.TypedReference -> failwith "todo"
            | PrimitiveType.IntPtr -> failwith "todo"
            | PrimitiveType.UIntPtr -> failwith "todo"
            | PrimitiveType.Object -> failwith "todo"
        | TypeDefn.Array (elt, rank) -> failwith "todo"
        | TypeDefn.Pinned typeDefn -> failwith "todo"
        | TypeDefn.Pointer typeDefn -> failwith "todo"
        | TypeDefn.Byref typeDefn -> failwith "todo"
        | TypeDefn.OneDimensionalArrayLowerBoundZero elements -> failwith "todo"
        | TypeDefn.Modified _ -> failwith "todo: resolveBaseType of a type carrying a custom modifier (modreq/modopt)"
        | TypeDefn.FromReference (typeRef, signatureTypeKind) -> failwith "todo"
        | TypeDefn.FromDefinition (_identity, signatureTypeKind) -> failwith "todo"
        | TypeDefn.GenericInstantiation (generic, args) -> failwith "todo"
        | TypeDefn.FunctionPointer _ -> ResolvedBaseType.ValueType
        | TypeDefn.GenericTypeParameter index ->
            resolveBaseType methodGenerics executingMethod executingMethod.DeclaringTypeGenerics.[index]
        | TypeDefn.GenericMethodParameter index ->
            match methodGenerics with
            | None -> failwith "unexpectedly asked for a generic method parameter when we had none"
            | Some generics -> resolveBaseType methodGenerics executingMethod generics.[index]
