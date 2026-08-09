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

type ExceptionRegion =
    | Filter of filterOffset : int * ExceptionOffset
    /// Token is a TypeRef, TypeDef, or TypeSpec
    | Catch of MetadataToken * ExceptionOffset
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
        | ExceptionRegionKind.Catch -> ExceptionRegion.Catch (MetadataToken.ofEntityHandle r.CatchType, offset)
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
/// <c>MethodImplAttributes.Runtime</c> (used by the CLR for delegates today; multi-dim
/// array <c>Get</c>/<c>Set</c>/<c>Address</c>/<c>.ctor</c> coming soon). The
/// <see cref="UnsafeAccessor"/> variant is different: those methods carry
/// <c>ImplAttributes=IL</c> with <c>RVA=0</c>, and the runtime synthesises the body
/// from the <c>[UnsafeAccessor]</c> attribute rather than from <c>MethodImpl.Runtime</c>.
/// </summary>
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
    /// <c>Name</c> property on the attribute; <c>None</c> means "use the attributed
    /// method's name", per the attribute's documented default.
    /// </summary>
    | UnsafeAccessor of kind : UnsafeAccessorKind * targetName : string option

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
/// methods (delegates today, multi-dim arrays soon), and abstract methods with no
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
    /// <see cref="MethodInfo.NativeImport"/> field.
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
    /// Marked <c>[MethodAttributes.Abstract]</c> — virtual without a body. Direct dispatch
    /// is illegal; reachable only via mis-resolved <c>callvirt</c>.
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
/// Represents detailed information about a method in a .NET assembly.
/// This is a strongly-typed representation of MethodDefinition from System.Reflection.Metadata.
/// </summary>
/// A method the runtime supplies rather than metadata declaring: it has no row in the MethodDef
/// table, so none of <see cref="MetadataMethodFacts"/> exists for it.
///
/// CoreCLR builds these as real <c>MethodDesc</c>s over synthesised IL — see
/// <c>PInvoke::CreateStructMarshalILStub</c> (dllimport.cpp:5289). PawPrint has no IL synthesis,
/// so the case identifies *which* runtime behaviour this is and the interpreter supplies it
/// directly.
///
/// The case carries no payload: a synthesised method's identity is its declaring type plus its
/// kind. For a struct-marshal stub the declaring type is the type being marshalled, so two stubs
/// for the same type are the same method — which is exactly the per-MethodTable identity
/// CoreCLR's stub cache has.
[<RequireQualifiedAccess>]
type SynthesisedMethod =
    /// The struct-marshalling stub for this method's declaring type, as returned by
    /// <c>MarshalNative_TryGetStructMarshalStub</c>'s has-layout-non-blittable arm.
    | StructMarshalStub

[<RequireQualifiedAccess>]
module SynthesisedMethod =
    /// Whether calling this method obliges the runtime to initialise its declaring type first, as
    /// calling a declared method does.
    ///
    /// For a synthesised method the declaring type is the *subject* rather than the *owner* — the
    /// type it acts on, chosen so the method's identity is one-per-subject — so the ordinary
    /// "calling a member initialises its type" rule does not follow. Whatever initialisation a
    /// synthesised method's semantics genuinely require is part of those semantics and is
    /// discharged by its interpreter: the struct-marshal stub, for instance, runs `loadClass` on
    /// `StubHelpers.DateMarshaler` itself before calling into it.
    ///
    /// This is a total function on purpose. Answering `false` for every kind would be a fine
    /// approximation today and a silent trap tomorrow: a future kind whose semantics *do* require
    /// its subject initialised — a JIT-style allocation helper for a precise-init type, say, where
    /// CoreCLR emits the check at the call site — would inherit the skip without anyone being
    /// asked. Adding a case here breaks the build, which is where the question should be put.
    let initialisesDeclaringType (kind : SynthesisedMethod) : bool =
        match kind with
        | SynthesisedMethod.StructMarshalStub -> false

/// The facts that exist only because a method was read from a MethodDef row.
///
/// Split out of <see cref="MethodInfo"/> so a synthesised method cannot be asked for them. Every
/// one of these was previously a field on the method itself, which meant a synthesised method had
/// to invent a plausible-looking value for each — a metadata token that indexes nothing, an empty
/// Param collection, attribute flags of zero. Those are lies in fields other code keys on, and
/// the point of the split is that the compiler now forces each consumer to say what it does when
/// they are absent.
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
        /// Custom attributes defined on the method. I've never yet seen one of these in practice.
        /// </summary>
        CustomAttributes : WoofWare.PawPrint.CustomAttribute ImmutableArray

        MethodAttributes : MethodAttributes

        ImplAttributes : MethodImplAttributes

        NativeImport : NativeMethodImport option
    }

/// The facts every method has, however it came to exist.
type MethodCore<'typeGenerics, 'methodGenerics, 'methodVars> =
    {
        /// <summary>
        /// The type that declares this method, along with its assembly information.
        /// </summary>
        DeclaringType : ConcreteType<'typeGenerics>

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

    member this.DeclaringType : ConcreteType<'typeGenerics> = this.Core.DeclaringType
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
        $"{this.DeclaringType.Assembly.Name}.{this.DeclaringType.Name}.{this.Name}"

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
    /// inventing the metadata, which is the whole point of keeping the two apart.
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
        a.DeclaringType.Identity = b.DeclaringType.Identity
        && a.DeclaringType.Generics = b.DeclaringType.Generics
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

            isIntrinsicAttributeType constructor.DeclaringType.Namespace constructor.DeclaringType.Name
            && constructor.DeclaringType.Assembly.FullName.StartsWith (
                "System.Private.CoreLib, ",
                StringComparison.Ordinal
            )
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

    let mapTypeGenerics<'a, 'b, 'methodGen, 'vars>
        (f : 'a -> 'b)
        (m : MethodInfo<'a, 'methodGen, 'vars>)
        : MethodInfo<'b, 'methodGen, 'vars>
        =
        m
        |> mapCore (fun core ->
            {
                DeclaringType = core.DeclaringType |> ConcreteType.mapGeneric (fun _ -> f)
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
                DeclaringType = core.DeclaringType
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
                DeclaringType = core.DeclaringType
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

    let private readMetadataToken (assembly : AssemblyName) (reader : byref<BlobReader>) : SourcedMetadataToken =
        reader.ReadUInt32 () |> int |> SourcedMetadataToken.ofInt assembly

    let private readStringToken (assembly : AssemblyName) (reader : byref<BlobReader>) : SourcedStringToken =
        let value = reader.ReadUInt32 () |> int
        SourcedStringToken.ofInt assembly value

    // TODO: each opcode probably ought to store how many bytes it takes, so we can advance the program counter?
    let private readOpCode (reader : byref<BlobReader>) : ILOpCode =
        let op = reader.ReadByte ()

        if op = 0xFEuy then
            let op2 = reader.ReadByte ()
            LanguagePrimitives.EnumOfValue (0xFE00us ||| (uint16 op2))
        else
            LanguagePrimitives.EnumOfValue (uint16 op)

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

            let ilBytes = methodBody.GetILBytes ()
            use bytes = fixed ilBytes
            let mutable reader : BlobReader = BlobReader (bytes, ilBytes.Length)

            let rec readInstructions acc =
                if reader.Offset >= ilBytes.Length then
                    List.rev acc
                else
                    let offset = reader.Offset
                    let opCode = readOpCode (&reader)

                    let opCode =
                        match opCode with
                        | ILOpCode.Nop -> IlOp.Nullary NullaryIlOp.Nop
                        | ILOpCode.Break -> IlOp.Nullary NullaryIlOp.Break
                        | ILOpCode.Ldarg_0 -> IlOp.Nullary NullaryIlOp.LdArg0
                        | ILOpCode.Ldarg_1 -> IlOp.Nullary NullaryIlOp.LdArg1
                        | ILOpCode.Ldarg_2 -> IlOp.Nullary NullaryIlOp.LdArg2
                        | ILOpCode.Ldarg_3 -> IlOp.Nullary NullaryIlOp.LdArg3
                        | ILOpCode.Ldloc_0 -> IlOp.Nullary NullaryIlOp.Ldloc_0
                        | ILOpCode.Ldloc_1 -> IlOp.Nullary NullaryIlOp.Ldloc_1
                        | ILOpCode.Ldloc_2 -> IlOp.Nullary NullaryIlOp.Ldloc_2
                        | ILOpCode.Ldloc_3 -> IlOp.Nullary NullaryIlOp.Ldloc_3
                        | ILOpCode.Stloc_0 -> IlOp.Nullary NullaryIlOp.Stloc_0
                        | ILOpCode.Stloc_1 -> IlOp.Nullary NullaryIlOp.Stloc_1
                        | ILOpCode.Stloc_2 -> IlOp.Nullary NullaryIlOp.Stloc_2
                        | ILOpCode.Stloc_3 -> IlOp.Nullary NullaryIlOp.Stloc_3
                        | ILOpCode.Ldarg_s -> IlOp.UnaryConst (UnaryConstIlOp.Ldarg_s (reader.ReadByte ()))
                        | ILOpCode.Ldarga_s -> IlOp.UnaryConst (UnaryConstIlOp.Ldarga_s (reader.ReadByte ()))
                        | ILOpCode.Starg_s -> IlOp.UnaryConst (UnaryConstIlOp.Starg_s (reader.ReadByte ()))
                        | ILOpCode.Ldloc_s -> IlOp.UnaryConst (UnaryConstIlOp.Ldloc_s (reader.ReadByte ()))
                        | ILOpCode.Ldloca_s -> IlOp.UnaryConst (UnaryConstIlOp.Ldloca_s (reader.ReadByte ()))
                        | ILOpCode.Stloc_s -> IlOp.UnaryConst (UnaryConstIlOp.Stloc_s (reader.ReadSByte ()))
                        | ILOpCode.Ldnull -> IlOp.Nullary NullaryIlOp.LdNull
                        | ILOpCode.Ldc_i4_m1 -> IlOp.Nullary NullaryIlOp.LdcI4_m1
                        | ILOpCode.Ldc_i4_0 -> IlOp.Nullary NullaryIlOp.LdcI4_0
                        | ILOpCode.Ldc_i4_1 -> IlOp.Nullary NullaryIlOp.LdcI4_1
                        | ILOpCode.Ldc_i4_2 -> IlOp.Nullary NullaryIlOp.LdcI4_2
                        | ILOpCode.Ldc_i4_3 -> IlOp.Nullary NullaryIlOp.LdcI4_3
                        | ILOpCode.Ldc_i4_4 -> IlOp.Nullary NullaryIlOp.LdcI4_4
                        | ILOpCode.Ldc_i4_5 -> IlOp.Nullary NullaryIlOp.LdcI4_5
                        | ILOpCode.Ldc_i4_6 -> IlOp.Nullary NullaryIlOp.LdcI4_6
                        | ILOpCode.Ldc_i4_7 -> IlOp.Nullary NullaryIlOp.LdcI4_7
                        | ILOpCode.Ldc_i4_8 -> IlOp.Nullary NullaryIlOp.LdcI4_8
                        | ILOpCode.Ldc_i4_s -> IlOp.UnaryConst (UnaryConstIlOp.Ldc_I4_s (reader.ReadSByte ()))
                        | ILOpCode.Ldc_i4 -> IlOp.UnaryConst (UnaryConstIlOp.Ldc_I4 (reader.ReadInt32 ()))
                        | ILOpCode.Ldc_i8 -> IlOp.UnaryConst (UnaryConstIlOp.Ldc_I8 (reader.ReadInt64 ()))
                        | ILOpCode.Ldc_r4 -> IlOp.UnaryConst (UnaryConstIlOp.Ldc_R4 (reader.ReadSingle ()))
                        | ILOpCode.Ldc_r8 -> IlOp.UnaryConst (UnaryConstIlOp.Ldc_R8 (reader.ReadDouble ()))
                        | ILOpCode.Dup -> IlOp.Nullary NullaryIlOp.Dup
                        | ILOpCode.Pop -> IlOp.Nullary NullaryIlOp.Pop
                        | ILOpCode.Jmp ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Jmp, readMetadataToken assembly &reader)
                        | ILOpCode.Call ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Call, readMetadataToken assembly &reader)
                        | ILOpCode.Calli ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Calli, readMetadataToken assembly &reader)
                        | ILOpCode.Ret -> IlOp.Nullary NullaryIlOp.Ret
                        | ILOpCode.Br_s -> IlOp.UnaryConst (UnaryConstIlOp.Br_s (reader.ReadSByte ()))
                        | ILOpCode.Brfalse_s -> IlOp.UnaryConst (UnaryConstIlOp.Brfalse_s (reader.ReadSByte ()))
                        | ILOpCode.Brtrue_s -> IlOp.UnaryConst (UnaryConstIlOp.Brtrue_s (reader.ReadSByte ()))
                        | ILOpCode.Beq_s -> IlOp.UnaryConst (UnaryConstIlOp.Beq_s (reader.ReadSByte ()))
                        | ILOpCode.Bge_s -> IlOp.UnaryConst (UnaryConstIlOp.Bge_s (reader.ReadSByte ()))
                        | ILOpCode.Bgt_s -> IlOp.UnaryConst (UnaryConstIlOp.Bgt_s (reader.ReadSByte ()))
                        | ILOpCode.Ble_s -> IlOp.UnaryConst (UnaryConstIlOp.Ble_s (reader.ReadSByte ()))
                        | ILOpCode.Blt_s -> IlOp.UnaryConst (UnaryConstIlOp.Blt_s (reader.ReadSByte ()))
                        | ILOpCode.Bne_un_s -> IlOp.UnaryConst (UnaryConstIlOp.Bne_un_s (reader.ReadSByte ()))
                        | ILOpCode.Bge_un_s -> IlOp.UnaryConst (UnaryConstIlOp.Bge_un_s (reader.ReadSByte ()))
                        | ILOpCode.Bgt_un_s -> IlOp.UnaryConst (UnaryConstIlOp.Bgt_un_s (reader.ReadSByte ()))
                        | ILOpCode.Ble_un_s -> IlOp.UnaryConst (UnaryConstIlOp.Ble_un_s (reader.ReadSByte ()))
                        | ILOpCode.Blt_un_s -> IlOp.UnaryConst (UnaryConstIlOp.Blt_un_s (reader.ReadSByte ()))
                        | ILOpCode.Br -> IlOp.UnaryConst (UnaryConstIlOp.Br (reader.ReadInt32 ()))
                        | ILOpCode.Brfalse -> IlOp.UnaryConst (UnaryConstIlOp.Brfalse (reader.ReadInt32 ()))
                        | ILOpCode.Brtrue -> IlOp.UnaryConst (UnaryConstIlOp.Brtrue (reader.ReadInt32 ()))
                        | ILOpCode.Beq -> IlOp.UnaryConst (UnaryConstIlOp.Beq (reader.ReadInt32 ()))
                        | ILOpCode.Bge -> IlOp.UnaryConst (UnaryConstIlOp.Bge (reader.ReadInt32 ()))
                        | ILOpCode.Bgt -> IlOp.UnaryConst (UnaryConstIlOp.Bgt (reader.ReadInt32 ()))
                        | ILOpCode.Ble -> IlOp.UnaryConst (UnaryConstIlOp.Ble (reader.ReadInt32 ()))
                        | ILOpCode.Blt -> IlOp.UnaryConst (UnaryConstIlOp.Blt (reader.ReadInt32 ()))
                        | ILOpCode.Bne_un -> IlOp.UnaryConst (UnaryConstIlOp.Bne_un (reader.ReadInt32 ()))
                        | ILOpCode.Bge_un -> IlOp.UnaryConst (UnaryConstIlOp.Bge_un (reader.ReadInt32 ()))
                        | ILOpCode.Bgt_un -> IlOp.UnaryConst (UnaryConstIlOp.Bgt_un (reader.ReadInt32 ()))
                        | ILOpCode.Ble_un -> IlOp.UnaryConst (UnaryConstIlOp.Ble_un (reader.ReadInt32 ()))
                        | ILOpCode.Blt_un -> IlOp.UnaryConst (UnaryConstIlOp.Blt_un (reader.ReadInt32 ()))
                        | ILOpCode.Switch ->
                            let count = reader.ReadUInt32 ()

                            if count > uint32 System.Int32.MaxValue then
                                failwith "Debugger error: can't create a jump table with more than int32.Max entries"

                            let count = int count
                            let result = ImmutableArray.CreateBuilder count

                            for i = 0 to count - 1 do
                                result.Add (reader.ReadInt32 ())

                            IlOp.Switch (result.ToImmutable ())
                        | ILOpCode.Ldind_i -> IlOp.Nullary NullaryIlOp.Ldind_i
                        | ILOpCode.Ldind_i1 -> IlOp.Nullary NullaryIlOp.Ldind_i1
                        | ILOpCode.Ldind_u1 -> IlOp.Nullary NullaryIlOp.Ldind_u1
                        | ILOpCode.Ldind_i2 -> IlOp.Nullary NullaryIlOp.Ldind_i2
                        | ILOpCode.Ldind_u2 -> IlOp.Nullary NullaryIlOp.Ldind_u2
                        | ILOpCode.Ldind_i4 -> IlOp.Nullary NullaryIlOp.Ldind_i4
                        | ILOpCode.Ldind_u4 -> IlOp.Nullary NullaryIlOp.Ldind_u4
                        | ILOpCode.Ldind_i8 -> IlOp.Nullary NullaryIlOp.Ldind_i8
                        | ILOpCode.Ldind_r4 -> IlOp.Nullary NullaryIlOp.Ldind_r4
                        | ILOpCode.Ldind_r8 -> IlOp.Nullary NullaryIlOp.Ldind_r8
                        | ILOpCode.Ldind_ref -> IlOp.Nullary NullaryIlOp.Ldind_ref
                        | ILOpCode.Stind_ref -> IlOp.Nullary NullaryIlOp.Stind_ref
                        | ILOpCode.Stind_i1 -> IlOp.Nullary NullaryIlOp.Stind_I1
                        | ILOpCode.Stind_i2 -> IlOp.Nullary NullaryIlOp.Stind_I2
                        | ILOpCode.Stind_i4 -> IlOp.Nullary NullaryIlOp.Stind_I4
                        | ILOpCode.Stind_i8 -> IlOp.Nullary NullaryIlOp.Stind_I8
                        | ILOpCode.Stind_r4 -> IlOp.Nullary NullaryIlOp.Stind_R4
                        | ILOpCode.Stind_r8 -> IlOp.Nullary NullaryIlOp.Stind_R8
                        | ILOpCode.Add -> IlOp.Nullary NullaryIlOp.Add
                        | ILOpCode.Sub -> IlOp.Nullary NullaryIlOp.Sub
                        | ILOpCode.Mul -> IlOp.Nullary NullaryIlOp.Mul
                        | ILOpCode.Div -> IlOp.Nullary NullaryIlOp.Div
                        | ILOpCode.Div_un -> IlOp.Nullary NullaryIlOp.Div_un
                        | ILOpCode.Rem -> IlOp.Nullary NullaryIlOp.Rem
                        | ILOpCode.Rem_un -> IlOp.Nullary NullaryIlOp.Rem_un
                        | ILOpCode.And -> IlOp.Nullary NullaryIlOp.And
                        | ILOpCode.Or -> IlOp.Nullary NullaryIlOp.Or
                        | ILOpCode.Xor -> IlOp.Nullary NullaryIlOp.Xor
                        | ILOpCode.Shl -> IlOp.Nullary NullaryIlOp.Shl
                        | ILOpCode.Shr -> IlOp.Nullary NullaryIlOp.Shr
                        | ILOpCode.Shr_un -> IlOp.Nullary NullaryIlOp.Shr_un
                        | ILOpCode.Neg -> IlOp.Nullary NullaryIlOp.Neg
                        | ILOpCode.Not -> IlOp.Nullary NullaryIlOp.Not
                        | ILOpCode.Conv_i1 -> IlOp.Nullary NullaryIlOp.Conv_I1
                        | ILOpCode.Conv_i2 -> IlOp.Nullary NullaryIlOp.Conv_I2
                        | ILOpCode.Conv_i4 -> IlOp.Nullary NullaryIlOp.Conv_I4
                        | ILOpCode.Conv_i8 -> IlOp.Nullary NullaryIlOp.Conv_I8
                        | ILOpCode.Conv_r4 -> IlOp.Nullary NullaryIlOp.Conv_R4
                        | ILOpCode.Conv_r8 -> IlOp.Nullary NullaryIlOp.Conv_R8
                        | ILOpCode.Conv_u4 -> IlOp.Nullary NullaryIlOp.Conv_U4
                        | ILOpCode.Conv_u8 -> IlOp.Nullary NullaryIlOp.Conv_U8
                        | ILOpCode.Callvirt ->
                            IlOp.UnaryMetadataToken (
                                UnaryMetadataTokenIlOp.Callvirt,
                                readMetadataToken assembly &reader
                            )
                        | ILOpCode.Cpobj ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Cpobj, readMetadataToken assembly &reader)
                        | ILOpCode.Ldobj ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldobj, readMetadataToken assembly &reader)
                        | ILOpCode.Ldstr ->
                            IlOp.UnaryStringToken (UnaryStringTokenIlOp.Ldstr, readStringToken assembly &reader)
                        | ILOpCode.Newobj ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Newobj, readMetadataToken assembly &reader)
                        | ILOpCode.Castclass ->
                            IlOp.UnaryMetadataToken (
                                UnaryMetadataTokenIlOp.Castclass,
                                readMetadataToken assembly &reader
                            )
                        | ILOpCode.Isinst ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Isinst, readMetadataToken assembly &reader)
                        | ILOpCode.Conv_r_un -> IlOp.Nullary NullaryIlOp.Conv_r_un
                        | ILOpCode.Unbox ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Unbox, readMetadataToken assembly &reader)
                        | ILOpCode.Throw -> IlOp.Nullary NullaryIlOp.Throw
                        | ILOpCode.Ldfld ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldfld, readMetadataToken assembly &reader)
                        | ILOpCode.Ldflda ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldflda, readMetadataToken assembly &reader)
                        | ILOpCode.Stfld ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Stfld, readMetadataToken assembly &reader)
                        | ILOpCode.Ldsfld ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldsfld, readMetadataToken assembly &reader)
                        | ILOpCode.Ldsflda ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldsflda, readMetadataToken assembly &reader)
                        | ILOpCode.Stsfld ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Stsfld, readMetadataToken assembly &reader)
                        | ILOpCode.Stobj ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Stobj, readMetadataToken assembly &reader)
                        | ILOpCode.Conv_ovf_i_un -> IlOp.Nullary NullaryIlOp.Conv_ovf_i_un
                        | ILOpCode.Conv_ovf_i1_un -> IlOp.Nullary NullaryIlOp.Conv_ovf_i1_un
                        | ILOpCode.Conv_ovf_i2_un -> IlOp.Nullary NullaryIlOp.Conv_ovf_i2_un
                        | ILOpCode.Conv_ovf_i4_un -> IlOp.Nullary NullaryIlOp.Conv_ovf_i4_un
                        | ILOpCode.Conv_ovf_i8_un -> IlOp.Nullary NullaryIlOp.Conv_ovf_i8_un
                        | ILOpCode.Conv_ovf_u_un -> IlOp.Nullary NullaryIlOp.Conv_ovf_u_un
                        | ILOpCode.Conv_ovf_u1_un -> IlOp.Nullary NullaryIlOp.Conv_ovf_u1_un
                        | ILOpCode.Conv_ovf_u2_un -> IlOp.Nullary NullaryIlOp.Conv_ovf_u2_un
                        | ILOpCode.Conv_ovf_u4_un -> IlOp.Nullary NullaryIlOp.Conv_ovf_u4_un
                        | ILOpCode.Conv_ovf_u8_un -> IlOp.Nullary NullaryIlOp.Conv_ovf_u8_un
                        | ILOpCode.Box ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Box, readMetadataToken assembly &reader)
                        | ILOpCode.Newarr ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Newarr, readMetadataToken assembly &reader)
                        | ILOpCode.Ldlen -> IlOp.Nullary NullaryIlOp.LdLen
                        | ILOpCode.Ldelema ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldelema, readMetadataToken assembly &reader)
                        | ILOpCode.Ldelem_i1 -> IlOp.Nullary NullaryIlOp.Ldelem_i1
                        | ILOpCode.Ldelem_u1 -> IlOp.Nullary NullaryIlOp.Ldelem_u1
                        | ILOpCode.Ldelem_i2 -> IlOp.Nullary NullaryIlOp.Ldelem_i2
                        | ILOpCode.Ldelem_u2 -> IlOp.Nullary NullaryIlOp.Ldelem_u2
                        | ILOpCode.Ldelem_i4 -> IlOp.Nullary NullaryIlOp.Ldelem_i4
                        | ILOpCode.Ldelem_u4 -> IlOp.Nullary NullaryIlOp.Ldelem_u4
                        | ILOpCode.Ldelem_i8 -> IlOp.Nullary NullaryIlOp.Ldelem_i8
                        | ILOpCode.Ldelem_i -> IlOp.Nullary NullaryIlOp.Ldelem_i
                        | ILOpCode.Ldelem_r4 -> IlOp.Nullary NullaryIlOp.Ldelem_r4
                        | ILOpCode.Ldelem_r8 -> IlOp.Nullary NullaryIlOp.Ldelem_r8
                        | ILOpCode.Ldelem_ref -> IlOp.Nullary NullaryIlOp.Ldelem_ref
                        | ILOpCode.Stelem_i -> IlOp.Nullary NullaryIlOp.Stelem_i
                        | ILOpCode.Stelem_i1 -> IlOp.Nullary NullaryIlOp.Stelem_i1
                        | ILOpCode.Stelem_i2 -> IlOp.Nullary NullaryIlOp.Stelem_i2
                        | ILOpCode.Stelem_i4 -> IlOp.Nullary NullaryIlOp.Stelem_i4
                        | ILOpCode.Stelem_i8 -> IlOp.Nullary NullaryIlOp.Stelem_i8
                        | ILOpCode.Stelem_r4 -> IlOp.Nullary NullaryIlOp.Stelem_r4
                        | ILOpCode.Stelem_r8 -> IlOp.Nullary NullaryIlOp.Stelem_r8
                        | ILOpCode.Stelem_ref -> IlOp.Nullary NullaryIlOp.Stelem_ref
                        | ILOpCode.Ldelem ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldelem, readMetadataToken assembly &reader)
                        | ILOpCode.Stelem ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Stelem, readMetadataToken assembly &reader)
                        | ILOpCode.Unbox_any ->
                            IlOp.UnaryMetadataToken (
                                UnaryMetadataTokenIlOp.Unbox_Any,
                                readMetadataToken assembly &reader
                            )
                        | ILOpCode.Conv_ovf_i1 -> IlOp.Nullary NullaryIlOp.Conv_ovf_i1
                        | ILOpCode.Conv_ovf_u1 -> IlOp.Nullary NullaryIlOp.Conv_ovf_u1
                        | ILOpCode.Conv_ovf_i2 -> IlOp.Nullary NullaryIlOp.Conv_ovf_i2
                        | ILOpCode.Conv_ovf_u2 -> IlOp.Nullary NullaryIlOp.Conv_ovf_u2
                        | ILOpCode.Conv_ovf_i4 -> IlOp.Nullary NullaryIlOp.Conv_ovf_i4
                        | ILOpCode.Conv_ovf_u4 -> IlOp.Nullary NullaryIlOp.Conv_ovf_u4
                        | ILOpCode.Conv_ovf_i8 -> IlOp.Nullary NullaryIlOp.Conv_ovf_i8
                        | ILOpCode.Conv_ovf_u8 -> IlOp.Nullary NullaryIlOp.Conv_ovf_u8
                        | ILOpCode.Refanyval ->
                            IlOp.UnaryMetadataToken (
                                UnaryMetadataTokenIlOp.Refanyval,
                                readMetadataToken assembly &reader
                            )
                        | ILOpCode.Ckfinite -> IlOp.Nullary NullaryIlOp.Ckfinite
                        | ILOpCode.Mkrefany ->
                            IlOp.UnaryMetadataToken (
                                UnaryMetadataTokenIlOp.Mkrefany,
                                readMetadataToken assembly &reader
                            )
                        | ILOpCode.Ldtoken ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldtoken, readMetadataToken assembly &reader)
                        | ILOpCode.Conv_u2 -> IlOp.Nullary NullaryIlOp.Conv_U2
                        | ILOpCode.Conv_u1 -> IlOp.Nullary NullaryIlOp.Conv_U1
                        | ILOpCode.Conv_i -> IlOp.Nullary NullaryIlOp.Conv_I
                        | ILOpCode.Conv_ovf_i -> IlOp.Nullary NullaryIlOp.Conv_ovf_i
                        | ILOpCode.Conv_ovf_u -> IlOp.Nullary NullaryIlOp.Conv_ovf_u
                        | ILOpCode.Add_ovf -> IlOp.Nullary NullaryIlOp.Add_ovf
                        | ILOpCode.Add_ovf_un -> IlOp.Nullary NullaryIlOp.Add_ovf_un
                        | ILOpCode.Mul_ovf -> IlOp.Nullary NullaryIlOp.Mul_ovf
                        | ILOpCode.Mul_ovf_un -> IlOp.Nullary NullaryIlOp.Mul_ovf_un
                        | ILOpCode.Sub_ovf -> IlOp.Nullary NullaryIlOp.Sub_ovf
                        | ILOpCode.Sub_ovf_un -> IlOp.Nullary NullaryIlOp.Sub_ovf_un
                        | ILOpCode.Endfinally -> IlOp.Nullary NullaryIlOp.Endfinally
                        | ILOpCode.Leave -> IlOp.UnaryConst (UnaryConstIlOp.Leave (reader.ReadInt32 ()))
                        | ILOpCode.Leave_s -> IlOp.UnaryConst (UnaryConstIlOp.Leave_s (reader.ReadSByte ()))
                        | ILOpCode.Stind_i -> IlOp.Nullary NullaryIlOp.Stind_I
                        | ILOpCode.Conv_u -> IlOp.Nullary NullaryIlOp.Conv_U
                        | ILOpCode.Arglist -> IlOp.Nullary NullaryIlOp.Arglist
                        | ILOpCode.Ceq -> IlOp.Nullary NullaryIlOp.Ceq
                        | ILOpCode.Cgt -> IlOp.Nullary NullaryIlOp.Cgt
                        | ILOpCode.Cgt_un -> IlOp.Nullary NullaryIlOp.Cgt_un
                        | ILOpCode.Clt -> IlOp.Nullary NullaryIlOp.Clt
                        | ILOpCode.Clt_un -> IlOp.Nullary NullaryIlOp.Clt_un
                        | ILOpCode.Ldftn ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldftn, readMetadataToken assembly &reader)
                        | ILOpCode.Ldvirtftn ->
                            IlOp.UnaryMetadataToken (
                                UnaryMetadataTokenIlOp.Ldvirtftn,
                                readMetadataToken assembly &reader
                            )
                        | ILOpCode.Ldarg -> IlOp.UnaryConst (UnaryConstIlOp.Ldarg (reader.ReadUInt16 ()))
                        | ILOpCode.Ldarga -> IlOp.UnaryConst (UnaryConstIlOp.Ldarga (reader.ReadUInt16 ()))
                        | ILOpCode.Starg -> IlOp.UnaryConst (UnaryConstIlOp.Starg (reader.ReadUInt16 ()))
                        | ILOpCode.Ldloc -> IlOp.UnaryConst (UnaryConstIlOp.Ldloc (reader.ReadUInt16 ()))
                        | ILOpCode.Ldloca -> IlOp.UnaryConst (UnaryConstIlOp.Ldloca (reader.ReadUInt16 ()))
                        | ILOpCode.Stloc -> IlOp.UnaryConst (UnaryConstIlOp.Stloc (reader.ReadUInt16 ()))
                        | ILOpCode.Localloc -> IlOp.Nullary NullaryIlOp.Localloc
                        | ILOpCode.Endfilter -> IlOp.Nullary NullaryIlOp.Endfilter
                        | ILOpCode.Unaligned -> IlOp.UnaryConst (UnaryConstIlOp.Unaligned (reader.ReadByte ()))
                        | ILOpCode.Volatile -> IlOp.Nullary NullaryIlOp.Volatile
                        | ILOpCode.Tail -> IlOp.Nullary NullaryIlOp.Tail
                        | ILOpCode.Initobj ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Initobj, readMetadataToken assembly &reader)
                        | ILOpCode.Constrained ->
                            IlOp.UnaryMetadataToken (
                                UnaryMetadataTokenIlOp.Constrained,
                                readMetadataToken assembly &reader
                            )
                        | ILOpCode.Cpblk -> IlOp.Nullary NullaryIlOp.Cpblk
                        | ILOpCode.Initblk -> IlOp.Nullary NullaryIlOp.Initblk
                        | ILOpCode.Rethrow -> IlOp.Nullary NullaryIlOp.Rethrow
                        | ILOpCode.Sizeof ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Sizeof, readMetadataToken assembly &reader)
                        | ILOpCode.Refanytype -> IlOp.Nullary NullaryIlOp.Refanytype
                        | ILOpCode.Readonly -> IlOp.Nullary NullaryIlOp.Readonly
                        | i -> failwithf "Unknown opcode: %A" i

                    readInstructions ((opCode, offset) :: acc)

            let instructions = readInstructions []

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

                let mutable parsedName = None
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
                            // ReadSerializedString returns null for the explicit-null
                            // encoding (0xFF); treat that the same as "Name not set".
                            parsedName <- if isNull argValue then Some None else Some (Some argValue)
                        else
                            malformed <- true

                    i <- i + 1

                if malformed then
                    None
                else
                    let name =
                        match parsedName with
                        | Some n -> n
                        | None -> None

                    Some (kind, name)

    /// <summary>
    /// Scan a method's custom attributes for <c>[UnsafeAccessor]</c> and parse the
    /// kind and (optional) target name. Returns <c>None</c> when the attribute is
    /// absent or the blob fails to match the expected shape.
    /// </summary>
    let private tryReadUnsafeAccessor
        (metadataReader : MetadataReader)
        (methodDef : MethodDefinition)
        : (UnsafeAccessorKind * string option) option
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
                    | Some (kind, targetName) ->
                        MethodBody.RuntimeProvided (RuntimeBehaviour.UnsafeAccessor (kind, targetName))
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

        let typeSig =
            TypeMethodSignature.make
                (function
                | TypeDefn.Void -> MethodReturnType.Void
                | retType -> MethodReturnType.Returns retType)
                methodSig

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
                DeclaringType = declaringType
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
            resolveBaseType methodGenerics executingMethod executingMethod.DeclaringType.Generics.[index]
        | TypeDefn.GenericMethodParameter index ->
            match methodGenerics with
            | None -> failwith "unexpectedly asked for a generic method parameter when we had none"
            | Some generics -> resolveBaseType methodGenerics executingMethod generics.[index]
