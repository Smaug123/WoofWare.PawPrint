namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

/// What the body CoreCLR synthesises for an `[UnsafeAccessor]` declaration does, once the target
/// member it names has been resolved.
///
/// CoreCLR emits IL (`vm/unsafeaccessors.cpp`, `GenerateAccessor`): the stub arguments, then one
/// instruction. PawPrint has no IL synthesis, so that one instruction is data here and
/// `UnsafeAccessorDispatch.execute` is its interpreter -- the same shape `StructMarshalPlan` takes
/// for the struct-marshalling stub.
[<RequireQualifiedAccess>]
type internal UnsafeAccessorPlan =
    /// `newobj`, over every declared argument. The handle is the type being constructed.
    | Construct of
        ctor : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> *
        targetType : ConcreteTypeHandle

    /// `newobj` of one of an array type's constructors, over every declared argument. An array's
    /// constructors have no body, so this allocates rather than calling anything. The constructor's
    /// array type is the one the stub constructs, which need not be the accessor's return type.
    | ConstructArray of ctor : ArrayConstructor

    /// `callvirt`, over every declared argument including the first (which is the receiver).
    | CallInstance of target : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>

    /// `call`, over every declared argument *except* the first, whose only job was to name the type.
    | CallStatic of target : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>

    /// `ldarg.0; ldflda`.
    | InstanceFieldAddress of
        field : WoofWare.PawPrint.FieldInfo<GenericParamFromMetadata, TypeDefn> *
        declaringType : ConcreteTypeHandle

    /// `ldsflda`. The first declared argument is not read at all.
    | StaticFieldAddress of
        field : WoofWare.PawPrint.FieldInfo<GenericParamFromMetadata, TypeDefn> *
        declaringType : ConcreteTypeHandle *
        typeGenerics : ImmutableArray<ConcreteTypeHandle>

/// Why an `[UnsafeAccessor]` declaration could not be honoured, in the vocabulary of the exception
/// CoreCLR raises for it. Every one of these is raised *into the guest*: real .NET raises them from
/// the accessor's first invocation, before the accessor's declaring type is initialised, where the
/// guest's own `try`/`catch` can see them; CoreLib's own accessors are written expecting exactly
/// that.
///
/// These are the whole of what dispatch answers with an exception. A declaration whose answer on
/// CoreCLR is some other exception, or whose answer PawPrint cannot decide, is refused with a
/// `failwith` naming the shape rather than approximated: see the TODOs in `resolve`.
[<RequireQualifiedAccess>]
type internal UnsafeAccessorRefusal =
    /// `COR_E_BADIMAGEFORMAT`, which CoreCLR raises for a declaration whose signature cannot
    /// describe an accessor at all (`BFA_INVALID_UNSAFEACCESSOR`), and for a `StaticMethod` whose
    /// body would be a `call` to an abstract method, which is not valid IL. The message is the
    /// one CoreCLR's own text carries; guests do not assert on it.
    | BadImageFormat of message : string

    /// `MemberLoader::ThrowMissingMethodException`. Carries the target type and member name, which
    /// is what CoreCLR's message names.
    | MissingMethod of targetType : string * name : string

    /// `MemberLoader::ThrowMissingFieldException`.
    | MissingField of targetType : string * name : string

    /// `AmbiguousMatchException` (`Arg_AmbiguousMatchException_UnsafeAccessor`), which the lookup
    /// raises when more than one of the target type's methods matches the declaration.
    | AmbiguousMatch

    /// `InvalidProgramException` (`Argument_GenTypeConstraintsNotEqual`), which
    /// `VerifyDeclarationSatisfiesTargetConstraints` raises once the lookup has found a method of a
    /// generic type, unless the accessor's own declaring type has exactly as many type parameters:
    /// those are what the target type's parameters are checked against, so an accessor on a
    /// non-generic type has nothing to supply.
    | GenericTypeConstraintsNotEqual

    /// `VerificationException` (`IDS_EE_METHOD_CONSTRAINTS_VIOLATION`), which CoreCLR raises while
    /// instantiating a generic target method with a type argument that one of its type parameters
    /// refuses. Each field is spelled as `TypeString::AppendType` spells it in that message: the
    /// target type, the target method's name, the offending type argument, and the name of the
    /// *target's* type parameter it violates.
    | MethodConstraintsViolation of
        targetType : string *
        methodName : string *
        typeArgument : string *
        typeParameter : string

    /// `InvalidOperationException` (`InvalidOperation_CantInstantiateAbstractClass`), which the JIT
    /// raises compiling a `Constructor` accessor's `newobj` of an abstract class.
    | CantInstantiateAbstractClass

[<RequireQualifiedAccess>]
module internal UnsafeAccessorDispatch =

    [<Literal>]
    let private invalidUsageMessage = "Invalid usage of UnsafeAccessorAttribute."

    /// The name of the member to look for: the attribute's `Name` property, or the accessor's own
    /// name when it is absent, per `UnsafeAccessorAttribute`'s documented default. A constructor's
    /// name is the runtime's to choose, and `resolve` has already refused a declaration that
    /// supplied one.
    let private targetMemberName
        (kind : UnsafeAccessorKind)
        (targetName : string option)
        (accessorName : string)
        : string
        =
        match kind with
        | UnsafeAccessorKind.Constructor -> ".ctor"
        | UnsafeAccessorKind.Method
        | UnsafeAccessorKind.StaticMethod
        | UnsafeAccessorKind.Field
        | UnsafeAccessorKind.StaticField ->
            // Truncated at the first NUL, because CoreCLR hands the attribute's UTF-8 buffer
            // to `strcmp`. `Name = "M\0suffix"` is valid C#, and measured on real .NET 10 it binds
            // the member called `M`.
            let name = targetName |> Option.defaultValue accessorName

            match name.IndexOf '\000' with
            | -1 -> name
            | i -> name.Substring (0, i)

    /// Whether the member the accessor reaches is a static one, which is what decides both the
    /// candidate filter and whether the declaration's first argument is passed on or merely read
    /// for its type.
    let private isTargetStatic (kind : UnsafeAccessorKind) : bool =
        match kind with
        | UnsafeAccessorKind.StaticMethod
        | UnsafeAccessorKind.StaticField -> true
        | UnsafeAccessorKind.Constructor
        | UnsafeAccessorKind.Method
        | UnsafeAccessorKind.Field -> false

    /// Is the *outermost* element of this signature position a generic parameter?
    ///
    /// `ValidateTargetType` refuses `ELEMENT_TYPE_VAR`/`ELEMENT_TYPE_MVAR` where the target type is
    /// read from, and it asks that of the element type it peeks off the blob -- so a `ref T` is a
    /// BYREF and passes, and only a bare `T` is refused. Measured on real .NET 10:
    /// `[UnsafeAccessor(Field)] static extern ref int X<T>(ref T target)` reaches a struct `T`'s
    /// field, while the same accessor over a bare `T` raises `BadImageFormatException`.
    ///
    /// This is a question about the *blob*: the concretized handle has already had the variable
    /// substituted away. Custom modifiers are peeled because `PeekElemType` skips them.
    let rec private namesGenericParameter (ty : TypeDefn) : bool =
        match ty with
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _ -> true
        | TypeDefn.Modified m -> namesGenericParameter m.Unmodified
        | _ -> false

    /// Is this signature position a `ref` to a generic parameter? See `resolve` for why that
    /// shape is refused when the parameter is instantiated with a reference type.
    let rec private namesByrefToGenericParameter (ty : TypeDefn) : bool =
        match ty with
        | TypeDefn.Modified m -> namesByrefToGenericParameter m.Unmodified
        | TypeDefn.Byref inner -> namesGenericParameter inner
        | _ -> false

    /// Is this a byref, once custom modifiers are peeled off? The `ref` return a field accessor
    /// must declare, and the `ref` receiver an instance member of a value type must take.
    let rec private isByref (ty : TypeDefn) : bool =
        match ty with
        | TypeDefn.Byref _ -> true
        | TypeDefn.Modified m -> isByref m.Unmodified
        | _ -> false

    /// The type a `ref T` return addresses: what a field accessor's declared return type is
    /// compared against. `None` when the return is not a byref, which the caller has already
    /// refused.
    let rec private byrefElement (ty : TypeDefn) : TypeDefn option =
        match ty with
        | TypeDefn.Byref inner -> Some inner
        | TypeDefn.Modified m -> byrefElement m.Unmodified
        | _ -> None

    /// The type this signature element describes with every `modreq`/`modopt` deleted, at every
    /// depth.
    ///
    /// CoreCLR compares an accessor's signature with `MetaSig::CompareState.IgnoreCustomModifiers`
    /// set: always for a field (`TrySetTargetField` asserts it, unsafeaccessors.cpp:750) and on the
    /// first pass for a method (`TrySetTargetMethod`, :587). `CompareElementType` then consumes
    /// modifiers wherever it meets them rather than only at the top, so the strip has to recurse.
    /// Measured against real .NET 10: an accessor spelling `ref int` binds a `private volatile int`
    /// field, whose signature is `int32 modreq(IsVolatile)`, and one spelling a `void` return binds
    /// an `init` accessor, whose return is `void modreq(IsExternalInit)`.
    ///
    /// `TypeDefn.stripCustomModifiers` is deliberately shallow -- it answers "what type does this
    /// element name", for which a modifier on an array's element is not in the way -- so it is not
    /// this.
    let rec stripModifiersDeep (ty : TypeDefn) : TypeDefn =
        match ty with
        | TypeDefn.Modified m -> stripModifiersDeep m.Unmodified
        | TypeDefn.Array (element, shape) -> TypeDefn.Array (stripModifiersDeep element, shape)
        | TypeDefn.Pinned element -> TypeDefn.Pinned (stripModifiersDeep element)
        | TypeDefn.Pointer element -> TypeDefn.Pointer (stripModifiersDeep element)
        | TypeDefn.Byref element -> TypeDefn.Byref (stripModifiersDeep element)
        | TypeDefn.OneDimensionalArrayLowerBoundZero element ->
            TypeDefn.OneDimensionalArrayLowerBoundZero (stripModifiersDeep element)
        | TypeDefn.GenericInstantiation (generic, args) ->
            TypeDefn.GenericInstantiation (stripModifiersDeep generic, args |> ImmutableArray.map stripModifiersDeep)
        | TypeDefn.FunctionPointer signature -> TypeDefn.FunctionPointer (stripSignatureModifiersDeep signature)
        | TypeDefn.PrimitiveType _
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _
        | TypeDefn.FromDefinition _
        | TypeDefn.FromReference _
        | TypeDefn.Void -> ty

    /// A return column with its custom modifiers deleted.
    ///
    /// The `Returns Void` fold is not cosmetic. A return spelled `void` *under* a custom modifier
    /// decodes as `Returns TypeDefn.Void` rather than `Void` -- `TypeMethodSignature.make` keeps
    /// the blob's spelling -- and every C# `init` accessor is spelled exactly that way, as
    /// `void modreq(IsExternalInit)`. Deleting the modifier without folding would leave a
    /// `Returns Void` that no accessor's own `void` return could ever equal, so an `init` setter
    /// would be unreachable.
    and stripReturnModifiersDeep (returnType : MethodReturnType<TypeDefn>) : MethodReturnType<TypeDefn> =
        match returnType with
        | MethodReturnType.Void -> MethodReturnType.Void
        | MethodReturnType.Returns ty ->
            match stripModifiersDeep ty with
            | TypeDefn.Void -> MethodReturnType.Void
            | stripped -> MethodReturnType.Returns stripped

    and stripSignatureModifiersDeep (signature : TypeMethodSignature<TypeDefn>) : TypeMethodSignature<TypeDefn> =
        { signature with
            ParameterTypes = signature.ParameterTypes |> List.map stripModifiersDeep
            ReturnType = stripReturnModifiersDeep signature.ReturnType
        }

    /// A method signature holding exactly the types to be compared, spelled with the header the
    /// comparison should use. `compareSignatureTypes` compares `Header` and `GenericParameterCount`
    /// as CoreCLR compares the leading bytes of a blob, so both are part of the comparand rather
    /// than checked separately.
    let private comparandSignature
        (header : ComparableSignatureHeader)
        (genericParameterCount : int)
        (returnType : MethodReturnType<TypeDefn>)
        (parameterTypes : TypeDefn list)
        : TypeMethodSignature<TypeDefn>
        =
        {
            Header = header
            ParameterTypes = parameterTypes
            GenericParameterCount = genericParameterCount
            // Accessors are never vararg -- `resolve` refuses one -- so there is no
            // sentinel and every parameter is required.
            RequiredParameterCount = List.length parameterTypes
            ReturnType = returnType
        }

    /// The header to compare a candidate's signature under: the *declaration's* calling convention
    /// and generic flag, but the *candidate's* `this` bits.
    ///
    /// CoreCLR compares `callConvDecl & IMAGE_CEE_CS_CALLCONV_MASK` against the candidate's
    /// (unsafeaccessors.cpp:421), and that mask covers neither `HASTHIS` nor `EXPLICITTHIS` nor
    /// `GENERIC`: an accessor is always static while the target it reaches usually is not, so
    /// comparing those bits would reject every instance member. `GENERIC` *is* compared, a few
    /// lines further down, together with the generic-parameter count -- which is what leaving that
    /// bit as the declaration spells it reproduces.
    let private comparisonHeader
        (declaration : ComparableSignatureHeader)
        (candidate : ComparableSignatureHeader)
        : ComparableSignatureHeader
        =
        let thisBits =
            candidate.Get.Attributes
            &&& (SignatureAttributes.Instance ||| SignatureAttributes.ExplicitThis)

        let genericBit =
            if declaration.Get.IsGeneric then
                SignatureAttributes.Generic
            else
                SignatureAttributes.None

        SignatureHeader (SignatureKind.Method, declaration.Get.CallingConvention, thisBits ||| genericBit)
        |> ComparableSignatureHeader.Make

    /// The header both sides of a *field* comparison are read under. CoreCLR compares one signature
    /// element -- the declaration's `ref` return against the field's type (unsafeaccessors.cpp:704)
    /// -- so neither blob's leading bytes take part, and expressing that comparison as a
    /// one-parameter method signature means picking a header that cannot itself distinguish them.
    let private fieldComparisonHeader : ComparableSignatureHeader =
        SignatureHeader (SignatureKind.Method, SignatureCallingConvention.Default, SignatureAttributes.None)
        |> ComparableSignatureHeader.Make

    /// A name for the target type in the messages CoreCLR builds -- `ThrowMissingMethodException`
    /// and `ThrowMissingFieldException` name it the same way.
    ///
    /// Namespace-qualified but not nesting-qualified: measured on real .NET 10, a missing member of
    /// `Outer.Inner.Namespaced` reports `'Outer.Inner.Namespaced.NoSuch'`, while one of a type
    /// nested in a namespace-less class reports the bare nested name. A nested type's metadata
    /// `Namespace` is empty, so taking the namespace when there is one gives both.
    let private describeTargetType (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>) : string =
        if System.String.IsNullOrEmpty typeInfo.Namespace then
            typeInfo.Name
        else
            $"%s{typeInfo.Namespace}.%s{typeInfo.Name}"

    /// One type variable a signature element mentions.
    [<RequireQualifiedAccess>]
    type private MentionedParameter =
        /// `!index`, a variable of the type that declares the signature.
        | OfType of index : int
        /// `!!index`, a variable of the method whose signature it is.
        | OfMethod of index : int

    /// The type variables this signature element mentions, at any depth.
    let rec private mentionedParameters (ty : TypeDefn) : Set<MentionedParameter> =
        match ty with
        | TypeDefn.GenericTypeParameter index -> Set.singleton (MentionedParameter.OfType index)
        | TypeDefn.GenericMethodParameter index -> Set.singleton (MentionedParameter.OfMethod index)
        | TypeDefn.Modified m -> mentionedParameters m.Unmodified
        | TypeDefn.Array (element, _)
        | TypeDefn.Pinned element
        | TypeDefn.Pointer element
        | TypeDefn.Byref element
        | TypeDefn.OneDimensionalArrayLowerBoundZero element -> mentionedParameters element
        | TypeDefn.GenericInstantiation (generic, args) ->
            args
            |> Seq.map mentionedParameters
            |> Set.unionMany
            |> Set.union (mentionedParameters generic)
        | TypeDefn.FunctionPointer signature ->
            let returned =
                match signature.ReturnType with
                | MethodReturnType.Void -> Set.empty
                | MethodReturnType.Returns ret -> mentionedParameters ret

            signature.ParameterTypes
            |> Seq.map mentionedParameters
            |> Set.unionMany
            |> Set.union returned
        | TypeDefn.PrimitiveType _
        | TypeDefn.FromDefinition _
        | TypeDefn.FromReference _
        | TypeDefn.Void -> Set.empty

    /// Does `ClassLoader::CanonicalizeGenericArg` (generics.cpp:27) replace this type argument with
    /// `System.__Canon`? It does for every reference type -- a class, an interface, `string`, an
    /// array -- and for no value type, not even a generic struct instantiated over a reference
    /// type, which keeps its own definition.
    let private isReplacedByCanon
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (describe : string)
        (argument : ConcreteTypeHandle)
        : bool
        =
        match argument with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ -> true
        | ConcreteTypeHandle.Concrete _ ->
            match AllConcreteTypes.tryTypeInfo state._LoadedAssemblies state.ConcreteTypes argument with
            | Some (_, typeInfo) -> not (DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeInfo)
            | None -> failwith $"BUG: %s{describe}: concrete type argument %O{argument} has no TypeDef row"
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ ->
            failwith
                $"TODO: %s{describe} is instantiated with %O{argument}, which is not a valid type argument; CoreCLR refuses the instantiation when it loads it, before any accessor runs"

    /// Does the declaration name a method of this signature, in the sense of
    /// `DoesMethodMatchUnsafeAccessorDeclaration` (unsafeaccessors.cpp:388) with every custom
    /// modifier ignored? `candidateAssemblyFullName` is the assembly the candidate's signature is
    /// read in.
    let private declarationMatches
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (kind : UnsafeAccessorKind)
        (accessorAssemblyFullName : string)
        (declarationSignature : TypeMethodSignature<TypeDefn>)
        (candidateAssemblyFullName : string)
        (candidateSignature : TypeMethodSignature<TypeDefn>)
        (state : IlMachineState)
        : IlMachineState * bool
        =
        let isConstructor =
            match kind with
            | UnsafeAccessorKind.Constructor -> true
            | _ -> false

        // The declaration carries one extra argument that the target does not: the one naming the
        // target type, which for an instance member is also the receiver. A constructor
        // declaration has no such argument -- it names its type through the return -- so its
        // argument list is compared whole.
        let declarationParameters =
            if isConstructor then
                declarationSignature.ParameterTypes
            else
                match declarationSignature.ParameterTypes with
                | [] ->
                    failwith
                        "BUG: an [UnsafeAccessor] lookup reached a non-constructor accessor with no parameters; `resolve` refuses that as BadImageFormat"
                | _ :: rest -> rest

        let candidateSignature = stripSignatureModifiersDeep candidateSignature

        // A constructor candidate must return void; CoreCLR checks that in place of comparing the
        // return column (unsafeaccessors.cpp:481), which is skipped for this kind. No C# compiler
        // emits a non-void `.ctor` and no guest here reaches this arm, but the check is what makes
        // skipping the return column safe on an image that does.
        let returnAcceptable =
            if not isConstructor then
                true
            else
                match candidateSignature.ReturnType with
                | MethodReturnType.Void -> true
                | MethodReturnType.Returns _ -> false

        if not returnAcceptable then
            state, false
        else

        let header = comparisonHeader declarationSignature.Header candidateSignature.Header

        // Both sides' type variables are left standing rather than substituted: CoreCLR compares
        // these blobs with no substitution on either side (`pSubst1 = pSubst2 = NULL`,
        // unsafeaccessors.cpp:401/408), so a target spelling `!0` matches only a declaration
        // spelling `!0` -- never one spelling the type that instantiates it, and never the
        // accessor's own `!!0`. Measured against real .NET 10: a non-generic accessor over `C<int>`
        // does *not* find `C<T>::M(T)`, and one declared on `A<T>` taking `T` does.
        let declarationComparand : TypeConcretization.UnsubstitutedComparand =
            {
                Signature =
                    comparandSignature
                        header
                        declarationSignature.GenericParameterCount
                        (stripReturnModifiersDeep declarationSignature.ReturnType)
                        (declarationParameters |> List.map stripModifiersDeep)
                AssemblyFullName = accessorAssemblyFullName
            }

        let candidateComparand : TypeConcretization.UnsubstitutedComparand =
            {
                Signature = candidateSignature
                AssemblyFullName = candidateAssemblyFullName
            }

        IlMachineTypeResolution.signaturesEquivalentWithoutSubstitution
            loggerFactory
            baseClassTypes
            state
            isConstructor // the return column is compared for every other kind
            declarationComparand
            candidateComparand

    /// Find the one declared method on the target type that the declaration names, in the sense of
    /// `TrySetTargetMethod` (unsafeaccessors.cpp:584): the type's *own* methods only -- no
    /// base-class walk -- filtered by name and static-ness, then matched signature against
    /// signature.
    let private findTargetMethod
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (describe : string)
        (kind : UnsafeAccessorKind)
        (name : string)
        (accessorAssemblyFullName : string)
        (declarationSignature : TypeMethodSignature<TypeDefn>)
        (targetTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (state : IlMachineState)
        : IlMachineState *
          Result<
              WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>,
              UnsafeAccessorRefusal
           >
        =
        let candidates =
            targetTypeInfo.Methods
            |> List.filter (fun candidate -> candidate.Name = name && candidate.IsStatic = isTargetStatic kind)

        let state, matching =
            ((state, []), candidates)
            ||> List.fold (fun (state, acc) candidate ->
                let state, matches =
                    declarationMatches
                        loggerFactory
                        baseClassTypes
                        kind
                        accessorAssemblyFullName
                        declarationSignature
                        targetTypeInfo.AssemblyFullName
                        (MethodInfo.requireRawSignature "[UnsafeAccessor] target lookup" candidate)
                        state

                if matches then state, candidate :: acc else state, acc
            )

        match matching with
        | [ single ] ->
            // A value type's *virtual* method is two candidates to CoreCLR, not one: it generates
            // an unboxing stub beside every such method, and `IntroducedMethodIterator` yields
            // both. They are the same declaration, so no comparison separates them and the
            // modifier retry cannot either -- the lookup is simply ambiguous. Measured on real .NET
            // 10: an accessor over `ref S` naming either an `override ToString` or an implicitly
            // implemented interface method raises `AmbiguousMatchException`, while a non-virtual
            // instance method and a static one bind.
            //
            // This is `MethodTableBuilder::NeedsTightlyBoundUnboxingStub`, which also exempts a
            // generic method (`mcInstantiated`) and an `RTSpecialName` one: measured, a struct's
            // generic interface implementation binds rather than being ambiguous.
            let hasUnboxingStub =
                DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies targetTypeInfo
                && not single.IsStatic
                && single.IsVirtual
                && single.Generics.IsEmpty
                && not (
                    single.TryMetadata
                    |> Option.exists (fun facts -> facts.MethodAttributes.HasFlag MethodAttributes.RTSpecialName)
                )

            if hasUnboxingStub then
                state, Error UnsafeAccessorRefusal.AmbiguousMatch
            else
                state, Ok single
        | [] -> state, Error (UnsafeAccessorRefusal.MissingMethod (describeTargetType targetTypeInfo, name))
        | _ :: _ :: _ ->
            // ECMA-335 II.22.26 makes (name, signature) unique within a type, and a custom modifier
            // is part of the signature -- so two survivors of a modifier-blind comparison differ in
            // their modifiers and nothing else. That is exactly where CoreCLR retries the search
            // requiring modifiers to match the declaration exactly, and reports
            // `AmbiguousMatchException` only if the retry does not settle on one
            // (unsafeaccessors.cpp:625-637). Reproducing the retry needs a comparison that carries
            // an ignore-modifiers *flag* rather than one over stripped signatures, which is what
            // `stripSignatureModifiersDeep` gives; without it, "ambiguous" and "the retry would
            // have picked one" are indistinguishable here, so neither answer can be given.
            failwith
                $"TODO: %s{describe} found %d{List.length matching} declared methods named %s{name} on %s{describeTargetType targetTypeInfo} matching the declaration's signature once custom modifiers are ignored; CoreCLR would retry requiring them to match exactly (unsafeaccessors.cpp:625-637), which needs a comparison this does not have"

    /// Find the one declared field on the target type that the declaration names, in the sense of
    /// `TrySetTargetField` (unsafeaccessors.cpp:723): the type's own fields only, filtered by name
    /// and static-ness, with the declaration's `ref` return compared against the field's type.
    let private findTargetField
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (kind : UnsafeAccessorKind)
        (name : string)
        (accessorAssemblyFullName : string)
        (returnedType : TypeDefn)
        (targetTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (state : IlMachineState)
        : IlMachineState *
          Result<WoofWare.PawPrint.FieldInfo<GenericParamFromMetadata, TypeDefn>, UnsafeAccessorRefusal>
        =
        let candidates =
            targetTypeInfo.Fields
            |> List.filter (fun candidate ->
                candidate.Name = name
                && candidate.IsStatic = isTargetStatic kind
                // A literal (`const`) has a Field row and no storage: its value lives in the
                // Constant table and every read of it was folded away at compile time. CoreCLR's
                // `ApproxFieldDescIterator` walks `FieldDesc`s rather than metadata rows, and a
                // literal has none, so it is simply not a candidate. Measured on real .NET 10: an
                // accessor naming a `private const int` gets `MissingFieldException`. Admitting it
                // here would be worse than a wrong answer -- `staticFieldAddress` would mint a
                // zero-initialised slot and hand the guest a writable byref to a field that has no
                // storage at all.
                && not (candidate.Attributes.HasFlag FieldAttributes.Literal)
            )

        let state, matching =
            ((state, []), candidates)
            ||> List.fold (fun (state, acc) candidate ->
                // With no substitution on either side, as for a method (unsafeaccessors.cpp:660
                // and :667).
                let declarationComparand : TypeConcretization.UnsubstitutedComparand =
                    {
                        Signature =
                            comparandSignature
                                fieldComparisonHeader
                                0
                                MethodReturnType.Void
                                [ stripModifiersDeep returnedType ]
                        AssemblyFullName = accessorAssemblyFullName
                    }

                let candidateComparand : TypeConcretization.UnsubstitutedComparand =
                    {
                        Signature =
                            comparandSignature
                                fieldComparisonHeader
                                0
                                MethodReturnType.Void
                                [ stripModifiersDeep candidate.Signature ]
                        AssemblyFullName = targetTypeInfo.AssemblyFullName
                    }

                let state, matches =
                    IlMachineTypeResolution.signaturesEquivalentWithoutSubstitution
                        loggerFactory
                        baseClassTypes
                        state
                        true // the two types under comparison are both in the parameter list
                        declarationComparand
                        candidateComparand

                if matches then state, candidate :: acc else state, acc
            )
            |> fun (state, matching) -> state, List.rev matching

        // The *first* match in metadata order wins, and there is no ambiguity check:
        // `TrySetTargetField` returns as soon as one matches (unsafeaccessors.cpp:761), unlike
        // `TrySetTargetMethod`, which keeps looking so that it can report an ambiguity. Two fields
        // of one type may share a name if their signatures differ, and a modifier-blind comparison
        // can leave both -- no C# compiler emits that, but a metadata writer may.
        match matching with
        | first :: _ -> state, Ok first
        | [] -> state, Error (UnsafeAccessorRefusal.MissingField (describeTargetType targetTypeInfo, name))

    /// Whether CoreCLR shares code over `System.__Canon` for an instantiation with this type
    /// argument: `ClassLoader::CanonicalizeGenericArg` (generics.cpp:27) replaces a reference type,
    /// arrays included, by `__Canon`, and a value type by its canonical MethodTable. So a value type
    /// is shared exactly when one of its own type arguments is -- `ValueTuple<string>` shares with
    /// `ValueTuple<object>` -- and a non-generic one never is.
    ///
    /// A byref, pointer or function pointer is not a valid type argument, and CoreCLR asserts that
    /// none reaches the canonicalisation; one is refused rather than classified.
    let rec private isSharedTypeArgument
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (describe : string)
        (argument : ConcreteTypeHandle)
        : bool
        =
        match argument with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ -> true
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ ->
            failwith
                $"TODO: %s{describe} is instantiated with %s{AllConcreteTypes.describe state._LoadedAssemblies state.ConcreteTypes argument}, which is not a valid type argument; PawPrint does not model how CoreCLR refuses it"
        | ConcreteTypeHandle.Concrete _ ->

        match AllConcreteTypes.tryTypeInfo state._LoadedAssemblies state.ConcreteTypes argument with
        | None ->
            failwith $"BUG: %s{describe} is instantiated with the handle %O{argument}, which names no registered type"
        | Some (concrete, typeInfo) ->
            if DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeInfo then
                concrete.Generics
                |> Seq.exists (isSharedTypeArgument baseClassTypes state describe)
            else
                true

    /// The constraint checks CoreCLR makes once the lookup has found a target method, for the shapes
    /// whose answer this dispatcher can state: `VerifyDeclarationSatisfiesTargetConstraints`
    /// (unsafeaccessors.cpp:513), which compares the declaration's *typical* instantiation with the
    /// target's as part of the lookup, and then `MethodDesc::SatisfiesMethodConstraints`
    /// (genmeth.cpp:1594), which checks the accessor's actual type arguments as the stub
    /// instantiates the target.
    ///
    /// A target method of a generic type fails the first check outright unless the accessor's
    /// declaring type has exactly as many type parameters as the target type: that check sets each
    /// of the accessor type's own variables against the target type's parameter at the same index,
    /// whatever types the accessor's signature instantiates the target type with. A type parameter
    /// of either the target type or the target method that names a constraint is refused, because
    /// deciding either check for it needs `TypeVarTypeDesc::SatisfiesConstraints`'s assignability
    /// walk, which PawPrint does not have. What is left is the one constraint every type parameter
    /// carries: the *absence* of `allows ref struct` refuses a byref-like argument
    /// (typedesc.cpp:1606) -- a refusal only the second check can make, since the declaration's own
    /// type variables are not byref-like.
    let private checkTargetConstraints
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (describe : string)
        (accessorTypeParameterCount : int)
        (targetTypeHandle : ConcreteTypeHandle)
        (targetTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (accessorMethodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (target : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        : Result<unit, UnsafeAccessorRefusal>
        =
        let constrained (metadata : GenericParamMetadata) : bool =
            metadata.Constraint.IsSome
            || metadata.RequiresParameterlessConstructor
            || not metadata.Constraints.IsEmpty

        // A non-generic target type has no parameters to check, whatever the accessor's type has
        // (unsafeaccessors.cpp:537, where only a generic target type supplies instantiations).
        if
            not targetTypeInfo.Generics.IsEmpty
            && targetTypeInfo.Generics.Length <> accessorTypeParameterCount
        then
            Error UnsafeAccessorRefusal.GenericTypeConstraintsNotEqual
        else

        // The argument each of these is checked against is the accessor type's own variable, and a
        // variable satisfies an unconstrained parameter without being examined at all
        // (typedesc.cpp:1530-1562): not even `allows ref struct` is asked of it, that being asked only
        // of a type that is not a variable (:1612).
        for parameter, metadata in targetTypeInfo.Generics do
            if constrained metadata then
                failwith
                    $"TODO: %s{describe} names %s{target.Name} of a generic type whose type parameter %s{parameter.Name} carries a constraint; deciding whether the accessor type's own type parameter satisfies it needs the walk of CoreCLR's TypeVarTypeDesc::SatisfiesConstraints, which PawPrint does not have"

        if target.Generics.Length <> accessorMethodGenerics.Length then
            // Unreachable after a successful signature match, which compares the generic-parameter
            // counts as CoreCLR compares the blobs' leading bytes; stated rather than assumed.
            failwith
                $"BUG: %s{describe} matched %s{target.Name} with %d{target.Generics.Length} generic parameters against %d{accessorMethodGenerics.Length} type arguments"

        // Every parameter is screened before any argument is checked, because the lookup's check
        // over all of them precedes the instantiation's check over any one of them.
        for parameter, metadata in target.Generics do
            if constrained metadata then
                failwith
                    $"TODO: %s{describe} names the generic method %s{target.Name}, whose type parameter %s{parameter.Name} carries a constraint; deciding whether the accessor's own type argument satisfies it needs the assignability walk of CoreCLR's TypeVarTypeDesc::SatisfiesConstraints, which PawPrint does not have"

        let isByRefLike (argument : ConcreteTypeHandle) : bool =
            match AllConcreteTypes.tryTypeInfo state._LoadedAssemblies state.ConcreteTypes argument with
            | Some (_, typeInfo) -> DumpedAssembly.isByRefLike baseClassTypes state._LoadedAssemblies typeInfo
            | None ->
                // A structural handle: a byref, pointer, array or function pointer. None of those
                // is a byref-like *type* -- `Span<T>` is nominal -- so the anti-constraint does not
                // bear on them.
                false

        // The first violated parameter in declaration order is the one reported.
        let firstViolation =
            Seq.zip target.Generics accessorMethodGenerics
            |> Seq.tryFind (fun ((_, metadata), argument) -> isByRefLike argument && not metadata.AllowsByRefLike)

        match firstViolation with
        | None -> Ok ()
        | Some ((parameter, _), argument) ->
            let render (handle : ConcreteTypeHandle) : string =
                NativeRuntimeTypeHelpers.runtimeTypeHandleName
                    describe
                    state
                    NativeRuntimeTypeHelpers.formatNamespaceFlag
                    (RuntimeTypeHandleTarget.Closed handle)

            UnsafeAccessorRefusal.MethodConstraintsViolation (
                render targetTypeHandle,
                target.Name,
                render argument,
                parameter.Name
            )
            |> Error

    /// The constructor a stub's `newobj` runs when an accessor binds `ctor`, which is not always one
    /// of `ctor`'s own array type.
    ///
    /// `Module::CreateArrayMethodTable` (array.cpp:229) gives an array whose element is a reference
    /// type other than a szarray no methods of its own: it shares those of `object`'s array of the
    /// same kind and rank. The stub names its target by MethodDesc, so what it constructs is that
    /// `object` array. Measured on real .NET 10: an accessor returning `string[]` builds an
    /// `object[]`, as do ones over an interface, a delegate, a generic class or a `string[,]`
    /// element, while `string[][]` and `int[,]` are built exactly. A `newobj` token names the exact
    /// array type instead, and the JIT allocates that.
    let private constructedByStub
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (describe : string)
        (ctor : ArrayConstructor)
        : ArrayConstructor
        =
        let element =
            match ctor with
            | ArrayConstructor.SzArray (element, _)
            | ArrayConstructor.MultiDim (element, _, _) -> element

        let sharesObjectArrayMethods =
            match element with
            | ConcreteTypeHandle.OneDimArrayZero _ -> false
            | ConcreteTypeHandle.Array _ -> true
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ -> false
            | ConcreteTypeHandle.Byref _ -> failwith $"BUG: %s{describe} bound a constructor of an array of byrefs"
            | ConcreteTypeHandle.Concrete _ ->
                match AllConcreteTypes.tryTypeInfo state._LoadedAssemblies state.ConcreteTypes element with
                | Some (_, typeInfo) -> not (DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeInfo)
                | None -> failwith $"BUG: %s{describe}: array element type %O{element} has no TypeDef row"

        if not sharesObjectArrayMethods then
            ctor
        else

        let objectHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Object

        match ctor with
        | ArrayConstructor.SzArray (_, depth) -> ArrayConstructor.SzArray (objectHandle, depth)
        | ArrayConstructor.MultiDim (_, rank, lowerBounds) ->
            ArrayConstructor.MultiDim (objectHandle, rank, lowerBounds)

    /// Read an `[UnsafeAccessor]` declaration and resolve the member it names, reproducing
    /// `MethodDesc::TryGenerateUnsafeAccessor` (unsafeaccessors.cpp:1027) down to the point where
    /// CoreCLR would emit IL.
    ///
    /// Runs on every invocation, where CoreCLR resolves once as it JITs the stub. That is only a
    /// cost: resolution reads metadata and registers concrete types, both idempotent, and a
    /// declaration that cannot be resolved raises on every call in CoreCLR too, since every call
    /// re-enters the prestub.
    let resolve
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (kind : UnsafeAccessorKind)
        (targetName : string option)
        (hasTypeNameOverrides : bool)
        (accessor : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (state : IlMachineState)
        : IlMachineState * Result<UnsafeAccessorPlan, UnsafeAccessorRefusal>
        =
        let describe =
            $"[UnsafeAccessor] %s{MethodOwner.describe accessor.Owner}::%s{accessor.Name}"

        // `UnsafeAccessor` must be on a static method (unsafeaccessors.cpp:1046), and CoreCLR asks
        // that before it parses anything: an instance accessor that is *also* an unsupported shape
        // is a `BadImageFormatException` the guest can catch, not a refusal.
        if not accessor.IsStatic then
            state, Error (UnsafeAccessorRefusal.BadImageFormat invalidUsageMessage)
        else

        if hasTypeNameOverrides then
            failwith
                $"TODO: %s{describe} names at least one of its types with [UnsafeAccessorType], which gives the type as an assembly-qualified string rather than in the signature. PawPrint resolves the target from the signature, so it would look the member up on the wrong type (usually System.Object) and silently miss it"

        let rawSignature = MethodInfo.requireRawSignature $"%s{describe} dispatch" accessor

        if rawSignature.RequiredParameterCount <> List.length rawSignature.ParameterTypes then
            failwith
                $"TODO: %s{describe} declares a vararg signature; no C# accessor is one, and the comparison here assumes every parameter is required"

        let concreteSignature = accessor.Signature

        // The type whose members are searched, read from the return type for a constructor and
        // from the first parameter for everything else (unsafeaccessors.cpp:1063).
        let targetPosition : Result<TypeDefn * ConcreteTypeHandle, UnsafeAccessorRefusal> =
            match kind with
            | UnsafeAccessorKind.Constructor ->
                // A return type is required (there is otherwise no type to construct), it must not
                // be byref, and the runtime picks the name so the attribute must not have supplied
                // one.
                let namedAnything =
                    match targetName with
                    | Some name -> name <> ""
                    | None -> false

                match rawSignature.ReturnType, concreteSignature.ReturnType with
                | MethodReturnType.Returns raw, MethodReturnType.Returns concrete when
                    not (isByref raw) && not namedAnything
                    ->
                    Ok (raw, concrete)
                | _ -> Error (UnsafeAccessorRefusal.BadImageFormat invalidUsageMessage)
            | UnsafeAccessorKind.Method
            | UnsafeAccessorKind.StaticMethod ->
                match rawSignature.ParameterTypes, concreteSignature.ParameterTypes with
                | raw :: _, concrete :: _ -> Ok (raw, concrete)
                | _ -> Error (UnsafeAccessorRefusal.BadImageFormat invalidUsageMessage)
            | UnsafeAccessorKind.Field
            | UnsafeAccessorKind.StaticField ->
                // Field access takes exactly one argument for the target type, and returns a byref
                // to the field (unsafeaccessors.cpp:1127).
                match rawSignature.ParameterTypes, concreteSignature.ParameterTypes with
                | [ raw ], [ concrete ] ->
                    match rawSignature.ReturnType with
                    | MethodReturnType.Returns ret when isByref ret -> Ok (raw, concrete)
                    | _ -> Error (UnsafeAccessorRefusal.BadImageFormat invalidUsageMessage)
                | _ -> Error (UnsafeAccessorRefusal.BadImageFormat invalidUsageMessage)

        match targetPosition with
        | Error refusal -> state, Error refusal
        | Ok (rawTarget, concreteTarget) ->

        // A generic parameter in the position the target type is read from is refused by
        // `ValidateTargetType` before it is ever resolved.
        if namesGenericParameter rawTarget then
            state, Error (UnsafeAccessorRefusal.BadImageFormat invalidUsageMessage)
        else

        let name = targetMemberName kind targetName accessor.Name

        // `ValidateTargetType` (unsafeaccessors.cpp:367): strip one `byref`, then insist on a type
        // whose members can be enumerated. CoreCLR blocks every `TypeDesc` -- a pointer, a
        // function pointer -- because those degrade in ways its member lookup cannot follow.
        let strippedTarget =
            match concreteTarget with
            | ConcreteTypeHandle.Byref inner -> inner
            | other -> other

        // The type a field accessor's `ref` return addresses, which is what a candidate field's own
        // type is compared against.
        let fieldReturnedType () : TypeDefn =
            match rawSignature.ReturnType with
            | MethodReturnType.Returns ret ->
                match byrefElement ret with
                | Some element -> element
                | None ->
                    failwith
                        "BUG: a field accessor whose return is not a byref reached member lookup; it is refused as BadImageFormat above"
            | MethodReturnType.Void ->
                failwith
                    "BUG: a field accessor with a void return reached member lookup; it is refused as BadImageFormat above"

        // A generic method over a reference type is compiled once, for its canonical instantiation,
        // and the stub is generated from that: so a `ref T` target position whose `T` is a
        // reference type is `System.__Canon` to the lookup, not the exact class. Measured on real
        // .NET 10: `ref int X<T>(ref T t)` reaches a struct `T`'s field and reports
        // `'System.__Canon.x'` missing for a class `T`, however real the member is on the class.
        //
        // Only the whole target position canonicalises to `__Canon`. A reference type *inside* it
        // leaves the outer definition in place -- `Box<T>` over a class is `Box<__Canon>`, which is
        // still `Box`1` with `Box`1`'s members and name -- so for a nominal target the exact
        // instantiation searches the same members and reports the same name. An array is the
        // exception, and is dealt with below.
        if
            namesByrefToGenericParameter rawTarget
            && isReplacedByCanon baseClassTypes state describe strippedTarget
        then
            let canon =
                baseClassTypes.Corelib.TryGetTopLevelTypeDef "System" "__Canon"
                |> Option.defaultWith (fun () -> failwith "BUG: CoreLib defines no System.__Canon")

            let state, refusal =
                match kind with
                | UnsafeAccessorKind.Method
                | UnsafeAccessorKind.StaticMethod ->
                    let state, found =
                        findTargetMethod
                            loggerFactory
                            baseClassTypes
                            describe
                            kind
                            name
                            accessor.DeclaringAssemblyFullName
                            rawSignature
                            canon
                            state

                    state,
                    (match found with
                     | Ok _ -> None
                     | Error refusal -> Some refusal)
                | UnsafeAccessorKind.Field
                | UnsafeAccessorKind.StaticField ->
                    let state, found =
                        findTargetField
                            loggerFactory
                            baseClassTypes
                            kind
                            name
                            accessor.DeclaringAssemblyFullName
                            (fieldReturnedType ())
                            canon
                            state

                    state,
                    (match found with
                     | Ok _ -> None
                     | Error refusal -> Some refusal)
                | UnsafeAccessorKind.Constructor ->
                    failwith
                        "BUG: a constructor accessor's target is its return type, which is refused above if it is a byref"

            match refusal with
            | Some refusal -> state, Error refusal
            | None ->
                // CoreLib's `__Canon` declares no members at all. Measured on real .NET 10, even a
                // `.ctor` accessor of the instance-method kind, reaching `__Canon` through the
                // `ref T` of a generic declaring type, reports `'System.__Canon..ctor'` missing.
                failwith $"BUG: %s{describe} bound a member of System.__Canon, which declares none"
        else

        match strippedTarget with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            // An array is *not* a TypeDesc in modern CoreCLR -- arrays have MethodTables -- so it
            // is a legal target there. Its only members are its constructors and its
            // `Get`/`Set`/`Address` accessors (`ArrayClass::GenerateArrayAccessorCallSig`,
            // array.cpp:68), which spell the element type as the class type variable `!0`: a
            // declaration on a non-generic type cannot spell `!0`, and the comparison substitutes
            // nothing, so none of the three can match it. What is left is the instance `.ctor`s,
            // which only the constructor kind and the instance-method kind can reach. Measured on
            // real .NET 10, every other lookup is reported missing, for all four non-constructor
            // kinds.

            // The array searched is the one the *canonical* instantiation names, so a shared type
            // argument anywhere inside it changes the name reported: measured on real .NET 10,
            // `T[]` over `string` is `System.__Canon[]`, and `List<T>[]` over `string` is
            // `System.__Canon[]` too, because loading an array over a shared instantiation
            // canonicalises the element again (clsload.cpp:3435). The same is true of the accessor
            // type's own type parameters: measured, `A<T>`'s `T[]` over `string` is
            // `System.__Canon[]` as well. It changes what a constructor builds as well as what a
            // lookup reports: measured, `A<T>`'s constructor accessor returning `T[]` builds an
            // `object[]` over `string`, and over `int[]` it reports `System.__Canon[]..ctor`
            // missing for the jagged constructor `int[][]` declares. When every type parameter
            // the target mentions is instantiated with its own canonical form, the canonical
            // array is the exact one.
            let shared =
                mentionedParameters rawTarget
                |> Seq.map (fun parameter ->
                    match parameter with
                    | MentionedParameter.OfType index ->
                        $"type parameter %d{index}", accessor.DeclaringTypeGenerics.[index]
                    | MentionedParameter.OfMethod index ->
                        $"method type parameter %d{index}", accessor.Generics.[index]
                )
                |> Seq.filter (fun (_, argument) -> isSharedTypeArgument baseClassTypes state describe argument)
                |> Seq.tryHead

            // `MemberLoader`'s messages name the target as `MethodTable::_GetFullyQualifiedNameForClass`
            // does, which for an array is `TypeDesc::ConstructName` over the element's
            // `TypeHandle::GetName`.
            let arrayName () : string =
                NativeRuntimeTypeHelpers.typeHandleGetName
                    describe
                    state
                    (RuntimeTypeHandleTarget.Closed strippedTarget)

            match kind with
            | UnsafeAccessorKind.Method when name = ".ctor" ->
                // Measured on real .NET 10: an instance-method accessor over `int[,]` whose
                // signature matches a constructor binds it, and the stub then fails to compile with
                // an `InvalidProgramException` whose message is the JIT's; one whose signature
                // matches none reports `.ctor` missing. PawPrint does not reproduce the JIT's
                // refusal.
                failwith
                    $"TODO: %s{describe} names an array's .ctor through the instance-method kind; CoreCLR binds it if the signature matches one of the array's constructors, and the JIT then refuses the stub with a message PawPrint does not reproduce"
            | UnsafeAccessorKind.Method when
                not accessor.DeclaringTypeGenerics.IsEmpty
                && (name = "Get" || name = "Set" || name = "Address")
                ->
                // A declaration on a generic type *can* spell `!0`, and the comparison takes it to be
                // the array's element variable by position alone. Measured on real .NET 10:
                // `A<T>.Get(T[] a, int i)` returning `T` binds `int[]::Get` for `A<int>`, and
                // `Set` binds on `int[,]` likewise.
                failwith
                    $"TODO: %s{describe} is declared on a generic type and names an array's %s{name}; CoreCLR binds it if the signature matches the array's accessor, which PawPrint does not model"
            | _ ->

            match shared with
            | Some (parameter, argument) ->
                failwith
                    $"TODO: %s{describe} names an array whose type mentions %s{parameter}, instantiated with %O{argument}; CoreCLR searches, and constructs, the canonical array instantiated over System.__Canon, and PawPrint does not model canonical forms"
            | None ->

            match kind with
            | UnsafeAccessorKind.Constructor ->
                let state, matching =
                    ((state, []), ArrayConstructor.declaredOn strippedTarget)
                    ||> List.fold (fun (state, acc) ctor ->
                        let state, matches =
                            declarationMatches
                                loggerFactory
                                baseClassTypes
                                kind
                                accessor.DeclaringAssemblyFullName
                                rawSignature
                                baseClassTypes.Corelib.Name.FullName
                                (ArrayConstructor.signature ctor)
                                state

                        if matches then state, ctor :: acc else state, acc
                    )

                match matching with
                | [ ctor ] ->
                    state, Ok (UnsafeAccessorPlan.ConstructArray (constructedByStub baseClassTypes state describe ctor))
                | [] -> state, Error (UnsafeAccessorRefusal.MissingMethod (arrayName (), name))
                | _ :: _ :: _ ->
                    failwith
                        $"BUG: %s{describe} matched %d{List.length matching} constructors of %O{strippedTarget}, which differ in their parameter counts"
            | UnsafeAccessorKind.Method
            | UnsafeAccessorKind.StaticMethod -> state, Error (UnsafeAccessorRefusal.MissingMethod (arrayName (), name))
            | UnsafeAccessorKind.Field
            | UnsafeAccessorKind.StaticField -> state, Error (UnsafeAccessorRefusal.MissingField (arrayName (), name))
        | _ ->

        match AllConcreteTypes.tryTypeInfo state._LoadedAssemblies state.ConcreteTypes strippedTarget with
        | None -> state, Error (UnsafeAccessorRefusal.BadImageFormat invalidUsageMessage)
        | Some (targetType, targetTypeInfo) ->

        let targetIsValueType =
            DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies targetTypeInfo

        // An instance member of a value type must be reached through a byref, or the accessor
        // would be handed a copy (unsafeaccessors.cpp:1111 and :1134).
        let instanceOfValueTypeNeedsByref =
            match kind with
            | UnsafeAccessorKind.Method
            | UnsafeAccessorKind.Field -> targetIsValueType && not (isByref rawTarget)
            | UnsafeAccessorKind.Constructor
            | UnsafeAccessorKind.StaticMethod
            | UnsafeAccessorKind.StaticField -> false

        if instanceOfValueTypeNeedsByref then
            state, Error (UnsafeAccessorRefusal.BadImageFormat invalidUsageMessage)
        else

        // The mirror of that check, and a shape with no answer to give. A *reference* type's
        // receiver reached through a byref is accepted by CoreCLR -- `ValidateTargetType` strips
        // the byref and the stub emits its `callvirt`/`ldflda` against a `Target&` where a `Target`
        // belongs -- and what runs is a read of whatever the byref addresses as though it were the
        // object. Measured on real .NET 10, a field accessor over `ref Target` returns a number
        // derived from the local's address, differing from run to run. PawPrint models a reference
        // as an opaque handle, so there is no such number for it to produce.
        //
        // Asked only once the lookup has succeeded, because this is the *body* refusing rather
        // than the binding: measured, an accessor of this shape naming a member that does not
        // exist reports the missing member. And only for the two kinds that use the first argument
        // as a receiver -- the static kinds read it for its type alone and never dereference it.
        let refuseByrefReferenceReceiver () : unit =
            let usesReceiver =
                match kind with
                | UnsafeAccessorKind.Method
                | UnsafeAccessorKind.Field -> true
                | UnsafeAccessorKind.Constructor
                | UnsafeAccessorKind.StaticMethod
                | UnsafeAccessorKind.StaticField -> false

            if usesReceiver && isByref rawTarget && not targetIsValueType then
                failwith
                    $"TODO: %s{describe} reaches a reference type's member through a `ref` to the reference. CoreCLR accepts that and dereferences the byref as though it addressed the object, so what it produces is derived from an address and differs from run to run; PawPrint models a reference as an opaque handle and has no address to produce one from"

        // The lookup took the accessor type's `!i` to be the target type's `!i` by index alone, so
        // the target it bound has the declared signature only if the two denote the same type. They
        // do in the documented shape, where the accessor's signature spells the target type as
        // `Target<!0, !1, ...>`; otherwise CoreCLR binds all the same, and its unverified stub then
        // hands the target a value of the accessor's type where the target's belongs. Measured on
        // real .NET 10, `A<T, U>` binds `Boxed<U>`'s `T _typed` through a `ref T` return.
        //
        // Only the positions the lookup compared matter: those are where a `!i` can have been
        // matched against the target's. The accessor's own `!!i` need no such check, because the
        // stub instantiates the target method with the accessor's method type arguments.
        let refuseMisalignedTypeVariables () : unit =
            let compared =
                match kind with
                | UnsafeAccessorKind.Constructor -> rawSignature.ParameterTypes
                | UnsafeAccessorKind.Method
                | UnsafeAccessorKind.StaticMethod ->
                    let returned =
                        match rawSignature.ReturnType with
                        | MethodReturnType.Void -> []
                        | MethodReturnType.Returns ty -> [ ty ]

                    returned @ List.tail rawSignature.ParameterTypes
                | UnsafeAccessorKind.Field
                | UnsafeAccessorKind.StaticField -> [ fieldReturnedType () ]

            for parameter in compared |> Seq.map mentionedParameters |> Set.unionMany do
                match parameter with
                | MentionedParameter.OfMethod _ -> ()
                | MentionedParameter.OfType index ->
                    if index >= targetType.Generics.Length then
                        failwith
                            $"BUG: %s{describe} bound a member of %s{describeTargetType targetTypeInfo} by matching the accessor type's type parameter %d{index}, which that type does not have"

                    let accessorArgument = accessor.DeclaringTypeGenerics.[index]
                    let targetArgument = targetType.Generics.[index]

                    if accessorArgument <> targetArgument then
                        failwith
                            $"TODO: %s{describe} binds a member of %s{describeTargetType targetTypeInfo} by taking the accessor type's type parameter %d{index} to be the target type's, but the accessor instantiates it with %O{accessorArgument} and the target type is instantiated with %O{targetArgument}. CoreCLR binds by position alone and its unverified stub passes one where the other belongs, which PawPrint does not reproduce"

        match kind with
        | UnsafeAccessorKind.Constructor
        | UnsafeAccessorKind.Method
        | UnsafeAccessorKind.StaticMethod ->
            let state, found =
                findTargetMethod
                    loggerFactory
                    baseClassTypes
                    describe
                    kind
                    name
                    accessor.DeclaringAssemblyFullName
                    rawSignature
                    targetTypeInfo
                    state

            match found with
            | Error refusal -> state, Error refusal
            | Ok target ->

            match
                checkTargetConstraints
                    baseClassTypes
                    state
                    describe
                    accessor.DeclaringTypeGenerics.Length
                    strippedTarget
                    targetTypeInfo
                    accessor.Generics
                    target
            with
            | Error refusal -> state, Error refusal
            | Ok () ->

            refuseByrefReferenceReceiver ()

            // A value type's generic virtual method has no unboxing stub, so it binds (see
            // `findTargetMethod`), and the stub's `callvirt` then runs it for a value-type
            // instantiation. For an instantiation shared over `System.__Canon` -- a reference-type
            // argument, or a value type instantiated over one -- measured on real .NET 10 the
            // process dies with SIGSEGV on the call, which is not an answer PawPrint can give.
            match kind with
            | UnsafeAccessorKind.Method when
                targetIsValueType
                && target.IsVirtual
                && not target.Generics.IsEmpty
                && accessor.Generics
                   |> Seq.exists (isSharedTypeArgument baseClassTypes state describe)
                ->
                failwith
                    $"TODO: %s{describe} names the generic virtual method %s{name} of a value type and instantiates it over System.__Canon; real .NET 10 crashes the process with SIGSEGV calling such an accessor, which PawPrint cannot reproduce"
            | _ -> ()

            // Two shapes the *body* CoreCLR emits refuses, both of them after the lookup has
            // succeeded -- measured on real .NET 10, an abstract class with no matching
            // constructor reports the missing constructor rather than the abstract class.
            match kind, target.Body with
            | UnsafeAccessorKind.StaticMethod, MethodBody.Abstract ->
                // The body is a `call`, and a `call` to an abstract method is not valid IL. A
                // `static abstract` interface member is the one static method that can be
                // abstract. The message is the JIT's rather than the attribute's.
                state, Error (UnsafeAccessorRefusal.BadImageFormat "Bad IL format.")
            | UnsafeAccessorKind.Constructor, _ when targetTypeInfo.TypeAttributes.HasFlag TypeAttributes.Abstract ->
                // The body is a `newobj`, which the JIT refuses for an abstract class
                // (`CEEInfo::getNewHelper`).
                state, Error UnsafeAccessorRefusal.CantInstantiateAbstractClass
            | _ ->

            refuseMisalignedTypeVariables ()

            let state, concretizedTarget, _declaringTypeHandle =
                ExecutionConcretization.concretizeMethodWithAllGenerics
                    loggerFactory
                    baseClassTypes
                    targetType.Generics
                    target
                    accessor.Generics
                    state

            let plan =
                match kind with
                | UnsafeAccessorKind.Constructor -> UnsafeAccessorPlan.Construct (concretizedTarget, strippedTarget)
                | UnsafeAccessorKind.Method -> UnsafeAccessorPlan.CallInstance concretizedTarget
                | UnsafeAccessorKind.StaticMethod -> UnsafeAccessorPlan.CallStatic concretizedTarget
                | UnsafeAccessorKind.Field
                | UnsafeAccessorKind.StaticField -> failwith "unreachable: the field kinds are handled by the other arm"

            state, Ok plan
        | UnsafeAccessorKind.Field
        | UnsafeAccessorKind.StaticField ->

        let state, found =
            findTargetField
                loggerFactory
                baseClassTypes
                kind
                name
                accessor.DeclaringAssemblyFullName
                (fieldReturnedType ())
                targetTypeInfo
                state

        match found with
        | Error refusal -> state, Error refusal
        | Ok field ->

        refuseByrefReferenceReceiver ()
        refuseMisalignedTypeVariables ()

        let plan =
            match kind with
            | UnsafeAccessorKind.Field -> UnsafeAccessorPlan.InstanceFieldAddress (field, strippedTarget)
            | UnsafeAccessorKind.StaticField ->
                UnsafeAccessorPlan.StaticFieldAddress (field, strippedTarget, targetType.Generics)
            | UnsafeAccessorKind.Constructor
            | UnsafeAccessorKind.Method
            | UnsafeAccessorKind.StaticMethod -> failwith "unreachable: the method kinds are handled by the other arm"

        state, Ok plan

    /// The exception CoreCLR raises for a refusal, and the message it carries.
    ///
    /// The prose is localisable and so is not a machine-independent fact; it is reproduced in the
    /// invariant culture because a guest that prints a caught exception should read as it does on
    /// real .NET, and it is measured rather than derived.
    let private exceptionFor
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (refusal : UnsafeAccessorRefusal)
        : TypeInfo<GenericParamFromMetadata, TypeDefn> * string
        =
        match refusal with
        | UnsafeAccessorRefusal.BadImageFormat message -> baseClassTypes.BadImageFormatException, message
        | UnsafeAccessorRefusal.MissingMethod (targetType, name) ->
            baseClassTypes.MissingMethodException, $"Method not found: '%s{targetType}.%s{name}'."
        | UnsafeAccessorRefusal.MissingField (targetType, name) ->
            baseClassTypes.MissingFieldException, $"Field not found: '%s{targetType}.%s{name}'."
        | UnsafeAccessorRefusal.AmbiguousMatch ->
            baseClassTypes.AmbiguousMatchException, "Ambiguity in binding of UnsafeAccessorAttribute."
        | UnsafeAccessorRefusal.GenericTypeConstraintsNotEqual ->
            baseClassTypes.InvalidProgramException, "Generic type constraints do not match."
        | UnsafeAccessorRefusal.MethodConstraintsViolation (targetType, methodName, typeArgument, typeParameter) ->
            baseClassTypes.VerificationException,
            $"Method %s{targetType}.%s{methodName}: type argument '%s{typeArgument}' violates the constraint of type parameter '%s{typeParameter}'."
        | UnsafeAccessorRefusal.CantInstantiateAbstractClass ->
            baseClassTypes.InvalidOperationException, "Instances of abstract classes cannot be created."

    /// Is this receiver null, in either of the two ways an accessor's first argument can be?
    ///
    /// A reference-typed target's receiver arrives as `NullObjectRef`; a value type's arrives as a
    /// managed pointer, and the null one of those is `ManagedPointerSource.Null` --
    /// `Unsafe.NullRef<S>()` is how a guest produces it. Both are null to the `callvirt` and the
    /// `ldflda` CoreCLR's stub emits: measured on real .NET 10, an accessor handed
    /// `ref Unsafe.NullRef<S>()` raises `NullReferenceException` from the accessor itself, for the
    /// method kind and for the field kind alike -- including a field at a non-zero offset, whose
    /// address is merely taken and never read.
    let private receiverIsNull (receiver : EvalStackValue) : bool =
        match receiver with
        | EvalStackValue.NullObjectRef -> true
        | EvalStackValue.ManagedPointer ManagedPointerSource.Null -> true
        | _ -> false

    /// The program counter an accessor's frame carries once it has dispatched to its target.
    ///
    /// An accessor's frame has no IL, so nothing else moves its program counter and any non-zero
    /// value means "the target has already run". Zero is what a fresh frame carries.
    [<Literal>]
    let private dispatchedProgramCounter = 1

    /// Raise into the guest from the accessor's own frame, which stays on the stack.
    let private raiseFromAccessor
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (exceptionType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (message : string option)
        (state : IlMachineState)
        : ExecutionResult
        =
        let state, _whatWeDid =
            IlMachineStateExecution.raiseRuntimeExceptionWithMessage
                loggerFactory
                baseClassTypes
                exceptionType
                message
                thread
                state

        ExecutionResult.stepped (state, WhatWeDid.SuspendedForManagedCall)

    /// Refuse an accessor whose `UnsafeAccessorKind` names none of the five kinds.
    ///
    /// CoreCLR parses the attribute's integer, keeps it, and reaches the `default:` of the switch
    /// that consumes it (unsafeaccessors.cpp:1146), which is the same `BFA_INVALID_UNSAFEACCESSOR`
    /// refusal a malformed declaration gets -- measured on real .NET 10 as a catchable
    /// `BadImageFormatException` on the first invocation. The accessor's own declaring type is not
    /// initialised: the stub fails to compile before the method's prologue could run.
    ///
    /// Two things *are* read before that switch, in this order. The accessor must be static
    /// (unsafeaccessors.cpp:1045), asked before the attribute is even parsed: an instance accessor
    /// is the same `BadImageFormatException` whatever else its declaration says. Then
    /// `ProcessUnsafeAccessorTypeAttributes` resolves any `[UnsafeAccessorType]` names, so a static
    /// declaration carrying one can raise for the named type (a `TypeLoadException`, say) before
    /// its kind is ever examined. That attribute is refused wherever it appears, this path included.
    let executeInvalidKind
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (instruction : MethodState)
        (hasTypeNameOverrides : bool)
        (state : IlMachineState)
        : ExecutionResult
        =
        let accessor = instruction.ExecutingMethod

        if accessor.IsStatic && hasTypeNameOverrides then
            failwith
                $"TODO: [UnsafeAccessor] %s{MethodOwner.describe accessor.Owner}::%s{accessor.Name} names none of the five kinds and also names at least one of its types with [UnsafeAccessorType]; CoreCLR resolves those names before it examines the kind, so what it raises depends on the named types, which PawPrint does not resolve"

        let exceptionType, message =
            exceptionFor baseClassTypes (UnsafeAccessorRefusal.BadImageFormat invalidUsageMessage)

        raiseFromAccessor loggerFactory baseClassTypes thread exceptionType (Some message) state

    /// Run an `[UnsafeAccessor]` accessor's synthesised body.
    ///
    /// The accessor's own frame is a real declared method's frame, not a trampoline: real .NET
    /// names it in the stack trace of everything that goes wrong here, both the binding failures
    /// (which it raises from the accessor's first invocation, as it JITs the stub) and the
    /// `NullReferenceException` the stub's own `callvirt`/`ldflda` produces. So every raise below
    /// happens with the frame still on the stack, and only the paths that reach the target pop it
    /// -- which is also what puts the target's frame directly above the accessor's caller, as it is
    /// on real .NET.
    ///
    /// The frame arrives *without* a pending type-initialisation check (`callMethod` leaves one
    /// off for this body kind), because CoreCLR binds the target while compiling the stub, which
    /// is before the method's prologue: a declaration that fails to bind raises without the
    /// accessor's declaring type having been initialised. Measured on .NET 10 with a counter on a
    /// third type, the counter is zero inside the `catch`. So this binds first and then runs the
    /// initialiser itself, before the body.
    let execute
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (instruction : MethodState)
        (kind : UnsafeAccessorKind)
        (targetName : string option)
        (hasTypeNameOverrides : bool)
        (state : IlMachineState)
        : ExecutionResult
        =
        let accessor = instruction.ExecutingMethod

        let describe =
            $"[UnsafeAccessor] %s{MethodOwner.describe accessor.Owner}::%s{accessor.Name}"

        let raiseFromAccessor = raiseFromAccessor loggerFactory baseClassTypes thread

        /// Push the arguments from `firstArgument` onwards onto the accessor's *own* evaluation
        /// stack. `StaticMethod` skips argument 0, whose only job was to name the type; every other
        /// calling kind passes the lot.
        let pushArguments (firstArgument : int) (state : IlMachineState) : IlMachineState =
            let mutable s = state

            for i = firstArgument to instruction.Arguments.Length - 1 do
                s <- IlMachineState.pushToEvalStack instruction.Arguments.[i] thread s

            s

        /// Mark the accessor's frame as having dispatched, so the re-entry that follows the target's
        /// return is distinguishable from the first pass.
        ///
        /// The frame has no IL, so its program counter is otherwise unused and stays at zero; a
        /// void target leaves nothing on the evaluation stack, so there is nothing else to read.
        let markDispatched (state : IlMachineState) : IlMachineState =
            state
            |> IlMachineState.mapFrame
                thread
                state.ThreadState.[thread].ActiveMethodState
                (MethodState.setProgramCounter dispatchedProgramCounter)

        /// Call `target`, leaving the accessor's frame on the stack beneath it.
        ///
        /// Real .NET keeps that frame: measured on .NET 10, an exception out of a target reports
        /// `Target.Boom -> Program.Access -> Program.Caller` with the accessor between the target
        /// and whoever called it. The frame is therefore returned when the target returns, not
        /// before, which also puts the target's return value on the accessor's own stack rather
        /// than on its caller's.
        let callTarget
            (target : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
            (firstArgument : int)
            (virtualDispatch : bool)
            (state : IlMachineState)
            : ExecutionResult
            =
            let state = state |> pushArguments firstArgument |> markDispatched
            let threadState = state.ThreadState.[thread]

            let state, commitment =
                IlMachineStateExecution.callMethodWithCommitment
                    loggerFactory
                    baseClassTypes
                    None
                    ConstructionState.NotConstructing
                    virtualDispatch
                    false
                    false // `markDispatched` has already moved this frame's program counter
                    IlMachineStateExecution.CallSiteTransition.StaysCooperative
                    IlMachineStateExecution.CallRoute.NamedByInstruction // CoreCLR generates the accessor as IL that calls the target
                    target.Generics
                    target
                    thread
                    threadState
                    None
                    ReturnValueDisposition.PushToCaller
                    false // wrapExceptionInTargetInvocation
                    state

            match commitment with
            | IlMachineStateExecution.CallCommitment.Aborted fatal ->
                ExecutionResult.stepped (state, WhatWeDid.Aborted fatal)
            | IlMachineStateExecution.CallCommitment.Committed
            | IlMachineStateExecution.CallCommitment.Raised ->
                ExecutionResult.stepped (state, WhatWeDid.SuspendedForManagedCall)

        /// Return the accessor's frame with `pointer` as its result -- the shape both field kinds
        /// take, whose declared return is a byref.
        let returnAddress (pointer : ManagedPointerSource) (state : IlMachineState) : ExecutionResult =
            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer pointer) thread state

            match IlMachineState.returnStackFrame loggerFactory baseClassTypes thread state with
            | ReturnFrameResult.NormalReturn state -> ExecutionResult.stepped (state, WhatWeDid.Executed)
            | result -> failwith $"unexpected ReturnFrameResult from %s{describe}: %A{result}"

        if instruction.IlOpIndex = dispatchedProgramCounter then
            // The target has run and returned into this frame; the only thing left is to hand its
            // result to whoever called the accessor.
            match IlMachineState.returnStackFrame loggerFactory baseClassTypes thread state with
            | ReturnFrameResult.NormalReturn state -> ExecutionResult.stepped (state, WhatWeDid.Executed)
            | result -> failwith $"unexpected ReturnFrameResult returning %s{describe}: %A{result}"
        else

        let state, plan =
            resolve loggerFactory baseClassTypes kind targetName hasTypeNameOverrides accessor state

        match plan with
        | Error refusal ->
            let exceptionType, message = exceptionFor baseClassTypes refusal
            raiseFromAccessor exceptionType (Some message) state
        | Ok plan ->

        // The prologue: the accessor is static, so entering it initialises its declaring type.
        // Nothing has been pushed yet, so a suspension here simply re-enters this frame at
        // program counter zero, which resolves and reaches this point again.
        let accessorDeclaringType =
            match
                AllConcreteTypes.findExistingConcreteType
                    state.ConcreteTypes
                    accessor.RequiredDeclaringType.Identity
                    accessor.DeclaringTypeGenerics
            with
            | Some handle -> handle
            | None ->
                failwith $"BUG: %s{describe} is executing, but its declaring type is not registered in AllConcreteTypes"

        match IlMachineStateExecution.loadClass loggerFactory baseClassTypes accessorDeclaringType thread state with
        | StateLoadResult.FirstLoadThis state -> ExecutionResult.stepped (state, WhatWeDid.SuspendedForClassInit)
        | StateLoadResult.ThrowingTypeInitializationException state ->
            ExecutionResult.stepped (state, WhatWeDid.ThrowingTypeInitializationException)
        | StateLoadResult.UnhandledTypeInitializationException (state, exn) ->
            ExecutionResult.UnhandledException (state, thread, exn)
        | StateLoadResult.Blocked (state, blockedBy) ->
            ExecutionResult.stepped (state, WhatWeDid.BlockedOnClassInit blockedBy)
        | StateLoadResult.NothingToDo state ->

        match plan with
        | UnsafeAccessorPlan.CallStatic target -> callTarget target 1 false state
        | UnsafeAccessorPlan.ConstructArray ctor ->
            let arguments =
                instruction.Arguments
                |> Seq.map (fun argument ->
                    match argument with
                    | CliType.Numeric (CliNumericType.Int32 value) -> value
                    | other ->
                        failwith
                            $"BUG: %s{describe} bound an array constructor, whose parameters are all int32, but was handed %O{other}"
                )
                |> ImmutableArray.CreateRange

            match ArrayConstructor.plan ctor arguments with
            | Error error ->
                // The stub's `newobj` raises this from inside the accessor's body, so after the
                // prologue above has initialised the accessor's declaring type.
                let exceptionType, message = ArrayConstructor.exceptionFor baseClassTypes error
                raiseFromAccessor exceptionType message state
            | Ok allocation ->

            let array, state = ArrayConstruction.allocate baseClassTypes allocation state

            let state =
                IlMachineState.pushToEvalStack (CliType.ObjectRef (Some array)) thread state

            match IlMachineState.returnStackFrame loggerFactory baseClassTypes thread state with
            | ReturnFrameResult.NormalReturn state -> ExecutionResult.stepped (state, WhatWeDid.Executed)
            | result -> failwith $"unexpected ReturnFrameResult from %s{describe}: %A{result}"
        | UnsafeAccessorPlan.Construct (ctor, targetType) ->
            let state = state |> pushArguments 0 |> markDispatched

            UnaryMetadataObjectOps.constructObject
                loggerFactory
                (loggerFactory.CreateLogger "UnsafeAccessor")
                baseClassTypes
                thread
                ctor
                targetType
                false // `markDispatched` has already moved this frame's program counter
                state
            |> fun state -> ExecutionResult.stepped (state, WhatWeDid.SuspendedForManagedCall)
        | UnsafeAccessorPlan.CallInstance target ->
            // CoreCLR emits `callvirt` for the instance-method kind (unsafeaccessors.cpp:968), so a
            // null receiver faults here rather than inside the target.
            if receiverIsNull (EvalStackValue.ofCliType instruction.Arguments.[0]) then
                raiseFromAccessor baseClassTypes.NullReferenceException None state
            else
                callTarget target 0 true state
        | UnsafeAccessorPlan.InstanceFieldAddress (field, declaringType) ->
            let receiver = EvalStackValue.ofCliType instruction.Arguments.[0]

            if receiverIsNull receiver then
                raiseFromAccessor baseClassTypes.NullReferenceException None state
            else

            let fieldId = FieldId.metadata declaringType field.Handle field.Name

            let state, pointer =
                UnaryMetadataFieldOps.instanceFieldAddress
                    loggerFactory
                    baseClassTypes
                    describe
                    field
                    fieldId
                    receiver
                    state

            returnAddress pointer state
        | UnsafeAccessorPlan.StaticFieldAddress (field, declaringType, typeGenerics) ->
            // The owner is resolved before anything else, both so the `[ThreadStatic]`-implies-not-
            // RVA assert inside `forField` fires on every path and because it is baked into the
            // byref handed out: the pointer addresses *this* thread's slot forever after.
            let owner = StaticOwner.forField thread field

            // `ldsflda` initialises the declaring type, and the accessor's frame stays on the stack
            // while it does: nothing has been written yet, so the re-entry that follows the
            // initialiser's return simply resolves and dispatches again.
            match IlMachineStateExecution.loadClass loggerFactory baseClassTypes declaringType thread state with
            | StateLoadResult.FirstLoadThis state -> ExecutionResult.stepped (state, WhatWeDid.SuspendedForClassInit)
            | StateLoadResult.ThrowingTypeInitializationException state ->
                ExecutionResult.stepped (state, WhatWeDid.ThrowingTypeInitializationException)
            | StateLoadResult.UnhandledTypeInitializationException (state, exn) ->
                ExecutionResult.UnhandledException (state, thread, exn)
            | StateLoadResult.Blocked (state, blockedBy) ->
                ExecutionResult.stepped (state, WhatWeDid.BlockedOnClassInit blockedBy)
            | StateLoadResult.NothingToDo state ->

            let declaringAssy =
                state.LoadedAssembly field.DeclaringType.AssemblyFullName
                |> Option.defaultWith (fun () ->
                    failwith
                        $"%s{describe}: declaring assembly %s{field.DeclaringType.AssemblyFullName} of the target field is not loaded, but resolving the target is what loads it"
                )

            let state, pointer =
                UnaryMetadataFieldOps.staticFieldAddress
                    loggerFactory
                    baseClassTypes
                    declaringAssy
                    field
                    declaringType
                    typeGenerics
                    owner
                    state

            returnAddress pointer state
