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
/// the accessor's first invocation, where the guest's own `try`/`catch` can see them, and CoreLib's
/// own accessors are written expecting exactly that.
[<RequireQualifiedAccess>]
type internal UnsafeAccessorRefusal =
    /// `COR_E_BADIMAGEFORMAT`, which CoreCLR raises for a declaration whose signature cannot
    /// describe an accessor at all.
    | BadImageFormat

    /// `MemberLoader::ThrowMissingMethodException`. Carries the target type and member name, which
    /// is what CoreCLR's message names.
    | MissingMethod of targetType : string * name : string

    /// `MemberLoader::ThrowMissingFieldException`.
    | MissingField of targetType : string * name : string

    /// `kInvalidProgramException` from `VerifyDeclarationSatisfiesTargetConstraints`.
    | InvalidProgram of message : string

    /// `COR_E_BADIMAGEFORMAT` again, but raised where the *stub* is compiled rather than where it
    /// is generated: the `StaticMethod` kind's body is a `call`, and a `call` to an abstract method
    /// is not valid IL. A `static abstract` interface member is the one static method that can be
    /// abstract, so it is the only way here. Its message is the JIT's rather than the attribute's,
    /// which is why it is not `BadImageFormat`.
    | AbstractStaticTarget

    /// `kInvalidOperationException` with `Acc_CreateAbst`, from the `newobj` the `Constructor`
    /// kind's body performs: a constructor accessor may name an abstract class's constructor, and
    /// the class cannot be instantiated.
    | InvalidOperation of message : string

    /// `IDS_EE_METHOD_CONSTRAINTS_VIOLATION` from `TypeVarTypeDesc::SatisfiesConstraints`: the
    /// accessor's own type argument does not satisfy the target method's type parameter. Carries
    /// the three names CoreCLR's message interpolates, so the message is built where the exception
    /// is rather than where the violation is found.
    | ConstraintViolation of targetMethod : string * typeArgument : string * parameterName : string

[<RequireQualifiedAccess>]
module internal UnsafeAccessorDispatch =

    /// The name of the member to look for: the attribute's `Name` property, or the accessor's own
    /// name when it is absent, per `UnsafeAccessorAttribute`'s documented default. A constructor's
    /// name is the runtime's to choose, and `TryGenerateUnsafeAccessor` has already refused a
    /// declaration that supplied one.
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
        | UnsafeAccessorKind.StaticField -> targetName |> Option.defaultValue accessorName

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

    /// `ValidateTargetType` (unsafeaccessors.cpp:367) over a concretized handle: strip one `byref`,
    /// then insist on a nominal type. CoreCLR blocks every `TypeDesc` here -- a pointer, an array,
    /// a function pointer -- because those degrade in ways its member lookup cannot follow.
    let private validateTargetType
        (state : IlMachineState)
        (handle : ConcreteTypeHandle)
        : Result<
              ConcreteTypeHandle * ConcreteType<ConcreteTypeHandle> * TypeInfo<GenericParamFromMetadata, TypeDefn>,
              UnsafeAccessorRefusal
           >
        =
        let stripped =
            match handle with
            | ConcreteTypeHandle.Byref inner -> inner
            | other -> other

        match AllConcreteTypes.tryTypeInfo state._LoadedAssemblies state.ConcreteTypes stripped with
        | Some (concreteType, typeInfo) -> Ok (stripped, concreteType, typeInfo)
        | None -> Error UnsafeAccessorRefusal.BadImageFormat

    /// Does this signature element name a generic parameter, once byrefs and custom modifiers are
    /// peeled off? `ValidateTargetType` refuses `ELEMENT_TYPE_VAR`/`ELEMENT_TYPE_MVAR` in the
    /// position the target type is read from, and that is a question about the *blob*: the
    /// concretized handle has already had the variable substituted away.
    let rec private namesGenericParameter (ty : TypeDefn) : bool =
        match ty with
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _ -> true
        | TypeDefn.Byref inner -> namesGenericParameter inner
        | TypeDefn.Modified m -> namesGenericParameter m.Unmodified
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

    /// A name for the target type in the message CoreCLR's `ThrowMissingMethodException` builds.
    let private describeTargetType (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>) : string = typeInfo.Name

    /// Find the one declared method on the target type that the declaration names, in the sense of
    /// `TrySetTargetMethod` (unsafeaccessors.cpp:584): the type's *own* methods only -- no
    /// base-class walk -- filtered by name and static-ness, then matched signature against
    /// signature.
    let private findTargetMethod
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (kind : UnsafeAccessorKind)
        (name : string)
        (accessorAssemblyFullName : string)
        (declarationSignature : TypeMethodSignature<TypeDefn>)
        (targetType : ConcreteType<ConcreteTypeHandle>)
        (targetTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (state : IlMachineState)
        : IlMachineState *
          Result<
              WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>,
              UnsafeAccessorRefusal
           >
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
                        "BUG: findTargetMethod reached a non-constructor accessor with no parameters; `resolve` refuses that as BadImageFormat"
                | _ :: rest -> rest

        let candidates =
            targetTypeInfo.Methods
            |> List.filter (fun candidate -> candidate.Name = name && candidate.IsStatic = isTargetStatic kind)

        let state, matching =
            ((state, []), candidates)
            ||> List.fold (fun (state, acc) candidate ->
                let candidateSignature =
                    MethodInfo.requireRawSignature "[UnsafeAccessor] target lookup" candidate
                    |> stripSignatureModifiersDeep

                // A constructor candidate must return void; CoreCLR checks that in place of
                // comparing the return column (unsafeaccessors.cpp:481), which is skipped for this
                // kind. No C# compiler emits a non-void `.ctor` and no guest here reaches this
                // arm, but the check is what makes skipping the return column safe on an image
                // that does.
                let returnAcceptable =
                    if not isConstructor then
                        true
                    else
                        match candidateSignature.ReturnType with
                        | MethodReturnType.Void -> true
                        | MethodReturnType.Returns _ -> false

                if not returnAcceptable then
                    state, acc
                else

                let header = comparisonHeader declarationSignature.Header candidateSignature.Header

                let declarationComparand : TypeConcretization.SignatureComparand =
                    {
                        Signature =
                            comparandSignature
                                header
                                declarationSignature.GenericParameterCount
                                (stripReturnModifiersDeep declarationSignature.ReturnType)
                                (declarationParameters |> List.map stripModifiersDeep)
                        AssemblyFullName = accessorAssemblyFullName
                        // The accessor's declaring type is non-generic (`resolve` refuses
                        // otherwise), so no `!i` can appear on this side at all.
                        DeclaringTypeGenerics = TypeConcretization.SubstitutionContext.ofClosed ImmutableArray.Empty
                    }

                let candidateComparand : TypeConcretization.SignatureComparand =
                    {
                        Signature = candidateSignature
                        AssemblyFullName = targetType.AssemblyFullName
                        // The target's own type variables, left standing rather than substituted:
                        // CoreCLR compares these blobs with no substitution on either side
                        // (`pSubst1 = pSubst2 = NULL`, unsafeaccessors.cpp:399/409), so a target
                        // spelling `!0` matches only a declaration spelling `!0` -- never one
                        // spelling the type that instantiates it. Measured against real .NET 10: a
                        // non-generic accessor over `C<int>` does *not* find `C<T>::M(T)`.
                        DeclaringTypeGenerics =
                            TypeConcretization.SubstitutionContext.forDefinition
                                targetType.Identity
                                targetType.Generics.Length
                    }

                let state, matches =
                    IlMachineTypeResolution.signaturesEquivalent
                        loggerFactory
                        baseClassTypes
                        state
                        isConstructor // the return column is compared for every other kind
                        declarationComparand
                        candidateComparand

                if matches then state, candidate :: acc else state, acc
            )

        match matching with
        | [ single ] -> state, Ok single
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
                $"TODO: [UnsafeAccessor] found %d{List.length matching} declared methods named %s{name} on %s{targetTypeInfo.Namespace}.%s{targetTypeInfo.Name} matching the declaration's signature once custom modifiers are ignored; CoreCLR would retry requiring them to match exactly (unsafeaccessors.cpp:625-637), which needs a comparison this does not have"

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
        (targetType : ConcreteType<ConcreteTypeHandle>)
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
                let declarationComparand : TypeConcretization.SignatureComparand =
                    {
                        Signature =
                            comparandSignature
                                fieldComparisonHeader
                                0
                                MethodReturnType.Void
                                [ stripModifiersDeep returnedType ]
                        AssemblyFullName = accessorAssemblyFullName
                        DeclaringTypeGenerics = TypeConcretization.SubstitutionContext.ofClosed ImmutableArray.Empty
                    }

                let candidateComparand : TypeConcretization.SignatureComparand =
                    {
                        Signature =
                            comparandSignature
                                fieldComparisonHeader
                                0
                                MethodReturnType.Void
                                [ stripModifiersDeep candidate.Signature ]
                        AssemblyFullName = targetType.AssemblyFullName
                        DeclaringTypeGenerics =
                            TypeConcretization.SubstitutionContext.forDefinition
                                targetType.Identity
                                targetType.Generics.Length
                    }

                let state, matches =
                    IlMachineTypeResolution.signaturesEquivalent
                        loggerFactory
                        baseClassTypes
                        state
                        true // the two types under comparison are both in the parameter list
                        declarationComparand
                        candidateComparand

                if matches then state, candidate :: acc else state, acc
            )

        match matching with
        | [ single ] -> state, Ok single
        | [] -> state, Error (UnsafeAccessorRefusal.MissingField (describeTargetType targetTypeInfo, name))
        | _ :: _ :: _ ->
            failwith
                $"BUG: [UnsafeAccessor] found %d{List.length matching} declared fields named %s{name} on %s{targetTypeInfo.Namespace}.%s{targetTypeInfo.Name}; a field name is unique within a type, so the candidate filter is wrong"

    /// What `TypeVarTypeDesc::SatisfiesConstraints` would say about one (parameter, argument) pair,
    /// for the pairs PawPrint can decide.
    [<RequireQualifiedAccess>]
    type private ConstraintVerdict =
        /// The parameter constrains nothing that this argument could violate.
        | Satisfied
        /// The argument is byref-like and the parameter has no `allows ref struct`.
        | ByRefLikeRefused
        /// The parameter carries constraints PawPrint cannot check.
        | Undecidable

    /// Would `TypeVarTypeDesc::SatisfiesConstraints` accept `argument` for this generic parameter?
    ///
    /// Declaring no constraints is not the same as accepting everything: a parameter without
    /// `allows ref struct` (`gpAllowByRefLike`) refuses a byref-like argument, so the *absence* of
    /// that anti-constraint is itself a constraint (typedesc.cpp:1606). That one is decidable here,
    /// because "is this type byref-like" is a fact about the argument alone; every other constraint
    /// needs an assignability walk PawPrint does not have, so a parameter carrying one makes the
    /// pair undecidable rather than accepted.
    ///
    /// Measured on real .NET 10: an accessor whose own parameter says `allows ref struct`, over a
    /// target parameter that does not, raises `VerificationException` when instantiated with a
    /// `Span<int>` and runs the target when instantiated with an `int`.
    let private constraintVerdict
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        ((_, metadata) : GenericParamFromMetadata)
        (argument : ConcreteTypeHandle)
        : ConstraintVerdict
        =
        let argumentIsByRefLike =
            match AllConcreteTypes.tryTypeInfo state._LoadedAssemblies state.ConcreteTypes argument with
            | Some (_, typeInfo) -> DumpedAssembly.isByRefLike baseClassTypes state._LoadedAssemblies typeInfo
            | None ->
                // A structural handle: a byref, pointer, array or function pointer. None of those is
                // a byref-like *type* -- `Span<T>` is nominal -- so the anti-constraint does not
                // bear on them.
                false

        if
            metadata.Constraint.IsSome
            || metadata.RequiresParameterlessConstructor
            || not metadata.Constraints.IsEmpty
        then
            ConstraintVerdict.Undecidable
        elif argumentIsByRefLike && not metadata.AllowsByRefLike then
            ConstraintVerdict.ByRefLikeRefused
        else
            ConstraintVerdict.Satisfied

    /// A type argument's name, as CoreCLR's constraint-violation message interpolates it.
    ///
    /// Reflection's rendering, not the IL keyword form: `System.Span`1[System.Int32]`. A handle
    /// that is not one of the shapes a type argument takes falls back to the diagnostic rendering,
    /// whose `#handle` marks it as not having come from here.
    let rec private renderTypeArgument (state : IlMachineState) (handle : ConcreteTypeHandle) : string =
        let recurse = renderTypeArgument state

        match handle with
        | ConcreteTypeHandle.Byref inner -> recurse inner + "&"
        | ConcreteTypeHandle.Pointer inner -> recurse inner + "*"
        | ConcreteTypeHandle.OneDimArrayZero element -> recurse element + "[]"
        | ConcreteTypeHandle.Array (element, rank) ->
            let inside = if rank <= 1 then "*" else String.replicate (rank - 1) ","

            recurse element + "[" + inside + "]"
        | ConcreteTypeHandle.FunctionPointer _ ->
            AllConcreteTypes.describe state._LoadedAssemblies state.ConcreteTypes handle
        | ConcreteTypeHandle.Concrete _ ->

        match AllConcreteTypes.tryTypeInfo state._LoadedAssemblies state.ConcreteTypes handle with
        | None -> AllConcreteTypes.describe state._LoadedAssemblies state.ConcreteTypes handle
        | Some (concreteType, typeInfo) ->

        let bare =
            if System.String.IsNullOrEmpty typeInfo.Namespace then
                typeInfo.Name
            else
                $"%s{typeInfo.Namespace}.%s{typeInfo.Name}"

        if concreteType.Generics.IsEmpty then
            bare
        else
            let args = concreteType.Generics |> Seq.map recurse |> String.concat ","
            $"%s{bare}[%s{args}]"

    /// `VerifyDeclarationSatisfiesTargetConstraints` (unsafeaccessors.cpp:513) for the shapes this
    /// dispatcher accepts. The accessor's declaring type is non-generic, so the declaration
    /// supplies no class instantiation: a target on a generic type therefore has no arguments to
    /// satisfy its parameters, which is what CoreCLR reports rather than a lookup failure.
    /// Measured against real .NET 10: a non-generic accessor whose signature does match a member
    /// of a generic type gets `InvalidProgramException`, not `MissingMethodException`.
    let private verifyConstraints
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (describe : string)
        (targetType : ConcreteType<ConcreteTypeHandle>)
        (accessorMethodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (target : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        : Result<unit, UnsafeAccessorRefusal>
        =
        if targetType.Generics.IsEmpty && target.Generics.IsEmpty then
            // `HasClassOrMethodInstantiation()` is false, and CoreCLR returns immediately.
            Ok ()
        elif not targetType.Generics.IsEmpty then
            Error (UnsafeAccessorRefusal.InvalidProgram "Generic type constraints do not match.")
        elif target.Generics.Length <> accessorMethodGenerics.Length then
            // Unreachable after a successful signature match, which compares the generic-parameter
            // counts as CoreCLR compares the blobs' leading bytes; stated rather than assumed.
            Error (UnsafeAccessorRefusal.InvalidProgram "Generic method constraints do not match.")
        else

        let verdicts =
            Seq.zip target.Generics accessorMethodGenerics
            |> Seq.map (fun (parameter, argument) ->
                parameter, argument, constraintVerdict baseClassTypes state parameter argument
            )
            |> List.ofSeq

        match
            verdicts
            |> List.tryFind (fun (_, _, verdict) -> verdict = ConstraintVerdict.ByRefLikeRefused)
        with
        | Some ((parameter, _), argument, _) ->
            Error (
                UnsafeAccessorRefusal.ConstraintViolation (
                    $"%s{(MethodOwner.requireDeclaringType describe target.Owner).Name}.%s{target.Name}",
                    renderTypeArgument state argument,
                    parameter.Name
                )
            )
        | None ->

        if
            verdicts
            |> List.forall (fun (_, _, verdict) -> verdict = ConstraintVerdict.Satisfied)
        then
            Ok ()
        else
            // Every remaining case needs `TypeVarTypeDesc::SatisfiesConstraints`: whether the
            // accessor's own type arguments satisfy constraints that name *types*. PawPrint has no
            // assignability walk to answer that, and answering "yes" would run a target the real
            // runtime refuses to bind.
            failwith
                $"TODO: %s{describe} names a generic method whose type parameters carry constraints beyond `allows ref struct`; deciding whether the accessor's own type arguments satisfy them needs the assignability walk of CoreCLR's TypeVarTypeDesc::SatisfiesConstraints, which PawPrint does not have"

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

        if hasTypeNameOverrides then
            failwith
                $"TODO: %s{describe} names at least one of its types with [UnsafeAccessorType], which gives the type as an assembly-qualified string rather than in the signature. PawPrint resolves the target from the signature, so it would look the member up on the wrong type (usually System.Object) and silently miss it"

        if not accessor.DeclaringTypeGenerics.IsEmpty then
            failwith
                $"TODO: %s{describe} is declared on a generic type. CoreCLR compares the declaration's signature blob against the target's with no substitution on either side, so the two types' variables are identified positionally by index alone (unsafeaccessors.cpp:399/409); PawPrint's signature comparison identifies a variable by its owning definition as well, and refuses to compare variables of two different owners"

        let rawSignature = MethodInfo.requireRawSignature $"%s{describe} dispatch" accessor

        if rawSignature.RequiredParameterCount <> List.length rawSignature.ParameterTypes then
            failwith
                $"TODO: %s{describe} declares a vararg signature; no C# accessor is one, and the comparison here assumes every parameter is required"

        let concreteSignature = accessor.Signature

        // `UnsafeAccessor` must be on a static method (unsafeaccessors.cpp:1046).
        if not accessor.IsStatic then
            state, Error UnsafeAccessorRefusal.BadImageFormat
        else

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
                | _ -> Error UnsafeAccessorRefusal.BadImageFormat
            | UnsafeAccessorKind.Method
            | UnsafeAccessorKind.StaticMethod ->
                match rawSignature.ParameterTypes, concreteSignature.ParameterTypes with
                | raw :: _, concrete :: _ -> Ok (raw, concrete)
                | _ -> Error UnsafeAccessorRefusal.BadImageFormat
            | UnsafeAccessorKind.Field
            | UnsafeAccessorKind.StaticField ->
                // Field access takes exactly one argument for the target type, and returns a byref
                // to the field (unsafeaccessors.cpp:1127).
                match rawSignature.ParameterTypes, concreteSignature.ParameterTypes with
                | [ raw ], [ concrete ] ->
                    match rawSignature.ReturnType with
                    | MethodReturnType.Returns ret when isByref ret -> Ok (raw, concrete)
                    | _ -> Error UnsafeAccessorRefusal.BadImageFormat
                | _ -> Error UnsafeAccessorRefusal.BadImageFormat

        match targetPosition with
        | Error refusal -> state, Error refusal
        | Ok (rawTarget, concreteTarget) ->

        // A generic parameter in the position the target type is read from is refused by
        // `ValidateTargetType` before it is ever resolved.
        if namesGenericParameter rawTarget then
            state, Error UnsafeAccessorRefusal.BadImageFormat
        else

        match validateTargetType state concreteTarget with
        | Error refusal -> state, Error refusal
        | Ok (targetTypeHandle, targetType, targetTypeInfo) ->

        // An instance member of a value type must be reached through a byref, or the accessor
        // would be handed a copy (unsafeaccessors.cpp:1111 and :1134).
        let instanceOfValueTypeNeedsByref =
            match kind with
            | UnsafeAccessorKind.Method
            | UnsafeAccessorKind.Field ->
                DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies targetTypeInfo
                && not (isByref rawTarget)
            | UnsafeAccessorKind.Constructor
            | UnsafeAccessorKind.StaticMethod
            | UnsafeAccessorKind.StaticField -> false

        if instanceOfValueTypeNeedsByref then
            state, Error UnsafeAccessorRefusal.BadImageFormat
        else

        let name = targetMemberName kind targetName accessor.Name

        match kind with
        | UnsafeAccessorKind.Constructor
        | UnsafeAccessorKind.Method
        | UnsafeAccessorKind.StaticMethod ->
            let state, found =
                findTargetMethod
                    loggerFactory
                    baseClassTypes
                    kind
                    name
                    accessor.DeclaringAssemblyFullName
                    rawSignature
                    targetType
                    targetTypeInfo
                    state

            match found with
            | Error refusal -> state, Error refusal
            | Ok target ->

            match verifyConstraints baseClassTypes state describe targetType accessor.Generics target with
            | Error refusal -> state, Error refusal
            | Ok () ->

            // Two shapes the *body* CoreCLR emits refuses, both of them after the lookup has
            // succeeded -- measured on real .NET 10, where an abstract class with no matching
            // constructor reports the missing constructor rather than the abstract class.
            let bodyRefusal =
                match kind with
                | UnsafeAccessorKind.StaticMethod ->
                    // The body is a `call`, and a `call` to an abstract method is not valid IL.
                    match target.Body with
                    | MethodBody.Abstract -> Some UnsafeAccessorRefusal.AbstractStaticTarget
                    | _ -> None
                | UnsafeAccessorKind.Constructor ->
                    // The body is a `newobj`, which cannot allocate an abstract class.
                    if targetTypeInfo.TypeAttributes.HasFlag TypeAttributes.Abstract then
                        Some (UnsafeAccessorRefusal.InvalidOperation "Instances of abstract classes cannot be created.")
                    else
                        None
                | UnsafeAccessorKind.Method ->
                    // The body is a `callvirt`, which an abstract target is exactly what dispatches.
                    None
                | UnsafeAccessorKind.Field
                | UnsafeAccessorKind.StaticField -> None

            match bodyRefusal with
            | Some refusal -> state, Error refusal
            | None ->

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
                | UnsafeAccessorKind.Constructor -> UnsafeAccessorPlan.Construct (concretizedTarget, targetTypeHandle)
                | UnsafeAccessorKind.Method -> UnsafeAccessorPlan.CallInstance concretizedTarget
                | UnsafeAccessorKind.StaticMethod -> UnsafeAccessorPlan.CallStatic concretizedTarget
                | UnsafeAccessorKind.Field
                | UnsafeAccessorKind.StaticField -> failwith "unreachable: the field kinds are handled by the other arm"

            state, Ok plan
        | UnsafeAccessorKind.Field
        | UnsafeAccessorKind.StaticField ->

        let returnedType =
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

        let state, found =
            findTargetField
                loggerFactory
                baseClassTypes
                kind
                name
                accessor.DeclaringAssemblyFullName
                returnedType
                targetType
                targetTypeInfo
                state

        match found with
        | Error refusal -> state, Error refusal
        | Ok field ->

        let plan =
            match kind with
            | UnsafeAccessorKind.Field -> UnsafeAccessorPlan.InstanceFieldAddress (field, targetTypeHandle)
            | UnsafeAccessorKind.StaticField ->
                UnsafeAccessorPlan.StaticFieldAddress (field, targetTypeHandle, targetType.Generics)
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
        | UnsafeAccessorRefusal.BadImageFormat ->
            baseClassTypes.BadImageFormatException, "Invalid usage of UnsafeAccessorAttribute."
        | UnsafeAccessorRefusal.MissingMethod (targetType, name) ->
            baseClassTypes.MissingMethodException, $"Method not found: '%s{targetType}.%s{name}'."
        | UnsafeAccessorRefusal.MissingField (targetType, name) ->
            baseClassTypes.MissingFieldException, $"Field not found: '%s{targetType}.%s{name}'."
        | UnsafeAccessorRefusal.InvalidProgram message -> baseClassTypes.InvalidProgramException, message
        | UnsafeAccessorRefusal.AbstractStaticTarget -> baseClassTypes.BadImageFormatException, "Bad IL format."
        | UnsafeAccessorRefusal.InvalidOperation message -> baseClassTypes.InvalidOperationException, message
        | UnsafeAccessorRefusal.ConstraintViolation (targetMethod, typeArgument, parameterName) ->
            baseClassTypes.VerificationException,
            $"Method %s{targetMethod}: type argument '%s{typeArgument}' violates the constraint of type parameter '%s{parameterName}'."

    /// Run an `[UnsafeAccessor]` accessor's synthesised body.
    ///
    /// The accessor's own frame is a real declared method's frame, not a trampoline: real .NET
    /// names it in the stack trace of everything that goes wrong here, both the binding failures
    /// (which it raises from the accessor's first invocation, as it JITs the stub) and the
    /// `NullReferenceException` the stub's own `callvirt`/`ldflda` produces. So every raise below
    /// happens with the frame still on the stack, and only the paths that reach the target pop it
    /// -- which is also what puts the target's frame directly above the accessor's caller, as it is
    /// on real .NET.
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

        // The call that entered the accessor has already advanced past itself, so the target's
        // frame must record the original call site rather than the caller's resume point: exception
        // dispatch reads that offset both to decide which of the caller's `try` regions cover a
        // throw and to name the frame.
        let originalCallSitePC =
            instruction.ReturnState |> Option.map (fun rs -> rs.CallSiteIlOpIndex)

        /// Raise into the guest from the accessor's own frame, which stays on the stack.
        let raiseFromAccessor
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

        /// Pop the accessor's frame and call `target`, having pushed the arguments from
        /// `firstArgument` onwards. `StaticMethod` skips argument 0, whose only job was to name the
        /// type; every other calling kind passes the lot.
        let callTarget
            (target : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
            (firstArgument : int)
            (virtualDispatch : bool)
            (state : IlMachineState)
            : ExecutionResult
            =
            match state |> IlMachineState.returnFromSyntheticStackFrame thread with
            | ReturnFrameResult.NoFrameToReturn -> failwith $"unexpectedly nowhere to return from %s{describe}"
            | ReturnFrameResult.DispatchException _ ->
                failwith $"unexpected exception dispatch from %s{describe} frame pop"
            | ReturnFrameResult.NormalReturn state ->

            let state =
                let mutable s = state

                for i = firstArgument to instruction.Arguments.Length - 1 do
                    s <- IlMachineState.pushToEvalStack instruction.Arguments.[i] thread s

                s

            let threadState = state.ThreadState.[thread]

            let state, commitment =
                IlMachineStateExecution.callMethodWithCommitment
                    loggerFactory
                    baseClassTypes
                    None
                    ConstructionState.NotConstructing
                    virtualDispatch
                    false
                    false // the accessor frame is gone; there is no program counter to advance
                    IlMachineStateExecution.CallSiteTransition.StaysCooperative
                    target.Generics
                    target
                    thread
                    threadState
                    originalCallSitePC
                    ReturnValueDisposition.PushToCaller
                    false // wrapExceptionInTargetInvocation
                    state

            match commitment with
            | IlMachineStateExecution.CallCommitment.Aborted fatal ->
                ExecutionResult.stepped (state, WhatWeDid.Aborted fatal)
            | IlMachineStateExecution.CallCommitment.Committed
            | IlMachineStateExecution.CallCommitment.Raised -> ExecutionResult.stepped (state, WhatWeDid.Executed)

        /// Return the accessor's frame with `pointer` as its result -- the shape both field kinds
        /// take, whose declared return is a byref.
        let returnAddress (pointer : ManagedPointerSource) (state : IlMachineState) : ExecutionResult =
            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer pointer) thread state

            match IlMachineState.returnStackFrame loggerFactory baseClassTypes thread state with
            | ReturnFrameResult.NormalReturn state -> ExecutionResult.stepped (state, WhatWeDid.Executed)
            | result -> failwith $"unexpected ReturnFrameResult from %s{describe}: %A{result}"

        let state, plan =
            resolve loggerFactory baseClassTypes kind targetName hasTypeNameOverrides accessor state

        match plan with
        | Error refusal ->
            let exceptionType, message = exceptionFor baseClassTypes refusal
            raiseFromAccessor exceptionType (Some message) state
        | Ok (UnsafeAccessorPlan.CallStatic target) -> callTarget target 1 false state
        | Ok (UnsafeAccessorPlan.Construct (ctor, targetType)) ->
            match state |> IlMachineState.returnFromSyntheticStackFrame thread with
            | ReturnFrameResult.NoFrameToReturn -> failwith $"unexpectedly nowhere to return from %s{describe}"
            | ReturnFrameResult.DispatchException _ ->
                failwith $"unexpected exception dispatch from %s{describe} frame pop"
            | ReturnFrameResult.NormalReturn state ->

            let state =
                let mutable s = state

                for i = 0 to instruction.Arguments.Length - 1 do
                    s <- IlMachineState.pushToEvalStack instruction.Arguments.[i] thread s

                s

            UnaryMetadataObjectOps.constructObject
                loggerFactory
                baseClassTypes
                thread
                ctor
                targetType
                false // the accessor frame is gone; there is no program counter to advance
                originalCallSitePC
                state
            |> fun state -> ExecutionResult.stepped (state, WhatWeDid.Executed)
        | Ok (UnsafeAccessorPlan.CallInstance target) ->
            // CoreCLR emits `callvirt` for the instance-method kind (unsafeaccessors.cpp:968), so a
            // null receiver faults here rather than inside the target.
            match EvalStackValue.ofCliType instruction.Arguments.[0] with
            | EvalStackValue.NullObjectRef -> raiseFromAccessor baseClassTypes.NullReferenceException None state
            | _ -> callTarget target 0 true state
        | Ok (UnsafeAccessorPlan.InstanceFieldAddress (field, declaringType)) ->
            let receiver = EvalStackValue.ofCliType instruction.Arguments.[0]

            match receiver with
            | EvalStackValue.NullObjectRef -> raiseFromAccessor baseClassTypes.NullReferenceException None state
            | _ ->

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
        | Ok (UnsafeAccessorPlan.StaticFieldAddress (field, declaringType, typeGenerics)) ->
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
