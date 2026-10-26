namespace WoofWare.PawPrint.Analysis

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Microsoft.Extensions.Logging
open WoofWare.PawPrint

/// Why part of a method's behaviour is hidden from the analysis. Each is a place where an exception
/// the analysis cannot name may arise, which is what makes <c>Escapes.Unknown</c> true.
[<RequireQualifiedAccess>]
type Opacity =
    /// A <c>callvirt</c> to a method that may be overridden, on an object whose class the IL does not
    /// decide: the method named may not be the one that runs. That includes a <c>constrained.</c>
    /// call whose type does not decide it: an instance method's on a class that may be derived from,
    /// a class's method holding a slot that means something else in the table of the value type the
    /// prefix names (<c>ClassMethodOnValueType.Undecided</c>), or a type variable of a definition
    /// analysed for every instantiation at once.
    | VirtualCall
    /// <c>calli</c>, or a call through a dynamic method's scope: no metadata names the target. So too
    /// a field a dynamic method's scope names, whose type's initializer the access may run.
    | IndirectCall
    /// <c>ldvirtftn</c> of an interface method, which on an object implementing
    /// <c>IDynamicInterfaceCastable</c> asks that object's <c>GetInterfaceImplementation</c>.
    | InterfaceMethodPointer
    /// <c>constrained. ldftn</c> of a static virtual method on a type that does not decide the
    /// method it loads, such as a type variable of a definition analysed for every instantiation at
    /// once. Where that method is a reabstraction or is ambiguous, CoreCLR raises
    /// <c>EntryPointNotFoundException</c> or <c>AmbiguousImplementationException</c>, at the
    /// instruction itself in code compiled for an exact instantiation.
    | ConstrainedMethodPointer
    /// A <c>constrained.</c> call that boxes its receiver, of a byref-like type, to call a method the
    /// type does not declare, such as a default interface body. CoreCLR's JIT refuses to compile the
    /// method over it (<c>getBoxHelper</c> raises <c>InvalidProgramException</c>), before the method
    /// runs, which the analysis does not follow. So too such a call on a type variable that allows a
    /// byref-like type, in a definition analysed for every instantiation at once.
    | BoxedByRefLikeReceiver
    /// An <c>[UnmanagedCallersOnly]</c> method, or a method calling a P/Invoke, whose attribute
    /// (<c>[UnmanagedCallersOnly]</c>, or the P/Invoke's <c>[UnmanagedCallConv]</c>) CoreCLR cannot
    /// read the calling conventions of (<c>UnmanagedConventionRead</c>), which it reads as the JIT
    /// begins to compile the method or compiles the call (<c>getUnmanagedCallConv</c>), before any
    /// of the method runs. Where it cannot parse the blob,
    /// <c>IfFailThrow</c> raises the exception of the failure's <c>HRESULT</c>: a
    /// <c>COMException</c> for most such blobs, an <c>OverflowException</c> where a string runs past
    /// the blob's end; where the blob says
    /// <c>CallConvs = null</c>, CoreCLR reads the elements of the null array it parsed, and the
    /// compilation faults in the runtime, which kills the process.
    | UnreadableCallingConvention
    /// A value type in the signature of an <c>[UnmanagedCallersOnly]</c> method, which CoreCLR loads
    /// and then, before the JIT compiles the method, judges whether it needs marshalling
    /// (<c>PInvoke::MarshalingRequired</c>): by whether it is blittable or an enum, valid for
    /// generic marshalling and free of <c>Int128</c> (with runtime marshalling disabled, by whether
    /// it holds a reference or has an auto-layout part). Where it does, CoreCLR refuses to compile
    /// the method (<c>InvalidProgramException</c>) before any of it runs. The analysis does not
    /// model that verdict.
    | ValueTypeMarshallingVerdict
    /// A <c>constrained.</c> call on a value type of a class's method that cannot be overridden (it is
    /// final, on a sealed class, or not virtual), where the value type does not declare the method
    /// holding that slot itself and does not derive from the class: CoreCLR boxes the receiver and
    /// runs the method named on the box (<c>TryResolveConstraintMethodApprox</c>, methodtable.cpp;
    /// <c>CORINFO_BOX_THIS</c>, <c>getCallInfo</c>, jitinterface.cpp), whose body then reads it as an
    /// instance of a class it is not. So too where the method named is another value type's, which
    /// CoreCLR runs on the receiver's address. The analysis follows the method, but not what it does
    /// with such a receiver.
    | MistypedReceiver
    /// The method is <c>InternalCall</c>, <c>PInvoke</c> or runtime-provided: its behaviour is not IL.
    | NativeBody
    /// The method is abstract.
    | AbstractBody
    /// Something CoreCLR runs for an <c>[Intrinsic]</c> in place of its IL that the analysis does not
    /// model: the JIT's expansion of the method's call to itself where the JIT's tables do not say
    /// what the instruction raises (<c>HardwareInstruction.contract</c>) or the expansion is not
    /// recognised, a body the VM substitutes that is not transcribed (a
    /// <c>StructureMarshaler&lt;T&gt;</c> method's among them, <c>IntrinsicBody.StructMarshalling</c>),
    /// or a method the JIT replaces at every call to it (<c>IntrinsicBody.CallSiteExpansion</c>).
    | IntrinsicExpansion
    /// A MemberRef whose target turns on how a type variable of this method is instantiated. So too,
    /// in a definition analysed for every instantiation at once, a type the JIT loads in full, or a
    /// method it instantiates, over the method's type variables, where some instantiation of them
    /// may be one whose constraints CoreCLR rejects before the method runs.
    | DependsOnInstantiation
    /// An instruction CoreCLR's JIT checks access of, in a generic method or a method of a generic
    /// type, whose token instantiates over the method's own type variables a type or method whose
    /// constraints, or whose full load's, may reject them. CoreCLR checks those constraints with
    /// the variables themselves as the arguments (`CEEInfo::GetMethodForSecurity`), as it compiles
    /// any instantiation of the method, before the method runs, raising `TypeLoadException` or
    /// `VerificationException`; the analysis does not model that check.
    | ConstraintsOverTypeVariables
    /// A type the JIT loads in full, or a method it instantiates, whose constraints the analysis does
    /// not decide: an abstract argument can be cast to an interface constraint that declares abstract
    /// static virtual methods, each of which CoreCLR requires to resolve on the argument
    /// (`ConstraintVerdict.Undecided`), or the types the load reaches nest deeper than the analysis
    /// follows. Where the check fails, CoreCLR raises `TypeLoadException` or
    /// `VerificationException` before the method runs.
    | UndecidedConstraint
    /// A token naming a type in an assembly the analysis does not find. Before raising
    /// <c>FileNotFoundException</c>, the runtime runs the program's <c>AssemblyLoadContext.Resolving</c>
    /// and <c>AppDomain.AssemblyResolve</c> handlers, which may throw, or load an assembly whose
    /// code then runs.
    | UnresolvedAssembly
    /// A <c>throw</c> whose operand's type the analysis does not know.
    | UntypedThrow
    /// A <c>rethrow</c>: what it raises is what the enclosing handler caught.
    | Rethrow
    /// An instruction <c>OpcodeFaults</c> declines to classify.
    | UnmodelledOpcode
    /// An instruction whose token names a method, field or type that the visibility of each does
    /// not show the method may access (<c>MetadataAccess</c>), where the analysis may not assume it
    /// can (<c>Assumption.MembersAccessible</c>). Where it cannot, CoreCLR raises the access
    /// exception there through a QCall the analysis does not follow.
    | UndecidedAccess

/// A place the analysis cannot see through, from which something it cannot name may escape: one of
/// those that make an answer's <c>Escapes.Unknown</c> true (<c>EscapeAnalysis.unknownSources</c>).
type OpaqueSite =
    {
        /// The method the place is in.
        Method : MethodKey
        /// The offset of the instruction in the IL CoreCLR runs for the method: its own, or what the
        /// VM substitutes for it. <c>None</c> where the place is no instruction: the method has no IL
        /// the analysis follows (<c>NativeBody</c>, <c>AbstractBody</c>, <c>IntrinsicExpansion</c>),
        /// or binding what its body names fails in a way the analysis cannot follow, or the JIT may
        /// refuse to compile it, before any of it runs (<c>DependsOnInstantiation</c>,
        /// <c>UnresolvedAssembly</c>, <c>BoxedByRefLikeReceiver</c>, <c>UnreadableCallingConvention</c>,
        /// <c>ValueTypeMarshallingVerdict</c>, <c>ConstraintsOverTypeVariables</c>,
        /// <c>UndecidedConstraint</c>).
        Offset : int option
        Reason : Opacity
        /// The method the instruction's token names, where it names one. For a call, that need not
        /// be the method that runs.
        Names : MethodKey option
    }

/// What a call passes as one of its arguments, as far as the calling body says.
[<RequireQualifiedAccess>]
type internal SpelledArgument =
    /// What the evaluation stack holds there.
    | Value of StackValue
    /// The address of a location of the integer type `location` (`StackInteger.ofParameter`) that
    /// holds one of `holds` when the call is made, as the stack held them when the body stored
    /// them, and that only the method called can store to until it stores through a pointer or
    /// runs other code (`LocalValues.held`).
    | Pointer of location : PrimitiveType * holds : Set<SpelledInteger>

/// Something that happens outside a method's body, so that none of its own handlers can catch it.
[<RequireQualifiedAccess>]
type internal OutsideBodyFact =
    | Raises of ThrownType
    /// What happens turns on something the analysis cannot see, such as how a type variable of the
    /// method is instantiated when the JIT binds a member of it.
    | Opaque of Opacity
    /// The runtime runs this method, which has no type variables, passing it `passes`, `this`
    /// first, as the body's spellings say: another module's initializer, as binding a token first
    /// activates the module, or what the runtime runs to throw that initializer's own exception
    /// again, or the constructor it runs to make the exception a failure to bind raises, or what
    /// the JIT runs to take and release a synchronized method's monitor.
    | Runs of MethodKey * passes : SpelledArgument list

/// How a call's token instantiates the method it names, as the calling body spells it.
[<RequireQualifiedAccess>]
type internal CalleeSpelling =
    /// The callee has no type variables, so every call reaches the same method.
    | Fixed
    /// The token names a generic definition rather than an instantiation of it: a MethodDef or
    /// FieldDef of a generic type's member, or a MemberRef whose parent is a generic type named
    /// without arguments. Nothing says which instantiation runs.
    | Typical
    /// The token, of the calling body's assembly, whose type arguments, read against the calling
    /// body's instantiation, instantiate the callee: a MethodSpec, or a MemberRef whose parent is a
    /// TypeSpec or a non-generic type with a generic ancestor. For a type initializer that a field's
    /// access runs, the token is the field's, and its declaring type's arguments are the
    /// initializer's.
    | Spelled of MetadataToken

/// A method a body calls, and how the call instantiates it.
type internal Callee =
    {
        Callee : MethodKey
        Spelling : CalleeSpelling
    }

/// A call instruction, as far as the body says.
[<RequireQualifiedAccess>]
type internal CallSite =
    /// A call of this method.
    | Direct of Callee
    /// A call of a static virtual method, or a `callvirt` of a method a derived type may override,
    /// which a `constrained.` prefix naming this type, a token of the calling body's assembly,
    /// directs: the type decides what runs.
    | Constrained of constrainedType : MetadataToken * Callee
    /// A `callvirt` of a method a derived type may override, with no `constrained.` prefix, on an
    /// object that is one of these (`LocalFacts.Spellings` spells them): each one's class decides
    /// what runs on it.
    | Virtual of receivers : Set<SpelledObject> * Callee
    /// A method the runtime runs by itself at this instruction: the parameterless constructor of an
    /// exception it makes there (`ExceptionMaking`), a type initializer the instruction runs, or
    /// what it runs to throw again a type initializer's own exception
    /// (`Exception.InternalPreserveStackTrace`). So too a CoreLib method that a native method calls
    /// (`NativeContract.Runs`), in place of the native's body, and one that the JIT compiles the
    /// instruction to a call of, such as a cast's `CastHelpers` method. The analysis does not take
    /// what the instruction leaves on the stack to be what the method returns.
    | Runtime of Callee

/// A query whose answer the types it names decide, in an instance that says what they are.
[<RequireQualifiedAccess>]
type internal TypeQuery =
    /// Whether this type is a value type: `typeof(X).IsValueType`, which the JIT compiles to a
    /// constant, making none of its calls (`Assumption.JitOptimises`).
    | IsValueType of SpelledType
    /// Whether these two types are the same type where `equality`, and whether they are different
    /// types otherwise: `typeof(X) == typeof(Y)`, or `!=`, which the JIT compiles to a comparison
    /// of the types' handles, making none of its calls (`Assumption.JitOptimises`).
    | Identity of SpelledType * SpelledType * equality : bool
    /// Whether storage of this type holds a reference or a byref the GC tracks:
    /// `RuntimeHelpers.IsReferenceOrContainsReferences<T>()`, a constant however the method is
    /// compiled.
    | ContainsReferences of SpelledType

/// What a `brtrue` or `brfalse` tests of the value it pops.
[<RequireQualifiedAccess>]
type internal Tested =
    /// Whether it is null, for a value that is one of these objects, each spelled as of a
    /// reference type.
    | References of Set<SpelledObject>
    /// Whether it is zero, for a value that is one of these integers.
    | Integers of Set<SpelledInteger>
    /// Whether it is zero, for the answer to this query.
    | Types of TypeQuery

/// What a body's conditional branches test, from which an instance of the body works out which of
/// its offsets a run may execute (`ControlFlow.mayExecute`).
type internal Branching =
    {
        Body : MethodInstructions<TypeDefn>
        /// The Boolean that the instruction before a branch is known to push, by its offset, in
        /// every instance: what a capability query returns on this CPU, or whether values that are
        /// all null or zero, or all neither, are.
        Constants : Map<int, bool>
        /// What the `brtrue` or `brfalse` after the instruction at each offset tests, where only an
        /// instance can decide it.
        Tests : Map<int, Tested>
        /// What each `ret` returns, by its offset; a `jmp` returns `Unknown`.
        Returns : Map<int, StackValue>
    }

/// Which of CoreLib's `CastHelpers` methods the JIT calls for a cast to a type, by what the type is
/// (`CEEInfo::getCastingHelperStatic`, jitinterface.cpp): an `IsInstanceOf` one for `isinst`, and
/// a `ChkCast` one for `castclass`.
[<RequireQualifiedAccess>]
type internal CastTarget =
    /// A class or a value type, of a type with no variant type parameters, other than a
    /// `Nullable<T>` that CoreCLR does not ask about as `T`.
    | Class
    /// An interface with no variant type parameters.
    | Interface
    /// Anything else: `__Canon`, which shared code casts to for a type variable; a type with
    /// variant type parameters; an array; a `Nullable<T>` of shared code; or a type that is no
    /// class, such as a pointer.
    | Any

/// An instruction that CoreCLR's JIT compiles to a call of one of CoreLib's `CastHelpers` methods
/// for a type of some kinds (`JitHelper`).
[<RequireQualifiedAccess>]
type internal HelpedInstruction =
    | Isinst
    | Castclass
    /// An `unbox.any`, which CoreCLR compiles as a `castclass` where the type is a reference type.
    | UnboxAny
    | Unbox
    | Box
    /// An `ldelema` with no `readonly.` prefix.
    | Ldelema
    /// A `stelem` of a type. A `stelem.ref`, which names none, always calls `StelemRef`.
    | Stelem

/// One of CoreLib's `CastHelpers` methods, which `jithelpers.h` binds a JIT helper to, and which
/// the JIT calls for an instruction at tier 0, under MinOpts and in debuggable code. Where it
/// optimises, it calls the same method at fewer instructions.
[<RequireQualifiedAccess>]
type internal JitHelper =
    /// A cast's (`impCastClassOrIsInstToTree`, importer.cpp): a `ChkCast` one where `throwing`, for
    /// a `castclass` or an `unbox.any` of a reference type, and an `IsInstanceOf` one for an
    /// `isinst`.
    | Cast of throwing : bool * CastTarget
    /// `Unbox`, for an `unbox`, or an `unbox.any` of a value type, of any type but a
    /// `Nullable<T>` (`getUnBoxHelper`, jitinterface.cpp).
    | Unbox
    /// `Unbox_Nullable`, for an `unbox` or `unbox.any` of a `Nullable<T>`.
    | UnboxNullable
    /// `Box`, for a `box` of a value type that is no primitive type, enum, byref-like type or
    /// `Nullable<T>` (`impImportAndPushBox`, importer.cpp). The JIT boxes a primitive or an enum
    /// inline, and a byref-like type only where it folds the box away.
    | Box
    /// `Box_Nullable`, for a `box` of a `Nullable<T>`.
    | BoxNullable
    /// `LdelemaRef`, for an `ldelema` of a reference type (`CEE_LDELEMA`, importer.cpp).
    | LdelemaRef
    /// `StelemRef`, for a `stelem.ref`, or a `stelem` of a reference type (`CEE_STELEM_REF`,
    /// importer.cpp), even of null.
    | StelemRef

/// An instruction whose `CastHelpers` method only an instance of the body decides: one of a type
/// variable, or a cast to a `Nullable<T>` over one.
type internal GenericHelperSite =
    {
        /// The type, as the body spells it.
        Type : TypeDefn
        Instruction : HelpedInstruction
    }

/// A `constrained.` call of an instance method on a type that may be a value type. The JIT boxes
/// the call's receiver, as `box` does, where the type is a value type and the method that runs is
/// no method the type declares (`CORINFO_BOX_THIS`, `getCallInfo`, jitinterface.cpp), which only
/// an instance of the body decides.
type internal ReceiverBoxSite =
    {
        /// The type the prefix names, a token of the body's assembly.
        Constrained : MetadataToken
        /// The method the call names.
        Named : Callee
        /// Whether some instance of the body may name a byref-like type there, whose box the JIT
        /// refuses.
        MayBeByRefLike : bool
    }

/// Whether the JIT boxes the receiver of a `constrained.` call (`ReceiverBoxSite`), as an instance
/// of the body making the call decides.
[<RequireQualifiedAccess>]
type internal ReceiverBox =
    /// It passes the receiver's address to a method the type declares, or the reference there to
    /// one a class has.
    | Unboxed
    /// It boxes it, by this `CastHelpers` method, or inline (`impImportAndPushBox`, importer.cpp).
    | Boxed of JitHelper option
    /// It refuses to, the type being byref-like (`getBoxHelper`, jitinterface.cpp).
    | Refused
    /// Only an instance decides.
    | Undecided

/// What the JIT's resolving the token of an instruction after a `box` yields, as it matches those
/// instructions against a pattern to fold the box into (`BoxPattern`).
[<RequireQualifiedAccess>]
type internal TokenResolution<'resolved> =
    /// The token names this.
    | Resolves of 'resolved
    /// Resolving it raises what binding the token does (`bind`), before the JIT can refuse the box.
    | Raises
    /// The analysis cannot tell what the token names: it binds to an open generic definition, or
    /// to a type the analysis does not instantiate, such as a type variable in a definition
    /// analysed for every instantiation at once.
    | Undecided

/// An instruction the JIT refuses to compile a method over for the type its token names
/// (`ResolutionFault.TokenBinding`), raising before the method runs at all.
[<RequireQualifiedAccess>]
type internal RefusalSite =
    /// A `box` of a byref-like type that the JIT does not fold away (`BoxPattern`):
    /// `InvalidProgramException` (`CEE_BOX`, importer.cpp).
    | Box of boxed : SpelledType * BoxFollower<TokenResolution<SpelledType>>
    /// A `newarr` of a byref-like element type, whose array type does not load:
    /// `TypeLoadException` (`ClassLoader::CreateTypeHandleForTypeKey`, clsload.cpp).
    | Newarr of element : SpelledType

/// A check of constraints the JIT makes binding what a body names, before the body runs, that only
/// an instance of the body decides, since what it checks is spelled over the body's type variables.
[<RequireQualifiedAccess>]
type internal InstanceCheck =
    /// A type the JIT loads in full, spelled in this assembly: each instantiation its full load
    /// reaches must satisfy its constraints.
    | LoadsInFull of spelledIn : string * TypeDefn
    /// A type of the signature of the member a token of the body names, which the JIT loads in full,
    /// spelled in this assembly over the member's own type variables (its declaring type's and its
    /// own), which the instantiation the token gives the member decides.
    | LoadsInMember of spelledIn : string * TypeDefn * token : MetadataToken
    /// A method specification of this assembly the JIT instantiates the method of: its arguments
    /// must satisfy the method's constraints.
    | Instantiates of spelledIn : string * MethodSpecificationHandle

/// What one method body does by itself: the exceptions it raises, the places it cannot see
/// through, and the methods it calls, each at the IL offset where it happens, so that the body's
/// own exception clauses can be applied to all three alike.
type internal LocalFacts =
    {
        /// The constraint checks binding what the body names makes, before it runs, that only an
        /// instance decides (`instanceFacts`); what the rest of binding raises is in `OutsideBody`.
        InstanceChecks : InstanceCheck list
        Raises : (int * ThrownType) list
        /// Each instruction the body cannot see through, and the method its token names, if any.
        Opaque : (int * Opacity * MethodKey option) list
        /// Where an assumption's contract stands in for code the body runs: an exception the
        /// contract does not list may arise there if the assumption fails.
        Assumed : (int * Assumption) list
        /// Each instruction at which the body comes to rely on an assumption about what a value
        /// holds (`Assumption.concernsValues`), by offset: a run past it relies on the assumption,
        /// whatever handlers the body has.
        Relies : (int * Assumption) list
        /// Each call, and what it passes: each argument, `this` first, or for `newobj` the object
        /// it makes. Empty for a call the JIT emits in place of the body's own.
        Calls : (int * CallSite * SpelledArgument list) list
        /// Each `ldtoken` of a type that mentions a type variable, by its offset, with the type as
        /// the body spells it. An instance whose shared code looks the type up at run time raises
        /// there what the runtime's call making its `RuntimeType` does (`lookupRaises`), which
        /// `Raises` does not have.
        LookedUp : (int * SpelledType) list
        /// Each instruction whose `CastHelpers` method only an instance decides, by its offset.
        /// `Calls` has the runtime's call of each one some instance calls there.
        HelperSites : (int * GenericHelperSite) list
        /// Each `constrained.` call the JIT compiles that may box its receiver, by its offset.
        /// `Calls` has the runtime's call of each `CastHelpers` method a `box` of the type the
        /// prefix names may call there, where a run may make the call.
        ReceiverBoxes : (int * ReceiverBoxSite) list
        /// Each `box` and `newarr` the JIT compiles, by its offset, which it may refuse to
        /// compile the body over, as only the type its token names decides (`refusalAt`).
        Refusals : (int * RefusalSite) list
        /// Each `constrained. ldftn` of a static virtual method, with the type its prefix names, a
        /// token of the body's assembly. It calls nothing, but the type decides the method whose
        /// pointer it loads, and dispatch failing there may raise at the `ldftn`.
        ConstrainedPointers : (int * MetadataToken * Callee) list
        /// Each `rethrow`, with the `catch` or `filter` clause whose handler it is in: it re-raises
        /// what that clause caught.
        Rethrows : (int * ExceptionRegion) list
        Regions : ExceptionRegion list
        /// What happens outside the body, so that none of its own handlers can catch it: what
        /// binding the tokens the body names and the types of its locals and `catch` clauses
        /// throws before it runs (a member or type the assembly it is looked for in does not have,
        /// another module's initializer, which it runs, or a member of a type variable, which turns
        /// on the instantiation), what the JIT throws refusing to compile it for what a token
        /// names (`abstractCallees`; in an instance, `Refusals` and `ReceiverBoxes`), and what taking and releasing a
        /// synchronized method's monitor throws.
        /// A method with no IL the analysis follows is opaque here, as a whole.
        OutsideBody : Set<OutsideBodyFact>
        /// What compiling the body raises wherever the JIT compiles its code: in the method itself,
        /// which `OutsideBody` has too, and in any method the JIT inlines the body into, past every
        /// handler of that method, and of each method that one is inlined into in turn. These are what
        /// the loads fail with that the JIT may make asking for an `ldftn`'s address.
        RaisedWhereInlined : Set<OutsideBodyFact>
        /// The types the body spells for the objects on its evaluation stack, which a
        /// `SpelledObject` names by index.
        Spellings : SpelledType[]
        /// What the body's `ret`s return, over every path to them.
        Returns : StackValue
        /// What each `stfld` and `stsfld` the body may execute stores, by its offset.
        Stored : Map<int, StackValue>
        /// The arguments, by index, whose values can decide something of what the body does: what
        /// it passes to a call, a virtual call's receiver among them, what it returns, and what a
        /// branch tests.
        Consults : Set<int>
        /// What decides which offsets a run of one instance of the body may execute; `None` for a
        /// method with no IL the analysis follows.
        Branching : Branching option
    }

/// What a `rethrow` re-raises of one thing its clause's protected block raised.
[<RequireQualifiedAccess>]
type internal Rethrown =
    /// The clause cannot catch it.
    | Nothing
    | Thrown of ThrownType
    /// Something the analysis cannot name.
    | Unknown

/// Something a body raises, as a handler may catch it and a `rethrow` re-raise it.
[<RequireQualifiedAccess>]
type internal Raised =
    | Named of ThrownType
    /// Something the analysis cannot name.
    | Unnamed
    /// Something an assumption's contract leaves out, which arises only if the assumption fails:
    /// of the type a `rethrow` narrowed it to, if one did, or else unnamed.
    | Assumed of Assumption * narrowed : ThrownType option

/// What a call instruction's token names.
[<RequireQualifiedAccess>]
type internal CallTarget =
    | Method of Callee
    | ArrayAccessor of arrayType : TypeDefn * ArrayAccessor
    | Missing
    | TypeMissing
    | AssemblyMissing
    | DependsOnInstantiation

/// What CoreCLR runs when a method is called (`IntrinsicBody.classify`).
[<RequireQualifiedAccess>]
type internal Runs =
    /// IL: the method's own, or what CoreCLR's VM substitutes for it. `selfCall` is the JIT's
    /// expansion of the IL's call to the method itself, where it has one; that call is not a call.
    | Il of MethodInstructions<TypeDefn> * selfCall : JitExpansion option
    /// One of the runtime's own operations, the whole of the method.
    | Primitive of IntrinsicPrimitive
    /// Native code whose behaviour `NativeMethod` describes, the whole of the method.
    | Native of NativeMethod
    /// CoreLib's resource lookup, whose contract (`ResourceLookup.raises`) the analysis takes in
    /// place of its body.
    | ResourceLookup
    /// CoreLib's `Object.GetType`, whose contract (`ObjectGetType`) the analysis takes in place of
    /// its body.
    | ObjectGetType
    /// The method this assumption summarises, whose contract the analysis takes in place of its
    /// body.
    | Assumed of Assumption
    /// Nothing the analysis can see into.
    | Opaque of Opacity

/// Which instantiation of a method definition a summary describes.
[<RequireQualifiedAccess>]
type internal Instantiation =
    /// Every instantiation at once: the definition's type variables stand for any types, so a call
    /// whose target turns on one of them is opaque.
    | Open
    /// One instantiation: the declaring type's arguments, then the method's own.
    | Closed of typeArguments : ConcreteTypeHandle list * methodArguments : ConcreteTypeHandle list

/// The objects a value may be, as an instance of a method decides them.
type internal ObjectClasses =
    {
        /// The class of each object it may be: each type, and whether the object's class is
        /// exactly it, rather than it or one derived from it.
        Classes : Set<ConcreteTypeHandle * bool>
        /// Whether it may be null.
        MayBeNull : bool
    }

[<RequireQualifiedAccess>]
module internal ObjectClasses =

    /// No object, and not null either: a value nothing reaches, from which a join starts.
    let none : ObjectClasses =
        {
            Classes = Set.empty
            MayBeNull = false
        }

    /// Null, and no object.
    let onlyNull : ObjectClasses =
        {
            Classes = Set.empty
            MayBeNull = true
        }

    /// A value that may be anything either may be.
    let union (a : ObjectClasses) (b : ObjectClasses) : ObjectClasses =
        {
            Classes = Set.union a.Classes b.Classes
            MayBeNull = a.MayBeNull || b.MayBeNull
        }

/// What the calls reaching an instance of a method pass as one of its arguments.
[<RequireQualifiedAccess>]
type internal PassedValue =
    /// These objects, as an argument of any type but the integer types `StackInteger.ofParameter`
    /// names.
    | Objects of ObjectClasses
    /// These integers, as `ldarg` pushes the argument, of one of the integer types
    /// `StackInteger.ofParameter` names (`StackInteger.passedAs`).
    | Integers of Set<StackInteger>
    /// A managed pointer, as an argument of a byref type, to a location of the integer type
    /// `location` that holds one of `holds` on entry, as the stack held them when they were stored
    /// there (`StackInteger.loadedAs`), and that only the method can store to until it stores
    /// through a pointer or runs other code.
    | Pointer of location : PrimitiveType * holds : Set<StackInteger>

/// What a parameter takes, as far as what its callers pass it is followed.
[<RequireQualifiedAccess>]
type internal ParameterKind =
    /// An integer of this type (`StackInteger.ofParameter`).
    | Integer of PrimitiveType
    /// A managed pointer.
    | Byref
    /// Anything else, an object among them.
    | Other

/// One instantiation of a method definition, as the calls reaching it call it, which a summary is
/// computed for.
type internal MethodInstance =
    {
        Definition : MethodKey
        Arguments : Instantiation
        /// What these calls pass as some of the method's arguments, by index, `this` at 0. An
        /// argument absent here may be any value of its declared type, null among them.
        Passed : Map<int, PassedValue>
    }

/// What a call dispatched on a receiver's type does in one instance of the method making it.
[<RequireQualifiedAccess>]
type internal DispatchOutcome =
    /// It calls this instance.
    | Reaches of MethodInstance
    /// It calls this instance on a receiver that is no instance of the method's declaring type
    /// (`Opacity.MistypedReceiver`), whose body the analysis follows without knowing what it does
    /// with that receiver, or returns.
    | ReachesMistyped of MethodInstance
    /// It throws this instead of calling anything, which the runtime makes by running this
    /// constructor, whose own raises escape in its place.
    | Raises of ThrownType * made : MethodKey
    /// The receiver's type does not decide what runs.
    | Undecided

/// What the analysis is working out while it resolves what a call returns: a return that
/// depends on one of these is only its declared type.
[<RequireQualifiedAccess>]
type internal Pending =
    /// What an instance's `ret`s return.
    | Return of MethodInstance
    /// What the call at this offset of an instance returns.
    | Call of MethodInstance * offset : int
    /// What the writers of this field store in it.
    | Field of FieldKey

/// What one instance of a method calls, each at the IL offset of the call: the instances it
/// reaches, what calls raise instead of reaching anything, and the calls whose target the instance
/// does not decide. A `constrained. ldftn` counts as a call here, though it reaches nothing.
type internal InstanceCalls =
    {
        Callees : (int * MethodInstance) list
        Raises : (int * ThrownType) list
        /// Each call whose target the instance does not decide, and the method its token names.
        /// A `constrained. ldftn` whose target the instance does not decide is one.
        Undecided : (int * Opacity * MethodKey option) list
        /// What the runtime runs outside the body (`OutsideBodyFact.Runs`), past all its handlers.
        Outside : MethodInstance list
        /// Of `Outside`, what the runtime runs for what compiling the body raises wherever the JIT
        /// compiles its code (`LocalFacts.RaisedWhereInlined`).
        RaisedWhereInlined : MethodInstance list
    }

/// Where an assembly's own IL writes its fields.
type internal FieldWrites =
    {
        /// Each `stfld` and `stsfld` naming a field of the assembly by its definition, by the
        /// field's row: the method making it, and its offset.
        Stores : Map<int, (MethodDefinitionHandle * int) list>
        /// The rows of the fields whose address an `ldflda` or `ldsflda` of the assembly takes,
        /// through which anything may store to them, and of those whose name a member reference of
        /// the assembly spells, which may name one of them.
        Unfollowed : Set<int>
    }

/// Why the JIT's binding or loading of what a token names fails, which decides what the binding
/// throws.
[<RequireQualifiedAccess>]
type internal BindFailure =
    /// A type reference's assembly binds and declares no such type: `TypeLoadException`, made by its
    /// `(string, string, string, int)` constructor (`ExceptionMaking.TypeLoadConstructor`).
    | TypeAbsent
    /// No assembly is found for a type reference: `FileNotFoundException`, unless one of the
    /// program's resolve handlers throws or supplies one (`Opacity.UnresolvedAssembly`).
    | AssemblyUnavailable
    /// An instantiation the load reaches has an argument CoreCLR admits in no instantiation
    /// (`ConstraintSatisfaction.malformedArgument`): `TypeLoadException`, made by its
    /// `(string, string, string, int)` constructor, as `ClassLoader::CreateTypeHandleForTypeKey`
    /// throws it.
    | MalformedTypeInstantiation
    /// An instantiation the load reaches has an argument its type parameter's constraints reject
    /// (`SatisfiesClassConstraints`): `TypeLoadException`, made by its parameterless constructor
    /// (`COMPlusThrow`, `EEMessageException`).
    | ConstraintViolation
    /// A method specification instantiates its method with an argument CoreCLR admits in no
    /// instantiation: `BadImageFormatException`, made by its parameterless constructor
    /// (`COMPlusThrowHR`).
    | MalformedMethodInstantiation
    /// A method specification instantiates its method with an argument the method's constraints
    /// reject (`SatisfiesMethodConstraintsForInstantiation`): `VerificationException`, made by its
    /// parameterless constructor.
    | MethodConstraintViolation
    /// The analysis does not decide whether an instantiation the load reaches, or a method
    /// specification, satisfies its constraints (`Opacity.UndecidedConstraint`).
    | UndecidedConstraint

/// An escape analysis in progress: the assemblies loaded so far and every answer computed so far.
/// Immutable; each query returns the state to ask the next one of.
type EscapeAnalysisState =
    private
        {
            LoggerFactory : ILoggerFactory
            RuntimeDirs : string list
            /// The architecture the JIT compiles for, and the CPU it compiles for: what a capability
            /// query answers, and whether a hardware instruction is emitted or throws.
            Target : JitTarget
            Profile : HardwareIntrinsicsProfile
            /// The runtime whose JIT compiles the code: the one the CoreLib belongs to.
            Runtime : EmulatedRuntime
            /// The assumptions the analysis may take a contract from in place of a method's body.
            Assumptions : Set<Assumption>
            /// Whether a callee's summary takes the classes of the objects its caller passes it
            /// (`MethodInstance.Passed`), rather than every call sharing the one its declared
            /// parameter types give.
            ArgumentContexts : bool
            /// The assemblies loaded so far, the concrete types instantiated so far, and what the type
            /// system has memoised about them.
            TypeSystem : TypeSystemState
            BaseTypes : BaseClassTypes<DumpedAssembly>
            Facts : Map<MethodKey, LocalFacts>
            /// What each instance summarised so far calls.
            InstanceCalls : Map<MethodInstance, InstanceCalls>
            /// Whether dispatch on a receiver of each type definition can be resolved, as far as it
            /// has been asked (`dispatchBinds`).
            DispatchBinds : Map<ResolvedTypeIdentity, bool>
            /// The first type reference CoreCLR's full load of each type definition reaches that
            /// names nothing (`TypeLoad.loadMiss`), as far as it has been asked.
            TypeLoads : Map<ResolvedTypeIdentity, TypeLoad.LoadMiss option>
            /// The type definitions loads so far have brought in full without meeting a miss, which a
            /// load from them passes over.
            LoadedDefinitions : TypeLoad.LoadedDefinitions
            /// What CoreCLR's check of the constraints of each instantiation the full load of each
            /// type reaches finds (`closureConstraintFailure`), as far as it has been asked; `None`
            /// where every one satisfies its constraints.
            ClosureConstraints : Map<ConcreteTypeHandle, BindFailure option>
            /// Whether the full load of an instantiation of each generic definition may break a
            /// constraint through its type parameters (`loadMayBreakThroughParameters`), as far as
            /// it has been asked.
            ParameterBreaks : Map<ResolvedTypeIdentity, bool>
            /// What CoreCLR's checks find of the closed instantiations the full load of an
            /// instantiation of each generic definition reaches (`definitionClosedPartsFailure`), as
            /// far as it has been asked.
            ClosedPartFailures : Map<ResolvedTypeIdentity, BindFailure option>
            /// What the constraint checks of each instance's body that only the instance decides
            /// raise (`instanceConstraintFacts`), by the instance's definition and arguments, as far
            /// as it has been asked: the same whatever its callers pass it.
            InstanceConstraints : Map<MethodKey * Instantiation, Set<OutsideBodyFact>>
            /// What CoreCLR raises instantiating each generic method with each declaring type's
            /// arguments and its own (`methodInstantiationFailure`), as far as it has been asked.
            MethodInstantiations :
                Map<MethodKey * ConcreteTypeHandle list * ConcreteTypeHandle list, BindFailure option>
            Summaries : Map<MethodInstance, Escapes>
            /// What may escape every method each instance summarised so far is inlined into, past
            /// every handler: what compiling the instance, or a method it calls, raises wherever the
            /// JIT compiles that code (`LocalFacts.RaisedWhereInlined`). Its summary has it too.
            /// An instance absent here lets nothing escape so.
            InlinedSummaries : Map<MethodInstance, Escapes>
            /// Each type definition's base type, as far as it has been asked; `None` at the root.
            Bases : Map<ResolvedTypeIdentity, ResolvedTypeIdentity option>
            /// The objects each instance returns, as far as it has been asked (`returnsOf`);
            /// `None` where only the declared return type says.
            Returns : Map<MethodInstance, ObjectClasses option>
            /// The objects each field holds, as far as it has been asked (`fieldContents`); `None`
            /// where only its declared type says.
            FieldContents : Map<FieldKey, ObjectClasses option>
            /// Where the IL of each assembly asked about so far writes its fields (`fieldWrites`).
            FieldWrites : Map<string, FieldWrites>
            /// The facts of the IL of each method asked about so far whose body an answer takes a
            /// contract for in place of that IL (`writerFacts`).
            OwnFacts : Map<MethodKey, LocalFacts>
            /// The facts of each instance asked about so far (`instanceFacts`).
            InstanceFacts : Map<MethodInstance, LocalFacts>
        }

/// <summary>
/// Which exceptions can escape a method, computed from its IL and that of everything it calls,
/// across assemblies, without running any of it.
/// </summary>
/// <remarks>
/// The answer is an over-approximation: every exception a run of the method can let escape is in
/// it, either named in <c>Escapes.Types</c> or covered by <c>Escapes.Unknown</c>. That includes
/// the faults the runtime raises by itself (<c>OpcodeFaults</c>), resource exhaustion among them,
/// so almost every method can escape <c>StackOverflowException</c>; a report that wants to drop
/// those does so knowingly.
///
/// The runtime makes most of the exceptions it raises by itself by running the exception's
/// parameterless constructor, which looks up its message, so what that constructor can raise is in
/// the answer too, a failure to bind a member or to dispatch a call among them, and an array
/// allocation's <c>OutOfMemoryException</c> for lengths too large to allocate. Where it then looks
/// up the message by a resource name, as for a multidimensional array constructor's
/// <c>ArgumentOutOfRangeException</c>, what the lookup can raise is in the answer too. A failed type
/// binding's <c>TypeLoadException</c> it makes by a constructor taking the type's name, which
/// formats its message with <c>String.Format</c>, so where a type fails to bind, what that
/// constructor can raise is in the answer too. The analysis cannot yet tell which formatting
/// methods <c>String.Format</c> calls on the strings it is given, so that answer is unknown.
///
/// A type initializer runs where CoreCLR's <c>CEEInfo::initClass</c> runs it: at an access to a
/// static field of its type, and at a call to a method of its type that is static, a constructor, or
/// an instance method of a value type or an interface, unless the type is marked
/// <c>beforefieldinit</c>. There, should it fail, the runtime raises a
/// <c>TypeInitializationException</c>, or the initializer's own exception should making that fail,
/// which it throws again, running <c>Exception.InternalPreserveStackTrace</c> on it. So the answer
/// has the wrapper, what the initializer raises, and what that method raises. A module's
/// initializer is a type initializer too, which runs as code first binds into its module: before
/// the body that binds into it runs, so past all its handlers, or where a call that dispatch lands
/// in the module is made.
///
/// An interface cast or an array store calls <c>IDynamicInterfaceCastable.IsInterfaceImplemented</c>
/// on an object whose class implements it. Where the analysis follows the cast helpers' fallback,
/// which asks it, as from <c>castclass</c> and <c>isinst</c>, that it throws nothing but the
/// <c>InvalidCastException</c> its documentation asks for is
/// <c>Assumption.DynamicCastsAsDocumented</c>; from <c>stelem.ref</c>, whose helper the analysis
/// does not yet follow, it is assumed without being listed.
///
/// The IL is assumed to be well typed: a value held where the IL spells a type (an argument, a
/// local, a field, a call's result) is of that type, as the JIT assumes when it devirtualises. So a
/// <c>callvirt</c> on an object of a sealed class runs that class's implementation, and a
/// <c>throw</c> of a value of a static type raises that type or a subtype. An object a call
/// returns is of the classes the callee's <c>ret</c>s return in the instance the call reaches.
///
/// A method's summary is computed for each combination of objects its callers pass it, as the
/// arguments its body makes a virtual call on, returns, passes on or tests: an object passed is of
/// the classes the caller says, so a helper called with a <c>new Dog()</c> runs <c>Dog</c>'s
/// overrides there, and is null or not as the caller says, so a <c>brtrue</c> or <c>brfalse</c>
/// straight after loading it goes only the way that decides. An integer constant passed as an
/// argument of an integer type decides such a branch the same way, once converted to the
/// parameter's type. Asked about by itself, a method may be passed any value its parameter types
/// admit, null among them, and so may one whose body stores to the argument or takes its address.
///
/// The native libraries the framework ships with its CoreLib (<c>FrameworkShim</c>) are assumed to
/// be present and to export what its P/Invokes import, so that calling one of their functions
/// raises nothing. Were one missing, the runtime would run the program's
/// <c>ResolvingUnmanagedDll</c> handlers, which could throw anything.
///
/// CoreLib's lookup of its own resource strings, <c>SR.InternalGetResourceString</c>, which gives
/// every CoreLib exception its message, is assumed to raise only <c>OutOfMemoryException</c>,
/// <c>TypeInitializationException</c>, <c>NullReferenceException</c> (for a null key),
/// <c>ThreadInterruptedException</c> (while it waits for its lock) and
/// <c>StackOverflowException</c>. That holds when only CoreLib writes CoreLib's private static
/// fields, so that the lookup's resource manager is CoreLib's own; when the runtime is installed
/// completely and intact, so that its resources files read; and when every culture the lookup
/// walks (the current UI culture and each one its <c>Parent</c> chain reaches) that is of a
/// <c>CultureInfo</c> subclass the program defines behaves as CoreLib's own <c>CultureInfo</c>
/// would for that culture: its overrides raise nothing, its <c>Name</c> is a valid culture name, its
/// <c>Parent</c> chain ends at the invariant culture, and it changes no culture state. Reading the
/// code could not establish these instead, and without them, constructing almost any CoreLib
/// exception would be unknown. Constructing one runs type initializers, though, <c>SR</c>'s and
/// those the lookup runs, and should one fail, the runtime may throw its exception again as above.
/// <c>SR</c>'s initializer, and the stack trace that reads, are unknown unless the caller allows
/// <c>Assumption.CoreLibTypeInitializers</c> and <c>Assumption.StackTracePreserved</c>; allowed no
/// assumption, constructing almost any CoreLib exception is unknown still.
///
/// CoreLib's <c>Object.GetType</c> is taken to raise only what allocating its result raises
/// (<c>ObjectGetType</c>): its slow path, a QCall, finishes loading a type that is not fully
/// loaded, but an object's type always is.
///
/// A caller may allow further assumptions (<c>Assumption</c>), each of which replaces a method's
/// body with a contract, or code with what the JIT compiles it to; an answer lists those it relied
/// on in <c>Escapes.Assumes</c>.
///
/// That holds for assemblies that agree with each other. A member or type that a body names and
/// the loaded assembly it is looked for in lacks is reported, as the exception binding it throws,
/// and signals that they do not; so too a type that CoreCLR's full load of a type the JIT loads
/// reaches: its base types, its interfaces, the types of its value-type fields and, outside CoreLib,
/// the constraints of its type parameters and of its generic methods' (<c>TypeLoad</c>). So too an
/// instantiation the JIT loads or makes, or one such a load reaches, whose arguments a constraint
/// added since rejects: <c>TypeLoadException</c> for a type, <c>VerificationException</c> for a
/// method, before the method runs. Over a generic method's own type variables, where CoreCLR checks
/// them as themselves, or in a definition analysed for every instantiation at once, such a check is
/// unknown. A load that misses is reported as the miss, without the order of the rest of the load:
/// CoreCLR may check an instantiation before a later part of one load misses. A member that has
/// become inaccessible to its caller is not checked.
///
/// Each method is taken to be compiled by the JIT when it is first called. Code compiled ahead of
/// time (ReadyToRun: the shared framework's, and a program's published that way) binds what it
/// names as it runs instead, so that a failure reported as escaping a method past all its handlers
/// is raised there at the instruction, where the method's own handlers can catch it, or not at all
/// on a path that does not run. CoreCLR also loads no constraints in a ReadyToRun image that skips
/// type validation, which the analysis does not tell from any other assembly outside CoreLib: it
/// loads them, which can only add exceptions that may escape.
///
/// A generic method's IL is read once, since every instantiation runs the same IL, but its summary
/// is computed for each closed instantiation a call reaches. An instantiation decides what a
/// <c>constrained.</c> call on one of the method's type variables runs when it makes that a value
/// type or a sealed class, or the method called is a static virtual, which no derived class's
/// instance can receive. Asked about by itself, a generic definition stands for every
/// instantiation at once, so such a call is opaque in it. An instantiation whose type arguments nest
/// more than eight deep is analysed as its definition instead, so that a method calling itself at
/// ever deeper instantiations reaches finitely many.
/// </remarks>
[<RequireQualifiedAccess>]
module EscapeAnalysis =

    /// Begin an analysis over the assemblies `context` has loaded, loading any others it needs from
    /// `runtimeDirs`, of what they do when CoreCLR's JIT compiles them for `target` on a CPU
    /// `profile` describes. The JIT is that of the runtime `context`'s CoreLib belongs to, which
    /// must be one PawPrint supports. `profile` must not mark as supported a class of another
    /// target's instruction sets: CoreLib gives those bodies that throw, whatever the CPU. The
    /// analysis takes the contract of each of `assumptions` in place of the method it summarises;
    /// with none, every answer follows from the code alone.
    let create
        (loggerFactory : ILoggerFactory)
        (runtimeDirs : string seq)
        (target : JitTarget)
        (profile : HardwareIntrinsicsProfile)
        (assumptions : Set<Assumption>)
        (context : TypeConcretization.ConcretizationContext<DumpedAssembly>)
        : EscapeAnalysisState
        =
        let foreign =
            JitTarget.all
            |> List.filter (fun other -> other <> target)
            |> List.map JitTarget.instructionSetNamespace
            |> Set.ofList

        for intrinsicClass in Set.union profile.IsSupported profile.IsHardwareAccelerated do
            if foreign.Contains intrinsicClass.Namespace then
                invalidArg
                    (nameof profile)
                    $"The profile marks %O{intrinsicClass} supported, but the JIT compiles for %A{target}"

        let runtime =
            match EmulatedRuntime.classify context.BaseTypes.Corelib with
            | Ok runtime -> runtime
            | Error unsupported ->
                invalidArg
                    (nameof context)
                    $"The CoreLib %s{unsupported.CoreLib} is .NET major %d{unsupported.FoundMajor}, whose JIT the analysis does not describe; it describes %s{EmulatedRuntime.describeSupported ()}"

        {
            LoggerFactory = loggerFactory
            RuntimeDirs = List.ofSeq runtimeDirs
            Target = target
            Profile = profile
            Runtime = runtime
            Assumptions = assumptions
            ArgumentContexts = true
            TypeSystem =
                { TypeSystemState.Empty with
                    _LoadedAssemblies = context.LoadedAssemblies
                    ConcreteTypes = context.ConcreteTypes
                }
            BaseTypes = context.BaseTypes
            Facts = Map.empty
            InstanceCalls = Map.empty
            DispatchBinds = Map.empty
            TypeLoads = Map.empty
            LoadedDefinitions = TypeLoad.nothingLoaded
            ClosureConstraints = Map.empty
            ParameterBreaks = Map.empty
            ClosedPartFailures = Map.empty
            InstanceConstraints = Map.empty
            MethodInstantiations = Map.empty
            Summaries = Map.empty
            InlinedSummaries = Map.empty
            Bases = Map.empty
            Returns = Map.empty
            FieldContents = Map.empty
            FieldWrites = Map.empty
            OwnFacts = Map.empty
            InstanceFacts = Map.empty
        }

    /// The analysis `state` begins, with nothing summarised yet, summarising every call of a method
    /// alike, whatever its caller passes it.
    let internal withoutArgumentContexts (state : EscapeAnalysisState) : EscapeAnalysisState =
        { state with
            ArgumentContexts = false
            InstanceCalls = Map.empty
            Summaries = Map.empty
            InlinedSummaries = Map.empty
            Returns = Map.empty
            FieldContents = Map.empty
            InstanceFacts = Map.empty
        }

    let private assemblyOf (state : EscapeAnalysisState) (fullName : string) : DumpedAssembly =
        state.TypeSystem._LoadedAssemblies.ByDefinitionName fullName

    let private withAssemblies (state : EscapeAnalysisState) (assemblies : LoadedAssemblies) : EscapeAnalysisState =
        { state with
            TypeSystem =
                { state.TypeSystem with
                    _LoadedAssemblies = assemblies
                }
        }

    /// The CoreLib type of this namespace and name.
    let private corelibType (state : EscapeAnalysisState) (ns : string) (name : string) : ResolvedTypeIdentity =
        match state.BaseTypes.Corelib.TryGetTopLevelTypeDef ns name with
        | Some ty -> ty.Identity
        | None -> failwith $"CoreLib declares no %s{ns}.%s{name}"

    /// The CoreLib exception type of this name, which the runtime raises by itself.
    let private corelibException (state : EscapeAnalysisState) (name : string) : ResolvedTypeIdentity =
        corelibType state "System" name

    /// The parameterless instance constructor of a CoreLib exception type.
    let private parameterlessConstructor (state : EscapeAnalysisState) (name : ExceptionName) : MethodKey =
        let corelib = state.BaseTypes.Corelib

        let found =
            corelib.TryGetTopLevelTypeDef name.Namespace name.Name
            |> Option.toList
            |> List.collect (fun ty -> List.ofSeq ty.Methods)
            |> List.filter (fun m -> m.Name = ".ctor" && not m.IsStatic && m.Signature.ParameterTypes.IsEmpty)
            |> List.choose (fun m -> m.TryMetadata)

        match found with
        | [ facts ] -> MethodKey.make corelib facts.Handle
        | found ->
            failwith
                $"EscapeAnalysis: expected %s{corelib.DefinitionFullName} to define one %O{name}(), found %d{found.Length}"

    /// The CoreLib method a native method calls (`NativeContract.Runs`), and what it passes: the one
    /// of its name taking an argument per one the call spells. A CoreLib may declare others of the
    /// name, as .NET 11's declares an `UnmanagedCallersOnly` overload of
    /// `DynamicInterfaceCastableHelpers.IsInterfaceImplemented`, which its VM calls instead.
    let private nativeCall (state : EscapeAnalysisState) (call : ManagedCall) : MethodKey * SpelledArgument list =
        let corelib = state.BaseTypes.Corelib

        let ns, name =
            match call.DeclaringType.LastIndexOf '.' with
            | -1 -> "", call.DeclaringType
            | dot -> call.DeclaringType.Substring (0, dot), call.DeclaringType.Substring (dot + 1)

        let found =
            corelib.TryGetTopLevelTypeDef ns name
            |> Option.toList
            |> List.collect (fun ty ->
                ty.Methods
                |> List.filter (fun m ->
                    m.Name = call.Method
                    && (if m.IsStatic then 0 else 1) + m.Signature.ParameterTypes.Length = call.Arguments.Length
                )
                |> List.choose (fun m -> m.TryMetadata |> Option.map (fun facts -> ty, m, facts))
            )

        match found with
        | [ ty, method, facts ] when ty.Generics.IsEmpty && method.Signature.GenericParameterCount = 0 ->
            let passes =
                call.Arguments
                |> List.map (fun argument ->
                    match argument with
                    | NativeArgument.Unnamed -> SpelledArgument.Value StackValue.Unknown
                )

            MethodKey.make corelib facts.Handle, passes
        | found ->
            failwith
                $"EscapeAnalysis: expected %s{corelib.DefinitionFullName} to define one %s{call.DeclaringType}::%s{call.Method}, without type variables, of %d{call.Arguments.Length} arguments, found %d{found.Length} of that name and arity"

    /// The method CoreCLR runs to look up the message of an exception it makes with one by resource
    /// name (`ExceptionMaking.ResourceMessage`): that runtime's `NativeCall.ResourceMessage`.
    let private resourceMessageLookup (state : EscapeAnalysisState) : MethodKey =
        nativeCall state (NativeCall.resolve state.Runtime NativeCall.ResourceMessage)
        |> fst

    /// Whether the analysis's caller allows `assumption`.
    let internal allows (state : EscapeAnalysisState) (assumption : Assumption) : bool =
        state.Assumptions.Contains assumption

    /// The managed code a native method calls (`NativeContract.Runs`), and what it passes.
    let internal nativeCallee (state : EscapeAnalysisState) (call : NativeCall) : MethodKey * SpelledArgument list =
        nativeCall state (NativeCall.resolve state.Runtime call)

    /// The one static method of CoreLib's `ns.typeName` of this name that takes `arity` arguments.
    let private corelibStatic
        (state : EscapeAnalysisState)
        (ns : string)
        (typeName : string)
        (name : string)
        (arity : int)
        : MethodKey
        =
        let corelib = state.BaseTypes.Corelib

        let found =
            corelib.TryGetTopLevelTypeDef ns typeName
            |> Option.toList
            |> List.collect (fun ty -> List.ofSeq ty.Methods)
            |> List.filter (fun m -> m.Name = name && m.IsStatic && m.Signature.ParameterTypes.Length = arity)
            |> List.choose (fun m -> m.TryMetadata)

        match found with
        | [ facts ] -> MethodKey.make corelib facts.Handle
        | found ->
            failwith
                $"EscapeAnalysis: expected %s{corelib.DefinitionFullName} to define one static %s{ns}.%s{typeName}.%s{name} of %d{arity} arguments, found %d{found.Length}"

    /// The `(string, string, string, int)` instance constructor of a CoreLib exception type, which
    /// CoreCLR runs to make a `TypeLoadException` (`ExceptionMaking.TypeLoadConstructor`).
    let private typeLoadConstructor (state : EscapeAnalysisState) (name : ExceptionName) : MethodKey =
        let corelib = state.BaseTypes.Corelib
        let string = TypeDefn.PrimitiveType PrimitiveType.String

        let found =
            corelib.TryGetTopLevelTypeDef name.Namespace name.Name
            |> Option.toList
            |> List.collect (fun ty -> List.ofSeq ty.Methods)
            |> List.filter (fun m ->
                m.Name = ".ctor"
                && not m.IsStatic
                && m.Signature.ParameterTypes = [
                    string
                    string
                    string
                    TypeDefn.PrimitiveType PrimitiveType.Int32
                ]
            )
            |> List.choose (fun m -> m.TryMetadata)

        match found with
        | [ facts ] -> MethodKey.make corelib facts.Handle
        | found ->
            failwith
                $"EscapeAnalysis: expected %s{corelib.DefinitionFullName} to define one %O{name}(string, string, string, int), found %d{found.Length}"

    /// `RuntimeWrappedException`'s `(object)` constructor, which CoreCLR runs to wrap a thrown object
    /// that is not an exception (`WrapNonCompliantException`).
    let private wrapperConstructor (state : EscapeAnalysisState) : MethodKey =
        let corelib = state.BaseTypes.Corelib

        let found =
            corelib.TryGetTopLevelTypeDef "System.Runtime.CompilerServices" "RuntimeWrappedException"
            |> Option.toList
            |> List.collect (fun ty -> List.ofSeq ty.Methods)
            |> List.filter (fun m ->
                m.Name = ".ctor"
                && not m.IsStatic
                && m.Signature.ParameterTypes = [ TypeDefn.PrimitiveType PrimitiveType.Object ]
            )
            |> List.choose (fun m -> m.TryMetadata)

        match found with
        | [ facts ] -> MethodKey.make corelib facts.Handle
        | found ->
            failwith
                $"EscapeAnalysis: expected %s{corelib.DefinitionFullName} to define one RuntimeWrappedException(object), found %d{found.Length}"

    /// The kind of member whose handle an `ldtoken` loads.
    [<RequireQualifiedAccess>]
    type private MemberHandle =
        | Method
        | Field

    /// The kind of member whose handle `ldtoken` of `token` in `assembly` loads; `None` where it
    /// loads a type's.
    let private memberHandleOf (assembly : DumpedAssembly) (token : MetadataToken) : MemberHandle option =
        match token with
        | MetadataToken.MethodDef _
        | MetadataToken.MethodSpecification _ -> Some MemberHandle.Method
        | MetadataToken.FieldDefinition _ -> Some MemberHandle.Field
        | MetadataToken.MemberReference handle ->
            match assembly.Members.[handle].Signature with
            | MemberSignature.Method _ -> Some MemberHandle.Method
            | MemberSignature.Field _ -> Some MemberHandle.Field
        | MetadataToken.TypeDefinition _
        | MetadataToken.TypeReference _
        | MetadataToken.TypeSpecification _ -> None
        | other -> failwith $"An ldtoken in %s{assembly.DefinitionFullName} names %O{other}"

    /// `FromPtr(IntPtr)` of `RuntimeMethodInfoStub` or `RuntimeFieldInfoStub`, which the JIT calls in
    /// place of `ldtoken` of a method or a field to make the stub its handle holds: the methods that
    /// `CORINFO_HELP_METHODDESC_TO_STUBRUNTIMEMETHOD` and `CORINFO_HELP_FIELDDESC_TO_STUBRUNTIMEFIELD`
    /// are bound to (jithelpers.h).
    let private stubMaker (state : EscapeAnalysisState) (handle : MemberHandle) : MethodKey =
        let corelib = state.BaseTypes.Corelib

        let stubType =
            match handle with
            | MemberHandle.Method -> "RuntimeMethodInfoStub"
            | MemberHandle.Field -> "RuntimeFieldInfoStub"

        let found =
            corelib.TryGetTopLevelTypeDef "System" stubType
            |> Option.toList
            |> List.collect (fun ty -> List.ofSeq ty.Methods)
            |> List.filter (fun m ->
                m.Name = "FromPtr"
                && m.IsStatic
                && m.Signature.ParameterTypes = [ TypeDefn.PrimitiveType PrimitiveType.IntPtr ]
            )
            |> List.choose (fun m -> m.TryMetadata)

        match found with
        | [ facts ] -> MethodKey.make corelib facts.Handle
        | found ->
            failwith
                $"EscapeAnalysis: expected %s{corelib.DefinitionFullName} to define one %s{stubType}.FromPtr(IntPtr), found %d{found.Length}"

    /// The size of a value of `primitive` for the JIT compiling for `target`, where CoreCLR takes it
    /// to be a primitive type (`CorIsPrimitiveType` of `GetVerifierCorElementType`); `None` for a
    /// string or an object, which it takes to be a class, and for a typed reference.
    let private primitiveSize (target : JitTarget) (primitive : PrimitiveType) : int option =
        match primitive with
        | PrimitiveType.Boolean
        | PrimitiveType.SByte
        | PrimitiveType.Byte -> Some 1
        | PrimitiveType.Char
        | PrimitiveType.Int16
        | PrimitiveType.UInt16 -> Some 2
        | PrimitiveType.Int32
        | PrimitiveType.UInt32
        | PrimitiveType.Single -> Some 4
        | PrimitiveType.Int64
        | PrimitiveType.UInt64
        | PrimitiveType.Double -> Some 8
        | PrimitiveType.IntPtr
        | PrimitiveType.UIntPtr ->
            match target with
            | JitTarget.Arm64
            | JitTarget.X64 -> Some 8
        | PrimitiveType.String
        | PrimitiveType.Object
        | PrimitiveType.TypedReference -> None

    /// What the runtime raises where it raises the CoreLib exception of this full name by itself,
    /// making it as `making` says: the exception, and the methods it runs to make it, whose own
    /// raises escape there too.
    let private runtimeRaises
        (state : EscapeAnalysisState)
        (fullName : string)
        (making : ExceptionMaking)
        : ThrownType * MethodKey list
        =
        let name =
            match ExceptionName.parse fullName with
            | Some name -> name
            | None -> failwith $"EscapeAnalysis: %s{fullName} is not an exception type's full name"

        let thrown = ThrownType.Exactly (corelibType state name.Namespace name.Name)

        match making with
        | ExceptionMaking.ParameterlessConstructor -> thrown, [ parameterlessConstructor state name ]
        | ExceptionMaking.ResourceMessage ->
            match state.Runtime with
            | EmulatedRuntime.Net10 -> thrown, [ parameterlessConstructor state name ; resourceMessageLookup state ]
            // .NET 11 looks the message up through an `[UnmanagedCallersOnly]` method of CoreLib's
            // (`Environment.GetResourceString`), and ends the process where the constructor or the
            // lookup raises (`TestExceptionMaking`, parked on .NET 11). The analysis takes the
            // constructor's raises to escape, as it does for a making by the constructor alone.
            | EmulatedRuntime.Net11 -> thrown, [ parameterlessConstructor state name ]
        | ExceptionMaking.TypeLoadConstructor -> thrown, [ typeLoadConstructor state name ]
        | ExceptionMaking.Preallocated -> thrown, []
        | ExceptionMaking.InitializerFailure ->
            failwith
                $"BUG: %s{fullName} is raised for a type initializer, which the site running it accounts for (`initializerFailure`)"

    /// What the runtime raises where it raises the CoreLib exception of this full name by itself,
    /// making it by running its parameterless constructor (`EEException::CreateThrowable`): the
    /// exception, and that constructor.
    let private constructedRaise (state : EscapeAnalysisState) (fullName : string) : ThrownType * MethodKey =
        match runtimeRaises state fullName ExceptionMaking.ParameterlessConstructor with
        | thrown, [ made ] -> thrown, made
        | thrown, made ->
            failwith $"BUG: making %O{thrown} by its parameterless constructor runs %d{made.Length} methods"

    /// What the JIT's own call of `RuntimeTypeHandle.GetRuntimeTypeFromHandle` raises, which the
    /// analysis takes in place of the method's IL: the `OutOfMemoryException` of allocating the
    /// `RuntimeType`, which CoreCLR raises preallocated, and the `StackOverflowException` of making
    /// a call. The JIT makes the call (`CORINFO_HELP_TYPEHANDLE_TO_RUNTIMETYPE` and
    /// `CORINFO_HELP_GETSYNCFROMCLASSHANDLE`, jithelpers.h) only with a type that is fully loaded:
    /// one its code's generic dictionary holds, which `Dictionary::PopulateEntry` loads to
    /// `CLASS_LOADED`; a type argument of the exact class or method its code runs for, which a full
    /// load of that class or method loads fully too; or that class itself, which the prestub loads
    /// fully before the code runs. So the method's slow path, the QCall
    /// `RuntimeTypeHandle_GetRuntimeTypeFromHandleSlow`, has nothing left to load
    /// (`MethodTable::CheckRestore`) and only allocates. It refuses only a runtime-async
    /// continuation, on .NET 11, which is of no type a token names. CoreLib's own calls of the
    /// method get no such promise, so they run its IL.
    let private lookupRaises (state : EscapeAnalysisState) : ThrownType list =
        [ "System.OutOfMemoryException" ; "System.StackOverflowException" ]
        |> List.map (fun name ->
            match runtimeRaises state name ExceptionMaking.Preallocated with
            | thrown, [] -> thrown
            | thrown, made -> failwith $"BUG: raising %O{thrown} preallocated runs %d{made.Length} methods"
        )

    /// What the runtime raises for an `OpcodeFault` that `op` raises (`runtimeRaises`).
    let private faultRaises
        (state : EscapeAnalysisState)
        (op : IlOp)
        (fault : OpcodeFault)
        : ThrownType * MethodKey list
        =
        runtimeRaises state (OpcodeFault.typeName fault) (ExceptionMaking.ofOpcodeFault op fault)

    /// A method the runtime runs by itself, which has no type variables.
    let private runtimeRuns (method : MethodKey) : CallSite =
        CallSite.Runtime
            {
                Callee = method
                Spelling = CalleeSpelling.Fixed
            }

    /// What `runtimeRaises` says the runtime raises at `offset`, as raises and as calls to the
    /// methods it runs to make them.
    let private raisedAt
        (offset : int)
        (raised : (ThrownType * MethodKey list) list)
        : (int * ThrownType) list * (int * CallSite * SpelledArgument list) list
        =
        let raises = raised |> List.map (fun (thrown, _) -> offset, thrown)

        let calls =
            raised
            |> List.collect (fun (_, made) -> made |> List.map (fun method -> offset, runtimeRuns method, []))

        raises, calls

    /// What CoreCLR runs on an exception it throws again, to keep the stack trace it was thrown with
    /// (`ExceptionPreserveStackTrace`): `Exception.InternalPreserveStackTrace`.
    let private preserveStackTrace (state : EscapeAnalysisState) : MethodKey =
        let corelib = state.BaseTypes.Corelib

        let found =
            corelib.TryGetTopLevelTypeDef "System" "Exception"
            |> Option.toList
            |> List.collect (fun ty -> List.ofSeq ty.Methods)
            |> List.filter (fun m ->
                m.Name = "InternalPreserveStackTrace"
                && not m.IsStatic
                && m.Signature.ParameterTypes.IsEmpty
            )
            |> List.choose (fun m -> m.TryMetadata)

        match found with
        | [ facts ] -> MethodKey.make corelib facts.Handle
        | found ->
            failwith
                $"EscapeAnalysis: expected %s{corelib.DefinitionFullName} to define one Exception.InternalPreserveStackTrace(), found %d{found.Length}"

    /// What a site running a type initializer raises besides what the initializer itself does
    /// (`ExceptionMaking.InitializerFailure`): the `TypeInitializationException` wrapping that, and,
    /// should making the wrapper fail, what the runtime runs to throw the initializer's own
    /// exception again, which has no type variables.
    let private initializerFailure (state : EscapeAnalysisState) : ThrownType * MethodKey =
        ThrownType.Exactly (corelibException state "TypeInitializationException"), preserveStackTrace state

    /// What binding a type reference in `assembly` finds.
    let private bindTypeRef
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (typeRef : TypeRef)
        : EscapeAnalysisState * TypeReferenceIdentity
        =
        let assemblies, identity =
            TypeResolution.tryResolveTypeRefIdentity
                state.LoggerFactory
                state.RuntimeDirs
                assembly
                typeRef
                state.TypeSystem._LoadedAssemblies

        withAssemblies state assemblies, identity

    /// The definition a type reference in `assembly` names, if it binds.
    let private resolveTypeRef
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (typeRef : TypeRef)
        : EscapeAnalysisState * ResolvedTypeIdentity option
        =
        match bindTypeRef state assembly typeRef with
        | state, TypeReferenceIdentity.Resolved identity -> state, Some identity
        | state, TypeReferenceIdentity.TypeAbsent _
        | state, TypeReferenceIdentity.AssemblyUnavailable _ -> state, None
        | _, TypeReferenceIdentity.Unmodelled unmodelled -> failwith unmodelled.Diagnostic

    /// Does the base-type chain of the definition `identity` bind? TypeSystem loads it before it
    /// instantiates the type (`TypeConcretization`), as CoreCLR loads a type's parent first, so where
    /// it does not, the type has no runtime type. Loads the assemblies the chain needs.
    let private baseChainBinds
        (state : EscapeAnalysisState)
        (identity : ResolvedTypeIdentity)
        : EscapeAnalysisState * bool
        =
        let assemblies, failure =
            BaseChainLoading.tryEnsureTypeDefinitionBaseAssembliesLoaded
                (TypeResolution.directoryLoader state.LoggerFactory state.RuntimeDirs)
                state.TypeSystem._LoadedAssemblies
                (state.TypeSystem._LoadedAssemblies.ByDefinitionName identity.AssemblyFullName)
                identity.TypeDefinition.Get

        withAssemblies state assemblies, failure.IsNone

    /// The type definition a type spelling names, looking through custom modifiers and
    /// instantiations. `None` for a spelling that names no definition: a type variable, an array, a
    /// pointer.
    let private nominalIdentity
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (spelling : TypeDefn)
        : EscapeAnalysisState * ResolvedTypeIdentity option
        =
        match TypeDefn.stripCustomModifiers spelling with
        | TypeDefn.GenericInstantiation (root, _) ->
            match TypeDefn.stripCustomModifiers root with
            | TypeDefn.FromDefinition (identity, _) -> state, Some identity
            | TypeDefn.FromReference (typeRef, _) -> resolveTypeRef state assembly typeRef
            | _ -> state, None
        | TypeDefn.FromDefinition (identity, _) -> state, Some identity
        | TypeDefn.FromReference (typeRef, _) -> resolveTypeRef state assembly typeRef
        | TypeDefn.PrimitiveType primitive ->
            state, Some (BaseClassTypes.ofPrimitive state.BaseTypes primitive).Identity
        | _ -> state, None

    let private definitionOf
        (state : EscapeAnalysisState)
        (identity : ResolvedTypeIdentity)
        : DumpedAssembly * TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        let assembly = assemblyOf state identity.AssemblyFullName
        assembly, assembly.TypeDefs.[identity.TypeDefinition.Get]

    let private baseOf
        (state : EscapeAnalysisState)
        (identity : ResolvedTypeIdentity)
        : EscapeAnalysisState * ResolvedTypeIdentity option
        =
        match state.Bases.TryFind identity with
        | Some known -> state, known
        | None ->
            let assembly, ty = definitionOf state identity

            let state, result =
                match ty.BaseType with
                | None -> state, None
                | Some (BaseTypeInfo.TypeDef handle) -> state, Some assembly.TypeDefs.[handle].Identity
                | Some (BaseTypeInfo.TypeRef handle) -> resolveTypeRef state assembly assembly.TypeRefs.[handle]
                | Some (BaseTypeInfo.TypeSpec handle) ->
                    nominalIdentity state assembly assembly.TypeSpecs.[handle].Signature

            { state with
                Bases = state.Bases.Add (identity, result)
            },
            result

    /// Is `ty`, or some type it derives from, `ancestor`? Answered on type definitions, which is
    /// exact for a non-generic ancestor: every instantiation of a type shares its definition's base
    /// chain.
    let private derivesFrom
        (state : EscapeAnalysisState)
        (ty : ResolvedTypeIdentity)
        (ancestor : ResolvedTypeIdentity)
        : EscapeAnalysisState * bool
        =
        // Metadata could make a chain cyclic; CoreCLR would refuse to load it.
        let rec go (state : EscapeAnalysisState) (current : ResolvedTypeIdentity) (seen : Set<ResolvedTypeIdentity>) =
            if current = ancestor then
                state, true
            elif seen.Contains current then
                failwith $"The base chain of %O{ty} reaches %O{current} twice"
            else
                match baseOf state current with
                | state, Some parent -> go state parent (seen.Add current)
                | state, None -> state, false

        go state ty Set.empty

    /// The size of `spelling`, a type `assembly` spells, where CoreCLR takes it to be a primitive
    /// type (`getTypeForPrimitiveValueClass`, jitinterface.cpp): a primitive type, or an enum, whose
    /// size is that of its underlying type. `None` for any other type.
    let private primitiveValueSize
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (spelling : TypeDefn)
        : EscapeAnalysisState * int option
        =
        match TypeDefn.stripCustomModifiers spelling with
        | TypeDefn.PrimitiveType primitive -> state, primitiveSize state.Target primitive
        | TypeDefn.FromDefinition _
        | TypeDefn.FromReference _ ->
            match nominalIdentity state assembly spelling with
            | state, None -> state, None
            | state, Some identity ->

            match baseOf state identity with
            | state, Some parent when parent = corelibType state "System" "Enum" ->
                let _, ty = definitionOf state identity

                match ty.Fields |> List.filter (fun field -> not field.IsStatic) with
                | [ value ] ->
                    match value.Signature with
                    | TypeDefn.PrimitiveType primitive -> state, primitiveSize state.Target primitive
                    | _ -> state, None
                | _ -> state, None
            | state, _ -> state, None
        | _ -> state, None

    /// The size of the data of `field`, a static field with an RVA, that `getArrayInitializationData`
    /// (jitinterface.cpp) offers (`FieldDesc::LoadSize`): that of a primitive type, or of a struct
    /// of sequential or explicit layout with a declared size and no instance fields, which is how
    /// compilers declare a block of data. `None` for a field of any other type, whose size is not
    /// worked out here; CoreCLR ignores the declared size of a type of auto layout
    /// (`HasLayoutMetadata`, methodtablebuilder.cpp).
    let private fieldDataSize
        (state : EscapeAnalysisState)
        (field : FieldInfo<GenericParamFromMetadata, TypeDefn>)
        : EscapeAnalysisState * int option
        =
        match field.Signature with
        | TypeDefn.PrimitiveType primitive -> state, primitiveSize state.Target primitive
        | TypeDefn.FromDefinition (identity, _) ->
            let _, ty = definitionOf state identity

            match ty.TypeAttributes &&& TypeAttributes.LayoutMask, ty.Layout with
            | (TypeAttributes.SequentialLayout | TypeAttributes.ExplicitLayout), Layout.Custom (size, _) when
                size > 0
                && ty.Generics.IsEmpty
                && ty.Fields |> List.forall (fun field -> field.IsStatic)
                ->
                match baseOf state identity with
                | state, Some parent when parent = corelibType state "System" "ValueType" -> state, Some size
                | state, _ -> state, None
            | _ -> state, None
        | _ -> state, None

    /// The type a `catch` clause names, when it is one this analysis can compare against: a TypeDef
    /// or TypeRef. A TypeSpec names an instantiation, and deciding whether an exception is one needs
    /// its arguments, so such a clause is treated as catching nothing, which over-reports.
    let private catchType
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        : EscapeAnalysisState * ResolvedTypeIdentity option
        =
        match token with
        | MetadataToken.TypeDefinition handle -> state, Some assembly.TypeDefs.[handle].Identity
        | MetadataToken.TypeReference handle -> resolveTypeRef state assembly assembly.TypeRefs.[handle]
        | _ -> state, None

    let private isInterface (state : EscapeAnalysisState) (ty : ResolvedTypeIdentity) : bool =
        (snd (definitionOf state ty)).TypeAttributes.HasFlag TypeAttributes.Interface

    let private wrapperType (state : EscapeAnalysisState) : ResolvedTypeIdentity =
        corelibType state "System.Runtime.CompilerServices" "RuntimeWrappedException"

    /// Does a `catch` clause for `caught`, in a body of `assembly`, certainly stop what was thrown?
    /// `thrown` is `None` for one the analysis cannot name.
    ///
    /// A clause sees a thrown object that is not an exception as `RuntimeWrappedException` if
    /// `assembly` wraps such throws, and as itself if not, in which case it also sees any thrown
    /// `RuntimeWrappedException` as the object that wraps; an unknown one may be either, so only a
    /// clause catching everything it could be seen as stops it.
    let private catchStops
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (caught : ResolvedTypeIdentity)
        (thrown : ThrownType option)
        : EscapeAnalysisState * bool
        =
        let objectType = state.BaseTypes.Object.Identity
        let exceptionType = state.BaseTypes.Exception.Identity
        let wraps = lazy (RuntimeCompatibility.wrapsNonExceptionThrows assembly)

        if caught = objectType then
            state, true
        else

        match thrown with
        | None -> state, caught = exceptionType && wraps.Force ()
        | Some (ThrownType.Exactly ty)
        | Some (ThrownType.SubtypeOf ty) ->

        // A clause in an assembly that does not wrap sees any `RuntimeWrappedException`
        // unwrapped, even one thrown explicitly, and what it wraps may be anything.
        let state, mayBeUnwrapped =
            if wraps.Force () then
                state, false
            else
                let wrapper = wrapperType state

                match thrown with
                | Some (ThrownType.SubtypeOf _) ->
                    match derivesFrom state wrapper ty with
                    | state, true -> state, true
                    | state, false -> state, isInterface state ty
                | _ -> state, ty = wrapper

        if mayBeUnwrapped then
            state, false
        else

        match derivesFrom state ty exceptionType with
        | state, true -> derivesFrom state ty caught
        | state, false ->
            // What is thrown is not an exception, or, below `object` or an interface, may be
            // either; each possibility must be caught.
            let mayBeException =
                match thrown with
                | Some (ThrownType.SubtypeOf _) -> ty = objectType || isInterface state ty
                | _ -> false

            let state, nonExceptionStopped =
                if wraps.Force () then
                    derivesFrom state (wrapperType state) caught
                else
                    derivesFrom state ty caught

            state, nonExceptionStopped && (not mayBeException || caught = exceptionType)

    /// Does an exception raised at `offset` get past `regions`, a body's clauses in the order the
    /// runtime tries them? `thrown` is `None` for one the analysis cannot name. A `finally` or
    /// `fault` never stops one, and a `filter` may decline, so neither counts.
    let private escapesHandlers
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (regions : ExceptionRegion list)
        (offset : int)
        (thrown : ThrownType option)
        : EscapeAnalysisState * bool
        =
        let rec go (state : EscapeAnalysisState) (regions : ExceptionRegion list) =
            match regions with
            | [] -> state, true
            | ExceptionRegion.Catch (ExceptionCatchType.FromMetadata token, o) :: rest when
                offset >= o.TryOffset && offset < o.TryOffset + o.TryLength
                ->
                match catchType state assembly token with
                | state, Some caught ->
                    match catchStops state assembly caught thrown with
                    | state, true -> state, false
                    | state, false -> go state rest
                | state, None -> go state rest
            | _ :: rest -> go state rest

        go state regions

    /// What a `rethrow` in the handler of `clause`, a clause of a body in `assembly`, re-raises of
    /// `thrown`, which reached the clause from its protected block. It re-raises what the clause
    /// caught, as it was thrown.
    let private rethrownBy
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (clause : ExceptionRegion)
        (thrown : ThrownType option)
        : EscapeAnalysisState * Rethrown
        =
        let asThrown =
            match thrown with
            | Some thrown -> Rethrown.Thrown thrown
            | None -> Rethrown.Unknown

        match clause with
        | ExceptionRegion.Catch (ExceptionCatchType.FromMetadata token, _) ->
            match catchType state assembly token with
            | state, None -> state, asThrown
            | state, Some caught ->

            match catchStops state assembly caught thrown with
            | state, true -> state, asThrown
            | state, false ->

            let exceptionType = state.BaseTypes.Exception.Identity
            let wrapper = wrapperType state
            let wraps = RuntimeCompatibility.wrapsNonExceptionThrows assembly

            // `catchStops` is exact for an object of a known class, except a
            // `RuntimeWrappedException` that a clause in an assembly that does not wrap sees as
            // what it wraps: CoreCLR matches a clause's type against the base chain of what the
            // clause sees, and nothing else, so a clause for an interface catches nothing
            // (`ShouldTypedClauseCatchThisException`, ExceptionHandling.cs).
            let state, never =
                match thrown with
                | _ when isInterface state caught -> state, true
                | Some (ThrownType.Exactly ty) -> state, wraps || ty <> wrapper
                | Some (ThrownType.SubtypeOf ty) when not (isInterface state ty) ->
                    match derivesFrom state ty exceptionType with
                    | state, false -> state, false
                    | state, true ->

                    match derivesFrom state caught ty with
                    | state, true -> state, false
                    | state, false ->

                    match derivesFrom state ty caught with
                    | state, true -> state, false
                    | state, false ->
                        if wraps then
                            state, true
                        else
                            match derivesFrom state wrapper ty with
                            | state, isWrapperAncestor -> state, not isWrapperAncestor
                | _ -> state, false

            if never then
                state, Rethrown.Nothing
            else

            // In an assembly that wraps, a clause for a type other than an ancestor of
            // `RuntimeWrappedException` sees only exceptions, each as itself, so what it catches
            // is one of that type.
            let state, narrowable =
                if not wraps then
                    state, false
                else
                    match derivesFrom state wrapper caught with
                    | state, isWrapperAncestor -> state, not isWrapperAncestor

            match thrown with
            | None
            | Some (ThrownType.SubtypeOf _) when narrowable -> state, Rethrown.Thrown (ThrownType.SubtypeOf caught)
            | _ -> state, asThrown
        // A `filter` handler holds whatever its filter accepted.
        | _ -> state, asThrown

    /// Whether a substitution mentions a type variable, which only a generic definition named
    /// without arguments leaves standing.
    let rec private mentionsVariable (arguments : ImmutableArray<TypeConcretization.SubstitutionArgument>) : bool =
        arguments
        |> Seq.exists (fun argument ->
            match argument with
            | TypeConcretization.SubstitutionArgument.Formal _ -> true
            | TypeConcretization.SubstitutionArgument.Closed _ -> false
            | TypeConcretization.SubstitutionArgument.Spelled (_, _, context) -> mentionsVariable context
        )

    /// What a call instruction's metadata token names, and whether it names the generic definition
    /// of the callee's declaring type rather than an instantiation of it.
    let rec private callTargetOf
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        : EscapeAnalysisState * CallTarget * bool
        =
        match token with
        | MetadataToken.MethodDef handle ->
            let method = assembly.Methods.[handle]
            let typical = not method.DeclaringTypeGenerics.IsEmpty

            let spelling =
                if typical || not method.Generics.IsEmpty then
                    CalleeSpelling.Typical
                else
                    CalleeSpelling.Fixed

            let callee =
                {
                    Callee = MethodKey.make assembly handle
                    Spelling = spelling
                }

            state, CallTarget.Method callee, typical
        | MetadataToken.MethodSpecification handle ->
            match callTargetOf state assembly assembly.MethodSpecs.[handle].Method with
            | state, CallTarget.Method callee, typical ->
                let spelling =
                    if typical then
                        CalleeSpelling.Typical
                    else
                        CalleeSpelling.Spelled token

                state,
                CallTarget.Method
                    { callee with
                        Spelling = spelling
                    },
                typical
            | unresolved -> unresolved
        | MetadataToken.MemberReference handle ->
            let ctx : TypeConcretization.ConcretizationContext<DumpedAssembly> =
                {
                    ConcreteTypes = state.TypeSystem.ConcreteTypes
                    LoadedAssemblies = state.TypeSystem._LoadedAssemblies
                    BaseTypes = state.BaseTypes
                }

            let ctx, target =
                MethodReferenceResolution.resolve state.LoggerFactory state.RuntimeDirs ctx assembly handle

            let state =
                { state with
                    TypeSystem =
                        { state.TypeSystem with
                            _LoadedAssemblies = ctx.LoadedAssemblies
                            ConcreteTypes = ctx.ConcreteTypes
                        }
                }

            match target with
            | MethodReferenceTarget.Defined (declaring, method, declaringTypeArguments) ->
                // A TypeSpec parent spells the declaring type's arguments; any other parent spells
                // none, so a type variable left in them is the parent's own.
                let typical =
                    match assembly.Members.[handle].Parent with
                    | MetadataToken.TypeSpecification _ -> false
                    | _ -> mentionsVariable declaringTypeArguments.Arguments

                let definition = declaring.Methods.[method]

                let spelling =
                    // A generic method named without a MethodSpec has no arguments to run with.
                    if typical || not definition.Generics.IsEmpty then
                        CalleeSpelling.Typical
                    elif definition.DeclaringTypeGenerics.IsEmpty then
                        CalleeSpelling.Fixed
                    else
                        CalleeSpelling.Spelled token

                let callee =
                    {
                        Callee = MethodKey.make declaring method
                        Spelling = spelling
                    }

                state, CallTarget.Method callee, typical
            | MethodReferenceTarget.ArrayMethod (arrayType, accessor) ->
                state, CallTarget.ArrayAccessor (arrayType, accessor), false
            | MethodReferenceTarget.Missing -> state, CallTarget.Missing, false
            | MethodReferenceTarget.ParentTypeMissing _ -> state, CallTarget.TypeMissing, false
            | MethodReferenceTarget.ParentAssemblyUnavailable _ -> state, CallTarget.AssemblyMissing, false
            | MethodReferenceTarget.DependsOnInstantiation -> state, CallTarget.DependsOnInstantiation, false
            // Binding the reference reaches a type in an assembly that is not there, comparing its
            // signature with a candidate's or laying out a searched type's method table.
            | MethodReferenceTarget.Unbound (TypeBindingFailure.AssemblyUnavailable _) ->
                state, CallTarget.AssemblyMissing, false
            | MethodReferenceTarget.Unbound (TypeBindingFailure.Unmodelled unmodelled) -> failwith unmodelled.Diagnostic
            | MethodReferenceTarget.Unbound (TypeBindingFailure.TypeAbsent _ as failure)
            | MethodReferenceTarget.Unbound (TypeBindingFailure.BaseTypeAbsent _ as failure) ->
                failwith
                    $"Binding a call's reference in %s{assembly.DefinitionFullName} reached a type that is not declared, laying out a searched type's method table (%O{failure}): CoreCLR raises TypeLoadException loading that type, which the analysis does not model"
            | MethodReferenceTarget.Unbound (TypeBindingFailure.MethodImplDeclarationAbsent miss) ->
                failwith
                    $"Binding a call's reference in %s{assembly.DefinitionFullName} reached a type whose MethodImpl declares an override of %s{miss.Description}, which names no method: CoreCLR raises TypeLoadException loading that type, which the analysis does not model"
            | MethodReferenceTarget.Unbound (TypeBindingFailure.Refused refusal) ->
                failwith
                    $"Binding a call's reference in %s{assembly.DefinitionFullName} reached a type CoreCLR refuses to load, which the analysis does not model: %s{TypeLoadRefusal.describe refusal}"
        | other -> failwith $"A call in %s{assembly.DefinitionFullName} names %O{other}, which is not a method"

    /// What a call instruction's metadata token names.
    let private callTarget
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        : EscapeAnalysisState * CallTarget
        =
        let state, target, _ = callTargetOf state assembly token
        state, target

    let private methodOf
        (state : EscapeAnalysisState)
        (key : MethodKey)
        : DumpedAssembly * MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        =
        let assembly = assemblyOf state key.AssemblyFullName
        assembly, assembly.Methods.[key.Method.Get]

    let private declaringTypeOf (state : EscapeAnalysisState) (key : MethodKey) : ResolvedTypeIdentity =
        let _, method = methodOf state key
        method.RequiredDeclaringType.Identity

    /// Can the method a `callvirt` names be overridden, so that some other method may run?
    let private isOverridable (state : EscapeAnalysisState) (key : MethodKey) : bool =
        let _, method = methodOf state key

        if not method.IsVirtual || method.IsFinal then
            false
        else
            let _, declaring = definitionOf state (declaringTypeOf state key)
            not (declaring.TypeAttributes.HasFlag TypeAttributes.Sealed)

    /// Does the type a `constrained.` prefix names decide what a `callvirt` of the instance method
    /// `key` after it does, though the method cannot be overridden? It does for a class's method,
    /// which CoreCLR resolves on a value type by its slot (`ConstrainedType.resolveClassMethodOnValueType`),
    /// and runs, where it runs the method named, on a box of the value type that may be no instance
    /// of the class; except for a method of `Object` that is not virtual, which runs on whatever the
    /// prefix names, boxed if need be, every box being an `Object`.
    let private constrainedTypeDecides (state : EscapeAnalysisState) (key : MethodKey) : bool =
        let _, method = methodOf state key
        let declaringType = declaringTypeOf state key
        let _, declaring = definitionOf state declaringType

        not method.IsStatic
        && not (declaring.TypeAttributes.HasFlag TypeAttributes.Interface)
        && (method.IsVirtual || declaringType <> state.BaseTypes.Object.Identity)

    /// The type initializer of a type definition, if it has one.
    let private typeInitializer (state : EscapeAnalysisState) (identity : ResolvedTypeIdentity) : MethodKey option =
        let assembly, ty = definitionOf state identity

        ty.Methods
        |> List.tryFind (fun m -> m.Name = ".cctor" && m.IsStatic)
        |> Option.map (fun m -> MethodKey.make assembly (MethodInfo.requireMetadata ".cctor" m).Handle)

    /// The type initializer that calling `key` runs first, as CoreCLR's `CEEInfo::initClass`
    /// decides: its declaring type's, for a static method or a constructor, or for an instance
    /// method of a value type or an interface. Not for an instance method of a class, whose
    /// constructor has run the initializer already; nor for the initializer itself; nor where the
    /// type is marked `beforefieldinit`, whose initializer waits for an access to one of its static
    /// fields; nor for a module's `<Module>`, whose initializer runs as code first binds into it.
    let private initializerOnCall
        (state : EscapeAnalysisState)
        (key : MethodKey)
        : EscapeAnalysisState * MethodKey option
        =
        let _, method = methodOf state key
        let identity = declaringTypeOf state key
        let _, ty = definitionOf state identity

        let isModule =
            identity.TypeDefinition.Get = MetadataTokens.TypeDefinitionHandle 1
            && ty.Name = "<Module>"

        if
            isModule
            || ty.TypeAttributes.HasFlag TypeAttributes.BeforeFieldInit
            || (method.IsStatic && method.Name = ".cctor")
        then
            state, None
        elif method.IsStatic || method.Name = ".ctor" then
            state, typeInitializer state identity
        elif ty.TypeAttributes.HasFlag TypeAttributes.Interface then
            state, typeInitializer state identity
        else
            let valueType = state.BaseTypes.ValueType.Identity
            let state, derivesFromValueType = derivesFrom state identity valueType

            if
                derivesFromValueType
                && identity <> valueType
                && identity <> state.BaseTypes.Enum.Identity
            then
                state, typeInitializer state identity
            else
                state, None

    /// A static virtual method, which only a `constrained.` call reaches, and which the type that
    /// prefix names decides.
    let private isStaticVirtual (state : EscapeAnalysisState) (key : MethodKey) : bool =
        let _, method = methodOf state key
        method.IsStatic && method.IsVirtual

    /// The method a `newobj` of `key` runs, when `key` is a constructor of `System.String`: the
    /// static `String.Ctor` CoreCLR runs in its place (`StringConstructor`), or `None` if there is
    /// no such `Ctor`, when what CoreCLR does is not known.
    let private stringConstructorImplementation (state : EscapeAnalysisState) (key : MethodKey) : MethodKey option =
        let assembly, method = methodOf state key

        match StringConstructor.implementation state.BaseTypes.String method with
        | Ok implementation ->
            Some (MethodKey.make assembly (MethodInfo.requireMetadata "String.Ctor" implementation).Handle)
        | Error _ -> None

    /// The runtime's own exceptions from one of the methods it supplies on an array type
    /// (`ExceptionMaking.ofArrayMethod`, `runtimeRaises`).
    let private arrayAccessorRaises
        (state : EscapeAnalysisState)
        (arrayType : TypeDefn)
        (accessor : ArrayAccessor)
        : (ThrownType * MethodKey list) list
        =
        ExceptionMaking.ofArrayMethod arrayType accessor
        |> List.map (fun (fullName, making) -> runtimeRaises state fullName making)

    /// What failing to bind does.
    let private bindFailureFacts (state : EscapeAnalysisState) (failure : BindFailure) : OutsideBodyFact list =
        let raised (fullName : string) (making : ExceptionMaking) =
            let thrown, made = runtimeRaises state fullName making

            OutsideBodyFact.Raises thrown
            :: List.map (fun made -> OutsideBodyFact.Runs (made, [])) made

        match failure with
        | BindFailure.TypeAbsent
        | BindFailure.MalformedTypeInstantiation ->
            raised "System.TypeLoadException" ExceptionMaking.TypeLoadConstructor
        | BindFailure.AssemblyUnavailable ->
            [
                OutsideBodyFact.Raises (ThrownType.Exactly (corelibType state "System.IO" "FileNotFoundException"))
                OutsideBodyFact.Opaque Opacity.UnresolvedAssembly
            ]
        | BindFailure.ConstraintViolation -> raised "System.TypeLoadException" ExceptionMaking.ParameterlessConstructor
        | BindFailure.MalformedMethodInstantiation ->
            raised "System.BadImageFormatException" ExceptionMaking.ParameterlessConstructor
        | BindFailure.MethodConstraintViolation ->
            raised "System.Security.VerificationException" ExceptionMaking.ParameterlessConstructor
        | BindFailure.UndecidedConstraint -> [ OutsideBodyFact.Opaque Opacity.UndecidedConstraint ]

    /// The first type reference in a spelling that fails to bind, and why, which makes binding the
    /// token that spells it throw; `None` if all of them bind. Custom modifiers and function pointer
    /// signatures are not bound.
    let rec private spellingBindFailure
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (spelling : TypeDefn)
        : EscapeAnalysisState * BindFailure option
        =
        match spelling with
        | TypeDefn.FromReference (typeRef, _) ->
            match bindTypeRef state assembly typeRef with
            | state, TypeReferenceIdentity.Resolved _ -> state, None
            | state, TypeReferenceIdentity.TypeAbsent _ -> state, Some BindFailure.TypeAbsent
            | state, TypeReferenceIdentity.AssemblyUnavailable _ -> state, Some BindFailure.AssemblyUnavailable
            | _, TypeReferenceIdentity.Unmodelled unmodelled -> failwith unmodelled.Diagnostic
        | TypeDefn.GenericInstantiation (root, arguments) ->
            firstBindFailure state assembly (Seq.append [ root ] arguments)
        | TypeDefn.Array (element, _)
        | TypeDefn.OneDimensionalArrayLowerBoundZero element
        | TypeDefn.Pointer element
        | TypeDefn.Byref element
        | TypeDefn.Pinned element -> spellingBindFailure state assembly element
        | TypeDefn.Modified modified -> spellingBindFailure state assembly modified.Unmodified
        | TypeDefn.FromDefinition _
        | TypeDefn.PrimitiveType _
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _
        | TypeDefn.FunctionPointer _
        | TypeDefn.Void -> state, None

    /// The first type reference in `spellings`, in order, that fails to bind, and why.
    and private firstBindFailure
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (spellings : TypeDefn seq)
        : EscapeAnalysisState * BindFailure option
        =
        ((state, None), spellings)
        ||> Seq.fold (fun (state, failure) spelling ->
            match failure with
            | Some _ -> state, failure
            | None -> spellingBindFailure state assembly spelling
        )

    /// What CoreCLR raises where a load meets `miss`. A load the Loader does not model fails the
    /// analysis instead.
    let private bindFailureOfMiss (miss : TypeLoad.LoadMiss) : BindFailure =
        match miss with
        | TypeLoad.LoadMiss.TypeAbsent _ -> BindFailure.TypeAbsent
        | TypeLoad.LoadMiss.AssemblyUnavailable _ -> BindFailure.AssemblyUnavailable
        | TypeLoad.LoadMiss.Unmodelled unmodelled -> failwith unmodelled.Diagnostic

    /// Why CoreCLR's full load of a type `assembly` spells fails, at the first type reference it
    /// reaches that names nothing (`TypeLoad.loadMiss`): the base classes, interfaces and value-type
    /// fields of each type it names, transitively, as well as the references it spells. The JIT
    /// loads so each type a token names and each type argument of a method it instantiates. `None`
    /// if no reference the load reaches fails to bind.
    let private spellingLoadFailure
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (spelling : TypeDefn)
        : EscapeAnalysisState * BindFailure option
        =
        let load (state : EscapeAnalysisState) (spellingAssembly : DumpedAssembly) (spelling : TypeDefn) =
            let assemblies, loaded, miss =
                TypeLoad.loadMissFrom
                    state.LoggerFactory
                    state.RuntimeDirs
                    state.BaseTypes
                    spellingAssembly
                    spelling
                    state.LoadedDefinitions
                    state.TypeSystem._LoadedAssemblies

            { withAssemblies state assemblies with
                LoadedDefinitions = loaded
            },
            miss

        // A type definition's load is memoised; it is the same wherever a token names it.
        let ofDefinition (state : EscapeAnalysisState) (identity : ResolvedTypeIdentity) =
            match state.TypeLoads.TryFind identity with
            | Some miss -> state, Option.map bindFailureOfMiss miss
            | None ->
                let state, miss =
                    load
                        state
                        (assemblyOf state identity.AssemblyFullName)
                        (TypeDefn.FromDefinition (identity, SignatureTypeKind.Unknown))

                { state with
                    TypeLoads = state.TypeLoads.Add (identity, miss)
                },
                Option.map bindFailureOfMiss miss

        match TypeDefn.stripCustomModifiers spelling with
        | TypeDefn.FromDefinition (identity, _) -> ofDefinition state identity
        | TypeDefn.FromReference (typeRef, _) ->
            match bindTypeRef state assembly typeRef with
            | state, TypeReferenceIdentity.Resolved identity -> ofDefinition state identity
            | state, TypeReferenceIdentity.TypeAbsent _ -> state, Some BindFailure.TypeAbsent
            | state, TypeReferenceIdentity.AssemblyUnavailable _ -> state, Some BindFailure.AssemblyUnavailable
            | _, TypeReferenceIdentity.Unmodelled unmodelled -> failwith unmodelled.Diagnostic
        | _ ->
            let state, miss = load state assembly spelling
            state, Option.map bindFailureOfMiss miss

    /// The first of `spellings`, in order, whose full load fails (`spellingLoadFailure`), and why.
    let private firstLoadFailure
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (spellings : TypeDefn seq)
        : EscapeAnalysisState * BindFailure option
        =
        ((state, None), spellings)
        ||> Seq.fold (fun (state, failure) spelling ->
            match failure with
            | Some _ -> state, failure
            | None -> spellingLoadFailure state assembly spelling
        )

    /// The types a method signature spells: its return type, if any, and its parameters'.
    let private signatureTypes (signature : TypeMethodSignature<TypeDefn>) : TypeDefn list =
        match signature.ReturnType with
        | MethodReturnType.Void -> signature.ParameterTypes
        | MethodReturnType.Returns ty -> ty :: signature.ParameterTypes

    /// Why CoreCLR's approximate load of a value type `assembly` spells fails, at the first type
    /// reference it reaches that names nothing (`TypeLoad.approximateLoadMissFrom`), or `None` if no
    /// reference it reaches fails to bind.
    let private approximateLoadFailure
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (spelling : TypeDefn)
        : EscapeAnalysisState * BindFailure option
        =
        let assemblies, miss =
            TypeLoad.approximateLoadMissFrom
                state.LoggerFactory
                state.RuntimeDirs
                assembly
                spelling
                state.LoadedDefinitions
                state.TypeSystem._LoadedAssemblies

        withAssemblies state assemblies, Option.map bindFailureOfMiss miss

    /// Why CoreCLR fails to make a delegate of type `delegateType` over `target`, the method an
    /// `ldftn` or `ldvirtftn` names: the first type at `JitTypePosition.ShuffleThunkSignature`,
    /// in the `Invoke` method's signature and then the target's, each as its definition spells it,
    /// whose load fails. `None` where it needs no thunk to shuffle the arguments, the delegate being
    /// closed over its target (`JitTypeLoads.delegateClosure`), or where every load succeeds.
    let private shuffleThunkFailure
        (state : EscapeAnalysisState)
        (delegateType : ResolvedTypeIdentity)
        (target : MethodKey)
        : EscapeAnalysisState * BindFailure option
        =
        let delegateAssembly, definition = definitionOf state delegateType

        let invoke =
            match definition.Methods |> List.filter (fun method -> method.Name = "Invoke") with
            | [ invoke ] -> invoke
            | methods ->
                failwith
                    $"BUG: delegate type %O{delegateType} has %d{methods.Length} methods named Invoke, but CoreCLR loads a delegate type only with one"

        let targetAssembly, targetMethod = methodOf state target

        match
            JitTypeLoads.delegateClosure
                invoke.Signature.ParameterTypes.Length
                targetMethod.Signature.ParameterTypes.Length
                targetMethod.IsStatic
        with
        | DelegateClosure.Closed -> state, None
        | DelegateClosure.Open ->
            let spellings =
                (signatureTypes invoke.Signature
                 |> List.map (fun spelling -> delegateAssembly, spelling))
                @ (signatureTypes targetMethod.Signature
                   |> List.map (fun spelling -> targetAssembly, spelling))

            ((state, None), spellings)
            ||> List.fold (fun (state, failure) (assembly, spelling) ->
                match failure with
                | Some _ -> state, failure
                | None ->
                    match JitTypeLoads.load JitTypePosition.ShuffleThunkSignature spelling with
                    | None -> state, None
                    | Some (JitTypeLoad.Approximately loaded) -> approximateLoadFailure state assembly loaded
                    | Some other ->
                        failwith
                            $"BUG: JitTypeLoads.load answers %O{other} for %O{spelling} in a delegate's shuffle thunk"
            )

    /// Whether a spelling mentions a type variable of the body that spells it.
    let rec private mentionsTypeVariable (spelling : TypeDefn) : bool =
        match spelling with
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _ -> true
        | TypeDefn.GenericInstantiation (root, arguments) ->
            mentionsTypeVariable root || Seq.exists mentionsTypeVariable arguments
        | TypeDefn.Array (element, _)
        | TypeDefn.OneDimensionalArrayLowerBoundZero element
        | TypeDefn.Pointer element
        | TypeDefn.Byref element
        | TypeDefn.Pinned element -> mentionsTypeVariable element
        | TypeDefn.Modified modified -> mentionsTypeVariable modified.Unmodified
        | TypeDefn.FunctionPointer signature -> signatureTypes signature |> List.exists mentionsTypeVariable
        | TypeDefn.FromReference _
        | TypeDefn.FromDefinition _
        | TypeDefn.PrimitiveType _
        | TypeDefn.Void -> false

    /// Does TypeSystem instantiate a spelling? Every type reference it reads must name a type, as
    /// `spellingBindFailure`, and through a function pointer's signature as well, whose types
    /// TypeSystem instantiates although the JIT binds none of them; and every type it names must have
    /// a base chain that binds (`baseChainBinds`). False too where a type argument carries a custom
    /// modifier, which TypeSystem cannot instantiate (`TypeConcretization.carriesCustomModifier`).
    let rec private concretizable
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (spelling : TypeDefn)
        : EscapeAnalysisState * bool
        =
        let all (state : EscapeAnalysisState) (spellings : TypeDefn seq) =
            ((state, true), spellings)
            ||> Seq.fold (fun (state, soFar) spelling ->
                if soFar then
                    concretizable state assembly spelling
                else
                    state, false
            )

        match spelling with
        | TypeDefn.FunctionPointer signature -> all state (signatureTypes signature)
        | TypeDefn.GenericInstantiation (_, arguments) when
            Seq.exists TypeConcretization.carriesCustomModifier arguments
            ->
            state, false
        | TypeDefn.GenericInstantiation (root, arguments) -> all state (Seq.append [ root ] arguments)
        | TypeDefn.Array (element, _)
        | TypeDefn.OneDimensionalArrayLowerBoundZero element
        | TypeDefn.Pointer element
        | TypeDefn.Byref element
        | TypeDefn.Pinned element -> concretizable state assembly element
        | TypeDefn.Modified modified -> concretizable state assembly modified.Unmodified
        | TypeDefn.FromReference (typeRef, _) ->
            match resolveTypeRef state assembly typeRef with
            | state, Some identity -> baseChainBinds state identity
            | state, None -> state, false
        | TypeDefn.FromDefinition (identity, _) -> baseChainBinds state identity
        | TypeDefn.PrimitiveType _
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _
        | TypeDefn.Void -> state, true

    /// The type definitions a spelling names that resolve, arguments included.
    let rec private namedIdentities
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (spelling : TypeDefn)
        : EscapeAnalysisState * ResolvedTypeIdentity list
        =
        match spelling with
        | TypeDefn.FromReference (typeRef, _) ->
            match resolveTypeRef state assembly typeRef with
            | state, Some identity -> state, [ identity ]
            | state, None -> state, []
        | TypeDefn.FromDefinition (identity, _) -> state, [ identity ]
        | TypeDefn.GenericInstantiation (root, arguments) ->
            ((state, []), Seq.append [ root ] arguments)
            ||> Seq.fold (fun (state, acc) spelling ->
                let state, named = namedIdentities state assembly spelling
                state, named @ acc
            )
        | TypeDefn.Array (element, _)
        | TypeDefn.OneDimensionalArrayLowerBoundZero element
        | TypeDefn.Pointer element
        | TypeDefn.Byref element
        | TypeDefn.Pinned element -> namedIdentities state assembly element
        | TypeDefn.Modified modified -> namedIdentities state assembly modified.Unmodified
        | TypeDefn.PrimitiveType _
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _
        | TypeDefn.FunctionPointer _
        | TypeDefn.Void -> state, []

    /// A type a body of `assemblyName` spells, as the instantiation `typeArguments` and
    /// `methodArguments` makes it, or why it does not bind.
    let private instantiate
        (state : EscapeAnalysisState)
        (assemblyName : string)
        (typeArguments : ConcreteTypeHandle seq)
        (methodArguments : ConcreteTypeHandle seq)
        (spelling : TypeDefn)
        : EscapeAnalysisState * Result<ConcreteTypeHandle, TypeBindingFailure>
        =
        let typeSystem, handle =
            TypeSystemState.concretizeType
                state.LoggerFactory
                state.RuntimeDirs
                state.BaseTypes
                state.TypeSystem
                assemblyName
                (ImmutableArray.CreateRange typeArguments)
                (ImmutableArray.CreateRange methodArguments)
                spelling

        { state with
            TypeSystem = typeSystem
        },
        handle

    /// What a type that does not bind where TypeSystem instantiates it raises there.
    let private failureOfBinding (failure : TypeBindingFailure) : BindFailure =
        match failure with
        | TypeBindingFailure.AssemblyUnavailable _ -> BindFailure.AssemblyUnavailable
        | TypeBindingFailure.TypeAbsent _
        | TypeBindingFailure.BaseTypeAbsent _ -> BindFailure.TypeAbsent
        | TypeBindingFailure.Unmodelled unmodelled -> failwith unmodelled.Diagnostic

    /// What CoreCLR raises for a verdict on an instantiation's constraints: for a method's,
    /// `MethodConstraintViolation`, and for a type's, `ConstraintViolation`.
    let private verdictFailure (ofMethod : bool) (verdict : ConstraintVerdict) : BindFailure option =
        match verdict with
        | ConstraintVerdict.Satisfied -> None
        | ConstraintVerdict.Violated _ ->
            Some (
                if ofMethod then
                    BindFailure.MethodConstraintViolation
                else
                    BindFailure.ConstraintViolation
            )
        | ConstraintVerdict.Undecided _ -> Some BindFailure.UndecidedConstraint

    /// CoreLib's `System.__Canon`, which stands, in code shared between instantiations, for every
    /// reference type its type arguments are.
    let private canonHandle (state : EscapeAnalysisState) : EscapeAnalysisState * ConcreteTypeHandle =
        match
            instantiate
                state
                state.BaseTypes.Corelib.DefinitionFullName
                []
                []
                (TypeDefn.FromDefinition (corelibType state "System" "__Canon", SignatureTypeKind.Class))
        with
        | state, Ok canon -> state, canon
        | _, Error failure -> failwith $"BUG: CoreLib's System.__Canon does not bind: %O{failure}"

    /// Whether a type contains `canon` (`System.__Canon`) anywhere: whether it is what code shared
    /// between instantiations sees in place of a type, which CoreCLR exempts from the check of an
    /// instantiation's constraints (`IsSharedByGenericInstantiations`).
    let rec private containsCanon
        (state : EscapeAnalysisState)
        (canon : ConcreteTypeHandle)
        (handle : ConcreteTypeHandle)
        : bool
        =
        match handle with
        | ConcreteTypeHandle.Concrete _ ->
            handle = canon
            || (
                match AllConcreteTypes.lookup handle state.TypeSystem.ConcreteTypes with
                | Some concrete -> concrete.Generics |> Seq.exists (containsCanon state canon)
                | None -> failwith $"BUG: concrete type handle %O{handle} is not registered"
            )
        | ConcreteTypeHandle.OneDimArrayZero element
        | ConcreteTypeHandle.Array (element, _)
        | ConcreteTypeHandle.Byref element
        | ConcreteTypeHandle.Pointer element -> containsCanon state canon element
        | ConcreteTypeHandle.FunctionPointer _ -> false

    /// What code shared between instantiations sees of an instance's type argument
    /// (`ClassLoader::CanonicalizeGenericArg`): `System.__Canon` for a reference type, and a value
    /// type with its own arguments so made.
    let rec private canonicalArgument
        (state : EscapeAnalysisState)
        (canon : ConcreteTypeHandle)
        (handle : ConcreteTypeHandle)
        : EscapeAnalysisState * ConcreteTypeHandle
        =
        if TypeSystemState.isReferenceTypeHandle state.BaseTypes "canonicalArgument" state.TypeSystem handle then
            state, canon
        else

        match AllConcreteTypes.lookup handle state.TypeSystem.ConcreteTypes with
        | Some concrete when not concrete.Generics.IsEmpty ->
            let state, arguments =
                ((state, []), concrete.Generics)
                ||> Seq.fold (fun (state, arguments) argument ->
                    let state, argument = canonicalArgument state canon argument
                    state, argument :: arguments
                )

            let arguments = List.rev arguments

            if arguments = List.ofSeq concrete.Generics then
                state, handle
            else

            let spelling =
                TypeDefn.GenericInstantiation (
                    TypeDefn.FromDefinition (concrete.Identity, SignatureTypeKind.ValueType),
                    arguments
                    |> List.mapi (fun index _ -> TypeDefn.GenericTypeParameter index)
                    |> ImmutableArray.CreateRange
                )

            match instantiate state concrete.Identity.AssemblyFullName arguments [] spelling with
            | state, Ok canonical -> state, canonical
            | _, Error failure -> failwith $"BUG: the canonical form of %O{handle} does not bind: %O{failure}"
        | _ -> state, handle

    /// How deeply a type's arguments and elements nest.
    let rec private nestingOf (state : EscapeAnalysisState) (handle : ConcreteTypeHandle) : int =
        match handle with
        | ConcreteTypeHandle.Concrete _ ->
            match AllConcreteTypes.lookup handle state.TypeSystem.ConcreteTypes with
            | Some concrete -> 1 + (concrete.Generics |> Seq.map (nestingOf state) |> Seq.fold max 0)
            | None -> failwith $"BUG: concrete type handle %O{handle} is not registered"
        | ConcreteTypeHandle.OneDimArrayZero element
        | ConcreteTypeHandle.Array (element, _)
        | ConcreteTypeHandle.Byref element
        | ConcreteTypeHandle.Pointer element -> 1 + nestingOf state element
        | ConcreteTypeHandle.FunctionPointer _ -> 1

    /// How deeply the types a full load reaches may nest before the analysis stops following it. A
    /// type whose load expands without end CoreCLR refuses; the analysis does not decide that.
    let private closureNestingLimit : int = 16

    /// <summary>
    /// What CoreCLR's checks of the instantiations its full load of <paramref name="handle" />
    /// reaches find, at the first that fails: an argument it admits in no instantiation, or one its
    /// type parameter's constraints reject (<c>ConstraintSatisfaction</c>). The load reaches what
    /// <c>MethodTable::DoFullyLoad</c> brings (<c>TypeLoad.fullLoadParts</c>, instantiated as each
    /// type is), each type's generic arguments, and a constructed type's elements, and CoreCLR
    /// checks each instantiation after what it brings. <c>None</c> where every instantiation
    /// satisfies its constraints.
    /// </summary>
    /// <remarks>
    /// An instantiation containing <c>System.__Canon</c> is what shared code sees, which CoreCLR does
    /// not check, though it checks what that instantiation's load brings that does not contain it.
    /// The load is assumed to meet no type reference that names nothing (<c>TypeLoad</c>), which the
    /// caller decides first.
    /// </remarks>
    let private closureConstraintFailure
        (state : EscapeAnalysisState)
        (handle : ConcreteTypeHandle)
        : EscapeAnalysisState * BindFailure option
        =
        match state.ClosureConstraints.TryFind handle with
        | Some known -> state, known
        | None ->

        let state, canon = canonHandle state

        // Each step is skipped once one before it has found a failure.
        let rec firstFailure
            (acc : (EscapeAnalysisState * Set<ConcreteTypeHandle>) * BindFailure option)
            (steps :
                ((EscapeAnalysisState * Set<ConcreteTypeHandle>)
                    -> (EscapeAnalysisState * Set<ConcreteTypeHandle>) * BindFailure option) list)
            =
            match acc, steps with
            | (_, Some _), _
            | _, [] -> acc
            | (walked, None), step :: rest -> firstFailure (step walked) rest

        let rec walk
            (state : EscapeAnalysisState, visited : Set<ConcreteTypeHandle>)
            (handle : ConcreteTypeHandle)
            : (EscapeAnalysisState * Set<ConcreteTypeHandle>) * BindFailure option
            =
            if visited.Contains handle then
                (state, visited), None
            else

            match state.ClosureConstraints.TryFind handle with
            | Some known -> (state, visited), known
            | None ->

            let visited = visited.Add handle

            if nestingOf state handle > closureNestingLimit then
                (state, visited), Some BindFailure.UndecidedConstraint
            else

            match handle with
            | ConcreteTypeHandle.OneDimArrayZero element
            | ConcreteTypeHandle.Array (element, _)
            | ConcreteTypeHandle.Byref element
            | ConcreteTypeHandle.Pointer element -> walk (state, visited) element
            // `TypeDesc::DoFullyLoad` completes a function pointer without its signature's types.
            | ConcreteTypeHandle.FunctionPointer _ -> (state, visited), None
            | ConcreteTypeHandle.Concrete _ ->

            let concrete, typeInfo =
                match TypeSystemState.tryGetConcreteTypeInfo state.TypeSystem handle with
                | Some found -> found
                | None -> failwith $"BUG: concrete type handle %O{handle} has no TypeDef row"

            let definedIn = assemblyOf state concrete.Identity.AssemblyFullName

            let part (isField : bool) (spelling : TypeDefn) (state : EscapeAnalysisState, visited) =
                match instantiate state definedIn.DefinitionFullName concrete.Generics [] spelling with
                | state, Error failure -> (state, visited), Some (failureOfBinding failure)
                | state, Ok child ->
                    if not isField then
                        walk (state, visited) child
                    else

                    // A field of an enum type has a primitive `FieldDesc`, which is not loaded.
                    let typeSystem, isEnum =
                        TypeSystemState.isEnumValueType
                            state.LoggerFactory
                            state.RuntimeDirs
                            state.BaseTypes
                            state.TypeSystem
                            child

                    let state =
                        { state with
                            TypeSystem = typeSystem
                        }

                    match isEnum with
                    | Ok true -> (state, visited), None
                    | Ok false -> walk (state, visited) child
                    | Error failure -> (state, visited), Some (failureOfBinding failure)

            let parts = TypeLoad.fullLoadParts definedIn typeInfo

            let parents =
                parts
                |> List.choose (fun part' ->
                    match part' with
                    | TypeLoad.FullLoadPart.Base spelling
                    | TypeLoad.FullLoadPart.Interface spelling -> Some (part false spelling)
                    | TypeLoad.FullLoadPart.ValueTypeField _ -> None
                    | TypeLoad.FullLoadPart.UnmodelledInterface unmodelled -> failwith unmodelled.Diagnostic
                )

            let fields =
                parts
                |> List.choose (fun part' ->
                    match part' with
                    | TypeLoad.FullLoadPart.ValueTypeField spelling -> Some (part true spelling)
                    | _ -> None
                )

            let arguments =
                concrete.Generics
                |> Seq.map (fun argument walked -> walk walked argument)
                |> List.ofSeq

            // `DoFullyLoad` brings the parents, then the generic arguments, then the fields, and checks
            // the instantiation itself last.
            let self (state : EscapeAnalysisState, visited) =
                if concrete.Generics.IsEmpty || containsCanon state canon handle then
                    (state, visited), None
                else

                let arguments = List.ofSeq concrete.Generics

                match ConstraintSatisfaction.malformedArgument state.BaseTypes state.TypeSystem arguments with
                | Some _ -> (state, visited), Some BindFailure.MalformedTypeInstantiation
                | None ->

                let typeSystem, verdict =
                    ConstraintSatisfaction.satisfiesClosed
                        state.LoggerFactory
                        state.RuntimeDirs
                        state.BaseTypes
                        state.TypeSystem
                        definedIn.DefinitionFullName
                        concrete.Generics
                        ImmutableArray.Empty
                        typeInfo.Generics
                        arguments

                let state =
                    { state with
                        TypeSystem = typeSystem
                    }

                match verdict with
                | Ok verdict -> (state, visited), verdictFailure false verdict
                | Error failure -> (state, visited), Some (failureOfBinding failure)

            firstFailure ((state, visited), None) (parents @ arguments @ fields @ [ self ])

        let (state, _), found = walk (state, Set.empty) handle

        { state with
            ClosureConstraints = state.ClosureConstraints.Add (handle, found)
        },
        found

    /// Whether a spelling names a generic type definition bare, without arguments: its typical
    /// instantiation, over its own type parameters, which CoreCLR does not check as it checks an
    /// instantiation's constraints.
    let private namesBareGenericDefinition
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (spelling : TypeDefn)
        : EscapeAnalysisState * bool
        =
        let isGeneric (identity : ResolvedTypeIdentity) =
            not (snd (definitionOf state identity)).Generics.IsEmpty

        match TypeDefn.stripCustomModifiers spelling with
        | TypeDefn.FromDefinition (identity, _) -> state, isGeneric identity
        | TypeDefn.FromReference (typeRef, _) ->
            match resolveTypeRef state assembly typeRef with
            | state, Some identity -> state, isGeneric identity
            | state, None -> state, false
        | _ -> state, false

    /// <summary>
    /// What CoreCLR's checks find of the closed instantiations the full load of a type
    /// <paramref name="assembly" /> spells over a body's type variables reaches, which are the same
    /// whatever instantiates the variables: each part of the spelling that mentions no variable,
    /// and, for each generic definition the spelling or such a load instantiates over variables,
    /// each part its full load brings that mentions none of its type parameters
    /// (<c>closureConstraintFailure</c>). <c>None</c> where every one satisfies its constraints.
    /// </summary>
    let rec private closedPartsFailure
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (spelling : TypeDefn)
        : EscapeAnalysisState * BindFailure option
        =
        let firstOf (state : EscapeAnalysisState) (spellings : TypeDefn list) =
            ((state, None), spellings)
            ||> List.fold (fun (state, found) spelling ->
                match found with
                | Some _ -> state, found
                | None -> closedPartsFailure state assembly spelling
            )

        if not (mentionsTypeVariable spelling) then
            match concretizable state assembly spelling with
            | state, false -> state, None
            | state, true ->

            match namesBareGenericDefinition state assembly spelling with
            | state, true -> state, None
            | state, false ->

            match instantiate state assembly.DefinitionFullName [] [] spelling with
            | state, Error failure -> state, Some (failureOfBinding failure)
            | state, Ok handle -> closureConstraintFailure state handle
        else

        match spelling with
        | TypeDefn.GenericInstantiation (root, arguments) ->
            match firstOf state (List.ofSeq arguments) with
            | state, Some failure -> state, Some failure
            | state, None ->

            match nominalIdentity state assembly root with
            | state, Some identity -> definitionClosedPartsFailure state identity
            | state, None -> state, None
        | TypeDefn.Array (element, _)
        | TypeDefn.OneDimensionalArrayLowerBoundZero element
        | TypeDefn.Pointer element
        | TypeDefn.Byref element
        | TypeDefn.Pinned element -> closedPartsFailure state assembly element
        | TypeDefn.Modified modified -> closedPartsFailure state assembly modified.Unmodified
        | _ -> state, None

    /// `closedPartsFailure` of each part the full load of an instantiation of the generic
    /// definition `identity` brings, spelled over its own type parameters.
    and private definitionClosedPartsFailure
        (state : EscapeAnalysisState)
        (identity : ResolvedTypeIdentity)
        : EscapeAnalysisState * BindFailure option
        =
        match state.ClosedPartFailures.TryFind identity with
        | Some known -> state, known
        | None ->

        // A definition its own load comes back to adds nothing on the way round.
        let state =
            { state with
                ClosedPartFailures = state.ClosedPartFailures.Add (identity, None)
            }

        let definedIn, definition = definitionOf state identity

        let state, found =
            ((state, None), TypeLoad.fullLoadParts definedIn definition)
            ||> List.fold (fun (state, found) part ->
                match found, part with
                | Some _, _ -> state, found
                | None, TypeLoad.FullLoadPart.Base spelling
                | None, TypeLoad.FullLoadPart.Interface spelling -> closedPartsFailure state definedIn spelling
                | None, TypeLoad.FullLoadPart.ValueTypeField spelling ->
                    // A field of an enum type is not loaded; an enum is not generic, so a spelling
                    // mentioning a type parameter is not of one.
                    if mentionsTypeVariable spelling then
                        closedPartsFailure state definedIn spelling
                    else
                        match instantiate state definedIn.DefinitionFullName [] [] spelling with
                        | state, Error failure -> state, Some (failureOfBinding failure)
                        | state, Ok handle ->
                            let typeSystem, isEnum =
                                TypeSystemState.isEnumValueType
                                    state.LoggerFactory
                                    state.RuntimeDirs
                                    state.BaseTypes
                                    state.TypeSystem
                                    handle

                            let state =
                                { state with
                                    TypeSystem = typeSystem
                                }

                            match isEnum with
                            | Ok true -> state, None
                            | Ok false -> closureConstraintFailure state handle
                            | Error failure -> state, Some (failureOfBinding failure)
                | None, TypeLoad.FullLoadPart.UnmodelledInterface unmodelled -> failwith unmodelled.Diagnostic
            )

        { state with
            ClosedPartFailures = state.ClosedPartFailures.Add (identity, found)
        },
        found

    /// What the constraint checks of CoreCLR's full load of a type `assembly` spells raise, which
    /// loads without a miss (`closureConstraintFailure`). Of a spelling over the body's type
    /// variables, what is the same for every instantiation of them is checked here
    /// (`closedPartsFailure`), and the rest comes back as the check an instance makes
    /// (`InstanceCheck.LoadsInFull`). A generic definition spelled bare is its typical
    /// instantiation, which CoreCLR checks against its own type parameters, which the analysis does
    /// not model.
    let private fullLoadConstraints
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (spelling : TypeDefn)
        : EscapeAnalysisState * BindFailure option * InstanceCheck list
        =
        if mentionsTypeVariable spelling then
            let state, failure = closedPartsFailure state assembly spelling
            state, failure, [ InstanceCheck.LoadsInFull (assembly.DefinitionFullName, spelling) ]
        else

        match concretizable state assembly spelling with
        | state, false -> state, None, []
        | state, true ->

        match namesBareGenericDefinition state assembly spelling with
        | state, true -> state, None, []
        | state, false ->

        match instantiate state assembly.DefinitionFullName [] [] spelling with
        | state, Error failure -> state, Some (failureOfBinding failure), []
        | state, Ok handle ->
            let state, failure = closureConstraintFailure state handle
            state, failure, []

    /// <summary>
    /// What CoreCLR's full load of a type <paramref name="assembly" /> spells raises, where the JIT
    /// loads it in full (<c>JitTypeLoad.InFull</c>): the first type reference the load reaches that
    /// names nothing (<c>spellingLoadFailure</c>), and where there is none, the first instantiation
    /// the load reaches whose arguments are malformed or break its constraints
    /// (<c>fullLoadConstraints</c>).
    /// </summary>
    /// <remarks>
    /// That a load meeting a miss reaches no constraint check holds exactly for the instantiation
    /// whose definition misses, since CoreCLR loads a definition's constraints before it checks an
    /// instantiation's (measured with a constraint naming a type in an assembly that is not there).
    /// The analysis does not keep the order of the rest of one load: CoreCLR may check an
    /// instantiation before it reaches a later part that misses, and then raises that check's
    /// <c>TypeLoadException</c> where the analysis reports the miss.
    /// </remarks>
    let private jitFullLoad
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (spelling : TypeDefn)
        : EscapeAnalysisState * BindFailure option * InstanceCheck list
        =
        match spellingLoadFailure state assembly spelling with
        | state, Some failure -> state, Some failure, []
        | state, None -> fullLoadConstraints state assembly spelling

    /// The first of `spellings`, in order, whose full load by the JIT fails (`jitFullLoad`), and the
    /// checks only an instance makes of those before it.
    let private firstJitFullLoadFailure
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (spellings : TypeDefn seq)
        : EscapeAnalysisState * BindFailure option * InstanceCheck list
        =
        ((state, None, []), spellings)
        ||> Seq.fold (fun (state, failure, checks) spelling ->
            match failure with
            | Some _ -> state, failure, checks
            | None ->
                let state, failure, more = jitFullLoad state assembly spelling
                state, failure, checks @ more
        )

    /// What CoreCLR's instantiation of the generic method `callee`, with `typeArguments` as its
    /// declaring type's arguments and `methodArguments` as its own, raises: a method instantiation
    /// with the wrong number of arguments, or an argument CoreCLR admits in no instantiation
    /// (`BadImageFormatException`), or an argument the method's constraints reject
    /// (`VerificationException`). An instantiation either of whose argument lists contains
    /// `System.__Canon` is what shared code sees, which CoreCLR does not check.
    let private methodInstantiationFailure
        (state : EscapeAnalysisState)
        (callee : MethodKey)
        (typeArguments : ConcreteTypeHandle list)
        (methodArguments : ConcreteTypeHandle list)
        : EscapeAnalysisState * BindFailure option
        =
        let key = callee, typeArguments, methodArguments

        match state.MethodInstantiations.TryFind key with
        | Some failure -> state, failure
        | None ->

        let state, canon = canonHandle state

        let state, failure =
            if typeArguments @ methodArguments |> List.exists (containsCanon state canon) then
                state, None
            else

            let calleeAssembly, calleeMethod = methodOf state callee

            if
                calleeMethod.Generics.Length <> methodArguments.Length
                || (ConstraintSatisfaction.malformedArgument state.BaseTypes state.TypeSystem methodArguments).IsSome
            then
                state, Some BindFailure.MalformedMethodInstantiation
            else

            let typeSystem, verdict =
                ConstraintSatisfaction.satisfiesClosed
                    state.LoggerFactory
                    state.RuntimeDirs
                    state.BaseTypes
                    state.TypeSystem
                    calleeAssembly.DefinitionFullName
                    (ImmutableArray.CreateRange typeArguments)
                    (ImmutableArray.CreateRange methodArguments)
                    calleeMethod.Generics
                    methodArguments

            let state =
                { state with
                    TypeSystem = typeSystem
                }

            match verdict with
            | Ok verdict -> state, verdictFailure true verdict
            | Error failure -> state, Some (failureOfBinding failure)

        { state with
            MethodInstantiations = state.MethodInstantiations.Add (key, failure)
        },
        failure

    /// A type as spellings in different assemblies give it, so that two can be compared: each
    /// reference bound to the definition it names, each primitive to its CoreLib type.
    [<RequireQualifiedAccess>]
    type private Shape =
        | Named of ResolvedTypeIdentity * arguments : Shape list
        | Variable of ofMethod : bool * index : int
        | Array of element : Shape * rank : int option
        | Pointer of Shape
        | Byref of Shape
        /// A function pointer, `void`, or a reference that does not bind.
        | Opaque

    /// The shape of a spelling of `assembly`, each type variable `substitute` gives a shape for
    /// replaced by it. Custom modifiers are looked through.
    let rec private shapeOf
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (substitute : bool -> int -> Shape option)
        (spelling : TypeDefn)
        : EscapeAnalysisState * Shape
        =
        let recurse = shapeOf state assembly substitute

        match spelling with
        | TypeDefn.FromDefinition (identity, _) -> state, Shape.Named (identity, [])
        | TypeDefn.FromReference (typeRef, _) ->
            match resolveTypeRef state assembly typeRef with
            | state, Some identity -> state, Shape.Named (identity, [])
            | state, None -> state, Shape.Opaque
        | TypeDefn.PrimitiveType primitive ->
            state, Shape.Named ((BaseClassTypes.ofPrimitive state.BaseTypes primitive).Identity, [])
        | TypeDefn.GenericInstantiation (root, arguments) ->
            match shapeOf state assembly substitute root with
            | state, Shape.Named (identity, []) ->
                let state, arguments =
                    ((state, []), arguments)
                    ||> Seq.fold (fun (state, shapes) argument ->
                        let state, shape = shapeOf state assembly substitute argument
                        state, shape :: shapes
                    )

                state, Shape.Named (identity, List.rev arguments)
            | state, _ -> state, Shape.Opaque
        | TypeDefn.GenericTypeParameter index ->
            state, substitute false index |> Option.defaultValue (Shape.Variable (false, index))
        | TypeDefn.GenericMethodParameter index ->
            state, substitute true index |> Option.defaultValue (Shape.Variable (true, index))
        | TypeDefn.OneDimensionalArrayLowerBoundZero element ->
            let state, element = recurse element
            state, Shape.Array (element, None)
        | TypeDefn.Array (element, rank) ->
            let state, element = recurse element
            state, Shape.Array (element, Some rank)
        | TypeDefn.Pointer element ->
            let state, element = recurse element
            state, Shape.Pointer element
        | TypeDefn.Byref element ->
            let state, element = recurse element
            state, Shape.Byref element
        | TypeDefn.Pinned element -> recurse element
        | TypeDefn.Modified modified -> recurse modified.Unmodified
        | TypeDefn.FunctionPointer _
        | TypeDefn.Void -> state, Shape.Opaque

    let private noSubstitution : bool -> int -> Shape option = fun _ _ -> None

    /// Whether a shape mentions a type variable.
    let rec private shapeMentionsVariable (shape : Shape) : bool =
        match shape with
        | Shape.Variable _ -> true
        | Shape.Named (_, arguments) -> List.exists shapeMentionsVariable arguments
        | Shape.Array (element, _)
        | Shape.Pointer element
        | Shape.Byref element -> shapeMentionsVariable element
        | Shape.Opaque -> false

    /// Whether a shape is unknown anywhere in it.
    let rec private shapeHasOpaque (shape : Shape) : bool =
        match shape with
        | Shape.Opaque -> true
        | Shape.Named (_, arguments) -> List.exists shapeHasOpaque arguments
        | Shape.Array (element, _)
        | Shape.Pointer element
        | Shape.Byref element -> shapeHasOpaque element
        | Shape.Variable _ -> false

    /// The type variables in scope where shapes over type variables are spelled: an assembly, and
    /// the type parameters of its declaring type and of its method, whose `Shape.Variable`s these are.
    type private VariableScope =
        {
            Assembly : DumpedAssembly
            TypeGenerics : GenericParamFromMetadata ImmutableArray
            MethodGenerics : GenericParamFromMetadata ImmutableArray
        }

    /// Whether some generic parameter constrains its arguments by a special or a general
    /// constraint.
    let private constrains (metadata : GenericParamMetadata) : bool =
        metadata.RequiresReferenceType
        || metadata.RequiresNonNullableValueType
        || metadata.RequiresParameterlessConstructor
        || not metadata.Constraints.IsEmpty

    /// How many steps from a type its base classes and interfaces are followed, by
    /// `ancestorShapes`. Past that, an ancestor is not known, which can only leave a constraint
    /// undecided.
    let private ancestorDepthLimit : int = 8

    /// The shapes of `shapes`, and of every base class and interface each named type's definition
    /// declares, instantiated as the shape is, transitively: what a type of each such shape can be
    /// cast to without variance or the array rules. `System.Object` is among them.
    let private ancestorShapes (state : EscapeAnalysisState) (shapes : Shape list) : EscapeAnalysisState * Shape list =
        let rec walk (state : EscapeAnalysisState) (found : Shape list) (pending : (Shape * int) list) =
            match pending with
            | [] -> state, found
            | (shape, _) :: rest when List.contains shape found -> walk state found rest
            | (shape, depth) :: rest ->
                let found = shape :: found

                match shape with
                | Shape.Named (identity, arguments) when depth < ancestorDepthLimit ->
                    let definedIn, definition = definitionOf state identity
                    let argumentArray = Array.ofList arguments

                    let substitute (ofMethod : bool) (index : int) : Shape option =
                        if not ofMethod && index < argumentArray.Length then
                            Some argumentArray.[index]
                        else
                            Some Shape.Opaque

                    let state, parents =
                        ((state, []), TypeLoad.fullLoadParts definedIn definition)
                        ||> List.fold (fun (state, parents) part ->
                            match part with
                            | TypeLoad.FullLoadPart.Base spelling
                            | TypeLoad.FullLoadPart.Interface spelling ->
                                let state, parent = shapeOf state definedIn substitute spelling
                                state, (parent, depth + 1) :: parents
                            | TypeLoad.FullLoadPart.ValueTypeField _
                            | TypeLoad.FullLoadPart.UnmodelledInterface _ -> state, parents
                        )

                    walk state found (List.rev parents @ rest)
                | _ -> walk state found rest

        let state, found = walk state [] (shapes |> List.map (fun shape -> shape, 0))
        state, Shape.Named (state.BaseTypes.Object.Identity, []) :: found

    /// <summary>
    /// Whether <paramref name="argument" />, a shape over the type variables of
    /// <paramref name="scope" />, given as the argument of a type parameter
    /// <paramref name="parameter" /> of <paramref name="parameterAssembly" /> whose owner's type
    /// variables <paramref name="substitute" /> gives, satisfies that parameter's constraints for
    /// every instantiation of the variables, and with the variables standing for themselves.
    /// </summary>
    /// <remarks>
    /// <para>
    /// A type variable satisfies the parameter where its own constraints already demand at least as
    /// much: <c>class</c> needs <c>class</c> or a constraint to a class other than
    /// <c>System.Object</c>, <c>System.ValueType</c> and <c>System.Enum</c>, <c>struct</c> needs
    /// <c>struct</c>, <c>new()</c> needs
    /// <c>new()</c> or <c>struct</c>, a variable that allows a byref-like argument needs a parameter
    /// that does too, and each general constraint, instantiated, must be among the ancestors of the
    /// variable's own constraints (<c>ancestorShapes</c>), or <c>System.ValueType</c> for a
    /// <c>struct</c> variable.
    /// </para>
    /// <para>
    /// Another argument mentioning a variable satisfies it by what its definition is: a value type
    /// other than <c>Nullable</c> for <c>struct</c>, not one for <c>class</c>, a value type for
    /// <c>new()</c>, not byref-like unless the parameter allows it, and each general constraint,
    /// instantiated, among its own ancestors. A constraint mentioning a variable
    /// <paramref name="substitute" /> does not give decides nothing.
    /// </para>
    /// </remarks>
    let private argumentSatisfies
        (state : EscapeAnalysisState)
        (scope : VariableScope)
        (argument : Shape)
        (parameterAssembly : DumpedAssembly)
        (parameter : GenericParamMetadata)
        (substitute : bool -> int -> Shape option)
        : EscapeAnalysisState * bool
        =
        let valueTypeShape = Shape.Named (state.BaseTypes.ValueType.Identity, [])

        // Whether every general constraint, instantiated, is among `ancestors`.
        let general (state : EscapeAnalysisState) (ancestors : Shape list) (isValueType : bool) =
            ((state, true), parameter.Constraints)
            ||> Seq.fold (fun (state, satisfied) constraint' ->
                if not satisfied then
                    state, false
                else

                let state, required = shapeOf state parameterAssembly substitute constraint'

                state,
                not (shapeHasOpaque required)
                && ((required = valueTypeShape && isValueType) || List.contains required ancestors)
            )

        match argument with
        | Shape.Variable (ofMethod, index) ->
            let generics =
                if ofMethod then
                    scope.MethodGenerics
                else
                    scope.TypeGenerics

            if index >= generics.Length then
                state, false
            else

            let own = snd generics.[index]

            let state, owned =
                ((state, []), own.Constraints)
                ||> Seq.fold (fun (state, shapes) constraint' ->
                    let state, shape = shapeOf state scope.Assembly noSubstitution constraint'
                    state, shape :: shapes
                )

            // A variable constrained to a class other than `Object`, `ValueType` and `Enum` is a
            // reference type, as CoreCLR's `SatisfiesSpecialConstraintRecursive` takes it.
            let constrainedToClass =
                owned
                |> List.exists (fun shape ->
                    match shape with
                    | Shape.Named (identity, _) when
                        identity <> state.BaseTypes.Object.Identity
                        && identity <> state.BaseTypes.ValueType.Identity
                        && identity <> state.BaseTypes.Enum.Identity
                        ->
                        let _, definition = definitionOf state identity

                        not definition.IsInterface
                        && not (
                            LoadedTypeInfo.isValueType state.BaseTypes state.TypeSystem._LoadedAssemblies definition
                        )
                    | _ -> false
                )

            // CoreCLR's check of the variable standing for itself does not ask whether it allows a
            // byref-like argument, but an instantiation passing one fails the parameter's check.
            let special =
                (not parameter.RequiresReferenceType
                 || own.RequiresReferenceType
                 || constrainedToClass)
                && (not parameter.RequiresNonNullableValueType || own.RequiresNonNullableValueType)
                && (not parameter.RequiresParameterlessConstructor
                    || own.RequiresParameterlessConstructor
                    || own.RequiresNonNullableValueType)
                && (parameter.AllowsByRefLike || not own.AllowsByRefLike)

            if not special then
                state, false
            else

            let state, ancestors = ancestorShapes state owned
            general state ancestors own.RequiresNonNullableValueType
        | Shape.Named (identity, _) ->
            let _, definition = definitionOf state identity
            let loaded = state.TypeSystem._LoadedAssemblies
            let isValueType = LoadedTypeInfo.isValueType state.BaseTypes loaded definition
            let isNullable = identity = state.BaseTypes.Nullable.Identity

            let special =
                (not parameter.RequiresNonNullableValueType || (isValueType && not isNullable))
                && (not parameter.RequiresReferenceType || not isValueType)
                && (not parameter.RequiresParameterlessConstructor || isValueType)
                && (parameter.AllowsByRefLike
                    || not (LoadedTypeInfo.isByRefLike state.BaseTypes loaded definition))

            if not special then
                state, false
            else

            let state, ancestors = ancestorShapes state [ argument ]
            general state ancestors isValueType
        | Shape.Array _
        | Shape.Pointer _
        | Shape.Byref _
        | Shape.Opaque -> state, not (constrains parameter)

    /// <summary>
    /// Whether the type parameters <paramref name="generics" /> of
    /// <paramref name="ownerAssembly" />, given <paramref name="arguments" />, shapes over the
    /// type variables of <paramref name="scope" />, may reject them for some instantiation of those
    /// variables, or as themselves: whether an argument mentioning a variable does not satisfy its
    /// parameter as <c>argumentSatisfies</c> decides. An argument mentioning no variable is closed,
    /// and is checked exactly elsewhere.
    /// </summary>
    let private instantiationMayBreak
        (state : EscapeAnalysisState)
        (scope : VariableScope)
        (ownerAssembly : DumpedAssembly)
        (generics : GenericParamFromMetadata ImmutableArray)
        (substitute : bool -> int -> Shape option)
        (arguments : Shape list)
        : EscapeAnalysisState * bool
        =
        if generics.Length <> arguments.Length then
            state, true
        else

        ((state, false), Seq.zip generics arguments)
        ||> Seq.fold (fun (state, breaks) ((_, parameter), argument) ->
            if breaks || not (shapeMentionsVariable argument) then
                state, breaks
            else
                let state, satisfied =
                    argumentSatisfies state scope argument ownerAssembly parameter substitute

                state, not satisfied
        )

    /// <summary>
    /// Whether the full load of a type that a shape over the type variables of
    /// <paramref name="scope" /> gives may break a constraint, for some instantiation of the
    /// variables or for the variables themselves: at an instantiation the shape spells
    /// (<c>instantiationMayBreak</c>), or at one the full load of an instantiated definition brings
    /// through its type parameters (<c>loadMayBreakThroughParameters</c>). The instantiation
    /// <paramref name="typical" /> names, whose arguments are its own type parameters, is the scope's
    /// own declaring type, which is loaded already. What is closed is checked exactly elsewhere.
    /// </summary>
    let rec private shapeMayBreak
        (state : EscapeAnalysisState)
        (scope : VariableScope)
        (typical : ResolvedTypeIdentity option)
        (shape : Shape)
        : EscapeAnalysisState * bool
        =
        match shape with
        | Shape.Named (identity, arguments) when List.exists shapeMentionsVariable arguments ->
            let isTypical =
                typical = Some identity
                && arguments
                   |> List.indexed
                   |> List.forall (fun (index, argument) -> argument = Shape.Variable (false, index))

            if isTypical then
                state, false
            else

            let ownerAssembly, owner = definitionOf state identity
            let argumentArray = Array.ofList arguments

            let substitute (ofMethod : bool) (index : int) : Shape option =
                if not ofMethod && index < argumentArray.Length then
                    Some argumentArray.[index]
                else
                    Some Shape.Opaque

            match instantiationMayBreak state scope ownerAssembly owner.Generics substitute arguments with
            | state, true -> state, true
            | state, false ->

            match loadMayBreakThroughParameters state identity with
            | state, true -> state, true
            | state, false ->

            ((state, false), arguments)
            ||> List.fold (fun (state, breaks) argument ->
                if breaks then
                    state, true
                else
                    shapeMayBreak state scope typical argument
            )
        | Shape.Named _
        | Shape.Variable _
        | Shape.Opaque -> state, false
        | Shape.Array (element, _)
        | Shape.Pointer element
        | Shape.Byref element -> shapeMayBreak state scope typical element

    /// Whether CoreCLR's full load of an instantiation of the definition `identity` may break a
    /// constraint through the definition's type parameters: whether what the load brings
    /// (`TypeLoad.fullLoadParts`), each spelled over those parameters, may, as `shapeMayBreak` says
    /// with the definition's own type parameters in scope.
    and private loadMayBreakThroughParameters
        (state : EscapeAnalysisState)
        (identity : ResolvedTypeIdentity)
        : EscapeAnalysisState * bool
        =
        match state.ParameterBreaks.TryFind identity with
        | Some known -> state, known
        | None ->

        // A definition its own load comes back to adds nothing on the way round.
        let state =
            { state with
                ParameterBreaks = state.ParameterBreaks.Add (identity, false)
            }

        let definedIn, definition = definitionOf state identity

        let scope =
            {
                Assembly = definedIn
                TypeGenerics = definition.Generics
                MethodGenerics = ImmutableArray.Empty
            }

        let state, found =
            if definition.Generics.IsEmpty then
                state, false
            else

            ((state, false), TypeLoad.fullLoadParts definedIn definition)
            ||> List.fold (fun (state, found) part ->
                if found then
                    state, true
                else

                match part with
                | TypeLoad.FullLoadPart.Base spelling
                | TypeLoad.FullLoadPart.Interface spelling
                | TypeLoad.FullLoadPart.ValueTypeField spelling ->
                    let state, shape = shapeOf state definedIn noSubstitution spelling
                    shapeMayBreak state scope (Some identity) shape
                | TypeLoad.FullLoadPart.UnmodelledInterface _ -> state, true
            )

        { state with
            ParameterBreaks = state.ParameterBreaks.Add (identity, found)
        },
        found

    /// The scope of a body of `method` in `assembly`: its declaring type's and its own type
    /// parameters.
    let private scopeOf
        (assembly : DumpedAssembly)
        (method : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        : VariableScope
        =
        {
            Assembly = assembly
            TypeGenerics = method.DeclaringTypeGenerics
            MethodGenerics = method.Generics
        }

    /// Whether the full load of a type that a body of `method` in `assembly` spells over the
    /// method's type variables may break a constraint, for some instantiation of them or for the
    /// variables themselves (`shapeMayBreak`).
    let private spellingMayBreakConstraint
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (method : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        (spelling : TypeDefn)
        : EscapeAnalysisState * bool
        =
        if not (mentionsTypeVariable spelling) then
            state, false
        else

        let state, shape = shapeOf state assembly noSubstitution spelling
        shapeMayBreak state (scopeOf assembly method) (Some method.RequiredDeclaringType.Identity) shape

    /// Whether a method specification of `assembly`, in a body of `method`, may break a constraint
    /// as `spellingMayBreakConstraint` says of a type: at the method it instantiates over the
    /// body's type variables (`instantiationMayBreak`), the method's constraints instantiated with
    /// its parent's arguments where a member reference's parent spells them, or in the full load of
    /// what its arguments or its parent spell.
    let private specificationMayBreakConstraint
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (method : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        (handle : MethodSpecificationHandle)
        : EscapeAnalysisState * bool
        =
        let spec = assembly.MethodSpecs.[handle]
        let scope = scopeOf assembly method
        let typical = Some method.RequiredDeclaringType.Identity

        let state, arguments =
            ((state, []), spec.Signature)
            ||> Seq.fold (fun (state, shapes) argument ->
                let state, shape = shapeOf state assembly noSubstitution argument
                state, shape :: shapes
            )

        let arguments = List.rev arguments

        let parent =
            match spec.Method with
            | MetadataToken.MemberReference reference ->
                match assembly.Members.[reference].Parent with
                | MetadataToken.TypeSpecification parent -> Some assembly.TypeSpecs.[parent].Signature
                | _ -> None
            | _ -> None

        let state, parentShape =
            match parent with
            | Some parent ->
                let state, shape = shapeOf state assembly noSubstitution parent
                state, Some shape
            | None -> state, None

        let state, ownBreaks =
            if not (List.exists shapeMentionsVariable arguments) then
                state, false
            else

            match callTarget state assembly (MetadataToken.MethodSpecification handle) with
            | state, CallTarget.Method callee ->
                let calleeAssembly, calleeMethod = methodOf state callee.Callee
                let argumentArray = Array.ofList arguments

                let parentArguments =
                    match parentShape with
                    | Some (Shape.Named (_, parentArguments)) -> Array.ofList parentArguments
                    | _ -> [||]

                let substitute (ofMethod : bool) (index : int) : Shape option =
                    if ofMethod && index < argumentArray.Length then
                        Some argumentArray.[index]
                    elif not ofMethod && index < parentArguments.Length then
                        Some parentArguments.[index]
                    else
                        Some Shape.Opaque

                instantiationMayBreak state scope calleeAssembly calleeMethod.Generics substitute arguments
            | state, _ -> state, false

        if ownBreaks then
            state, true
        else

        ((state, false), Option.toList parentShape @ arguments)
        ||> List.fold (fun (state, breaks) shape ->
            if breaks then
                state, true
            else
                shapeMayBreak state scope typical shape
        )

    /// This assembly's module initializer, the type initializer of `<Module>`, if it has one.
    let private moduleInitializer (state : EscapeAnalysisState) (assemblyFullName : string) : MethodKey option =
        let assembly = assemblyOf state assemblyFullName

        match assembly.TypeDefs.TryGetValue (MetadataTokens.TypeDefinitionHandle 1) with
        | true, globalType when globalType.Name = "<Module>" -> typeInitializer state globalType.Identity
        | _ -> None

    /// Whether binding a token with this opcode activates the modules of what it names, running
    /// each one's module initializer first if it has not run: CoreCLR's `CEEInfo::resolveToken`
    /// does for a method, a static field, and the type of a `box`, `constrained.` or `ldtoken`. A
    /// field token counts whatever the opcode, since `ldfld` may name a static field.
    let private activatesModules (op : UnaryMetadataTokenIlOp) : bool =
        match op with
        | UnaryMetadataTokenIlOp.Call
        | UnaryMetadataTokenIlOp.Callvirt
        | UnaryMetadataTokenIlOp.Newobj
        | UnaryMetadataTokenIlOp.Ldftn
        | UnaryMetadataTokenIlOp.Ldvirtftn
        | UnaryMetadataTokenIlOp.Jmp
        | UnaryMetadataTokenIlOp.Stfld
        | UnaryMetadataTokenIlOp.Stsfld
        | UnaryMetadataTokenIlOp.Ldfld
        | UnaryMetadataTokenIlOp.Ldflda
        | UnaryMetadataTokenIlOp.Ldsfld
        | UnaryMetadataTokenIlOp.Ldsflda
        | UnaryMetadataTokenIlOp.Box
        | UnaryMetadataTokenIlOp.Constrained
        | UnaryMetadataTokenIlOp.Ldtoken -> true
        | UnaryMetadataTokenIlOp.Calli
        | UnaryMetadataTokenIlOp.Castclass
        | UnaryMetadataTokenIlOp.Newarr
        | UnaryMetadataTokenIlOp.Ldelema
        | UnaryMetadataTokenIlOp.Isinst
        | UnaryMetadataTokenIlOp.Unbox_Any
        | UnaryMetadataTokenIlOp.Stelem
        | UnaryMetadataTokenIlOp.Ldelem
        | UnaryMetadataTokenIlOp.Initobj
        | UnaryMetadataTokenIlOp.Stobj
        | UnaryMetadataTokenIlOp.Cpobj
        | UnaryMetadataTokenIlOp.Ldobj
        | UnaryMetadataTokenIlOp.Sizeof
        | UnaryMetadataTokenIlOp.Unbox
        | UnaryMetadataTokenIlOp.Mkrefany
        | UnaryMetadataTokenIlOp.Refanyval -> false

    /// The types whose modules binding `token` activates, as `CEEInfo::EnsureActive` walks them:
    /// the type it names, or the type declaring what it names, with every type argument spelled,
    /// and all their base types.
    let private activatedTypes
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        : EscapeAnalysisState * ResolvedTypeIdentity list
        =
        // A MemberRef's parent stands for the type declaring its target, which is the parent or
        // one of its base types.
        let rec spelled (state : EscapeAnalysisState) (token : MetadataToken) =
            match token with
            | MetadataToken.TypeDefinition handle -> state, [ assembly.TypeDefs.[handle].Identity ]
            | MetadataToken.MethodDef handle -> state, [ assembly.Methods.[handle].RequiredDeclaringType.Identity ]
            | MetadataToken.TypeReference handle ->
                namedIdentities
                    state
                    assembly
                    (TypeDefn.FromReference (assembly.TypeRefs.[handle], SignatureTypeKind.Unknown))
            | MetadataToken.TypeSpecification handle ->
                namedIdentities state assembly assembly.TypeSpecs.[handle].Signature
            | MetadataToken.MethodSpecification handle ->
                let spec = assembly.MethodSpecs.[handle]
                let state, inner = spelled state spec.Method

                ((state, inner), spec.Signature)
                ||> Seq.fold (fun (state, acc) argument ->
                    let state, named = namedIdentities state assembly argument
                    state, named @ acc
                )
            | MetadataToken.MemberReference handle -> spelled state assembly.Members.[handle].Parent
            | MetadataToken.FieldDefinition handle -> state, [ assembly.Fields.[handle].DeclaringType.Identity ]
            | _ -> state, []

        let state, named = spelled state token

        // Every base type, from each type named, and every type argument a base type spells.
        let rec withBases
            (state : EscapeAnalysisState)
            (pending : ResolvedTypeIdentity list)
            (seen : Set<ResolvedTypeIdentity>)
            =
            match pending with
            | [] -> state, seen
            | identity :: rest when seen.Contains identity -> withBases state rest seen
            | identity :: rest ->
                let definingAssembly, ty = definitionOf state identity

                let state, arguments =
                    match ty.BaseType with
                    | Some (BaseTypeInfo.TypeSpec handle) ->
                        namedIdentities state definingAssembly definingAssembly.TypeSpecs.[handle].Signature
                    | _ -> state, []

                match baseOf state identity with
                | state, Some parent -> withBases state (parent :: arguments @ rest) (seen.Add identity)
                | state, None -> withBases state (arguments @ rest) (seen.Add identity)

        let state, all = withBases state named Set.empty
        state, Set.toList all

    /// What a token an instruction names binds to, as far as what the JIT does after binding it
    /// depends on it.
    [<RequireQualifiedAccess>]
    type private TokenReferent =
        /// A method, or the reason a method reference names none (`callTarget`).
        | Method of CallTarget
        /// A field definition: the assembly that declares it, and its row there.
        | Field of declaringAssembly : DumpedAssembly * field : FieldDefinitionHandle
        /// A type, or a field reference that names no field definition.
        | Other

    /// The type a member token names as the member's owner, as `assembly` spells it, which the JIT
    /// loads before it resolves the member (`JitTypePosition.DeclaringType`): a method or field
    /// definition's declaring type, or that of the method a vararg call site's member reference
    /// names. `None` for a token naming anything but a member, and for any other member reference,
    /// whose parent resolving the reference loads (`MemberReferenceParent.resolve`).
    let private ownerSpelling (assembly : DumpedAssembly) (token : MetadataToken) : TypeDefn option =
        let definition (identity : ResolvedTypeIdentity) : TypeDefn =
            TypeDefn.FromDefinition (identity, SignatureTypeKind.Unknown)

        match token with
        | MetadataToken.MethodDef handle -> Some (definition assembly.Methods.[handle].RequiredDeclaringType.Identity)
        | MetadataToken.FieldDefinition handle -> Some (definition assembly.Fields.[handle].DeclaringType.Identity)
        | MetadataToken.MemberReference handle ->
            match assembly.Members.[handle].Parent with
            | MetadataToken.MethodDef parent ->
                Some (definition assembly.Methods.[parent].RequiredDeclaringType.Identity)
            | _ -> None
        | _ -> None

    /// Bind the metadata token an instruction names, as the JIT does before the body runs: what it
    /// names when that is a member, what binding it can throw, and the constraint checks binding it
    /// makes that only an instance of the body decides (`InstanceCheck`). The JIT loads a member's
    /// owner first, and resolves the member only if that load succeeds; it loads in full each type a
    /// token names, and the parent a member reference names (`jitFullLoad`), and instantiates the
    /// method a method specification names, checking its constraints
    /// (`InstanceCheck.Instantiates`).
    let rec private bindToken
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        : EscapeAnalysisState * TokenReferent * OutsideBodyFact list * InstanceCheck list
        =
        let raisesOf (failure : BindFailure option) : OutsideBodyFact list =
            failure |> Option.map (bindFailureFacts state) |> Option.defaultValue []

        let dependsOnInstantiation = OutsideBodyFact.Opaque Opacity.DependsOnInstantiation

        let state, ownerFailure, ownerChecks =
            match ownerSpelling assembly token with
            | None -> state, None, []
            | Some owner ->
                match JitTypeLoads.load JitTypePosition.DeclaringType owner with
                | None -> state, None, []
                | Some (JitTypeLoad.InFull owner) -> jitFullLoad state assembly owner
                | Some load ->
                    failwith
                        $"BUG: JitTypeLoads.load answers %O{load} for the owner of %O{token} in %s{assembly.DefinitionFullName}, which loads in full or not at all"

        // Where the owner fails to load, the member is not resolved, and only the owner's failure is
        // raised.
        let unlessOwnerFails (failures : OutsideBodyFact list) : OutsideBodyFact list =
            match ownerFailure with
            | Some _ -> raisesOf ownerFailure
            | None -> failures

        // A member reference's parent resolving the reference loads, and reports what fails to bind
        // (`callTarget`, `FieldReferenceResolution`); once it binds, what checking its full load's
        // constraints raises.
        let parentConstraints (state : EscapeAnalysisState) (handle : MemberReferenceHandle) =
            let spelling =
                match assembly.Members.[handle].Parent with
                | MetadataToken.TypeSpecification parent -> Some assembly.TypeSpecs.[parent].Signature
                | MetadataToken.TypeReference parent ->
                    Some (TypeDefn.FromReference (assembly.TypeRefs.[parent], SignatureTypeKind.Unknown))
                | MetadataToken.TypeDefinition parent ->
                    Some (TypeDefn.FromDefinition (assembly.TypeDefs.[parent].Identity, SignatureTypeKind.Unknown))
                | _ -> None

            match spelling with
            | None -> state, [], []
            | Some spelling ->
                let state, failure, checks = fullLoadConstraints state assembly spelling
                state, raisesOf failure, checks

        match token with
        | MetadataToken.MethodDef _ ->
            let state, target = callTarget state assembly token
            state, TokenReferent.Method target, unlessOwnerFails [], ownerChecks
        | MetadataToken.FieldDefinition handle ->
            state, TokenReferent.Field (assembly, handle), unlessOwnerFails [], ownerChecks
        | MetadataToken.MethodSpecification handle ->
            let spec = assembly.MethodSpecs.[handle]
            let state, _, failures, checks = bindToken state assembly spec.Method
            // The specification, not the method it instantiates, says how the callee is instantiated.
            let state, target = callTarget state assembly token

            let state, argumentsFailure, argumentChecks =
                firstJitFullLoadFailure state assembly spec.Signature

            let failures = raisesOf argumentsFailure @ failures

            // Once the method and its arguments load, CoreCLR instantiates it, which an instance
            // decides, the arguments being spelled in the body.
            let instantiates =
                match target, failures with
                | CallTarget.Method _, [] -> [ InstanceCheck.Instantiates (assembly.DefinitionFullName, handle) ]
                | _ -> []

            state, TokenReferent.Method target, failures, checks @ argumentChecks @ instantiates
        | MetadataToken.MemberReference handle ->
            match assembly.Members.[handle].Signature with
            | MemberSignature.Method _ ->
                let state, target = callTarget state assembly token

                let failures =
                    match target with
                    | CallTarget.Missing ->
                        let thrown, made = constructedRaise state "System.MissingMethodException"
                        [ OutsideBodyFact.Raises thrown ; OutsideBodyFact.Runs (made, []) ]
                    | CallTarget.TypeMissing -> raisesOf (Some BindFailure.TypeAbsent)
                    | CallTarget.AssemblyMissing -> raisesOf (Some BindFailure.AssemblyUnavailable)
                    | CallTarget.DependsOnInstantiation -> [ dependsOnInstantiation ]
                    | _ -> []

                let state, failures, checks =
                    match ownerFailure, failures with
                    | None, [] ->
                        let state, failures, checks = parentConstraints state handle
                        state, failures, ownerChecks @ checks
                    | _ -> state, failures, ownerChecks

                state, TokenReferent.Method target, unlessOwnerFails failures, checks
            | MemberSignature.Field _ ->
                let assemblies, target =
                    FieldReferenceResolution.resolve
                        state.LoggerFactory
                        state.RuntimeDirs
                        state.BaseTypes
                        state.TypeSystem._LoadedAssemblies
                        assembly
                        handle

                let state = withAssemblies state assemblies

                match target with
                | FieldReferenceTarget.Missing ->
                    let thrown, made = constructedRaise state "System.MissingFieldException"
                    state, TokenReferent.Other, [ OutsideBodyFact.Raises thrown ; OutsideBodyFact.Runs (made, []) ], []
                | FieldReferenceTarget.ParentTypeMissing _ ->
                    state, TokenReferent.Other, raisesOf (Some BindFailure.TypeAbsent), []
                | FieldReferenceTarget.ParentAssemblyUnavailable _
                | FieldReferenceTarget.SignatureAssemblyUnavailable _ ->
                    state, TokenReferent.Other, raisesOf (Some BindFailure.AssemblyUnavailable), []
                | FieldReferenceTarget.DependsOnInstantiation ->
                    state, TokenReferent.Other, [ dependsOnInstantiation ], []
                | FieldReferenceTarget.Defined (declaring, field) ->
                    let state, failures, checks = parentConstraints state handle
                    state, TokenReferent.Field (declaring, field), failures, checks
                | FieldReferenceTarget.Unmodelled unmodelled -> failwith unmodelled.Diagnostic
        // The JIT loads the type a type token names in full.
        | MetadataToken.TypeDefinition handle ->
            let state, failure, checks =
                jitFullLoad
                    state
                    assembly
                    (TypeDefn.FromDefinition (assembly.TypeDefs.[handle].Identity, SignatureTypeKind.Unknown))

            state, TokenReferent.Other, raisesOf failure, checks
        | MetadataToken.TypeReference handle ->
            let state, failure, checks =
                jitFullLoad
                    state
                    assembly
                    (TypeDefn.FromReference (assembly.TypeRefs.[handle], SignatureTypeKind.Unknown))

            state, TokenReferent.Other, raisesOf failure, checks
        | MetadataToken.TypeSpecification handle ->
            let state, failure, checks =
                jitFullLoad state assembly assembly.TypeSpecs.[handle].Signature

            state, TokenReferent.Other, raisesOf failure, checks
        | _ -> state, TokenReferent.Other, [], []

    /// What CoreCLR's `CEEInfo::resolveToken` raises refusing a token that binds, for the type it
    /// names: a byref or `System.Void` (`TypeOperand.ofTypeToken`), refused with
    /// `InvalidProgramException` for every kind of token but `ldtoken`'s, whose caller must not ask.
    /// For a member's token the type is the parent its reference names, and for a method
    /// specification's the parent of the method it instantiates.
    let rec private resolutionRefusal
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        : OutsideBodyFact list
        =
        match token with
        | MetadataToken.MethodSpecification handle ->
            resolutionRefusal state assembly assembly.MethodSpecs.[handle].Method
        | _ ->

        let typeToken =
            match token with
            | MetadataToken.MemberReference handle -> assembly.Members.[handle].Parent
            | _ -> token

        match TypeOperand.ofTypeToken state.BaseTypes state.TypeSystem._LoadedAssemblies assembly typeToken with
        | Ok None -> []
        // `COMPlusThrow(kInvalidProgramException)` makes it as `EEException::CreateThrowable` does.
        | Ok (Some _) ->
            let thrown, made = constructedRaise state "System.InvalidProgramException"
            [ OutsideBodyFact.Raises thrown ; OutsideBodyFact.Runs (made, []) ]
        | Error typeRef ->
            failwith
                $"BUG: %O{typeRef} in %s{assembly.DefinitionFullName} bound, loading what it names, but does not resolve against the loaded assemblies"

    /// The signature of the method `token` names, as `assembly` spells it at the token, or a
    /// `calli`'s call-site signature; `None` for a token naming anything else.
    let rec private spelledSignature
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        : TypeMethodSignature<TypeDefn> option
        =
        match token with
        | MetadataToken.MethodDef handle -> Some assembly.Methods.[handle].Signature
        | MetadataToken.MethodSpecification handle -> spelledSignature assembly assembly.MethodSpecs.[handle].Method
        | MetadataToken.MemberReference handle ->
            match assembly.Members.[handle].Signature with
            | MemberSignature.Method signature -> Some signature
            | MemberSignature.Field _ -> None
        | MetadataToken.StandaloneSignature handle ->
            (assembly.PeReader.GetMetadataReader().GetStandaloneSignature handle)
                .DecodeMethodSignature (TypeDefn.typeProvider assembly.Name, ())
            |> TypeMethodSignature.make
            |> Some
        | _ -> None

    /// What an instruction of `assembly` with opcode `op`, at `placement` in its method's body, does
    /// with the method it names, whose signature, as `assembly` spells it, is `signature`, given what
    /// `bindToken` bound its token to; `None` for `ldftn` and `ldvirtftn`, and for an instruction that
    /// names no method. Which CoreCLR implements by a P/Invoke, and by which calling convention, is the
    /// target's to say; a `calli`'s calling convention is the signature's.
    let private memberOperand
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (placement : UnmanagedCallPlacement)
        (op : UnaryMetadataTokenIlOp)
        (signature : TypeMethodSignature<TypeDefn>)
        (target : CallTarget option)
        : MemberOperand option
        =
        let unmanaged (convention : UnmanagedConventionRead) (target : UnmanagedTarget) : MemberOperand option =
            {
                Convention = convention
                Placement = placement
                CallerDisablesOptimisation = Debuggable.allowsJitOptimisation assembly = Some false
                Target = target
            }
            |> MemberOperand.UnmanagedCall
            |> Some

        match op with
        | UnaryMetadataTokenIlOp.Call
        | UnaryMetadataTokenIlOp.Callvirt
        | UnaryMetadataTokenIlOp.Newobj ->
            match target with
            | Some (CallTarget.Method callee) ->
                let calleeAssembly, calleeMethod = methodOf state callee.Callee

                match calleeMethod.Body with
                | MethodBody.PInvoke ->
                    unmanaged
                        (UnmanagedCallingConvention.ofPInvoke calleeAssembly callee.Callee.Method.Get)
                        (UnmanagedTarget.ofPInvoke calleeAssembly callee.Callee.Method.Get)
                | _ -> Some MemberOperand.Call
            | _ -> Some MemberOperand.Call
        | UnaryMetadataTokenIlOp.Jmp -> Some MemberOperand.Jump
        | UnaryMetadataTokenIlOp.Calli ->
            match signature.Header.Get.CallingConvention with
            | SignatureCallingConvention.Default
            | SignatureCallingConvention.VarArgs -> Some MemberOperand.Call
            | _ ->
                unmanaged
                    (UnmanagedCallingConvention.ofCallSite assembly signature)
                    (UnmanagedTarget.ofFunctionPointer assembly)
        | UnaryMetadataTokenIlOp.Ldtoken -> Some MemberOperand.Token
        | _ -> None

    /// As `spellingLoadFailure`, except that where the type reference at the head of `spelling`
    /// (an instantiation's generic type) names a type that the assembly it is looked for in lacks,
    /// nothing is loaded and nothing fails (`JitTypeLoad.InFullIfDefined`). A reference into an
    /// assembly that is not there still fails.
    let private spellingLoadFailureIfDefined
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (spelling : TypeDefn)
        : EscapeAnalysisState * BindFailure option
        =
        let rec head (spelling : TypeDefn) : TypeDefn =
            match TypeDefn.stripCustomModifiers spelling with
            | TypeDefn.GenericInstantiation (generic, _) -> head generic
            | spelling -> spelling

        match head spelling with
        | TypeDefn.FromReference (typeRef, _) ->
            match bindTypeRef state assembly typeRef with
            | state, TypeReferenceIdentity.TypeAbsent _ -> state, None
            | state, TypeReferenceIdentity.Resolved _
            | state, TypeReferenceIdentity.AssemblyUnavailable _ -> spellingLoadFailure state assembly spelling
            | _, TypeReferenceIdentity.Unmodelled unmodelled -> failwith unmodelled.Diagnostic
        | _ -> spellingLoadFailure state assembly spelling

    /// What the loads of the types of a member's signature fail with, where the JIT compiling an
    /// instruction naming the member makes them (`memberSignatureLoads`).
    type private SignatureLoadFailures =
        {
            /// The first load that fails which the JIT makes however it compiles the instruction. It
            /// makes no load after that one.
            Definite : BindFailure option
            /// Each load before `Definite` that the JIT may make or not, and that fails.
            Possible : BindFailure list
            /// The constraint checks of the loads before `Definite`, spelled over the member's own
            /// type variables, that the instantiation the token gives the member decides.
            Deferred : InstanceCheck list
        }

    /// What the JIT's loads of the types of `signature`, the signature of a member which `assembly`
    /// spells, fail with where it compiles an instruction with operand `operand`, meeting them at
    /// `positions` (`JitTypeLoads.memberPositions`, or a prefix of those, and
    /// `JitTypeLoads.unmanagedMeetings` for a call of unmanaged code), the member `token` names.
    let private memberSignatureLoads
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        (operand : MemberOperand)
        (positions : MemberPositions)
        (signature : MemberSignature)
        : EscapeAnalysisState * SignatureLoadFailures
        =
        let loads =
            match positions with
            | MemberPositions.BySignature ->
                match operand, signature with
                | MemberOperand.UnmanagedCall call, MemberSignature.Method signature ->
                    JitTypeLoads.unmanagedMeetings state.Target call signature
                    |> List.choose (fun (position, spelling) -> JitTypeLoads.load position spelling)
                | _ ->
                    failwith
                        $"BUG: JitTypeLoads.memberPositions gives %O{operand} of the member whose signature is %O{signature} by its signature"
            | MemberPositions.Described positions ->
                positions
                |> List.collect (fun position ->
                    let spellings =
                        match position, signature with
                        | JitTypePosition.CalleeReturn, MemberSignature.Method signature ->
                            match signature.ReturnType with
                            | MethodReturnType.Void -> []
                            | MethodReturnType.Returns returned -> [ returned ]
                        | JitTypePosition.CalleeParameter, MemberSignature.Method signature -> signature.ParameterTypes
                        | JitTypePosition.FunctionPointerSignature, MemberSignature.Method signature
                        | JitTypePosition.PossibleFunctionPointerSignature, MemberSignature.Method signature ->
                            signatureTypes signature
                        | JitTypePosition.FieldType, MemberSignature.Field (_, fieldType) -> [ fieldType ]
                        | other, _ ->
                            failwith
                                $"BUG: JitTypeLoads.memberPositions answers %O{other} for %O{operand} of the member whose signature is %O{signature}"

                    spellings |> List.choose (JitTypeLoads.load position)
                )

        let rec failureOf
            (state : EscapeAnalysisState)
            (load : JitTypeLoad)
            : EscapeAnalysisState * BindFailure option * InstanceCheck list
            =
            // A type spelled over the member's own type variables, the token's instantiation decides.
            let constraints (state : EscapeAnalysisState) (loaded : TypeDefn) =
                if mentionsTypeVariable loaded then
                    state, None, [ InstanceCheck.LoadsInMember (assembly.DefinitionFullName, loaded, token) ]
                else
                    fullLoadConstraints state assembly loaded

            match load with
            | JitTypeLoad.InFull loaded ->
                match spellingLoadFailure state assembly loaded with
                | state, Some failure -> state, Some failure, []
                | state, None -> constraints state loaded
            | JitTypeLoad.InFullIfDefined loaded ->
                match spellingLoadFailureIfDefined state assembly loaded with
                | state, Some failure -> state, Some failure, []
                | state, None -> constraints state loaded
            | JitTypeLoad.Approximately loaded ->
                let state, failure = approximateLoadFailure state assembly loaded
                state, failure, []
            | JitTypeLoad.Possibly load -> failureOf state load

        let state, failures =
            ((state,
              {
                  Definite = None
                  Possible = []
                  Deferred = []
              }),
             loads)
            ||> List.fold (fun (state, failures) load ->
                match failures.Definite, load with
                | Some _, _ -> state, failures
                | None, JitTypeLoad.Possibly _ ->
                    let state, failure, deferred = failureOf state load

                    state,
                    { failures with
                        Possible = Option.toList failure @ failures.Possible
                        Deferred = failures.Deferred @ deferred
                    }
                | None, load ->
                    let state, failure, deferred = failureOf state load

                    state,
                    { failures with
                        Definite = failure
                        Deferred = failures.Deferred @ deferred
                    }
            )

        state,
        { failures with
            Possible = List.rev failures.Possible
        }

    /// What CoreCLR's JIT checks a method of `assembly` may access where an instruction's token is
    /// `token`, given what `bindToken` bound a method token to; `None` where the analysis does not
    /// describe what the token names.
    let private accessTarget
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        (bound : CallTarget option)
        : EscapeAnalysisState * AccessTarget option
        =
        let typeSpelling (token : MetadataToken) : TypeDefn option =
            match token with
            | MetadataToken.TypeDefinition handle ->
                Some (TypeDefn.FromDefinition (assembly.TypeDefs.[handle].Identity, SignatureTypeKind.Unknown))
            | MetadataToken.TypeReference handle ->
                Some (TypeDefn.FromReference (assembly.TypeRefs.[handle], SignatureTypeKind.Unknown))
            | MetadataToken.TypeSpecification handle -> Some assembly.TypeSpecs.[handle].Signature
            | _ -> None

        let declaringOf (method : MethodDefinitionHandle) : TypeDefn option =
            Some (
                TypeDefn.FromDefinition (
                    assembly.Methods.[method].RequiredDeclaringType.Identity,
                    SignatureTypeKind.Unknown
                )
            )

        let ofMethod (parent : TypeDefn option) (instantiation : TypeDefn list) : AccessTarget option =
            match bound, parent with
            | Some (CallTarget.Method callee), Some parent ->
                let key = callee.Callee
                let definition = (assemblyOf state key.AssemblyFullName).Methods.[key.Method.Get]

                definition.TryMetadata
                |> Option.map (fun facts ->
                    AccessTarget.Member (
                        parent,
                        definition.RequiredDeclaringType.Identity,
                        MemberVisibility.ofMethodAttributes facts.MethodAttributes,
                        instantiation
                    )
                )
            // The runtime's accessors of an array type are public, so checking one is checking the
            // array type.
            | Some (CallTarget.ArrayAccessor (arrayType, _)), _ -> Some (AccessTarget.Type arrayType)
            | _ -> None

        match token with
        | MetadataToken.TypeDefinition _
        | MetadataToken.TypeReference _
        | MetadataToken.TypeSpecification _ -> state, typeSpelling token |> Option.map AccessTarget.Type
        | MetadataToken.MethodDef handle -> state, ofMethod (declaringOf handle) []
        | MetadataToken.MethodSpecification handle ->
            let spec = assembly.MethodSpecs.[handle]
            let instantiation = List.ofSeq spec.Signature

            match spec.Method with
            | MetadataToken.MethodDef method -> state, ofMethod (declaringOf method) instantiation
            | MetadataToken.MemberReference reference ->
                state, ofMethod (typeSpelling assembly.Members.[reference].Parent) instantiation
            | _ -> state, None
        | MetadataToken.MemberReference handle ->
            let parent = typeSpelling assembly.Members.[handle].Parent

            match assembly.Members.[handle].Signature with
            | MemberSignature.Method _ -> state, ofMethod parent []
            | MemberSignature.Field _ ->
                let assemblies, target =
                    FieldReferenceResolution.resolve
                        state.LoggerFactory
                        state.RuntimeDirs
                        state.BaseTypes
                        state.TypeSystem._LoadedAssemblies
                        assembly
                        handle

                let state = withAssemblies state assemblies

                match target, parent with
                | FieldReferenceTarget.Defined (declaringAssembly, field), Some parent ->
                    let field = declaringAssembly.Fields.[field]

                    state,
                    Some (
                        AccessTarget.Member (
                            parent,
                            field.DeclaringType.Identity,
                            MemberVisibility.ofFieldAttributes field.Attributes,
                            []
                        )
                    )
                | _ -> state, None
        | MetadataToken.FieldDefinition handle ->
            let field = assembly.Fields.[handle]

            state,
            Some (
                AccessTarget.Member (
                    TypeDefn.FromDefinition (field.DeclaringType.Identity, SignatureTypeKind.Unknown),
                    field.DeclaringType.Identity,
                    MemberVisibility.ofFieldAttributes field.Attributes,
                    []
                )
            )
        | _ -> state, None

    /// The static type of the value a call returns, as the call site's own signature spells it.
    let rec private returnTypeOfCall
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        : EscapeAnalysisState * ResolvedTypeIdentity option
        =
        let ofReturn (ret : MethodReturnType<TypeDefn>) =
            match ret with
            | MethodReturnType.Void -> state, None
            | MethodReturnType.Returns ty -> nominalIdentity state assembly ty

        match token with
        | MetadataToken.MethodDef handle -> ofReturn assembly.Methods.[handle].Signature.ReturnType
        | MetadataToken.MemberReference handle ->
            match assembly.Members.[handle].Signature with
            | MemberSignature.Method signature -> ofReturn signature.ReturnType
            | MemberSignature.Field _ -> state, None
        | MetadataToken.MethodSpecification handle ->
            returnTypeOfCall state assembly assembly.MethodSpecs.[handle].Method
        | _ -> state, None

    /// A type a member's signature spells, with the member's type variables replaced by the
    /// arguments a token supplies for them: `typeArguments` for its declaring type's, and
    /// `methodArguments` for a method's own. `None` where a variable has no argument.
    let rec private substituted
        (typeArguments : TypeDefn list)
        (methodArguments : TypeDefn list)
        (spelling : TypeDefn)
        : TypeDefn option
        =
        let go = substituted typeArguments methodArguments

        match spelling with
        | TypeDefn.GenericTypeParameter index -> List.tryItem index typeArguments
        | TypeDefn.GenericMethodParameter index -> List.tryItem index methodArguments
        | TypeDefn.GenericInstantiation (root, arguments) ->
            let arguments = arguments |> Seq.map go |> List.ofSeq

            if arguments |> List.forall Option.isSome then
                go root
                |> Option.map (fun root ->
                    TypeDefn.GenericInstantiation (root, arguments |> List.choose id |> ImmutableArray.CreateRange)
                )
            else
                None
        | TypeDefn.Array (element, rank) -> go element |> Option.map (fun element -> TypeDefn.Array (element, rank))
        | TypeDefn.OneDimensionalArrayLowerBoundZero element ->
            go element |> Option.map TypeDefn.OneDimensionalArrayLowerBoundZero
        | TypeDefn.Pointer element -> go element |> Option.map TypeDefn.Pointer
        | TypeDefn.Byref element -> go element |> Option.map TypeDefn.Byref
        | TypeDefn.Pinned element -> go element |> Option.map TypeDefn.Pinned
        // A modifier says nothing of what the value is.
        | TypeDefn.Modified modified -> go modified.Unmodified
        | TypeDefn.FunctionPointer _ ->
            if mentionsTypeVariable spelling then
                None
            else
                Some spelling
        | TypeDefn.FromReference _
        | TypeDefn.FromDefinition _
        | TypeDefn.PrimitiveType _
        | TypeDefn.Void -> Some spelling

    /// The arguments a member token of a body of `assembly` supplies for the member's declaring
    /// type's type variables and for a method's own, as that body spells them; empty where it
    /// supplies none.
    let rec private tokenArguments (assembly : DumpedAssembly) (token : MetadataToken) : TypeDefn list * TypeDefn list =
        match token with
        | MetadataToken.MethodSpecification handle ->
            let spec = assembly.MethodSpecs.[handle]
            fst (tokenArguments assembly spec.Method), List.ofSeq spec.Signature
        | MetadataToken.MemberReference handle ->
            match assembly.Members.[handle].Parent with
            | MetadataToken.TypeSpecification parent ->
                match TypeDefn.stripCustomModifiers assembly.TypeSpecs.[parent].Signature with
                | TypeDefn.GenericInstantiation (_, arguments) -> List.ofSeq arguments, []
                | _ -> [], []
            | _ -> [], []
        | _ -> [], []

    /// A type definition as a body spells it, instantiated with its own type parameters, and
    /// whether it is a value type. Deciding that walks the base chain, which loads whatever assembly
    /// it passes through.
    let private spelledDefinition
        (state : EscapeAnalysisState)
        (identity : ResolvedTypeIdentity)
        : EscapeAnalysisState * TypeDefn * bool
        =
        let valueType = state.BaseTypes.ValueType.Identity
        let state, derivesFromValueType = derivesFrom state identity valueType

        let isValueType =
            derivesFromValueType
            && identity <> valueType
            && identity <> state.BaseTypes.Enum.Identity

        let kind =
            if isValueType then
                SignatureTypeKind.ValueType
            else
                SignatureTypeKind.Class

        let _, definition = definitionOf state identity
        let named = TypeDefn.FromDefinition (identity, kind)

        let spelling =
            if definition.Generics.IsEmpty then
                named
            else
                TypeDefn.GenericInstantiation (
                    named,
                    definition.Generics
                    |> Seq.mapi (fun index _ -> TypeDefn.GenericTypeParameter index)
                    |> ImmutableArray.CreateRange
                )

        state, spelling, isValueType

    /// The type a type token of a body of `assembly` names, as that body spells it, with the full
    /// name of the assembly whose metadata spells it; `None` for a token that does not bind, or a
    /// definition or reference naming a generic type, whose type variables would not be the body's.
    let private spelledType
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (token : MetadataToken)
        : EscapeAnalysisState * (TypeDefn * string) option
        =
        let ofDefinition (state : EscapeAnalysisState) (identity : ResolvedTypeIdentity) =
            let _, definition = definitionOf state identity

            if definition.Generics.IsEmpty then
                let state, spelling, _ = spelledDefinition state identity
                state, Some (spelling, identity.AssemblyFullName)
            else
                state, None

        match token with
        | MetadataToken.TypeDefinition _
        | MetadataToken.TypeReference _
        | MetadataToken.TypeSpecification _ ->
            match bindToken state assembly token with
            | state, _, _ :: _, _ -> state, None
            | state, _, [], _ ->
                match token with
                | MetadataToken.TypeSpecification handle ->
                    state, Some (assembly.TypeSpecs.[handle].Signature, assembly.DefinitionFullName)
                | MetadataToken.TypeDefinition handle -> ofDefinition state assembly.TypeDefs.[handle].Identity
                | MetadataToken.TypeReference handle ->
                    match resolveTypeRef state assembly assembly.TypeRefs.[handle] with
                    | state, Some identity -> ofDefinition state identity
                    | state, None -> state, None
                | _ -> state, None
        | _ -> state, None

    /// What a body of `method`, which may be one the VM substitutes for its own, spells for each
    /// source of a value on its evaluation stack, for `StackValues.analyse`. A token at an
    /// instruction whose index is in `unbound` failed to bind, and spells nothing.
    let private bodySpellingsOf
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (method : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        (body : MethodInstructions<TypeDefn>)
        (unbound : Set<int>)
        : EscapeAnalysisState * BodySpellings
        =
        let spellings = System.Collections.Generic.List<SpelledType> ()

        let objectOf (exact : bool) (inBodyContext : bool) (spelling : TypeDefn) (spelledIn : string) : StackValue =
            match TypeDefn.stripCustomModifiers spelling with
            | TypeDefn.Byref _
            | TypeDefn.Pointer _
            | TypeDefn.Pinned _
            | TypeDefn.FunctionPointer _
            | TypeDefn.Void -> StackValue.Unknown
            | _ ->
                spellings.Add
                    {
                        Type = spelling
                        SpelledIn = spelledIn
                        InBodyContext = inBodyContext
                    }

                let index = spellings.Count - 1

                let spelled =
                    if exact then
                        SpelledObject.Exactly index
                    else
                        SpelledObject.Within index

                StackValue.Objects (Set.singleton spelled)

        // A type a signature spells in the body's own assembly, but in the context of the member
        // it belongs to: a type variable in it is not the body's.
        // A type the signature of the member `token` names spells, in that member's context: with
        // the arguments the token supplies it is the body's own; without them it only names a
        // type definition.
        let ofMemberSignature (token : MetadataToken) (spelling : TypeDefn) : StackValue =
            let typeArguments, methodArguments = tokenArguments assembly token

            match substituted typeArguments methodArguments spelling with
            | Some spelling -> objectOf false true spelling assembly.DefinitionFullName
            | None -> objectOf false false spelling assembly.DefinitionFullName

        let ofOwnSpelling (spelling : TypeDefn) : StackValue =
            objectOf false true spelling assembly.DefinitionFullName

        let ofTypeToken (exact : bool) (state : EscapeAnalysisState) (token : MetadataToken) =
            match spelledType state assembly token with
            | state, Some (spelling, spelledIn) ->
                // A nullable type named by `box`, `isinst` or `castclass` stands for the value it
                // holds, boxed, or null (ECMA-335 III.4.1, III.4.3, III.4.6).
                match nominalIdentity state (assemblyOf state spelledIn) spelling with
                | state, Some identity when identity = state.BaseTypes.Nullable.Identity -> state, StackValue.Unknown
                | state, _ -> state, objectOf exact true spelling spelledIn
            | state, None -> state, StackValue.Unknown

        let state, this =
            if method.IsStatic then
                state, []
            else
                let identity = method.RequiredDeclaringType.Identity

                // The declaring type's own type parameters are the body's.
                match spelledDefinition state identity with
                // `this` is a managed pointer to the value.
                | state, _, true -> state, [ StackValue.Unknown ]
                | state, spelling, false -> state, [ objectOf false true spelling identity.AssemblyFullName ]

        // The arguments the body stores to or takes the address of, which then need not hold what
        // the caller passed.
        let written =
            body.Instructions
            |> List.choose (fun (op, _) ->
                match op with
                | IlOp.UnaryConst (UnaryConstIlOp.Starg_s index)
                | IlOp.UnaryConst (UnaryConstIlOp.Ldarga_s index) -> Some (int index)
                | IlOp.UnaryConst (UnaryConstIlOp.Starg index)
                | IlOp.UnaryConst (UnaryConstIlOp.Ldarga index) -> Some (int index)
                | _ -> None
            )
            |> Set.ofList

        // What `ldarg` of the argument at `index` pushes, whose parameter's type is `parameter`
        // (`None` for `this`), and which its spelling alone makes `declared`.
        let passed (index : int) (parameter : TypeDefn option) (declared : StackValue) : StackValue =
            let isByref =
                match parameter |> Option.map TypeDefn.stripCustomModifiers with
                | Some (TypeDefn.Byref _) -> true
                | _ -> false

            match declared with
            | _ when written.Contains index -> declared
            | _ when (parameter |> Option.bind StackInteger.ofParameter).IsSome ->
                StackValue.Integers (Set.singleton (SpelledInteger.Argument index))
            | _ when isByref -> StackValue.Addresses (Set.singleton (SpelledAddress.Argument index))
            | StackValue.Objects objects ->
                match Set.toList objects with
                | [ SpelledObject.Within spelling ] ->
                    StackValue.Objects (Set.singleton (SpelledObject.Argument (index, spelling)))
                | _ -> declared
            | StackValue.Integers _
            | StackValue.Addresses _
            | StackValue.Unknown -> declared

        let arguments =
            let parameters =
                List.map (fun _ -> None) this @ List.map Some method.Signature.ParameterTypes

            this @ (method.Signature.ParameterTypes |> List.map ofOwnSpelling)
            |> List.zip parameters
            |> List.mapi (fun index (parameter, declared) -> passed index parameter declared)

        let locals =
            body.LocalVars
            |> Option.map (Seq.map ofOwnSpelling >> List.ofSeq)
            |> Option.defaultValue []

        let string =
            objectOf true true (TypeDefn.PrimitiveType PrimitiveType.String) state.BaseTypes.Corelib.DefinitionFullName

        let state, tokens =
            ((state, Map.empty), List.indexed body.Instructions)
            ||> List.fold (fun (state, tokens) (index, (op, offset)) ->
                let state, value =
                    match op with
                    | _ when unbound.Contains index -> state, None
                    | IlOp.UnaryMetadataToken ((UnaryMetadataTokenIlOp.Call | UnaryMetadataTokenIlOp.Callvirt),
                                               MetadataOperand.FromMetadata token) ->
                        let rec returned (token : MetadataToken) : MethodReturnType<TypeDefn> option =
                            match token with
                            | MetadataToken.MethodDef handle -> Some assembly.Methods.[handle].Signature.ReturnType
                            | MetadataToken.MemberReference handle ->
                                match assembly.Members.[handle].Signature with
                                | MemberSignature.Method signature -> Some signature.ReturnType
                                | MemberSignature.Field _ -> None
                            | MetadataToken.MethodSpecification handle -> returned assembly.MethodSpecs.[handle].Method
                            | _ -> None

                        match returned token.Token with
                        | Some (MethodReturnType.Returns ty) ->
                            match ofMemberSignature token.Token ty with
                            | StackValue.Objects declared ->
                                let declared =
                                    declared
                                    |> Seq.tryPick (fun object ->
                                        match object with
                                        | SpelledObject.Within index -> Some index
                                        | _ -> None
                                    )

                                state,
                                Some (StackValue.Objects (Set.singleton (SpelledObject.Returned (offset, declared))))
                            // A managed pointer, or something else that is not an object.
                            | StackValue.Integers _
                            | StackValue.Addresses _
                            | StackValue.Unknown -> state, Some StackValue.Unknown
                        | _ -> state, None
                    | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Newobj, MetadataOperand.FromMetadata token) ->
                        let parent =
                            match token.Token with
                            | MetadataToken.MethodDef handle ->
                                Some (
                                    MetadataToken.TypeDefinition
                                        assembly.Methods.[handle].RequiredDeclaringType.Identity.TypeDefinition.Get
                                )
                            | MetadataToken.MemberReference handle -> Some assembly.Members.[handle].Parent
                            | _ -> None

                        match parent with
                        | Some parent ->
                            let state, value = ofTypeToken true state parent
                            state, Some value
                        | None -> state, None
                    | IlOp.UnaryMetadataToken ((UnaryMetadataTokenIlOp.Ldfld | UnaryMetadataTokenIlOp.Ldsfld),
                                               MetadataOperand.FromMetadata token) ->
                        match token.Token with
                        | MetadataToken.FieldDefinition handle ->
                            // What the field's writers store in it, where the analysis can follow them.
                            match ofMemberSignature token.Token assembly.Fields.[handle].Signature with
                            | StackValue.Objects objects ->
                                match Set.toList objects with
                                | [ SpelledObject.Within declared ] ->
                                    let field = FieldKey.make assembly.DefinitionFullName handle

                                    state,
                                    Some (StackValue.Objects (Set.singleton (SpelledObject.Field (field, declared))))
                                | _ -> state, Some (StackValue.Objects objects)
                            | other -> state, Some other
                        | MetadataToken.MemberReference handle ->
                            match assembly.Members.[handle].Signature with
                            | MemberSignature.Field (_, ty) -> state, Some (ofMemberSignature token.Token ty)
                            | MemberSignature.Method _ -> state, None
                        | _ -> state, None
                    | IlOp.UnaryMetadataToken ((UnaryMetadataTokenIlOp.Castclass | UnaryMetadataTokenIlOp.Isinst | UnaryMetadataTokenIlOp.Unbox_Any | UnaryMetadataTokenIlOp.Ldobj | UnaryMetadataTokenIlOp.Ldelem | UnaryMetadataTokenIlOp.Box),
                                               MetadataOperand.FromMetadata token) ->
                        let state, value = ofTypeToken false state token.Token
                        state, Some value
                    | _ -> state, None

                match value with
                | Some value -> state, Map.add offset value tokens
                | None -> state, tokens
            )

        let state, caught =
            ((state, Map.empty), body.ExceptionRegions)
            ||> Seq.fold (fun (state, caught) region ->
                match region with
                | ExceptionRegion.Catch (ExceptionCatchType.FromMetadata token, offsets) ->
                    let state, value = ofTypeToken false state token
                    state, Map.add offsets.HandlerOffset value caught
                | _ -> state, caught
            )

        let binding = GenericBinding.AtDefinition

        let effects =
            StackShape.effectInputs
                {
                    Arguments = ImmutableArray.CreateRange (List.replicate arguments.Length SlotShape.Other)
                    Locals = ImmutableArray.CreateRange (List.replicate locals.Length SlotShape.Other)
                    ReturnsValue =
                        (StackShapeTokens.returnShape
                            assembly
                            binding
                            GenericSubstitution.None
                            method.Signature.ReturnType)
                            .IsSome
                    Tokens = StackShapeTokens.ofBody assembly binding (StackFlow.reachable body) body
                }

        state,
        {
            Spellings = List.ofSeq spellings
            Arguments = arguments
            Locals = locals
            LocalIntegers =
                body.LocalVars
                |> Option.map (
                    Seq.indexed
                    >> Seq.choose (fun (index, local) -> StackInteger.ofLocal local |> Option.map (fun p -> index, p))
                    >> Map.ofSeq
                )
                |> Option.defaultValue Map.empty
            String = string
            Tokens = tokens
            Caught = caught
            Effects = effects
        }

    /// Whether the value a `brtrue` or `brfalse` tests (`tested`) is known to be neither null nor
    /// zero (`Some true`), or known to be one of those (`Some false`), given what the body is passed
    /// as its arguments.
    let private testIsKnown (passed : Map<int, PassedValue>) (tested : Tested) : bool option =
        let isObject (object : SpelledObject) : bool option list =
            match object with
            | SpelledObject.Null -> [ Some false ]
            | SpelledObject.Exactly _ -> [ Some true ]
            | SpelledObject.Argument (index, _) ->
                match passed.TryFind index with
                | Some (PassedValue.Objects passed) when not passed.MayBeNull -> [ Some true ]
                | Some (PassedValue.Objects passed) when passed.Classes.IsEmpty -> [ Some false ]
                | Some (PassedValue.Objects _)
                | None -> [ None ]
                | Some (PassedValue.Integers _)
                | Some (PassedValue.Pointer _) ->
                    failwith
                        $"BUG: something other than objects was passed as argument %d{index}, which is spelled as one"
            | SpelledObject.Within _
            | SpelledObject.Returned _
            | SpelledObject.Field _ -> [ None ]

        let isNonZero (integer : SpelledInteger) : bool option list =
            match integer with
            | SpelledInteger.Constant value -> [ Some (not (StackInteger.isZero value)) ]
            | SpelledInteger.Argument index ->
                match passed.TryFind index with
                | Some (PassedValue.Integers values) ->
                    values |> Seq.map (StackInteger.isZero >> not >> Some) |> List.ofSeq
                | None -> [ None ]
                | Some (PassedValue.Objects _)
                | Some (PassedValue.Pointer _) ->
                    failwith $"BUG: something other than integers was passed as argument %d{index}, an integer"
            | SpelledInteger.Loaded (index, loaded) ->
                match passed.TryFind index with
                | Some (PassedValue.Pointer (location, holds)) ->
                    holds
                    |> Seq.map (fun held ->
                        StackInteger.loadedAs location loaded held
                        |> Option.map (StackInteger.isZero >> not)
                    )
                    |> List.ofSeq
                | None -> [ None ]
                | Some (PassedValue.Objects _)
                | Some (PassedValue.Integers _) ->
                    failwith $"BUG: something other than a pointer was passed as argument %d{index}, a byref"

        let known =
            match tested with
            | Tested.References objects -> objects |> Seq.collect isObject
            | Tested.Integers integers -> integers |> Seq.collect isNonZero
            // Only the instance's type arguments decide it (`typeQueryIsKnown`).
            | Tested.Types _ -> Seq.singleton None

        match known |> Seq.distinct |> List.ofSeq with
        | [ known ] -> known
        | _ -> None

    /// Whether `spelling` is of a reference type, whatever the instantiation: `string`, `object`,
    /// an array, or a class. A type variable may be a value type.
    let private isReference (state : EscapeAnalysisState) (spelling : SpelledType) : EscapeAnalysisState * bool =
        match TypeDefn.stripCustomModifiers spelling.Type with
        | TypeDefn.PrimitiveType PrimitiveType.String
        | TypeDefn.PrimitiveType PrimitiveType.Object
        | TypeDefn.Array _
        | TypeDefn.OneDimensionalArrayLowerBoundZero _ -> state, true
        | TypeDefn.FromDefinition _
        | TypeDefn.FromReference _
        | TypeDefn.GenericInstantiation _ ->
            match nominalIdentity state (assemblyOf state spelling.SpelledIn) spelling.Type with
            | state, Some identity ->
                let valueType = state.BaseTypes.ValueType.Identity
                let state, derivesFromValueType = derivesFrom state identity valueType

                state,
                not derivesFromValueType
                || identity = valueType
                || identity = state.BaseTypes.Enum.Identity
            | state, None -> state, false
        | _ -> state, false

    /// Where `assembly`'s own IL writes its fields.
    let private fieldWrites
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        : EscapeAnalysisState * FieldWrites
        =
        match state.FieldWrites.TryFind assembly.DefinitionFullName with
        | Some writes -> state, writes
        | None ->
            let rowOf (handle : FieldDefinitionHandle) : int =
                (FieldKey.make assembly.DefinitionFullName handle).Row

            // A member reference naming a field may name one of the assembly's own, of a generic
            // type's instantiation, say; which, the analysis does not resolve.
            let referenced =
                assembly.Members.Values
                |> Seq.choose (fun reference ->
                    match reference.Signature with
                    | MemberSignature.Field _ -> Some reference.PrettyName
                    | MemberSignature.Method _ -> None
                )
                |> Set.ofSeq

            let named =
                assembly.Fields
                |> Seq.filter (fun (KeyValue (_, field)) -> referenced.Contains field.Name)
                |> Seq.map (fun (KeyValue (handle, _)) -> rowOf handle)
                |> Set.ofSeq

            let stores, addressed =
                ((Map.empty, Set.empty), assembly.Methods)
                ||> Seq.fold (fun (stores, addressed) (KeyValue (handle, method)) ->
                    match MethodInfo.tryIlBody method with
                    | None -> stores, addressed
                    | Some body ->
                        ((stores, addressed), body.Instructions)
                        ||> List.fold (fun (stores, addressed) (op, offset) ->
                            match op with
                            | IlOp.UnaryMetadataToken ((UnaryMetadataTokenIlOp.Stfld | UnaryMetadataTokenIlOp.Stsfld),
                                                       MetadataOperand.FromMetadata token) ->
                                match token.Token with
                                | MetadataToken.FieldDefinition field ->
                                    let row = rowOf field

                                    Map.add
                                        row
                                        ((handle, offset) :: Option.defaultValue [] (Map.tryFind row stores))
                                        stores,
                                    addressed
                                | _ -> stores, addressed
                            | IlOp.UnaryMetadataToken ((UnaryMetadataTokenIlOp.Ldflda | UnaryMetadataTokenIlOp.Ldsflda),
                                                       MetadataOperand.FromMetadata token) ->
                                match token.Token with
                                | MetadataToken.FieldDefinition field -> stores, Set.add (rowOf field) addressed
                                | _ -> stores, addressed
                            | _ -> stores, addressed
                        )
                )

            let writes =
                {
                    Stores = stores
                    Unfollowed = Set.union named addressed
                }

            { state with
                FieldWrites = state.FieldWrites.Add (assembly.DefinitionFullName, writes)
            },
            writes

    /// Whether `Assumption.FieldsWrittenVisibly`, where the analysis may take it, says what `field`
    /// holds: what its own assembly's `stfld`s and `stsfld`s store in it, or null. So it is when
    /// no other assembly can name the field, it being private, or internal to an assembly that
    /// makes no other its friend (`InternalsVisibleTo`); its own assembly's IL never takes its
    /// address; no member reference of that assembly may name it; its type is a class of automatic
    /// layout: not explicit, where another field can lie over it (a struct's may lie under a field
    /// of a container of explicit layout that embeds the struct), and not sequential, which the
    /// interop marshaller writes as a formatted class (`Marshal.PtrToStructure`, a P/Invoke's `[Out]`
    /// parameter); and, in CoreLib, CoreCLR's native code does not bind it by name (`VmBoundField`),
    /// which it may write, nor is it an instance field of a class whose objects that code lays out
    /// as a C++ class of its own (`VmLaidOutClass`), through which it may write any of them.
    let private followableField (state : EscapeAnalysisState) (field : FieldKey) : EscapeAnalysisState * bool =
        if not (state.Assumptions.Contains Assumption.FieldsWrittenVisibly) then
            state, false
        else

        let assembly = assemblyOf state field.AssemblyFullName
        let definition = assembly.Fields.[field.Handle]

        let declaring =
            assembly.TypeDefs.[definition.DeclaringType.Identity.TypeDefinition.Get]

        let closedToOthers =
            match definition.Attributes &&& FieldAttributes.FieldAccessMask with
            | FieldAttributes.Private -> true
            | FieldAttributes.Assembly
            | FieldAttributes.FamANDAssem ->
                match assembly.Friends with
                | Ok friends -> friends.InternalsVisibleTo.Length = 0
                | Error _ -> false
            | _ -> false

        let inCorelib =
            assembly.DefinitionFullName = state.BaseTypes.Corelib.DefinitionFullName

        let declaringFullName =
            TypeInfo.fullName (fun handle -> assembly.TypeDefs.[handle]) declaring

        let boundByTheVm =
            inCorelib && VmBoundField.isBound declaringFullName definition.Name

        // A static field is part of no object, so no C++ layout reaches it.
        let laidOutByTheVm =
            inCorelib
            && not (definition.Attributes.HasFlag FieldAttributes.Static)
            && VmLaidOutClass.isLaidOut declaringFullName

        if
            not closedToOthers
            || boundByTheVm
            || laidOutByTheVm
            || definition.Attributes.HasFlag FieldAttributes.Literal
            || TypeLayoutKind.ofTypeAttributes declaring.TypeAttributes <> TypeLayoutKind.Auto
        then
            state, false
        else

        let state, ofValueType =
            derivesFrom state definition.DeclaringType.Identity state.BaseTypes.ValueType.Identity

        if ofValueType then
            state, false
        else

        let state, writes = fieldWrites state assembly
        state, not (writes.Unfollowed.Contains field.Row)

    /// Whether the analysis takes `field` to hold only what its assembly's IL stores in it, null among
    /// that (`followableField`), and the analysis to ask next.
    let internal followsField (state : EscapeAnalysisState) (field : FieldKey) : EscapeAnalysisState * bool =
        followableField state field

    /// Whether every object among `objects` is spelled, in `spellings`, as of a reference type, so
    /// that whether it is null is what a `brtrue` tests of it. An object a call returns that names
    /// no declared type is not.
    let private allReferences
        (state : EscapeAnalysisState)
        (spellings : SpelledType[])
        (objects : SpelledObject list)
        : EscapeAnalysisState * bool
        =
        ((state, true), objects)
        ||> List.fold (fun (state, soFar) object ->
            if not soFar then
                state, false
            else
                match object with
                | SpelledObject.Null -> state, true
                | SpelledObject.Exactly index
                | SpelledObject.Within index
                | SpelledObject.Argument (_, index)
                | SpelledObject.Field (_, index)
                | SpelledObject.Returned (_, Some index) -> isReference state spellings.[index]
                | SpelledObject.Returned (_, None) -> state, false
        )

    /// The objects the `ret`s among `sites` that a run executes return, over all of them; `Unknown`
    /// where one returns anything else.
    let private returnedOver (sites : Map<int, StackValue>) (executed : Set<int>) : StackValue =
        ((Some Set.empty), sites)
        ||> Map.fold (fun returned offset value ->
            match returned, value with
            | _ when not (executed.Contains offset) -> returned
            | Some returned, StackValue.Objects objects -> Some (Set.union returned objects)
            | _, StackValue.Integers _
            | _, StackValue.Addresses _
            | _, StackValue.Unknown
            | None, _ -> None
        )
        |> Option.map StackValue.Objects
        |> Option.defaultValue StackValue.Unknown

    /// The facts of a method whose body never runs: only `outside` happens, as the runtime enters it.
    let private outsideOnly (outside : Set<OutsideBodyFact>) : LocalFacts =
        {
            Raises = []
            Opaque = []
            Assumed = []
            Relies = []
            Calls = []
            ConstrainedPointers = []
            Rethrows = []
            Regions = []
            InstanceChecks = []
            OutsideBody = outside
            RaisedWhereInlined = Set.empty
            Spellings = [||]
            Returns = StackValue.Unknown
            Stored = Map.empty
            Consults = Set.empty
            LookedUp = []
            HelperSites = []
            ReceiverBoxes = []
            Refusals = []
            Branching = None
        }

    let private opaqueFromEntry (reason : Opacity) : LocalFacts =
        outsideOnly (Set.singleton (OutsideBodyFact.Opaque reason))

    /// What CoreCLR raises refusing to hand the JIT a signature (`SignatureRefusal`), and what making
    /// the exception runs (`ExceptionMaking.ofSignatureRefusal`).
    let private signatureRefusalFacts
        (state : EscapeAnalysisState)
        (refusal : SignatureRefusal)
        : OutsideBodyFact list
        =
        let name, making = ExceptionMaking.ofSignatureRefusal refusal
        let thrown, run = runtimeRaises state name making

        OutsideBodyFact.Raises thrown
        :: List.map (fun made -> OutsideBodyFact.Runs (made, [])) run

    /// Why CoreCLR fails the compilation of a method at an instruction the JIT compiles, before any
    /// of the method runs.
    [<RequireQualifiedAccess>]
    type private CompilationRefusal =
        /// It refuses to hand the JIT the signature the instruction names (`SignatureRefusal`).
        | Signature of SignatureRefusal
        /// It refuses the unmanaged calling convention of the call, whose signature's modifiers name
        /// two base conventions (`UnmanagedConventionRead.MultipleBases`).
        | MultipleCallingConventions
        /// It cannot read the unmanaged calling convention of the call from the P/Invoke's
        /// `[UnmanagedCallConv]` (`UnmanagedConventionRead.Unreadable`, `NullArray`), and raises
        /// what the analysis does not name, or kills the process.
        | UnreadableCallingConvention
        /// The JIT refuses the call, which passes no argument by Thiscall's convention
        /// (`UnmanagedCallingConvention.refusesThiscallWithoutArguments`).
        | ThiscallWithoutArguments

    /// What the refusal of a compilation raises, and what making the exception runs.
    let private compilationRefusalFacts
        (state : EscapeAnalysisState)
        (refusal : CompilationRefusal)
        : OutsideBodyFact list
        =
        let raising (name : string, making : ExceptionMaking) : OutsideBodyFact list =
            let thrown, run = runtimeRaises state name making

            OutsideBodyFact.Raises thrown
            :: List.map (fun made -> OutsideBodyFact.Runs (made, [])) run

        match refusal with
        | CompilationRefusal.Signature refusal -> signatureRefusalFacts state refusal
        | CompilationRefusal.MultipleCallingConventions -> raising ExceptionMaking.multipleCallingConventions
        | CompilationRefusal.ThiscallWithoutArguments -> raising ExceptionMaking.thiscallWithoutArguments
        | CompilationRefusal.UnreadableCallingConvention ->
            [ OutsideBodyFact.Opaque Opacity.UnreadableCallingConvention ]

    /// What CoreCLR decides of a method before its JIT runs, where the method is
    /// `[UnmanagedCallersOnly]` (`ThrowIfInvalidUnmanagedCallersOnlyUsage`, from `GetCompileFlags`).
    [<RequireQualifiedAccess>]
    type private CallersOnlyValidation =
        /// The JIT runs, and these come first: where a value type's marshalling verdict decides,
        /// the opacity of that verdict.
        | Compiles of Set<OutsideBodyFact>
        /// CoreCLR refuses to compile the method, raising this, before any of it runs and before
        /// the JIT loads the types of its parameters and locals.
        | Refused of Set<OutsideBodyFact>

    /// What CoreCLR decides of `method`, of `assembly`, before the JIT runs: nothing where it is
    /// not `[UnmanagedCallersOnly]`, nor where loading its return type fails, which comes first
    /// (`getMethodInfo`). Otherwise it refuses one that is not static, or is generic or declared in
    /// a generic type, or that needs marshalling (`PInvoke::MarshalingRequired`): one carrying
    /// `[LCIDConversion]`, or whose return type or a parameter does, walked in that order, loading
    /// in full each value type and instantiation it meets, so that one failing to load raises there
    /// instead. A value type's verdict, the analysis does not model (`ValueTypeMarshallingVerdict`).
    let private callersOnlyValidation
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (method : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        : EscapeAnalysisState * CallersOnlyValidation
        =
        // CoreCLR's refusal: the exception, and what making it runs.
        let refusal (state : EscapeAnalysisState) : Set<OutsideBodyFact> =
            let name, making = ExceptionMaking.unmanagedCallersOnlyInvalid
            let thrown, run = runtimeRaises state name making

            OutsideBodyFact.Raises thrown
            :: List.map (fun made -> OutsideBodyFact.Runs (made, [])) run
            |> Set.ofList

        // A type spelled over a type variable is left unchecked: CoreCLR refuses a generic
        // `[UnmanagedCallersOnly]` method, or one of a generic type, once its return type loads.
        let loadFails (state : EscapeAnalysisState) (spelling : TypeDefn) =
            match jitFullLoad state assembly spelling with
            | state, None, _ -> state, None
            | state, Some failure, _ -> state, Some (Set.ofList (bindFailureFacts state failure))

        let returned =
            match method.Signature.ReturnType with
            | MethodReturnType.Void -> TypeDefn.Void
            | MethodReturnType.Returns returned -> returned

        let returnLoadFails (state : EscapeAnalysisState) =
            match JitTypeLoads.load JitTypePosition.OwnReturn returned with
            | None -> state, false
            | Some (JitTypeLoad.InFull loaded) ->
                let state, failure = loadFails state loaded
                state, failure.IsSome
            | Some load ->
                failwith
                    $"BUG: JitTypeLoads.load answers %O{load} for a method's return type, which loads in full or not at all"

        if not (MethodInfo.isUnmanagedCallersOnly method) then
            state, CallersOnlyValidation.Compiles Set.empty
        else

        match returnLoadFails state with
        | state, true -> state, CallersOnlyValidation.Compiles Set.empty
        | state, false ->

        let handle =
            match method with
            | MethodInfo.Metadata (_, facts) -> facts.Handle
            | MethodInfo.Synthesised _ ->
                failwith "BUG: a synthesised method carries [UnmanagedCallersOnly], which only metadata can apply"

        if
            not method.IsStatic
            || method.Signature.GenericParameterCount <> 0
            || not method.RequiredDeclaringType.Generics.IsEmpty
            || UnmanagedCallersOnly.lcidParameterIndex (
                NamedAttribute.ofMethod "System.Runtime.InteropServices.LCIDConversionAttribute" assembly handle
               )
               <> -1
        then
            state, CallersOnlyValidation.Refused (refusal state)
        else

        let runtimeMarshalling =
            (NamedAttribute.ofAssembly "System.Runtime.CompilerServices.DisableRuntimeMarshallingAttribute" assembly)
                .IsEmpty

        let marshalling = MethodInfo.parameterMarshalling method

        // Where a sequence number names no Param row, or several, CoreCLR reads the parameter as
        // having no `[MarshalAs]`, or refuses the signature (`CollateParamTokens`), which is not
        // modelled.
        let marshalOf (sequence : int) : ParameterMarshal =
            marshalling
            |> List.tryFind (fun (row, _) -> row = sequence)
            |> Option.map snd
            |> Option.defaultValue ParameterMarshal.Absent

        let positions = returned :: method.Signature.ParameterTypes |> List.indexed

        // A value type's verdict before the position that refuses the method may refuse it first,
        // so its opacity stays with the refusal.
        let rec walk
            (state : EscapeAnalysisState)
            (opaque : Set<OutsideBodyFact>)
            (positions : (int * TypeDefn) list)
            : EscapeAnalysisState * CallersOnlyValidation
            =
            let refusedWith (state : EscapeAnalysisState) (refusal : Set<OutsideBodyFact>) =
                state, CallersOnlyValidation.Refused (Set.union opaque refusal)

            match positions with
            | [] -> state, CallersOnlyValidation.Compiles opaque
            | (sequence, spelling) :: rest ->

            let marshalAs =
                UnmanagedCallersOnly.marshalAsRequires runtimeMarshalling (marshalOf sequence)

            match UnmanagedCallersOnly.signatureTypeMarshalling assembly runtimeMarshalling spelling with
            | SignatureTypeMarshalling.Required -> refusedWith state (refusal state)
            | SignatureTypeMarshalling.NotRequired ->
                if marshalAs then
                    refusedWith state (refusal state)
                else
                    walk state opaque rest
            | SignatureTypeMarshalling.RequiredOnceLoaded loaded ->
                match loadFails state loaded with
                | state, Some failures -> refusedWith state failures
                | state, None -> refusedWith state (refusal state)
            | SignatureTypeMarshalling.DecidedByLayoutOnceLoaded loaded ->
                match loadFails state loaded with
                | state, Some failures -> refusedWith state failures
                // Whichever its verdict, a `[MarshalAs]` after it needs marshalling.
                | state, None when marshalAs -> refusedWith state (refusal state)
                | state, None ->
                    walk state (Set.add (OutsideBodyFact.Opaque Opacity.ValueTypeMarshallingVerdict) opaque) rest

        walk state Set.empty positions

    /// The exceptions an operation of the runtime's own can raise (`runtimeRaises`). Its contract
    /// says under which conditions on its arguments; the analysis does not track their values, so
    /// each may be raised.
    let private contractRaises
        (state : EscapeAnalysisState)
        (contract : IntrinsicContract)
        : (ThrownType * MethodKey list) list
        =
        contract.Raises
        |> List.map (fun (fault, _) ->
            let fullName =
                match fault with
                | PrimitiveFault.NullReference -> "System.NullReferenceException"
                | PrimitiveFault.DataMisaligned -> "System.DataMisalignedException"

            runtimeRaises state fullName (ExceptionMaking.ofPrimitiveFault fault)
        )

    /// What a fault the CPU reports raises, as the VM raises it (`runtimeRaises`); `None` for an
    /// out-of-range immediate, which the JIT throws by calling a helper in CoreLib.
    let private instructionRaises
        (state : EscapeAnalysisState)
        (fault : InstructionFault)
        : (ThrownType * MethodKey list) option
        =
        let made (fullName : string) =
            ExceptionMaking.ofInstructionFault fault
            |> Option.map (runtimeRaises state fullName)

        match fault with
        | InstructionFault.NullAddress -> made "System.NullReferenceException"
        | InstructionFault.ImmediateOutOfRange -> None
        | InstructionFault.ZeroDivisor -> made "System.DivideByZeroException"
        | InstructionFault.QuotientOverflow -> made "System.OverflowException"

    /// The `CastHelpers` method that `jithelpers.h` binds `helper`'s JIT helper to. It binds the
    /// array cast helpers to the `Any` ones.
    let private jitHelperMethod (state : EscapeAnalysisState) (helper : JitHelper) : MethodKey =
        let corelib = state.BaseTypes.Corelib

        let name, parameters =
            match helper with
            | JitHelper.Cast (throwing, target) ->
                let operation = if throwing then "ChkCast" else "IsInstanceOf"

                match target with
                | CastTarget.Class -> operation + "Class", 2
                | CastTarget.Interface -> operation + "Interface", 2
                | CastTarget.Any -> operation + "Any", 2
            | JitHelper.Unbox -> "Unbox", 2
            | JitHelper.UnboxNullable -> "Unbox_Nullable", 3
            | JitHelper.Box -> "Box", 2
            | JitHelper.BoxNullable -> "Box_Nullable", 2
            | JitHelper.LdelemaRef -> "LdelemaRef", 3
            | JitHelper.StelemRef -> "StelemRef", 3

        let found =
            corelib.TryGetTopLevelTypeDef "System.Runtime.CompilerServices" "CastHelpers"
            |> Option.toList
            |> List.collect (fun ty -> List.ofSeq ty.Methods)
            |> List.filter (fun m -> m.Name = name && m.IsStatic && m.Signature.ParameterTypes.Length = parameters)
            |> List.choose (fun m -> m.TryMetadata)

        match found with
        | [ facts ] -> MethodKey.make corelib facts.Handle
        | found ->
            failwith
                $"EscapeAnalysis: expected %s{corelib.DefinitionFullName} to define one CastHelpers.%s{name} of %d{parameters} parameters, found %d{found.Length}"

    /// Every `CastHelpers` method an `instruction` may call, whatever its type.
    let private candidateHelpers (instruction : HelpedInstruction) : JitHelper list =
        let casts (throwing : bool) : JitHelper list =
            [ CastTarget.Class ; CastTarget.Interface ; CastTarget.Any ]
            |> List.map (fun target -> JitHelper.Cast (throwing, target))

        match instruction with
        | HelpedInstruction.Isinst -> casts false
        | HelpedInstruction.Castclass -> casts true
        | HelpedInstruction.UnboxAny -> casts true @ [ JitHelper.Unbox ; JitHelper.UnboxNullable ]
        | HelpedInstruction.Unbox -> [ JitHelper.Unbox ; JitHelper.UnboxNullable ]
        | HelpedInstruction.Box -> [ JitHelper.Box ; JitHelper.BoxNullable ]
        | HelpedInstruction.Ldelema -> [ JitHelper.LdelemaRef ]
        | HelpedInstruction.Stelem -> [ JitHelper.StelemRef ]

    /// Whether the JIT boxes a value type of this definition without calling `CastHelpers.Box`
    /// (`impImportAndPushBox`, importer.cpp): a primitive type or an enum, which it boxes inline, or
    /// a byref-like type, which it boxes only where it folds the box away and otherwise refuses.
    let private boxedWithoutHelper
        (state : EscapeAnalysisState)
        (identity : ResolvedTypeIdentity)
        : EscapeAnalysisState * bool
        =
        let primitive =
            [
                PrimitiveType.Boolean
                PrimitiveType.Char
                PrimitiveType.SByte
                PrimitiveType.Byte
                PrimitiveType.Int16
                PrimitiveType.UInt16
                PrimitiveType.Int32
                PrimitiveType.UInt32
                PrimitiveType.Int64
                PrimitiveType.UInt64
                PrimitiveType.Single
                PrimitiveType.Double
                PrimitiveType.IntPtr
                PrimitiveType.UIntPtr
            ]
            |> List.exists (fun primitive -> (BaseClassTypes.ofPrimitive state.BaseTypes primitive).Identity = identity)

        let _, definition = definitionOf state identity

        if primitive || definition.HasIsByRefLikeAttribute then
            state, true
        else
            let enum = state.BaseTypes.Enum.Identity
            let state, derivesFromEnum = derivesFrom state identity enum
            state, derivesFromEnum && identity <> enum

    /// The `CastHelpers` methods that `instruction` of a type the body in `assembly` spells
    /// (`spelling`) calls, over every instance of the body, as `getCastingHelperStatic`,
    /// `getUnBoxHelper` and `impImportAndPushBox` decide them; and whether only an instance decides
    /// which (`instanceHelper`). Only on Unix: CoreCLR casts to a type with type equivalence by the
    /// `Any` helper, but only Windows builds have type equivalence (`FEATURE_TYPEEQUIVALENCE`).
    let rec private helpersAt
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (instruction : HelpedInstruction)
        (spelling : TypeDefn)
        : EscapeAnalysisState * Set<JitHelper> * bool
        =
        let cast (target : CastTarget) : JitHelper =
            JitHelper.Cast (instruction <> HelpedInstruction.Isinst, target)

        let unbox (nullable : bool) : JitHelper =
            if nullable then
                JitHelper.UnboxNullable
            else
                JitHelper.Unbox

        // A definition's, as the JIT reads its method table: for a cast, a type with variant type
        // parameters, an interface, a `Nullable<T>`, or a class or value type.
        let ofDefinition
            (state : EscapeAnalysisState)
            (identity : ResolvedTypeIdentity)
            (nullableOverVariable : bool)
            : EscapeAnalysisState * Set<JitHelper> * bool
            =
            let _, definition = definitionOf state identity
            // Whether it is a value type turns on its base chain, which may be in an assembly not
            // loaded yet.
            let state, _, valueType = spelledDefinition state identity
            let nullable = identity = state.BaseTypes.Nullable.Identity

            match instruction with
            | HelpedInstruction.Unbox -> state, Set.singleton (unbox nullable), false
            | HelpedInstruction.UnboxAny when valueType -> state, Set.singleton (unbox nullable), false
            | HelpedInstruction.Box ->
                if not valueType then
                    state, Set.empty, false
                elif nullable then
                    state, Set.singleton JitHelper.BoxNullable, false
                else
                    match boxedWithoutHelper state identity with
                    | state, true -> state, Set.empty, false
                    | state, false -> state, Set.singleton JitHelper.Box, false
            | HelpedInstruction.Ldelema ->
                if valueType then
                    state, Set.empty, false
                else
                    state, Set.singleton JitHelper.LdelemaRef, false
            | HelpedInstruction.Stelem ->
                if valueType then
                    state, Set.empty, false
                else
                    state, Set.singleton JitHelper.StelemRef, false
            | HelpedInstruction.Isinst
            | HelpedInstruction.Castclass
            | HelpedInstruction.UnboxAny ->
                if
                    definition.Generics
                    |> Seq.exists (fun (_, metadata) -> metadata.Variance.IsSome)
                then
                    state, Set.singleton (cast CastTarget.Any), false
                elif definition.IsInterface then
                    state, Set.singleton (cast CastTarget.Interface), false
                elif nullable && nullableOverVariable then
                    // `impCastClassOrIsInstToTree` asks about a `Nullable<T>` as `T`, a class to
                    // cast to, but only where the code is not shared over it.
                    state, Set.ofList [ cast CastTarget.Class ; cast CastTarget.Any ], true
                else
                    // A `Nullable<T>` of a type that names no type variable is asked about as `T`.
                    state, Set.singleton (cast CastTarget.Class), false

        // A type the analysis cannot bind fails the instruction's own binding, before it calls
        // anything, and so does every instruction it could be.
        let unbound : Set<JitHelper> = Set.ofList (candidateHelpers instruction)

        let ofRoot (state : EscapeAnalysisState) (root : TypeDefn) (nullableOverVariable : bool) =
            match root with
            | TypeDefn.FromDefinition (identity, _) -> ofDefinition state identity nullableOverVariable
            | TypeDefn.FromReference (typeRef, _) ->
                match resolveTypeRef state assembly typeRef with
                | state, Some identity -> ofDefinition state identity nullableOverVariable
                | state, None -> state, unbound, false
            | other -> failwith $"BUG: a %O{instruction} of %O{other} as a definition"

        match spelling with
        | TypeDefn.PrimitiveType primitive ->
            ofDefinition state (BaseClassTypes.ofPrimitive state.BaseTypes primitive).Identity false
        | TypeDefn.FromDefinition _
        | TypeDefn.FromReference _ -> ofRoot state spelling false
        | TypeDefn.GenericInstantiation (root, arguments) ->
            ofRoot state root (Seq.exists mentionsTypeVariable arguments)
        // Shared code names `__Canon`, a reference type; code for a value type names it, which a
        // cast takes to be a class, and which may be a `Nullable<T>`.
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _ ->
            let helpers =
                match instruction with
                | HelpedInstruction.Isinst
                | HelpedInstruction.Castclass -> [ cast CastTarget.Any ; cast CastTarget.Class ]
                | HelpedInstruction.UnboxAny -> [ cast CastTarget.Any ; JitHelper.Unbox ; JitHelper.UnboxNullable ]
                | HelpedInstruction.Unbox -> [ JitHelper.Unbox ; JitHelper.UnboxNullable ]
                | HelpedInstruction.Box -> [ JitHelper.Box ; JitHelper.BoxNullable ]
                | HelpedInstruction.Ldelema -> [ JitHelper.LdelemaRef ]
                | HelpedInstruction.Stelem -> [ JitHelper.StelemRef ]

            state, Set.ofList helpers, true
        | TypeDefn.Modified modified -> helpersAt state assembly instruction modified.Unmodified
        // An array is a reference type, which a cast casts to by the `Any` helper.
        | TypeDefn.Array _
        | TypeDefn.OneDimensionalArrayLowerBoundZero _ ->
            match instruction with
            | HelpedInstruction.Isinst
            | HelpedInstruction.Castclass
            | HelpedInstruction.UnboxAny -> state, Set.singleton (cast CastTarget.Any), false
            | HelpedInstruction.Unbox -> state, Set.singleton JitHelper.Unbox, false
            | HelpedInstruction.Box -> state, Set.empty, false
            | HelpedInstruction.Ldelema -> state, Set.singleton JitHelper.LdelemaRef, false
            | HelpedInstruction.Stelem -> state, Set.singleton JitHelper.StelemRef, false
        // A type that is no class, which the JIT takes an element of to be an integer.
        | TypeDefn.Pointer _
        | TypeDefn.Byref _
        | TypeDefn.Pinned _
        | TypeDefn.FunctionPointer _
        | TypeDefn.Void ->
            match instruction with
            | HelpedInstruction.Isinst
            | HelpedInstruction.Castclass
            | HelpedInstruction.UnboxAny -> state, Set.singleton (cast CastTarget.Any), false
            | HelpedInstruction.Unbox -> state, Set.singleton JitHelper.Unbox, false
            | HelpedInstruction.Box
            | HelpedInstruction.Ldelema
            | HelpedInstruction.Stelem -> state, Set.empty, false

    /// What CoreCLR runs when `key` is called, or the contract of one of `assumptions` that stands
    /// in for it. The VM's substitute for an intrinsic runs whatever IL CoreLib ships in its place,
    /// working or not.
    let private runsFor (assumptions : Set<Assumption>) (assembly : DumpedAssembly) (key : MethodKey) : Runs =
        let method = assembly.Methods.[key.Method.Get]

        let intrinsic =
            if IntrinsicBody.isIntrinsic assembly key.Method.Get then
                Some (IntrinsicBody.classify assembly key.Method.Get)
            else
                None

        let substituted =
            match intrinsic with
            | Some _ -> VmSubstitution.unsafeStub assembly key.Method.Get
            | None -> None

        match Assumption.summarises assembly key.Method.Get with
        | Some assumption when assumptions.Contains assumption -> Runs.Assumed assumption
        | _ when ResourceLookup.isLookup assembly key.Method.Get -> Runs.ResourceLookup
        | _ when ObjectGetType.isGetType assembly key.Method.Get -> Runs.ObjectGetType
        | _ ->

        match substituted, intrinsic with
        | Some stub, _ -> Runs.Il (stub, None)
        | None, Some IntrinsicBody.VmSubstitution ->
            // A substitute the VM chooses by instantiation, which may be one of the runtime's own
            // operations.
            match IntrinsicPrimitive.recognise assembly key.Method.Get with
            | Some primitive -> Runs.Primitive primitive
            | None -> Runs.Opaque Opacity.IntrinsicExpansion
        // The JIT replaces every call with code of its own, which no IL here describes.
        | None, Some IntrinsicBody.CallSiteExpansion -> Runs.Opaque Opacity.IntrinsicExpansion
        // Whether the VM replaces the body turns on whether the instantiation's type argument is
        // blittable, which the analysis does not decide.
        | None, Some (IntrinsicBody.StructMarshalling _) -> Runs.Opaque Opacity.IntrinsicExpansion
        | None, Some (IntrinsicBody.JitExpansion expansion) ->
            match method.Body with
            | MethodBody.Il body -> Runs.Il (body, Some expansion)
            | _ ->
                failwith
                    $"%O{key}: IntrinsicBody classifies it as a JIT expansion, whose IL calls itself, but it has no IL"
        | None, _ ->
            match method.Body with
            | MethodBody.Abstract -> Runs.Opaque Opacity.AbstractBody
            | MethodBody.InternalCall
            | MethodBody.PInvoke
            | MethodBody.RuntimeProvided _ ->
                match NativeMethod.recognise assembly key.Method.Get with
                | Some native when
                    (NativeMethod.contract native).Assumes
                    |> List.forall (Assumption.ofNative >> assumptions.Contains)
                    ->
                    Runs.Native native
                // Its contract holds under a claim the caller does not allow.
                | Some _
                | None -> Runs.Opaque Opacity.NativeBody
            | MethodBody.Il body -> Runs.Il (body, None)

    /// What one body does by itself.
    /// The facts of `key`'s body, where CoreCLR runs `runs` for it (`runsFor`).
    let private factsOfRuns
        (state : EscapeAnalysisState)
        (key : MethodKey)
        (runs : Runs)
        : EscapeAnalysisState * LocalFacts
        =
        let assembly, method = methodOf state key

        let contracted
            (raised : ThrownType list)
            (assumed : Assumption list)
            (runs : (MethodKey * SpelledArgument list) list)
            : LocalFacts
            =
            {
                Raises = raised |> List.map (fun thrown -> 0, thrown)
                Opaque = []
                Assumed = assumed |> List.map (fun assumption -> 0, assumption)
                Relies = []
                Calls = runs |> List.map (fun (method, passes) -> 0, runtimeRuns method, passes)
                ConstrainedPointers = []
                Rethrows = []
                Regions = []
                InstanceChecks = []
                OutsideBody = Set.empty
                RaisedWhereInlined = Set.empty
                Spellings = [||]
                Returns = StackValue.Unknown
                Stored = Map.empty
                Consults = Set.empty
                LookedUp = []
                HelperSites = []
                ReceiverBoxes = []
                Refusals = []
                Branching = None
            }

        let exactly (names : ExceptionName list) : ThrownType list =
            names
            |> List.map (fun name -> ThrownType.Exactly (corelibType state name.Namespace name.Name))

        match runs with
        | Runs.Opaque reason -> state, opaqueFromEntry reason
        | Runs.Primitive primitive ->
            let raised = contractRaises state (IntrinsicPrimitive.contract primitive)

            state,
            contracted (List.map fst raised) [] (raised |> List.collect snd |> List.map (fun method -> method, []))
        | Runs.Native native ->
            let contract = NativeMethod.contract native

            // CoreCLR makes each exception a native raises by running its parameterless constructor
            // (`EEException::CreateThrowable`).
            let constructors =
                contract.Raises
                |> List.map (fun name -> parameterlessConstructor state name, [])

            state,
            contracted
                (exactly contract.Raises)
                (contract.Assumes |> List.map Assumption.ofNative)
                (constructors @ List.map (nativeCallee state) contract.Runs)
        | Runs.ResourceLookup ->
            // A type initializer it runs may fail, and throwing that one's own exception again
            // preserves its stack trace.
            state, contracted (exactly ResourceLookup.raises) [] [ preserveStackTrace state, [] ]
        | Runs.ObjectGetType ->
            let constructors =
                ObjectGetType.constructs state.Runtime
                |> List.map (fun name -> parameterlessConstructor state name, [])

            // It returns the object's `RuntimeType`, never null.
            let runtimeType = corelibType state "System" "RuntimeType"
            let state, spelling, _ = spelledDefinition state runtimeType

            let returned =
                {
                    Type = spelling
                    SpelledIn = runtimeType.AssemblyFullName
                    InBodyContext = true
                }

            state,
            { contracted (exactly (ObjectGetType.raises state.Runtime)) [] constructors with
                Spellings = [| returned |]
                Returns = StackValue.Objects (Set.singleton (SpelledObject.Exactly 0))
            }
        | Runs.Assumed assumption ->
            state,
            contracted
                (exactly (Assumption.raises assumption))
                [ assumption ]
                (Assumption.constructs assumption
                 |> List.map (fun name -> parameterlessConstructor state name, []))
        | Runs.Il (body, selfCall) ->

        // CoreCLR converts the method's own signature for the JIT before anything else of it
        // (`getMethodInfo`), refusing some (`SignatureRefusal`): then none of the body runs, and the
        // JIT loads none of the types it names.
        match SignatureRefusal.ofSignature method.Signature with
        | Some refusal -> state, outsideOnly (Set.ofList (signatureRefusalFacts state refusal))
        | None ->

        match callersOnlyValidation state assembly method with
        // Nothing of the body runs, and the JIT loads none of its types.
        | state, CallersOnlyValidation.Refused refusal -> state, outsideOnly refusal
        | state, CallersOnlyValidation.Compiles validation ->

        let ops = body.Instructions |> Array.ofList

        // Inside a type initializer, touching the type it initializes cannot trigger it: the CLI
        // lets the initializing thread straight through (ECMA-335 I.8.9.5). Only for a non-generic
        // type, though: each instantiation of a generic one has its own statics and initializer, and
        // a definition cannot tell `G<int>` from the `G<string>` being initialized.
        let initializing =
            if
                method.Name = ".cctor"
                && method.IsStatic
                && method.RequiredDeclaringType.Generics.IsEmpty
            then
                Some method.RequiredDeclaringType.Identity
            else
                None

        // The type declaring the static field a token names, and how the token instantiates it;
        // `None` for an instance field, and for a token that does not bind, whose instruction never
        // runs.
        let staticFieldOwner
            (state : EscapeAnalysisState)
            (token : MetadataToken)
            : EscapeAnalysisState * (ResolvedTypeIdentity * CalleeSpelling) option
            =
            let owner (state : EscapeAnalysisState) (field : FieldInfo<GenericParamFromMetadata, TypeDefn>) spelling =
                if field.IsStatic then
                    let identity = field.DeclaringType.Identity
                    let _, ty = definitionOf state identity

                    let spelling =
                        if ty.Generics.IsEmpty then
                            CalleeSpelling.Fixed
                        else
                            spelling

                    state, Some (identity, spelling)
                else
                    state, None

            match token with
            | MetadataToken.FieldDefinition handle -> owner state assembly.Fields.[handle] CalleeSpelling.Typical
            | MetadataToken.MemberReference handle ->
                let assemblies, target =
                    FieldReferenceResolution.resolve
                        state.LoggerFactory
                        state.RuntimeDirs
                        state.BaseTypes
                        state.TypeSystem._LoadedAssemblies
                        assembly
                        handle

                let state = withAssemblies state assemblies

                match target with
                | FieldReferenceTarget.Defined (declaring, field) ->
                    // Only a TypeSpec parent spells the declaring type's arguments.
                    let spelling =
                        match assembly.Members.[handle].Parent with
                        | MetadataToken.TypeSpecification _ -> CalleeSpelling.Spelled token
                        | _ -> CalleeSpelling.Typical

                    owner state declaring.Fields.[field] spelling
                | FieldReferenceTarget.Missing
                | FieldReferenceTarget.DependsOnInstantiation
                | FieldReferenceTarget.ParentTypeMissing _
                | FieldReferenceTarget.ParentAssemblyUnavailable _
                | FieldReferenceTarget.SignatureAssemblyUnavailable _ -> state, None
                | FieldReferenceTarget.Unmodelled unmodelled -> failwith unmodelled.Diagnostic
            | other -> failwith $"A field instruction in %s{assembly.DefinitionFullName} names %O{other}"

        // What this instruction raises by running a type initializer, which `OpcodeFaults` says it
        // may (`initializerFailure`). An access to a static field runs its declaring type's, unless
        // that is the type this body initializes; one to an instance field runs none. A call runs
        // the initializer of what it reaches, which `callsOf` accounts for once dispatch decides
        // that. Nothing names what `calli` reaches, which leaves its call opaque, so it raises only
        // the wrapper here.
        let initializerAt
            (state : EscapeAnalysisState)
            (op : IlOp)
            (offset : int)
            : EscapeAnalysisState *
              (int * ThrownType) list *
              (int * CallSite * SpelledArgument list) list *
              (int * Opacity * MethodKey option) list
            =
            match op with
            | IlOp.UnaryMetadataToken ((UnaryMetadataTokenIlOp.Ldsfld | UnaryMetadataTokenIlOp.Stsfld | UnaryMetadataTokenIlOp.Ldsflda | UnaryMetadataTokenIlOp.Ldfld | UnaryMetadataTokenIlOp.Ldflda | UnaryMetadataTokenIlOp.Stfld),
                                       operand) ->
                match operand with
                // No metadata names the field, so nothing says whose initializer runs.
                | MetadataOperand.FromDynamicScope _ -> state, [], [], [ offset, Opacity.IndirectCall, None ]
                | MetadataOperand.FromMetadata token ->

                match staticFieldOwner state token.Token with
                | state, None -> state, [], [], []
                | state, Some (owner, _) when Some owner = initializing -> state, [], [], []
                | state, Some (owner, spelling) ->
                    match typeInitializer state owner with
                    | None -> state, [], [], []
                    | Some initializer ->
                        let wrapper, preserve = initializerFailure state

                        let initializer : Callee =
                            {
                                Callee = initializer
                                Spelling = spelling
                            }

                        state,
                        [ offset, wrapper ],
                        [ offset, CallSite.Runtime initializer, [] ; offset, runtimeRuns preserve, [] ],
                        []
            | IlOp.UnaryMetadataToken ((UnaryMetadataTokenIlOp.Call | UnaryMetadataTokenIlOp.Callvirt | UnaryMetadataTokenIlOp.Newobj | UnaryMetadataTokenIlOp.Jmp),
                                       _) -> state, [], [], []
            | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Calli, _) ->
                state,
                [
                    offset, ThrownType.Exactly (corelibException state "TypeInitializationException")
                ],
                [],
                []
            | other -> failwith $"BUG: OpcodeFaults says %A{other} runs a type initializer, and nothing here says whose"

        // The `constrained.` prefix on the instruction at `index`: its own index, and the type it
        // names. An instruction's prefixes immediately precede it, and none of them can be a
        // branch target.
        let constrainedPrefix (index : int) : (int * MetadataToken) option =
            let rec back (at : int) : (int * MetadataToken) option =
                if at < 0 then
                    None
                else
                    match fst ops.[at] with
                    | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Constrained, MetadataOperand.FromMetadata token) ->
                        Some (at, token.Token)
                    | IlOp.Nullary (NullaryIlOp.Tail | NullaryIlOp.Volatile | NullaryIlOp.Readonly)
                    | IlOp.UnaryConst (UnaryConstIlOp.Unaligned _) -> back (at - 1)
                    | _ -> None

            back (index - 1)

        // The delegate type whose constructor a `newobj` right after the instruction at `index`
        // calls, if it calls one.
        let delegateMade
            (state : EscapeAnalysisState)
            (index : int)
            : EscapeAnalysisState * ResolvedTypeIdentity option
            =
            if index + 1 >= ops.Length then
                state, None
            else

            match fst ops.[index + 1] with
            | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Newobj, MetadataOperand.FromMetadata constructor) ->
                match callTarget state assembly constructor.Token with
                | state, CallTarget.Method constructor ->
                    let made = declaringTypeOf state constructor.Callee

                    match derivesFrom state made state.BaseTypes.DelegateType.Identity with
                    | state, true -> state, Some made
                    | state, false -> state, None
                | state, _ -> state, None
            | _ -> state, None

        // What becomes of the address the `ldftn` at `index` pushes, by the instruction right after
        // it: a `newobj` of a delegate's constructor, a `pop`, or anything else.
        let addressUse (state : EscapeAnalysisState) (index : int) : EscapeAnalysisState * MethodAddressUse =
            let popped =
                index + 1 < ops.Length
                && (
                    match fst ops.[index + 1] with
                    | IlOp.Nullary NullaryIlOp.Pop -> true
                    | _ -> false
                )

            if popped then
                state, MethodAddressUse.Discarded
            else

            match delegateMade state index with
            | state, Some _ -> state, MethodAddressUse.DelegateTarget
            | state, None -> state, MethodAddressUse.FunctionPointer

        // 0. Bind the token the instruction names, which the JIT does before the body runs. This
        // binds every instruction's, as the JIT does for code no run reaches unless a branch it
        // folds cuts that code off; it folds a branch a capability query decides, so for code
        // only such a branch reaches, this is an over-approximation.
        let bind
            (
                state : EscapeAnalysisState,
                targets : Map<int, CallTarget>,
                unbound : Set<int>,
                bindingFailures : Set<OutsideBodyFact>,
                raisedWhereInlined : Set<OutsideBodyFact>,
                refusedCompilations : Map<int, CompilationRefusal>,
                instanceChecks : InstanceCheck list
            )
            (index : int)
            =
            let op, _ = ops.[index]

            let (state, methodTarget, unbound, bindingFailures, raisedWhereInlined, refusedCompilations, instanceChecks) =
                match op with
                | IlOp.UnaryMetadataToken (tokenOp, MetadataOperand.FromMetadata token) ->
                    let state, referent, failures, tokenChecks = bindToken state assembly token.Token

                    // CoreCLR checks the constraints of what an instruction it checks access of
                    // names in the method's open form, its type variables standing for themselves
                    // (`CEEInfo::GetMethodForSecurity`), which the analysis does not decide.
                    let state, overTypeVariables =
                        let checksAccess =
                            match OpcodeFaults.ofIlOp op with
                            | OpcodeFaults.Raises (_, resolution) ->
                                resolution
                                |> Set.exists (fun fault ->
                                    match fault with
                                    | ResolutionFault.MethodAccess
                                    | ResolutionFault.FieldAccess
                                    | ResolutionFault.TypeAccess -> true
                                    | ResolutionFault.TokenBinding
                                    | ResolutionFault.ScopeEntry
                                    | ResolutionFault.DispatchOnReceiver
                                    | ResolutionFault.DispatchOnConstrainedType -> false
                                )
                            | OpcodeFaults.Unmodelled -> false

                        let generic =
                            not method.Generics.IsEmpty || not method.DeclaringTypeGenerics.IsEmpty

                        if not (checksAccess && generic) then
                            state, []
                        else

                        let state, mayBreak =
                            match token.Token with
                            | MetadataToken.TypeSpecification handle ->
                                spellingMayBreakConstraint state assembly method assembly.TypeSpecs.[handle].Signature
                            | MetadataToken.MemberReference handle ->
                                match assembly.Members.[handle].Parent with
                                | MetadataToken.TypeSpecification parent ->
                                    spellingMayBreakConstraint
                                        state
                                        assembly
                                        method
                                        assembly.TypeSpecs.[parent].Signature
                                | _ -> state, false
                            | MetadataToken.MethodSpecification handle ->
                                specificationMayBreakConstraint state assembly method handle
                            | _ -> state, false

                        state,
                        (if mayBreak then
                             [ OutsideBodyFact.Opaque Opacity.ConstraintsOverTypeVariables ]
                         else
                             [])

                    // A token that binds may still be refused for the type it names.
                    let failures =
                        match tokenOp, failures with
                        | UnaryMetadataTokenIlOp.Ldtoken, _
                        | _, _ :: _ -> failures
                        | _, [] -> resolutionRefusal state assembly token.Token

                    let target =
                        match referent with
                        | TokenReferent.Method target -> Some target
                        | TokenReferent.Field _
                        | TokenReferent.Other -> None

                    let raisesOf (failure : BindFailure option) : OutsideBodyFact list =
                        failure |> Option.map (bindFailureFacts state) |> Option.defaultValue []

                    // Once the token binds, the JIT loads the types of the signature of the member
                    // it names where `JitTypeLoads.memberPositions` says: a method's as the token
                    // spells it, a field's as its definition does (`getFieldInfo`). A load it may
                    // make or not that fails may fail the compilation, which leaves the token
                    // bound; one it makes asking for an `ldftn`'s address it may make compiling
                    // whatever method the `ldftn` is inlined into, past every handler of that
                    // method too.
                    let state, failures, possible, inlined, refusedHere, signatureChecks =
                        let bound =
                            failures
                            |> List.forall (fun fact ->
                                match fact with
                                | OutsideBodyFact.Raises _ -> false
                                | OutsideBodyFact.Runs _
                                | OutsideBodyFact.Opaque _ -> true
                            )

                        let state, operand =
                            match tokenOp, referent, spelledSignature assembly token.Token with
                            | _ when not bound -> state, None
                            | (UnaryMetadataTokenIlOp.Ldfld | UnaryMetadataTokenIlOp.Ldflda | UnaryMetadataTokenIlOp.Stfld | UnaryMetadataTokenIlOp.Ldsfld | UnaryMetadataTokenIlOp.Ldsflda | UnaryMetadataTokenIlOp.Stsfld),
                              TokenReferent.Field (declaring, field),
                              _ ->
                                let field = declaring.Fields.[field]

                                state,
                                Some (
                                    declaring,
                                    MemberSignature.Field (field.SignatureHeader, field.Signature),
                                    MemberOperand.FieldAccess,
                                    None
                                )
                            | _, _, None -> state, None
                            | (UnaryMetadataTokenIlOp.Ldftn | UnaryMetadataTokenIlOp.Ldvirtftn), _, Some signature ->
                                let prefix =
                                    match constrainedPrefix index with
                                    | Some _ -> AddressConstraint.Constrained
                                    | None -> AddressConstraint.Unconstrained

                                let state, addressUse = addressUse state index

                                let state, delegateMade =
                                    match addressUse, target with
                                    | MethodAddressUse.DelegateTarget, Some (CallTarget.Method callee) ->
                                        match delegateMade state index with
                                        | state, Some made -> state, Some (made, callee.Callee)
                                        | _ ->
                                            failwith
                                                $"BUG: the ldftn at %d{index} takes a delegate's target, but no delegate is made after it"
                                    | _ -> state, None

                                state,
                                Some (
                                    assembly,
                                    MemberSignature.Method signature,
                                    MemberOperand.Address (
                                        JitTypeLoads.preparation tokenOp token.Token prefix addressUse
                                    ),
                                    delegateMade
                                )
                            | _, _, Some signature ->
                                state,
                                memberOperand
                                    state
                                    assembly
                                    (UnmanagedCallPlacement.ofRegions body.ExceptionRegions (snd ops.[index]))
                                    tokenOp
                                    signature
                                    target
                                |> Option.map (fun operand -> assembly, MemberSignature.Method signature, operand, None)

                        match operand with
                        | None -> state, failures, [], [], None, []
                        | Some (spellingAssembly, signature, operand, delegateMade) ->

                        // A `call` or `ldftn` naming an abstract method itself, CoreCLR refuses as
                        // it resolves it, before it converts the signature (`abstractCallees`).
                        let abstractTarget =
                            match tokenOp, target with
                            | (UnaryMetadataTokenIlOp.Call | UnaryMetadataTokenIlOp.Ldftn),
                              Some (CallTarget.Method callee) when (constrainedPrefix index).IsNone ->
                                match (snd (methodOf state callee.Callee)).Body with
                                | MethodBody.Abstract -> true
                                | _ -> false
                            | _ -> false

                        // CoreCLR converts the signature of the method the instruction names, or of
                        // `calli`'s call site, for the JIT before the JIT loads any type of it,
                        // refusing some (`SignatureRefusal`); some loads come before that: preparing
                        // the method as a function pointer, for some addresses. Of a call of
                        // unmanaged code, the JIT reads the calling convention once it has loaded the
                        // return type, which CoreCLR may refuse or fail to read; then the JIT meets
                        // nothing more of the signature (`JitTypeLoads.unmanagedMeetings`).
                        let refusal =
                            let signatureRefusal =
                                match signature with
                                | MemberSignature.Method methodSignature when
                                    SignatureRefusal.readsMemberSignature operand && not abstractTarget
                                    ->
                                    SignatureRefusal.ofSignature methodSignature
                                | MemberSignature.Method _
                                | MemberSignature.Field _ -> None

                            match signatureRefusal, operand with
                            | Some refusal, _ ->
                                Some (
                                    CompilationRefusal.Signature refusal,
                                    MemberPositions.Described (SignatureRefusal.positionsBeforeConversion operand)
                                )
                            | None, MemberOperand.UnmanagedCall call ->
                                match call.Convention, signature with
                                | UnmanagedConventionRead.Reads _, MemberSignature.Method methodSignature when
                                    UnmanagedCallingConvention.refusesThiscallWithoutArguments
                                        call.Convention
                                        methodSignature
                                    ->
                                    Some (CompilationRefusal.ThiscallWithoutArguments, MemberPositions.BySignature)
                                | UnmanagedConventionRead.Reads _, _ -> None
                                | UnmanagedConventionRead.MultipleBases, _ ->
                                    Some (CompilationRefusal.MultipleCallingConventions, MemberPositions.BySignature)
                                | UnmanagedConventionRead.Unreadable _, _
                                | UnmanagedConventionRead.NullArray, _ ->
                                    Some (CompilationRefusal.UnreadableCallingConvention, MemberPositions.BySignature)
                            | None, _ -> None

                        match refusal with
                        | Some (refusal, positions) ->
                            let state, loads =
                                memberSignatureLoads state spellingAssembly token.Token operand positions signature

                            let possible = loads.Possible |> List.collect (Some >> raisesOf)

                            match loads.Definite with
                            | Some _ -> state, failures @ raisesOf loads.Definite, possible, [], None, loads.Deferred
                            // What it raises the body accounts for where the JIT compiles the
                            // instruction (`refusedCompilations`).
                            | None -> state, failures, possible, [], Some refusal, loads.Deferred
                        | None ->

                            let state, loads =
                                memberSignatureLoads
                                    state
                                    spellingAssembly
                                    token.Token
                                    operand
                                    (JitTypeLoads.memberPositions operand)
                                    signature

                            // Making the delegate, the JIT compiles the `newobj` after the address
                            // instruction. CoreCLR walks the signatures to make its thunk where the
                            // JIT compiles it, or, for a static target that code shared between
                            // instantiations names through its type variables, as it runs
                            // (`Delegate_Construct`), where the method's own handlers can catch
                            // the failure. Either is taken to fail the compilation. Where the JIT
                            // compiles it, .NET 10's CoreCLR first refuses a target the token names
                            // that is `[UnmanagedCallersOnly]` (`UnmanagedCallersOnly.refusesDelegateTarget`,
                            // `ExceptionMaking.unmanagedCallersOnlyDelegateTarget`), and walks no
                            // signature.
                            let state, delegateFailures =
                                match loads.Definite, delegateMade with
                                | None, Some (made, target) ->
                                    let _, targetMethod = methodOf state target

                                    if
                                        MethodInfo.isUnmanagedCallersOnly targetMethod
                                        && UnmanagedCallersOnly.refusesDelegateTarget state.Runtime
                                    then
                                        let name, making = ExceptionMaking.unmanagedCallersOnlyDelegateTarget
                                        let thrown, run = runtimeRaises state name making

                                        state,
                                        OutsideBodyFact.Raises thrown
                                        :: List.map (fun made -> OutsideBodyFact.Runs (made, [])) run
                                    else
                                        let state, failure = shuffleThunkFailure state made target
                                        state, raisesOf failure
                                | _ -> state, []

                            // Where the JIT may begin the walk of an unmanaged call's signature for
                            // whether it needs marshalling, the VM may refuse the P/Invoke there
                            // instead (`JitTypeLoads.unmanagedWalkRefuses`). Where the return type
                            // fails to load first, this over-approximates.
                            let walkRefusal =
                                match operand, signature with
                                | MemberOperand.UnmanagedCall call, MemberSignature.Method methodSignature when
                                    JitTypeLoads.unmanagedWalkRefuses call methodSignature
                                    ->
                                    let name, making = ExceptionMaking.pInvokeDescriptionRefused
                                    let thrown, run = runtimeRaises state name making

                                    OutsideBodyFact.Raises thrown
                                    :: List.map (fun made -> OutsideBodyFact.Runs (made, [])) run
                                | _ -> []

                            let possible =
                                (loads.Possible |> List.collect (Some >> raisesOf))
                                @ walkRefusal
                                @ delegateFailures

                            let inlined =
                                match operand with
                                | MemberOperand.Address FunctionPointerPreparation.IfAddressRequested -> possible
                                | _ -> []

                            state, failures @ raisesOf loads.Definite, possible, inlined, None, loads.Deferred

                    // A token that fails to bind names nothing to instantiate: the JIT throws
                    // before the body runs.
                    let target, unbound =
                        if failures.IsEmpty then
                            target, unbound
                        else
                            let target =
                                match target with
                                | Some (CallTarget.Method callee) ->
                                    Some (
                                        CallTarget.Method
                                            { callee with
                                                Spelling = CalleeSpelling.Typical
                                            }
                                    )
                                | other -> other

                            target, Set.add index unbound

                    // Binding also runs the module initializer of every other module it activates;
                    // this module's has run, since its code is running. Should one fail, the
                    // runtime raises what any type initializer's failure does (`initializerFailure`).
                    let state, initializers =
                        if activatesModules tokenOp then
                            let state, activated = activatedTypes state assembly token.Token

                            state,
                            activated
                            |> List.map _.AssemblyFullName
                            |> List.distinct
                            |> List.filter (fun activated -> activated <> assembly.DefinitionFullName)
                            |> List.choose (moduleInitializer state)
                        else
                            state, []

                    let failures =
                        if initializers.IsEmpty then
                            failures
                        else
                            let wrapper, preserve = initializerFailure state

                            OutsideBodyFact.Raises wrapper
                            :: OutsideBodyFact.Runs (preserve, [])
                            :: (initializers
                                |> List.map (fun initializer -> OutsideBodyFact.Runs (initializer, [])))
                            @ failures

                    state,
                    target,
                    unbound,
                    Set.unionMany
                        [
                            bindingFailures
                            Set.ofList failures
                            Set.ofList possible
                            Set.ofList overTypeVariables
                        ],
                    Set.union raisedWhereInlined (Set.ofList inlined),
                    (match refusedHere with
                     | Some refusal -> Map.add index refusal refusedCompilations
                     | None -> refusedCompilations),
                    instanceChecks @ tokenChecks @ signatureChecks
                | _ -> state, None, unbound, bindingFailures, raisedWhereInlined, refusedCompilations, instanceChecks

            let targets =
                match methodTarget with
                | Some target -> Map.add index target targets
                | None -> targets

            state, targets, unbound, bindingFailures, raisedWhereInlined, refusedCompilations, instanceChecks

        let folder
            (targets : Map<int, CallTarget>)
            (unbound : Set<int>)
            (spellings : SpelledType[])
            (objectsAt : int -> int -> SpelledObject list option)
            (integersAt : int -> int -> Set<SpelledInteger> option)
            (calleeArguments : int -> int option)
            (passedAt : int -> bool -> SpelledArgument list)
            (
                state : EscapeAnalysisState,
                raises : (int * ThrownType) list,
                opaque : (int * Opacity * MethodKey option) list,
                calls : (int * CallSite * SpelledArgument list) list
            )
            (index : int)
            =
            let op, offset = ops.[index]
            let methodTarget = Map.tryFind index targets

            // Whether the operands on the stack may be what the instruction needs of them to raise
            // `fault`. An integer the body does not decide may be any value.
            let operandsAllow (fault : OpcodeFault) : bool =
                OpcodeFaults.requiredOperands op fault
                |> List.forall (fun required ->
                    match integersAt offset required.FromTop with
                    | None -> true
                    | Some integers ->
                        integers
                        |> Set.exists (fun integer ->
                            match integer with
                            | SpelledInteger.Constant value -> StackInteger.is required.Is value
                            | SpelledInteger.Argument _
                            | SpelledInteger.Loaded _ -> true
                        )
                )

            // 1. What the instruction raises by itself. What a `rethrow` raises is its handler's to
            // say, which `rethrows` below records.
            let state, raises, opaque, calls =
                match OpcodeFaults.ofIlOp op with
                | OpcodeFaults.Unmodelled ->
                    match op with
                    | IlOp.Nullary NullaryIlOp.Rethrow -> state, raises, opaque, calls
                    | _ -> state, raises, (offset, Opacity.UnmodelledOpcode, None) :: opaque, calls
                // What resolving the token raises is `accessChecks`' to account for, below.
                | OpcodeFaults.Raises (faults, _resolution) ->
                    ((state, raises, opaque, calls), List.filter operandsAllow faults)
                    ||> List.fold (fun (state, raises, opaque, calls) fault ->
                        match fault with
                        | OpcodeFault.TypeInitialization ->
                            let state, raised, ran, hidden = initializerAt state op offset
                            state, raised @ raises, hidden @ opaque, ran @ calls
                        | _ ->
                            let raised, constructed = raisedAt offset [ faultRaises state op fault ]
                            state, raised @ raises, opaque, constructed @ calls
                    )

            // 2. What the instruction throws explicitly: each object its operand may be. A null one
            // raises only the fault `OpcodeFaults` names for `throw`. One that may not be an
            // exception the runtime wraps, allocating a `RuntimeWrappedException` and running its
            // constructor, and it throws what either raises in the wrapper's place
            // (`WrapNonCompliantException`).
            let state, raises, opaque, calls =
                match op with
                | IlOp.Nullary NullaryIlOp.Throw ->
                    let state, thrown =
                        match objectsAt offset 0 with
                        | None -> state, None
                        | Some objects ->
                            ((state, Some []), objects)
                            ||> List.fold (fun (state, thrown) object ->
                                // A call's result, or an argument, is named by the type declared
                                // for it.
                                let named =
                                    match object with
                                    | SpelledObject.Null -> Some None
                                    | SpelledObject.Exactly index -> Some (Some (index, true))
                                    | SpelledObject.Within index
                                    | SpelledObject.Argument (_, index)
                                    | SpelledObject.Field (_, index)
                                    | SpelledObject.Returned (_, Some index) -> Some (Some (index, false))
                                    | SpelledObject.Returned (_, None) -> None

                                match thrown, named with
                                | None, _
                                | _, None -> state, None
                                | Some thrown, Some None -> state, Some thrown
                                | Some thrown, Some (Some (index, exact)) ->
                                    let spelling = spellings.[index]

                                    match
                                        nominalIdentity state (assemblyOf state spelling.SpelledIn) spelling.Type
                                    with
                                    | state, None -> state, None
                                    | state, Some identity when exact ->
                                        state, Some (ThrownType.Exactly identity :: thrown)
                                    | state, Some identity -> state, Some (ThrownType.SubtypeOf identity :: thrown)
                            )

                    let state, wrapped =
                        match thrown with
                        | None -> state, true
                        | Some thrown ->
                            ((state, false), thrown)
                            ||> List.fold (fun (state, wrapped) thrown ->
                                match wrapped, thrown with
                                | true, _ -> state, true
                                | false, ThrownType.Exactly ty
                                | false, ThrownType.SubtypeOf ty ->
                                    let state, isException = derivesFrom state ty state.BaseTypes.Exception.Identity
                                    state, not isException
                            )

                    let raises, calls =
                        if wrapped then
                            let raised, constructed =
                                raisedAt
                                    offset
                                    [
                                        runtimeRaises state "System.OutOfMemoryException" ExceptionMaking.Preallocated
                                    ]

                            raised @ raises, (offset, runtimeRuns (wrapperConstructor state), []) :: constructed @ calls
                        else
                            raises, calls

                    match thrown with
                    | Some thrown ->
                        state,
                        (thrown |> List.distinct |> List.map (fun thrown -> offset, thrown)) @ raises,
                        opaque,
                        calls
                    | None -> state, raises, (offset, Opacity.UntypedThrow, None) :: opaque, calls
                | _ -> state, raises, opaque, calls

            // 3. What the instruction calls.
            let state, raises, opaque, calls =
                match op with
                | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Calli, _) ->
                    state, raises, (offset, Opacity.IndirectCall, None) :: opaque, calls
                | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldvirtftn, MetadataOperand.FromMetadata _) ->
                    let onInterface =
                        match methodTarget with
                        | Some (CallTarget.Method target) ->
                            let _, declaring = definitionOf state (declaringTypeOf state target.Callee)

                            if declaring.TypeAttributes.HasFlag TypeAttributes.Interface then
                                Some target.Callee
                            else
                                None
                        | _ -> None

                    match onInterface with
                    | Some target ->
                        state, raises, (offset, Opacity.InterfaceMethodPointer, Some target) :: opaque, calls
                    | None -> state, raises, opaque, calls
                | IlOp.UnaryMetadataToken ((UnaryMetadataTokenIlOp.Call | UnaryMetadataTokenIlOp.Callvirt | UnaryMetadataTokenIlOp.Newobj | UnaryMetadataTokenIlOp.Jmp) as call,
                                           operand) ->
                    match operand, methodTarget, selfCall with
                    | MetadataOperand.FromDynamicScope _, _, _ ->
                        state, raises, (offset, Opacity.IndirectCall, None) :: opaque, calls
                    | MetadataOperand.FromMetadata _, Some (CallTarget.Method callee), Some expansion when
                        callee.Callee = key
                        && call <> UnaryMetadataTokenIlOp.Newobj
                        && call <> UnaryMetadataTokenIlOp.Jmp
                        ->
                        // The body's call to itself, which the JIT expands where it stands
                        // (`gtIsRecursiveCall`): no call happens, so the raises are this offset's.
                        let raisedHere (raised : (ThrownType * MethodKey list) list) =
                            let raisedHere, constructed = raisedAt offset raised
                            state, raisedHere @ raises, opaque, constructed @ calls

                        match IntrinsicBody.expandSelfCall state.Profile expansion with
                        | SelfCallExpansion.Constant _ -> state, raises, opaque, calls
                        | SelfCallExpansion.ThrowPlatformNotSupported ->
                            // The JIT calls CoreLib's helper in place of the method, and what the
                            // helper raises is its own IL's to say.
                            let corelib = state.BaseTypes.Corelib

                            let helper =
                                MethodKey.make corelib (IntrinsicBody.platformNotSupportedHelper corelib)

                            let helper =
                                CallSite.Direct
                                    {
                                        Callee = helper
                                        Spelling = CalleeSpelling.Fixed
                                    }

                            state, raises, opaque, (offset, helper, []) :: calls
                        | SelfCallExpansion.Primitive primitive ->
                            raisedHere (contractRaises state (IntrinsicPrimitive.contract primitive))
                        | SelfCallExpansion.HardwareInstruction intrinsicClass ->
                            match
                                HardwareInstruction.contract state.Runtime state.Target intrinsicClass method.Name
                            with
                            | InstructionContract.Raises faults ->
                                let state, raises, opaque, calls =
                                    faults |> Set.toList |> List.choose (instructionRaises state) |> raisedHere

                                // The JIT throws for an out-of-range immediate by calling CoreLib's
                                // helper, and what that raises is its own IL's to say.
                                if faults.Contains InstructionFault.ImmediateOutOfRange then
                                    let corelib = state.BaseTypes.Corelib

                                    let helper = MethodKey.make corelib (IntrinsicBody.argumentOutOfRangeHelper corelib)

                                    let helper =
                                        CallSite.Direct
                                            {
                                                Callee = helper
                                                Spelling = CalleeSpelling.Fixed
                                            }

                                    state, raises, opaque, (offset, helper, []) :: calls
                                else
                                    state, raises, opaque, calls
                            | InstructionContract.Unknown ->
                                state, raises, (offset, Opacity.IntrinsicExpansion, Some key) :: opaque, calls
                        | SelfCallExpansion.Unrecognised ->
                            state, raises, (offset, Opacity.IntrinsicExpansion, Some key) :: opaque, calls
                    | MetadataOperand.FromMetadata _, Some (CallTarget.Method callee), _ ->
                        // A static virtual is dispatched on the type a `constrained.` prefix names,
                        // which is how the only legal call to one is written; an overridable
                        // method, on that type where the prefix is there; and a class's method that
                        // cannot be overridden is resolved on that type (`constrainedTypeDecides`).
                        if
                            isStaticVirtual state callee.Callee
                            || (call = UnaryMetadataTokenIlOp.Callvirt && isOverridable state callee.Callee)
                            || (call = UnaryMetadataTokenIlOp.Callvirt
                                && (constrainedPrefix index).IsSome
                                && constrainedTypeDecides state callee.Callee)
                        then
                            match constrainedPrefix index with
                            | Some (prefix, constrainedType) when not (unbound.Contains prefix) ->
                                state,
                                raises,
                                opaque,
                                (offset, CallSite.Constrained (constrainedType, callee), passedAt offset false)
                                :: calls
                            | Some _ ->
                                state, raises, (offset, Opacity.VirtualCall, Some callee.Callee) :: opaque, calls
                            | None ->
                                // A `callvirt` dispatches on its receiver, below the arguments.
                                let receivers =
                                    match call, calleeArguments offset with
                                    | UnaryMetadataTokenIlOp.Callvirt, Some arguments when arguments > 0 ->
                                        objectsAt offset (arguments - 1) |> Option.map Set.ofList
                                    | _ -> None

                                match receivers with
                                | Some receivers ->
                                    state,
                                    raises,
                                    opaque,
                                    (offset, CallSite.Virtual (receivers, callee), passedAt offset false) :: calls
                                | None ->
                                    state, raises, (offset, Opacity.VirtualCall, Some callee.Callee) :: opaque, calls
                        elif
                            call = UnaryMetadataTokenIlOp.Newobj
                            && declaringTypeOf state callee.Callee = state.BaseTypes.String.Identity
                        then
                            match stringConstructorImplementation state callee.Callee with
                            | Some implementation ->
                                let implementation =
                                    CallSite.Direct
                                        {
                                            Callee = implementation
                                            Spelling = CalleeSpelling.Fixed
                                        }

                                state, raises, opaque, (offset, implementation, passedAt offset false) :: calls
                            | None -> state, raises, (offset, Opacity.NativeBody, Some callee.Callee) :: opaque, calls
                        else
                            state,
                            raises,
                            opaque,
                            (offset, CallSite.Direct callee, passedAt offset (call = UnaryMetadataTokenIlOp.Newobj))
                            :: calls
                    | MetadataOperand.FromMetadata _, Some (CallTarget.ArrayAccessor (arrayType, accessor)), _ ->
                        let raised, constructed =
                            arrayAccessorRaises state arrayType accessor |> raisedAt offset

                        state, raised @ raises, opaque, constructed @ calls
                    | MetadataOperand.FromMetadata _, Some CallTarget.DependsOnInstantiation, _ ->
                        state, raises, (offset, Opacity.DependsOnInstantiation, None) :: opaque, calls
                    // Binding the token fails, which step 0 recorded; there is nothing to call.
                    | MetadataOperand.FromMetadata _, Some CallTarget.Missing, _
                    | MetadataOperand.FromMetadata _, Some CallTarget.TypeMissing, _
                    | MetadataOperand.FromMetadata _, Some CallTarget.AssemblyMissing, _ -> state, raises, opaque, calls
                    | MetadataOperand.FromMetadata token, None, _ ->
                        failwith
                            $"A call in %s{assembly.DefinitionFullName} names %O{token.Token}, which is not a method"
                | _ -> state, raises, opaque, calls

            state, raises, opaque, calls

        // The JIT loads the method's return type, the type of each of its parameters and locals,
        // and the type of every `catch` clause before the body runs, as it binds every token.
        // Swift's calling convention, which an `[UnmanagedCallersOnly]` method may name, makes it
        // load more of a parameter's type. A convention the JIT cannot read raises, or kills the
        // process, before it loads the parameters' types; as where the JIT refuses a body
        // (`BoxedByRefLikeReceiver`), the answer keeps what the method would otherwise raise, and
        // is unknown from its entry. A method whose own signature names an unmanaged convention,
        // which only hand-written IL can spell, CoreCLR reads from that signature instead; that is
        // not modelled, and such a method is read here by its attribute.
        let parameterPosition, conventionFailures =
            match MethodInfo.unmanagedCallersOnly method with
            | None -> JitTypePosition.OwnParameter, Set.empty
            | Some application ->
                match UnmanagedCallingConvention.ofCallersOnly state.Runtime application with
                | UnmanagedConventionRead.Unreadable _
                | UnmanagedConventionRead.NullArray ->
                    JitTypePosition.OwnParameter,
                    Set.singleton (OutsideBodyFact.Opaque Opacity.UnreadableCallingConvention)
                | UnmanagedConventionRead.MultipleBases ->
                    failwith "BUG: UnmanagedCallingConvention.ofCallersOnly answered MultipleBases"
                | convention when UnmanagedCallingConvention.isSwift state.Target convention ->
                    JitTypePosition.SwiftOwnParameter, Set.empty
                | UnmanagedConventionRead.Reads _ -> JitTypePosition.OwnParameter, Set.empty

        let ownTypes =
            [
                match method.Signature.ReturnType with
                | MethodReturnType.Void -> ()
                | MethodReturnType.Returns returned -> yield JitTypePosition.OwnReturn, returned
                for parameter in method.Signature.ParameterTypes do
                    yield parameterPosition, parameter
                for local in Option.defaultValue ImmutableArray.Empty body.LocalVars do
                    yield JitTypePosition.Local, local
            ]

        let state, localFailures, ownChecks =
            ((state, Set.union validation conventionFailures, []), ownTypes)
            ||> List.fold (fun (state, failures, checks) (position, spelling) ->
                match JitTypeLoads.load position spelling with
                | None -> state, failures, checks
                | Some (JitTypeLoad.InFull loaded) ->
                    match jitFullLoad state assembly loaded with
                    | state, None, more -> state, failures, checks @ more
                    | state, Some failure, more ->
                        state, Set.union failures (Set.ofList (bindFailureFacts state failure)), checks @ more
                | Some load ->
                    failwith
                        $"BUG: JitTypeLoads.load answers %O{load} at %O{position} in %s{assembly.DefinitionFullName}, which loads in full or not at all"
            )

        let state, localFailures, ownChecks =
            ((state, localFailures, ownChecks), body.ExceptionRegions)
            ||> Seq.fold (fun (state, failures, checks) region ->
                match region with
                | ExceptionRegion.Catch (ExceptionCatchType.FromMetadata token, _) ->
                    let state, _, clauseFailures, clauseChecks = bindToken state assembly token

                    let clauseFailures =
                        match clauseFailures with
                        | [] -> resolutionRefusal state assembly token
                        | _ :: _ -> clauseFailures

                    state, Set.union failures (Set.ofList clauseFailures), checks @ clauseChecks
                | _ -> state, failures, checks
            )

        let state, targets, unbound, bindingFailures, raisedWhereInlined, refusedCompilations, instanceChecks =
            ((state, Map.empty, Set.empty, localFailures, Set.empty, Map.empty, ownChecks), [ 0 .. ops.Length - 1 ])
            ||> List.fold bind

        // Which objects each slot of the stack may hold, by the types the body spells.
        let state, bodySpellings = bodySpellingsOf state assembly method body unbound
        let values = StackValues.analyse bodySpellings body
        let spellings = Array.ofList bodySpellings.Spellings

        // A synchronized method takes a monitor around its body, outside its handlers, as the JIT
        // has it on a target with funclets, which every `JitTarget` is: the runtime's
        // `SynchronizedMethodHelpers`, its `Enter(lock, ref acquired)` before the body, with
        // `acquired` false, and its `Exit(lock, ref acquired)` after it, however it ends. The lock
        // is the receiver as the caller passed it, which `call` may pass as null, or for a static
        // method its type's `RuntimeType`, which the JIT may make by calling
        // `RuntimeTypeHandle.GetRuntimeTypeFromHandle` (`CORINFO_HELP_GETSYNCFROMCLASSHANDLE`), which
        // raises only what `lookupRaises` says.
        let state, spellings, monitor =
            let synchronized =
                match method with
                | MethodInfo.Metadata (_, facts) -> facts.ImplAttributes.HasFlag MethodImplAttributes.Synchronized
                | MethodInfo.Synthesised _ -> false

            if not synchronized then
                state, spellings, []
            else
                let state, spellings, lock =
                    if method.IsStatic then
                        let runtimeType = corelibType state "System" "RuntimeType"
                        let state, spelling, _ = spelledDefinition state runtimeType

                        let spelled =
                            {
                                Type = spelling
                                SpelledIn = runtimeType.AssemblyFullName
                                InBodyContext = true
                            }

                        state,
                        Array.append spellings [| spelled |],
                        StackValue.Objects (Set.singleton (SpelledObject.Exactly spellings.Length))
                    else
                        let receiver =
                            match bodySpellings.Arguments with
                            | StackValue.Objects objects :: _ ->
                                match Set.toList objects with
                                | [ SpelledObject.Argument (0, spelling) ]
                                | [ SpelledObject.Within spelling ] ->
                                    StackValue.Objects (Set.singleton (SpelledObject.Argument (0, spelling)))
                                | _ -> StackValue.Unknown
                            | _ -> StackValue.Unknown

                        state, spellings, receiver

                let helpers = SynchronizedMethodHelpers.onRuntime state.Runtime

                let acquired =
                    SpelledArgument.Pointer (
                        PrimitiveType.Boolean,
                        Set.singleton (SpelledInteger.Constant (StackInteger.Int32 0))
                    )

                state,
                spellings,
                [
                    OutsideBodyFact.Runs (
                        corelibStatic state "System.Threading" "Monitor" helpers.Enter 2,
                        [ SpelledArgument.Value lock ; acquired ]
                    )
                    OutsideBodyFact.Runs (
                        corelibStatic state "System.Threading" "Monitor" helpers.Exit 2,
                        [ SpelledArgument.Value lock ; SpelledArgument.Value StackValue.Unknown ]
                    )
                    if method.IsStatic then
                        yield! lookupRaises state |> List.map OutsideBodyFact.Raises
                ]

        let outsideBody = Set.union bindingFailures (Set.ofList monitor)

        let valueAt (offset : int) (fromTop : int) : StackValue =
            match Map.tryFind offset values.Entry with
            | Some stack when fromTop < stack.Length -> List.item fromTop stack
            | _ -> StackValue.Unknown

        let objectsAt (offset : int) (fromTop : int) : SpelledObject list option =
            match valueAt offset fromTop with
            | StackValue.Objects objects -> Some (Set.toList objects)
            | StackValue.Integers _
            | StackValue.Addresses _
            | StackValue.Unknown -> None

        let integersAt (offset : int) (fromTop : int) : Set<SpelledInteger> option =
            match valueAt offset fromTop with
            | StackValue.Integers integers -> Some integers
            | StackValue.Objects _
            | StackValue.Addresses _
            | StackValue.Unknown -> None

        let calleeArguments (offset : int) : int option =
            Map.tryFind offset bodySpellings.Effects.Callees
            |> Option.map (fun arity -> arity.Arguments)

        // What the body's locals of integer types hold, where it takes their addresses.
        let locals =
            let types =
                body.LocalVars
                |> Option.map (Seq.map StackInteger.ofParameter >> List.ofSeq)
                |> Option.defaultValue []

            LocalValues.analyse body types values

        // What the call at `offset` passes, `this` first, which is the object it makes where
        // `makesObject`. A slot the flow could not type may hold anything.
        let passedAt (offset : int) (makesObject : bool) : SpelledArgument list =
            let made =
                if makesObject then
                    [
                        Map.tryFind offset bodySpellings.Tokens
                        |> Option.defaultValue StackValue.Unknown
                        |> SpelledArgument.Value
                    ]
                else
                    []

            let passed (value : StackValue) : SpelledArgument =
                match value with
                | StackValue.Addresses addresses ->
                    match LocalValues.held locals offset addresses with
                    | Some (location, holds) -> SpelledArgument.Pointer (location, holds)
                    | None -> SpelledArgument.Value value
                | StackValue.Objects _
                | StackValue.Integers _
                | StackValue.Unknown -> SpelledArgument.Value value

            let arguments =
                match calleeArguments offset with
                | None -> []
                | Some count ->
                    [
                        for fromTop in count - 1 .. -1 .. 0 do
                            passed (valueAt offset fromTop)
                    ]

            made @ arguments

        // What each call to a capability query returns on this CPU, which decides a branch on it.
        let capabilities =
            ops
            |> Seq.indexed
            |> Seq.choose (fun (index, (op, offset)) ->
                match op, Map.tryFind index targets with
                | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Call, MetadataOperand.FromMetadata _),
                  Some (CallTarget.Method callee) ->
                    let calleeAssembly, _ = methodOf state callee.Callee

                    IntrinsicBody.constantResult state.Profile calleeAssembly callee.Callee.Method.Get
                    |> Option.map (fun value -> offset, value)
                | _ -> None
            )
            |> Map.ofSeq

        // Where the JIT compiles a query of `typeof`s to what their types decide
        // (`Assumption.JitOptimises`), and makes none of its calls:
        //   - `typeof(X).IsValueType`, `ldtoken X` then calls of `Type.GetTypeFromHandle` and
        //     `Type.get_IsValueType`, to a constant;
        //   - `typeof(X) == typeof(Y)`, `ldtoken X`, a call of `GetTypeFromHandle`, `ldtoken Y`,
        //     another, then a call of `Type.op_Equality`, or `op_Inequality` for `!=`, to a
        //     comparison of X's and Y's handles (`gtFoldTypeCompare`, gentree.cpp);
        //   - `t == typeof(X)`, whatever pushes `t`, then `ldtoken X`, a call of
        //     `GetTypeFromHandle`, then one of `op_Equality` or `op_Inequality`, to a comparison
        //     of `t` with X's `RuntimeType` by reference (`gtFoldTypeEqualityCall`), which is what
        //     the operator decides for a `RuntimeType`. What pushes `t` runs: tier 0 does not
        //     expand `Object.GetType`, so `o.GetType() == typeof(X)` still calls it. The analysis
        //     does not decide a branch on it.
        // `impIntrinsic` (importercalls.cpp) folds them while the operands are still the
        // `GetTypeFromHandle`s it expanded, so where no branch lands between the instructions. By
        // the offset of the query's last call: the indices of its calls, those of its `ldtoken`s,
        // whose `RuntimeType`s it does not make, and what a branch on it tests, where the body
        // spells its types.
        // So too `RuntimeHelpers.CreateSpan<T>` of a field's handle, `ldtoken` of the field then the
        // call, which `impCreateSpanIntrinsic` (importercalls.cpp) expands to a span over the field's
        // data, making neither the call nor the field's stub, where no branch lands on the call, the
        // handle is a constant (so of a field of a type that is not generic), the field has data
        // (`IsRVA`) at least one `T` long, and `T` is a primitive type or an enum. By the index of
        // the `ldtoken`.
        // The JIT folds them only where it makes its
        // tier-0 optimisations: not in a method marked `NoOptimization` or one whose assembly
        // disables optimisation, nor in one that calls `StubHelpers.NextCallReturnAddress`, where
        // it expands only the intrinsics it must.
        let state, typeQueries, createdSpans, referenceQueries =
            let typeType = corelibType state "System" "Type"

            let runtimeHelpers =
                corelibType state "System.Runtime.CompilerServices" "RuntimeHelpers"

            let calledMethod (index : int) : (DumpedAssembly * MethodKey) option =
                match fst ops.[index], Map.tryFind index targets with
                | IlOp.UnaryMetadataToken ((UnaryMetadataTokenIlOp.Call | UnaryMetadataTokenIlOp.Callvirt),
                                           MetadataOperand.FromMetadata _),
                  Some (CallTarget.Method callee) when not (unbound.Contains index) ->
                    let calleeAssembly, _ = methodOf state callee.Callee
                    Some (calleeAssembly, callee.Callee)
                | _ -> None

            // Whether the instruction at `index` calls the intrinsic of `Type` of this name.
            let callsTypeIntrinsic (index : int) (name : string) : bool =
                match calledMethod index with
                | Some (calleeAssembly, callee) ->
                    declaringTypeOf state callee = typeType
                    && (snd (methodOf state callee)).Name = name
                    && IntrinsicBody.isIntrinsic calleeAssembly callee.Method.Get
                | None -> false

            let optimised =
                state.Assumptions.Contains Assumption.JitOptimises
                && Debuggable.allowsJitOptimisation assembly = Some true
                && (
                    match method with
                    | MethodInfo.Metadata (_, facts) ->
                        not (facts.ImplAttributes.HasFlag MethodImplAttributes.NoOptimization)
                    | MethodInfo.Synthesised _ -> false
                )
                && [ 0 .. ops.Length - 1 ]
                   |> List.forall (fun index ->
                       match calledMethod index with
                       | Some (_, callee) ->
                           not (
                               declaringTypeOf state callee = corelibType state "System.StubHelpers" "StubHelpers"
                               && (snd (methodOf state callee)).Name = "NextCallReturnAddress"
                           )
                       | None -> true
                   )

            // The type token of a `typeof`, `ldtoken` then a call of `GetTypeFromHandle`, at `index`.
            let typeofAt (index : int) : MetadataToken option =
                if index + 1 >= ops.Length then
                    None
                else
                    match fst ops.[index] with
                    | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldtoken, MetadataOperand.FromMetadata token) when
                        not (unbound.Contains index)
                        && callsTypeIntrinsic (index + 1) "GetTypeFromHandle"
                        ->
                        Some token.Token
                    | _ -> None

            let spelledOf
                (state : EscapeAnalysisState)
                (token : MetadataToken)
                : EscapeAnalysisState * SpelledType option
                =
                let state, spelled = spelledType state assembly token

                state,
                spelled
                |> Option.map (fun (spelling, spelledIn) ->
                    {
                        Type = spelling
                        SpelledIn = spelledIn
                        InBodyContext = true
                    }
                )

            // No protected block or handler starts or ends between the instructions either: each
            // falls through to the next, and CoreCLR rejects IL that falls into or out of one.
            let landedOn = ControlFlow.landedOn body

            // Whether a branch lands on any instruction after the one at `index`, up to `last`.
            let landsAfter (index : int) (last : int) : bool =
                [ index + 1 .. last ] |> List.exists (fun i -> landedOn.Contains (snd ops.[i]))

            // Whether `CreateSpan<element>` of the field `handle`, at `index`, is one the JIT
            // expands.
            let expandsSpan
                (state : EscapeAnalysisState)
                (index : int)
                (handle : FieldDefinitionHandle)
                (element : TypeDefn)
                : EscapeAnalysisState * bool
                =
                let field = assembly.Fields.[handle]

                if
                    landsAfter index (index + 1)
                    || not field.IsStatic
                    || field.RelativeVirtualAddress.IsNone
                    || not field.DeclaringType.Generics.IsEmpty
                then
                    state, false
                else
                    let state, elementSize = primitiveValueSize state assembly element
                    let state, dataSize = fieldDataSize state field

                    match elementSize, dataSize with
                    | Some elementSize, Some dataSize -> state, dataSize >= elementSize
                    | _ -> state, false

            // Where a branch may test `RuntimeHelpers.IsReferenceOrContainsReferences<T>()`, which is
            // a constant for each instance however the method is compiled: the JIT replaces the call
            // with its answer at tier 0 (`impIntrinsic`, importercalls.cpp), and where it does not,
            // the method's own body is a call to itself, which the JIT must expand. The call stays,
            // raising nothing. By its offset.
            let referenceQueries =
                (Map.empty, [ 0 .. ops.Length - 1 ])
                ||> List.fold (fun queries index ->
                    match fst ops.[index], calledMethod index with
                    | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Call, MetadataOperand.FromMetadata call),
                      Some (calleeAssembly, callee) when
                        declaringTypeOf state callee = runtimeHelpers
                        && IntrinsicBody.isIntrinsic calleeAssembly callee.Method.Get
                        && IntrinsicPrimitive.recognise calleeAssembly callee.Method.Get = Some
                            IntrinsicPrimitive.IsReferenceOrContainsReferences
                        ->
                        match call.Token with
                        | MetadataToken.MethodSpecification spec ->
                            match List.ofSeq assembly.MethodSpecs.[spec].Signature with
                            | [ argument ] ->
                                let spelled =
                                    {
                                        Type = argument
                                        SpelledIn = assembly.DefinitionFullName
                                        InBodyContext = true
                                    }

                                Map.add (snd ops.[index]) (TypeQuery.ContainsReferences spelled) queries
                            | _ -> queries
                        | _ -> queries
                    | _ -> queries
                )

            if not optimised then
                state, Map.empty, Set.empty, referenceQueries
            else

            let state, createdSpans =
                ((state, Set.empty), [ 0 .. ops.Length - 2 ])
                ||> List.fold (fun (state, spans) index ->
                    match fst ops.[index], fst ops.[index + 1], calledMethod (index + 1) with
                    | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldtoken, MetadataOperand.FromMetadata field),
                      IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Call, MetadataOperand.FromMetadata call),
                      Some (calleeAssembly, callee) when
                        not (unbound.Contains index)
                        && declaringTypeOf state callee = runtimeHelpers
                        && (snd (methodOf state callee)).Name = "CreateSpan"
                        && IntrinsicBody.isIntrinsic calleeAssembly callee.Method.Get
                        ->
                        match field.Token, call.Token with
                        | MetadataToken.FieldDefinition handle, MetadataToken.MethodSpecification spec ->
                            match List.ofSeq assembly.MethodSpecs.[spec].Signature with
                            | [ element ] ->
                                match expandsSpan state index handle element with
                                | state, true -> state, Set.add index spans
                                | state, false -> state, spans
                            | _ -> state, spans
                        | _ -> state, spans
                    | _ -> state, spans
                )

            let state, typeQueries =
                ((state, Map.empty), [ 0 .. ops.Length - 1 ])
                ||> List.fold (fun (state, queries) index ->
                    match typeofAt index with
                    | None -> state, queries
                    | Some x ->

                    if index + 2 < ops.Length && callsTypeIntrinsic (index + 2) "get_IsValueType" then
                        if landsAfter index (index + 2) then
                            state, queries
                        else
                            let state, x = spelledOf state x

                            state,
                            Map.add
                                (snd ops.[index + 2])
                                {|
                                    Gone = [ index + 1 ; index + 2 ]
                                    Elided = [ index ]
                                    Tested = Option.map (TypeQuery.IsValueType >> Tested.Types) x
                                |}
                                queries
                    else

                    // Whether the instruction at `index` calls `op_Equality`, `Some true`, or
                    // `op_Inequality`, `Some false`.
                    let equalityAt (index : int) : bool option =
                        if index >= ops.Length then
                            None
                        elif callsTypeIntrinsic index "op_Equality" then
                            Some true
                        elif callsTypeIntrinsic index "op_Inequality" then
                            Some false
                        else
                            None

                    match typeofAt (index + 2), equalityAt (index + 4) with
                    | Some y, Some equality when not (landsAfter index (index + 4)) ->
                        let state, x = spelledOf state x
                        let state, y = spelledOf state y

                        state,
                        Map.add
                            (snd ops.[index + 4])
                            {|
                                Gone = [ index + 1 ; index + 3 ; index + 4 ]
                                Elided = [ index ; index + 2 ]
                                Tested =
                                    Option.map2 (fun x y -> Tested.Types (TypeQuery.Identity (x, y, equality))) x y
                            |}
                            queries
                    | _ ->

                    // Where the first operand is itself a `typeof`, the comparison of handles above
                    // has the operator already.
                    match equalityAt (index + 2) with
                    | Some _ when
                        not (landsAfter index (index + 2))
                        && not (Map.containsKey (snd ops.[index + 2]) queries)
                        ->
                        // The JIT still makes X's `RuntimeType` to compare `t` with.
                        state,
                        Map.add
                            (snd ops.[index + 2])
                            {|
                                Gone = [ index + 1 ; index + 2 ]
                                Elided = []
                                Tested = None
                            |}
                            queries
                    | _ -> state, queries
                )

            state, typeQueries, createdSpans, referenceQueries

        let folded =
            typeQueries
            |> Map.values
            |> Seq.collect (fun query -> query.Gone)
            |> Set.ofSeq
            |> Set.union (createdSpans |> Set.map (fun index -> index + 1))

        let elided =
            typeQueries
            |> Map.values
            |> Seq.collect (fun query -> query.Elided)
            |> Set.ofSeq
            |> Set.union createdSpans

        // What each `brtrue` or `brfalse` tests, by the offset of the instruction before it, which
        // pushes the value: integers, or objects each spelled as a reference. A branch on a value
        // type, such as an `IntPtr` that `newobj` made, tests the value, not whether it is null.
        // Where the instruction before does not fall through, something else lands on the branch.
        let state, tested =
            ((state, Map.empty), Seq.pairwise ops)
            ||> Seq.fold (fun (state, tested) ((before, beforeOffset), (op, offset)) ->
                match op with
                | IlOp.UnaryConst (UnaryConstIlOp.Brtrue _ | UnaryConstIlOp.Brtrue_s _ | UnaryConstIlOp.Brfalse _ | UnaryConstIlOp.Brfalse_s _) when
                    not (capabilities.ContainsKey beforeOffset)
                    ->
                    match Map.tryFind beforeOffset typeQueries |> Option.map (fun query -> query.Tested) with
                    | Some (Some query) -> state, Map.add beforeOffset query tested
                    | Some None -> state, tested
                    | None ->

                    match Map.tryFind beforeOffset referenceQueries with
                    | Some query -> state, Map.add beforeOffset (Tested.Types query) tested
                    | None ->

                    match ControlFlow.successorsOf beforeOffset before, valueAt offset 0 with
                    | Successors.FallThrough, StackValue.Objects objects ->
                        let state, references = allReferences state spellings (Set.toList objects)

                        if references then
                            state, Map.add beforeOffset (Tested.References objects) tested
                        else
                            state, tested
                    | Successors.FallThrough, StackValue.Integers integers ->
                        state, Map.add beforeOffset (Tested.Integers integers) tested
                    | _ -> state, tested
                | _ -> state, tested
            )

        // A test the body decides by itself, whatever its callers pass, is a constant like a
        // capability query's; the rest only an instance decides.
        let constants =
            (capabilities, tested)
            ||> Map.fold (fun constants offset tested ->
                match testIsKnown Map.empty tested with
                | Some known -> Map.add offset known constants
                | None -> constants
            )

        let tests =
            tested |> Map.filter (fun offset _ -> not (constants.ContainsKey offset))

        let executed = ControlFlow.mayExecute body constants

        // What the JIT compiles: what control reaches from the entry once it folds the branches
        // on capability queries. It folds other branches too, so this holds all it compiles and
        // perhaps more. It refuses to compile the body over some instructions there, whatever a
        // run would reach: what it raises then escapes before the body runs.
        let compiled = ControlFlow.mayExecute body capabilities

        // Each `call` or `ldftn` the JIT compiles whose token names an abstract method itself,
        // rather than through a `constrained.` prefix, which it refuses: `BadImageFormatException`
        // (`CEEInfo::getCallInfo`, jitinterface.cpp). None of them runs.
        let abstractCallees =
            [ 0 .. ops.Length - 1 ]
            |> List.filter (fun index ->
                match fst ops.[index], Map.tryFind index targets with
                | IlOp.UnaryMetadataToken ((UnaryMetadataTokenIlOp.Call | UnaryMetadataTokenIlOp.Ldftn),
                                           MetadataOperand.FromMetadata _),
                  Some (CallTarget.Method callee) when
                    compiled.Contains (snd ops.[index])
                    && not (unbound.Contains index)
                    && (constrainedPrefix index).IsNone
                    ->
                    match (snd (methodOf state callee.Callee)).Body with
                    | MethodBody.Abstract -> true
                    | _ -> false
                | _ -> false
            )
            |> Set.ofList

        let outsideBody =
            if abstractCallees.IsEmpty then
                outsideBody
            else
                // `COMPlusThrowHR` makes it as `EEException::CreateThrowable` does.
                let thrown, made = constructedRaise state "System.BadImageFormatException"

                outsideBody
                |> Set.add (OutsideBodyFact.Raises thrown)
                |> Set.add (OutsideBodyFact.Runs (made, []))

        // Each instruction the JIT compiles at which CoreCLR refuses the compilation
        // (`CompilationRefusal`), wherever a run would reach it. None of them runs.
        let outsideBody =
            ((outsideBody, Set.empty), refusedCompilations)
            ||> Map.fold (fun (outside, seen) index refusal ->
                if compiled.Contains (snd ops.[index]) && not (Set.contains refusal seen) then
                    Set.union outside (Set.ofList (compilationRefusalFacts state refusal)), Set.add refusal seen
                else
                    outside, seen
            )
            |> fst

        // The type the token of the instruction at `index` names, as the body spells it; `None`
        // for one that does not bind, or binds to an open generic definition.
        let typeOperand (state : EscapeAnalysisState) (index : int) : EscapeAnalysisState * SpelledType option =
            match fst ops.[index] with
            | IlOp.UnaryMetadataToken (_, MetadataOperand.FromMetadata token) when not (unbound.Contains index) ->
                match spelledType state assembly token.Token with
                | state, Some (spelling, spelledIn) ->
                    state,
                    Some
                        {
                            Type = spelling
                            SpelledIn = spelledIn
                            InBodyContext = true
                        }
                | state, None -> state, None
            | _ -> state, None

        // What follows the `box` at `index` in its block (`BoxPattern.follower`), with what each
        // instruction there names. Those instructions are the ones straight after the box.
        let boxFollower
            (state : EscapeAnalysisState)
            (index : int)
            : EscapeAnalysisState * BoxFollower<TokenResolution<SpelledType>>
            =
            // What the instruction at `at` names, which must be the `expected` one the follower read.
            let named (state : EscapeAnalysisState) (at : int) (expected : UnaryMetadataTokenIlOp) =
                match fst ops.[at] with
                | IlOp.UnaryMetadataToken (actual, _) when actual = expected ->
                    if unbound.Contains at then
                        state, TokenResolution.Raises
                    else
                        match typeOperand state at with
                        | state, Some spelled -> state, TokenResolution.Resolves spelled
                        | state, None -> state, TokenResolution.Undecided
                | other ->
                    failwith
                        $"BUG: BoxPattern.follower read %O{expected} straight after the box at index %d{index}, but instruction %d{at} is %O{other}"

            match BoxPattern.follower body (snd ops.[index]) with
            | BoxFollower.Branch -> state, BoxFollower.Branch
            | BoxFollower.Other -> state, BoxFollower.Other
            | BoxFollower.Unboxed _ ->
                let state, unboxed = named state (index + 1) UnaryMetadataTokenIlOp.Unbox_Any
                state, BoxFollower.Unboxed unboxed
            | BoxFollower.TestedCast _ ->
                let state, cast = named state (index + 1) UnaryMetadataTokenIlOp.Isinst
                state, BoxFollower.TestedCast cast
            | BoxFollower.CastUnboxed _ ->
                let state, cast = named state (index + 1) UnaryMetadataTokenIlOp.Isinst
                let state, unboxed = named state (index + 2) UnaryMetadataTokenIlOp.Unbox_Any
                state, BoxFollower.CastUnboxed (cast, unboxed)

        // Whether some instance of the body may name a byref-like type here: one a type variable
        // stands for, where its parameter allows it, or one whose definition is byref-like. No
        // array, pointer or byref is (`TypeHandle::IsByRefLike`, typehandle.cpp).
        let rec mayBeByRefLike (state : EscapeAnalysisState) (spelled : SpelledType) (spelling : TypeDefn) =
            let ofDefinition (state : EscapeAnalysisState) (identity : ResolvedTypeIdentity) =
                let _, definition = definitionOf state identity
                state, definition.HasIsByRefLikeAttribute

            match spelling with
            | TypeDefn.GenericTypeParameter index -> state, (snd method.DeclaringTypeGenerics.[index]).AllowsByRefLike
            | TypeDefn.GenericMethodParameter index -> state, (snd method.Generics.[index]).AllowsByRefLike
            | TypeDefn.FromDefinition (identity, _) -> ofDefinition state identity
            | TypeDefn.FromReference (typeRef, _) ->
                match resolveTypeRef state (assemblyOf state spelled.SpelledIn) typeRef with
                | state, Some identity -> ofDefinition state identity
                | state, None -> state, false
            | TypeDefn.GenericInstantiation (root, _) -> mayBeByRefLike state spelled root
            | TypeDefn.Modified modified -> mayBeByRefLike state spelled modified.Unmodified
            | _ -> state, false

        // Each `box` and `newarr` the JIT compiles that it may refuse, as only the type it names
        // decides: not a `box` it folds into a branch, which it folds whatever the type.
        let state, refusals =
            ((state, []), [ 0 .. ops.Length - 1 ])
            ||> List.fold (fun (state, refusals) index ->
                let op, offset = ops.[index]

                let site =
                    match op with
                    | IlOp.UnaryMetadataToken ((UnaryMetadataTokenIlOp.Box | UnaryMetadataTokenIlOp.Newarr) as tokenOp,
                                               MetadataOperand.FromMetadata _) when compiled.Contains offset ->
                        Some tokenOp
                    | _ -> None

                match site with
                | None -> state, refusals
                | Some tokenOp ->

                match typeOperand state index with
                | state, None -> state, refusals
                | state, Some named ->

                match mayBeByRefLike state named named.Type with
                | state, false -> state, refusals
                | state, true ->

                match tokenOp with
                | UnaryMetadataTokenIlOp.Box ->
                    match boxFollower state index with
                    | state, BoxFollower.Branch -> state, refusals
                    | state, follower -> state, (offset, RefusalSite.Box (named, follower)) :: refusals
                | _ -> state, (offset, RefusalSite.Newarr named) :: refusals
            )

        let state, raises, opaque, calls =
            ((state, [], [], []), [ 0 .. ops.Length - 1 ])
            ||> List.fold (fun acc index ->
                if
                    executed.Contains (snd ops.[index])
                    && not (folded.Contains index)
                    && not (abstractCallees.Contains index)
                    // CoreCLR refuses the signature it names, and none of it runs.
                    && not (refusedCompilations.ContainsKey index)
                then
                    folder targets unbound spellings objectsAt integersAt calleeArguments passedAt acc index
                else
                    acc
            )

        // What resolving each instruction's token can raise (`OpcodeFaults`' resolution faults).
        // Where the JIT checks what the token names may be accessed, and the visibility of each
        // does not settle the check (`MetadataAccess`), the analysis assumes the access is allowed
        // where it may, and otherwise cannot see what the instruction raises.
        let state, accessAssumed, opaque =
            ((state, [], opaque), [ 0 .. ops.Length - 1 ])
            ||> List.fold (fun (state, assumed, opaque) index ->
                let op, offset = ops.[index]

                // A token that fails to bind fails before the body runs (`bind`, above).
                if
                    not (executed.Contains offset)
                    || folded.Contains index
                    || unbound.Contains index
                    || refusedCompilations.ContainsKey index
                then
                    state, assumed, opaque
                else

                match OpcodeFaults.ofIlOp op with
                // Opaque already, at step 1.
                | OpcodeFaults.Unmodelled -> state, assumed, opaque
                | OpcodeFaults.Raises (_, resolution) ->
                    let checksAccess =
                        resolution
                        |> Set.exists (fun fault ->
                            match fault with
                            | ResolutionFault.MethodAccess
                            | ResolutionFault.FieldAccess
                            | ResolutionFault.TypeAccess -> true
                            // Bound, and refused for the type it names (`resolutionRefusal`), at
                            // `bind`, above. The JIT refusing to compile the body for what a token
                            // binds to is `abstractCallees`' and `refusals`'.
                            | ResolutionFault.TokenBinding -> false
                            // Only a `DynamicMethod`'s body has scope entries, and the analysis
                            // reads bodies from metadata.
                            | ResolutionFault.ScopeEntry -> false
                            // Decided where a call's target is dispatched (`dispatchOn`), whose
                            // failure the call raises (`DispatchOutcome.Raises`), and for a
                            // `constrained. ldftn` from `ConstrainedPointers`.
                            | ResolutionFault.DispatchOnReceiver
                            | ResolutionFault.DispatchOnConstrainedType -> false
                        )

                    if not checksAccess then
                        state, assumed, opaque
                    else

                    let state, decision =
                        match op with
                        | IlOp.UnaryMetadataToken (_, MetadataOperand.FromMetadata token) ->
                            match accessTarget state assembly token.Token (Map.tryFind index targets) with
                            | state, Some target ->
                                state,
                                MetadataAccess.decide
                                    state.TypeSystem._LoadedAssemblies
                                    assembly
                                    method.RequiredDeclaringType.Identity
                                    target
                            | state, None -> state, AccessDecision.Undecided
                        | _ -> state, AccessDecision.Undecided

                    match decision with
                    | AccessDecision.Accessible -> state, assumed, opaque
                    | AccessDecision.Undecided when state.Assumptions.Contains Assumption.MembersAccessible ->
                        state, (offset, Assumption.MembersAccessible) :: assumed, opaque
                    | AccessDecision.Undecided -> state, assumed, (offset, Opacity.UndecidedAccess, None) :: opaque
            )

        // Where the JIT makes the `RuntimeType` of a type at run time: an `ldtoken` of a type that
        // mentions a type variable, which shared code looks up in its generic dictionary, so that
        // the JIT cannot embed the `RuntimeType` as it does an exact type's (`fgMorphCall`,
        // morph.cpp; `CEEInfo::getRuntimeTypePointer`). Only an instance says whether its code is
        // shared (`instanceFacts`). A query of types the JIT folds compares handles instead.
        // What each `stfld` and `stsfld` a run may execute stores in its field: the value on top of
        // the stack.
        let stored =
            [ 0 .. ops.Length - 1 ]
            |> List.choose (fun index ->
                match fst ops.[index] with
                | IlOp.UnaryMetadataToken ((UnaryMetadataTokenIlOp.Stfld | UnaryMetadataTokenIlOp.Stsfld), _) when
                    executed.Contains (snd ops.[index])
                    ->
                    Some (snd ops.[index], valueAt (snd ops.[index]) 0)
                | _ -> None
            )
            |> Map.ofList

        // A load of a field the analysis follows relies on its writers being the IL that names it.
        let state, fieldAssumed =
            ((state, []), [ 0 .. ops.Length - 1 ])
            ||> List.fold (fun (state, fieldAssumed) index ->
                match fst ops.[index] with
                | IlOp.UnaryMetadataToken ((UnaryMetadataTokenIlOp.Ldfld | UnaryMetadataTokenIlOp.Ldsfld),
                                           MetadataOperand.FromMetadata token) when
                    executed.Contains (snd ops.[index]) && not (unbound.Contains index)
                    ->
                    match token.Token with
                    | MetadataToken.FieldDefinition field ->
                        match followableField state (FieldKey.make assembly.DefinitionFullName field) with
                        | state, true -> state, (snd ops.[index], Assumption.FieldsWrittenVisibly) :: fieldAssumed
                        | state, false -> state, fieldAssumed
                    | _ -> state, fieldAssumed
                | _ -> state, fieldAssumed
            )

        let state, lookedUp =
            ((state, []), [ 0 .. ops.Length - 1 ])
            ||> List.fold (fun (state, lookedUp) index ->
                match fst ops.[index] with
                | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldtoken, MetadataOperand.FromMetadata token) when
                    executed.Contains (snd ops.[index])
                    && not (unbound.Contains index)
                    && not (elided.Contains index)
                    ->
                    match spelledType state assembly token.Token with
                    | state, Some (spelling, spelledIn) when mentionsTypeVariable spelling ->
                        let spelled =
                            {
                                Type = spelling
                                SpelledIn = spelledIn
                                InBodyContext = true
                            }

                        state, (snd ops.[index], spelled) :: lookedUp
                    | state, _ -> state, lookedUp
                | _ -> state, lookedUp
            )

        // Where the JIT makes the stub that the handle of a method or a field holds: at every
        // `ldtoken` of one, which it imports as a call of the stub's `FromPtr` (`CEE_LDTOKEN`,
        // importer.cpp), in shared code as elsewhere, but for a field whose `CreateSpan` it expands.
        // It expands `RuntimeHelpers.InitializeArray` of one too, but only where it optimises fully
        // (`impIntrinsic`), which a method it compiles first does not.
        let stubs =
            [ 0 .. ops.Length - 1 ]
            |> List.choose (fun index ->
                match fst ops.[index] with
                | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldtoken, MetadataOperand.FromMetadata token) when
                    executed.Contains (snd ops.[index])
                    && not (unbound.Contains index)
                    && not (elided.Contains index)
                    ->
                    memberHandleOf assembly token.Token
                    |> Option.map (fun handle -> snd ops.[index], runtimeRuns (stubMaker state handle), [])
                | _ -> None
            )

        // Where the JIT calls one of CoreLib's `CastHelpers` methods: at every `isinst` and
        // `castclass`, and every `unbox.any` of a reference type, which it imports as a call of the
        // helper `getCastingHelperStatic` picks (`impCastClassOrIsInstToTree`, importer.cpp); at
        // every `unbox`, and `unbox.any` of a value type, the one `getUnBoxHelper` picks; at a `box`
        // of a struct (`impImportAndPushBox`); at an `ldelema` of a reference type with no
        // `readonly.` prefix; and at every `stelem.ref`, and `stelem` of a reference type, whatever
        // it stores. It decides a cast or an unbox without one only where it optimises
        // (`impOptimizeCastClassOrIsInst`), which a method it compiles first does not, or where it
        // folds a `box` that the cast tests (`impBoxPatternMatch`), and it skips a store's call only
        // where it optimises too (`gtCanSkipCovariantStoreCheck`); the analysis follows neither:
        // there it follows the calls.
        let state, helperCalls, helperSites =
            ((state, [], []), [ 0 .. ops.Length - 1 ])
            ||> List.fold (fun (state, helperCalls, helperSites) index ->
                let readonlyPrefixed =
                    index > 0
                    && (
                        match fst ops.[index - 1] with
                        | IlOp.Nullary NullaryIlOp.Readonly -> true
                        | _ -> false
                    )

                let helped =
                    match fst ops.[index] with
                    | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Isinst, MetadataOperand.FromMetadata token) ->
                        Some (token, HelpedInstruction.Isinst)
                    | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Castclass, MetadataOperand.FromMetadata token) ->
                        Some (token, HelpedInstruction.Castclass)
                    | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Unbox_Any, MetadataOperand.FromMetadata token) ->
                        Some (token, HelpedInstruction.UnboxAny)
                    | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Unbox, MetadataOperand.FromMetadata token) ->
                        Some (token, HelpedInstruction.Unbox)
                    | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Box, MetadataOperand.FromMetadata token) ->
                        Some (token, HelpedInstruction.Box)
                    | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldelema, MetadataOperand.FromMetadata token) when
                        not readonlyPrefixed
                        ->
                        Some (token, HelpedInstruction.Ldelema)
                    | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Stelem, MetadataOperand.FromMetadata token) ->
                        Some (token, HelpedInstruction.Stelem)
                    | _ -> None

                match helped with
                | Some (token, instruction) when
                    executed.Contains (snd ops.[index])
                    && not (unbound.Contains index)
                    && not (elided.Contains index)
                    ->
                    let offset = snd ops.[index]

                    match spelledType state assembly token.Token with
                    | state, Some (spelling, spelledIn) ->
                        let state, helpers, byInstance =
                            helpersAt state (assemblyOf state spelledIn) instruction spelling

                        let called =
                            helpers
                            |> Seq.map (fun helper -> offset, runtimeRuns (jitHelperMethod state helper), [])
                            |> List.ofSeq

                        let helperSites =
                            if byInstance then
                                (offset,
                                 {
                                     Type = spelling
                                     Instruction = instruction
                                 })
                                :: helperSites
                            else
                                helperSites

                        state, called @ helperCalls, helperSites
                    // A token that does not bind, which fails before the instruction calls
                    // anything, or of a generic type's definition, which the JIT refuses.
                    | state, None -> state, helperCalls, helperSites
                | _ ->
                    match fst ops.[index] with
                    | IlOp.Nullary NullaryIlOp.Stelem_ref when
                        executed.Contains (snd ops.[index]) && not (elided.Contains index)
                        ->
                        let called =
                            snd ops.[index], runtimeRuns (jitHelperMethod state JitHelper.StelemRef), []

                        state, called :: helperCalls, helperSites
                    | _ -> state, helperCalls, helperSites
            )

        // Where the JIT boxes the receiver of a `constrained.` call, as `box` does
        // (`impTransformThis`, importercalls.cpp): where the type the prefix names is a value type
        // and the method that runs is no method the type declares (`CORINFO_BOX_THIS`,
        // `getCallInfo`, jitinterface.cpp), at every level of optimisation. Which method runs, an
        // instance decides, so `Calls` has a call of each `CastHelpers` method a `box` of the type
        // may call where a run may make the call, and `instanceFacts` keeps the one an instance's
        // box calls. It refuses to box a byref-like type, compiling the body, whatever a run would
        // reach, so every call it compiles is a site.
        let state, helperCalls, receiverBoxes =
            ((state, helperCalls, []), [ 0 .. ops.Length - 1 ])
            ||> List.fold (fun (state, helperCalls, receiverBoxes) index ->
                let op, offset = ops.[index]

                match op, Map.tryFind index targets, constrainedPrefix index with
                | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Callvirt, MetadataOperand.FromMetadata _),
                  Some (CallTarget.Method callee),
                  Some (prefix, constrainedType) when
                    compiled.Contains offset
                    && not (unbound.Contains index)
                    && not (unbound.Contains prefix)
                    && not (elided.Contains index)
                    && not (snd (methodOf state callee.Callee)).IsStatic
                    ->
                    match typeOperand state prefix with
                    | state, None -> state, helperCalls, receiverBoxes
                    | state, Some spelled ->
                        let state, helpers, _ =
                            helpersAt state (assemblyOf state spelled.SpelledIn) HelpedInstruction.Box spelled.Type

                        let state, byRefLike = mayBeByRefLike state spelled spelled.Type

                        if helpers.IsEmpty && not byRefLike then
                            state, helperCalls, receiverBoxes
                        else
                            let called =
                                if executed.Contains offset then
                                    helpers
                                    |> Seq.map (fun helper -> offset, runtimeRuns (jitHelperMethod state helper), [])
                                    |> List.ofSeq
                                else
                                    []

                            let site =
                                {
                                    Constrained = constrainedType
                                    Named = callee
                                    MayBeByRefLike = byRefLike
                                }

                            state, called @ helperCalls, (offset, site) :: receiverBoxes
                | _ -> state, helperCalls, receiverBoxes
            )

        let calls = stubs @ helperCalls @ calls

        let regions = List.ofSeq body.ExceptionRegions

        // Each `rethrow` belongs to the innermost handler, or filter, around it. Only a `catch`
        // or `filter` handler may hold one (ECMA-335 III.4.24), so one anywhere else is opaque.
        let opaque, rethrows =
            ((List.rev opaque, []), ops)
            ||> Array.fold (fun (opaque, rethrows) (op, offset) ->
                match op with
                | IlOp.Nullary NullaryIlOp.Rethrow when executed.Contains offset ->
                    let blocks =
                        regions
                        |> List.collect (fun region ->
                            let handlerOf (o : ExceptionOffset) =
                                o.HandlerOffset, o.HandlerLength, Some region

                            match region with
                            | ExceptionRegion.Catch (_, o) -> [ handlerOf o ]
                            | ExceptionRegion.Filter (filterOffset, o) ->
                                [ handlerOf o ; filterOffset, o.HandlerOffset - filterOffset, None ]
                            | ExceptionRegion.Finally o
                            | ExceptionRegion.Fault o -> [ o.HandlerOffset, o.HandlerLength, None ]
                        )
                        |> List.filter (fun (start, length, _) -> offset >= start && offset < start + length)

                    match blocks |> List.sortBy (fun (_, length, _) -> length) with
                    | (_, _, Some clause) :: _ -> opaque, (offset, clause) :: rethrows
                    | _ -> opaque @ [ offset, Opacity.Rethrow, None ], rethrows
                | _ -> opaque, rethrows
            )

        // Each `constrained. ldftn` of a static virtual a run can reach. One whose prefix fails to
        // bind is opaque, as a call's is.
        let opaque, pointers =
            ((opaque, []), [ 0 .. ops.Length - 1 ])
            ||> List.fold (fun (opaque, pointers) index ->
                let op, offset = ops.[index]

                match op, Map.tryFind index targets with
                | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldftn, MetadataOperand.FromMetadata _),
                  Some (CallTarget.Method callee) when executed.Contains offset && isStaticVirtual state callee.Callee ->
                    match constrainedPrefix index with
                    | Some (prefix, constrainedType) when not (unbound.Contains prefix) ->
                        opaque, (offset, constrainedType, callee) :: pointers
                    | Some _ -> opaque @ [ offset, Opacity.ConstrainedMethodPointer, Some callee.Callee ], pointers
                    | None -> opaque, pointers
                | _ -> opaque, pointers
            )

        // What each `ret` returns: the top of the stack there. A `jmp` returns whatever the
        // method it transfers to does, which this does not follow.
        let returnSites =
            ops
            |> Seq.choose (fun (op, offset) ->
                match op with
                | IlOp.Nullary NullaryIlOp.Ret -> Some (offset, valueAt offset 0)
                | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Jmp, _) -> Some (offset, StackValue.Unknown)
                | _ -> None
            )
            |> Map.ofSeq

        let returns = returnedOver returnSites executed

        let consults =
            let ofIntegers (integers : Set<SpelledInteger>) : int list =
                integers
                |> Seq.choose (fun integer ->
                    match integer with
                    | SpelledInteger.Argument index
                    | SpelledInteger.Loaded (index, _) -> Some index
                    | SpelledInteger.Constant _ -> None
                )
                |> List.ofSeq

            let ofValue (value : StackValue) : int list =
                match value with
                | StackValue.Objects objects ->
                    objects
                    |> Seq.choose (fun object ->
                        match object with
                        | SpelledObject.Argument (index, _) -> Some index
                        | _ -> None
                    )
                    |> List.ofSeq
                | StackValue.Integers integers -> ofIntegers integers
                | StackValue.Addresses _
                | StackValue.Unknown -> []

            [
                // A virtual call's receiver is among what it passes.
                for _, _, passes in calls do
                    for passed in passes do
                        match passed with
                        | SpelledArgument.Value value -> yield! ofValue value
                        | SpelledArgument.Pointer (_, holds) -> yield! ofIntegers holds

                for fact in outsideBody do
                    match fact with
                    | OutsideBodyFact.Runs (_, passes) ->
                        for passed in passes do
                            match passed with
                            | SpelledArgument.Value value -> yield! ofValue value
                            | SpelledArgument.Pointer (_, holds) -> yield! ofIntegers holds
                    | OutsideBodyFact.Raises _
                    | OutsideBodyFact.Opaque _ -> ()

                yield! ofValue returns

                for tested in tests.Values do
                    match tested with
                    | Tested.References objects -> yield! ofValue (StackValue.Objects objects)
                    | Tested.Integers integers -> yield! ofValue (StackValue.Integers integers)
                    | Tested.Types _ -> ()
            ]
            |> Set.ofList

        state,
        {
            Raises = List.rev raises
            Opaque = opaque
            // Were the JIT not to fold a query, or expand a span, its calls would run there.
            Assumed =
                Seq.append (typeQueries |> Map.keys) (createdSpans |> Seq.map (fun index -> snd ops.[index + 1]))
                |> Seq.filter executed.Contains
                |> Seq.map (fun offset -> offset, Assumption.JitOptimises)
                |> Seq.append (List.rev accessAssumed)
                |> List.ofSeq
            Relies = List.rev fieldAssumed
            Calls = List.rev calls
            LookedUp = List.rev lookedUp
            HelperSites = List.rev helperSites
            ReceiverBoxes = List.rev receiverBoxes
            Refusals = List.rev refusals
            ConstrainedPointers = List.rev pointers
            Rethrows = List.rev rethrows
            Regions = regions
            InstanceChecks = instanceChecks
            OutsideBody = outsideBody
            RaisedWhereInlined = raisedWhereInlined
            Spellings = spellings
            Returns = returns
            Stored = stored
            Consults = consults
            Branching =
                Some
                    {
                        Body = body
                        Constants = constants
                        Tests = tests
                        Returns = returnSites
                    }
        }

    /// How deeply an instance's type arguments may nest. A generic method can call itself at a
    /// deeper instantiation each time, so the instances reachable from one call are unbounded; one
    /// nested deeper than this is analysed as the open definition instead.
    let private instantiationDepthLimit : int = 8

    /// The type arguments a call's token spells, over the calling body's type variables.
    let rec private spelledArguments (assembly : DumpedAssembly) (token : MetadataToken) : TypeDefn list =
        match token with
        | MetadataToken.MethodSpecification handle ->
            let spec = assembly.MethodSpecs.[handle]
            List.ofSeq spec.Signature @ spelledArguments assembly spec.Method
        | MetadataToken.MemberReference handle ->
            match assembly.Members.[handle].Parent with
            | MetadataToken.TypeSpecification parent -> [ assembly.TypeSpecs.[parent].Signature ]
            | _ -> []
        | _ -> []

    /// How deeply a type's arguments nest.
    let rec private depthOf (state : EscapeAnalysisState) (handle : ConcreteTypeHandle) : int =
        match handle with
        | ConcreteTypeHandle.Concrete _ ->
            match AllConcreteTypes.lookup handle state.TypeSystem.ConcreteTypes with
            | Some ty -> 1 + (ty.Generics |> Seq.map (depthOf state) |> Seq.fold max 0)
            | None -> failwith $"Concrete type handle %O{handle} is not registered"
        | ConcreteTypeHandle.Byref element
        | ConcreteTypeHandle.Pointer element
        | ConcreteTypeHandle.OneDimArrayZero element
        | ConcreteTypeHandle.Array (element, _) -> 1 + depthOf state element
        | ConcreteTypeHandle.FunctionPointer signature ->
            let returned =
                match signature.ReturnType with
                | MethodReturnType.Void -> []
                | MethodReturnType.Returns ty -> [ ty ]

            1
            + (returned @ signature.ParameterTypes
               |> List.map (depthOf state)
               |> List.fold max 0)

    /// The instance of `definition` these arguments make, or its open definition if they nest
    /// deeper than `instantiationDepthLimit`.
    let private instanceOf
        (state : EscapeAnalysisState)
        (definition : MethodKey)
        (typeArguments : ConcreteTypeHandle list)
        (methodArguments : ConcreteTypeHandle list)
        : MethodInstance
        =
        let arguments =
            if
                typeArguments @ methodArguments
                |> List.exists (fun argument -> depthOf state argument > instantiationDepthLimit)
            then
                Instantiation.Open
            else
                Instantiation.Closed (typeArguments, methodArguments)

        {
            Definition = definition
            Arguments = arguments
            Passed = Map.empty
        }

    /// A type a body of `assembly` spells, as that body's instantiation makes it.
    let private concretize
        (state : EscapeAnalysisState)
        (assembly : string)
        (typeArguments : ConcreteTypeHandle list)
        (methodArguments : ConcreteTypeHandle list)
        (spelling : TypeDefn)
        : EscapeAnalysisState * ConcreteTypeHandle
        =
        let typeSystem, handle =
            TypeSystemState.concretizeType
                state.LoggerFactory
                state.RuntimeDirs
                state.BaseTypes
                state.TypeSystem
                assembly
                (ImmutableArray.CreateRange typeArguments)
                (ImmutableArray.CreateRange methodArguments)
                spelling

        { state with
            TypeSystem = typeSystem
        },
        match handle with
        | Ok handle -> handle
        | Error failure -> failwith (TypeBindingFailure.describe failure)

    /// The arguments of the declaring type and of the method that a call's token, of a body of
    /// `assembly` whose instantiation is `typeArguments` and `methodArguments`, instantiates its
    /// callee with: a MemberRef's as the interpreter binds them
    /// (`MemberReferenceInstantiation.resolveMember`), and a MethodSpec's own.
    let rec private spelledInstantiation
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (typeArguments : ConcreteTypeHandle list)
        (methodArguments : ConcreteTypeHandle list)
        (token : MetadataToken)
        : EscapeAnalysisState * ConcreteTypeHandle list * ConcreteTypeHandle list
        =
        match token with
        | MetadataToken.MethodSpecification handle ->
            let spec = assembly.MethodSpecs.[handle]

            let state, calleeMethodArguments =
                ((state, []), spec.Signature)
                ||> Seq.fold (fun (state, handles) argument ->
                    let state, handle =
                        concretize state assembly.DefinitionFullName typeArguments methodArguments argument

                    state, handle :: handles
                )

            let state, calleeTypeArguments, _ =
                match spec.Method with
                | MetadataToken.MethodDef _ -> state, [], []
                | inner -> spelledInstantiation state assembly typeArguments methodArguments inner

            state, calleeTypeArguments, List.rev calleeMethodArguments
        | MetadataToken.MemberReference handle ->
            let typeSystem, declaringTypeArguments =
                match
                    MemberReferenceInstantiation.resolveMember
                        state.LoggerFactory
                        state.RuntimeDirs
                        state.BaseTypes
                        assembly
                        (ImmutableArray.CreateRange typeArguments)
                        (ImmutableArray.CreateRange methodArguments)
                        handle
                        state.TypeSystem
                with
                | typeSystem, Ok resolved -> typeSystem, resolved.TargetTypeGenerics
                | _, Error failure -> failwith (MemberReferenceInstantiation.describeFailure assembly handle failure)

            let state =
                { state with
                    TypeSystem = typeSystem
                }

            // Closed already: the interpreter concretises them in the calling assembly with no
            // context (`ExecutionConcretization.concretizeMethodForExecution`), as here.
            let state, calleeTypeArguments =
                ((state, []), declaringTypeArguments)
                ||> Seq.fold (fun (state, handles) argument ->
                    let state, handle = concretize state assembly.DefinitionFullName [] [] argument
                    state, handle :: handles
                )

            state, List.rev calleeTypeArguments, []
        | other -> failwith $"A call in %s{assembly.DefinitionFullName} spells an instantiation with %O{other}"

    /// The instance a call of `callee` from a body of `assembly` reaches, when that body runs as
    /// `caller`.
    let private calleeInstance
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (caller : Instantiation)
        (callee : Callee)
        : EscapeAnalysisState * MethodInstance
        =
        let unknown : MethodInstance =
            {
                Definition = callee.Callee
                Arguments = Instantiation.Open
                Passed = Map.empty
            }

        match callee.Spelling with
        | CalleeSpelling.Fixed -> state, instanceOf state callee.Callee [] []
        | CalleeSpelling.Typical -> state, unknown
        | CalleeSpelling.Spelled token ->
            let context =
                match caller with
                | Instantiation.Closed (typeArguments, methodArguments) -> Some (typeArguments, methodArguments)
                | Instantiation.Open ->
                    if spelledArguments assembly token |> List.exists mentionsTypeVariable then
                        None
                    else
                        Some ([], [])

            match context with
            | None -> state, unknown
            | Some (typeArguments, methodArguments) ->
                let state, calleeTypeArguments, calleeMethodArguments =
                    spelledInstantiation state assembly typeArguments methodArguments token

                state, instanceOf state callee.Callee calleeTypeArguments calleeMethodArguments

    /// Whether everything dispatch reads of a receiver of the type `identity` binds: its base types,
    /// the interfaces any of them implements, and the signature of every method of each, which
    /// dispatch compares to find what implements a method. It reads no method's locals.
    let rec private dispatchBinds
        (state : EscapeAnalysisState)
        (identity : ResolvedTypeIdentity)
        : EscapeAnalysisState * bool
        =
        match state.DispatchBinds.TryFind identity with
        | Some known -> state, known
        | None ->

        let assembly, ty = definitionOf state identity

        // A type a spelling names, if the spelling binds.
        let named (state : EscapeAnalysisState) (spelling : TypeDefn) : EscapeAnalysisState * bool =
            match concretizable state assembly spelling with
            | state, false -> state, false
            | state, true ->
                match nominalIdentity state assembly spelling with
                | state, Some related -> dispatchBinds state related
                | state, None -> state, true

        let typeToken (state : EscapeAnalysisState) (token : MetadataToken) : EscapeAnalysisState * bool =
            match token with
            | MetadataToken.TypeDefinition handle -> dispatchBinds state assembly.TypeDefs.[handle].Identity
            | MetadataToken.TypeReference handle ->
                match resolveTypeRef state assembly assembly.TypeRefs.[handle] with
                | state, Some related -> dispatchBinds state related
                | state, None -> state, false
            | MetadataToken.TypeSpecification handle -> named state assembly.TypeSpecs.[handle].Signature
            | _ -> state, false

        let methodBinds
            (state : EscapeAnalysisState)
            (method : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
            : EscapeAnalysisState * bool
            =
            ((state, true), signatureTypes method.Signature)
            ||> List.fold (fun (state, soFar) spelling ->
                if soFar then
                    concretizable state assembly spelling
                else
                    state, false
            )

        let checks : (EscapeAnalysisState -> EscapeAnalysisState * bool) list =
            [
                match ty.BaseType with
                | None -> ()
                | Some (BaseTypeInfo.TypeDef handle) ->
                    yield fun state -> typeToken state (MetadataToken.TypeDefinition handle)
                | Some (BaseTypeInfo.TypeRef handle) ->
                    yield fun state -> typeToken state (MetadataToken.TypeReference handle)
                | Some (BaseTypeInfo.TypeSpec handle) ->
                    yield fun state -> typeToken state (MetadataToken.TypeSpecification handle)
                for implemented in ty.ImplementedInterfaces do
                    yield fun state -> typeToken state implemented.InterfaceHandle
                for method in ty.Methods do
                    yield fun state -> methodBinds state method
            ]

        let state, binds =
            ((state, true), checks)
            ||> List.fold (fun (state, soFar) check -> if soFar then check state else state, false)

        { state with
            DispatchBinds = state.DispatchBinds.Add (identity, binds)
        },
        binds

    /// A type a body spells, as an instance of the body running as `caller` makes it; `spelledIn`
    /// names the assembly whose metadata spells it. `None` where the instance leaves a type
    /// variable of it open; where the type fails to bind, which fails that body before it runs, as
    /// its facts report; and where TypeSystem does not instantiate it (`concretizable`): where a type
    /// it names derives from one that does not bind, which CoreCLR cannot load either, or where
    /// TypeSystem does not model the shape, although CoreCLR may load it.
    let private concretizeIn
        (state : EscapeAnalysisState)
        (caller : Instantiation)
        (spelling : TypeDefn)
        (spelledIn : string)
        : EscapeAnalysisState * ConcreteTypeHandle option
        =
        let context =
            match caller with
            | Instantiation.Closed (typeArguments, methodArguments) -> Some (typeArguments, methodArguments)
            | Instantiation.Open ->
                if mentionsTypeVariable spelling then
                    None
                else
                    Some ([], [])

        match context with
        | None -> state, None
        | Some (typeArguments, methodArguments) ->

        match concretizable state (assemblyOf state spelledIn) spelling with
        | state, false -> state, None
        | state, true ->
            let state, handle =
                concretize state spelledIn typeArguments methodArguments spelling

            state, Some handle

    /// What a call of `callee` does, named by a body of `assembly` running as `caller`, dispatched
    /// on a receiver of the type `receiver`. The type decides it where the receiver's class is
    /// exactly that type (`exact`), and otherwise for a value type and for a sealed class, as for a
    /// `constrained.` prefix naming it (ECMA-335 III.2.1). Undecided where an instance of a derived
    /// class may receive the call.
    let private dispatchOn
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (caller : Instantiation)
        (receiver : ConcreteTypeHandle)
        (exact : bool)
        (callee : Callee)
        : EscapeAnalysisState * DispatchOutcome
        =
        let state, named = calleeInstance state assembly caller callee

        match named.Arguments with
        | Instantiation.Open -> state, DispatchOutcome.Undecided
        | Instantiation.Closed (namedTypeArguments, namedMethodArguments) ->

        let _, definition = methodOf state named.Definition

        // Instantiating the method the call names reads its locals, which the JIT does not unless
        // that method runs.
        let namedLocals =
            match definition.Body with
            | MethodBody.Il body -> body.LocalVars |> Option.map List.ofSeq |> Option.defaultValue []
            | _ -> []

        let state, binds =
            match TypeSystemState.tryGetConcreteTypeInfo state.TypeSystem receiver with
            | None -> state, true
            | Some (_, receiverType) ->
                match dispatchBinds state receiverType.Identity with
                | state, true -> dispatchBinds state definition.RequiredDeclaringType.Identity
                | state, false -> state, false

        let state, binds =
            if binds then
                let definitionAssembly = assemblyOf state definition.DeclaringAssemblyFullName

                ((state, true), namedLocals)
                ||> List.fold (fun (state, soFar) spelling ->
                    if soFar then
                        concretizable state definitionAssembly spelling
                    else
                        state, false
                )
            else
                state, false

        // It builds every type of the method's signature too, which fails where one does not load.
        // The JIT loads a parameter's class only where the method that runs is compiled, and so
        // which method that is, and what its compilation raises, is left undecided here.
        let state, binds =
            if binds then
                let state, failure =
                    firstLoadFailure
                        state
                        (assemblyOf state definition.DeclaringAssemblyFullName)
                        (signatureTypes definition.Signature)

                state, failure.IsNone
            else
                state, false

        if not binds then
            state, DispatchOutcome.Undecided
        else

        let typeSystem, concretized, declaringType =
            match
                MethodConcretisation.concretizeMethodWithAllGenerics
                    state.LoggerFactory
                    state.RuntimeDirs
                    state.BaseTypes
                    (ImmutableArray.CreateRange namedTypeArguments)
                    definition
                    (ImmutableArray.CreateRange namedMethodArguments)
                    state.TypeSystem
            with
            | typeSystem, Ok (concretized, declaringType) -> typeSystem, concretized, declaringType
            | _, Error failure -> failwith (TypeBindingFailure.describe failure)

        let implementationOn
            (walkBaseTypes : bool)
            (typeSystem : TypeSystemState)
            : TypeSystemState * VirtualImplementation
            =
            match
                ConcreteVirtualDispatch.tryResolveVirtualImplementation
                    state.LoggerFactory
                    state.RuntimeDirs
                    state.BaseTypes
                    concretized.Generics
                    concretized
                    receiver
                    walkBaseTypes
                    typeSystem
            with
            | typeSystem, Ok implementation -> typeSystem, implementation
            | _, Error failure -> failwith (TypeBindingFailure.describe failure)

        // What `callvirt` runs on a receiver of exactly the type `receiver`: an override, or else
        // the method the call names.
        let dispatchedOn (typeSystem : TypeSystemState) : TypeSystemState * VirtualImplementation =
            match implementationOn true typeSystem with
            | typeSystem, VirtualImplementation.NotOverridden ->
                let named : DispatchedMethod =
                    {
                        Definition = definition
                        TypeGenerics = ImmutableArray.CreateRange namedTypeArguments
                        MethodGenerics = ImmutableArray.CreateRange namedMethodArguments
                    }

                typeSystem, VirtualImplementation.Found named
            | decided -> decided

        // A type that is not one the method's declaring type admits leaves what runs to the
        // receiver's class: one implementing `IDynamicInterfaceCastable` is asked for it.
        let typeSystem, admitted =
            match
                TypeAssignability.isConcreteTypeAssignableTo
                    state.LoggerFactory
                    state.RuntimeDirs
                    state.BaseTypes
                    typeSystem
                    receiver
                    declaringType
            with
            | typeSystem, Ok admitted -> typeSystem, admitted
            | _, Error failure -> failwith (TypeBindingFailure.describe failure)

        // `None` where the type does not decide what runs.
        let typeSystem, runs =
            match TypeSystemState.tryGetConcreteTypeInfo typeSystem receiver with
            | _ when not admitted -> typeSystem, None
            // An array may be of a covariant derived element type.
            | None -> typeSystem, None
            | Some (_, receiverType) ->
                if definition.IsStatic then
                    // No object receives the call, so the type decides it, however it is derived
                    // from.
                    match
                        StaticVirtualDispatch.resolve
                            state.LoggerFactory
                            state.RuntimeDirs
                            state.BaseTypes
                            receiver
                            concretized
                            typeSystem
                    with
                    | _, Error failure -> failwith (TypeBindingFailure.describe failure)
                    | typeSystem, Ok (VirtualImplementation.Found runs) when not runs.Definition.IsStatic ->
                        typeSystem, None
                    | typeSystem, Ok decided -> typeSystem, Some decided
                elif LoadedTypeInfo.isValueType state.BaseTypes typeSystem._LoadedAssemblies receiverType then
                    match implementationOn false typeSystem with
                    | typeSystem, VirtualImplementation.NotOverridden ->
                        // A value type that does not implement the method itself is boxed, and the
                        // call dispatched on the box (ECMA-335 III.2.1): to a method it inherits from
                        // Object, ValueType or Enum, or to an interface's default body. A boxed
                        // Nullable is its underlying value or null, not a Nullable.
                        if receiverType.Identity <> state.BaseTypes.Nullable.Identity then
                            let typeSystem, decided = dispatchedOn typeSystem
                            typeSystem, Some decided
                        else
                            typeSystem, None
                    | typeSystem, decided -> typeSystem, Some decided
                elif exact || receiverType.TypeAttributes.HasFlag TypeAttributes.Sealed then
                    let typeSystem, decided = dispatchedOn typeSystem
                    typeSystem, Some decided
                else
                    typeSystem, None

        let state =
            { state with
                TypeSystem = typeSystem
            }

        match runs with
        | None
        | Some VirtualImplementation.NotOverridden
        | Some (VirtualImplementation.Unmodelled _) -> state, DispatchOutcome.Undecided
        | Some (VirtualImplementation.Ambiguous _) ->
            state, DispatchOutcome.Raises (constructedRaise state "System.Runtime.AmbiguousImplementationException")
        | Some (VirtualImplementation.Reabstracted _) ->
            state, DispatchOutcome.Raises (constructedRaise state "System.EntryPointNotFoundException")
        // As `DispatchFailure.NotImplemented` and `DispatchFailure.StaticNotImplemented` say.
        | Some VirtualImplementation.NotImplemented when definition.IsStatic ->
            state, DispatchOutcome.Raises (constructedRaise state "System.Runtime.AmbiguousImplementationException")
        | Some VirtualImplementation.NotImplemented ->
            state, DispatchOutcome.Raises (constructedRaise state "System.EntryPointNotFoundException")
        | Some (VirtualImplementation.Found runs) ->
            match runs.Definition.TryMetadata with
            | None -> state, DispatchOutcome.Undecided
            | Some facts ->
                let key =
                    MethodKey.make (assemblyOf state runs.Definition.DeclaringAssemblyFullName) facts.Handle

                state,
                DispatchOutcome.Reaches (
                    instanceOf state key (List.ofSeq runs.TypeGenerics) (List.ofSeq runs.MethodGenerics)
                )

    /// The type CoreCLR resolves a `constrained.` call of `callee` against, where the prefix names
    /// a token of `assembly`, when the body making the call runs as `caller`, with the method it
    /// dispatches there: the type the prefix names and `callee`, or the underlying primitive of an
    /// enum it names and what CoreCLR dispatches on that (`ConstrainedType.resolve`). `None` where
    /// only an instance decides it.
    let private constrainedReceiver
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (caller : Instantiation)
        (constrainedType : MetadataToken)
        (callee : Callee)
        : EscapeAnalysisState * (ConcreteTypeHandle * Callee) option
        =
        let typeSystem, spelling, spellingAssembly =
            match
                TypeSystemState.resolveTypeMetadataToken
                    state.LoggerFactory
                    state.RuntimeDirs
                    state.BaseTypes
                    state.TypeSystem
                    assembly
                    constrainedType
            with
            | typeSystem, Ok (spelling, spellingAssembly) -> typeSystem, spelling, spellingAssembly
            | _, Error failure -> failwith (TypeBindingFailure.describe failure)

        let state =
            { state with
                TypeSystem = typeSystem
            }

        match concretizeIn state caller spelling spellingAssembly.DefinitionFullName with
        | state, None -> state, None
        | state, Some named ->
            let _, called = methodOf state callee.Callee

            let typeSystem, resolved =
                ConstrainedType.resolve
                    state.LoggerFactory
                    state.RuntimeDirs
                    state.BaseTypes
                    state.TypeSystem
                    named
                    called

            let state =
                { state with
                    TypeSystem = typeSystem
                }

            let resolved =
                match resolved with
                | Ok resolved -> resolved
                | Error failure -> failwith (TypeBindingFailure.describe failure)

            match resolved with
            | ConstrainedType.AsNamed receiver
            | ConstrainedType.EnumUnderlying (_, receiver, RetargetedCall.Named) -> state, Some (receiver, callee)
            | ConstrainedType.EnumUnderlying (_, receiver, RetargetedCall.ObjectGetHashCode) ->
                let getHashCode = ConstrainedType.objectGetHashCode state.BaseTypes

                let dispatched : Callee =
                    {
                        Callee =
                            MethodKey.make
                                (assemblyOf state getHashCode.DeclaringAssemblyFullName)
                                (MethodInfo.requireMetadata "Object.GetHashCode" getHashCode).Handle
                        Spelling = CalleeSpelling.Fixed
                    }

                state, Some (receiver, dispatched)
            | ConstrainedType.Undecided _ -> state, None

    /// What a `constrained.` call of `callee`, a class's method that cannot be overridden, does on
    /// `receiver`, the type CoreCLR resolves it against, when the body of `assembly` making it runs as
    /// `caller`. On a reference type, it calls the method named on the reference the receiver's
    /// address holds. On a value type, it calls what `ConstrainedType.resolveClassMethodOnValueType`
    /// resolves it to on the receiver's address, or else the method named on a box of it, which
    /// that method's class may not be one of.
    let private notOverridableOn
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (caller : Instantiation)
        (receiver : ConcreteTypeHandle)
        (callee : Callee)
        : EscapeAnalysisState * DispatchOutcome
        =
        let state, named = calleeInstance state assembly caller callee

        match named.Arguments with
        | Instantiation.Open -> state, DispatchOutcome.Undecided
        | Instantiation.Closed (namedTypeArguments, namedMethodArguments) ->

        if TypeSystemState.isReferenceTypeHandle state.BaseTypes "EscapeAnalysis" state.TypeSystem receiver then
            state, DispatchOutcome.Reaches named
        else

        match TypeSystemState.tryGetConcreteTypeInfo state.TypeSystem receiver with
        | None -> state, DispatchOutcome.Undecided
        | Some (concrete, _) ->

        let receiverGenerics = List.ofSeq concrete.Generics

        // Is the method named one of the receiver's own instantiation, or of a class it derives from?
        // A value type's bases (`ValueType`, `Enum`, `Object`) are not generic, so their identities
        // say so.
        let rec derivesFrom (state : EscapeAnalysisState) (identity : ResolvedTypeIdentity) =
            if identity = declaringTypeOf state callee.Callee then
                state, (identity <> concrete.Identity || namedTypeArguments = receiverGenerics)
            else
                match baseOf state identity with
                | state, Some parent -> derivesFrom state parent
                | state, None -> state, false

        let _, method = methodOf state callee.Callee

        let typeSystem, resolved =
            ConstrainedType.resolveClassMethodOnValueType
                state.LoggerFactory
                state.RuntimeDirs
                state.BaseTypes
                state.TypeSystem
                receiver
                method

        let state =
            { state with
                TypeSystem = typeSystem
            }

        let resolved =
            match resolved with
            | Ok resolved -> resolved
            | Error failure -> failwith (TypeBindingFailure.describe failure)

        match resolved with
        | ClassMethodOnValueType.Undecided -> state, DispatchOutcome.Undecided
        | ClassMethodOnValueType.Resolves runs ->
            match runs.TryMetadata with
            | None -> state, DispatchOutcome.Undecided
            | Some facts ->
                let key =
                    MethodKey.make (assemblyOf state runs.DeclaringAssemblyFullName) facts.Handle

                if key = callee.Callee then
                    // The method named is a value type's, which is the receiver's only if the
                    // receiver is of that type.
                    match derivesFrom state concrete.Identity with
                    | state, true -> state, DispatchOutcome.Reaches named
                    | state, false -> state, DispatchOutcome.ReachesMistyped named
                else
                    // The receiver's own method, in the slot of the method named.
                    state, DispatchOutcome.Reaches (instanceOf state key receiverGenerics namedMethodArguments)
        | ClassMethodOnValueType.Boxes when concrete.Identity = state.BaseTypes.Nullable.Identity ->
            // A boxed Nullable is its underlying value or null, not a Nullable.
            state, DispatchOutcome.Undecided
        | ClassMethodOnValueType.Boxes ->
            match derivesFrom state concrete.Identity with
            | state, true -> state, DispatchOutcome.Reaches named
            | state, false -> state, DispatchOutcome.ReachesMistyped named

    /// What a `constrained.` call does, when the body of `assembly` making it runs as `caller`, if
    /// the type CoreCLR resolves it against decides it (`constrainedReceiver`, `dispatchOn`,
    /// `notOverridableOn`).
    let private constrainedInstance
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (caller : Instantiation)
        (constrainedType : MetadataToken)
        (callee : Callee)
        : EscapeAnalysisState * DispatchOutcome
        =
        match constrainedReceiver state assembly caller constrainedType callee with
        | state, Some (receiver, dispatched) when
            isStaticVirtual state dispatched.Callee || isOverridable state dispatched.Callee
            ->
            dispatchOn state assembly caller receiver false dispatched
        | state, Some (receiver, dispatched) -> notOverridableOn state assembly caller receiver dispatched
        | state, None -> state, DispatchOutcome.Undecided

    /// Whether the JIT boxes the receiver of `site`, a `constrained.` call the body of `assembly`
    /// makes running as `caller`. It does where the type CoreCLR resolves the call against
    /// (`constrainedReceiver`: an enum's underlying primitive, where it retargets the call) is a
    /// value type and the method the call runs is no method that type declares
    /// (`TryResolveConstraintMethodApprox`, methodtable.cpp, finds none): one it inherits from a
    /// class, a default interface body, or one that is not virtual, such as `Object.GetType`
    /// (`getCallInfo`, jitinterface.cpp). It boxes as `box` does. Where dispatch fails, it boxes
    /// before the call raises.
    let private receiverBox
        (state : EscapeAnalysisState)
        (assembly : DumpedAssembly)
        (caller : Instantiation)
        (site : ReceiverBoxSite)
        : EscapeAnalysisState * ReceiverBox
        =
        match constrainedReceiver state assembly caller site.Constrained site.Named with
        | state, None -> state, ReceiverBox.Undecided
        | state, Some (receiver, dispatched) ->

        match AllConcreteTypes.lookup receiver state.TypeSystem.ConcreteTypes with
        | _ when TypeSystemState.isReferenceTypeHandle state.BaseTypes "EscapeAnalysis" state.TypeSystem receiver ->
            state, ReceiverBox.Unboxed
        | None -> state, ReceiverBox.Undecided
        | Some concrete ->

        let named = dispatched.Callee
        let _, method = methodOf state named

        let state, declared =
            if not method.IsVirtual then
                state, Some false
            elif not (isOverridable state named) then
                let _, declaring = definitionOf state (declaringTypeOf state named)

                if declaring.TypeAttributes.HasFlag TypeAttributes.Interface then
                    state, Some (declaringTypeOf state named = concrete.Identity)
                else
                    let typeSystem, resolved =
                        ConstrainedType.resolveClassMethodOnValueType
                            state.LoggerFactory
                            state.RuntimeDirs
                            state.BaseTypes
                            state.TypeSystem
                            receiver
                            method

                    let state =
                        { state with
                            TypeSystem = typeSystem
                        }

                    let resolved =
                        match resolved with
                        | Ok resolved -> resolved
                        | Error failure -> failwith (TypeBindingFailure.describe failure)

                    match resolved with
                    | ClassMethodOnValueType.Resolves _ -> state, Some true
                    | ClassMethodOnValueType.Boxes -> state, Some false
                    | ClassMethodOnValueType.Undecided -> state, None
            else
                match dispatchOn state assembly caller receiver false dispatched with
                | state, DispatchOutcome.Reaches reached ->
                    state, Some (declaringTypeOf state reached.Definition = concrete.Identity)
                | state, DispatchOutcome.Raises _ -> state, Some false
                | state, DispatchOutcome.Undecided -> state, None
                | _, DispatchOutcome.ReachesMistyped reached ->
                    failwith
                        $"BUG: dispatching %O{named} on %O{receiver} reached %O{reached.Definition} on a mistyped receiver, which only a method that cannot be overridden does"

        match declared with
        | None -> state, ReceiverBox.Undecided
        | Some true -> state, ReceiverBox.Unboxed
        | Some false ->
            let _, definition = definitionOf state concrete.Identity

            if definition.HasIsByRefLikeAttribute then
                state, ReceiverBox.Refused
            elif concrete.Identity = state.BaseTypes.Nullable.Identity then
                state, ReceiverBox.Boxed (Some JitHelper.BoxNullable)
            else
                match boxedWithoutHelper state concrete.Identity with
                | state, true -> state, ReceiverBox.Boxed None
                | state, false -> state, ReceiverBox.Boxed (Some JitHelper.Box)

    /// The facts of a method definition, read once.
    let private factsFor (state : EscapeAnalysisState) (key : MethodKey) : EscapeAnalysisState * LocalFacts =
        match state.Facts.TryFind key with
        | Some facts -> state, facts
        | None ->
            let assembly = assemblyOf state key.AssemblyFullName
            let state, facts = factsOfRuns state key (runsFor state.Assumptions assembly key)

            { state with
                Facts = state.Facts.Add (key, facts)
            },
            facts

    /// Do the classes `identity` derives from, transitively, bind, which `LoadedTypeInfo.isValueType`
    /// reads? Loads the assemblies they need. Where one does not bind, CoreCLR cannot load the type
    /// `identity` names.
    let private basesBind (state : EscapeAnalysisState) (identity : ResolvedTypeIdentity) : EscapeAnalysisState * bool =
        let rec go
            (state : EscapeAnalysisState)
            (seen : Set<ResolvedTypeIdentity>)
            (identity : ResolvedTypeIdentity)
            : EscapeAnalysisState * bool
            =
            if seen.Contains identity then
                state, true
            else
                let assembly, ty = definitionOf state identity

                let state, binds =
                    match ty.BaseType with
                    | None
                    | Some (BaseTypeInfo.TypeDef _) -> state, true
                    | Some (BaseTypeInfo.TypeRef handle) ->
                        let state, failure =
                            spellingBindFailure
                                state
                                assembly
                                (TypeDefn.FromReference (assembly.TypeRefs.[handle], SignatureTypeKind.Unknown))

                        state, failure.IsNone
                    | Some (BaseTypeInfo.TypeSpec handle) ->
                        concretizable state assembly assembly.TypeSpecs.[handle].Signature

                if not binds then
                    state, false
                else
                    match baseOf state identity with
                    | state, Some parent -> go state (seen.Add identity) parent
                    | state, None -> state, true

        go state Set.empty identity

    /// Does every type that `GcReferences.containsReferences` reads for `handle` load: each type it
    /// walks, with the types it derives from, and the type of each instance field of each value type
    /// among them, transitively? Loads the assemblies they need, which the walk itself does not.
    /// Where one does not load, CoreCLR cannot load `handle`, so no instance whose arguments name it
    /// runs.
    let private containsReferencesLoads
        (state : EscapeAnalysisState)
        (handle : ConcreteTypeHandle)
        : EscapeAnalysisState * bool
        =
        // Each type is checked once, however many fields reach it, and a type already being checked
        // is not checked again from within itself.
        let rec walk
            (state : EscapeAnalysisState, checkedSoFar : Set<ConcreteTypeHandle>)
            (handle : ConcreteTypeHandle)
            : EscapeAnalysisState * Set<ConcreteTypeHandle> * bool
            =
            match handle with
            | ConcreteTypeHandle.Concrete _ when not (checkedSoFar.Contains handle) ->
                let checkedSoFar = checkedSoFar.Add handle

                match TypeSystemState.tryGetConcreteTypeInfo state.TypeSystem handle with
                | None -> failwith $"Concrete type handle %O{handle} is not registered"
                | Some (concrete, ty) ->

                match basesBind state ty.Identity with
                | state, false -> state, checkedSoFar, false
                | state, true when
                    not (LoadedTypeInfo.isValueType state.BaseTypes state.TypeSystem._LoadedAssemblies ty)
                    ->
                    state, checkedSoFar, true
                | state, true ->
                    let assembly = assemblyOf state concrete.AssemblyFullName

                    ((state, checkedSoFar, true), ty.Fields)
                    ||> List.fold (fun (state, checkedSoFar, soFar) field ->
                        if not soFar || field.IsStatic then
                            state, checkedSoFar, soFar
                        else
                            match concretizable state assembly field.Signature with
                            | state, false -> state, checkedSoFar, false
                            | state, true ->
                                let state, fieldType =
                                    concretize
                                        state
                                        concrete.AssemblyFullName
                                        (List.ofSeq concrete.Generics)
                                        []
                                        field.Signature

                                walk (state, checkedSoFar) fieldType
                    )
            // The walk reads nothing of an array, a byref or a pointer.
            | _ -> state, checkedSoFar, true

        let state, _, loads = walk (state, Set.empty) handle
        state, loads

    /// The answer to `query` in `instance`; `None` where the instance does not decide its types,
    /// where whether a type is a value type is asked of one that is not a class or struct, or of one
    /// whose base classes do not bind (`basesBind`), or where
    /// whether a type holds references is asked of one CoreCLR cannot load (`containsReferencesLoads`).
    let private typeQueryIsKnown
        (state : EscapeAnalysisState)
        (instance : MethodInstance)
        (query : TypeQuery)
        : EscapeAnalysisState * bool option
        =
        let concretized (state : EscapeAnalysisState) (spelled : SpelledType) =
            concretizeIn state instance.Arguments spelled.Type spelled.SpelledIn

        // Two handles with no function pointer type in them, type arguments included, are the same
        // handle exactly where they are the same type. Types with one are left undecided: which
        // parts of a function pointer's signature CoreCLR's type identity tells apart is not
        // modelled here.
        let rec named (state : EscapeAnalysisState) (handle : ConcreteTypeHandle) : bool =
            match handle with
            | ConcreteTypeHandle.Concrete _ ->
                match TypeSystemState.tryGetConcreteTypeInfo state.TypeSystem handle with
                | Some (concrete, _) -> Seq.forall (named state) concrete.Generics
                | None -> failwith $"Concrete type handle %O{handle} is not registered"
            | ConcreteTypeHandle.Byref inner
            | ConcreteTypeHandle.Pointer inner
            | ConcreteTypeHandle.OneDimArrayZero inner
            | ConcreteTypeHandle.Array (inner, _) -> named state inner
            | ConcreteTypeHandle.FunctionPointer _ -> false

        match query with
        | TypeQuery.IsValueType spelled ->
            match concretized state spelled with
            | state, Some (ConcreteTypeHandle.Concrete _ as handle) ->
                match TypeSystemState.tryGetConcreteTypeInfo state.TypeSystem handle with
                | Some (_, ty) ->
                    match basesBind state ty.Identity with
                    | state, false -> state, None
                    | state, true ->
                        state, Some (LoadedTypeInfo.isValueType state.BaseTypes state.TypeSystem._LoadedAssemblies ty)
                | None -> failwith $"Concrete type handle %O{handle} is not registered"
            | state, _ -> state, None
        | TypeQuery.Identity (first, second, equality) ->
            let state, first = concretized state first
            let state, second = concretized state second

            match first, second with
            | Some first, Some second when List.forall (named state) [ first ; second ] ->
                state, Some ((first = second) = equality)
            | _ -> state, None
        | TypeQuery.ContainsReferences spelled ->
            match concretized state spelled with
            | state, Some handle ->
                match containsReferencesLoads state handle with
                | state, false -> state, None
                | state, true ->

                let typeSystem, holds =
                    match
                        GcReferences.containsReferences
                            state.LoggerFactory
                            state.RuntimeDirs
                            state.BaseTypes
                            state.TypeSystem
                            handle
                    with
                    | typeSystem, Ok holds -> typeSystem, holds
                    | _, Error failure -> failwith (TypeBindingFailure.describe failure)

                { state with
                    TypeSystem = typeSystem
                },
                Some holds
            | state, None -> state, None

    /// The `CastHelpers` method, if any, that `site`, an instruction of `instance` naming a type
    /// variable, or a cast to a `Nullable<T>` over one, calls, given the declaring type's arguments,
    /// `typeArguments`, and the method's, `methodArguments`. Shared code names the canonical form
    /// of the type, where a reference type is `__Canon`; CoreCLR asks about a `Nullable<T>` as `T`
    /// only where it is not shared (`impCastClassOrIsInstToTree`).
    let rec private instanceHelper
        (state : EscapeAnalysisState)
        (typeArguments : ConcreteTypeHandle list)
        (methodArguments : ConcreteTypeHandle list)
        (site : GenericHelperSite)
        (spelling : TypeDefn)
        : EscapeAnalysisState * JitHelper option
        =
        let throwing = site.Instruction <> HelpedInstruction.Isinst

        let ofArgument (arguments : ConcreteTypeHandle list) (index : int) : EscapeAnalysisState * JitHelper option =
            let argument =
                match List.tryItem index arguments with
                | Some argument -> argument
                | None ->
                    failwith
                        $"BUG: a %O{site.Instruction} of %O{site.Type} names type variable %d{index}, which the instance has no argument for"

            let identity =
                AllConcreteTypes.lookup argument state.TypeSystem.ConcreteTypes
                |> Option.map (fun concrete -> concrete.Identity)

            let nullable = identity = Some state.BaseTypes.Nullable.Identity

            let reference =
                TypeSystemState.isReferenceTypeHandle state.BaseTypes "EscapeAnalysis" state.TypeSystem argument

            match site.Instruction with
            | HelpedInstruction.Unbox ->
                state,
                Some (
                    if nullable then
                        JitHelper.UnboxNullable
                    else
                        JitHelper.Unbox
                )
            | HelpedInstruction.Isinst
            | HelpedInstruction.Castclass
            | HelpedInstruction.UnboxAny when reference -> state, Some (JitHelper.Cast (throwing, CastTarget.Any))
            | HelpedInstruction.UnboxAny ->
                state,
                Some (
                    if nullable then
                        JitHelper.UnboxNullable
                    else
                        JitHelper.Unbox
                )
            | HelpedInstruction.Isinst
            | HelpedInstruction.Castclass ->
                if
                    nullable
                    && CodeSharing.canonicalisesToCanon state.BaseTypes state.TypeSystem argument
                then
                    state, Some (JitHelper.Cast (throwing, CastTarget.Any))
                else
                    state, Some (JitHelper.Cast (throwing, CastTarget.Class))
            | HelpedInstruction.Box ->
                if reference then
                    state, None
                elif nullable then
                    state, Some JitHelper.BoxNullable
                else
                    match identity with
                    | Some identity ->
                        match boxedWithoutHelper state identity with
                        | state, true -> state, None
                        | state, false -> state, Some JitHelper.Box
                    | None ->
                        failwith
                            $"BUG: a box of %O{site.Type} names type variable %d{index}, whose argument %O{argument} is no registered type"
            | HelpedInstruction.Ldelema -> state, (if reference then Some JitHelper.LdelemaRef else None)
            | HelpedInstruction.Stelem -> state, (if reference then Some JitHelper.StelemRef else None)

        match spelling with
        | TypeDefn.GenericTypeParameter index -> ofArgument typeArguments index
        | TypeDefn.GenericMethodParameter index -> ofArgument methodArguments index
        | TypeDefn.Modified modified -> instanceHelper state typeArguments methodArguments site modified.Unmodified
        // A cast to a `Nullable<T>` over a type variable.
        | TypeDefn.GenericInstantiation _ when
            site.Instruction = HelpedInstruction.Isinst
            || site.Instruction = HelpedInstruction.Castclass
            || site.Instruction = HelpedInstruction.UnboxAny
            ->
            if
                CodeSharing.namesSharedArgument state.BaseTypes state.TypeSystem typeArguments methodArguments spelling
            then
                state, Some (JitHelper.Cast (throwing, CastTarget.Any))
            else
                state, Some (JitHelper.Cast (throwing, CastTarget.Class))
        | other -> failwith $"BUG: a %O{site.Instruction} of %O{other} recorded as one only an instance decides"

    /// What the JIT raises, refusing to compile a body running as `caller` over `site`: nothing
    /// where it compiles it, and opaque where only an instance of an open definition decides.
    /// Shared code names the canonical form of a type, which the JIT's comparisons of types decide
    /// less often (`JitTypeComparison`), so it may refuse a box of one that it folds where it
    /// inlines the body into a caller that names the type as it is. Shared code's answer is
    /// reported, which covers the inlined copy's.
    let private refusalAt
        (state : EscapeAnalysisState)
        (caller : Instantiation)
        (site : RefusalSite)
        : EscapeAnalysisState * OutsideBodyFact list
        =
        let named =
            match site with
            | RefusalSite.Box (named, _)
            | RefusalSite.Newarr named -> named

        let shared (spelled : SpelledType) : bool =
            match caller with
            | Instantiation.Closed (typeArguments, methodArguments) ->
                CodeSharing.namesSharedArgument
                    state.BaseTypes
                    state.TypeSystem
                    typeArguments
                    methodArguments
                    spelled.Type
            | Instantiation.Open -> false

        match concretizeIn state caller named.Type named.SpelledIn with
        | state, None when caller = Instantiation.Open && mentionsTypeVariable named.Type ->
            state, [ OutsideBodyFact.Opaque Opacity.DependsOnInstantiation ]
        | state, handle ->

        let state, byRefLike =
            match handle with
            | Some handle ->
                match TypeSystemState.tryGetConcreteTypeInfo state.TypeSystem handle with
                | Some (_, ty) ->
                    state, LoadedTypeInfo.isByRefLike state.BaseTypes state.TypeSystem._LoadedAssemblies ty
                | None -> state, false
            // The token binds, which loads the type it names in full (`bind`), but the analysis
            // does not instantiate it (`concretizable`). It is byref-like where its definition is,
            // whatever its type arguments.
            | None ->
                match nominalIdentity state (assemblyOf state named.SpelledIn) named.Type with
                | state, Some identity ->
                    let _, ty = definitionOf state identity
                    state, LoadedTypeInfo.isByRefLike state.BaseTypes state.TypeSystem._LoadedAssemblies ty
                // No array, pointer or function pointer is byref-like.
                | state, None -> state, false

        if not byRefLike then
            state, []
        else

        // Whether the JIT may refuse the body over the site.
        let state, refuses =
            match site with
            | RefusalSite.Newarr _ -> state, true
            | RefusalSite.Box (boxed, follower) ->

            let view (handle : ConcreteTypeHandle) (spelled : SpelledType) : JitTypeView =
                {
                    Type = handle
                    Canonical = shared spelled
                }

            let boxed = handle |> Option.map (fun handle -> view handle boxed)

            // Where the analysis does not instantiate the boxed type, it proves it equal to no type
            // and decides no cast from it, where the JIT may do either and fold the box: it reports
            // the refusal.
            let provesEqual (a : JitTypeView option) (b : JitTypeView option) : bool =
                match a, b with
                | Some a, Some b -> JitTypeComparison.provesEqual a b
                | _, _ -> false

            // What the JIT resolves a type the follower names to, in this instance. Where the
            // analysis does not instantiate it, it cannot tell whether the type loads.
            let resolve
                (state : EscapeAnalysisState)
                (operand : TokenResolution<SpelledType>)
                : EscapeAnalysisState * TokenResolution<JitTypeView>
                =
                match operand with
                | TokenResolution.Raises -> state, TokenResolution.Raises
                | TokenResolution.Undecided -> state, TokenResolution.Undecided
                | TokenResolution.Resolves spelled ->
                    match concretizeIn state caller spelled.Type spelled.SpelledIn with
                    | state, Some handle -> state, TokenResolution.Resolves (view handle spelled)
                    | state, None -> state, TokenResolution.Undecided

            let resolveInto
                (state : EscapeAnalysisState)
                (operand : TokenResolution<SpelledType>)
                (follower : JitTypeView -> BoxFollower<JitTypeView>)
                =
                match resolve state operand with
                | state, TokenResolution.Resolves resolved -> state, TokenResolution.Resolves (follower resolved)
                | state, TokenResolution.Raises -> state, TokenResolution.Raises
                | state, TokenResolution.Undecided -> state, TokenResolution.Undecided

            let state, resolved =
                match follower with
                | BoxFollower.Branch -> state, TokenResolution.Resolves BoxFollower.Branch
                | BoxFollower.Other -> state, TokenResolution.Resolves BoxFollower.Other
                | BoxFollower.Unboxed unboxed -> resolveInto state unboxed BoxFollower.Unboxed
                | BoxFollower.TestedCast cast -> resolveInto state cast BoxFollower.TestedCast
                | BoxFollower.CastUnboxed (cast, unboxed) ->
                    match resolve state cast with
                    // The JIT resolves the `unbox.any`'s token only once it proves the `isinst`'s
                    // type is the boxed type.
                    | state, TokenResolution.Resolves cast when provesEqual (Some cast) boxed ->
                        resolveInto state unboxed (fun unboxed -> BoxFollower.CastUnboxed (cast, unboxed))
                    | state, TokenResolution.Resolves _ -> state, TokenResolution.Resolves BoxFollower.Other
                    | state, TokenResolution.Raises -> state, TokenResolution.Raises
                    | state, TokenResolution.Undecided -> state, TokenResolution.Undecided

            match resolved with
            | TokenResolution.Raises -> state, false
            | TokenResolution.Undecided -> state, true
            | TokenResolution.Resolves follower ->
                let decidesCast (from : JitTypeView option) (target : JitTypeView option) : bool option =
                    match from, target with
                    | Some from, Some target ->
                        JitTypeComparison.decidesCastFromValueType state.BaseTypes state.TypeSystem from target
                    | _, _ -> None

                match BoxPattern.foldsByRefLike provesEqual decidesCast boxed (BoxFollower.map Some follower) with
                | Some folds -> state, not folds
                // A cast from a canonical form to a generic interface, which the JIT decides only
                // where the canonical form casts to it.
                | None -> state, true

        if not refuses then
            state, []
        else

        match site with
        // `COMPlusThrow(kInvalidProgramException)`, for the JIT's `CORJIT_BADCODE`, makes it as
        // `EEException::CreateThrowable` does.
        | RefusalSite.Box _ ->
            let thrown, made = constructedRaise state "System.InvalidProgramException"
            state, [ OutsideBodyFact.Raises thrown ; OutsideBodyFact.Runs (made, []) ]
        // `ThrowTypeLoadException`, for the array type, makes it as `EETypeLoadException` does.
        | RefusalSite.Newarr _ ->
            let thrown, made =
                runtimeRaises state "System.TypeLoadException" ExceptionMaking.TypeLoadConstructor

            state,
            OutsideBodyFact.Raises thrown
            :: List.map (fun made -> OutsideBodyFact.Runs (made, [])) made

    /// <summary>
    /// What the constraint checks binding the body of <paramref name="instance" /> makes raise, where
    /// only an instance decides them (<c>LocalFacts.InstanceChecks</c>), before the body runs.
    /// </summary>
    /// <remarks>
    /// The JIT resolves the body's tokens in the context its code runs in: with each type argument
    /// in its canonical form where the code is shared between instantiations
    /// (<c>canonicalArgument</c>), whose instantiations containing <c>System.__Canon</c> CoreCLR
    /// does not check. A definition analysed for every instantiation at once leaves open what is
    /// spelled over its type variables (<c>Opacity.DependsOnInstantiation</c>), where some
    /// instantiation may break a constraint; what is not spelled so it checks exactly.
    /// </remarks>
    let private instanceConstraintFacts
        (state : EscapeAnalysisState)
        (instance : MethodInstance)
        (checks : InstanceCheck list)
        : EscapeAnalysisState * Set<OutsideBodyFact>
        =
        let assembly, method = methodOf state instance.Definition

        let raisesOf (state : EscapeAnalysisState) (failure : BindFailure option) : Set<OutsideBodyFact> =
            failure
            |> Option.map (bindFailureFacts state >> Set.ofList)
            |> Option.defaultValue Set.empty

        // What a check raises with the body's type variables standing for these arguments.
        let checkIn
            (state : EscapeAnalysisState)
            (typeArguments : ConcreteTypeHandle list)
            (methodArguments : ConcreteTypeHandle list)
            (check : InstanceCheck)
            : EscapeAnalysisState * BindFailure option
            =
            let loadInFull (state : EscapeAnalysisState) spelledIn typeArguments methodArguments spelling =
                match instantiate state spelledIn typeArguments methodArguments spelling with
                | state, Error failure -> state, Some (failureOfBinding failure)
                | state, Ok handle -> closureConstraintFailure state handle

            match check with
            | InstanceCheck.LoadsInFull (spelledIn, spelling) ->
                loadInFull state spelledIn typeArguments methodArguments spelling
            | InstanceCheck.LoadsInMember (spelledIn, spelling, token) ->
                let state, memberTypeArguments, memberMethodArguments =
                    match token with
                    | MetadataToken.MethodSpecification _
                    | MetadataToken.MemberReference _ ->
                        spelledInstantiation state assembly typeArguments methodArguments token
                    // A member definition of a type of this assembly: the body's own type, whose
                    // variables are the body's.
                    | _ -> state, typeArguments, []

                loadInFull state spelledIn memberTypeArguments memberMethodArguments spelling
            | InstanceCheck.Instantiates (spelledIn, handle) ->
                let spelledAssembly = assemblyOf state spelledIn
                let token = MetadataToken.MethodSpecification handle

                match callTarget state spelledAssembly token with
                | state, CallTarget.Method callee ->
                    let state, calleeTypeArguments, calleeMethodArguments =
                        spelledInstantiation state spelledAssembly typeArguments methodArguments token

                    methodInstantiationFailure state callee.Callee calleeTypeArguments calleeMethodArguments
                | state, _ -> state, None

        match instance.Arguments with
        | Instantiation.Closed (typeArguments, methodArguments) ->
            let state, canon = canonHandle state

            let canonical (state : EscapeAnalysisState) (arguments : ConcreteTypeHandle list) =
                let state, canonical =
                    ((state, []), arguments)
                    ||> List.fold (fun (state, made) argument ->
                        let state, argument = canonicalArgument state canon argument
                        state, argument :: made
                    )

                state, List.rev canonical

            let state, typeArguments = canonical state typeArguments
            let state, methodArguments = canonical state methodArguments

            ((state, Set.empty), checks)
            ||> List.fold (fun (state, facts) check ->
                let state, failure = checkIn state typeArguments methodArguments check
                state, Set.union facts (raisesOf state failure)
            )
        | Instantiation.Open ->
            ((state, Set.empty), checks)
            ||> List.fold (fun (state, facts) check ->
                let overVariables, mayBreak =
                    match check with
                    | InstanceCheck.LoadsInFull (spelledIn, spelling) ->
                        true,
                        fun state -> spellingMayBreakConstraint state (assemblyOf state spelledIn) method spelling
                    | InstanceCheck.LoadsInMember (spelledIn, spelling, token) ->
                        spelledArguments assembly token |> List.exists mentionsTypeVariable,
                        fun state ->
                            // The member's own type variables, as the token instantiates them over
                            // the body's.
                            let shapesOf (state : EscapeAnalysisState) (spellings : TypeDefn seq) =
                                let state, shapes =
                                    ((state, []), spellings)
                                    ||> Seq.fold (fun (state, shapes) spelling ->
                                        let state, shape = shapeOf state assembly noSubstitution spelling
                                        state, shape :: shapes
                                    )

                                state, Array.ofList (List.rev shapes)

                            let parentArguments (state : EscapeAnalysisState) (token : MetadataToken) =
                                match token with
                                | MetadataToken.MemberReference reference ->
                                    match assembly.Members.[reference].Parent with
                                    | MetadataToken.TypeSpecification parent ->
                                        match
                                            shapeOf state assembly noSubstitution assembly.TypeSpecs.[parent].Signature
                                        with
                                        | state, Shape.Named (_, arguments) -> state, Array.ofList arguments
                                        | state, _ -> state, [||]
                                    | _ -> state, [||]
                                | _ -> state, [||]

                            let state, typeArguments, methodArguments =
                                match token with
                                | MetadataToken.MethodSpecification handle ->
                                    let spec = assembly.MethodSpecs.[handle]
                                    let state, methodArguments = shapesOf state spec.Signature
                                    let state, typeArguments = parentArguments state spec.Method
                                    state, typeArguments, methodArguments
                                | _ ->
                                    let state, typeArguments = parentArguments state token
                                    state, typeArguments, [||]

                            let substitute (ofMethod : bool) (index : int) : Shape option =
                                let arguments = if ofMethod then methodArguments else typeArguments

                                if index < arguments.Length then
                                    Some arguments.[index]
                                else
                                    Some Shape.Opaque

                            let state, shape = shapeOf state (assemblyOf state spelledIn) substitute spelling

                            // A member variable the token does not instantiate decides nothing.
                            if shapeHasOpaque shape then
                                state, true
                            else
                                shapeMayBreak
                                    state
                                    (scopeOf assembly method)
                                    (Some method.RequiredDeclaringType.Identity)
                                    shape
                    | InstanceCheck.Instantiates (spelledIn, handle) ->
                        spelledArguments (assemblyOf state spelledIn) (MetadataToken.MethodSpecification handle)
                        |> List.exists mentionsTypeVariable,
                        fun state -> specificationMayBreakConstraint state (assemblyOf state spelledIn) method handle

                if overVariables then
                    match mayBreak state with
                    | state, true -> state, Set.add (OutsideBodyFact.Opaque Opacity.DependsOnInstantiation) facts
                    | state, false -> state, facts
                else
                    let state, failure = checkIn state [] [] check
                    state, Set.union facts (raisesOf state failure)
            )

    /// The facts of `instance`: those of its definition, less what a run of the instance cannot
    /// reach, given which of the values its callers pass are null or zero (`MethodInstance.Passed`),
    /// and the type arguments it has where the JIT compiles a query of a type to a constant.
    let private instanceFacts
        (state : EscapeAnalysisState)
        (instance : MethodInstance)
        : EscapeAnalysisState * LocalFacts
        =
        match state.InstanceFacts.TryFind instance with
        | Some facts -> state, facts
        | None ->
            let state, facts = factsFor state instance.Definition

            let state, refined =
                match facts.Branching with
                | None -> state, facts
                | Some branching ->
                    let state, decided =
                        ((state, []), Map.toList branching.Tests)
                        ||> List.fold (fun (state, decided) (offset, tested) ->
                            let state, known =
                                match tested with
                                | Tested.Types query -> typeQueryIsKnown state instance query
                                | Tested.References _
                                | Tested.Integers _ -> state, testIsKnown instance.Passed tested

                            match known with
                            | Some known -> state, (offset, known) :: decided
                            | None -> state, decided
                        )

                    if decided.IsEmpty then
                        state, facts
                    else
                        let constants =
                            (branching.Constants, decided)
                            ||> List.fold (fun constants (offset, known) -> Map.add offset known constants)

                        let executed = ControlFlow.mayExecute branching.Body constants
                        let at (offset : int) : bool = executed.Contains offset

                        // Every field is written out, so that one added to `LocalFacts` must be
                        // decided on here.
                        state,
                        {
                            Raises = facts.Raises |> List.filter (fst >> at)
                            Opaque = facts.Opaque |> List.filter (fun (offset, _, _) -> at offset)
                            Assumed = facts.Assumed |> List.filter (fst >> at)
                            Relies = facts.Relies |> List.filter (fst >> at)
                            Calls = facts.Calls |> List.filter (fun (offset, _, _) -> at offset)
                            LookedUp = facts.LookedUp |> List.filter (fst >> at)
                            // Read only to drop `Calls` at its offsets, which are filtered.
                            HelperSites = facts.HelperSites
                            // What the JIT compiles, which no value a caller passes decides.
                            ReceiverBoxes = facts.ReceiverBoxes
                            Refusals = facts.Refusals
                            ConstrainedPointers =
                                facts.ConstrainedPointers |> List.filter (fun (offset, _, _) -> at offset)
                            Rethrows = facts.Rethrows |> List.filter (fst >> at)
                            Regions = facts.Regions
                            InstanceChecks = facts.InstanceChecks
                            OutsideBody = facts.OutsideBody
                            RaisedWhereInlined = facts.RaisedWhereInlined
                            Spellings = facts.Spellings
                            Returns = returnedOver branching.Returns executed
                            Stored = facts.Stored |> Map.filter (fun offset _ -> at offset)
                            Consults = facts.Consults
                            Branching = facts.Branching
                        }

            // The JIT makes the `RuntimeType` of a type that the instance's code looks up, and embeds
            // that of any other type, which it makes as it compiles.
            let refined =
                let lookedUp =
                    match instance.Arguments with
                    | Instantiation.Open -> refined.LookedUp
                    | Instantiation.Closed (typeArguments, methodArguments) ->
                        refined.LookedUp
                        |> List.filter (fun (_, spelled) ->
                            CodeSharing.namesSharedArgument
                                state.BaseTypes
                                state.TypeSystem
                                typeArguments
                                methodArguments
                                spelled.Type
                        )

                if lookedUp.IsEmpty then
                    refined
                else
                    let raised = lookupRaises state

                    { refined with
                        Raises =
                            (lookedUp
                             |> List.collect (fun (offset, _) -> raised |> List.map (fun thrown -> offset, thrown)))
                            @ refined.Raises
                    }

            // The JIT calls the one `CastHelpers` method, if any, that the instance's instruction
            // picks.
            let state, refined =
                match instance.Arguments with
                | Instantiation.Open -> state, refined
                | Instantiation.Closed (typeArguments, methodArguments) ->
                    let state, excluded =
                        ((state, []), refined.HelperSites)
                        ||> List.fold (fun (state, excluded) (offset, site) ->
                            let state, picked =
                                instanceHelper state typeArguments methodArguments site site.Type

                            let others =
                                candidateHelpers site.Instruction
                                |> List.filter (fun helper -> Some helper <> picked)
                                |> List.map (fun helper -> offset, jitHelperMethod state helper)

                            state, others @ excluded
                        )

                    let excluded = Set.ofList excluded

                    if excluded.IsEmpty then
                        state, refined
                    else
                        state,
                        { refined with
                            Calls =
                                refined.Calls
                                |> List.filter (fun (offset, site, _) ->
                                    match site with
                                    | CallSite.Runtime callee -> not (excluded.Contains (offset, callee.Callee))
                                    | CallSite.Direct _
                                    | CallSite.Constrained _
                                    | CallSite.Virtual _ -> true
                                )
                        }

            // The JIT boxes a `constrained.` call's receiver by the one `CastHelpers` method, if any,
            // that the instance's call picks, and refuses to box a byref-like one.
            let state, refined =
                if refined.ReceiverBoxes.IsEmpty then
                    state, refined
                else
                    let assembly, _ = methodOf state instance.Definition
                    let candidates = candidateHelpers HelpedInstruction.Box

                    let state, excluded, refused =
                        ((state, [], false), refined.ReceiverBoxes)
                        ||> List.fold (fun (state, excluded, refused) (offset, site) ->
                            let state, box = receiverBox state assembly instance.Arguments site

                            let kept =
                                match box with
                                | ReceiverBox.Boxed helper -> Option.toList helper
                                | ReceiverBox.Undecided -> candidates
                                | ReceiverBox.Unboxed
                                | ReceiverBox.Refused -> []

                            let refused =
                                match box with
                                | ReceiverBox.Refused -> true
                                | ReceiverBox.Undecided -> refused || site.MayBeByRefLike
                                | ReceiverBox.Boxed _
                                | ReceiverBox.Unboxed -> refused

                            let others =
                                candidates
                                |> List.filter (fun helper -> not (List.contains helper kept))
                                |> List.map (fun helper -> offset, jitHelperMethod state helper)

                            state, others @ excluded, refused
                        )

                    let excluded = Set.ofList excluded

                    state,
                    { refined with
                        // The JIT refuses to compile the body, before it runs.
                        OutsideBody =
                            if refused then
                                Set.add (OutsideBodyFact.Opaque Opacity.BoxedByRefLikeReceiver) refined.OutsideBody
                            else
                                refined.OutsideBody
                        Calls =
                            refined.Calls
                            |> List.filter (fun (offset, site, _) ->
                                match site with
                                | CallSite.Runtime callee -> not (excluded.Contains (offset, callee.Callee))
                                | CallSite.Direct _
                                | CallSite.Constrained _
                                | CallSite.Virtual _ -> true
                            )
                    }

            // What the JIT raises, should it refuse to compile the instance.
            let state, refused =
                ((state, Set.empty), refined.Refusals)
                ||> List.fold (fun (state, refused) (_, site) ->
                    let state, facts = refusalAt state instance.Arguments site
                    state, Set.union refused (Set.ofList facts)
                )

            let refined =
                if refused.IsEmpty then
                    refined
                else
                    { refined with
                        OutsideBody = Set.union refined.OutsideBody refused
                    }

            // What the constraint checks only an instance decides raise, before the body runs.
            let state, constrained =
                let key = instance.Definition, instance.Arguments

                match state.InstanceConstraints.TryFind key with
                | Some known -> state, known
                | None ->
                    let state, constrained =
                        instanceConstraintFacts state instance refined.InstanceChecks

                    { state with
                        InstanceConstraints = state.InstanceConstraints.Add (key, constrained)
                    },
                    constrained

            let refined =
                if constrained.IsEmpty then
                    refined
                else
                    { refined with
                        OutsideBody = Set.union refined.OutsideBody constrained
                    }

            { state with
                InstanceFacts = state.InstanceFacts.Add (instance, refined)
            },
            refined

    /// The IL that says what `instance` stores, where it writes a field (`fieldContents`), and its
    /// facts: where CoreCLR runs that IL, the instance's own; where an answer takes an assumption's
    /// contract, or CoreLib's resource lookup or `Object.GetType`, in place of it, those of the IL
    /// itself, since the contract says what the IL raises, and the IL still stores what it stores.
    /// `None` where no IL of the method says (a native, or an intrinsic whose IL is not what it
    /// runs).
    let private writerFacts
        (state : EscapeAnalysisState)
        (instance : MethodInstance)
        : EscapeAnalysisState * (MethodInstructions<TypeDefn> * LocalFacts) option
        =
        let key = instance.Definition
        let assembly, method = methodOf state key

        match runsFor state.Assumptions assembly key, MethodInfo.tryIlBody method with
        | Runs.Il (body, _), _ ->
            let state, facts = instanceFacts state instance
            state, Some (body, facts)
        | (Runs.Assumed _ | Runs.ResourceLookup | Runs.ObjectGetType), Some own ->
            match state.OwnFacts.TryFind key with
            | Some facts -> state, Some (own, facts)
            | None ->
                let state, facts = factsOfRuns state key (Runs.Il (own, None))

                { state with
                    OwnFacts = state.OwnFacts.Add (key, facts)
                },
                Some (own, facts)
        | _ -> state, None

    /// How many classes an object a call returns, or passes, may be of before only the type
    /// declared for it says what it is.
    let private returnedClassesLimit : int = 8

    /// The objects an object among `objects` may be in `instance`, whose facts are `facts`; `None`
    /// where the instance does not decide their classes. `inProgress` holds the returns being
    /// worked out; a return that depends on one of them is its declared type.
    let rec private objectTypes
        (state : EscapeAnalysisState)
        (inProgress : Set<Pending>)
        (instance : MethodInstance)
        (facts : LocalFacts)
        (objects : Set<SpelledObject>)
        : EscapeAnalysisState * ObjectClasses option
        =
        // An object of the type spelled at `index`, or of one derived from it unless `exact`, or
        // null where `mayBeNull`.
        let ofSpelling (state : EscapeAnalysisState) (index : int) (exact : bool) (mayBeNull : bool) =
            let spelling = facts.Spellings.[index]

            if not spelling.InBodyContext then
                state, None
            else
                match concretizeIn state instance.Arguments spelling.Type spelling.SpelledIn with
                | state, Some handle ->
                    state,
                    Some
                        {
                            Classes = Set.singleton (handle, exact)
                            MayBeNull = mayBeNull
                        }
                | state, None -> state, None

        ((state, Some ObjectClasses.none), objects)
        ||> Set.fold (fun (state, soFar) object ->
            match soFar with
            | None -> state, None
            | Some soFar ->
                let state, objects =
                    match object with
                    | SpelledObject.Null -> state, Some ObjectClasses.onlyNull
                    | SpelledObject.Exactly index -> ofSpelling state index true false
                    | SpelledObject.Within index -> ofSpelling state index false true
                    | SpelledObject.Returned (call, declared) ->
                        match returnedBy state inProgress instance facts call with
                        | state, Some objects -> state, Some objects
                        | state, None ->
                            match declared with
                            | Some index -> ofSpelling state index false true
                            | None -> state, None
                    | SpelledObject.Argument (index, declared) ->
                        match instance.Passed.TryFind index with
                        | Some (PassedValue.Objects passed) -> state, Some passed
                        | Some (PassedValue.Integers _)
                        | Some (PassedValue.Pointer _) ->
                            failwith
                                $"BUG: something other than objects was passed as argument %d{index}, which is spelled as one"
                        | None -> ofSpelling state declared false true
                    | SpelledObject.Field (field, declared) ->
                        match fieldContents state inProgress field with
                        | state, Some objects -> state, Some objects
                        | state, None -> ofSpelling state declared false true

                state, objects |> Option.map (ObjectClasses.union soFar)
        )

    /// The objects `field` holds: null, or what a `stfld` or `stsfld` of its assembly stores in
    /// it, as each writer's own instance decides, where the analysis follows its writers
    /// (`followableField`); `None` where only its declared type says, which is also the answer for
    /// one that depends on a field in `inProgress`.
    and private fieldContents
        (state : EscapeAnalysisState)
        (inProgress : Set<Pending>)
        (field : FieldKey)
        : EscapeAnalysisState * ObjectClasses option
        =
        match state.FieldContents.TryFind field with
        | Some known -> state, known
        | None when inProgress.Contains (Pending.Field field) -> state, None
        | None ->
            let state, followable = followableField state field

            let state, contents =
                if not followable then
                    state, None
                else
                    let assembly = assemblyOf state field.AssemblyFullName
                    let state, writes = fieldWrites state assembly
                    let inProgress = Set.add (Pending.Field field) inProgress

                    // Until a writer runs, the field holds null.
                    let unwritten =
                        {
                            Classes = Set.empty
                            MayBeNull = true
                        }

                    ((state, Some unwritten), Option.defaultValue [] (writes.Stores.TryFind field.Row))
                    ||> List.fold (fun (state, soFar) (writer, offset) ->
                        match soFar with
                        | None -> state, None
                        | Some soFar ->
                            let key = MethodKey.make assembly writer
                            let _, method = methodOf state key

                            // A generic writer stands for every instantiation of it.
                            let instance =
                                if method.DeclaringTypeGenerics.IsEmpty && method.Generics.IsEmpty then
                                    instanceOf state key [] []
                                else
                                    {
                                        Definition = key
                                        Arguments = Instantiation.Open
                                        Passed = Map.empty
                                    }

                            // The writer's facts say what it stores only where they are of the IL
                            // whose store this is.
                            let storedBy (op : IlOp option) : MetadataToken option =
                                match op with
                                | Some (IlOp.UnaryMetadataToken ((UnaryMetadataTokenIlOp.Stfld | UnaryMetadataTokenIlOp.Stsfld),
                                                                 MetadataOperand.FromMetadata token)) ->
                                    Some token.Token
                                | _ -> None

                            let scanned =
                                MethodInfo.tryIlBody method
                                |> Option.bind (fun own -> storedBy (own.Locations.TryFind offset))

                            match writerFacts state instance with
                            | state, None -> state, None
                            | state, Some (body, _) when
                                scanned.IsNone || storedBy (body.Locations.TryFind offset) <> scanned
                                ->
                                state, None
                            | state, Some (_, facts) ->

                            match Map.tryFind offset facts.Stored with
                            | Some (StackValue.Objects objects) ->
                                match objectTypes state inProgress instance facts objects with
                                | state, Some stored -> state, Some (ObjectClasses.union soFar stored)
                                | state, None -> state, None
                            | Some (StackValue.Integers _ | StackValue.Addresses _ | StackValue.Unknown) -> state, None
                            // No run of the writer reaches the store.
                            | None -> state, Some soFar
                    )

            let contents =
                contents
                |> Option.filter (fun contents -> contents.Classes.Count <= returnedClassesLimit)

            { state with
                FieldContents = state.FieldContents.Add (field, contents)
            },
            contents

    /// `reached`, as a call in `instance` that passes `passes` (`LocalFacts.Calls`) reaches it:
    /// with the classes of the objects, or the integers, it passes as each argument `reached`'s body
    /// consults, where `instance` decides them. A call dispatched on a receiver of a class passes an
    /// object of that class, `receiver`, as `this`.
    and private passedTo
        (state : EscapeAnalysisState)
        (inProgress : Set<Pending>)
        (instance : MethodInstance)
        (facts : LocalFacts)
        (passes : SpelledArgument list)
        (receiver : (ConcreteTypeHandle * bool) option)
        (reached : MethodInstance)
        : EscapeAnalysisState * MethodInstance
        =
        if not state.ArgumentContexts then
            state, reached
        else

        let state, reachedFacts = factsFor state reached.Definition

        // What the parameter at each index takes, `this` at 0.
        let parameters : ParameterKind list =
            let _, method = methodOf state reached.Definition

            let ofParameter (parameter : TypeDefn) : ParameterKind =
                match StackInteger.ofParameter parameter, TypeDefn.stripCustomModifiers parameter with
                | Some integer, _ -> ParameterKind.Integer integer
                | None, TypeDefn.Byref _ -> ParameterKind.Byref
                | None, _ -> ParameterKind.Other

            (if method.IsStatic then [] else [ ParameterKind.Other ])
            @ List.map ofParameter method.Signature.ParameterTypes

        // `convert` of every one of `values`; `None` where it fails for one of them.
        let convertedAll
            (convert : StackInteger -> StackInteger option)
            (values : Set<StackInteger>)
            : Set<StackInteger> option
            =
            let converted = values |> Set.toList |> List.map convert

            if List.forall Option.isSome converted then
                Some (converted |> List.choose id |> Set.ofList)
            else
                None

        // The integers among `integers`, as `instance` decides them.
        let integersOf (integers : Set<SpelledInteger>) : Set<StackInteger> option =
            (Some Set.empty, integers)
            ||> Set.fold (fun soFar integer ->
                match soFar, integer with
                | None, _ -> None
                | Some soFar, SpelledInteger.Constant value -> Some (Set.add value soFar)
                | Some soFar, SpelledInteger.Argument index ->
                    match instance.Passed.TryFind index with
                    | Some (PassedValue.Integers values) -> Some (Set.union values soFar)
                    | None -> None
                    | Some (PassedValue.Objects _)
                    | Some (PassedValue.Pointer _) ->
                        failwith $"BUG: something other than integers was passed as argument %d{index}, an integer"
                | Some soFar, SpelledInteger.Loaded (index, loaded) ->
                    match instance.Passed.TryFind index with
                    | Some (PassedValue.Pointer (location, holds)) ->
                        convertedAll (StackInteger.loadedAs location loaded) holds
                        |> Option.map (Set.union soFar)
                    | None -> None
                    | Some (PassedValue.Objects _)
                    | Some (PassedValue.Integers _) ->
                        failwith $"BUG: something other than a pointer was passed as argument %d{index}, a byref"
            )

        let state, passed =
            ((state, Map.empty), reachedFacts.Consults)
            ||> Set.fold (fun (state, passed) index ->
                match index, receiver with
                | 0, Some receiver ->
                    state,
                    Map.add
                        0
                        (PassedValue.Objects
                            {
                                Classes = Set.singleton receiver
                                MayBeNull = false
                            })
                        passed
                | _ ->
                    match List.tryItem index parameters, List.tryItem index passes with
                    | Some (ParameterKind.Integer parameter),
                      Some (SpelledArgument.Value (StackValue.Integers integers)) ->
                        match
                            integersOf integers
                            |> Option.bind (convertedAll (StackInteger.passedAs parameter))
                        with
                        | Some values -> state, Map.add index (PassedValue.Integers values) passed
                        | None -> state, passed
                    | Some ParameterKind.Byref, Some (SpelledArgument.Pointer (location, holds)) ->
                        match integersOf holds with
                        | Some holds -> state, Map.add index (PassedValue.Pointer (location, holds)) passed
                        | None -> state, passed
                    | Some ParameterKind.Other, Some (SpelledArgument.Value (StackValue.Objects objects)) ->
                        match objectTypes state inProgress instance facts objects with
                        | state, Some objects when objects.Classes.Count <= returnedClassesLimit ->
                            state, Map.add index (PassedValue.Objects objects) passed
                        | state, _ -> state, passed
                    | Some (ParameterKind.Integer _), _
                    | Some ParameterKind.Byref, _
                    | Some ParameterKind.Other, _
                    | None, _ -> state, passed
            )

        state,
        { reached with
            Passed = passed
        }

    /// What `outcome`, of dispatching a call in `instance` that passes `passes`, reaches, as
    /// `passedTo` makes it.
    and private passedToOutcome
        (state : EscapeAnalysisState)
        (inProgress : Set<Pending>)
        (instance : MethodInstance)
        (facts : LocalFacts)
        (passes : SpelledArgument list)
        (receiver : (ConcreteTypeHandle * bool) option)
        (outcome : DispatchOutcome)
        : EscapeAnalysisState * DispatchOutcome
        =
        match outcome with
        | DispatchOutcome.Reaches reached ->
            let state, reached =
                passedTo state inProgress instance facts passes receiver reached

            state, DispatchOutcome.Reaches reached
        | DispatchOutcome.ReachesMistyped reached ->
            let state, reached =
                passedTo state inProgress instance facts passes receiver reached

            state, DispatchOutcome.ReachesMistyped reached
        | DispatchOutcome.Raises _
        | DispatchOutcome.Undecided -> state, outcome

    /// What the call at the offset `call` of `instance` returns: what every instance it reaches
    /// returns. `None` where the call is not decided, or what one of those returns is not known.
    and private returnedBy
        (state : EscapeAnalysisState)
        (inProgress : Set<Pending>)
        (instance : MethodInstance)
        (facts : LocalFacts)
        (call : int)
        : EscapeAnalysisState * ObjectClasses option
        =
        let pending = Pending.Call (instance, call)

        if inProgress.Contains pending then
            state, None
        else

        let inProgress = Set.add pending inProgress
        let assembly = assemblyOf state instance.Definition.AssemblyFullName

        let reachedBy (outcomes : DispatchOutcome list) : MethodInstance list option =
            if
                outcomes
                |> List.forall (fun outcome ->
                    match outcome with
                    | DispatchOutcome.Reaches _ -> true
                    // What the method returns on that receiver is not known.
                    | DispatchOutcome.ReachesMistyped _
                    | DispatchOutcome.Raises _
                    | DispatchOutcome.Undecided -> false
                )
            then
                outcomes
                |> List.choose (fun outcome ->
                    match outcome with
                    | DispatchOutcome.Reaches reached -> Some reached
                    | DispatchOutcome.ReachesMistyped _
                    | DispatchOutcome.Raises _
                    | DispatchOutcome.Undecided -> None
                )
                |> Some
            else
                None

        let state, reached =
            // The call the instruction makes, not a method the runtime runs there by itself.
            let made =
                facts.Calls
                |> List.tryFind (fun (offset, site, _) ->
                    match site with
                    | CallSite.Runtime _ -> false
                    | CallSite.Direct _
                    | CallSite.Constrained _
                    | CallSite.Virtual _ -> offset = call
                )

            match made with
            | None -> state, None
            | Some (_, CallSite.Runtime ran, _) ->
                failwith $"BUG: looking for the call at %d{call}, the search found %O{ran.Callee}, which it skips"
            | Some (_, CallSite.Direct callee, passes) ->
                let state, reached = calleeInstance state assembly instance.Arguments callee

                let state, reached = passedTo state inProgress instance facts passes None reached

                state, Some [ reached ]
            | Some (_, CallSite.Constrained (constrainedType, callee), passes) ->
                let state, outcome =
                    constrainedInstance state assembly instance.Arguments constrainedType callee

                let state, outcome =
                    passedToOutcome state inProgress instance facts passes None outcome

                state, reachedBy [ outcome ]
            | Some (_, CallSite.Virtual (receivers, callee), passes) ->
                match virtualOutcomes state inProgress instance facts passes receivers callee with
                | state, Some outcomes -> state, reachedBy outcomes
                | state, None -> state, None

        match reached with
        | None -> state, None
        | Some reached ->
            ((state, Some ObjectClasses.none), reached)
            ||> List.fold (fun (state, soFar) reached ->
                match soFar with
                | None -> state, None
                | Some soFar ->
                    match returnsOf state inProgress reached with
                    | state, Some objects -> state, Some (ObjectClasses.union soFar objects)
                    | state, None -> state, None
            )

    /// What a `callvirt` of `callee` in `instance`, passing `passes`, does on each class its
    /// receiver, one of `receivers`, may be of; `None` where the instance does not decide those
    /// classes.
    and private virtualOutcomes
        (state : EscapeAnalysisState)
        (inProgress : Set<Pending>)
        (instance : MethodInstance)
        (facts : LocalFacts)
        (passes : SpelledArgument list)
        (receivers : Set<SpelledObject>)
        (callee : Callee)
        : EscapeAnalysisState * DispatchOutcome list option
        =
        let assembly = assemblyOf state instance.Definition.AssemblyFullName

        match objectTypes state inProgress instance facts receivers with
        | state, None -> state, None
        | state, Some objects ->
            let state, outcomes =
                ((state, []), Set.toList objects.Classes)
                ||> List.fold (fun (state, outcomes) (receiver, exact) ->
                    let state, outcome =
                        dispatchOn state assembly instance.Arguments receiver exact callee

                    let state, outcome =
                        passedToOutcome state inProgress instance facts passes (Some (receiver, exact)) outcome

                    state, outcome :: outcomes
                )

            state, Some (List.rev outcomes)

    /// The objects `instance` returns, over every `ret` a run of it reaches. `None` where only its
    /// declared return type says, which is also the answer for a return that depends on one in
    /// `inProgress`.
    and private returnsOf
        (state : EscapeAnalysisState)
        (inProgress : Set<Pending>)
        (instance : MethodInstance)
        : EscapeAnalysisState * ObjectClasses option
        =
        match state.Returns.TryFind instance with
        | Some known -> state, known
        | None when inProgress.Contains (Pending.Return instance) -> state, None
        | None ->
            let state, facts = instanceFacts state instance

            let state, types =
                match facts.Returns with
                | StackValue.Unknown
                | StackValue.Integers _
                | StackValue.Addresses _ -> state, None
                | StackValue.Objects objects ->
                    objectTypes state (Set.add (Pending.Return instance) inProgress) instance facts objects

            let types =
                types
                |> Option.filter (fun types -> types.Classes.Count <= returnedClassesLimit)

            { state with
                Returns = state.Returns.Add (instance, types)
            },
            types

    /// What `instance` calls, from the facts of its definition.
    let private callsOf
        (state : EscapeAnalysisState)
        (instance : MethodInstance)
        (facts : LocalFacts)
        : EscapeAnalysisState * InstanceCalls
        =
        let assembly = assemblyOf state instance.Definition.AssemblyFullName

        // What a dispatched call at `offset` runs by landing on `reached` rather than on what the
        // body names, and what that raises besides (`initializerFailure`). No token of this body
        // names the method dispatch lands on, so binding none of them runs its module's
        // initializer: that runs when the method first binds into its own module, out of this call.
        let landing
            (state : EscapeAnalysisState)
            (offset : int)
            (reached : MethodInstance)
            : (int * MethodInstance) list * (int * ThrownType) list
            =
            if reached.Definition.AssemblyFullName = instance.Definition.AssemblyFullName then
                [], []
            else
                match moduleInitializer state reached.Definition.AssemblyFullName with
                | None -> [], []
                | Some initializer ->
                    let wrapper, preserve = initializerFailure state

                    [
                        offset, instanceOf state initializer [] []
                        offset, instanceOf state preserve [] []
                    ],
                    [ offset, wrapper ]

        // What a call at `offset` that reaches `reached` runs first: the type initializer CoreCLR
        // runs on calling it (`initializerOnCall`), as `reached`'s type arguments instantiate it,
        // and what running one runs and raises besides (`initializerFailure`). None inside that same
        // initializer, which lets its own thread straight through (ECMA-335 I.8.9.5); of a generic
        // one analysed for every instantiation at once, nothing says which it initializes.
        let initializerOf
            (state : EscapeAnalysisState)
            (offset : int)
            (reached : MethodInstance)
            : EscapeAnalysisState * (int * MethodInstance) list * (int * ThrownType) list
            =
            match initializerOnCall state reached.Definition with
            | state, None -> state, [], []
            | state, Some initializer ->
                let initializer =
                    match reached.Arguments with
                    | Instantiation.Closed (typeArguments, _) -> instanceOf state initializer typeArguments []
                    | Instantiation.Open ->
                        {
                            Definition = initializer
                            Arguments = Instantiation.Open
                            Passed = Map.empty
                        }

                if
                    initializer.Definition = instance.Definition
                    && initializer.Arguments = instance.Arguments
                    && instance.Arguments <> Instantiation.Open
                then
                    state, [], []
                else
                    let wrapper, preserve = initializerFailure state

                    state, [ offset, initializer ; offset, instanceOf state preserve [] [] ], [ offset, wrapper ]

        // `reached`, called at `offset`, among what the instance calls, with what calling it runs
        // first.
        let reaching (offset : int) (reached : MethodInstance) (state, callees, raises, undecided) =
            let state, ran, raised = initializerOf state offset reached
            state, (offset, reached) :: ran @ callees, raised @ raises, undecided

        // Fold the outcomes of dispatching one call into what the instance calls: undecided if any
        // receiver leaves it so.
        let dispatched offset callee (state, callees, raises, undecided) (outcomes : DispatchOutcome list) =
            if outcomes |> List.contains DispatchOutcome.Undecided then
                state, callees, raises, (offset, Opacity.VirtualCall, Some callee.Callee) :: undecided
            else
                ((state, callees, raises, undecided), List.distinct outcomes)
                ||> List.fold (fun (state, callees, raises, undecided) outcome ->
                    match outcome with
                    | DispatchOutcome.Reaches reached ->
                        let ran, raised = landing state offset reached
                        reaching offset reached (state, ran @ callees, raised @ raises, undecided)
                    | DispatchOutcome.ReachesMistyped reached ->
                        let ran, raised = landing state offset reached

                        reaching
                            offset
                            reached
                            (state,
                             ran @ callees,
                             raised @ raises,
                             (offset, Opacity.MistypedReceiver, Some callee.Callee) :: undecided)
                    | DispatchOutcome.Raises (thrown, made) ->
                        state, (offset, instanceOf state made [] []) :: callees, (offset, thrown) :: raises, undecided
                    | DispatchOutcome.Undecided ->
                        state, callees, raises, (offset, Opacity.VirtualCall, Some callee.Callee) :: undecided
                )

        let state, callees, raises, undecided =
            ((state, [], [], []), facts.Calls)
            ||> List.fold (fun (state, callees, raises, undecided) (offset, site, passes) ->
                match site with
                | CallSite.Direct callee ->
                    let state, reached = calleeInstance state assembly instance.Arguments callee

                    let state, reached = passedTo state Set.empty instance facts passes None reached

                    reaching offset reached (state, callees, raises, undecided)
                | CallSite.Runtime callee ->
                    let state, reached = calleeInstance state assembly instance.Arguments callee
                    let state, reached = passedTo state Set.empty instance facts passes None reached
                    reaching offset reached (state, callees, raises, undecided)
                | CallSite.Constrained (constrainedType, callee) ->
                    let state, outcome =
                        constrainedInstance state assembly instance.Arguments constrainedType callee

                    let state, outcome =
                        passedToOutcome state Set.empty instance facts passes None outcome

                    dispatched offset callee (state, callees, raises, undecided) [ outcome ]
                | CallSite.Virtual (receivers, callee) ->
                    // A null receiver raises what `callvirt` itself does, and calls nothing.
                    match virtualOutcomes state Set.empty instance facts passes receivers callee with
                    | state, Some outcomes -> dispatched offset callee (state, callees, raises, undecided) outcomes
                    | state, None ->
                        state, callees, raises, (offset, Opacity.VirtualCall, Some callee.Callee) :: undecided
            )

        // A `constrained. ldftn` calls nothing it resolves to. Where its dispatch fails, CoreCLR's
        // `getCallInfo` (jitinterface.cpp) plants a throw at the `ldftn` in code compiled for an
        // exact instantiation, which runs the constructor making the exception. In shared code the
        // pointer is a stub that throws when called, unless the JIT inlined the frame into exact
        // code, which turns on the JIT tier, so the `ldftn` may raise it in every instance. That is
        // sound only while every use of the pointer is opaque (a delegate's constructor and
        // `Invoke` are native, and `calli` is an indirect call): otherwise a handler around the
        // `ldftn` that the pointer's call is outside would be credited with catching what the call
        // raises in shared code. `TestEscapeAnalysis` pins that opacity ("calling a pointer a
        // constrained ldftn loads stays opaque").
        let state, callees, raises, undecided =
            ((state, callees, raises, undecided), facts.ConstrainedPointers)
            ||> List.fold (fun (state, callees, raises, undecided) (offset, constrainedType, callee) ->
                match constrainedInstance state assembly instance.Arguments constrainedType callee with
                | state, DispatchOutcome.Reaches _
                | state, DispatchOutcome.ReachesMistyped _ -> state, callees, raises, undecided
                | state, DispatchOutcome.Raises (thrown, made) ->
                    state, (offset, instanceOf state made [] []) :: callees, (offset, thrown) :: raises, undecided
                | state, DispatchOutcome.Undecided ->
                    state, callees, raises, (offset, Opacity.ConstrainedMethodPointer, Some callee.Callee) :: undecided
            )

        let runs (state : EscapeAnalysisState) (outsideBody : Set<OutsideBodyFact>) =
            ((state, []), outsideBody)
            ||> Seq.fold (fun (state, outside) fact ->
                match fact with
                | OutsideBodyFact.Runs (method, passes) ->
                    let state, reached =
                        passedTo state Set.empty instance facts passes None (instanceOf state method [] [])

                    state, reached :: outside
                | OutsideBodyFact.Raises _
                | OutsideBodyFact.Opaque _ -> state, outside
            )

        let state, outside = runs state facts.OutsideBody
        let state, raisedWhereInlined = runs state facts.RaisedWhereInlined

        state,
        {
            Callees = List.rev callees
            Raises = List.rev raises
            Undecided = List.rev undecided
            Outside = List.rev outside
            RaisedWhereInlined = List.rev raisedWhereInlined
        }

    /// The full name of a type the analysis has loaded, for reporting.
    let typeName (state : EscapeAnalysisState) (identity : ResolvedTypeIdentity) : string =
        let assembly, ty = definitionOf state identity
        TypeInfo.fullName (fun handle -> assembly.TypeDefs.[handle]) ty

    /// The full name of a method the analysis has loaded, `Type::Method`, for reporting.
    let methodName (state : EscapeAnalysisState) (key : MethodKey) : string =
        let _, method = methodOf state key
        typeName state (declaringTypeOf state key) + "::" + method.Name

    /// The instance `escapes` summarises for `method`: a generic definition is asked about for every
    /// instantiation at once.
    let private rootOf (state : EscapeAnalysisState) (method : MethodKey) : MethodInstance =
        let _, definition = methodOf state method

        if definition.DeclaringTypeGenerics.IsEmpty && definition.Generics.IsEmpty then
            instanceOf state method [] []
        else
            {
                Definition = method
                Arguments = Instantiation.Open
                Passed = Map.empty
            }

    /// The block a clause protects.
    let private protectedBy (clause : ExceptionRegion) : ExceptionOffset =
        match clause with
        | ExceptionRegion.Catch (_, o)
        | ExceptionRegion.Filter (_, o)
        | ExceptionRegion.Finally o
        | ExceptionRegion.Fault o -> o

    /// <summary>
    /// What may escape `method`, and the state to ask the next question of.
    /// </summary>
    /// <remarks>
    /// Computes the summary of every method `method` can reach through calls whose target the
    /// analysis can name and has not summarised already, loading whatever assemblies they live in,
    /// and remembers them all.
    /// </remarks>
    let escapes (state : EscapeAnalysisState) (method : MethodKey) : EscapeAnalysisState * Escapes =
        let root = rootOf state method

        match state.Summaries.TryFind root with
        | Some known -> state, known
        | None ->

        // Every instance reachable from `root` that has no summary yet, with what it calls and the
        // local facts of its definition.
        let rec discover (state : EscapeAnalysisState) (pending : MethodInstance list) (found : MethodInstance list) =
            match pending with
            | [] -> state, found
            | key :: rest when state.Summaries.ContainsKey key || state.InstanceCalls.ContainsKey key ->
                discover state rest found
            | key :: rest ->
                let state, facts = instanceFacts state key
                let state, calls = callsOf state key facts

                let state =
                    { state with
                        InstanceCalls = state.InstanceCalls.Add (key, calls)
                    }

                discover state ((calls.Callees |> List.map snd) @ calls.Outside @ rest) (key :: found)

        let state, reachable = discover state [ root ] []

        // Whether a raise at `offset` of `key`'s body gets out of it.
        let escapesAt (state : EscapeAnalysisState) (key : MethodInstance) (offset : int) (thrown : ThrownType option) =
            let facts = state.InstanceFacts.[key]
            escapesHandlers state (assemblyOf state key.Definition.AssemblyFullName) facts.Regions offset thrown

        // What each `rethrow` in `key`'s body re-raises, given what its callees let escape: what
        // its clause caught of what the clause's protected block raised. That block may hold
        // another `rethrow`, in a nested handler or in the protected block of a clause nested in
        // a handler, so the rethrows depend on each other; they are solved together, to the least
        // fixed point.
        let rethrown
            (state : EscapeAnalysisState)
            (key : MethodInstance)
            (summaryOf : MethodInstance -> Escapes)
            : EscapeAnalysisState * (int * Raised) list
            =
            let facts = state.InstanceFacts.[key]
            let calls = state.InstanceCalls.[key]
            let assembly = assemblyOf state key.Definition.AssemblyFullName

            let rec solve
                (state : EscapeAnalysisState)
                (current : Map<int, Set<Raised>>)
                : EscapeAnalysisState * Map<int, Set<Raised>>
                =
                let state, next =
                    ((state, Map.empty), facts.Rethrows)
                    ||> List.fold (fun (state, next) (offset, clause) ->
                        let o = protectedBy clause

                        let inside (at : int) =
                            at >= o.TryOffset && at < o.TryOffset + o.TryLength

                        let raised =
                            [
                                for at, thrown in facts.Raises @ calls.Raises do
                                    if inside at then
                                        yield at, Raised.Named thrown
                                for at, _, _ in facts.Opaque @ calls.Undecided do
                                    if inside at then
                                        yield at, Raised.Unnamed
                                for at, assumption in facts.Assumed do
                                    if inside at then
                                        yield at, Raised.Assumed (assumption, None)
                                for at, callee in calls.Callees do
                                    if inside at then
                                        let calleeEscapes = summaryOf callee

                                        for thrown in calleeEscapes.Types do
                                            yield at, Raised.Named thrown

                                        if calleeEscapes.Unknown then
                                            yield at, Raised.Unnamed

                                        for assumption in calleeEscapes.Assumes do
                                            yield at, Raised.Assumed (assumption, None)
                                for KeyValue (at, rethrownThere) in current do
                                    if inside at then
                                        for thrown in rethrownThere do
                                            yield at, thrown
                            ]
                            |> List.distinct

                        // The clauses the runtime tries before this one.
                        let before = facts.Regions |> List.takeWhile (fun region -> region <> clause)

                        let state, here =
                            ((state, Set.empty), raised)
                            ||> List.fold (fun (state, here) (at, raised) ->
                                let thrown =
                                    match raised with
                                    | Raised.Named thrown -> Some thrown
                                    | Raised.Unnamed -> None
                                    | Raised.Assumed (_, narrowed) -> narrowed

                                match escapesHandlers state assembly before at thrown with
                                | state, false -> state, here
                                | state, true ->
                                    match raised, rethrownBy state assembly clause thrown with
                                    | _, (state, Rethrown.Nothing) -> state, here
                                    // What arises only if an assumption fails is re-raised only then,
                                    // as the clause narrows it.
                                    | Raised.Assumed (assumption, _), (state, Rethrown.Thrown thrown) ->
                                        state, Set.add (Raised.Assumed (assumption, Some thrown)) here
                                    | Raised.Assumed (assumption, _), (state, Rethrown.Unknown) ->
                                        state, Set.add (Raised.Assumed (assumption, None)) here
                                    | _, (state, Rethrown.Thrown thrown) -> state, Set.add (Raised.Named thrown) here
                                    | _, (state, Rethrown.Unknown) -> state, Set.add Raised.Unnamed here
                            )

                        state, Map.add offset here next
                    )

                if next = current then state, current else solve state next

            let state, solved =
                solve state (facts.Rethrows |> List.map (fun (offset, _) -> offset, Set.empty) |> Map.ofList)

            state,
            [
                for KeyValue (offset, rethrownHere) in solved do
                    for thrown in rethrownHere do
                        yield offset, thrown
            ]

        let seedOf (state : EscapeAnalysisState) (key : MethodInstance) : EscapeAnalysisState * Escapes =
            let facts = state.InstanceFacts.[key]
            let calls = state.InstanceCalls.[key]

            // What happens outside the body is past all its handlers.
            let outside =
                facts.OutsideBody
                |> Seq.choose (fun fact ->
                    match fact with
                    | OutsideBodyFact.Raises thrown -> Some thrown
                    | OutsideBodyFact.Opaque _
                    | OutsideBodyFact.Runs _ -> None
                )
                |> Set.ofSeq

            let opaqueOutside =
                facts.OutsideBody
                |> Set.exists (fun fact ->
                    match fact with
                    | OutsideBodyFact.Raises _
                    | OutsideBodyFact.Runs _ -> false
                    | OutsideBodyFact.Opaque _ -> true
                )

            let state, types =
                ((state, outside), facts.Raises @ calls.Raises)
                ||> List.fold (fun (state, types) (offset, thrown) ->
                    match escapesAt state key offset (Some thrown) with
                    | state, true -> state, Set.add thrown types
                    | state, false -> state, types
                )

            let state, unknown =
                ((state, opaqueOutside), facts.Opaque @ calls.Undecided)
                ||> List.fold (fun (state, unknown) (offset, _, _) ->
                    if unknown then
                        state, true
                    else
                        escapesAt state key offset None
                )

            let state, assumes =
                ((state, Set.empty), facts.Assumed)
                ||> List.fold (fun (state, assumes) (offset, assumption) ->
                    if Set.contains assumption assumes then
                        state, assumes
                    else
                        match escapesAt state key offset None with
                        | state, true -> state, Set.add assumption assumes
                        | state, false -> state, assumes
                )

            let assumes = Set.union assumes (facts.Relies |> List.map snd |> Set.ofList)

            // A requested abort can arise anywhere in the body, and no handler stops it, since a
            // catch raises it again at its end (`Assumption.NoThreadAborts`). Where CoreCLR cannot
            // allocate the abort's exception, it raises its preallocated `OutOfMemoryException`
            // instead (`CLRException::GetBestException`), which a handler catches as usual; but the
            // abort can arise where no handler of the body protects it, as in a handler's own code.
            let types, assumes =
                if state.Assumptions.Contains Assumption.NoThreadAborts then
                    types, Set.add Assumption.NoThreadAborts assumes
                else
                    types
                    |> Set.add (ThrownType.Exactly (corelibType state "System.Threading" "ThreadAbortException"))
                    |> Set.add (ThrownType.Exactly (corelibType state "System" "OutOfMemoryException")),
                    assumes

            state,
            {
                Types = types
                Unknown = unknown
                Assumes = assumes
            }

        let state, seeds =
            ((state, Map.empty), reachable)
            ||> List.fold (fun (state, seeds) key ->
                let state, seed = seedOf state key
                state, Map.add key seed seeds
            )

        // What compiling each method raises wherever the JIT compiles its code, which escapes
        // every method it is inlined into past all handlers.
        // An instance absent from these maps, as from `InlinedSummaries`, lets nothing escape so.
        let inlinedSeeds =
            reachable
            |> List.filter (fun key -> not state.InstanceFacts.[key].RaisedWhereInlined.IsEmpty)
            |> List.map (fun key ->
                let facts = state.InstanceFacts.[key]

                key,
                {
                    Types =
                        facts.RaisedWhereInlined
                        |> Seq.choose (fun fact ->
                            match fact with
                            | OutsideBodyFact.Raises thrown -> Some thrown
                            | OutsideBodyFact.Runs _
                            | OutsideBodyFact.Opaque _ -> None
                        )
                        |> Set.ofSeq
                    Unknown =
                        facts.RaisedWhereInlined
                        |> Set.exists (fun fact ->
                            match fact with
                            | OutsideBodyFact.Opaque _ -> true
                            | OutsideBodyFact.Raises _
                            | OutsideBodyFact.Runs _ -> false
                        )
                    Assumes = Set.empty
                }
            )
            |> Map.ofList

        // Iterate to the least fixed point: each method lets escape its own seed and whatever its
        // callees let escape past its handlers. Besides, what may escape every method it is
        // inlined into is what compiling it raises wherever the JIT compiles its code, and what
        // may escape every method each of its callees is inlined into, as the JIT may inline that
        // callee into it and it into another; that escapes it too, past all its handlers.
        let rec iterate
            (state : EscapeAnalysisState)
            (current : Map<MethodInstance, Escapes>)
            (currentInlined : Map<MethodInstance, Escapes>)
            =
            let summaryOf (key : MethodInstance) : Escapes =
                match state.Summaries.TryFind key with
                | Some known -> known
                | None -> current.[key]

            let inlinedOf (key : MethodInstance) : Escapes option =
                match state.InlinedSummaries.TryFind key with
                | Some known -> Some known
                | None -> currentInlined.TryFind key

            let join (acc : Escapes option) (escapes : Escapes option) : Escapes option =
                match acc, escapes with
                | acc, None -> acc
                | None, escapes -> escapes
                | Some acc, Some escapes -> Some (Escapes.union acc escapes)

            let nextInlined =
                (Map.empty, reachable)
                ||> List.fold (fun nextInlined key ->
                    let calls = state.InstanceCalls.[key]

                    let inlined =
                        (inlinedSeeds.TryFind key, calls.RaisedWhereInlined)
                        ||> List.fold (fun acc made -> join acc (Some (summaryOf made)))

                    let inlined =
                        (inlined, calls.Callees)
                        ||> List.fold (fun acc (_, callee) -> join acc (inlinedOf callee))

                    match inlined with
                    | Some inlined -> Map.add key inlined nextInlined
                    | None -> nextInlined
                )

            let state, next =
                ((state, current), reachable)
                ||> List.fold (fun (state, next) key ->
                    let calls = state.InstanceCalls.[key]

                    let state, escaping =
                        ((state, seeds.[key]), calls.Callees)
                        ||> List.fold (fun (state, acc) (offset, callee) ->
                            let calleeEscapes = summaryOf callee

                            let state, types =
                                ((state, acc.Types), calleeEscapes.Types)
                                ||> Set.fold (fun (state, types) thrown ->
                                    if types.Contains thrown then
                                        state, types
                                    else
                                        match escapesAt state key offset (Some thrown) with
                                        | state, true -> state, Set.add thrown types
                                        | state, false -> state, types
                                )

                            let state, unknown =
                                if acc.Unknown || not calleeEscapes.Unknown then
                                    state, acc.Unknown
                                else
                                    escapesAt state key offset None

                            let state, assumes =
                                ((state, acc.Assumes), calleeEscapes.Assumes)
                                ||> Set.fold (fun (state, assumes) assumption ->
                                    if Set.contains assumption assumes then
                                        state, assumes
                                    // What a callee relies on of what values hold, its caller does
                                    // too, past every handler.
                                    elif Assumption.concernsValues assumption then
                                        state, Set.add assumption assumes
                                    else
                                        match escapesAt state key offset None with
                                        | state, true -> state, Set.add assumption assumes
                                        | state, false -> state, assumes
                                )

                            state,
                            {
                                Types = types
                                Unknown = unknown
                                Assumes = assumes
                            }
                        )

                    // What runs outside the body is past all its handlers.
                    let escaping =
                        (escaping, calls.Outside)
                        ||> List.fold (fun acc callee -> Escapes.union acc (summaryOf callee))

                    let state, rethrows = rethrown state key summaryOf

                    let state, escaping =
                        ((state, escaping), rethrows)
                        ||> List.fold (fun (state, acc) (offset, raised) ->
                            match raised with
                            | Raised.Named thrown when not (acc.Types.Contains thrown) ->
                                match escapesAt state key offset (Some thrown) with
                                | state, true ->
                                    state,
                                    { acc with
                                        Types = acc.Types.Add thrown
                                    }
                                | state, false -> state, acc
                            | Raised.Named _ -> state, acc
                            | Raised.Unnamed when not acc.Unknown ->
                                match escapesAt state key offset None with
                                | state, unknown ->
                                    state,
                                    { acc with
                                        Unknown = unknown
                                    }
                            | Raised.Unnamed -> state, acc
                            | Raised.Assumed (assumption, narrowed) when not (acc.Assumes.Contains assumption) ->
                                match escapesAt state key offset narrowed with
                                | state, true ->
                                    state,
                                    { acc with
                                        Assumes = acc.Assumes.Add assumption
                                    }
                                | state, false -> state, acc
                            | Raised.Assumed _ -> state, acc
                        )

                    let escaping =
                        match nextInlined.TryFind key with
                        | Some inlined -> Escapes.union escaping inlined
                        | None -> escaping

                    state, Map.add key escaping next
                )

            if next = current && nextInlined = currentInlined then
                state, current, currentInlined
            else
                iterate state next nextInlined

        let state, solved, solvedInlined = iterate state seeds inlinedSeeds

        let state =
            { state with
                Summaries =
                    (state.Summaries, solved)
                    ||> Map.fold (fun acc key value -> Map.add key value acc)
                InlinedSummaries =
                    (state.InlinedSummaries, solvedInlined)
                    ||> Map.fold (fun acc key value -> Map.add key value acc)
            }

        state, state.Summaries.[root]

    /// Where `unknownSources` looks for something the analysis cannot name.
    [<RequireQualifiedAccess>]
    type private UnnamedAt =
        /// Escaping this instance.
        | Escaping of MethodInstance
        /// Re-raised by the `rethrow` at this offset of this instance.
        | Rethrown of MethodInstance * offset : int
        /// Escaping every method this instance is inlined into, past all handlers.
        | Inlined of MethodInstance

    /// <summary>
    /// The places that make the answer of <c>escapes state method</c> unknown, and the state to ask
    /// the next question of.
    /// </summary>
    /// <remarks>
    /// A place is one where something the analysis cannot name may arise and then escape
    /// <c>method</c>, as <c>escapes</c> follows it: past every handler on the way, out of every
    /// method it is raised in or called through, and through every <c>rethrow</c> that re-raises it
    /// unnamed. The set is empty exactly when <c>Escapes.Unknown</c> is false. Where an assumption's
    /// contract stands in for code is not such a place: <c>Escapes.Assumes</c> lists those.
    /// </remarks>
    let unknownSources (state : EscapeAnalysisState) (method : MethodKey) : EscapeAnalysisState * Set<OpaqueSite> =
        let state, _ = escapes state method

        let instanceCalls (key : MethodInstance) : InstanceCalls =
            match state.InstanceCalls.TryFind key with
            | Some calls -> calls
            | None ->
                failwith
                    $"BUG: %O{key.Definition} is reachable from %O{method}, which escapes summarised, but its calls are not recorded"

        let site (key : MethodInstance) (offset : int option) (reason : Opacity) (names : MethodKey option) =
            {
                Method = key.Definition
                Offset = offset
                Reason = reason
                Names = names
            }

        // What arises unnamed at an offset of `key`'s body that `within` admits and gets past
        // `regions`: the places it arises at, and where else to look for what arises there.
        let reaching
            (state : EscapeAnalysisState)
            (key : MethodInstance)
            (regions : ExceptionRegion list)
            (within : int -> bool)
            : EscapeAnalysisState * OpaqueSite list * UnnamedAt list
            =
            let facts = state.InstanceFacts.[key]
            let calls = instanceCalls key
            let assembly = assemblyOf state key.Definition.AssemblyFullName

            let passes (state : EscapeAnalysisState) (offset : int) : EscapeAnalysisState * bool =
                if within offset then
                    escapesHandlers state assembly regions offset None
                else
                    state, false

            let state, sites =
                ((state, []), facts.Opaque @ calls.Undecided)
                ||> List.fold (fun (state, sites) (offset, reason, names) ->
                    match passes state offset with
                    | state, true -> state, site key (Some offset) reason names :: sites
                    | state, false -> state, sites
                )

            let onward =
                (calls.Callees
                 |> List.map (fun (offset, callee) -> offset, UnnamedAt.Escaping callee))
                @ (facts.Rethrows
                   |> List.map (fun (offset, _) -> offset, UnnamedAt.Rethrown (key, offset)))

            let state, onward =
                ((state, []), onward)
                ||> List.fold (fun (state, onward) (offset, next) ->
                    match passes state offset with
                    | state, true -> state, next :: onward
                    | state, false -> state, onward
                )

            state, sites, onward

        let rec walk
            (state : EscapeAnalysisState)
            (pending : UnnamedAt list)
            (visited : Set<UnnamedAt>)
            (sources : Set<OpaqueSite>)
            : EscapeAnalysisState * Set<OpaqueSite>
            =
            match pending with
            | [] -> state, sources
            | next :: rest when visited.Contains next -> walk state rest visited sources
            | next :: rest ->

            let state, sites, onward =
                match next with
                | UnnamedAt.Escaping key ->
                    let facts = state.InstanceFacts.[key]

                    // What happens outside the body is past all its handlers.
                    let outside =
                        facts.OutsideBody
                        |> Seq.choose (fun fact ->
                            match fact with
                            | OutsideBodyFact.Opaque reason -> Some (site key None reason None)
                            | OutsideBodyFact.Raises _
                            | OutsideBodyFact.Runs _ -> None
                        )
                        |> List.ofSeq

                    let ranOutside = (instanceCalls key).Outside |> List.map UnnamedAt.Escaping
                    let state, sites, onward = reaching state key facts.Regions (fun _ -> true)
                    state, outside @ sites, UnnamedAt.Inlined key :: ranOutside @ onward
                | UnnamedAt.Inlined key ->
                    let facts = state.InstanceFacts.[key]
                    let calls = instanceCalls key

                    let sites =
                        facts.RaisedWhereInlined
                        |> Seq.choose (fun fact ->
                            match fact with
                            | OutsideBodyFact.Opaque reason -> Some (site key None reason None)
                            | OutsideBodyFact.Raises _
                            | OutsideBodyFact.Runs _ -> None
                        )
                        |> List.ofSeq

                    let onward =
                        (calls.RaisedWhereInlined |> List.map UnnamedAt.Escaping)
                        @ (calls.Callees |> List.map (snd >> UnnamedAt.Inlined))

                    state, sites, onward
                | UnnamedAt.Rethrown (key, offset) ->
                    let facts = state.InstanceFacts.[key]
                    let assembly = assemblyOf state key.Definition.AssemblyFullName

                    let clause =
                        facts.Rethrows
                        |> List.pick (fun (at, clause) -> if at = offset then Some clause else None)

                    // It re-raises what its clause caught of what the clause's protected block
                    // raised past the clauses the runtime tries first, and that unnamed only where
                    // the clause does not narrow it.
                    match rethrownBy state assembly clause None with
                    | state, Rethrown.Unknown ->
                        let o = protectedBy clause
                        let before = facts.Regions |> List.takeWhile (fun region -> region <> clause)
                        reaching state key before (fun at -> at >= o.TryOffset && at < o.TryOffset + o.TryLength)
                    | state, Rethrown.Nothing
                    | state, Rethrown.Thrown _ -> state, [], []

            walk state (onward @ rest) (visited.Add next) (Set.union sources (Set.ofList sites))

        walk state [ UnnamedAt.Escaping (rootOf state method) ] Set.empty Set.empty

    /// The definition of every method that `escapes` follows from `method`, whether the method calls
    /// it or the runtime runs it, `method` among them.
    let internal reachable (state : EscapeAnalysisState) (method : MethodKey) : EscapeAnalysisState * Set<MethodKey> =
        let state, _ = escapes state method

        let rec walk (pending : MethodInstance list) (visited : Set<MethodInstance>) : Set<MethodInstance> =
            match pending with
            | [] -> visited
            | key :: rest when visited.Contains key -> walk rest visited
            | key :: rest ->
                match state.InstanceCalls.TryFind key with
                | Some calls -> walk ((calls.Callees |> List.map snd) @ calls.Outside @ rest) (visited.Add key)
                | None ->
                    failwith
                        $"BUG: %O{key.Definition} is reachable from %O{method}, which escapes summarised, but its calls are not recorded"

        state, walk [ rootOf state method ] Set.empty |> Set.map (fun key -> key.Definition)
