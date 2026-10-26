namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Dispatch over a type whose interfaces, overrides, MethodImpls or fields name a type that does not
/// bind, and laying out its method table, report that failure as a result: CoreCLR would have refused
/// to load the type, so no call could dispatch on it.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDispatchBindingFailure =

    /// How a type of the image fails to bind, which every dispatch question asked of it either
    /// answers without reaching, or fails with.
    [<RequireQualifiedAccess>]
    type private Expected =
        /// `[CoreLib]System.<name>`, which CoreLib does not declare.
        | Absent of name : string
        /// A type scoped to the assembly `Nowhere`, which no runtime directory supplies.
        | Unavailable

    type private Image =
        {
            Bytes : byte[]
            /// Each type the image declares, with how it fails.
            Types : (string * TypeDefinitionHandle * Expected) list
        }

    let private emit () : Image =
        let metadata = MetadataBuilder ()
        let ilStream = BlobBuilder ()

        metadata.AddModule (
            0,
            metadata.GetOrAddString "UnboundDispatch.dll",
            metadata.GetOrAddGuid (Guid "6c1f0d2e-7a3b-4c5d-8e9f-0a1b2c3d4e5f"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "UnboundDispatch",
            Version (1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            Unchecked.defaultof<AssemblyFlags>,
            AssemblyHashAlgorithm.None
        )
        |> ignore<AssemblyDefinitionHandle>

        let corelibName = typeof<obj>.Assembly.GetName ()

        let corelib : EntityHandle =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString corelibName.Name,
                corelibName.Version,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddBlob (corelibName.GetPublicKeyToken ()),
                Unchecked.defaultof<AssemblyFlags>,
                Unchecked.defaultof<BlobHandle>
            )
            |> AssemblyReferenceHandle.op_Implicit

        let nowhere : EntityHandle =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString "Nowhere",
                Version (1, 0, 0, 0),
                Unchecked.defaultof<StringHandle>,
                Unchecked.defaultof<BlobHandle>,
                Unchecked.defaultof<AssemblyFlags>,
                Unchecked.defaultof<BlobHandle>
            )
            |> AssemblyReferenceHandle.op_Implicit

        let typeRef (scope : EntityHandle) (ns : string) (name : string) : EntityHandle =
            metadata.AddTypeReference (scope, metadata.GetOrAddString ns, metadata.GetOrAddString name)
            |> TypeReferenceHandle.op_Implicit

        let object = typeRef corelib "System" "Object"
        let valueType = typeRef corelib "System" "ValueType"
        let disposable = typeRef corelib "System" "IDisposable"
        let exceptionType = typeRef corelib "System" "Exception"
        let nowhereException = typeRef nowhere "System" "Exception"
        let equatable = typeRef corelib "System" "IEquatable`1"
        let list = typeRef corelib "System.Collections.Generic" "List`1"
        let gone = typeRef corelib "System" "Gone"
        let goneGeneric = typeRef corelib "System" "Gone`1"
        let interfaceGone = typeRef corelib "System" "IGone"
        let nowhereGone = typeRef nowhere "System" "Gone"

        // `IEquatable<class [Nowhere]System.Gone>`.
        let equatableOfUnbound : EntityHandle =
            let blob = BlobBuilder ()

            let arguments =
                BlobEncoder(blob).TypeSpecificationSignature().GenericInstantiation (equatable, 1, false)

            arguments.AddArgument().Type (nowhereGone, false)

            metadata.AddTypeSpecification (metadata.GetOrAddBlob blob)
            |> TypeSpecificationHandle.op_Implicit

        let signature
            (isInstance : bool)
            (returns : ReturnTypeEncoder -> unit)
            (parameters : EntityHandle list)
            : BlobHandle
            =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(isInstanceMethod = isInstance)
                .Parameters (
                    parameters.Length,
                    returns,
                    fun (encoder : ParametersEncoder) ->
                        for parameter in parameters do
                            encoder.AddParameter().Type().Type (parameter, false)
                )

            metadata.GetOrAddBlob blob

        let returnsVoid (returns : ReturnTypeEncoder) = returns.Void ()

        // `instance void G<T>()`.
        let genericVoid =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(SignatureCallingConvention.Default, 1, true)
                .Parameters (0, returnsVoid, (fun (_ : ParametersEncoder) -> ()))

            metadata.GetOrAddBlob blob

        let instanceVoid = signature true returnsVoid []
        let takesException = signature true returnsVoid [ exceptionType ]
        let takesUnboundException = signature true returnsVoid [ nowhereException ]
        let voidOfObject = signature true returnsVoid [ object ]

        let body =
            let il = InstructionEncoder (BlobBuilder ())
            il.OpCode ILOpCode.Ret
            MethodBodyStreamEncoder(ilStream).AddMethodBody il

        let publicVirtual =
            MethodAttributes.Public
            ||| MethodAttributes.Virtual
            ||| MethodAttributes.HideBySig
            ||| MethodAttributes.NewSlot

        let publicOverride =
            MethodAttributes.Public
            ||| MethodAttributes.Virtual
            ||| MethodAttributes.HideBySig

        let privateOverride =
            MethodAttributes.Private
            ||| MethodAttributes.Virtual
            ||| MethodAttributes.Final
            ||| MethodAttributes.HideBySig
            ||| MethodAttributes.NewSlot

        let mutable nextMethod = 1

        let methodWithBody
            (body : int)
            (attributes : MethodAttributes)
            (name : string)
            (signature : BlobHandle)
            : MethodDefinitionHandle
            =
            nextMethod <- nextMethod + 1

            metadata.AddMethodDefinition (
                attributes,
                MethodImplAttributes.IL,
                metadata.GetOrAddString name,
                signature,
                body,
                MetadataTokens.ParameterHandle 1
            )

        let method = methodWithBody body

        metadata.AddTypeDefinition (
            TypeAttributes.NotPublic,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            MetadataTokens.MethodDefinitionHandle 1
        )
        |> ignore<TypeDefinitionHandle>

        // A type declaring the methods `declare` adds, after its row, which owns the methods added
        // from here to the next row.
        let declareKind
            (attributes : TypeAttributes)
            (name : string)
            (extends : EntityHandle)
            (fieldList : int)
            (declare : TypeDefinitionHandle -> unit)
            : TypeDefinitionHandle
            =
            let handle =
                metadata.AddTypeDefinition (
                    attributes,
                    metadata.GetOrAddString "N",
                    metadata.GetOrAddString name,
                    extends,
                    MetadataTokens.FieldDefinitionHandle fieldList,
                    MetadataTokens.MethodDefinitionHandle nextMethod
                )

            declare handle
            handle

        let declareType =
            declareKind (TypeAttributes.Public ||| TypeAttributes.BeforeFieldInit)

        // `interface N.ITakes { void Take(System.Exception); }`, so that a method or a declaration
        // of the same name and arity compares its parameter with a class reference of the same name.
        let takes : EntityHandle =
            declareKind
                (TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract)
                "ITakes"
                Unchecked.defaultof<EntityHandle>
                1
                (fun _ ->
                    methodWithBody
                        -1
                        (MethodAttributes.Public
                         ||| MethodAttributes.Virtual
                         ||| MethodAttributes.Abstract
                         ||| MethodAttributes.HideBySig
                         ||| MethodAttributes.NewSlot)
                        "Take"
                        takesException
                    |> ignore<MethodDefinitionHandle>
                )
            |> TypeDefinitionHandle.op_Implicit

        let implements (owner : TypeDefinitionHandle) (iface : EntityHandle) : unit =
            metadata.AddInterfaceImplementation (owner, iface)
            |> ignore<InterfaceImplementationHandle>

        let methodImpl (owner : TypeDefinitionHandle) (body : EntityHandle) (declaration : EntityHandle) =
            metadata.AddMethodImplementation (owner, body, declaration)
            |> ignore<MethodImplementationHandle>

        let memberRef (parent : EntityHandle) (name : string) (signature : BlobHandle) : EntityHandle =
            metadata.AddMemberReference (parent, metadata.GetOrAddString name, signature)
            |> MemberReferenceHandle.op_Implicit

        let implementsAbsent =
            declareType "ImplementsAbsent" object 1 (fun owner -> implements owner interfaceGone)

        let implementsUnboundArgument =
            declareType "ImplementsUnboundArgument" object 1 (fun owner -> implements owner equatableOfUnbound)

        // Placing `ITakes.Take(System.Exception)` compares it with this public virtual of the same
        // name, whose parameter names `[Nowhere]System.Exception`.
        let takesUnbound =
            declareType
                "TakesUnbound"
                object
                1
                (fun owner ->
                    implements owner takes

                    method publicVirtual "Take" takesUnboundException
                    |> ignore<MethodDefinitionHandle>
                )

        // A MethodImpl whose declaration names its method on an instantiation over an unbound type.
        let declaresOnUnboundArgument =
            declareType
                "DeclaresOnUnboundArgument"
                object
                1
                (fun owner ->
                    implements owner disposable
                    let equals = method privateOverride "Equals" voidOfObject

                    methodImpl
                        owner
                        (MethodDefinitionHandle.op_Implicit equals)
                        (memberRef equatableOfUnbound "Equals" voidOfObject)
                )

        // A MethodImpl declaring `ITakes::Take` with a parameter that names
        // `[Nowhere]System.Exception`, which comparing it with the interface's method must bind.
        let declaresUnboundSignature =
            declareType
                "DeclaresUnboundSignature"
                object
                1
                (fun owner ->
                    implements owner takes
                    let impl = method privateOverride "TakeImpl" takesException

                    methodImpl
                        owner
                        (MethodDefinitionHandle.op_Implicit impl)
                        (memberRef takes "Take" takesUnboundException)
                )

        // A MethodImpl whose body is a MemberRef on a type CoreLib does not declare.
        let bodyOnAbsent =
            declareType
                "BodyOnAbsent"
                object
                1
                (fun owner ->
                    implements owner disposable
                    method privateOverride "Body" instanceVoid |> ignore<MethodDefinitionHandle>
                    methodImpl owner (memberRef gone "Body" instanceVoid) (memberRef disposable "Dispose" instanceVoid)
                )

        // A MethodImpl whose body is a MemberRef on the type itself, whose signature names
        // `[Nowhere]System.Exception` where the method the type declares names `System.Exception`.
        let bodyUnboundSignature =
            declareType
                "BodyUnboundSignature"
                object
                1
                (fun owner ->
                    implements owner disposable
                    method privateOverride "Body" takesException |> ignore<MethodDefinitionHandle>

                    methodImpl
                        owner
                        (memberRef (TypeDefinitionHandle.op_Implicit owner) "Body" takesUnboundException)
                        (memberRef disposable "Dispose" instanceVoid)
                )

        // `interface N.IStatic { static abstract void M(System.Exception); }`.
        let staticInterface : EntityHandle =
            declareKind
                (TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract)
                "IStatic"
                Unchecked.defaultof<EntityHandle>
                1
                (fun _ ->
                    methodWithBody
                        -1
                        (MethodAttributes.Public
                         ||| MethodAttributes.Static
                         ||| MethodAttributes.Virtual
                         ||| MethodAttributes.Abstract
                         ||| MethodAttributes.HideBySig
                         ||| MethodAttributes.NewSlot)
                        "M"
                        (signature false returnsVoid [ exceptionType ])
                    |> ignore<MethodDefinitionHandle>
                )
            |> TypeDefinitionHandle.op_Implicit

        // Implements `IStatic.M` by a MethodImpl whose body's parameter names
        // `[Nowhere]System.Exception`, which instantiating the body for the call must bind.
        let staticUnbound =
            declareType
                "StaticUnbound"
                object
                1
                (fun owner ->
                    implements owner staticInterface

                    let body =
                        method
                            (MethodAttributes.Public
                             ||| MethodAttributes.Static
                             ||| MethodAttributes.HideBySig)
                            "M"
                            (signature false returnsVoid [ nowhereException ])

                    methodImpl
                        owner
                        (MethodDefinitionHandle.op_Implicit body)
                        (memberRef staticInterface "M" (signature false returnsVoid [ exceptionType ]))
                )

        // The method's one type parameter, constrained to derive from `bound`.
        let constrain (method : MethodDefinitionHandle) (bound : EntityHandle) : unit =
            let parameter =
                metadata.AddGenericParameter (
                    MethodDefinitionHandle.op_Implicit method,
                    GenericParameterAttributes.None,
                    metadata.GetOrAddString "T",
                    0
                )

            metadata.AddGenericParameterConstraint (parameter, bound)
            |> ignore<GenericParameterConstraintHandle>

        let mutable baseTake = Unchecked.defaultof<MethodDefinitionHandle>
        let mutable baseG = Unchecked.defaultof<MethodDefinitionHandle>

        // `class N.Base { virtual void Take(System.Exception); virtual void G<T>() where T : System.Exception; }`,
        // whose slots the types below override or name in a MethodImpl, so that laying out their
        // method tables compares a class reference with one of the same name.
        let baseType : EntityHandle =
            declareType
                "Base"
                object
                1
                (fun _ ->
                    baseTake <- method publicVirtual "Take" takesException
                    baseG <- method publicVirtual "G" genericVoid
                    constrain baseG exceptionType
                )
            |> TypeDefinitionHandle.op_Implicit

        // Overrides `Base.Take` by name and signature, with a parameter naming
        // `[Nowhere]System.Exception`, so that placing it in `Base`'s slot must bind that.
        let overridesUnbound =
            declareType
                "OverridesUnbound"
                baseType
                1
                (fun _ ->
                    method publicOverride "Take" takesUnboundException
                    |> ignore<MethodDefinitionHandle>
                )

        // Overrides `Base.G<T>`, constraining `T` to `[Nowhere]System.Exception`, so that placing it
        // compares that constraint with `Base.G`'s.
        let constrainsUnbound =
            declareType
                "ConstrainsUnbound"
                baseType
                1
                (fun _ ->
                    let g = method publicOverride "G" genericVoid
                    constrain g nowhereException
                )

        // A MethodImpl declaring `Base::Take` by a MemberRef whose parameter names
        // `[Nowhere]System.Exception`, which finding the slot it names compares with `Base.Take`.
        let declaresUnboundOverride =
            declareType
                "DeclaresUnboundOverride"
                baseType
                1
                (fun owner ->
                    let impl = method privateOverride "TakeImpl" takesException

                    methodImpl
                        owner
                        (MethodDefinitionHandle.op_Implicit impl)
                        (memberRef baseType "Take" takesUnboundException)
                )

        // A MethodImpl installing, in `Base.Take`'s slot, a body whose parameter names
        // `[Nowhere]System.Exception`.
        let implementsUnboundBody =
            declareType
                "ImplementsUnboundBody"
                baseType
                1
                (fun owner ->
                    let impl = method privateOverride "TakeImpl" takesUnboundException

                    methodImpl
                        owner
                        (MethodDefinitionHandle.op_Implicit impl)
                        (MethodDefinitionHandle.op_Implicit baseTake)
                )

        // A MethodImpl installing, in `Base.G<T>`'s slot, a body constraining its `T` to
        // `[Nowhere]System.Exception`.
        let implementsUnboundConstraint =
            declareType
                "ImplementsUnboundConstraint"
                baseType
                1
                (fun owner ->
                    let impl = method privateOverride "GImpl" genericVoid
                    constrain impl nowhereException

                    methodImpl
                        owner
                        (MethodDefinitionHandle.op_Implicit impl)
                        (MethodDefinitionHandle.op_Implicit baseG)
                )

        // A MethodImpl whose declaration is a MemberRef on a type CoreLib does not declare.
        let declaresOnAbsent =
            declareType
                "DeclaresOnAbsent"
                object
                1
                (fun owner ->
                    let impl = method privateOverride "MImpl" instanceVoid
                    methodImpl owner (MethodDefinitionHandle.op_Implicit impl) (memberRef gone "M" instanceVoid)
                )

        // A MethodImpl whose body is a MemberRef on `[CoreLib]System.Gone`1<object>`, which CoreLib does
        // not declare.
        let bodyOnAbsentInstantiation =
            let instantiation : EntityHandle =
                let blob = BlobBuilder ()

                let arguments =
                    BlobEncoder(blob).TypeSpecificationSignature().GenericInstantiation (goneGeneric, 1, false)

                arguments.AddArgument().Object ()

                metadata.AddTypeSpecification (metadata.GetOrAddBlob blob)
                |> TypeSpecificationHandle.op_Implicit

            declareType
                "BodyOnAbsentInstantiation"
                object
                1
                (fun owner ->
                    implements owner disposable
                    method privateOverride "Body" instanceVoid |> ignore<MethodDefinitionHandle>

                    methodImpl
                        owner
                        (memberRef instantiation "Body" instanceVoid)
                        (memberRef disposable "Dispose" instanceVoid)
                )

        // A value type with a field of a type CoreLib does not declare, the only type owning a field
        // but the one after it.
        let holdsGone =
            declareType
                "HoldsGone"
                valueType
                1
                (fun _ ->
                    let blob = BlobBuilder ()
                    BlobEncoder(blob).Field().Type().Type (gone, false)

                    metadata.AddFieldDefinition (
                        FieldAttributes.Public,
                        metadata.GetOrAddString "F",
                        metadata.GetOrAddBlob blob
                    )
                    |> ignore<FieldDefinitionHandle>
                )

        // A value type whose field is the value type above, so that the failure is its field's.
        let holdsHoldsGone =
            declareType
                "HoldsHoldsGone"
                valueType
                2
                (fun _ ->
                    let blob = BlobBuilder ()
                    BlobEncoder(blob).Field().Type().Type (TypeDefinitionHandle.op_Implicit holdsGone, true)

                    metadata.AddFieldDefinition (
                        FieldAttributes.Public,
                        metadata.GetOrAddString "G",
                        metadata.GetOrAddBlob blob
                    )
                    |> ignore<FieldDefinitionHandle>
                )

        // Derives from `OverridesUnbound`, so that laying out its parent's slots fails.
        let derivesOverridesUnbound =
            declareType "DerivesOverridesUnbound" (TypeDefinitionHandle.op_Implicit overridesUnbound) 3 ignore

        // Derives from `ImplementsUnboundBody`, so that what its parent's slots hold fails.
        let derivesImplementsUnboundBody =
            declareType "DerivesImplementsUnboundBody" (TypeDefinitionHandle.op_Implicit implementsUnboundBody) 3 ignore

        // A value type with a MethodImpl whose declaration is a MemberRef on a type CoreLib does not
        // declare.
        let valueDeclaresOnAbsent =
            declareType
                "ValueDeclaresOnAbsent"
                valueType
                3
                (fun owner ->
                    let impl = method privateOverride "MImpl" instanceVoid
                    methodImpl owner (MethodDefinitionHandle.op_Implicit impl) (memberRef gone "M" instanceVoid)
                )

        // Extends a type CoreLib does not declare.
        let extendsAbsent = declareType "ExtendsAbsent" gone 3 ignore

        // Extends `[CoreLib]System.Gone`1<object>`, which CoreLib does not declare.
        let extendsAbsentInstantiation =
            let instantiation : EntityHandle =
                let blob = BlobBuilder ()

                let arguments =
                    BlobEncoder(blob).TypeSpecificationSignature().GenericInstantiation (goneGeneric, 1, false)

                arguments.AddArgument().Object ()

                metadata.AddTypeSpecification (metadata.GetOrAddBlob blob)
                |> TypeSpecificationHandle.op_Implicit

            declareType "ExtendsAbsentInstantiation" instantiation 3 ignore

        // Extends `List<class [Nowhere]System.Gone>`: the base's definition binds, its argument does not.
        let extendsUnboundInstantiation =
            let instantiation : EntityHandle =
                let blob = BlobBuilder ()

                let arguments =
                    BlobEncoder(blob).TypeSpecificationSignature().GenericInstantiation (list, 1, false)

                arguments.AddArgument().Type (nowhereGone, false)

                metadata.AddTypeSpecification (metadata.GetOrAddBlob blob)
                |> TypeSpecificationHandle.op_Implicit

            declareType "ExtendsUnboundInstantiation" instantiation 3 ignore

        let types =
            [
                "ImplementsAbsent", implementsAbsent, Expected.Absent "IGone"
                "ImplementsUnboundArgument", implementsUnboundArgument, Expected.Unavailable
                "TakesUnbound", takesUnbound, Expected.Unavailable
                "DeclaresOnUnboundArgument", declaresOnUnboundArgument, Expected.Unavailable
                "DeclaresUnboundSignature", declaresUnboundSignature, Expected.Unavailable
                "BodyOnAbsent", bodyOnAbsent, Expected.Absent "Gone"
                "BodyUnboundSignature", bodyUnboundSignature, Expected.Unavailable
                "StaticUnbound", staticUnbound, Expected.Unavailable
                "OverridesUnbound", overridesUnbound, Expected.Unavailable
                "ConstrainsUnbound", constrainsUnbound, Expected.Unavailable
                "DeclaresUnboundOverride", declaresUnboundOverride, Expected.Unavailable
                "ImplementsUnboundBody", implementsUnboundBody, Expected.Unavailable
                "ImplementsUnboundConstraint", implementsUnboundConstraint, Expected.Unavailable
                "DeclaresOnAbsent", declaresOnAbsent, Expected.Absent "Gone"
                "BodyOnAbsentInstantiation", bodyOnAbsentInstantiation, Expected.Absent "Gone`1"
                "HoldsGone", holdsGone, Expected.Absent "Gone"
                "HoldsHoldsGone", holdsHoldsGone, Expected.Absent "Gone"
                "DerivesOverridesUnbound", derivesOverridesUnbound, Expected.Unavailable
                "DerivesImplementsUnboundBody", derivesImplementsUnboundBody, Expected.Unavailable
                "ValueDeclaresOnAbsent", valueDeclaresOnAbsent, Expected.Absent "Gone"
                "ExtendsAbsent", extendsAbsent, Expected.Absent "Gone"
                "ExtendsAbsentInstantiation", extendsAbsentInstantiation, Expected.Absent "Gone`1"
                "ExtendsUnboundInstantiation", extendsUnboundInstantiation, Expected.Unavailable
            ]

        // `<type>::Probe()`, a reference to a method no type declares, so that binding it searches each
        // type's method table and its base's.
        for _, handle, _ in types do
            memberRef (TypeDefinitionHandle.op_Implicit handle) "Probe" instanceVoid
            |> ignore<EntityHandle>

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = Characteristics.Dll),
                MetadataRootBuilder metadata,
                ilStream
            )

        let image = BlobBuilder ()
        peBuilder.Serialize image |> ignore<BlobContentId>

        {
            Bytes = image.ToArray ()
            Types = types
        }

    type private Fixture =
        {
            LoggerFactory : Microsoft.Extensions.Logging.ILoggerFactory
            BaseClassTypes : BaseClassTypes<DumpedAssembly>
            Corelib : DumpedAssembly
            Analysed : DumpedAssembly
            Types : (string * TypeDefinitionHandle * Expected) list
        }

    let private fixture () : Fixture =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let corelib =
            Assembly.readFile loggerFactory (Path.Combine (HostImage.frameworkDirectory, "System.Private.CoreLib.dll"))

        let image = emit ()

        let analysed =
            Assembly.read loggerFactory (Some "UnboundDispatch.dll") (new MemoryStream (image.Bytes))

        {
            LoggerFactory = loggerFactory
            BaseClassTypes = BaseClassTypes.ofCorelib corelib
            Corelib = corelib
            Analysed = analysed
            Types = image.Types
        }

    let private typeSystem (f : Fixture) : TypeSystemState =
        { TypeSystemState.Empty with
            _LoadedAssemblies = LoadedAssemblies.ofAssemblies [ f.Corelib ; f.Analysed ]
        }

    let private dirs = HostImage.runtimeDirs

    let private tryConcretize
        (f : Fixture)
        (assembly : DumpedAssembly)
        (handle : TypeDefinitionHandle)
        (kind : SignatureTypeKind)
        (state : TypeSystemState)
        : TypeSystemState * Result<ConcreteTypeHandle, TypeBindingFailure>
        =
        TypeSystemState.concretizeType
            f.LoggerFactory
            dirs
            f.BaseClassTypes
            state
            assembly.DefinitionFullName
            ImmutableArray.Empty
            ImmutableArray.Empty
            (TypeDefn.FromDefinition (
                ResolvedTypeIdentity.ofDefinitionInAssembly assembly.DefinitionFullName handle,
                kind
            ))

    let private concretize
        (f : Fixture)
        (assembly : DumpedAssembly)
        (handle : TypeDefinitionHandle)
        (kind : SignatureTypeKind)
        (state : TypeSystemState)
        : TypeSystemState * ConcreteTypeHandle
        =
        tryConcretize f assembly handle kind state |> BindingResult.bound

    let private corelibType (f : Fixture) (name : string) : TypeDefinitionHandle =
        f.Corelib.TypeDefs
        |> Seq.find (fun (KeyValue (_, ty)) -> ty.Namespace = "System" && ty.Name = name)
        |> _.Key

    /// The method `name` of the CoreLib type `typeName`, which declares one of that name, instantiated.
    let private corelibMethod
        (f : Fixture)
        (typeName : string)
        (name : string)
        (state : TypeSystemState)
        : TypeSystemState * WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let method =
            f.Corelib.TypeDefs.[corelibType f typeName].Methods
            |> List.find (fun m -> m.Name = name)

        let state, (concretized, _) =
            MethodConcretisation.concretizeMethodWithAllGenerics
                f.LoggerFactory
                dirs
                f.BaseClassTypes
                ImmutableArray.Empty
                method
                ImmutableArray.Empty
                state
            |> BindingResult.bound

        state, concretized

    /// `IDisposable`, and its `Dispose` instantiated.
    let private dispose
        (f : Fixture)
        (state : TypeSystemState)
        : TypeSystemState *
          ConcreteTypeHandle *
          WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let disposable = corelibType f "IDisposable"
        let state, handle = concretize f f.Corelib disposable SignatureTypeKind.Class state

        let method =
            f.Corelib.TypeDefs.[disposable].Methods
            |> List.find (fun m -> m.Name = "Dispose")

        let state, (concretized, _) =
            MethodConcretisation.concretizeMethodWithAllGenerics
                f.LoggerFactory
                dirs
                f.BaseClassTypes
                ImmutableArray.Empty
                method
                ImmutableArray.Empty
                state
            |> BindingResult.bound

        state, handle, concretized

    /// Whether `result` is `expected`'s failure. A type absent from a base chain is reported as
    /// `BaseTypeAbsent` by concretising a type, which loads its base chain, and as `TypeAbsent` by a
    /// walk that resolves the base reference itself.
    let private isExpected (expected : Expected) (result : Result<'a, TypeBindingFailure>) : bool =
        match expected, result with
        | Expected.Absent name,
          Error (TypeBindingFailure.TypeAbsent (TypeResolutionMiss.TopLevelTypeAbsent (_, Some "System", absent)))
        | Expected.Absent name,
          Error (TypeBindingFailure.BaseTypeAbsent (TypeResolutionMiss.TopLevelTypeAbsent (_, Some "System", absent))) ->
            absent = name
        | Expected.Unavailable, Error (TypeBindingFailure.AssemblyUnavailable reference) ->
            reference.Name.Name = "Nowhere"
        | _ -> false

    let private describe (result : Result<'a, TypeBindingFailure>) : string =
        match result with
        | Ok _ -> "Ok"
        | Error failure -> $"Error %s{TypeBindingFailure.describe failure}"

    /// A question PawPrint asks, as an outcome that is either a success or the failure.
    let private ask
        (name : string)
        (question : unit -> Result<'a, TypeBindingFailure>)
        : string * (unit -> Result<unit, TypeBindingFailure>)
        =
        name, (fun () -> question () |> Result.map ignore)

    /// Each walk over the method table of the definition `handle`, from a state in which nothing about
    /// it has yet been asked.
    let private definitionQuestions
        (f : Fixture)
        (handle : TypeDefinitionHandle)
        : (string * (unit -> Result<unit, TypeBindingFailure>)) list
        =
        let identity =
            ResolvedTypeIdentity.ofDefinitionInAssembly f.Analysed.DefinitionFullName handle

        let onDefinition
            (name : string)
            (question : TypeSystemState -> TypeSystemState * Result<'a, TypeBindingFailure>)
            : string * (unit -> Result<unit, TypeBindingFailure>)
            =
            ask name (fun () -> question (typeSystem f) |> snd)

        [
            onDefinition
                "vtableOfDefinition"
                (fun state ->
                    ConcreteMethodTable.vtableOfDefinition f.LoggerFactory dirs f.BaseClassTypes "test" state identity
                )
            onDefinition
                "contentVtableOfDefinition"
                (fun state ->
                    ConcreteMethodTable.contentVtableOfDefinition
                        f.LoggerFactory
                        dirs
                        f.BaseClassTypes
                        "test"
                        state
                        identity
                )
            onDefinition
                "placedSlotsOfDefinition"
                (fun state ->
                    ConcreteMethodTable.placedSlotsOfDefinition
                        f.LoggerFactory
                        dirs
                        f.BaseClassTypes
                        "test"
                        state
                        identity
                )
            onDefinition
                "slotTableOfDefinition"
                (fun state ->
                    ConcreteMethodTable.slotTableOfDefinition
                        f.LoggerFactory
                        dirs
                        f.BaseClassTypes
                        "test"
                        state
                        identity
                )
            onDefinition
                "numVirtualsOfDefinition"
                (fun state ->
                    ConcreteMethodTable.numVirtualsOfDefinition
                        f.LoggerFactory
                        dirs
                        f.BaseClassTypes
                        "test"
                        state
                        identity
                )
        ]

    /// Every dispatch and method-table question PawPrint asks of a type, from a state in which nothing
    /// about the type has yet been asked. A question about a receiver fails as concretising the
    /// receiver does.
    let private questions
        (f : Fixture)
        (handle : TypeDefinitionHandle)
        : (string * (unit -> Result<unit, TypeBindingFailure>)) list
        =

        // The state with the receiver and `IDisposable.Dispose` concretised, then `question` of them.
        let onReceiver
            (name : string)
            (question :
                TypeSystemState *
                ConcreteTypeHandle *
                ConcreteTypeHandle *
                WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
                    -> Result<'a, TypeBindingFailure>)
            : string * (unit -> Result<unit, TypeBindingFailure>)
            =
            ask
                name
                (fun () ->
                    match tryConcretize f f.Analysed handle SignatureTypeKind.Class (typeSystem f) with
                    | _, Error failure -> Error failure
                    | state, Ok receiver ->
                        let state, disposable, dispose = dispose f state
                        question (state, receiver, disposable, dispose)
                )

        // The first instance virtual the type itself declares, which a `constrained.` call can name.
        let ownVirtual () =
            let typeInfo = f.Analysed.TypeDefs.[handle]

            if typeInfo.IsInterface then
                None
            else
                typeInfo.Methods |> List.tryFind (fun m -> m.IsVirtual && not m.IsStatic)

        let corelibValueType (name : string) (state : TypeSystemState) =
            concretize f f.Corelib (corelibType f name) SignatureTypeKind.ValueType state

        [
            onReceiver
                "interfaceMapOf"
                (fun (state, receiver, _, _) ->

                    ConcreteInterfaceDispatch.interfaceMapOf
                        f.LoggerFactory
                        dirs
                        f.BaseClassTypes
                        "test"
                        state
                        receiver
                    |> snd
                )
            onReceiver
                "ownDispatchMapOf"
                (fun (state, receiver, _, _) ->

                    ConcreteInterfaceDispatch.ownDispatchMapOf
                        f.LoggerFactory
                        dirs
                        f.BaseClassTypes
                        "test"
                        state
                        receiver
                    |> snd
                )
            onReceiver
                "tryFindImplementationSlot"
                (fun (state, receiver, disposable, dispose) ->

                    ConcreteInterfaceDispatch.tryFindImplementationSlot
                        f.LoggerFactory
                        dirs
                        f.BaseClassTypes
                        "test"
                        state
                        receiver
                        true
                        disposable
                        (dispose.DeclaringAssemblyFullName, dispose.IdentityKey)
                    |> snd
                )
            onReceiver
                "interfaceMapHandles"
                (fun (state, receiver, _, _) ->

                    ConcreteVirtualDispatch.interfaceMapHandles f.LoggerFactory dirs f.BaseClassTypes state receiver
                    |> snd
                )
            onReceiver
                "tryResolveVirtualImplementation"
                (fun (state, receiver, _, dispose) ->

                    ConcreteVirtualDispatch.tryResolveVirtualImplementation
                        f.LoggerFactory
                        dirs
                        f.BaseClassTypes
                        ImmutableArray.Empty
                        dispose
                        receiver
                        true
                        state
                    |> snd
                )
            onReceiver
                "isConcreteTypeAssignableTo"
                (fun (state, receiver, disposable, _) ->

                    TypeAssignability.isConcreteTypeAssignableTo
                        f.LoggerFactory
                        dirs
                        f.BaseClassTypes
                        state
                        receiver
                        disposable
                    |> snd
                )
            onReceiver
                "containsReferences"
                (fun (state, receiver, _, _) ->

                    GcReferences.containsReferences f.LoggerFactory dirs f.BaseClassTypes state receiver
                    |> snd
                )
            onReceiver
                "resolveConstrainedStaticInterfaceMethod"
                (fun (state, receiver, _, _) ->

                    let staticInterface =
                        f.Analysed.TypeDefs |> Seq.find (fun (KeyValue (_, ty)) -> ty.Name = "IStatic")

                    let state, _ =
                        concretize f f.Analysed staticInterface.Key SignatureTypeKind.Class state

                    let m = staticInterface.Value.Methods |> List.exactlyOne

                    let state, (concretized, _) =
                        MethodConcretisation.concretizeMethodWithAllGenerics
                            f.LoggerFactory
                            dirs
                            f.BaseClassTypes
                            ImmutableArray.Empty
                            m
                            ImmutableArray.Empty
                            state
                        |> BindingResult.bound

                    StaticVirtualDispatch.resolveConstrainedStaticInterfaceMethod
                        f.LoggerFactory
                        dirs
                        f.BaseClassTypes
                        "test"
                        receiver
                        (m
                         |> MethodInfo.mapTypeGenerics (fun (par, _) ->
                             TypeDefn.GenericTypeParameter par.SequenceNumber
                         ))
                        concretized
                        state
                    |> snd
                )
            onReceiver
                "vtableOfClosed"
                (fun (state, receiver, _, _) ->

                    ConcreteMethodTable.vtableOfClosed f.LoggerFactory dirs f.BaseClassTypes "test" state receiver
                    |> snd
                )
            onReceiver
                "slotTableOfClosed"
                (fun (state, receiver, _, _) ->

                    ConcreteMethodTable.slotTableOfClosed f.LoggerFactory dirs f.BaseClassTypes "test" state receiver
                    |> snd
                )
            onReceiver
                "dispatchTableOfClosed"
                (fun (state, receiver, _, _) ->

                    ConcreteMethodTable.dispatchTableOfClosed
                        f.LoggerFactory
                        dirs
                        f.BaseClassTypes
                        "test"
                        state
                        receiver
                    |> snd
                )
            ask
                "MethodReferenceResolution.resolve"
                (fun () ->
                    let state = typeSystem f

                    let probe =
                        f.Analysed.Members
                        |> Seq.find (fun (KeyValue (_, m)) ->
                            m.PrettyName = "Probe" && m.Parent = MetadataToken.TypeDefinition handle
                        )
                        |> _.Key

                    let ctx : TypeConcretization.ConcretizationContext<DumpedAssembly> =
                        {
                            ConcreteTypes = state.ConcreteTypes
                            LoadedAssemblies = state._LoadedAssemblies
                            BaseTypes = f.BaseClassTypes
                        }

                    match
                        MethodReferenceResolution.resolve f.LoggerFactory dirs ctx f.Analysed probe
                        |> snd
                    with
                    | MethodReferenceTarget.Unbound failure -> Error failure
                    | MethodReferenceTarget.ParentTypeMissing miss -> Error (TypeBindingFailure.TypeAbsent miss)
                    | MethodReferenceTarget.ParentAssemblyUnavailable reference ->
                        Error (TypeBindingFailure.AssemblyUnavailable reference)
                    | MethodReferenceTarget.Missing -> Ok ()
                    | other -> failwith $"a reference to Probe, which no type declares, bound to %A{other}"
                )
            ask
                "ConstrainedType.resolve"
                (fun () ->
                    match ownVirtual () with
                    | None -> Ok ()
                    | Some called ->
                        let state, enum = corelibValueType "DayOfWeek" (typeSystem f)

                        ConstrainedType.resolve f.LoggerFactory dirs f.BaseClassTypes state enum called
                        |> snd
                        |> Result.map ignore
                )
            ask
                "ConstrainedType.resolveClassMethodOnValueType"
                (fun () ->
                    match ownVirtual () with
                    | None -> Ok ()
                    | Some called ->
                        let state, int32 = corelibValueType "Int32" (typeSystem f)

                        ConstrainedType.resolveClassMethodOnValueType
                            f.LoggerFactory
                            dirs
                            f.BaseClassTypes
                            state
                            int32
                            called
                        |> snd
                        |> Result.map ignore
                )
            onReceiver
                "ConstrainedType.resolve of Object.GetHashCode on the receiver"
                (fun (state, receiver, _, _) ->
                    ConstrainedType.resolve
                        f.LoggerFactory
                        dirs
                        f.BaseClassTypes
                        state
                        receiver
                        (ConstrainedType.objectGetHashCode f.BaseClassTypes)
                    |> snd
                )
            onReceiver
                "tryResolveVirtualImplementation of Object.ToString"
                (fun (state, receiver, _, _) ->
                    let state, toString = corelibMethod f "Object" "ToString" state

                    ConcreteVirtualDispatch.tryResolveVirtualImplementation
                        f.LoggerFactory
                        dirs
                        f.BaseClassTypes
                        ImmutableArray.Empty
                        toString
                        receiver
                        true
                        state
                    |> snd
                )
            onReceiver
                "ConstrainedType.resolveClassMethodOnValueType of Object.ToString"
                (fun (state, receiver, _, _) ->
                    let typeInfo = f.Analysed.TypeDefs.[handle]

                    if not (LoadedTypeInfo.isValueType f.BaseClassTypes state._LoadedAssemblies typeInfo) then
                        Ok ()
                    else
                        let toString =
                            f.Corelib.TypeDefs.[corelibType f "Object"].Methods
                            |> List.find (fun m -> m.Name = "ToString")

                        ConstrainedType.resolveClassMethodOnValueType
                            f.LoggerFactory
                            dirs
                            f.BaseClassTypes
                            state
                            receiver
                            toString
                        |> snd
                        |> Result.map ignore
                )
        ]
        @ definitionQuestions f handle

    [<Test>]
    let ``every dispatch question either answers or fails as the receiver's type fails to bind`` () : unit =
        let f = fixture ()

        let outcomes =
            [
                for name, handle, expected in f.Types do
                    for question, ask in questions f handle do
                        let outcome = ask ()

                        match outcome with
                        | Ok () -> ()
                        | Error _ when isExpected expected outcome -> ()
                        | other ->
                            failwith
                                $"%s{question} of %s{name}: expected success or %A{expected}, got %s{describe other}"

                        yield name, question, Result.isError outcome
            ]

        // Every type fails at least one question, so none of its shapes is vacuously never reached.
        for name, _, _ in f.Types do
            outcomes
            |> List.exists (fun (n, _, failed) -> n = name && failed)
            |> shouldEqual true

    /// Which question fails for which type: the one that reaches the shape the type was made for.
    let private reaches : (string * string) list =
        [
            "ImplementsAbsent", "interfaceMapOf"
            "ImplementsAbsent", "isConcreteTypeAssignableTo"
            "ImplementsAbsent", "interfaceMapHandles"
            "ImplementsUnboundArgument", "interfaceMapOf"
            "ImplementsUnboundArgument", "isConcreteTypeAssignableTo"
            "ImplementsUnboundArgument", "interfaceMapHandles"
            "TakesUnbound", "ownDispatchMapOf"
            "TakesUnbound", "tryFindImplementationSlot"
            "TakesUnbound", "tryResolveVirtualImplementation"
            "DeclaresOnUnboundArgument", "ownDispatchMapOf"
            "DeclaresUnboundSignature", "ownDispatchMapOf"
            "BodyOnAbsent", "ownDispatchMapOf"
            "BodyUnboundSignature", "ownDispatchMapOf"
            "StaticUnbound", "resolveConstrainedStaticInterfaceMethod"
            "HoldsGone", "containsReferences"
            "HoldsHoldsGone", "containsReferences"
            "OverridesUnbound", "vtableOfClosed"
            "OverridesUnbound", "slotTableOfClosed"
            "OverridesUnbound", "dispatchTableOfClosed"
            "OverridesUnbound", "MethodReferenceResolution.resolve"
            "OverridesUnbound", "ConstrainedType.resolve"
            "OverridesUnbound", "ConstrainedType.resolveClassMethodOnValueType"
            "OverridesUnbound", "tryResolveVirtualImplementation"
            "ConstrainsUnbound", "vtableOfClosed"
            "ConstrainsUnbound", "dispatchTableOfClosed"
            "ConstrainsUnbound", "MethodReferenceResolution.resolve"
            "DeclaresUnboundOverride", "dispatchTableOfClosed"
            "DeclaresUnboundOverride", "ownDispatchMapOf"
            "ImplementsUnboundBody", "dispatchTableOfClosed"
            "ImplementsUnboundConstraint", "dispatchTableOfClosed"
            "DeclaresOnAbsent", "dispatchTableOfClosed"
            "DeclaresOnAbsent", "ownDispatchMapOf"
            "BodyOnAbsentInstantiation", "ownDispatchMapOf"
            "OverridesUnbound", "tryResolveVirtualImplementation of Object.ToString"
            "OverridesUnbound", "vtableOfDefinition"
            "OverridesUnbound", "contentVtableOfDefinition"
            "OverridesUnbound", "placedSlotsOfDefinition"
            "OverridesUnbound", "slotTableOfDefinition"
            "OverridesUnbound", "numVirtualsOfDefinition"
            "ImplementsUnboundBody", "contentVtableOfDefinition"
            "DerivesOverridesUnbound", "vtableOfClosed"
            "DerivesOverridesUnbound", "dispatchTableOfClosed"
            "DerivesImplementsUnboundBody", "dispatchTableOfClosed"
            "ValueDeclaresOnAbsent", "dispatchTableOfClosed"
            "ValueDeclaresOnAbsent", "ConstrainedType.resolveClassMethodOnValueType of Object.ToString"
            "ExtendsAbsent", "interfaceMapOf"
            "ExtendsAbsent", "isConcreteTypeAssignableTo"
            "ExtendsAbsent", "dispatchTableOfClosed"
            "ExtendsAbsent", "tryResolveVirtualImplementation of Object.ToString"
            "ExtendsAbsent", "vtableOfDefinition"
            "ExtendsAbsentInstantiation", "interfaceMapOf"
            "ExtendsAbsentInstantiation", "isConcreteTypeAssignableTo"
            "ExtendsAbsentInstantiation", "dispatchTableOfClosed"
            "ExtendsAbsentInstantiation", "tryResolveVirtualImplementation of Object.ToString"
            "ExtendsAbsentInstantiation", "vtableOfDefinition"
            "ExtendsUnboundInstantiation", "interfaceMapOf"
            "ExtendsUnboundInstantiation", "isConcreteTypeAssignableTo"
            "ExtendsUnboundInstantiation", "interfaceMapHandles"
            "ExtendsUnboundInstantiation", "tryResolveVirtualImplementation of Object.ToString"
            "ExtendsUnboundInstantiation", "ConstrainedType.resolve of Object.GetHashCode on the receiver"
        ]

    [<Test>]
    let ``each shape fails the questions that reach it`` () : unit =
        let f = fixture ()

        for name, question in reaches do
            let _, handle, expected = f.Types |> List.find (fun (n, _, _) -> n = name)
            let _, ask = questions f handle |> List.find (fun (q, _) -> q = question)
            let outcome = ask ()

            if not (isExpected expected outcome) then
                failwith $"%s{question} of %s{name}: expected %A{expected}, got %s{describe outcome}"
