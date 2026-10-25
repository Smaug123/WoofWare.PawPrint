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

/// Dispatch over a type whose interfaces, MethodImpls or fields name a type that does not bind
/// reports that failure as a result: CoreCLR would have refused to load the type, so no call could
/// dispatch on it.
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
        let gone = typeRef corelib "System" "Gone"
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
            Types =
                [
                    "ImplementsAbsent", implementsAbsent, Expected.Absent "IGone"
                    "ImplementsUnboundArgument", implementsUnboundArgument, Expected.Unavailable
                    "TakesUnbound", takesUnbound, Expected.Unavailable
                    "DeclaresOnUnboundArgument", declaresOnUnboundArgument, Expected.Unavailable
                    "DeclaresUnboundSignature", declaresUnboundSignature, Expected.Unavailable
                    "BodyOnAbsent", bodyOnAbsent, Expected.Absent "Gone"
                    "BodyUnboundSignature", bodyUnboundSignature, Expected.Unavailable
                    "StaticUnbound", staticUnbound, Expected.Unavailable
                    "HoldsGone", holdsGone, Expected.Absent "Gone"
                    "HoldsHoldsGone", holdsHoldsGone, Expected.Absent "Gone"
                ]
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

    let private concretize
        (f : Fixture)
        (assembly : DumpedAssembly)
        (handle : TypeDefinitionHandle)
        (kind : SignatureTypeKind)
        (state : TypeSystemState)
        : TypeSystemState * ConcreteTypeHandle
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
        |> BindingResult.bound

    let private corelibType (f : Fixture) (name : string) : TypeDefinitionHandle =
        f.Corelib.TypeDefs
        |> Seq.find (fun (KeyValue (_, ty)) -> ty.Namespace = "System" && ty.Name = name)
        |> _.Key

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

    /// Whether `result` is `expected`'s failure.
    let private isExpected (expected : Expected) (result : Result<'a, TypeBindingFailure>) : bool =
        match expected, result with
        | Expected.Absent name,
          Error (TypeBindingFailure.TypeAbsent (TypeResolutionMiss.TopLevelTypeAbsent (_, Some "System", absent))) ->
            absent = name
        | Expected.Unavailable, Error (TypeBindingFailure.AssemblyUnavailable reference) ->
            reference.Name.Name = "Nowhere"
        | _ -> false

    let private describe (result : Result<'a, TypeBindingFailure>) : string =
        match result with
        | Ok _ -> "Ok"
        | Error failure -> $"Error %s{TypeBindingFailure.describe failure}"

    /// Every dispatch question PawPrint asks of a receiver type, each as an outcome that is either a
    /// success or the failure, from a state in which nothing about the receiver has yet been asked.
    let private questions
        (f : Fixture)
        (handle : TypeDefinitionHandle)
        : (string * (unit -> Result<unit, TypeBindingFailure>)) list
        =
        let fresh () =
            let state = typeSystem f
            let state, receiver = concretize f f.Analysed handle SignatureTypeKind.Class state
            let state, disposable, dispose = dispose f state
            state, receiver, disposable, dispose

        let ask
            (name : string)
            (question : unit -> Result<'a, TypeBindingFailure>)
            : string * (unit -> Result<unit, TypeBindingFailure>)
            =
            name, (fun () -> question () |> Result.map ignore)

        [
            ask
                "interfaceMapOf"
                (fun () ->
                    let state, receiver, _, _ = fresh ()

                    ConcreteInterfaceDispatch.interfaceMapOf
                        f.LoggerFactory
                        dirs
                        f.BaseClassTypes
                        "test"
                        state
                        receiver
                    |> snd
                )
            ask
                "ownDispatchMapOf"
                (fun () ->
                    let state, receiver, _, _ = fresh ()

                    ConcreteInterfaceDispatch.ownDispatchMapOf
                        f.LoggerFactory
                        dirs
                        f.BaseClassTypes
                        "test"
                        state
                        receiver
                    |> snd
                )
            ask
                "tryFindImplementationSlot"
                (fun () ->
                    let state, receiver, disposable, dispose = fresh ()

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
            ask
                "interfaceMapHandles"
                (fun () ->
                    let state, receiver, _, _ = fresh ()

                    ConcreteVirtualDispatch.interfaceMapHandles f.LoggerFactory dirs f.BaseClassTypes state receiver
                    |> snd
                )
            ask
                "tryResolveVirtualImplementation"
                (fun () ->
                    let state, receiver, _, dispose = fresh ()

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
            ask
                "isConcreteTypeAssignableTo"
                (fun () ->
                    let state, receiver, disposable, _ = fresh ()

                    TypeAssignability.isConcreteTypeAssignableTo
                        f.LoggerFactory
                        dirs
                        f.BaseClassTypes
                        state
                        receiver
                        disposable
                    |> snd
                )
            ask
                "containsReferences"
                (fun () ->
                    let state, receiver, _, _ = fresh ()

                    GcReferences.containsReferences f.LoggerFactory dirs f.BaseClassTypes state receiver
                    |> snd
                )
            ask
                "resolveConstrainedStaticInterfaceMethod"
                (fun () ->
                    let state, receiver, _, _ = fresh ()

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
        ]

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
