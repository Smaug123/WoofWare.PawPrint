namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open System.Runtime.Loader
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `MethodReferenceResolution` against the real runtime, over generated worlds of the shapes no
/// compiler emits: a MemberRef whose parent is a *derived* type, or a struct, or an interface, naming
/// a method some ancestor declares. The framework sweep in `TestMethodReferenceResolution` cannot
/// see these, because C# always names the declaring type.
///
/// Each world is a handful of types (classes, structs and interfaces, generic or not, classes
/// extending earlier classes through extends clauses that instantiate them), their methods, and
/// MemberRefs whose names and signatures are drawn from the same small alphabet as the methods', so
/// that many of them hit. The world is emitted as raw metadata, loaded into a collectible load
/// context on the test host, and `Module.ResolveMethod` on each MemberRef is the oracle.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMethodReferenceResolutionGenerated =

    /// A type as a signature spells it, in the vocabulary of whichever type the signature belongs to.
    [<RequireQualifiedAccess>]
    type Ty =
        | Int32
        | String
        /// Only as a primitive parent: the generator never puts it in a signature.
        | Object
        /// `!!i` of the method whose IL uses the reference; hand-written cases only.
        | MethodVar of int
        /// `modopt(System.Runtime.CompilerServices.IsConst)` on a type; hand-written cases only.
        | Modified of Ty
        /// `!i` of the type the signature belongs to.
        | Var of int
        | ArrayOf of Ty
        /// A type of the world, applied to arguments (none, for a non-generic one).
        | World of index : int * arguments : Ty list

    [<RequireQualifiedAccess>]
    type Kind =
        | Class
        | Struct
        | Interface

    [<RequireQualifiedAccess>]
    type Virtualness =
        | NonVirtual
        /// A new vtable slot.
        | NewSlot
        /// Reuses a slot of the same name and signature from an ancestor, if there is one.
        | ReuseSlot

    type GeneratedMethod =
        {
            Name : string
            IsStatic : bool
            Virtualness : Virtualness
            Parameters : Ty list
            Return : Ty option
            IsVarArg : bool
        }

    type GeneratedType =
        {
            Kind : Kind
            Arity : int
            /// For a class: the world class it extends and the arguments its extends clause gives,
            /// or `None` for `System.Object`.
            Base : (int * Ty list) option
            Methods : GeneratedMethod list
        }

    [<RequireQualifiedAccess>]
    type GeneratedReference =
        /// A MemberRef on a world type, instantiated with closed arguments when generic.
        | OnType of
            parent : int *
            arguments : Ty list *
            name : string *
            isStatic : bool *
            parameters : Ty list *
            ret : Ty option
        /// A vararg call site: parent is a MethodDef (by world type and method index), with extra
        /// arguments after the sentinel.
        | VarArg of typeIndex : int * methodIndex : int * fixedParameters : Ty list * extra : Ty list
        /// A method named on `int32[,]`: one the runtime supplies, one `System.Array` or `Object`
        /// declares, or neither.
        | OnArray of name : string * parameters : Ty list * ret : Ty option * returnsByref : bool
        /// A MemberRef whose parent is a TypeSpec spelling this type: a primitive (`ELEMENT_TYPE_I4`,
        /// which CoreCLR reads as the CoreLib type), a vector, a type variable, or any of those under
        /// a custom modifier.
        | OnTypeSpec of parent : Ty * name : string * parameters : Ty list * ret : Ty option
        /// As `OnTypeSpec`, an instance method with the vararg calling convention and no extra
        /// arguments.
        | OnTypeSpecVarArg of parent : Ty * name : string * parameters : Ty list

    type World =
        {
            Types : GeneratedType list
            References : GeneratedReference list
        }

    let private names : string list =
        [ "M" ; "M" ; "N" ; ".ctor" ; "ToString" ; "GetHashCode" ]

    let private pick (random : Random) (xs : 'a list) : 'a = xs.[random.Next xs.Length]

    /// A type in the vocabulary of something with `arity` type variables. Kept to a small alphabet
    /// so that signatures drawn independently often coincide.
    let rec private genTy (random : Random) (arity : int) (depth : int) : Ty =
        match random.Next (if depth > 0 then 3 else 4) with
        | 0 -> Ty.Int32
        | 1 -> Ty.String
        | 2 when arity > 0 -> Ty.Var (random.Next arity)
        | 2 -> Ty.Int32
        | _ -> Ty.ArrayOf (genTy random arity (depth + 1))

    let private genClosed (random : Random) : Ty =
        pick random [ Ty.Int32 ; Ty.String ; Ty.ArrayOf Ty.Int32 ]

    let private genSignature (random : Random) (arity : int) : Ty list * Ty option =
        let parameters = List.init (random.Next 3) (fun _ -> genTy random arity 0)

        let ret =
            match random.Next 3 with
            | 0 -> None
            | _ -> Some (genTy random arity 0)

        parameters, ret

    let private genMethod (random : Random) (kind : Kind) (arity : int) : GeneratedMethod =
        let name = pick random names

        if name = ".ctor" then
            {
                Name = name
                IsStatic = false
                Virtualness = Virtualness.NonVirtual
                Parameters = List.init (random.Next 2) (fun _ -> genTy random arity 0)
                Return = None
                IsVarArg = false
            }
        else
            let isStatic = random.Next 4 = 0
            let parameters, ret = genSignature random arity

            let virtualness =
                match kind, isStatic with
                | Kind.Interface, false -> Virtualness.NewSlot
                | _, true -> Virtualness.NonVirtual
                | Kind.Struct, false -> pick random [ Virtualness.NonVirtual ; Virtualness.ReuseSlot ]
                | Kind.Class, false ->
                    pick random [ Virtualness.NonVirtual ; Virtualness.NewSlot ; Virtualness.ReuseSlot ]

            {
                Name = name
                IsStatic = isStatic
                Virtualness = virtualness
                Parameters = parameters
                Return = ret
                IsVarArg = false
            }

    let private genWorld (random : Random) : World =
        let typeCount = 1 + random.Next 4

        let types =
            (([] : GeneratedType list), [ 0 .. typeCount - 1 ])
            ||> List.fold (fun earlier _ ->
                let kind =
                    match random.Next 5 with
                    | 0 -> Kind.Struct
                    | 1 -> Kind.Interface
                    | _ -> Kind.Class

                // Differing arities between a type and its base are what separate a positional
                // reading of `!i` from one through the extends clause.
                let arity = random.Next 3

                let baseType =
                    let classes =
                        earlier |> List.indexed |> List.filter (fun (_, t) -> t.Kind = Kind.Class)

                    match kind with
                    | Kind.Class when not classes.IsEmpty && random.Next 4 > 0 ->
                        let index, baseDef = pick random classes
                        Some (index, List.init baseDef.Arity (fun _ -> genTy random arity 0))
                    | _ -> None

                let methods =
                    List.init (random.Next 5) (fun _ -> genMethod random kind arity)
                    // An interface has no instance constructor.
                    |> List.filter (fun m -> not (kind = Kind.Interface && m.Name = ".ctor"))
                    |> List.distinctBy (fun m -> m.Name, m.IsStatic, m.Parameters, m.Return)
                    // A static and an instance method differing only there is legal metadata, but
                    // keep one per name and parameter list so a load failure is about something else.
                    |> List.distinctBy (fun m -> m.Name, m.Parameters, m.Return)

                let methods =
                    // A vararg method, for the MethodDef-parented references to aim at.
                    if kind = Kind.Class && arity = 0 && random.Next 3 = 0 then
                        methods
                        @ [
                            {
                                Name = "V"
                                IsStatic = true
                                Virtualness = Virtualness.NonVirtual
                                Parameters = List.init (random.Next 2) (fun _ -> genTy random 0 0)
                                Return = None
                                IsVarArg = true
                            }
                        ]
                    else
                        methods

                earlier
                @ [
                    {
                        Kind = kind
                        Arity = arity
                        Base = baseType
                        Methods = methods
                    }
                ]
            )

        let references =
            List.init
                (4 + random.Next 8)
                (fun _ ->
                    match random.Next 12 with
                    | 0 ->
                        let name =
                            pick random [ "Get" ; "Set" ; "Address" ; ".ctor" ; "GetLength" ; "ToString" ]

                        let parameters = List.init (random.Next 5) (fun _ -> Ty.Int32)

                        let ret =
                            if name = ".ctor" then
                                None
                            else
                                pick random [ None ; Some Ty.Int32 ; Some Ty.String ]

                        GeneratedReference.OnArray (name, parameters, ret, name = "Address" && random.Next 2 = 0)
                    | 2 ->
                        let parent = pick random [ Ty.Int32 ; Ty.String ; Ty.Object ]

                        let name =
                            pick random [ "ToString" ; "GetHashCode" ; "CompareTo" ; "get_Length" ; "MemberwiseClone" ]

                        let parameters =
                            List.init (random.Next 2) (fun _ -> pick random [ Ty.Int32 ; Ty.String ])

                        let ret = pick random [ None ; Some Ty.Int32 ; Some Ty.String ; Some Ty.Object ]
                        GeneratedReference.OnTypeSpec (parent, name, parameters, ret)
                    | 1 ->
                        let varargs =
                            types
                            |> List.indexed
                            |> List.collect (fun (ti, t) ->
                                t.Methods
                                |> List.indexed
                                |> List.filter (fun (_, m) -> m.IsVarArg)
                                |> List.map (fun (mi, m) -> ti, mi, m)
                            )

                        match varargs with
                        | [] ->
                            let parent = random.Next types.Length
                            let parameters, ret = genSignature random types.[parent].Arity

                            GeneratedReference.OnType (
                                parent,
                                List.init types.[parent].Arity (fun _ -> genClosed random),
                                pick random names,
                                false,
                                parameters,
                                ret
                            )
                        | _ ->
                            let ti, mi, m = pick random varargs

                            let fixedParameters =
                                if random.Next 3 = 0 then
                                    List.init (random.Next 2) (fun _ -> genTy random 0 0)
                                else
                                    m.Parameters

                            GeneratedReference.VarArg (
                                ti,
                                mi,
                                fixedParameters,
                                List.init (random.Next 3) (fun _ -> Ty.String)
                            )
                    | n when n < 7 ->
                        // A method of the parent or one of its ancestors, its signature copied as
                        // that type spells it. Where the extends clauses map the ancestor's `!i`
                        // to the parent's own, it names that method; where they do not, it is a
                        // near miss, which is the case that tells positional reading from
                        // substitution apart.
                        let parent = random.Next types.Length

                        let rec chain (index : int) : int list =
                            match types.[index].Base with
                            | Some (baseIndex, _) -> index :: chain baseIndex
                            | None -> [ index ]

                        let arity = types.[parent].Arity

                        let rec respell (ty : Ty) : Ty =
                            match ty with
                            | Ty.Var i when i >= arity -> Ty.Int32
                            | Ty.ArrayOf element -> Ty.ArrayOf (respell element)
                            | other -> other

                        let candidates =
                            chain parent
                            |> List.collect (fun index -> types.[index].Methods)
                            |> List.filter (fun m -> not m.IsVarArg)

                        let arguments = List.init arity (fun _ -> genClosed random)

                        match candidates with
                        | [] -> GeneratedReference.OnType (parent, arguments, "M", false, [], None)
                        | _ ->
                            let m = pick random candidates

                            GeneratedReference.OnType (
                                parent,
                                arguments,
                                m.Name,
                                m.IsStatic,
                                List.map respell m.Parameters,
                                Option.map respell m.Return
                            )
                    | _ ->
                        let parent = random.Next types.Length
                        let name = pick random names
                        let parameters, ret = genSignature random types.[parent].Arity
                        let isStatic = name <> ".ctor" && random.Next 4 = 0

                        GeneratedReference.OnType (
                            parent,
                            List.init types.[parent].Arity (fun _ -> genClosed random),
                            name,
                            isStatic,
                            (if name = ".ctor" then
                                 List.truncate 1 parameters
                             else
                                 parameters),
                            (if name = ".ctor" then None else ret)
                        )
                )

        {
            Types = types
            References = references
        }

    /// The emitted image, and the MemberRef handle of each of `world.References`, in order.
    let private emit (seed : int) (assemblyName : string) (world : World) : byte[] * MemberReferenceHandle list =
        let metadata = MetadataBuilder ()
        let ilStream = BlobBuilder ()
        let bodies = MethodBodyStreamEncoder ilStream

        metadata.AddModule (
            0,
            metadata.GetOrAddString (assemblyName + ".dll"),
            metadata.GetOrAddGuid (Guid (seed, 0s, 0s, Array.zeroCreate 8)),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString assemblyName,
            Version (1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            Unchecked.defaultof<AssemblyFlags>,
            AssemblyHashAlgorithm.None
        )
        |> ignore<AssemblyDefinitionHandle>

        let corelibName = typeof<obj>.Assembly.GetName ()

        let corelibRef =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString corelibName.Name,
                corelibName.Version,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddBlob (corelibName.GetPublicKeyToken ()),
                Unchecked.defaultof<AssemblyFlags>,
                Unchecked.defaultof<BlobHandle>
            )

        let corelibType (name : string) : EntityHandle =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit corelibRef : EntityHandle),
                metadata.GetOrAddString "System",
                metadata.GetOrAddString name
            )
            |> TypeReferenceHandle.op_Implicit

        let objectRef = corelibType "Object"
        let valueTypeRef = corelibType "ValueType"

        let isConstRef : EntityHandle =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit corelibRef : EntityHandle),
                metadata.GetOrAddString "System.Runtime.CompilerServices",
                metadata.GetOrAddString "IsConst"
            )
            |> TypeReferenceHandle.op_Implicit

        // Row 1 is `<Module>`; world type i is row i + 2.
        let typeHandle (index : int) : EntityHandle =
            MetadataTokens.TypeDefinitionHandle (index + 2)
            |> TypeDefinitionHandle.op_Implicit

        let isValueType (index : int) : bool = world.Types.[index].Kind = Kind.Struct

        let rec encode (encoder : SignatureTypeEncoder) (ty : Ty) : unit =
            match ty with
            | Ty.Int32 -> encoder.Int32 ()
            | Ty.String -> encoder.String ()
            | Ty.Object -> encoder.Object ()
            | Ty.Var i -> encoder.GenericTypeParameter i
            | Ty.MethodVar i -> encoder.GenericMethodTypeParameter i
            | Ty.Modified inner ->
                encoder.CustomModifiers().AddModifier (isConstRef, true)
                |> ignore<CustomModifiersEncoder>

                encode encoder inner
            | Ty.ArrayOf element -> encode (encoder.SZArray ()) element
            | Ty.World (index, []) -> encoder.Type (typeHandle index, isValueType index)
            | Ty.World (index, arguments) ->
                let args =
                    encoder.GenericInstantiation (typeHandle index, arguments.Length, isValueType index)

                for a in arguments do
                    encode (args.AddArgument ()) a

        let signatureReturningByref
            (convention : SignatureCallingConvention)
            (isStatic : bool)
            (parameters : Ty list)
            (extra : Ty list)
            (ret : Ty option)
            (returnsByref : bool)
            : BlobHandle
            =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(convention, 0, not isStatic)
                .Parameters (
                    parameters.Length + extra.Length,
                    (fun returnType ->
                        match ret with
                        | None -> returnType.Void ()
                        | Some ty -> encode (returnType.Type returnsByref) ty
                    ),
                    (fun ps ->
                        for p in parameters do
                            encode (ps.AddParameter().Type ()) p

                        if not extra.IsEmpty then
                            let rest = ps.StartVarArgs ()

                            for p in extra do
                                encode (rest.AddParameter().Type ()) p
                    )
                )

            metadata.GetOrAddBlob blob

        let signature convention isStatic parameters extra ret =
            signatureReturningByref convention isStatic parameters extra ret false

        let typeSpec (ty : Ty) : EntityHandle =
            let blob = BlobBuilder ()
            encode (BlobEncoder(blob).TypeSpecificationSignature ()) ty
            TypeSpecificationHandle.op_Implicit (metadata.AddTypeSpecification (metadata.GetOrAddBlob blob))

        // `ldnull; throw` is a valid body whatever the signature, and nothing runs it.
        let throwingBody =
            let code = InstructionEncoder (BlobBuilder ())
            code.OpCode ILOpCode.Ldnull
            code.OpCode ILOpCode.Throw
            bodies.AddMethodBody code

        metadata.AddTypeDefinition (
            TypeAttributes.Class,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            MetadataTokens.MethodDefinitionHandle 1
        )
        |> ignore<TypeDefinitionHandle>

        let mutable nextMethodRow = 1

        let methodRows =
            Collections.Generic.Dictionary<int * int, MethodDefinitionHandle> ()

        for ti, ty in List.indexed world.Types do
            let firstMethod = MetadataTokens.MethodDefinitionHandle nextMethodRow

            for mi, m in List.indexed ty.Methods do
                let attributes =
                    let access = MethodAttributes.Public ||| MethodAttributes.HideBySig

                    let special =
                        if m.Name = ".ctor" then
                            MethodAttributes.SpecialName ||| MethodAttributes.RTSpecialName
                        else
                            enum 0

                    let dispatch =
                        match m.Virtualness with
                        | Virtualness.NonVirtual -> enum 0
                        | Virtualness.NewSlot -> MethodAttributes.Virtual ||| MethodAttributes.NewSlot
                        | Virtualness.ReuseSlot -> MethodAttributes.Virtual

                    let abstractness =
                        if ty.Kind = Kind.Interface && not m.IsStatic then
                            MethodAttributes.Abstract
                        else
                            enum 0

                    let staticness = if m.IsStatic then MethodAttributes.Static else enum 0
                    access ||| special ||| dispatch ||| abstractness ||| staticness

                let convention =
                    if m.IsVarArg then
                        SignatureCallingConvention.VarArgs
                    else
                        SignatureCallingConvention.Default

                let handle =
                    metadata.AddMethodDefinition (
                        attributes,
                        MethodImplAttributes.IL,
                        metadata.GetOrAddString m.Name,
                        signature convention m.IsStatic m.Parameters [] m.Return,
                        (if attributes.HasFlag MethodAttributes.Abstract then
                             -1
                         else
                             throwingBody),
                        MetadataTokens.ParameterHandle 1
                    )

                methodRows.[(ti, mi)] <- handle
                nextMethodRow <- nextMethodRow + 1

            let attributes, baseType =
                match ty.Kind with
                | Kind.Class -> TypeAttributes.Public ||| TypeAttributes.Class, None
                | Kind.Struct ->
                    TypeAttributes.Public
                    ||| TypeAttributes.Sealed
                    ||| TypeAttributes.SequentialLayout,
                    Some valueTypeRef
                | Kind.Interface ->
                    TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract,
                    Some Unchecked.defaultof<EntityHandle>

            let baseType =
                match baseType, ty.Base with
                | Some b, _ -> b
                | None, None -> objectRef
                | None, Some (index, []) -> typeHandle index
                | None, Some (index, arguments) -> typeSpec (Ty.World (index, arguments))

            let name = if ty.Arity = 0 then $"T%d{ti}" else $"T%d{ti}`%d{ty.Arity}"

            metadata.AddTypeDefinition (
                attributes,
                metadata.GetOrAddString "W",
                metadata.GetOrAddString name,
                baseType,
                MetadataTokens.FieldDefinitionHandle 1,
                firstMethod
            )
            |> ignore<TypeDefinitionHandle>

        for ti, ty in List.indexed world.Types do
            for p in 0 .. ty.Arity - 1 do
                metadata.AddGenericParameter (
                    (typeHandle ti),
                    GenericParameterAttributes.None,
                    metadata.GetOrAddString $"G%d{p}",
                    p
                )
                |> ignore<GenericParameterHandle>

        let references =
            world.References
            |> List.map (fun reference ->
                match reference with
                | GeneratedReference.OnType (parent, arguments, name, isStatic, parameters, ret) ->
                    let parentHandle =
                        if arguments.IsEmpty then
                            typeHandle parent
                        else
                            typeSpec (Ty.World (parent, arguments))

                    metadata.AddMemberReference (
                        parentHandle,
                        metadata.GetOrAddString name,
                        signature SignatureCallingConvention.Default isStatic parameters [] ret
                    )
                | GeneratedReference.VarArg (ti, mi, fixedParameters, extra) ->
                    metadata.AddMemberReference (
                        (MethodDefinitionHandle.op_Implicit methodRows.[(ti, mi)] : EntityHandle),
                        metadata.GetOrAddString "V",
                        signature SignatureCallingConvention.VarArgs true fixedParameters extra None
                    )
                | GeneratedReference.OnTypeSpec (parent, name, parameters, ret) ->
                    metadata.AddMemberReference (
                        typeSpec parent,
                        metadata.GetOrAddString name,
                        signature SignatureCallingConvention.Default false parameters [] ret
                    )
                | GeneratedReference.OnTypeSpecVarArg (parent, name, parameters) ->
                    metadata.AddMemberReference (
                        typeSpec parent,
                        metadata.GetOrAddString name,
                        signature SignatureCallingConvention.VarArgs false parameters [] None
                    )
                | GeneratedReference.OnArray (name, parameters, ret, returnsByref) ->
                    let blob = BlobBuilder ()

                    BlobEncoder(blob)
                        .TypeSpecificationSignature()
                        .Array (
                            (fun element -> element.Int32 ()),
                            (fun shape -> shape.Shape (2, ImmutableArray.Empty, ImmutableArray.Create (0, 0)))
                        )

                    let parent : EntityHandle =
                        metadata.AddTypeSpecification (metadata.GetOrAddBlob blob)
                        |> TypeSpecificationHandle.op_Implicit

                    metadata.AddMemberReference (
                        parent,
                        metadata.GetOrAddString name,
                        signatureReturningByref SignatureCallingConvention.Default false parameters [] ret returnsByref
                    )
            )

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = Characteristics.Dll),
                MetadataRootBuilder metadata,
                ilStream
            )

        let image = BlobBuilder ()
        peBuilder.Serialize image |> ignore<BlobContentId>
        image.ToArray (), references

    let private newSlot (name : string) (parameters : Ty list) : GeneratedMethod =
        {
            Name = name
            IsStatic = false
            Virtualness = Virtualness.NewSlot
            Parameters = parameters
            Return = None
            IsVarArg = false
        }

    /// Shapes the generator is unlikely to reach by chance, each of which an earlier version of the
    /// resolver got wrong.
    let handWritten : (string * World) list =
        [
            // `FindMethod` walks the vtable slots from the end, not the ancestry. With `A<T>.M(T)`,
            // `B<T> : A<T>` adding a new slot `M(int)`, `C<T> : B<T>` overriding `M(T)`, and
            // `D : C<int>`, both of D's `M` slots read as `M(int)`, and the later one is `B.M`'s,
            // although `C.M` is the more derived declaration.
            "slot order under substitution",
            {
                Types =
                    [
                        {
                            Kind = Kind.Class
                            Arity = 1
                            Base = None
                            Methods = [ newSlot "M" [ Ty.Var 0 ] ]
                        }
                        {
                            Kind = Kind.Class
                            Arity = 1
                            Base = Some (0, [ Ty.Var 0 ])
                            Methods = [ newSlot "M" [ Ty.Int32 ] ]
                        }
                        {
                            Kind = Kind.Class
                            Arity = 1
                            Base = Some (1, [ Ty.Var 0 ])
                            Methods =
                                [
                                    { newSlot "M" [ Ty.Var 0 ] with
                                        Virtualness = Virtualness.ReuseSlot
                                    }
                                ]
                        }
                        {
                            Kind = Kind.Class
                            Arity = 0
                            Base = Some (2, [ Ty.Int32 ])
                            Methods = []
                        }
                    ]
                References = [ GeneratedReference.OnType (3, [], "M", false, [ Ty.Int32 ], None) ]
            }
            // Two overloads of a generic base that coincide once a derived type's extends clause
            // closes it: CoreCLR takes whichever it meets first searching from the end.
            "non-virtual overloads coinciding under substitution",
            {
                Types =
                    [
                        {
                            Kind = Kind.Class
                            Arity = 1
                            Base = None
                            Methods =
                                [
                                    { newSlot "M" [ Ty.Var 0 ] with
                                        Virtualness = Virtualness.NonVirtual
                                    }
                                    { newSlot "M" [ Ty.Int32 ] with
                                        Virtualness = Virtualness.NonVirtual
                                    }
                                ]
                        }
                        {
                            Kind = Kind.Class
                            Arity = 0
                            Base = Some (0, [ Ty.Int32 ])
                            Methods = []
                        }
                    ]
                References = [ GeneratedReference.OnType (1, [], "M", false, [ Ty.Int32 ], None) ]
            }
            "virtual overloads coinciding under substitution",
            {
                Types =
                    [
                        {
                            Kind = Kind.Class
                            Arity = 1
                            Base = None
                            Methods = [ newSlot "M" [ Ty.Var 0 ] ; newSlot "M" [ Ty.Int32 ] ]
                        }
                        {
                            Kind = Kind.Class
                            Arity = 0
                            Base = Some (0, [ Ty.Int32 ])
                            Methods = []
                        }
                    ]
                References = [ GeneratedReference.OnType (1, [], "M", false, [ Ty.Int32 ], None) ]
            }
            "primitive parents",
            {
                Types = []
                References =
                    [
                        GeneratedReference.OnTypeSpec (Ty.Int32, "ToString", [], Some Ty.String)
                        GeneratedReference.OnTypeSpec (Ty.Int32, "CompareTo", [ Ty.Int32 ], Some Ty.Int32)
                        GeneratedReference.OnTypeSpec (Ty.Object, "ToString", [], Some Ty.String)
                        GeneratedReference.OnTypeSpec (Ty.Object, "GetHashCode", [], Some Ty.Int32)
                        GeneratedReference.OnTypeSpec (Ty.String, "get_Length", [], Some Ty.Int32)
                        // Object's non-virtual methods are found from a class and not from a
                        // struct, whose search stops at its own method table.
                        GeneratedReference.OnTypeSpec (Ty.Int32, "MemberwiseClone", [], Some Ty.Object)
                        GeneratedReference.OnTypeSpec (Ty.String, "MemberwiseClone", [], Some Ty.Object)
                    ]
            }
            // A custom modifier annotates a signature without changing the type it describes, so a
            // parent under one is the parent without it.
            "parents under custom modifiers",
            {
                Types = []
                References =
                    [
                        GeneratedReference.OnTypeSpec (Ty.Modified Ty.Object, "ToString", [], Some Ty.String)
                        GeneratedReference.OnTypeSpec (
                            Ty.ArrayOf (Ty.Modified (Ty.ArrayOf Ty.Int32)),
                            ".ctor",
                            [ Ty.Int32 ; Ty.Int32 ],
                            None
                        )
                        GeneratedReference.OnTypeSpec (
                            Ty.Modified (Ty.ArrayOf Ty.Int32),
                            "Get",
                            [ Ty.Int32 ],
                            Some Ty.Int32
                        )
                        GeneratedReference.OnTypeSpec (
                            Ty.ArrayOf (Ty.ArrayOf Ty.Int32),
                            ".ctor",
                            [ Ty.Int32 ; Ty.Int32 ],
                            None
                        )
                        GeneratedReference.OnTypeSpec (
                            Ty.ArrayOf (Ty.ArrayOf Ty.Int32),
                            ".ctor",
                            [ Ty.Int32 ; Ty.Int32 ; Ty.Int32 ],
                            None
                        )
                    ]
            }
            "what structs and interfaces inherit",
            {
                Types =
                    [
                        {
                            Kind = Kind.Struct
                            Arity = 0
                            Base = None
                            Methods = []
                        }
                        {
                            Kind = Kind.Interface
                            Arity = 0
                            Base = None
                            Methods = []
                        }
                        {
                            Kind = Kind.Class
                            Arity = 0
                            Base = None
                            Methods = []
                        }
                    ]
                References =
                    [
                        GeneratedReference.OnType (0, [], "ToString", false, [], Some Ty.String)
                        GeneratedReference.OnType (0, [], "MemberwiseClone", false, [], Some Ty.Object)
                        GeneratedReference.OnType (1, [], "ToString", false, [], Some Ty.String)
                        GeneratedReference.OnType (2, [], "MemberwiseClone", false, [], Some Ty.Object)
                    ]
            }
        ]

    [<RequireQualifiedAccess>]
    type private Oracle =
        | Method of MethodBase
        | Missing
        /// The world is one CoreCLR will not load at all, so it says nothing about the reference.
        | Refused of exn

    let private askReflection (m : Module) (token : int) : Oracle =
        try
            Oracle.Method (m.ResolveMethod token)
        with
        | :? MissingMethodException -> Oracle.Missing
        | :? TypeLoadException
        | :? BadImageFormatException as e -> Oracle.Refused e

    [<Test>]
    let ``MemberRefs in generated worlds resolve where the real runtime binds them`` () : unit =
        let frameworkDir = FrameworkUnderTest.sharedFrameworkDirectory ()
        let runtimeDirs = FrameworkUnderTest.runtimeDirs ()
        let _, loggerFactory = LoggerFactory.makeTest ()

        let corelib =
            Assembly.readFile loggerFactory (Path.Combine (frameworkDir, "System.Private.CoreLib.dll"))

        let baseClassTypes = BaseClassTypes.ofCorelib corelib

        let failures = ResizeArray<string> ()
        let outcomes = Collections.Generic.Dictionary<string, int> ()

        let count (outcome : string) =
            outcomes.[outcome] <-
                match outcomes.TryGetValue outcome with
                | true, n -> n + 1
                | false, _ -> 1

        let worlds =
            [ for seed in 0..599 -> $"seed %d{seed}", genWorld (Random seed) ] @ handWritten

        for index, (label, world) in List.indexed worlds do
            let assemblyName = $"GeneratedWorld%d{index}"
            let image, references = emit index assemblyName world

            let context = AssemblyLoadContext (assemblyName, isCollectible = true)

            try
                let reflected = context.LoadFromStream (new MemoryStream (image))

                let analysed =
                    Assembly.read loggerFactory (Some $"%s{assemblyName}.dll") (new MemoryStream (image))

                let loaded = LoadedAssemblies.ofAssemblies [ corelib ; analysed ]

                let mutable ctx : TypeConcretization.ConcretizationContext<DumpedAssembly> =
                    {
                        ConcreteTypes = Corelib.concretizeAll loaded baseClassTypes AllConcreteTypes.Empty
                        LoadedAssemblies = loaded
                        BaseTypes = baseClassTypes
                    }

                for reference, handle in List.zip world.References references do
                    let token =
                        MetadataTokens.GetToken (MemberReferenceHandle.op_Implicit handle : EntityHandle)

                    match askReflection reflected.ManifestModule token with
                    | Oracle.Refused _ -> count "world refused by CoreCLR"
                    | oracle ->
                        if index >= 600 then
                            count "hand-written reference compared"

                        let describe () =
                            $"%s{label}, reference %A{reference}%s{Environment.NewLine}world %A{world.Types}"

                        let ours =
                            try
                                let ctx', ours =
                                    MethodReferenceResolution.resolve
                                        loggerFactory
                                        runtimeDirs
                                        ctx
                                        (ctx.LoadedAssemblies.ByDefinitionName analysed.DefinitionFullName)
                                        handle

                                ctx <- ctx'
                                Ok ours
                            with e ->
                                Error e

                        match oracle, ours with
                        | Oracle.Method mb, Ok (MethodReferenceTarget.Defined (declaringAssembly, method)) ->
                            let theirs = mb.Module.Assembly.GetName().Name, mb.MetadataToken

                            let ours =
                                declaringAssembly.Name.Name,
                                MetadataTokens.GetToken (MethodDefinitionHandle.op_Implicit method : EntityHandle)

                            if theirs = ours then
                                if mb.Module.Assembly <> reflected then
                                    count "defined in CoreLib"
                                else
                                    match reference with
                                    | GeneratedReference.OnType (parent, _, _, _, _, _) when
                                        mb.DeclaringType.Name.Split('`').[0] <> $"T%d{parent}"
                                        ->
                                        count "defined on an ancestor in this world"
                                    | _ -> count "defined here"
                            else
                                failures.Add
                                    $"runtime binds %s{mb.DeclaringType.FullName}::%s{mb.Name} %A{theirs}, resolver %A{ours}: %s{describe ()}"
                        | Oracle.Method mb, Ok (MethodReferenceTarget.ArrayMethod (_, accessor)) when
                            TestMethodReferenceResolution.arrayAccessorIs accessor mb
                            ->
                            count "array method"
                        | Oracle.Missing, Ok MethodReferenceTarget.Missing -> count "missing"
                        | oracle, ours -> failures.Add $"runtime %A{oracle}, resolver %A{ours}: %s{describe ()}"
            finally
                context.Unload ()

        for KeyValue (outcome, n) in outcomes do
            TestContext.Progress.WriteLine $"%s{outcome}: %d{n}"

        if failures.Count > 0 then
            failwith (
                $"%d{failures.Count} disagreements; the first few:%s{Environment.NewLine}"
                + (failures
                   |> Seq.truncate 5
                   |> String.concat (Environment.NewLine + Environment.NewLine))
            )

        // Every hand-written reference is a case an earlier resolver got wrong, so each must have
        // been put to the runtime rather than refused.
        match outcomes.TryGetValue "hand-written reference compared" with
        | true, n ->
            n
            |> shouldEqual (handWritten |> List.sumBy (fun (_, world) -> world.References.Length))
        | false, _ -> failwith "no hand-written reference was compared"

        // Vacuity guards: each kind of answer must actually have been compared.
        for outcome in
            [
                "defined here"
                "defined on an ancestor in this world"
                "defined in CoreLib"
                "array method"
                "missing"
            ] do
            match outcomes.TryGetValue outcome with
            | true, n -> n |> shouldBeGreaterThan 20
            | false, _ -> failwith $"no generated reference came out as %s{outcome}"

    /// References whose target no reading of the spelling alone can decide, because a type variable
    /// of the context that uses them stands where the answer is: a vector of `!!0` has one
    /// constructor per level of vector nesting in `!!0`, and `!0::ToString` is whichever `ToString`
    /// the instantiation has. Each is asked of the runtime under two instantiations. Where the
    /// resolver says the answer depends on the instantiation, the two must differ; where it answers
    /// outright, both must be that answer.
    [<Test>]
    let ``a reference whose target depends on the instantiation says so`` () : unit =
        let frameworkDir = FrameworkUnderTest.sharedFrameworkDirectory ()
        let runtimeDirs = FrameworkUnderTest.runtimeDirs ()
        let _, loggerFactory = LoggerFactory.makeTest ()

        let corelib =
            Assembly.readFile loggerFactory (Path.Combine (frameworkDir, "System.Private.CoreLib.dll"))

        let baseClassTypes = BaseClassTypes.ofCorelib corelib

        let cases : (GeneratedReference * Type * Type * bool) list =
            [
                // (reference, first instantiation, second, whether the resolver should defer)
                GeneratedReference.OnTypeSpec (Ty.ArrayOf (Ty.MethodVar 0), ".ctor", [ Ty.Int32 ; Ty.Int32 ], None),
                typeof<int>,
                typeof<int[]>,
                true
                GeneratedReference.OnTypeSpec (Ty.ArrayOf (Ty.MethodVar 0), ".ctor", [ Ty.Int32 ], None),
                typeof<int>,
                typeof<int[]>,
                false
                GeneratedReference.OnTypeSpec (Ty.ArrayOf (Ty.MethodVar 0), "Get", [ Ty.Int32 ], Some (Ty.MethodVar 0)),
                typeof<int>,
                typeof<string>,
                false
                GeneratedReference.OnTypeSpec (Ty.ArrayOf (Ty.MethodVar 0), ".ctor", [ Ty.String ], None),
                typeof<int>,
                typeof<int[]>,
                false
                // No array has a constructor taking nothing, or one with the vararg convention.
                GeneratedReference.OnTypeSpec (Ty.ArrayOf (Ty.MethodVar 0), ".ctor", [], None),
                typeof<int>,
                typeof<int[]>,
                false
                GeneratedReference.OnTypeSpecVarArg (Ty.ArrayOf (Ty.MethodVar 0), ".ctor", [ Ty.Int32 ; Ty.Int32 ]),
                typeof<int>,
                typeof<int[]>,
                false
                GeneratedReference.OnTypeSpec (
                    Ty.ArrayOf (Ty.MethodVar 0),
                    ".ctor",
                    [ Ty.Int32 ; Ty.Int32 ; Ty.Int32 ],
                    None
                ),
                typeof<int>,
                typeof<int[][]>,
                true
                GeneratedReference.OnTypeSpec (Ty.MethodVar 0, "ToString", [], Some Ty.String),
                typeof<int>,
                typeof<string>,
                true
            ]

        let world =
            {
                Types = []
                References = cases |> List.map (fun (reference, _, _, _) -> reference)
            }

        let assemblyName = "InstantiationDependentWorld"
        let image, handles = emit 0 assemblyName world

        let analysed =
            Assembly.read loggerFactory (Some $"%s{assemblyName}.dll") (new MemoryStream (image))

        let loaded = LoadedAssemblies.ofAssemblies [ corelib ; analysed ]

        let mutable ctx : TypeConcretization.ConcretizationContext<DumpedAssembly> =
            {
                ConcreteTypes = Corelib.concretizeAll loaded baseClassTypes AllConcreteTypes.Empty
                LoadedAssemblies = loaded
                BaseTypes = baseClassTypes
            }

        do
            // Each question in a load context of its own: CoreCLR caches a MemberRef's resolution in
            // its module whenever the method found is not generic (`StoreMemberRef`), so a second
            // question in the same module is answered from the first instantiation's lookup.
            let under (token : int) (argument : Type) : Oracle =
                let fresh =
                    AssemblyLoadContext ($"%s{assemblyName}-%s{argument.Name}", isCollectible = true)

                try
                    let reflected = fresh.LoadFromStream (new MemoryStream (image))

                    try
                        Oracle.Method (reflected.ManifestModule.ResolveMethod (token, [| argument |], [| argument |]))
                    with :? MissingMethodException ->
                        Oracle.Missing
                finally
                    fresh.Unload ()

            let describeOracle (oracle : Oracle) : string =
                match oracle with
                | Oracle.Method mb -> $"%s{mb.DeclaringType.FullName}::%s{mb.Name}/%d{mb.GetParameters().Length}"
                | Oracle.Missing -> "missing"
                | Oracle.Refused e -> $"refused: %s{e.Message}"

            let agrees (oracle : Oracle) (ours : MethodReferenceTarget) : bool =
                match oracle, ours with
                | Oracle.Missing, MethodReferenceTarget.Missing -> true
                | Oracle.Method mb, MethodReferenceTarget.ArrayMethod (_, accessor) ->
                    TestMethodReferenceResolution.arrayAccessorIs accessor mb
                | Oracle.Method mb, MethodReferenceTarget.Defined (declaringAssembly, method) ->
                    mb.Module.Assembly.GetName().Name = declaringAssembly.Name.Name
                    && mb.MetadataToken = MetadataTokens.GetToken (
                        MethodDefinitionHandle.op_Implicit method : EntityHandle
                    )
                | _ -> false

            for (reference, first, second, shouldDefer), handle in List.zip cases handles do
                let token =
                    MetadataTokens.GetToken (MemberReferenceHandle.op_Implicit handle : EntityHandle)

                let firstAnswer = under token first
                let secondAnswer = under token second

                let ctx', ours =
                    MethodReferenceResolution.resolve
                        loggerFactory
                        runtimeDirs
                        ctx
                        (ctx.LoadedAssemblies.ByDefinitionName analysed.DefinitionFullName)
                        handle

                ctx <- ctx'

                let describe () =
                    $"%A{reference}: resolver %A{ours}; runtime %s{describeOracle firstAnswer} under %s{first.Name}, %s{describeOracle secondAnswer} under %s{second.Name}"

                match ours with
                | MethodReferenceTarget.DependsOnInstantiation ->
                    if not shouldDefer || describeOracle firstAnswer = describeOracle secondAnswer then
                        failwith (describe ())
                | ours ->
                    if shouldDefer || not (agrees firstAnswer ours) || not (agrees secondAnswer ours) then
                        failwith (describe ())
