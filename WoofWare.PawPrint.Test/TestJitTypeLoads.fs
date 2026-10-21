namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// `JitTypeLoads` checked against the host's CoreCLR, over every type of a small grammar at every
/// position it describes. A client compiled against a provider declaring every type it names is
/// loaded in a collectible `AssemblyLoadContext` against one lacking some of them, and without the
/// assembly `Optional` at all; each probe method puts one type at one position, and either the JIT
/// fails to compile the probe (the exception escapes it), or a method the probe calls (the probe's
/// own `catch` stops it), or nothing fails. The rule predicts which, with `TypeLoad.loadMiss`
/// saying whether a type it loads in full fails to load.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestJitTypeLoads =

    /// What running a probe does.
    [<RequireQualifiedAccess>]
    type Outcome =
        /// Nothing fails to load.
        | Runs
        /// Compiling a method the probe calls fails with the exception of this name, which the
        /// probe's own handler catches.
        | Caught of exceptionName : string
        /// Compiling the probe itself fails with the exception of this name.
        | Escapes of exceptionName : string

    let private providerSource (complete : bool) : string =
        let gone =
            """
public class Gone { }
public struct GoneStruct { }
public class GoneException : System.Exception { }
"""

        """
namespace Provider;
public class Kept { }
public struct KeptStruct { }
public class KeptGeneric<T> { }
public struct KeptGenericStruct<T> { }
"""
        + (if complete then gone else "")

    /// An assembly the client is compiled against, which is not there when it is loaded.
    let private optionalSource : string =
        """
namespace Optional;
public class Thing { }
public struct ThingStruct { }
public class ThingException : System.Exception { }
"""

    let private compileProvider (complete : bool) : byte[] =
        Roslyn.compileOptimizedAssembly "Provider" OutputKind.DynamicallyLinkedLibrary [ providerSource complete ]

    /// A type of the grammar as the client spells it: one of a few that load or fail to load, under
    /// at most two constructors.
    type private Shape =
        | Kept
        | Gone
        /// A client class deriving from `Gone`.
        | DerivesGone
        | KeptStruct
        | GoneStruct
        /// A client struct with a field of type `GoneStruct`.
        | HoldsGone
        | Missing
        | MissingStruct
        | Array of Shape
        | Pointer of Shape
        | KeptGeneric of Shape
        | KeptGenericStruct of Shape
        | ReturnsFunction of Shape
        | TakesFunction of Shape

    let rec private render (shape : Shape) : string =
        match shape with
        | Kept -> "Provider.Kept"
        | Gone -> "Provider.Gone"
        | DerivesGone -> "DerivesGone"
        | KeptStruct -> "Provider.KeptStruct"
        | GoneStruct -> "Provider.GoneStruct"
        | HoldsGone -> "HoldsGone"
        | Missing -> "Optional.Thing"
        | MissingStruct -> "Optional.ThingStruct"
        | Array element -> $"%s{render element}[]"
        | Pointer target -> $"%s{render target}*"
        | KeptGeneric argument -> $"Provider.KeptGeneric<%s{render argument}>"
        | KeptGenericStruct argument -> $"Provider.KeptGenericStruct<%s{render argument}>"
        | ReturnsFunction returned -> $"delegate*<%s{render returned}>"
        | TakesFunction parameter -> $"delegate*<%s{render parameter}, void>"

    /// C# accepts a pointer or a function pointer nowhere as a type argument.
    let private isTypeArgument (shape : Shape) : bool =
        match shape with
        | Pointer _
        | ReturnsFunction _
        | TakesFunction _ -> false
        | _ -> true

    /// Whether `null` is a value of the type, so that a probe can pass one without naming the type
    /// anywhere the JIT would load it.
    let private takesNull (shape : Shape) : bool =
        match shape with
        | KeptStruct
        | GoneStruct
        | HoldsGone
        | MissingStruct
        | KeptGenericStruct _ -> false
        | _ -> true

    let private leaves : Shape list =
        [
            Kept
            Gone
            DerivesGone
            KeptStruct
            GoneStruct
            HoldsGone
            Missing
            MissingStruct
        ]

    let private wrap (shape : Shape) : Shape list =
        [
            Array shape
            Pointer shape
            if isTypeArgument shape then
                KeptGeneric shape
                KeptGenericStruct shape
            ReturnsFunction shape
            TakesFunction shape
        ]

    /// Every shape with at most two constructors.
    let private shapes : Shape list =
        let once = leaves |> List.collect wrap
        leaves @ once @ (once |> List.collect wrap)

    /// The exceptions a probe tells apart from running to the end.
    let private catches : string =
        "catch (System.TypeLoadException) { return \"TypeLoadException\"; } "
        + "catch (System.IO.FileNotFoundException) { return \"FileNotFoundException\"; } "
        + "catch (System.NullReferenceException) { } return \"\";"


    /// How the client's metadata spells a probe's shape, and the client's own types.
    type private Spellings =
        {
            /// The shape, as a method returning it spells it.
            Shape : TypeDefn
            /// The client type of this name.
            Definition : string -> TypeDefn
        }

    /// Where a probe, and a method it calls, meet a type, each in the order the JIT meets them.
    type private Meetings =
        {
            Caller : (JitTypePosition * TypeDefn) list
            Callee : (JitTypePosition * TypeDefn) list
        }

    /// One probe: a method of `Probes` whose body puts a shape at one position.
    type private Probe =
        {
            Name : string
            Body : string
            Meets : Spellings -> Meetings
        }

    /// A token naming a method definition, a method specification or a field definition, for
    /// `JitTypeLoads.memberPositions`, which reads only which of them it is.
    let private methodDefinition : MetadataToken =
        MetadataToken.MethodDef (Ecma335.MetadataTokens.MethodDefinitionHandle 1)

    let private methodSpecification : MetadataToken =
        MetadataToken.MethodSpecification (Ecma335.MetadataTokens.MethodSpecificationHandle 1)

    let private fieldDefinition : MetadataToken =
        MetadataToken.FieldDefinition (Ecma335.MetadataTokens.FieldDefinitionHandle 1)

    /// The positions the JIT reads of the signature of a method `op` names by `token`, returning
    /// `returned` and taking `parameters`, where the instruction takes its address for `addressUse`.
    let private addressMeetings
        (op : UnaryMetadataTokenIlOp)
        (token : MetadataToken)
        (addressUse : MethodAddressUse)
        (returned : TypeDefn)
        (parameters : TypeDefn list)
        : (JitTypePosition * TypeDefn) list
        =
        JitTypeLoads.memberPositions op token addressUse
        |> List.collect (fun position ->
            match position with
            | JitTypePosition.CalleeReturn -> [ position, returned ]
            | JitTypePosition.CalleeParameter -> parameters |> List.map (fun parameter -> position, parameter)
            | JitTypePosition.FunctionPointerSignature ->
                returned :: parameters |> List.map (fun spelling -> position, spelling)
            | _ -> failwith $"memberPositions gave %A{position} for the method %A{op} names"
        )

    /// The positions the JIT reads of the signature of a method definition a call `op` names.
    let private signatureMeetings
        (op : UnaryMetadataTokenIlOp)
        (returned : TypeDefn)
        (parameters : TypeDefn list)
        : (JitTypePosition * TypeDefn) list
        =
        addressMeetings op methodDefinition MethodAddressUse.FunctionPointer returned parameters

    /// The positions the JIT reads of a field of type `fieldType` that `op` names, declared by
    /// `owner`.
    let private fieldMeetings
        (op : UnaryMetadataTokenIlOp)
        (owner : TypeDefn)
        (fieldType : TypeDefn)
        : (JitTypePosition * TypeDefn) list
        =
        (JitTypePosition.DeclaringType, owner)
        :: (JitTypeLoads.memberPositions op fieldDefinition MethodAddressUse.FunctionPointer
            |> List.map (fun position ->
                match position with
                | JitTypePosition.FieldType -> position, fieldType
                | _ -> failwith $"memberPositions gave %A{position} for the field %A{op} names"
            ))

    let private meets caller callee : Meetings =
        {
            Caller = caller
            Callee = callee
        }

    /// The declarations `k`th of `shapes`'s probes use: methods of `Probes`, and types of the
    /// client, each suffixed with `k`. None of them runs but to throw `NullReferenceException`.
    let private declarations (k : int) (shape : Shape) : string * string =
        let s = render shape

        let methods =
            [
                $"static void Out%d{k}(out %s{s} d) => throw null;"
                $"static void Takes%d{k}(%s{s} d) => throw null;"
                $"static void TakesRef%d{k}(ref %s{s} d) => throw null;"
                $"static %s{s} Returns%d{k}() => throw null;"
                $"static ref %s{s} ReturnsRef%d{k}() => throw null;"
                $"static Instance%d{k} GetInstance%d{k}() => throw null;"
                $"static Virtual%d{k} GetVirtual%d{k}() => throw null;"
            ]

        let types =
            [
                $"public unsafe class Static%d{k} {{ public static %s{s} F; }}"
                $"public unsafe class Instance%d{k} {{ public %s{s} F; }}"
                $"public unsafe class Virtual%d{k} {{ public virtual %s{s} M() => throw null; public virtual void T(%s{s} d) => throw null; public void NT(%s{s} d) => throw null; public void GT<U>(%s{s} d) => throw null; }}"
                $"public unsafe class Constructed%d{k} {{ public Constructed%d{k}(%s{s} d) => throw null; }}"
                $"public unsafe delegate void TakesDelegate%d{k}(%s{s} d);"
                if isTypeArgument shape then
                    $"public unsafe class Derived%d{k} : Provider.KeptGeneric<%s{s}> {{ public static void M() => throw null; }}"
            ]

        String.concat "\n    " methods, String.concat "\n" types

    /// The probes of `k`th of `shapes`.
    let private probes (k : int) (shape : Shape) : Probe list =
        let s = render shape

        let probe (name : string) (body : string) (meets : Spellings -> Meetings) : Probe =
            {
                Name = $"%s{name}%d{k}"
                Body = body
                Meets = meets
            }

        let call = UnaryMetadataTokenIlOp.Call

        [
            probe
                "Local"
                $"%s{s} x; Out%d{k}(out x);"
                (fun spelt ->
                    let byref = TypeDefn.Byref spelt.Shape

                    meets
                        ((JitTypePosition.Local, spelt.Shape)
                         :: signatureMeetings call TypeDefn.Void [ byref ])
                        [ JitTypePosition.OwnParameter, byref ]
                )
            probe
                "ByrefParameter"
                $"TakesRef%d{k}(ref *(%s{s}*)null);"
                (fun spelt ->
                    let byref = TypeDefn.Byref spelt.Shape
                    meets (signatureMeetings call TypeDefn.Void [ byref ]) [ JitTypePosition.OwnParameter, byref ]
                )
            probe
                "Return"
                $"Returns%d{k}();"
                (fun spelt -> meets (signatureMeetings call spelt.Shape []) [ JitTypePosition.OwnReturn, spelt.Shape ])
            probe
                "ByrefReturn"
                $"ReturnsRef%d{k}();"
                (fun spelt ->
                    let byref = TypeDefn.Byref spelt.Shape
                    meets (signatureMeetings call byref []) [ JitTypePosition.OwnReturn, byref ]
                )
            probe
                "VirtualReturn"
                $"GetVirtual%d{k}().M();"
                (fun spelt ->
                    let owner = spelt.Definition $"Virtual%d{k}"

                    meets
                        (signatureMeetings call owner []
                         @ (JitTypePosition.DeclaringType, owner)
                           :: signatureMeetings UnaryMetadataTokenIlOp.Callvirt spelt.Shape [])
                        []
                )
            probe
                "AddressReturn"
                $"Use((void*)(delegate*<%s{s}>)&Returns%d{k});"
                (fun spelt -> meets (signatureMeetings UnaryMetadataTokenIlOp.Ldftn spelt.Shape []) [])
            // A function pointer to a method passes it nothing, but CoreCLR loads the value types of
            // its signature first; for a delegate's target, only where `ldvirtftn` or a method
            // specification names it. Each delegate here is over an instance method and closed over
            // its receiver, which CoreCLR makes without a thunk to shuffle the arguments.
            probe
                "AddressParameter"
                $"Use((void*)(delegate*<%s{s}, void>)&Takes%d{k});"
                (fun spelt ->
                    meets
                        (addressMeetings
                            UnaryMetadataTokenIlOp.Ldftn
                            methodDefinition
                            MethodAddressUse.FunctionPointer
                            TypeDefn.Void
                            [ spelt.Shape ])
                        []
                )
            probe
                "DelegateParameter"
                $"Keep(new TakesDelegate%d{k}(GetVirtual%d{k}().NT));"
                (fun spelt ->
                    meets
                        (signatureMeetings call (spelt.Definition $"Virtual%d{k}") []
                         @ (JitTypePosition.DeclaringType, spelt.Definition $"TakesDelegate%d{k}")
                           :: addressMeetings
                               UnaryMetadataTokenIlOp.Ldftn
                               methodDefinition
                               MethodAddressUse.DelegateTarget
                               TypeDefn.Void
                               [ spelt.Shape ])
                        []
                )
            probe
                "GenericDelegateParameter"
                $"Keep(new TakesDelegate%d{k}(GetVirtual%d{k}().GT<int>));"
                (fun spelt ->
                    meets
                        (signatureMeetings call (spelt.Definition $"Virtual%d{k}") []
                         @ (JitTypePosition.DeclaringType, spelt.Definition $"TakesDelegate%d{k}")
                           :: addressMeetings
                               UnaryMetadataTokenIlOp.Ldftn
                               methodSpecification
                               MethodAddressUse.DelegateTarget
                               TypeDefn.Void
                               [ spelt.Shape ])
                        []
                )
            probe
                "VirtualDelegateParameter"
                $"Keep(new TakesDelegate%d{k}(GetVirtual%d{k}().T));"
                (fun spelt ->
                    let owner = spelt.Definition $"Virtual%d{k}"

                    meets
                        (signatureMeetings call owner []
                         @ (JitTypePosition.DeclaringType, spelt.Definition $"TakesDelegate%d{k}")
                           :: addressMeetings
                               UnaryMetadataTokenIlOp.Ldvirtftn
                               methodDefinition
                               MethodAddressUse.DelegateTarget
                               TypeDefn.Void
                               [ spelt.Shape ])
                        []
                )
            // The local, if Roslyn keeps one, is a function pointer, which the JIT does not load;
            // the call never runs.
            probe
                "IndirectReturn"
                $"delegate*<%s{s}> f = null; if (Never) f();"
                (fun spelt -> meets (signatureMeetings UnaryMetadataTokenIlOp.Calli spelt.Shape []) [])
            probe
                "StaticField"
                $"TakesRef%d{k}(ref Static%d{k}.F);"
                (fun spelt ->
                    let byref = TypeDefn.Byref spelt.Shape

                    meets
                        (fieldMeetings UnaryMetadataTokenIlOp.Ldsflda (spelt.Definition $"Static%d{k}") spelt.Shape
                         @ signatureMeetings call TypeDefn.Void [ byref ])
                        [ JitTypePosition.OwnParameter, byref ]
                )
            probe
                "InstanceField"
                $"TakesRef%d{k}(ref GetInstance%d{k}().F);"
                (fun spelt ->
                    let owner = spelt.Definition $"Instance%d{k}"
                    let byref = TypeDefn.Byref spelt.Shape

                    meets
                        (signatureMeetings call owner []
                         @ fieldMeetings UnaryMetadataTokenIlOp.Ldflda owner spelt.Shape
                         @ signatureMeetings call TypeDefn.Void [ byref ])
                        [ JitTypePosition.OwnParameter, byref ]
                )
            probe
                "TypeToken"
                $"UseType(typeof(%s{s}));"
                (fun spelt -> meets [ JitTypePosition.TypeToken, spelt.Shape ] [])
            if takesNull shape then
                probe
                    "Parameter"
                    $"Takes%d{k}(null);"
                    (fun spelt ->
                        meets
                            (signatureMeetings call TypeDefn.Void [ spelt.Shape ])
                            [ JitTypePosition.OwnParameter, spelt.Shape ]
                    )

                probe
                    "ConstructorParameter"
                    $"new Constructed%d{k}(null);"
                    (fun spelt ->
                        meets
                            ((JitTypePosition.DeclaringType, spelt.Definition $"Constructed%d{k}")
                             :: signatureMeetings UnaryMetadataTokenIlOp.Newobj TypeDefn.Void [ spelt.Shape ])
                            [ JitTypePosition.OwnParameter, spelt.Shape ]
                    )

                probe
                    "IndirectParameter"
                    $"delegate*<%s{s}, void> f = null; if (Never) f(null);"
                    (fun spelt ->
                        meets (signatureMeetings UnaryMetadataTokenIlOp.Calli TypeDefn.Void [ spelt.Shape ]) []
                    )
            if isTypeArgument shape then
                probe
                    "MethodArgument"
                    $"Generic<%s{s}>();"
                    (fun spelt -> meets [ JitTypePosition.MethodArgument, spelt.Shape ] [])

                probe
                    "Owner"
                    $"Derived%d{k}.M();"
                    (fun spelt -> meets [ JitTypePosition.DeclaringType, spelt.Definition $"Derived%d{k}" ] [])
        ]

    /// The client: every shape's declarations and probes.
    let private clientSource : string =
        let declared = shapes |> List.mapi declarations

        let probeMethods =
            shapes
            |> List.mapi probes
            |> List.concat
            |> List.map (fun probe ->
                $"    public static string %s{probe.Name}() {{ try {{ %s{probe.Body} }} %s{catches} }}"
            )

        [
            "namespace Client;"
            "public class DerivesGone : Provider.Gone { }"
            "public struct HoldsGone { public Provider.GoneStruct F; }"
            yield! List.map snd declared
            "public static unsafe class Probes"
            "{"
            "    static bool Never;"
            "    static void Use(void* p) => throw null;"
            "    static void UseType(System.Type t) => throw null;"
            "    static void Keep(object o) => throw null;"
            "    static void Generic<T>() => throw null;"
            yield! declared |> List.map (fun (methods, _) -> "    " + methods)
            yield! probeMethods
            "}"
        ]
        |> String.concat "\n"

    /// What the host's CoreCLR does at each probe.
    let private hostOutcomes (client : byte[]) (incompleteProvider : byte[]) (names : string list) : Outcome list =
        let context =
            Runtime.Loader.AssemblyLoadContext ("JitTypeLoads", isCollectible = true)

        context.add_Resolving (
            Func<_, _, _> (fun (context : Runtime.Loader.AssemblyLoadContext) (name : AssemblyName) ->
                if name.Name = "Provider" then
                    context.LoadFromStream (new MemoryStream (incompleteProvider))
                else
                    null
            )
        )

        try
            let probes =
                context.LoadFromStream(new MemoryStream (client)).GetType "Client.Probes"

            names
            |> List.map (fun name ->
                try
                    match probes.GetMethod(name).Invoke ((null : obj), [||]) :?> string with
                    | "" -> Outcome.Runs
                    | caught -> Outcome.Caught caught
                with :? TargetInvocationException as e ->
                    Outcome.Escapes (e.InnerException.GetType().Name)
            )
        finally
            context.Unload ()

    [<Test>]
    let ``the JIT loads in full exactly the types JitTypeLoads says, wherever a type of the grammar appears``
        ()
        : unit
        =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let incompleteProvider = compileProvider false

        let references =
            [
                compileProvider true
                Roslyn.compileOptimizedAssembly "Optional" OutputKind.DynamicallyLinkedLibrary [ optionalSource ]
            ]
            |> List.map (fun image ->
                MetadataReference.CreateFromImage (ImmutableArray.CreateRange image) :> MetadataReference
            )

        let clientImage =
            Roslyn.compileOptimizedAssemblyWithReferences
                "Client"
                OutputKind.DynamicallyLinkedLibrary
                references
                [ clientSource ]

        let allProbes = shapes |> List.mapi (fun k shape -> k, shape, probes k shape)

        let names =
            allProbes
            |> List.collect (fun (_, _, probes) -> probes |> List.map (fun p -> p.Name))

        let host = hostOutcomes clientImage incompleteProvider names

        let client =
            Assembly.read loggerFactory (Some "Client.dll") (new MemoryStream (clientImage))

        let corelib = Assembly.readFile loggerFactory HostImage.coreLibPath
        let baseClassTypes = BaseClassTypes.ofCorelib corelib

        let clientType (name : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
            client.TypeDefs.Values
            |> Seq.filter (fun ty -> ty.Namespace = "Client" && ty.Name = name)
            |> Seq.exactlyOne

        let definition (name : string) : TypeDefn =
            TypeDefn.FromDefinition ((clientType name).Identity, SignatureTypeKind.Class)

        let probesType = clientType "Probes"

        // The provider is where the loader looks for assemblies, but not loaded.
        let directory = Directory.CreateTempSubdirectory "jit-type-loads"

        let predicted =
            try
                File.WriteAllBytes (Path.Combine (directory.FullName, "Provider.dll"), incompleteProvider)
                let dirs = Seq.append [ directory.FullName ] HostImage.runtimeDirs |> List.ofSeq

                // Why loading `spelling` in full fails.
                let loadFails (assemblies : LoadedAssemblies) (spelling : TypeDefn) : LoadedAssemblies * string option =
                    let assemblies, miss =
                        TypeLoad.loadMiss loggerFactory dirs baseClassTypes client spelling assemblies

                    let failure =
                        match miss with
                        | None -> None
                        | Some (TypeLoad.LoadMiss.TypeAbsent _) -> Some "TypeLoadException"
                        | Some (TypeLoad.LoadMiss.AssemblyUnavailable _) -> Some "FileNotFoundException"

                    assemblies, failure

                // Why the first of `meetings` whose type the JIT loads fails to load.
                let firstMiss
                    (assemblies : LoadedAssemblies)
                    (meetings : (JitTypePosition * TypeDefn) list)
                    : LoadedAssemblies * string option
                    =
                    ((assemblies, None), meetings)
                    ||> List.fold (fun (assemblies, found) (position, spelling) ->
                        match found, JitTypeLoads.load position spelling with
                        | Some _, _
                        | None, None -> assemblies, found
                        | None, Some (JitTypeLoad.InFull loaded) -> loadFails assemblies loaded
                        | None, Some (JitTypeLoad.InFullIfDefined loaded) ->
                            let head =
                                match TypeDefn.stripCustomModifiers loaded with
                                | TypeDefn.GenericInstantiation (generic, _) -> TypeDefn.stripCustomModifiers generic
                                | loaded -> loaded

                            match head with
                            | TypeDefn.FromReference (typeRef, _) ->
                                match
                                    TypeResolution.tryResolveTypeRefIdentity
                                        loggerFactory
                                        dirs
                                        client
                                        typeRef
                                        assemblies
                                with
                                | assemblies, TypeReferenceIdentity.Resolved _ -> loadFails assemblies loaded
                                | assemblies, TypeReferenceIdentity.TypeAbsent _ -> assemblies, None
                                | assemblies, TypeReferenceIdentity.AssemblyUnavailable _ ->
                                    assemblies, Some "FileNotFoundException"
                            | _ -> loadFails assemblies loaded
                    )

                ((LoadedAssemblies.ofAssemblies [ corelib ; client ], []), allProbes)
                ||> List.fold (fun (assemblies, outcomes) (k, _, probes) ->
                    let shape =
                        let returns = probesType.Methods |> List.find (fun m -> m.Name = $"Returns%d{k}")

                        match returns.Signature.ReturnType with
                        | MethodReturnType.Returns shape -> shape
                        | MethodReturnType.Void -> failwith $"Returns%d{k} returns nothing"

                    let spelt =
                        {
                            Shape = shape
                            Definition = definition
                        }

                    ((assemblies, outcomes), probes)
                    ||> List.fold (fun (assemblies, outcomes) probe ->
                        let meetings = probe.Meets spelt
                        let assemblies, escapes = firstMiss assemblies meetings.Caller
                        let assemblies, caught = firstMiss assemblies meetings.Callee

                        let outcome =
                            match escapes, caught with
                            | Some escapes, _ -> Outcome.Escapes escapes
                            | None, Some caught -> Outcome.Caught caught
                            | None, None -> Outcome.Runs

                        assemblies, outcome :: outcomes
                    )
                )
                |> snd
                |> List.rev
            finally
                directory.Delete true

        let disagreements =
            List.zip3 names host predicted
            |> List.choose (fun (name, host, predicted) ->
                if host = predicted then
                    None
                else
                    Some $"%s{name}: the host %A{host}, JitTypeLoads %A{predicted}"
            )

        if not disagreements.IsEmpty then
            failwith (String.concat Environment.NewLine disagreements)

        // Enough of each outcome for the comparison to say something.
        let count (outcome : Outcome) =
            host |> List.filter ((=) outcome) |> List.length

        count Outcome.Runs |> shouldBeGreaterThan 100
        count (Outcome.Escapes "TypeLoadException") |> shouldBeGreaterThan 100
        count (Outcome.Escapes "FileNotFoundException") |> shouldBeGreaterThan 50
        count (Outcome.Caught "TypeLoadException") |> shouldBeGreaterThan 20
        count (Outcome.Caught "FileNotFoundException") |> shouldBeGreaterThan 10
