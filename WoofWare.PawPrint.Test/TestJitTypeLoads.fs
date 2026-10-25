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
        /// Compiling the method, prepared by itself, fails with the exception of this name.
        | Fails of exceptionName : string

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

    /// Whether the type is a pointer or a function pointer, which a method unmanaged code calls may
    /// take as it is.
    let private isUnmanaged (shape : Shape) : bool =
        match shape with
        | Pointer _
        | ReturnsFunction _
        | TakesFunction _ -> true
        | _ -> false

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


    /// How the client's metadata spells a probe's shape, the client's own types, and the signatures
    /// of its methods.
    type private Spellings =
        {
            /// The shape, as a method returning it spells it.
            Shape : TypeDefn
            /// The client type of this name.
            Definition : string -> TypeDefn
            /// The return type and the parameters of the method of `Probes` of this name.
            Signature : string -> TypeDefn * TypeDefn list
            /// The signature of the method of `Probes` of this name, as its definition spells it.
            MethodSignature : string -> TypeMethodSignature<TypeDefn>
            /// What CoreCLR reads of the P/Invoke of `Probes` of this name (`UnmanagedTarget.ofPInvoke`).
            PInvoke : string -> UnmanagedTarget
        }

    /// Where a probe, and a method it calls, meet a type, each in the order the JIT meets them.
    type private Meetings =
        {
            Caller : (JitTypePosition * TypeDefn) list
            Callee : (JitTypePosition * TypeDefn) list
        }

    /// How the host's JIT compiles the client: fully optimised in a collectible
    /// `AssemblyLoadContext`, as CoreCLR tiers none of the code there
    /// (`CodeVersionManager::IsMethodSupported`), and at tier 0 elsewhere, for a method's first call.
    [<RequireQualifiedAccess>]
    type private Tier =
        | Tier0
        | Optimised

    /// What becomes, at a probe, of the loads the JIT may make or not (`JitTypeLoad.Possibly`).
    [<RequireQualifiedAccess>]
    type private PossibleLoads =
        /// The JIT makes them where they are.
        | Made
        /// It makes none of them.
        | Skipped
        /// It makes those of the method the probe calls while compiling the probe, into which it
        /// inlines that method.
        | MadeByInliner

    /// One probe: a method of `Probes` whose body puts a shape at one position.
    type private Probe =
        {
            Name : string
            Body : string
            Meets : Spellings -> Meetings
            /// Whether the body runs in a `try` with `catch` clauses, which stop what fails to load
            /// for a method it calls; otherwise its failures escape too.
            InTry : bool
            /// What becomes of the loads the JIT may make or not, compiling at each tier.
            Possible : Tier -> PossibleLoads
        }

    /// A token naming a method definition or a method specification, for
    /// `JitTypeLoads.preparation`, which reads only which of them it is.
    let private methodDefinition : MetadataToken =
        MetadataToken.MethodDef (Ecma335.MetadataTokens.MethodDefinitionHandle 1)

    let private methodSpecification : MetadataToken =
        MetadataToken.MethodSpecification (Ecma335.MetadataTokens.MethodSpecificationHandle 1)

    /// The positions the JIT reads of the signature of a method an instruction names, which does
    /// `operand` with it, returning `returned` and taking `parameters`, in the order it reads them.
    let private signatureMeetings
        (operand : MemberOperand)
        (returned : TypeDefn)
        (parameters : TypeDefn list)
        : (JitTypePosition * TypeDefn) list
        =
        match JitTypeLoads.memberPositions operand with
        | MemberPositions.BySignature -> failwith $"memberPositions gives %A{operand} by its signature"
        | MemberPositions.Described positions ->

        positions
        |> List.collect (fun position ->
            match position with
            | JitTypePosition.CalleeReturn -> [ position, returned ]
            | JitTypePosition.CalleeParameter -> parameters |> List.map (fun parameter -> position, parameter)
            | JitTypePosition.FunctionPointerSignature
            | JitTypePosition.PossibleFunctionPointerSignature ->
                returned :: parameters |> List.map (fun spelling -> position, spelling)
            | _ -> failwith $"memberPositions gave %A{position} for the method %A{operand} names"
        )

    /// As `signatureMeetings`, for an `op`, `ldftn` or `ldvirtftn`, naming the method by `token`,
    /// with or without a `constrained.` prefix, whose address has the use `addressUse`.
    let private constrainedMeetings
        (op : UnaryMetadataTokenIlOp)
        (token : MetadataToken)
        (prefix : AddressConstraint)
        (addressUse : MethodAddressUse)
        (returned : TypeDefn)
        (parameters : TypeDefn list)
        : (JitTypePosition * TypeDefn) list
        =
        signatureMeetings
            (MemberOperand.Address (JitTypeLoads.preparation op token prefix addressUse))
            returned
            parameters

    /// As `constrainedMeetings`, without a `constrained.` prefix.
    let private addressMeetings
        (op : UnaryMetadataTokenIlOp)
        (token : MetadataToken)
        (addressUse : MethodAddressUse)
        (returned : TypeDefn)
        (parameters : TypeDefn list)
        : (JitTypePosition * TypeDefn) list
        =
        constrainedMeetings op token AddressConstraint.Unconstrained addressUse returned parameters

    /// The positions the JIT reads of a field of type `fieldType` that an instruction accesses,
    /// declared by `owner`.
    let private fieldMeetings (owner : TypeDefn) (fieldType : TypeDefn) : (JitTypePosition * TypeDefn) list =
        (JitTypePosition.DeclaringType, owner)
        :: (match JitTypeLoads.memberPositions MemberOperand.FieldAccess with
            | MemberPositions.BySignature -> failwith "memberPositions gives a field access by its signature"
            | MemberPositions.Described positions -> positions
            |> List.map (fun position ->
                match position with
                | JitTypePosition.FieldType -> position, fieldType
                | _ -> failwith $"memberPositions gave %A{position} for a field"
            ))

    let private meets caller callee : Meetings =
        {
            Caller = caller
            Callee = callee
        }

    /// Whether the host's JIT compiles Swift's calling convention as such: it does on Arm64, and on
    /// x64 other than Windows'.
    let private compilesSwift : bool =
        not (
            OperatingSystem.IsWindows ()
            && Runtime.InteropServices.RuntimeInformation.ProcessArchitecture = Runtime.InteropServices.Architecture.X64
        )

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
                $"static void* ConstrainedAddress%d{k}<T>() where T : IStatic%d{k} => (void*)(delegate*<%s{s}, void>)&T.Target<int>;"
                $"[System.Runtime.InteropServices.UnmanagedCallersOnly(CallConvs = new[] {{ typeof(System.Runtime.CompilerServices.CallConvSwift) }})] static void SwiftOwn%d{k}(%s{s}* p) {{ }}"
                $"[System.Runtime.InteropServices.UnmanagedCallersOnly(CallConvs = new[] {{ typeof(System.Runtime.CompilerServices.CallConvCdecl) }})] static void CdeclOwn%d{k}(%s{s}* p) {{ }}"
                if isUnmanaged shape then
                    $"[System.Runtime.InteropServices.UnmanagedCallersOnly(CallConvs = new[] {{ typeof(System.Runtime.CompilerServices.CallConvSwift) }})] static void SwiftOwnBare%d{k}(%s{s} p) {{ }}"
                    $"[System.Runtime.InteropServices.UnmanagedCallersOnly(CallConvs = new[] {{ typeof(System.Runtime.CompilerServices.CallConvCdecl) }})] static void CdeclOwnBare%d{k}(%s{s} p) {{ }}"
                // Calls of unmanaged code, each in a branch never taken, so that nothing makes the
                // stub CoreCLR uses where the JIT leaves the transition to unmanaged code to one. The
                // JIT inlining a caller into a probe would compile the call in the probe's `try`.
                $"[System.Runtime.InteropServices.DllImport(\"nowhere\")] static extern %s{s} NativeReturns%d{k}();"
                $"[System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)] static void CallsNativeReturns%d{k}() {{ if (Never) NativeReturns%d{k}(); }}"
                if takesNull shape then
                    $"[System.Runtime.InteropServices.DllImport(\"nowhere\")] static extern void NativeTakes%d{k}(%s{s} p);"
                    $"[System.Runtime.InteropServices.DllImport(\"nowhere\"), System.Runtime.InteropServices.UnmanagedCallConv(CallConvs = new[] {{ typeof(System.Runtime.CompilerServices.CallConvSwift) }})] static extern void SwiftNativeTakes%d{k}(%s{s} p);"
                    $"[System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)] static void CallsNative%d{k}() {{ if (Never) NativeTakes%d{k}(null); }}"
                    $"[System.Runtime.InteropServices.DllImport(\"nowhere\")] static extern void BoolThenNativeTakes%d{k}(bool b, %s{s} p);"
                    $"[System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)] static void CallsBoolThenNative%d{k}() {{ if (Never) BoolThenNativeTakes%d{k}(false, null); }}"
                    $"[System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)] static void CatchesNative%d{k}() {{ try {{ if (Never) NativeTakes%d{k}(null); }} catch (System.Exception) {{ }} }}"
                    $"[System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)] static void CallsSwiftNative%d{k}() {{ if (Never) SwiftNativeTakes%d{k}(null); }}"
                    $"[System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)] static void CatchesSwiftNative%d{k}() {{ try {{ if (Never) SwiftNativeTakes%d{k}(null); }} catch (System.Exception) {{ }} }}"
                    $"[System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)] static void CallsUnmanaged%d{k}() {{ if (Never) ((delegate* unmanaged<%s{s}, void>)Pointer)(null); }}"
                    $"[System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)] static void CallsSwiftUnmanaged%d{k}() {{ if (Never) ((delegate* unmanaged[Swift]<%s{s}, void>)Pointer)(null); }}"
                    $"[System.Runtime.InteropServices.DllImport(\"nowhere\", CallingConvention = System.Runtime.InteropServices.CallingConvention.FastCall)] static extern void FastcallNativeTakes%d{k}(%s{s} p);"
                    $"[System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)] static void CallsFastcallNative%d{k}() {{ if (Never) FastcallNativeTakes%d{k}(null); }}"
                    $"[System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)] static void CallsFastcallUnmanaged%d{k}() {{ if (Never) ((delegate* unmanaged[Fastcall]<%s{s}, void>)Pointer)(null); }}"
            ]

        let types =
            [
                $"public unsafe class Static%d{k} {{ public static %s{s} F; }}"
                $"public unsafe class Instance%d{k} {{ public %s{s} F; }}"
                $"public unsafe class Virtual%d{k} {{ public virtual %s{s} M() => throw null; public virtual void T(%s{s} d) => throw null; public void NT(%s{s} d) => throw null; public void GT<U>(%s{s} d) => throw null; }}"
                $"public unsafe class SharedTarget%d{k}<T> {{ public static void Takes(%s{s} d) => throw null; }}"
                $"public unsafe class Shared%d{k}<T> {{ public static void* Address() => (void*)(delegate*<%s{s}, void>)&SharedTarget%d{k}<T>.Takes; }}"
                $"public unsafe interface IStatic%d{k} {{ static abstract void Target<U>(%s{s} d); }}"
                $"public unsafe class StaticClass%d{k} : IStatic%d{k} {{ public static void Target<U>(%s{s} d) => throw null; }}"
                $"public unsafe struct StaticStruct%d{k} : IStatic%d{k} {{ public static void Target<U>(%s{s} d) => throw null; }}"
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
                InTry = true
                Possible = fun _ -> PossibleLoads.Made
            }

        let call = MemberOperand.Call

        // A probe calling the method of `Probes` named `helper`, whose compilation calls unmanaged
        // code at `placement` by `convention`, by the signature of the method of `Probes` named
        // `native`: that P/Invoke, or, where `indirect`, a function pointer.
        let unmanagedProbe
            (name : string)
            (helper : string)
            (native : string)
            (convention : UnmanagedConvention)
            (placement : UnmanagedCallPlacement)
            (indirect : bool)
            : Probe
            =
            probe
                name
                $"%s{helper}();"
                (fun spelt ->
                    let unmanagedCall =
                        {
                            Convention = convention
                            Placement = placement
                            CallerDisablesOptimisation = false
                            Target =
                                if indirect then
                                    UnmanagedTarget.FunctionPointer true
                                else
                                    spelt.PInvoke native
                        }

                    meets
                        (signatureMeetings call TypeDefn.Void [])
                        (JitTypeLoads.unmanagedMeetings unmanagedCall (spelt.MethodSignature native))
                )

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
                         @ (JitTypePosition.DeclaringType, owner) :: signatureMeetings call spelt.Shape [])
                        []
                )
            probe
                "AddressReturn"
                $"Use((void*)(delegate*<%s{s}>)&Returns%d{k});"
                (fun spelt ->
                    meets
                        (addressMeetings
                            UnaryMetadataTokenIlOp.Ldftn
                            methodDefinition
                            MethodAddressUse.FunctionPointer
                            spelt.Shape
                            [])
                        []
                )
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
            // A delegate over a static method that does not close over its first argument is open
            // over it, so CoreCLR makes a thunk to shuffle the arguments, walking the `Invoke`
            // method's signature and then the target's.
            probe
                "OpenDelegateParameter"
                $"Keep(new TakesDelegate%d{k}(Takes%d{k}));"
                (fun spelt ->
                    meets
                        ((JitTypePosition.DeclaringType, spelt.Definition $"TakesDelegate%d{k}")
                         :: addressMeetings
                             UnaryMetadataTokenIlOp.Ldftn
                             methodDefinition
                             MethodAddressUse.DelegateTarget
                             TypeDefn.Void
                             [ spelt.Shape ]
                         @ [
                             JitTypePosition.ShuffleThunkSignature, spelt.Shape
                             JitTypePosition.ShuffleThunkSignature, spelt.Shape
                         ])
                        []
                )
            // The local, if Roslyn keeps one, is a function pointer, which the JIT does not load;
            // the call never runs.
            probe
                "IndirectReturn"
                $"delegate*<%s{s}> f = null; if (Never) f();"
                (fun spelt -> meets (signatureMeetings call spelt.Shape []) [])
            // `discardAddresses` makes the address this takes a `pop`'s.
            probe
                "DiscardedParameter"
                $"Use((void*)(delegate*<%s{s}, void>)&Takes%d{k});"
                (fun spelt ->
                    meets
                        (addressMeetings
                            UnaryMetadataTokenIlOp.Ldftn
                            methodDefinition
                            MethodAddressUse.Discarded
                            TypeDefn.Void
                            [ spelt.Shape ])
                        []
                )
            // Optimising, the JIT folds the first address away, and asks for none. Code shared between
            // instantiations finds the second through a generic dictionary, and asks for none, unless
            // the JIT inlines it into the probe, which then asks for the address.
            { probe
                  "FoldedAddressParameter"
                  $"Use((void*)((nint)(delegate*<%s{s}, void>)&Takes%d{k} & 0));"
                  (fun spelt ->
                      meets
                          (addressMeetings
                              UnaryMetadataTokenIlOp.Ldftn
                              methodDefinition
                              MethodAddressUse.FunctionPointer
                              TypeDefn.Void
                              [ spelt.Shape ])
                          []
                  ) with
                Possible =
                    function
                    | Tier.Tier0 -> PossibleLoads.Made
                    | Tier.Optimised -> PossibleLoads.Skipped
            }
            // A `constrained.` address of a static virtual method, which CoreCLR prepares only where the
            // JIT resolves the implementation: not in code shared between the classes it is
            // instantiated over, but in code of a struct of its own.
            { probe
                  "ConstrainedClassAddress"
                  $"Use(ConstrainedAddress%d{k}<StaticClass%d{k}>());"
                  (fun spelt ->
                      meets
                          []
                          (constrainedMeetings
                              UnaryMetadataTokenIlOp.Ldftn
                              methodSpecification
                              AddressConstraint.Constrained
                              MethodAddressUse.FunctionPointer
                              TypeDefn.Void
                              [ spelt.Shape ])
                  ) with
                Possible =
                    function
                    | Tier.Tier0 -> PossibleLoads.Skipped
                    | Tier.Optimised -> PossibleLoads.Skipped
            }
            { probe
                  "ConstrainedStructAddress"
                  $"Use(ConstrainedAddress%d{k}<StaticStruct%d{k}>());"
                  (fun spelt ->
                      meets
                          []
                          (constrainedMeetings
                              UnaryMetadataTokenIlOp.Ldftn
                              methodSpecification
                              AddressConstraint.Constrained
                              MethodAddressUse.FunctionPointer
                              TypeDefn.Void
                              [ spelt.Shape ])
                  ) with
                Possible =
                    function
                    | Tier.Tier0 -> PossibleLoads.Made
                    | Tier.Optimised -> PossibleLoads.Made
            }
            { probe
                  "SharedAddressParameter"
                  $"Use(Shared%d{k}<object>.Address());"
                  (fun spelt ->
                      meets
                          []
                          (addressMeetings
                              UnaryMetadataTokenIlOp.Ldftn
                              methodDefinition
                              MethodAddressUse.FunctionPointer
                              TypeDefn.Void
                              [ spelt.Shape ])
                  ) with
                Possible =
                    function
                    | Tier.Tier0 -> PossibleLoads.Skipped
                    | Tier.Optimised -> PossibleLoads.MadeByInliner
            }
            probe
                "StaticField"
                $"TakesRef%d{k}(ref Static%d{k}.F);"
                (fun spelt ->
                    let byref = TypeDefn.Byref spelt.Shape

                    meets
                        (fieldMeetings (spelt.Definition $"Static%d{k}") spelt.Shape
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
                         @ fieldMeetings owner spelt.Shape
                         @ signatureMeetings call TypeDefn.Void [ byref ])
                        [ JitTypePosition.OwnParameter, byref ]
                )
            unmanagedProbe
                "UnmanagedReturn"
                $"CallsNativeReturns%d{k}"
                $"NativeReturns%d{k}"
                UnmanagedConvention.NotSwift
                UnmanagedCallPlacement.Unprotected
                false
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
                             :: signatureMeetings call TypeDefn.Void [ spelt.Shape ])
                            [ JitTypePosition.OwnParameter, spelt.Shape ]
                    )

                probe
                    "IndirectParameter"
                    $"delegate*<%s{s}, void> f = null; if (Never) f(null);"
                    (fun spelt -> meets (signatureMeetings call TypeDefn.Void [ spelt.Shape ]) [])

                // Each calls a method whose compilation calls unmanaged code, `NativeTakes` by
                // signature: without a handler, inside a `try` with a `catch`, after a `bool`, which
                // ends the walk for whether the call needs marshalling.
                unmanagedProbe
                    "UnmanagedParameter"
                    $"CallsNative%d{k}"
                    $"NativeTakes%d{k}"
                    UnmanagedConvention.NotSwift
                    UnmanagedCallPlacement.Unprotected
                    false

                unmanagedProbe
                    "CaughtUnmanagedParameter"
                    $"CatchesNative%d{k}"
                    $"NativeTakes%d{k}"
                    UnmanagedConvention.NotSwift
                    UnmanagedCallPlacement.Protected
                    false

                unmanagedProbe
                    "CaughtSwiftUnmanagedParameter"
                    $"CatchesSwiftNative%d{k}"
                    $"SwiftNativeTakes%d{k}"
                    UnmanagedConvention.MaybeSwift
                    UnmanagedCallPlacement.Protected
                    false

                unmanagedProbe
                    "BoolThenUnmanagedParameter"
                    $"CallsBoolThenNative%d{k}"
                    $"BoolThenNativeTakes%d{k}"
                    UnmanagedConvention.NotSwift
                    UnmanagedCallPlacement.Unprotected
                    false

                unmanagedProbe
                    "IndirectUnmanagedParameter"
                    $"CallsUnmanaged%d{k}"
                    $"NativeTakes%d{k}"
                    UnmanagedConvention.NotSwift
                    UnmanagedCallPlacement.Unprotected
                    true

                // By Fastcall's convention the JIT never makes the transition in the caller's code.
                unmanagedProbe
                    "FastcallUnmanagedParameter"
                    $"CallsFastcallNative%d{k}"
                    $"FastcallNativeTakes%d{k}"
                    UnmanagedConvention.NotSwift
                    UnmanagedCallPlacement.Unprotected
                    false

                unmanagedProbe
                    "FastcallIndirectUnmanagedParameter"
                    $"CallsFastcallUnmanaged%d{k}"
                    $"NativeTakes%d{k}"
                    UnmanagedConvention.NotSwift
                    UnmanagedCallPlacement.Unprotected
                    true

                if compilesSwift then
                    unmanagedProbe
                        "SwiftUnmanagedParameter"
                        $"CallsSwiftNative%d{k}"
                        $"SwiftNativeTakes%d{k}"
                        UnmanagedConvention.MaybeSwift
                        UnmanagedCallPlacement.Unprotected
                        false

                    unmanagedProbe
                        "SwiftIndirectUnmanagedParameter"
                        $"CallsSwiftUnmanaged%d{k}"
                        $"NativeTakes%d{k}"
                        UnmanagedConvention.Swift
                        UnmanagedCallPlacement.Unprotected
                        true
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

    /// Pairs of a return type and a parameter type, each failing to load, if at all, with an
    /// exception of its own, so that which of them the JIT loads first decides which exception it
    /// raises.
    let private mixed : (Shape * Shape) list =
        List.allPairs
            [ Kept ; Gone ; HoldsGone ; MissingStruct ]
            [ KeptStruct ; GoneStruct ; HoldsGone ; MissingStruct ]

    /// The declarations the `j`th of `mixed`'s probes use.
    let private mixedDeclarations (j : int) (returned : Shape, parameter : Shape) : string * string =
        let r = render returned
        let p = render parameter

        let methods =
            [
                $"static %s{r} MixedStatic%d{j}(%s{p} p) => throw null;"
                $"static MixedHolder%d{j} GetMixed%d{j}() => throw null;"
            ]

        let types =
            [
                $"public unsafe delegate %s{r} MixedDelegate%d{j}(%s{p} p);"
                $"public unsafe class MixedHolder%d{j} {{ public virtual %s{r} V(%s{p} p) => throw null; public %s{r} G<U>(%s{p} p) => throw null; }}"
            ]

        String.concat "\n    " methods, String.concat "\n" types

    /// The probes of the `j`th of `mixed`: its method's address taken as a function pointer, which
    /// CoreCLR prepares after reading the return type, and as a delegate's target through
    /// `ldvirtftn` and through a method specification, which it prepares before.
    let private mixedProbes (j : int) (returned : Shape, parameter : Shape) : Probe list =
        let r = render returned
        let p = render parameter

        let probe (name : string) (body : string) (meets : Spellings -> Meetings) : Probe =
            {
                Name = $"%s{name}%d{j}"
                Body = body
                Meets = meets
                InTry = true
                Possible = fun _ -> PossibleLoads.Made
            }

        let delegateTarget (op : UnaryMetadataTokenIlOp) (token : MetadataToken) (spelt : Spellings) : Meetings =
            let returned, parameters = spelt.Signature $"MixedStatic%d{j}"

            meets
                (signatureMeetings MemberOperand.Call (spelt.Definition $"MixedHolder%d{j}") []
                 @ (JitTypePosition.DeclaringType, spelt.Definition $"MixedDelegate%d{j}")
                   :: addressMeetings op token MethodAddressUse.DelegateTarget returned parameters)
                []

        [
            probe
                "MixedAddress"
                $"Use((void*)(delegate*<%s{p}, %s{r}>)&MixedStatic%d{j});"
                (fun spelt ->
                    let returned, parameters = spelt.Signature $"MixedStatic%d{j}"

                    meets
                        (addressMeetings
                            UnaryMetadataTokenIlOp.Ldftn
                            methodDefinition
                            MethodAddressUse.FunctionPointer
                            returned
                            parameters)
                        []
                )
            probe
                "MixedVirtual"
                $"Keep(new MixedDelegate%d{j}(GetMixed%d{j}().V));"
                (delegateTarget UnaryMetadataTokenIlOp.Ldvirtftn methodDefinition)
            probe
                "MixedGeneric"
                $"Keep(new MixedDelegate%d{j}(GetMixed%d{j}().G<int>));"
                (delegateTarget UnaryMetadataTokenIlOp.Ldftn methodSpecification)
        ]

    /// The image with the call of `Use` in each method of the type named `typeName` whose name
    /// starts `Discarded` replaced by a `pop` and `nop`s, `Use` being a method of that type, so that
    /// the address the method takes is popped unused, which C# has no way to say.
    let internal discardAddresses (typeName : string) (image : byte[]) : byte[] =
        let patched = Array.copy image
        use pe = new PortableExecutable.PEReader (ImmutableArray.CreateRange image)
        let metadata = pe.GetMetadataReader ()

        let methods =
            metadata.TypeDefinitions
            |> Seq.map metadata.GetTypeDefinition
            |> Seq.filter (fun ty -> metadata.GetString ty.Name = typeName)
            |> Seq.exactlyOne
            |> fun ty -> ty.GetMethods ()
            |> Seq.map (fun handle -> handle, metadata.GetMethodDefinition handle)
            |> List.ofSeq

        let useToken =
            methods
            |> List.filter (fun (_, m) -> metadata.GetString m.Name = "Use")
            |> List.exactlyOne
            |> fst
            |> fun handle -> Ecma335.MetadataTokens.GetToken (MethodDefinitionHandle.op_Implicit handle : EntityHandle)

        let call = Array.append [| 0x28uy |] (BitConverter.GetBytes useToken)

        for _, m in methods do
            if (metadata.GetString m.Name).StartsWith "Discarded" then
                let rva = m.RelativeVirtualAddress

                let section =
                    pe.PEHeaders.SectionHeaders
                    |> Seq.find (fun s -> rva >= s.VirtualAddress && rva < s.VirtualAddress + s.VirtualSize)

                let start = rva - section.VirtualAddress + section.PointerToRawData
                let size = (pe.GetMethodBody rva).Size

                let calls =
                    [ start .. start + size - call.Length ]
                    |> List.filter (fun i -> patched.[i .. i + call.Length - 1] = call)

                match calls with
                | [ at ] -> Array.blit [| 0x26uy ; 0uy ; 0uy ; 0uy ; 0uy |] 0 patched at call.Length
                | _ -> failwith $"%s{metadata.GetString m.Name} calls Use %d{calls.Length} times, not once"

        patched

    /// Every probe: the name of the method of `Probes` returning its shape, if it has one, and the
    /// probes of that shape or of the pair of `mixed`.
    let private allProbes : (string option * Probe list) list =
        (shapes |> List.mapi (fun k shape -> Some $"Returns%d{k}", probes k shape))
        @ (mixed |> List.mapi (fun j pair -> None, mixedProbes j pair))

    /// The client: every shape's and pair's declarations and probes.
    let private clientSource : string =
        let declared =
            (shapes |> List.mapi declarations) @ (mixed |> List.mapi mixedDeclarations)

        let probeMethods =
            allProbes
            |> List.collect snd
            |> List.map (fun probe ->
                if probe.InTry then
                    $"    public static string %s{probe.Name}() {{ try {{ %s{probe.Body} }} %s{catches} }}"
                else
                    $"    public static string %s{probe.Name}() {{ %s{probe.Body} return \"\"; }}"
            )

        [
            "namespace Client;"
            "public class DerivesGone : Provider.Gone { }"
            "public struct HoldsGone { public Provider.GoneStruct F; }"
            yield! List.map snd declared
            "public static unsafe class Probes"
            "{"
            "    static bool Never;"
            "    static void* Pointer;"
            "    static void Use(void* p) => throw null;"
            "    static void UseType(System.Type t) => throw null;"
            "    static void Keep(object o) => throw null;"
            "    static void Generic<T>() => throw null;"
            yield! declared |> List.map (fun (methods, _) -> "    " + methods)
            yield! probeMethods
            "}"
        ]
        |> String.concat "\n"

    /// What the host's CoreCLR does at each probe named `names`, and at preparing each method of
    /// `Probes` named `prepared`, compiling at `tier`. A context for tier 0 is not collectible, so it
    /// stays loaded until the test host exits.
    let private hostOutcomes
        (tier : Tier)
        (client : byte[])
        (incompleteProvider : byte[])
        (names : string list)
        (prepared : string list)
        : Outcome list * Outcome list
        =
        let context =
            Runtime.Loader.AssemblyLoadContext ($"JitTypeLoads-%A{tier}", isCollectible = (tier = Tier.Optimised))

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

            let probed =
                names
                |> List.map (fun name ->
                    try
                        match probes.GetMethod(name).Invoke ((null : obj), [||]) :?> string with
                        | "" -> Outcome.Runs
                        | caught -> Outcome.Caught caught
                    with :? TargetInvocationException as e ->
                        Outcome.Escapes (e.InnerException.GetType().Name)
                )

            let prepared =
                prepared
                |> List.map (fun name ->
                    let method = probes.GetMethod (name, BindingFlags.NonPublic ||| BindingFlags.Static)

                    try
                        Runtime.CompilerServices.RuntimeHelpers.PrepareMethod method.MethodHandle
                        Outcome.Runs
                    with e ->
                        Outcome.Fails (e.GetType().Name)
                )

            probed, prepared
        finally
            if context.IsCollectible then
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
            |> discardAddresses "Probes"

        let names =
            allProbes
            |> List.collect (fun (_, probes) -> probes |> List.map (fun p -> p.Name))

        // Each method Swift code calls, and its counterpart C code calls, with the position its
        // parameter is at.
        let preparedPositions =
            let swiftOwn =
                if compilesSwift then
                    JitTypePosition.SwiftOwnParameter
                else
                    JitTypePosition.OwnParameter

            shapes
            |> List.mapi (fun k shape ->
                [
                    $"SwiftOwn%d{k}", swiftOwn
                    $"CdeclOwn%d{k}", JitTypePosition.OwnParameter
                    if isUnmanaged shape then
                        $"SwiftOwnBare%d{k}", swiftOwn
                        $"CdeclOwnBare%d{k}", JitTypePosition.OwnParameter
                ]
            )
            |> List.concat

        let preparedNames = List.map fst preparedPositions

        let tiers = [ Tier.Tier0 ; Tier.Optimised ]

        let host =
            tiers
            |> List.map (fun tier -> tier, hostOutcomes tier clientImage incompleteProvider names preparedNames)
            |> Map.ofList

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

        let methodSignature (name : string) : TypeMethodSignature<TypeDefn> =
            (probesType.Methods |> List.find (fun m -> m.Name = name)).Signature

        let pInvoke (name : string) : UnmanagedTarget =
            let method = probesType.Methods |> List.find (fun m -> m.Name = name)
            UnmanagedTarget.ofPInvoke client method.TryMetadata.Value.Handle

        let signature (name : string) : TypeDefn * TypeDefn list =
            let signature = methodSignature name

            let returned =
                match signature.ReturnType with
                | MethodReturnType.Returns returned -> returned
                | MethodReturnType.Void -> TypeDefn.Void

            returned, signature.ParameterTypes

        // The provider is where the loader looks for assemblies, but not loaded.
        let directory = Directory.CreateTempSubdirectory "jit-type-loads"

        let predicted =
            try
                File.WriteAllBytes (Path.Combine (directory.FullName, "Provider.dll"), incompleteProvider)
                let dirs = Seq.append [ directory.FullName ] HostImage.runtimeDirs |> List.ofSeq

                // The exception a load meeting `miss` fails with.
                let failureOf (miss : TypeLoad.LoadMiss option) : string option =
                    match miss with
                    | None -> None
                    | Some (TypeLoad.LoadMiss.TypeAbsent _) -> Some "TypeLoadException"
                    | Some (TypeLoad.LoadMiss.AssemblyUnavailable _) -> Some "FileNotFoundException"
                    | Some (TypeLoad.LoadMiss.Unmodelled unmodelled) -> failwith unmodelled.Diagnostic

                // Why loading `spelling` in full fails.
                let loadFails (assemblies : LoadedAssemblies) (spelling : TypeDefn) : LoadedAssemblies * string option =
                    let assemblies, miss =
                        TypeLoad.loadMiss loggerFactory dirs baseClassTypes client spelling assemblies

                    assemblies, failureOf miss

                // Why `load` fails, where the JIT makes the loads it may make or not if `possible`.
                let rec loadMiss
                    (assemblies : LoadedAssemblies)
                    (possible : bool)
                    (load : JitTypeLoad)
                    : LoadedAssemblies * string option
                    =
                    match load with
                    | JitTypeLoad.InFull loaded -> loadFails assemblies loaded
                    | JitTypeLoad.Approximately loaded ->
                        let assemblies, miss =
                            TypeLoad.approximateLoadMissFrom
                                loggerFactory
                                dirs
                                client
                                loaded
                                TypeLoad.nothingLoaded
                                assemblies

                        assemblies, failureOf miss
                    | JitTypeLoad.Possibly load ->
                        if possible then
                            loadMiss assemblies possible load
                        else
                            assemblies, None
                    | JitTypeLoad.InFullIfDefined loaded ->
                        let head =
                            match TypeDefn.stripCustomModifiers loaded with
                            | TypeDefn.GenericInstantiation (generic, _) -> TypeDefn.stripCustomModifiers generic
                            | loaded -> loaded

                        match head with
                        | TypeDefn.FromReference (typeRef, _) ->
                            match
                                TypeResolution.tryResolveTypeRefIdentity loggerFactory dirs client typeRef assemblies
                            with
                            | assemblies, TypeReferenceIdentity.Resolved _ -> loadFails assemblies loaded
                            | assemblies, TypeReferenceIdentity.TypeAbsent _ -> assemblies, None
                            | assemblies, TypeReferenceIdentity.AssemblyUnavailable _ ->
                                assemblies, Some "FileNotFoundException"
                            | _, TypeReferenceIdentity.Unmodelled unmodelled -> failwith unmodelled.Diagnostic
                        | _ -> loadFails assemblies loaded

                // Why the first of `meetings` whose type the JIT loads fails to load, where it makes
                // the loads it may make or not if `possible`.
                let firstMiss
                    (assemblies : LoadedAssemblies)
                    (possible : bool)
                    (meetings : (JitTypePosition * TypeDefn) list)
                    : LoadedAssemblies * string option
                    =
                    ((assemblies, None), meetings)
                    ||> List.fold (fun (assemblies, found) (position, spelling) ->
                        match found, JitTypeLoads.load position spelling with
                        | Some _, _
                        | None, None -> assemblies, found
                        | None, Some load -> loadMiss assemblies possible load
                    )

                // What each probe and each prepared method does, compiled at `tier`.
                let predict (tier : Tier) : Outcome list * Outcome list =
                    ((LoadedAssemblies.ofAssemblies [ corelib ; client ], []), allProbes)
                    ||> List.fold (fun (assemblies, outcomes) (anchor, probes) ->
                        let spelt =
                            {
                                Shape =
                                    match anchor with
                                    | Some anchor -> fst (signature anchor)
                                    | None -> TypeDefn.Void
                                Definition = definition
                                Signature = signature
                                MethodSignature = methodSignature
                                PInvoke = pInvoke
                            }

                        ((assemblies, outcomes), probes)
                        ||> List.fold (fun (assemblies, outcomes) probe ->
                            let meetings = probe.Meets spelt

                            let isPossible (position : JitTypePosition, spelling : TypeDefn) : bool =
                                match JitTypeLoads.load position spelling with
                                | Some (JitTypeLoad.Possibly _) -> true
                                | _ -> false

                            let made, caller, callee =
                                match probe.Possible tier with
                                | PossibleLoads.Made -> true, meetings.Caller, meetings.Callee
                                | PossibleLoads.Skipped -> false, meetings.Caller, meetings.Callee
                                | PossibleLoads.MadeByInliner ->
                                    true,
                                    meetings.Caller @ List.filter isPossible meetings.Callee,
                                    List.filter (isPossible >> not) meetings.Callee

                            let assemblies, escapes = firstMiss assemblies made caller
                            let assemblies, caught = firstMiss assemblies made callee

                            let outcome =
                                match escapes, caught with
                                | Some escapes, _ -> Outcome.Escapes escapes
                                | None, Some caught -> Outcome.Caught caught
                                | None, None -> Outcome.Runs

                            assemblies, outcome :: outcomes
                        )
                    )
                    |> fun (assemblies, outcomes) ->
                        let _, prepared =
                            ((assemblies, []), preparedPositions)
                            ||> List.fold (fun (assemblies, outcomes) (name, position) ->
                                let parameter = signature name |> snd |> List.exactlyOne
                                let assemblies, failure = firstMiss assemblies true [ position, parameter ]

                                let outcome =
                                    match failure with
                                    | Some failure -> Outcome.Fails failure
                                    | None -> Outcome.Runs

                                assemblies, outcome :: outcomes
                            )

                        List.rev outcomes, List.rev prepared

                tiers |> List.map (fun tier -> tier, predict tier) |> Map.ofList
            finally
                directory.Delete true

        let disagreements =
            tiers
            |> List.collect (fun tier ->
                let host, hostPrepared = host.[tier]
                let predicted, predictedPrepared = predicted.[tier]

                List.zip3 (names @ preparedNames) (host @ hostPrepared) (predicted @ predictedPrepared)
                |> List.choose (fun (name, host, predicted) ->
                    if host = predicted then
                        None
                    else
                        Some $"%s{name} at %A{tier}: the host %A{host}, JitTypeLoads %A{predicted}"
                )
            )

        if not disagreements.IsEmpty then
            failwith (String.concat Environment.NewLine disagreements)

        // Enough of each outcome at each tier for the comparison to say something.
        for tier in tiers do
            let host, hostPrepared = host.[tier]

            let count (outcome : Outcome) =
                host |> List.filter ((=) outcome) |> List.length

            count Outcome.Runs |> shouldBeGreaterThan 100
            count (Outcome.Escapes "TypeLoadException") |> shouldBeGreaterThan 100
            count (Outcome.Escapes "FileNotFoundException") |> shouldBeGreaterThan 50
            count (Outcome.Caught "TypeLoadException") |> shouldBeGreaterThan 20
            count (Outcome.Caught "FileNotFoundException") |> shouldBeGreaterThan 10

            let countPrepared (outcome : Outcome) =
                hostPrepared |> List.filter ((=) outcome) |> List.length

            countPrepared Outcome.Runs |> shouldBeGreaterThan 100

            if compilesSwift then
                countPrepared (Outcome.Fails "TypeLoadException") |> shouldBeGreaterThan 50
                countPrepared (Outcome.Fails "FileNotFoundException") |> shouldBeGreaterThan 20
