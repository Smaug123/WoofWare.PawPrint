namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Reflection.Metadata
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// A struct implementing two interfaces that each give their common base interface's method a
/// default body, neither more specific than the other: `ILeft : IBase` and `IRight : IBase`, each
/// overriding `IBase.M`, and `Both : ILeft, IRight`. CoreCLR loads `Both`, and a call of `IBase.M`
/// on it throws `AmbiguousImplementationException`. C# refuses to compile such a type, so it is
/// emitted directly.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestAmbiguousDefaultInterfaceDispatch =

    /// The image, with `Run.Call()` making `constrained. Both callvirt IBase::M` on a default `Both`.
    let fabricate () : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "Diamond", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Diamond"

        let interfaceAttributes =
            TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract

        let baseInterface = modul.DefineType ("IBase", interfaceAttributes)

        let baseMethod =
            baseInterface.DefineMethod (
                "M",
                MethodAttributes.Public
                ||| MethodAttributes.Abstract
                ||| MethodAttributes.Virtual
                ||| MethodAttributes.HideBySig
                ||| MethodAttributes.NewSlot,
                typeof<int>,
                [||]
            )

        baseInterface.CreateType () |> ignore<Type>

        let withDefault (name : string) (value : int) : TypeBuilder =
            let derived = modul.DefineType (name, interfaceAttributes)
            derived.AddInterfaceImplementation baseInterface

            let body =
                derived.DefineMethod (
                    "IBase.M",
                    MethodAttributes.Private
                    ||| MethodAttributes.Final
                    ||| MethodAttributes.Virtual
                    ||| MethodAttributes.HideBySig
                    ||| MethodAttributes.NewSlot,
                    typeof<int>,
                    [||]
                )

            let il = body.GetILGenerator ()
            il.Emit (OpCodes.Ldc_I4, value)
            il.Emit OpCodes.Ret
            derived.DefineMethodOverride (body, baseMethod)
            derived.CreateType () |> ignore<Type>
            derived

        let left = withDefault "ILeft" 1
        let right = withDefault "IRight" 2

        let both =
            modul.DefineType (
                "Both",
                TypeAttributes.Public
                ||| TypeAttributes.Sealed
                ||| TypeAttributes.SequentialLayout,
                typeof<ValueType>,
                [| baseInterface :> Type ; left :> Type ; right :> Type |]
            )

        both.CreateType () |> ignore<Type>

        let run =
            modul.DefineType ("Run", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let call =
            run.DefineMethod ("Call", MethodAttributes.Public ||| MethodAttributes.Static, typeof<int>, [||])

        do
            let il = call.GetILGenerator ()
            let receiver = il.DeclareLocal both
            il.Emit (OpCodes.Ldloca, receiver)
            il.Emit (OpCodes.Initobj, both)
            il.Emit (OpCodes.Ldloca, receiver)
            il.Emit (OpCodes.Constrained, both)
            il.Emit (OpCodes.Callvirt, baseMethod)
            il.Emit OpCodes.Ret

        run.CreateType () |> ignore<Type>

        use stream = new MemoryStream ()
        builder.Save stream
        stream.ToArray ()

    [<Test>]
    let ``two equally specific default bodies make dispatch ambiguous`` () : unit =
        let image = fabricate ()

        // The real runtime loads `Both`, and throws when the call is dispatched.
        do
            let context =
                System.Runtime.Loader.AssemblyLoadContext ("Diamond", isCollectible = true)

            try
                let run = context.LoadFromStream(new MemoryStream (image)).GetType "Run"

                let thrown =
                    try
                        run.GetMethod("Call").Invoke ((null : obj), [||]) |> ignore<obj>
                        None
                    with :? TargetInvocationException as e ->
                        Some (e.InnerException.GetType().FullName)

                thrown |> shouldEqual (Some "System.Runtime.AmbiguousImplementationException")
            finally
                context.Unload ()

        let _, loggerFactory = LoggerFactory.makeTest ()
        let runtimeDirs = FrameworkUnderTest.runtimeDirs ()

        let corelib =
            Assembly.readFile
                loggerFactory
                (Path.Combine (FrameworkUnderTest.sharedFrameworkDirectory (), "System.Private.CoreLib.dll"))

        let diamond =
            Assembly.read loggerFactory (Some "Diamond.dll") (new MemoryStream (image))

        let bct = BaseClassTypes.ofCorelib corelib
        let loaded = LoadedAssemblies.ofAssemblies [ corelib ; diamond ]

        let state =
            { TypeSystemState.Empty with
                _LoadedAssemblies = loaded
                ConcreteTypes = Corelib.concretizeAll loaded bct AllConcreteTypes.Empty
            }

        let typeNamed (name : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
            diamond.TypeDefs.Values |> Seq.find (fun ty -> ty.Name = name)

        let state, receiver =
            TypeSystemState.concretizeType
                loggerFactory
                runtimeDirs
                bct
                state
                diamond.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition ((typeNamed "Both").Identity, SignatureTypeKind.ValueType))

        let method = (typeNamed "IBase").Methods |> List.find (fun m -> m.Name = "M")

        let state, concretized, _ =
            MethodConcretisation.concretizeMethodWithAllGenerics
                loggerFactory
                runtimeDirs
                bct
                ImmutableArray.Empty
                method
                ImmutableArray.Empty
                state

        let _, resolved =
            ConcreteVirtualDispatch.tryResolveVirtualImplementation
                loggerFactory
                runtimeDirs
                bct
                concretized.Generics
                concretized
                receiver
                true
                state

        match resolved with
        | VirtualImplementation.Ambiguous candidates ->
            candidates
            |> List.map (fun m -> m.RequiredDeclaringType.Name)
            |> List.sort
            |> shouldEqual [ "ILeft" ; "IRight" ]
        | other -> failwith $"expected an ambiguous dispatch, got %A{other}"

    /// A struct implementing `I<string>` and `I<Exception>` of a covariant `I<out T>`, each instantiation
    /// with a default body from an interface of its own, called through `I<object>`. Both bodies are
    /// variance-compatible with the call, but CoreCLR's variance pass takes one rather than throwing.
    let variantSource : string =
        """
public interface I<out T> { int M(int n); }
public interface L : I<string> { int I<string>.M(int n) => 1 / n; }
public interface R : I<System.Exception> { int I<System.Exception>.M(int n) => 2 / n; }
public struct S : L, R { }

public static class Run
{
    public static int Call<T>(T x, int n) where T : I<object> => x.M(n);
    public static int Go(int n) => Call(new S(), n);
}
"""

    [<Test>]
    let ``default bodies that conflict only through variance are not reported ambiguous`` () : unit =
        let image =
            Roslyn.compileAssembly "Variant" OutputKind.DynamicallyLinkedLibrary [] [ variantSource ]

        // The real runtime runs one of the bodies.
        do
            let context =
                System.Runtime.Loader.AssemblyLoadContext ("Variant", isCollectible = true)

            try
                let run = context.LoadFromStream(new MemoryStream (image)).GetType "Run"

                let thrown =
                    try
                        run.GetMethod("Go").Invoke ((null : obj), [| box 0 |]) |> ignore<obj>
                        None
                    with :? TargetInvocationException as e ->
                        Some (e.InnerException.GetType().FullName)

                thrown |> shouldEqual (Some "System.DivideByZeroException")
            finally
                context.Unload ()

        let _, loggerFactory = LoggerFactory.makeTest ()
        let runtimeDirs = FrameworkUnderTest.runtimeDirs ()

        let corelib =
            Assembly.readFile
                loggerFactory
                (Path.Combine (FrameworkUnderTest.sharedFrameworkDirectory (), "System.Private.CoreLib.dll"))

        let variant =
            Assembly.read loggerFactory (Some "Variant.dll") (new MemoryStream (image))

        let bct = BaseClassTypes.ofCorelib corelib
        let loaded = LoadedAssemblies.ofAssemblies [ corelib ; variant ]
        let concreteTypes = Corelib.concretizeAll loaded bct AllConcreteTypes.Empty

        let state =
            { TypeSystemState.Empty with
                _LoadedAssemblies = loaded
                ConcreteTypes = concreteTypes
            }

        let typeNamed (name : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
            variant.TypeDefs.Values |> Seq.find (fun ty -> ty.Name = name)

        let state, receiver =
            TypeSystemState.concretizeType
                loggerFactory
                runtimeDirs
                bct
                state
                variant.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition ((typeNamed "S").Identity, SignatureTypeKind.ValueType))

        let method = (typeNamed "I`1").Methods |> List.find (fun m -> m.Name = "M")

        let state, concretized, _ =
            MethodConcretisation.concretizeMethodWithAllGenerics
                loggerFactory
                runtimeDirs
                bct
                (ImmutableArray.Create (AllConcreteTypes.getRequiredNonGenericHandle concreteTypes bct.Object))
                method
                ImmutableArray.Empty
                state

        let _, resolved =
            ConcreteVirtualDispatch.tryResolveVirtualImplementation
                loggerFactory
                runtimeDirs
                bct
                concretized.Generics
                concretized
                receiver
                true
                state

        match resolved with
        | VirtualImplementation.Found found when
            found.Definition.RequiredDeclaringType.Name = "L"
            || found.Definition.RequiredDeclaringType.Name = "R"
            ->
            ()
        | VirtualImplementation.Unmodelled _ -> ()
        | other -> failwith $"expected one of the bodies, or a refusal to choose; got %A{other}"
