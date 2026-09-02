namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `ConcretePrimitive` is the classifier behind every "is this handle the CLR `int32`?" arm in
/// the intrinsics and at the native boundary. A guest assembly may declare its own
/// `System.Int32`, and that is an ordinary struct, so the pattern must answer for corelib's
/// types alone.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestConcretePrimitivePattern =

    let private corelib : DumpedAssembly =
        // The factory is intentionally undisposed: the returned DumpedAssembly.Logger closes over
        // its sinks, and disposing while the assembly is still live would silently drop events.
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory (typeof<obj>.Assembly.Location)

    let private assemblies : LoadedAssemblies =
        LoadedAssemblies.ofAssemblies [ corelib ]

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private baseConcreteTypes : AllConcreteTypes =
        Corelib.concretizeAll assemblies bct AllConcreteTypes.Empty

    /// Every primitive the pattern can answer, with the corelib TypeDef that is that primitive.
    let private primitives : (PrimitiveType * TypeInfo<GenericParamFromMetadata, TypeDefn>) list =
        [
            PrimitiveType.Boolean, bct.Boolean
            PrimitiveType.Char, bct.Char
            PrimitiveType.SByte, bct.SByte
            PrimitiveType.Byte, bct.Byte
            PrimitiveType.Int16, bct.Int16
            PrimitiveType.UInt16, bct.UInt16
            PrimitiveType.Int32, bct.Int32
            PrimitiveType.UInt32, bct.UInt32
            PrimitiveType.Int64, bct.Int64
            PrimitiveType.UInt64, bct.UInt64
            PrimitiveType.Single, bct.Single
            PrimitiveType.Double, bct.Double
            PrimitiveType.String, bct.String
            PrimitiveType.TypedReference, bct.TypedReference
            PrimitiveType.IntPtr, bct.IntPtr
            PrimitiveType.UIntPtr, bct.UIntPtr
            PrimitiveType.Object, bct.Object
        ]

    let private primitiveCases : obj[] list =
        primitives |> List.map (fun (primitive, ti) -> [| box primitive ; box ti |])

    [<TestCaseSource(nameof primitiveCases)>]
    let ``corelib's own primitive type matches``
        (primitive : PrimitiveType)
        (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : unit
        =
        let handle = AllConcreteTypes.getRequiredNonGenericHandle baseConcreteTypes ti

        match handle with
        | ConcretePrimitive baseConcreteTypes matched -> matched |> shouldEqual primitive
        | _ -> failwith $"corelib's %s{ti.Namespace}.%s{ti.Name} did not match ConcretePrimitive"

    /// A lookalike is a type declared by some other assembly under corelib's namespace and name.
    /// Its TypeDef handle is corelib's for convenience: the identity differs in its assembly, which
    /// is the only part of it the pattern is allowed to consult.
    [<TestCaseSource(nameof primitiveCases)>]
    let ``a lookalike declared by another assembly does not match``
        (_ : PrimitiveType)
        (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : unit
        =
        let guestIdentity =
            ResolvedTypeIdentity.ofDefinitionInAssembly
                "Guest, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null"
                ti.Identity.TypeDefinition.Get

        let lookalike =
            ConcreteType.makeFromIdentity guestIdentity ti.Namespace ti.Name ImmutableArray.Empty

        let handle, concreteTypes = AllConcreteTypes.add lookalike baseConcreteTypes

        match handle with
        | ConcretePrimitive concreteTypes matched ->
            failwith
                $"a guest's %s{ti.Namespace}.%s{ti.Name} matched ConcretePrimitive as %O{matched}; only corelib declares the primitives"
        | _ -> ()
