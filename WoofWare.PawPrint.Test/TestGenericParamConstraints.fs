namespace WoofWare.PawPrint.Test

open System.IO
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
module TestGenericParamConstraints =

    let private loadAssembly (assemblyName : string) (source : string) : DumpedAssembly =
        let image =
            Roslyn.compileAssembly assemblyName Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary [] [ source ]

        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = new MemoryStream (image)
        global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None stream

    let private findType (typeName : string) (assembly : DumpedAssembly) =
        assembly.TypeDefs.Values |> Seq.find (fun ty -> ty.Name = typeName)

    [<Test>]
    let ``base-class and interface constraints are surfaced as Constraints array`` () =
        let source =
            """
using System.Collections.Generic;

public class MyBase { }

public class C<T> where T : MyBase, IEnumerable<int> { }
"""

        let assembly = loadAssembly "GenericParamConstraintsTestAssembly" source
        let cType = findType "C`1" assembly

        Assert.That (cType.Generics.Length, Is.EqualTo 1)

        let _, paramMd = cType.Generics.[0]
        let constraints = paramMd.Constraints

        Assert.That (constraints.Length, Is.EqualTo 2, $"expected 2 constraints, got %i{constraints.Length}")

        let isMyBaseFromDefinition (t : TypeDefn) : bool =
            match t with
            | TypeDefn.FromDefinition (identity, _) ->
                identity.AssemblyFullName = assembly.Name.FullName
                && (
                    match assembly.TypeDefs.TryGetValue identity.TypeDefinition.Get with
                    | true, typeInfo -> typeInfo.Name = "MyBase"
                    | false, _ -> false
                )
            | _ -> false

        let isIEnumerableInt (t : TypeDefn) : bool =
            match t with
            | TypeDefn.GenericInstantiation (TypeDefn.FromReference (typeRef, _), args) ->
                typeRef.Name = "IEnumerable`1"
                && typeRef.Namespace = "System.Collections.Generic"
                && args.Length = 1
                && args.[0] = TypeDefn.PrimitiveType PrimitiveType.Int32
            | _ -> false

        Assert.That (
            constraints |> Seq.exists isMyBaseFromDefinition,
            $"expected MyBase FromDefinition in %A{constraints |> Seq.toList}"
        )

        Assert.That (
            constraints |> Seq.exists isIEnumerableInt,
            $"expected IEnumerable<int> GenericInstantiation in %A{constraints |> Seq.toList}"
        )

    [<Test>]
    let ``no constraints yields an empty Constraints array`` () =
        let source =
            """
public class C<T> { }
"""

        let assembly = loadAssembly "GenericParamConstraintsEmpty" source
        let cType = findType "C`1" assembly

        Assert.That (cType.Generics.Length, Is.EqualTo 1)
        let _, paramMd = cType.Generics.[0]
        Assert.That (paramMd.Constraints.Length, Is.EqualTo 0)

    [<Test>]
    let ``where T : class still yields an empty Constraints array`` () =
        // The "class" flag-style constraint is captured in Constraint, not Constraints.
        let source =
            """
public class C<T> where T : class { }
"""

        let assembly = loadAssembly "GenericParamConstraintsClassFlag" source
        let cType = findType "C`1" assembly

        let _, paramMd = cType.Generics.[0]
        Assert.That (paramMd.Constraint, Is.EqualTo (Some GenericConstraint.Reference))
        Assert.That (paramMd.Constraints.Length, Is.EqualTo 0)

    [<Test>]
    let ``where T : struct does not surface the synthetic System.ValueType row`` () =
        // Roslyn emits a TypeRef to System.ValueType alongside the
        // NotNullableValueTypeConstraint flag. That row is redundant with the
        // flag, so it must not appear in Constraints.
        let source =
            """
public class C<T> where T : struct { }
"""

        let assembly = loadAssembly "GenericParamConstraintsStructFlag" source
        let cType = findType "C`1" assembly

        let _, paramMd = cType.Generics.[0]
        Assert.That (paramMd.Constraint, Is.EqualTo (Some GenericConstraint.NonNullableValue))
        Assert.That (paramMd.RequiresParameterlessConstructor, Is.True)
        Assert.That (paramMd.Constraints.Length, Is.EqualTo 0)

    [<Test>]
    let ``where T : struct, IComparable keeps the user constraint and drops the synthetic row`` () =
        let source =
            """
using System;
public class C<T> where T : struct, IComparable { }
"""

        let assembly = loadAssembly "GenericParamConstraintsStructPlusInterface" source
        let cType = findType "C`1" assembly

        let _, paramMd = cType.Generics.[0]
        Assert.That (paramMd.Constraint, Is.EqualTo (Some GenericConstraint.NonNullableValue))
        Assert.That (paramMd.Constraints.Length, Is.EqualTo 1)

        match paramMd.Constraints.[0] with
        | TypeDefn.FromReference (typeRef, _) ->
            Assert.That (typeRef.Name, Is.EqualTo "IComparable")
            Assert.That (typeRef.Namespace, Is.EqualTo "System")
        | other -> Assert.Fail $"expected IComparable FromReference, got %O{other}"

    [<Test>]
    let ``where T : System.Enum surfaces the explicit constraint`` () =
        // Unlike `struct`, an explicit Enum constraint is not a flag-style
        // constraint: it must appear in Constraints.
        let source =
            """
public class C<T> where T : System.Enum { }
"""

        let assembly = loadAssembly "GenericParamConstraintsEnum" source
        let cType = findType "C`1" assembly

        let _, paramMd = cType.Generics.[0]
        Assert.That (paramMd.Constraint, Is.EqualTo (None : GenericConstraint option))
        Assert.That (paramMd.Constraints.Length, Is.EqualTo 1)

        match paramMd.Constraints.[0] with
        | TypeDefn.FromReference (typeRef, _) ->
            Assert.That (typeRef.Name, Is.EqualTo "Enum")
            Assert.That (typeRef.Namespace, Is.EqualTo "System")
        | other -> Assert.Fail $"expected System.Enum FromReference, got %O{other}"
