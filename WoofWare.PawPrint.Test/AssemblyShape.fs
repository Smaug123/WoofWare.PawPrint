namespace WoofWare.PawPrint.Test

open System
open FsCheck
open FsCheck.FSharp
open WoofWare.PawPrint

type TypeShape =
    {
        Name : string
        GenericArity : int
        NestedTypes : TypeShape list
    }

type NamespaceShape =
    {
        Name : string
        Types : TypeShape list
    }

type AssemblyShape =
    {
        Name : string
        Namespaces : NamespaceShape list
    }

type TypeSegment =
    {
        Name : string
        GenericArity : int
    }

type TypePath =
    {
        Namespace : string
        Segments : TypeSegment list
    }

[<RequireQualifiedAccess>]
module AssemblyShape =

    let private namespaceNamePool : string list = [ "N" ; "M" ; "Q" ; "R" ; "S" ; "T" ]

    let private typeKeyPool : (string * int) list =
        [
            "Outer", 0
            "Outer", 1
            "Inner", 0
            "Inner", 0
            "Inner", 1
            "Box", 0
            "Box", 1
            "Box", 2
            "Node", 0
            "Node", 1
            "Holder", 0
            "Holder", 1
            "Value", 0
            "Value", 1
            "Item", 0
            "Item", 1
            "Child", 0
            "Parent", 0
        ]

    let private sequenceGen<'a> (gens : Gen<'a> list) : Gen<'a list> =
        let folder (nextGen : Gen<'a>) (acc : Gen<'a list>) : Gen<'a list> =
            gen {
                let! next = nextGen
                let! rest = acc
                return next :: rest
            }

        List.foldBack folder gens (Gen.constant [])

    let rec private genDistinctFromPool<'a when 'a : equality> (count : int) (pool : 'a list) : Gen<'a list> =
        if count <= 0 then
            Gen.constant []
        else
            gen {
                let! raw = Gen.listOfLength (count * 3 + 2) (Gen.elements pool)
                let picked = raw |> List.distinct |> List.truncate count

                if List.length picked = count then
                    return picked
                else
                    return! genDistinctFromPool count pool
            }

    let metadataName (segment : TypeSegment) : string =
        if segment.GenericArity = 0 then
            segment.Name
        else
            $"{segment.Name}`{segment.GenericArity}"

    let expectedFullName (path : TypePath) : string =
        let typeName = path.Segments |> List.map metadataName |> String.concat "."

        if String.IsNullOrEmpty path.Namespace then
            typeName
        else
            $"{path.Namespace}.{typeName}"

    let rec private collectTypePaths
        (``namespace`` : string)
        (parents : TypeSegment list)
        (shape : TypeShape)
        : TypePath list
        =
        let segment =
            {
                Name = shape.Name
                GenericArity = shape.GenericArity
            }

        let currentSegments = parents @ [ segment ]

        let currentPath =
            {
                Namespace = ``namespace``
                Segments = currentSegments
            }

        currentPath
        :: (shape.NestedTypes
            |> List.collect (collectTypePaths ``namespace`` currentSegments))

    let allTypePaths (shape : AssemblyShape) : TypePath list =
        shape.Namespaces
        |> List.collect (fun ns -> ns.Types |> List.collect (collectTypePaths ns.Name []))

    let private renderTypeParameters (genericArity : int) : string =
        if genericArity = 0 then
            ""
        else
            [ 0 .. genericArity - 1 ]
            |> List.map (fun i -> $"T{i}")
            |> String.concat ", "
            |> fun ps -> $"<{ps}>"

    let private indent (level : int) : string = String.replicate (level * 4) " "

    let rec private renderType (level : int) (shape : TypeShape) : string list =
        let header =
            $"{indent level}public class {shape.Name}{renderTypeParameters shape.GenericArity}"

        if List.isEmpty shape.NestedTypes then
            [ $"{header} {{ }}" ]
        else
            [
                header
                $"{indent level}{{"
                yield! shape.NestedTypes |> List.collect (renderType (level + 1))
                $"{indent level}}}"
            ]

    let private renderNamespace (shape : NamespaceShape) : string =
        [
            $"namespace {shape.Name}"
            "{"
            yield! shape.Types |> List.collect (renderType 1)
            "}"
        ]
        |> String.concat "\n"

    let render (shape : AssemblyShape) : string =
        shape.Namespaces |> List.map renderNamespace |> String.concat "\n\n"

    let private actualTypePath
        (assy : DumpedAssembly)
        (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : string * string list
        =
        let rec go (current : TypeInfo<GenericParamFromMetadata, TypeDefn>) : string * string list =
            if current.IsNested then
                let parent = assy.TypeDefs.[current.DeclaringType]
                let ``namespace``, parentSegments = go parent
                ``namespace``, parentSegments @ [ current.Name ]
            else
                current.Namespace, [ current.Name ]

        go ty

    let findTypeDefByPath (assy : DumpedAssembly) (path : TypePath) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        let expectedSegments = path.Segments |> List.map metadataName

        assy.TypeDefs.Values
        |> Seq.filter (fun ty ->
            let actualNamespace, actualSegments = actualTypePath assy ty
            actualNamespace = path.Namespace && actualSegments = expectedSegments
        )
        |> Seq.exactlyOne

    let private genTypeCount : Gen<int> =
        Gen.frequency [ 2, Gen.constant 1 ; 2, Gen.constant 2 ; 1, Gen.constant 3 ]

    let private genNestedCount : Gen<int> =
        Gen.frequency [ 3, Gen.constant 0 ; 3, Gen.constant 1 ; 1, Gen.constant 2 ]

    let rec private genTypeShape (depth : int) ((name, genericArity) : string * int) : Gen<TypeShape> =
        gen {
            let! nestedKeys =
                if depth <= 0 then
                    Gen.constant []
                else
                    gen {
                        let! nestedCount = genNestedCount
                        return! genDistinctFromPool nestedCount typeKeyPool
                    }

            let! nestedTypes = nestedKeys |> List.map (genTypeShape (depth - 1)) |> sequenceGen

            return
                {
                    Name = name
                    GenericArity = genericArity
                    NestedTypes = nestedTypes
                }
        }

    let private genNamespaceShape (name : string) : Gen<NamespaceShape> =
        gen {
            let! typeCount = genTypeCount
            let! typeKeys = genDistinctFromPool typeCount typeKeyPool
            let! types = typeKeys |> List.map (genTypeShape 2) |> sequenceGen

            return
                {
                    Name = name
                    Types = types
                }
        }

    let gen : Gen<AssemblyShape> =
        gen {
            let! salt = Gen.choose (1, Int32.MaxValue)
            let! namespaceCount = Gen.frequency [ 3, Gen.constant 1 ; 1, Gen.constant 2 ]
            let! namespaceNames = genDistinctFromPool namespaceCount namespaceNamePool
            let! namespaces = namespaceNames |> List.map genNamespaceShape |> sequenceGen

            return
                {
                    Name = $"TypeIdentity.Property.SingleAssembly.{salt}"
                    Namespaces = namespaces
                }
        }

type AssemblyShapeArbitraries =
    static member AssemblyShape () : Arbitrary<AssemblyShape> = Arb.fromGen AssemblyShape.gen
