namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata

type FakeUnit = private | FakeUnit

[<RequireQualifiedAccess>]
module FakeUnit =
    let ofUnit () = FakeUnit.FakeUnit

    let toUnit (f : FakeUnit) =
        match f with
        | FakeUnit.FakeUnit -> ()

/// A type which has been concretised, runtime-representable, etc.
[<CustomEquality>]
[<CustomComparison>]
type ConcreteType<'typeGeneric when 'typeGeneric : comparison and 'typeGeneric :> IComparable<'typeGeneric>> =
    private
        {
            _AssemblyName : AssemblyName
            _Definition : ComparableTypeDefinitionHandle
            _Namespace : string
            _Name : string
            _Generics : 'typeGeneric list
        }

    member this.Assembly : AssemblyName = this._AssemblyName
    member this.Definition : ComparableTypeDefinitionHandle = this._Definition
    member this.Generics : 'typeGeneric list = this._Generics
    member this.Name = this._Name
    member this.Namespace = this._Namespace

    override this.Equals (other : obj) : bool =
        match other with
        | :? ConcreteType<'typeGeneric> as other ->
            this._Generics = other._Generics
            && this._Definition = other._Definition
            && this._AssemblyName.FullName = other._AssemblyName.FullName
        | _ -> false

    override this.GetHashCode () : int =
        hash (this._AssemblyName.FullName, this._Definition, this._Generics)

    interface IComparable<ConcreteType<'typeGeneric>> with
        member this.CompareTo (other : ConcreteType<'typeGeneric>) : int =
            let comp = this._AssemblyName.FullName.CompareTo other._AssemblyName.FullName

            if comp = 0 then
                let comp =
                    (this._Definition :> IComparable<ComparableTypeDefinitionHandle>).CompareTo other._Definition

                if comp = 0 then
                    let thisGen = (this._Generics : 'typeGeneric list) :> IComparable<'typeGeneric list>
                    thisGen.CompareTo other._Generics
                else
                    comp
            else
                comp

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? ConcreteType<'typeGeneric> as other ->
                (this :> IComparable<ConcreteType<'typeGeneric>>).CompareTo other
            | _ -> failwith "bad comparison"

/// Because a runtime type may depend on itself by some chain of generics, we need to break the indirection;
/// we do so by storing all concrete types in some global mapping and then referring to them only by handle.
type ConcreteTypeHandle = ConcreteTypeHandle of int

type AllConcreteTypes =
    {
        Mapping : Map<ConcreteTypeHandle, ConcreteType<ConcreteTypeHandle>>
        NextHandle : int
    }

    static member Empty =
        {
            Mapping = Map.empty
            NextHandle = 0
        }

[<RequireQualifiedAccess>]
module AllConcreteTypes =
    let lookup (cth : ConcreteTypeHandle) (this : AllConcreteTypes) : ConcreteType<ConcreteTypeHandle> option =
        this.Mapping
        |> Map.tryFind cth

    /// `source` is AssemblyName * Namespace * Name
    let add (ct : ConcreteType<ConcreteTypeHandle>) (this : AllConcreteTypes) : ConcreteTypeHandle * AllConcreteTypes =
        let toRet = ConcreteTypeHandle this.NextHandle
        let newState =
            {
                NextHandle = this.NextHandle + 1
                Mapping = this.Mapping |> Map.add toRet ct
            }
        toRet, newState

    let concretiseType
        (mapping : AllConcreteTypes)
        (typeGenerics : TypeDefn ImmutableArray)
        (methodGenerics : TypeDefn ImmutableArray)
        (defn : AssemblyName * TypeDefn)
        : Map<TypeDefn, ConcreteTypeHandle> * AllConcreteTypes
        =
        failwith ""

[<RequireQualifiedAccess>]
module ConcreteType =
    let make
        (mapping : AllConcreteTypes)
        (assemblyName : AssemblyName)
        (ns : string)
        (name : string)
        (defn : TypeDefinitionHandle)
        (generics : ConcreteTypeHandle list)
        : ConcreteTypeHandle * AllConcreteTypes
        =
        let toAdd =
            {
                _AssemblyName = assemblyName
                _Definition = ComparableTypeDefinitionHandle.Make defn
                _Name = name
                _Namespace = ns
                _Generics = generics
            }
        AllConcreteTypes.add toAdd mapping

    let make'
        (assemblyName : AssemblyName)
        (defn : TypeDefinitionHandle)
        (ns : string)
        (name : string)
        (genericParamCount : int)
        : ConcreteType<FakeUnit>
        =
        {
            _AssemblyName = assemblyName
            _Definition = ComparableTypeDefinitionHandle.Make defn
            _Namespace = ns
            _Name = name
            _Generics = List.replicate genericParamCount FakeUnit.FakeUnit
        }

    let mapGeneric<'a, 'b
        when 'a : comparison and 'a :> IComparable<'a> and 'b : equality and 'b : comparison and 'b :> IComparable<'b>>
        (f : int -> 'a -> 'b)
        (x : ConcreteType<'a>)
        : ConcreteType<'b>
        =
        let generics = x._Generics |> List.mapi f

        {
            _AssemblyName = x._AssemblyName
            _Definition = x._Definition
            _Generics = generics
            _Namespace = x._Namespace
            _Name = x._Name
        }
