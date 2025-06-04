namespace WoofWare.PawPrint

open System
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
            _Generics : 'typeGeneric list
        }

    member this.Assembly : AssemblyName = this._AssemblyName
    member this.Definition : ComparableTypeDefinitionHandle = this._Definition
    member this.Generics : 'typeGeneric list = this._Generics

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

type RuntimeConcreteType = ConcreteType<TypeDefn>

[<RequireQualifiedAccess>]
module ConcreteType =
    let make
        (assemblyName : AssemblyName)
        (defn : TypeDefinitionHandle)
        (generics : TypeDefn list)
        : RuntimeConcreteType
        =
        {
            _AssemblyName = assemblyName
            _Definition = ComparableTypeDefinitionHandle.Make defn
            _Generics = generics
        }

    let make'
        (assemblyName : AssemblyName)
        (defn : TypeDefinitionHandle)
        (genericParamCount : int)
        : ConcreteType<FakeUnit>
        =
        {
            _AssemblyName = assemblyName
            _Definition = ComparableTypeDefinitionHandle.Make defn
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
        }
