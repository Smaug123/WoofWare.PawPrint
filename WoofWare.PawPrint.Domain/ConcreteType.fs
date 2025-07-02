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
            /// Do not use this, because it's intended to be private; use the accessor `.Assembly : AssemblyName`
            /// instead.
            _AssemblyName : AssemblyName
            /// Do not use this, because it's intended to be private; use the accessor `.Definition` instead.
            _Definition : ComparableTypeDefinitionHandle
            /// Do not use this, because it's intended to be private; use the accessor `.Name` instead.
            _Name : string
            /// Do not use this, because it's intended to be private; use the accessor `.Namespace` instead.
            _Namespace : string
            /// Do not use this, because it's intended to be private; use the accessor `.Generics` instead.
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

            if comp <> 0 then
                comp
            else

            let comp =
                (this._Definition :> IComparable<ComparableTypeDefinitionHandle>).CompareTo other._Definition

            if comp <> 0 then
                comp
            else

            let thisGen = (this._Generics : 'typeGeneric list) :> IComparable<'typeGeneric list>
            thisGen.CompareTo other._Generics

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
        (ns : string)
        (name : string)
        (defn : TypeDefinitionHandle)
        (generics : TypeDefn list)
        : RuntimeConcreteType
        =
        {
            _AssemblyName = assemblyName
            _Definition = ComparableTypeDefinitionHandle.Make defn
            _Name = name
            _Namespace = ns
            _Generics = generics
        }

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
            _Name = name
            _Namespace = ns
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
            _Name = x._Name
            _Namespace = x._Namespace
        }
