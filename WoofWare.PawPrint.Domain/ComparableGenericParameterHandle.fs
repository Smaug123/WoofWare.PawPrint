namespace WoofWare.PawPrint

open System
open System.Reflection.Metadata

[<CustomEquality>]
[<CustomComparison>]
type ComparableGenericParameterHandle =
    private
        {
            _Inner : GenericParameterHandle
        }

    override this.Equals other =
        match other with
        | :? ComparableGenericParameterHandle as other -> this._Inner.GetHashCode () = other._Inner.GetHashCode ()
        | _ -> false

    override this.GetHashCode () : int = this._Inner.GetHashCode ()

    interface IComparable<ComparableGenericParameterHandle> with
        member this.CompareTo (other : ComparableGenericParameterHandle) : int =
            this._Inner.GetHashCode().CompareTo (other._Inner.GetHashCode ())

    interface IComparable with
        member this.CompareTo (other : obj) : int =
            match other with
            | :? ComparableGenericParameterHandle as other ->
                (this :> IComparable<ComparableGenericParameterHandle>).CompareTo other
            | _ -> failwith "invalid comparison"

    static member Make (h : GenericParameterHandle) =
        {
            _Inner = h
        }

    member this.Get = this._Inner
