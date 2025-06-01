namespace WoofWare.PawPrint

open System
open System.Reflection.Metadata

[<CustomEquality>]
[<CustomComparison>]
type ComparableSignatureHeader =
    private
        {
            _Inner : SignatureHeader
        }

    member this.Get = this._Inner

    override this.Equals (other : obj) =
        match other with
        | :? ComparableSignatureHeader as other -> this._Inner.RawValue = other._Inner.RawValue
        | _ -> false

    override this.GetHashCode () = this._Inner.RawValue.GetHashCode ()

    interface IComparable<ComparableSignatureHeader> with
        member this.CompareTo (other : ComparableSignatureHeader) =
            this._Inner.RawValue.CompareTo other._Inner.RawValue

    interface IComparable with
        member this.CompareTo (other : obj) =
            match other with
            | :? ComparableSignatureHeader as other -> (this :> IComparable<ComparableSignatureHeader>).CompareTo other
            | _ -> failwith "invalid comparison"

    static member Make x : ComparableSignatureHeader =
        {
            _Inner = x
        }
