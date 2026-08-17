namespace WoofWare.PawPrint

open System
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335

[<CustomEquality>]
[<CustomComparison>]
type ComparableConstantHandle =
    private
        {
            _Inner : ConstantHandle
        }

    override this.Equals other =
        match other with
        | :? ComparableConstantHandle as other -> this._Inner.GetHashCode () = other._Inner.GetHashCode ()
        | _ -> false

    override this.GetHashCode () : int = this._Inner.GetHashCode ()

    interface IComparable<ComparableConstantHandle> with
        member this.CompareTo (other : ComparableConstantHandle) : int =
            this._Inner.GetHashCode().CompareTo (other._Inner.GetHashCode ())

    interface IComparable with
        member this.CompareTo (other : obj) : int =
            match other with
            | :? ComparableConstantHandle as other -> (this :> IComparable<ComparableConstantHandle>).CompareTo other
            | _ -> failwith "invalid comparison"

    /// `ConstantHandle` inherits `ToString` from `obj`, so rendering one yields the literal text
    /// "System.Reflection.Metadata.ConstantHandle" -- which makes every diagnostic that names a
    /// constant useless for telling two of them apart. Render the metadata token instead, in the
    /// `0x0B######` form ildasm and the ECMA-335 tables use.
    override this.ToString () : string =
        let token =
            MetadataTokens.GetToken (ConstantHandle.op_Implicit this._Inner : EntityHandle)

        $"Constant(0x%08x{token})"

    static member Make (h : ConstantHandle) =
        {
            _Inner = h
        }

    member this.Get = this._Inner
