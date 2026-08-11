namespace WoofWare.PawPrint

open System
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335

[<CustomEquality>]
[<CustomComparison>]
type ComparablePropertyDefinitionHandle =
    private
        {
            _Inner : PropertyDefinitionHandle
        }

    override this.Equals other =
        match other with
        | :? ComparablePropertyDefinitionHandle as other -> this._Inner.GetHashCode () = other._Inner.GetHashCode ()
        | _ -> false

    override this.GetHashCode () : int = this._Inner.GetHashCode ()

    interface IComparable<ComparablePropertyDefinitionHandle> with
        member this.CompareTo (other : ComparablePropertyDefinitionHandle) : int =
            this._Inner.GetHashCode().CompareTo (other._Inner.GetHashCode ())

    interface IComparable with
        member this.CompareTo (other : obj) : int =
            match other with
            | :? ComparablePropertyDefinitionHandle as other ->
                (this :> IComparable<ComparablePropertyDefinitionHandle>).CompareTo other
            | _ -> failwith "invalid comparison"

    /// `PropertyDefinitionHandle` inherits `ToString` from `obj`, so rendering one yields the literal
    /// text "System.Reflection.Metadata.PropertyDefinitionHandle" -- which makes every diagnostic
    /// that names a property identity useless for telling two properties apart. Render the metadata
    /// token instead, in the `0x17######` form ildasm and the ECMA-335 tables use.
    override this.ToString () : string =
        let token =
            MetadataTokens.GetToken (PropertyDefinitionHandle.op_Implicit this._Inner : EntityHandle)

        $"Property(0x%08x{token})"

    static member Make (h : PropertyDefinitionHandle) =
        {
            _Inner = h
        }

    member this.Get = this._Inner
