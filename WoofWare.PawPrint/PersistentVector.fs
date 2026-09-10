namespace WoofWare.PawPrint

open System
open System.Collections
open System.Collections.Generic

/// One level of a `PersistentVector`'s trie. A `Leaf` holds up to 32 elements; a `Branch`
/// holds up to 32 children, and every child of a branch has the same depth, so the leaf
/// holding index `i` is found by consuming five bits of `i` per level from the top.
[<RequireQualifiedAccess>]
type internal PersistentVectorNode<'T> =
    | Leaf of 'T[]
    | Branch of PersistentVectorNode<'T>[]

[<RequireQualifiedAccess>]
module internal PersistentVectorNode =
    /// Bits of the index consumed per level.
    let bits = 5
    /// Fan-out of a node.
    let width = 1 <<< bits
    let mask = width - 1

    /// Builds the trie over `source`. Returns the shift the root is addressed at, which
    /// is `bits` times the number of branch levels: 0 when the root is itself a leaf.
    let ofArray (source : 'T[]) : int * PersistentVectorNode<'T> =
        let n = source.Length

        let leafCount = (n + mask) >>> bits

        let mutable level : PersistentVectorNode<'T>[] =
            Array.init
                leafCount
                (fun leaf ->
                    let start = leaf <<< bits
                    PersistentVectorNode.Leaf (Array.sub source start (min width (n - start)))
                )

        let mutable shift = 0

        while level.Length > 1 do
            let parentCount = (level.Length + mask) >>> bits

            level <-
                Array.init
                    parentCount
                    (fun parent ->
                        let start = parent <<< bits
                        PersistentVectorNode.Branch (Array.sub level start (min width (level.Length - start)))
                    )

            shift <- shift + bits

        if level.Length = 0 then
            0, PersistentVectorNode.Leaf [||]
        else
            shift, level.[0]

    /// `index` must already be known to lie within the vector.
    let rec item (index : int) (shift : int) (node : PersistentVectorNode<'T>) : 'T =
        match node with
        | PersistentVectorNode.Leaf cells -> cells.[index &&& mask]
        | PersistentVectorNode.Branch children -> item index (shift - bits) children.[(index >>> shift) &&& mask]

    /// Copies the path from the root to the leaf holding `index`, sharing every other node
    /// with the input. `index` must already be known to lie within the vector.
    let rec set (index : int) (value : 'T) (shift : int) (node : PersistentVectorNode<'T>) : PersistentVectorNode<'T> =
        match node with
        | PersistentVectorNode.Leaf cells ->
            let cells = Array.copy cells
            cells.[index &&& mask] <- value
            PersistentVectorNode.Leaf cells
        | PersistentVectorNode.Branch children ->
            let child = (index >>> shift) &&& mask
            let children = Array.copy children
            children.[child] <- set index value (shift - bits) children.[child]
            PersistentVectorNode.Branch children

    /// Writes every element, in index order, into `target` starting at `position`; returns
    /// the position after the last element written.
    let rec copyTo (target : 'T[]) (position : int) (node : PersistentVectorNode<'T>) : int =
        match node with
        | PersistentVectorNode.Leaf cells ->
            Array.blit cells 0 target position cells.Length
            position + cells.Length
        | PersistentVectorNode.Branch children -> children |> Array.fold (copyTo target) position

/// An immutable fixed-length vector with O(log32 n) element read and element replacement,
/// where replacement shares all but the replaced element's root-to-leaf path with the
/// original. That is what makes it the backing store for a guest array: `ImmutableArray`'s
/// `SetItem` copies every cell, so a guest loop storing into a 65536-element array copied
/// half a megabyte per store.
///
/// Equality and hashing are structural over the elements, in index order.
[<Sealed>]
type PersistentVector<'T> internal (length : int, shift : int, root : PersistentVectorNode<'T>) =
    member internal _.Shift = shift
    member internal _.Root = root

    member _.Length : int = length
    member _.IsEmpty : bool = length = 0

    member private _.CheckIndex (index : int) : unit =
        if index < 0 || index >= length then
            raise (IndexOutOfRangeException $"index %d{index} is outside the vector's length %d{length}")

    member this.Item
        with get (index : int) : 'T =
            this.CheckIndex index
            PersistentVectorNode.item index shift root

    /// A vector with `value` at `index` and every other element as in this one.
    member this.Set (index : int, value : 'T) : PersistentVector<'T> =
        this.CheckIndex index
        PersistentVector (length, shift, PersistentVectorNode.set index value shift root)

    member _.ToArray () : 'T[] =
        let target = Array.zeroCreate length
        let written = PersistentVectorNode.copyTo target 0 root

        if written <> length then
            failwith
                $"PersistentVector of length %d{length} holds %d{written} elements (this is a bug in PersistentVector)"

        target

    override this.Equals (other : obj) : bool =
        match other with
        | :? PersistentVector<'T> as other ->
            length = other.Length
            && Seq.forall2 (fun (a : 'T) (b : 'T) -> EqualityComparer<'T>.Default.Equals (a, b)) this other
        | _ -> false

    override this.GetHashCode () : int =
        let mutable hash = HashCode ()
        hash.Add length

        for element in this do
            hash.Add (element, EqualityComparer<'T>.Default)

        hash.ToHashCode ()

    interface IEquatable<PersistentVector<'T>> with
        member this.Equals (other : PersistentVector<'T>) : bool = this.Equals (other :> obj)

    interface IEnumerable<'T> with
        member this.GetEnumerator () : IEnumerator<'T> =
            (this.ToArray () :> IEnumerable<'T>).GetEnumerator ()

    interface IEnumerable with
        member this.GetEnumerator () : IEnumerator =
            (this.ToArray () :> IEnumerable).GetEnumerator ()

[<RequireQualifiedAccess>]
module PersistentVector =
    let empty<'T> : PersistentVector<'T> =
        PersistentVector<'T> (0, 0, PersistentVectorNode.Leaf [||])

    /// The vector holding `source`'s elements. Does not retain `source`.
    let ofArray (source : 'T[]) : PersistentVector<'T> =
        let shift, root = PersistentVectorNode.ofArray source
        PersistentVector (source.Length, shift, root)

    let ofSeq (source : seq<'T>) : PersistentVector<'T> = ofArray (Seq.toArray source)

    /// The vector of length `length` whose element at `i` is `f i`, with `f` called once per
    /// index in increasing order.
    let init (length : int) (f : int -> 'T) : PersistentVector<'T> = ofArray (Array.init length f)

    let length (v : PersistentVector<'T>) : int = v.Length

    let isEmpty (v : PersistentVector<'T>) : bool = v.IsEmpty

    /// Raises `IndexOutOfRangeException` when `index` is not within the vector.
    let item (index : int) (v : PersistentVector<'T>) : 'T = v.[index]

    /// A vector with `value` at `index` and every other element as in `v`. Raises
    /// `IndexOutOfRangeException` when `index` is not within the vector.
    let set (index : int) (value : 'T) (v : PersistentVector<'T>) : PersistentVector<'T> = v.Set (index, value)

    let toArray (v : PersistentVector<'T>) : 'T[] = v.ToArray ()

    let toSeq (v : PersistentVector<'T>) : seq<'T> = v :> seq<'T>
