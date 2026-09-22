// Produces the plan's §2.1 table: what ImmutableArray<byte> actually does
// under F#'s generic equality and comparison.
//
// Equality and hashing are structural (ImmutableArray<T> implements
// IStructuralEquatable, which F# honours, so the IEquatable reference
// comparison never gets a look in). Comparison is the hazard: it delegates to
// Array's IStructuralComparable, which refuses two arrays of unequal length,
// so Map/Set/List.sort over bare ImmutableArray keys throw as soon as the
// lengths differ.
//
// Note every case here uses DIFFERING lengths where it matters. A first
// version of this probe compared two 3-byte keys and reported that Map worked.
//
// Run: nix develop -c dotnet fsi fsharp-immutablearray-semantics.fsx
open System.Collections.Immutable
let ia (xs: byte list) = ImmutableArray.CreateRange xs

let t name f =
    printfn
        "%-46s %s"
        name
        (try
            f ()
         with e ->
             sprintf "THROWS %s" (e.GetType().Name))

t "equality, same length" (fun () -> string (ia [ 1uy; 2uy ] = ia [ 1uy; 2uy ]))
t "equality, DIFFERENT length" (fun () -> string (ia [ 1uy ] = ia [ 1uy; 2uy ]))
t "hash, structural" (fun () -> string (hash (ia [ 1uy; 2uy ]) = hash (ia [ 1uy; 2uy ])))
t "compare, same length" (fun () -> string (compare (ia [ 1uy; 2uy ]) (ia [ 1uy; 3uy ])))
t "compare, DIFFERENT length" (fun () -> string (compare (ia [ 1uy ]) (ia [ 1uy; 2uy ])))

t "Map of SAME-length keys" (fun () ->
    string (Map.containsKey (ia [ 1uy; 2uy ]) (Map.ofList [ ia [ 1uy; 2uy ], (); ia [ 9uy; 9uy ], () ])))

t "Map of DIFFERENT-length keys" (fun () ->
    string (Map.containsKey (ia [ 1uy ]) (Map.ofList [ ia [ 1uy ], (); ia [ 1uy; 2uy ], () ])))

t "Set of DIFFERENT-length" (fun () -> string (Set.count (Set.ofList [ ia [ 1uy ]; ia [ 1uy; 2uy ] ])))
t "sort DIFFERENT-length" (fun () -> string (List.sort [ ia [ 1uy; 2uy ]; ia [ 1uy ] ] |> List.length))
