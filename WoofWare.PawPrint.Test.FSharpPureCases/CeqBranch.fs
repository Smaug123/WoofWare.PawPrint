module CeqBranch

#nowarn "42"

let inline ceqInt32 (a : int32) (b : int32) : bool =
    (# "ceq" a b : bool #)

let inline ceqInt64 (a : int64) (b : int64) : bool =
    (# "ceq" a b : bool #)

let inline ceqNativeInt (a : nativeint) (b : nativeint) : bool =
    (# "ceq" a b : bool #)

let inline ceqFloat64 (a : float) (b : float) : bool =
    (# "ceq" a b : bool #)

let inline ceqObj (a : obj) (b : obj) : bool =
    (# "ceq" a b : bool #)

// Use mutable locals to prevent the F# compiler from constant-folding
// literal comparisons in Release builds.
let private testObjRefSame () : bool =
    let o = obj ()
    ceqObj o o

let main (_argv : string array) : int =
    let mutable i32a = 42
    let mutable i32b = 42
    let mutable i32c = 43
    let mutable i64a = 100L
    let mutable i64b = 100L
    let mutable i64c = 200L
    let mutable na = 7n
    let mutable nb = 7n
    let mutable nc = 8n
    let mutable fa = 3.14
    let mutable fb = 3.14
    let mutable fc = 2.71
    // int32: equal
    if not (ceqInt32 i32a i32b) then 1
    // int32: not equal
    elif ceqInt32 i32a i32c then 2
    // int64: equal
    elif not (ceqInt64 i64a i64b) then 3
    // int64: not equal
    elif ceqInt64 i64a i64c then 4
    // nativeint: equal
    elif not (ceqNativeInt na nb) then 5
    // nativeint: not equal
    elif ceqNativeInt na nc then 6
    // float64: equal
    elif not (ceqFloat64 fa fb) then 7
    // float64: not equal
    elif ceqFloat64 fa fc then 8
    // obj ref: same reference
    elif not (testObjRefSame ()) then 9
    // obj ref: different references
    elif ceqObj (obj ()) (obj ()) then 10
    // obj ref: both null
    elif not (ceqObj (null : obj) (null : obj)) then 11
    else 0
