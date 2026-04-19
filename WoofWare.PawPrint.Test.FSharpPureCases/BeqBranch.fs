module BeqBranch

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

let private testObjRefSame () : bool =
    let o = obj ()
    ceqObj o o

let main (_argv : string array) : int =
    // int32: equal
    if not (ceqInt32 42 42) then 1
    // int32: not equal
    elif ceqInt32 42 43 then 2
    // int64: equal
    elif not (ceqInt64 100L 100L) then 3
    // int64: not equal
    elif ceqInt64 100L 200L then 4
    // nativeint: equal
    elif not (ceqNativeInt 7n 7n) then 5
    // nativeint: not equal
    elif ceqNativeInt 7n 8n then 6
    // float64: equal
    elif not (ceqFloat64 3.14 3.14) then 7
    // float64: not equal
    elif ceqFloat64 3.14 2.71 then 8
    // obj ref: same reference
    elif not (testObjRefSame ()) then 9
    // obj ref: different references
    elif ceqObj (obj ()) (obj ()) then 10
    // obj ref: both null
    elif not (ceqObj (null : obj) (null : obj)) then 11
    else 0
