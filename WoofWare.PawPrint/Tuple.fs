namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module internal Tuple =
    let withLeft<'a, 'b> (x : 'a) (y : 'b) : 'a * 'b = x, y
    let withRight<'a, 'b> (y : 'b) (x : 'a) = x, y
    let lmap<'a, 'b, 'c> (f : 'a -> 'c) (x : 'a, y : 'b) : 'c * 'b = f x, y
    let rmap<'a, 'b, 'c> (f : 'b -> 'c) (x : 'a, y : 'b) : 'a * 'c = x, f y
